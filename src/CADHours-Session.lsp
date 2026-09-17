;;; ============================================================
;;; CADHours-Session.lsp  -  Session timing engine
;;;
;;; Owns one timing session per open drawing:
;;;
;;;   * starts when the drawing finishes loading and a job number is
;;;     known
;;;   * accrues working time from editor / database events
;;;   * treats any gap longer than IdleSeconds as a pause, crediting
;;;     at most IdleCreditSeconds of it as work
;;;   * stops billing while a different drawing is the current one
;;;   * writes a heartbeat row so a crash loses at most one interval
;;;   * commits a completed session row when the drawing closes
;;;
;;; There is no timer in AutoLISP, and none is needed: the gap between
;;; two consecutive activity events *is* the idle period, measured
;;; exactly.  Idle time is therefore settled retroactively on the next
;;; event, which costs nothing while the user is away.
;;;
;;; All state is namespace-local, so every open drawing keeps its own
;;; independent session.
;;; ============================================================

(vl-load-com)


;;; ---- session state -----------------------------------------------

(defun ch:reset-state ()
  (setq *ch-active*       nil     ; T while a session is open
        *ch-sid*          ""      ; session id
        *ch-job*          ""      ; job number this time is billed to
        *ch-task*         ""      ; optional task / phase
        *ch-mgr*          ""      ; project manager responsible for the job
        *ch-notes*        ""      ; optional note
        *ch-dwg*          ""      ; full path of the drawing
        *ch-doc-key*      ""      ; identity of this document
        *ch-start-date*   0.0     ; ch:now at session start
        *ch-start-parts*  nil     ; calendar parts at session start
        *ch-last*         0.0     ; ch:now of the last activity
        *ch-last-parts*   nil     ; calendar parts at the last heartbeat
        *ch-acc*          0.0     ; billed seconds
        *ch-idle*         0.0     ; discarded idle seconds
        *ch-saves*        0
        *ch-cmds*         0
        *ch-last-beat*    0.0
        *ch-current*      T       ; T while this drawing is the active one
        *ch-cur-checked*  nil     ; when *ch-current* was last verified
        *ch-last-save*    0.0     ; debounces double-reported saves
        *ch-obj-count*    0
        *ch-prompt-due*   nil     ; a job number still has to be collected
        *ch-last-prompt*  0.0
        *ch-in-dialog*    nil
  )
  (princ)
)

;;; Cache the handful of settings the hot path reads, so an activity
;;; event never has to walk the configuration list.
(defun ch:cache-settings ()
  (setq *ch-idle-limit*  (float (ch:cfg-int "IdleSeconds" 300))
        *ch-idle-credit* (float (ch:cfg-int "IdleCreditSeconds" 300))
        *ch-beat-secs*   (float (ch:cfg-int "HeartbeatSeconds" 60))
        *ch-obj-stride*  (max 1 (ch:cfg-int "ObjectEventStride" 20)))
  (if (< *ch-idle-credit* 0.0) (setq *ch-idle-credit* 0.0))
  (if (> *ch-idle-credit* *ch-idle-limit*)
    (setq *ch-idle-credit* *ch-idle-limit*)
  )
  (princ)
)

;;; A session id that is unique across users, machines and instances
(defun ch:new-sid ( / p frac)
  (setq p    (ch:parts)
        frac (rem (abs (fix (* 1.0e6 (- (ch:now) (fix (ch:now)))))) 10000))
  (strcat (ch:safe-name (ch:user)) "-"
          (ch:safe-name (ch:machine)) "-"
          (ch:compact-str p) "-"
          (ch:pad4 frac))
)


;;; ---- document identity -------------------------------------------

;;; Path + name of a VLA document, upper-cased.  Two open drawings can
;;; never share one, which makes it a usable identity.
(defun ch:doc-key (doc / p n)
  (setq p (vl-catch-all-apply 'vla-get-path (list doc)))
  (if (vl-catch-all-error-p p) (setq p ""))
  (setq n (vl-catch-all-apply 'vla-get-name (list doc)))
  (if (vl-catch-all-error-p n) (setq n ""))
  (strcase (strcat (ch:norm-path (ch:str p)) "\\" (ch:str n)))
)

;;; Number of open documents, 0 when it cannot be determined
(defun ch:doc-count ( / n)
  (setq n (vl-catch-all-apply
            '(lambda ()
               (vla-get-count (vla-get-documents (vlax-get-acad-object))))
            nil))
  (if (vl-catch-all-error-p n) 0 n)
)

(defun ch:active-doc ()
  (vla-get-activedocument (vlax-get-acad-object))
)

(defun ch:active-doc-key ()
  (ch:doc-key (ch:active-doc))
)

;;; Full path of the current drawing, "" for one never saved
(defun ch:dwg-path ( / pre nm)
  (setq pre (ch:str (getvar "DWGPREFIX"))
        nm  (ch:str (getvar "DWGNAME")))
  (if (or (= nm "") (= (getvar "DWGTITLED") 0))
    nm                                  ; unsaved: just "Drawing1.dwg"
    (strcat (ch:norm-path pre) "\\" nm)
  )
)


;;; ---- the clock ----------------------------------------------------

;;; Settle the time since the previous activity event.
;;;
;;; A gap no longer than IdleSeconds is billed in full.  A longer gap
;;; is billed for IdleCreditSeconds and the remainder is recorded as
;;; idle, which is what "pause after five minutes of inactivity"
;;; means once you measure it after the fact.
(defun ch:accrue ( / now gap credit lost)
  (if *ch-active*
    (progn
      (setq now (ch:now)
            gap (ch:secs *ch-last* now))
      (cond
        ((< gap 0.0) nil)                     ; clock stepped backwards
        ((<= gap *ch-idle-limit*)
          (setq *ch-acc* (+ *ch-acc* gap)))
        (T
          (setq credit (min gap *ch-idle-credit*)
                lost   (- gap credit))
          (setq *ch-acc*  (+ *ch-acc* credit)
                *ch-idle* (+ *ch-idle* lost))
          (ch:log-event "RESUME"
                        (strcat "idle " (ch:isecs lost) "s"))
        )
      )
      (setq *ch-last* now)
      (ch:maybe-beat now)
    )
  )
  (princ)
)

;;; Am I the drawing the user is actually working in?
;;;
;;; This has to be asked, not remembered.  Editor and database reactors
;;; are application-wide, so every open drawing is told about every
;;; command - and a drawing that believes it is current when it is not
;;; will bill the whole day's work a second time, on its own job number.
;;; Relying on :vlr-documentBecameCurrent alone is not enough: a drawing
;;; opened in the background never receives one, and starts life with
;;; the flag set.
;;;
;;; The answer is cached for a couple of seconds so the hot path costs
;;; one COM call every few seconds rather than one per event.  An error
;;; keeps the previous answer rather than defaulting either way, because
;;; guessing T over-bills and guessing nil stops the clock.
(defun ch:current-p ( / now k)
  (setq now (ch:now))
  (if (or (null *ch-cur-checked*)
          (> (abs (ch:secs *ch-cur-checked* now)) 2.0))
    (progn
      (setq k (vl-catch-all-apply 'ch:active-doc-key nil))
      (if (not (vl-catch-all-error-p k))
        (setq *ch-current* (= k *ch-doc-key*))
      )
      (setq *ch-cur-checked* now)
    )
  )
  *ch-current*
)

;;; Force the next ch:current-p to re-ask
(defun ch:recheck-current ()
  (setq *ch-cur-checked* nil)
  (princ)
)

;;; Called by every activity handler.
;;;
;;; A drawing that is not current still refreshes its live row, so that
;;; a session left open in a background tab does not look abandoned to
;;; ch:recover-live and get filed twice.
(defun ch:touch ()
  (if *ch-active*
    (if (ch:current-p)
      (ch:accrue)
      (ch:maybe-beat (ch:now))
    )
  )
  (princ)
)

(defun ch:maybe-beat (now)
  (if (> (ch:secs *ch-last-beat* now) *ch-beat-secs*)
    (progn
      (setq *ch-last-beat*  now
            *ch-last-parts* (ch:parts))
      (ch:write-live)
    )
  )
  (princ)
)


;;; ---- log records ---------------------------------------------------

;;; Column order is append-only on purpose.  "manager" arrives after
;;; "version" rather than next to "task", because inserting a column in
;;; the middle would silently shift every field in every row already on
;;; the share.
(defun ch:session-columns ()
  (list "session_id" "status" "user" "machine" "job" "task"
        "dwg_name" "dwg_path" "start_local" "end_local"
        "date" "week" "month"
        "active_sec" "idle_sec" "wall_sec" "active_hours"
        "saves" "commands" "notes" "version" "manager")
)

(defun ch:session-header ()
  (ch:csv-line (ch:session-columns))
)

(defun ch:event-columns ()
  (list "stamp" "event" "session_id" "user" "job" "dwg_path"
        "active_sec" "idle_sec" "note" "manager")
)

(defun ch:event-header ()
  (ch:csv-line (ch:event-columns))
)

;;; One session record.  END-PARTS is the calendar time the session is
;;; being closed at; for a live row that is the last heartbeat.
(defun ch:session-row (status end-parts / wall)
  (if (null end-parts) (setq end-parts (ch:parts)))
  (setq wall (ch:secs *ch-start-date* (ch:now)))
  (ch:csv-line
    (list
      *ch-sid*
      status
      (ch:user)
      (ch:machine)
      (if (= *ch-job* "") "UNASSIGNED" *ch-job*)
      *ch-task*
      (vl-filename-base *ch-dwg*)
      *ch-dwg*
      (ch:stamp *ch-start-parts*)
      (ch:stamp end-parts)
      (ch:date-str *ch-start-parts*)
      (ch:week-str *ch-start-parts*)
      (ch:month-str *ch-start-parts*)
      (ch:isecs *ch-acc*)
      (ch:isecs *ch-idle*)
      (ch:isecs wall)
      (ch:dec-hours *ch-acc*)
      (itoa *ch-saves*)
      (itoa *ch-cmds*)
      *ch-notes*
      *ch-version*
      *ch-mgr*
    )
  )
)

;;; Path of the monthly sessions file this user writes into.  One
;;; writer per user per machine per month keeps contention close to
;;; zero even on a busy share.
(defun ch:sessions-file (root month)
  (ch:path+ (ch:sessions-dir root month)
            (strcat (ch:safe-name (ch:user)) "__"
                    (ch:safe-name (ch:machine)) "__"
                    month ".csv"))
)

(defun ch:live-file (root)
  (ch:path+ (ch:live-dir root) (strcat *ch-sid* ".csv"))
)

(defun ch:event-file (root)
  (ch:path+ (ch:events-dir root (ch:date-str (ch:parts)))
            (strcat *ch-sid* ".csv"))
)

;;; Rewrite this session's live row.  Single writer, so no lock.
(defun ch:write-live ( / root)
  (if *ch-active*
    (progn
      (setq root (ch:root))
      (ch:safe 'ch:write-line-file
               (list (ch:live-file root)
                     (ch:session-row "RUNNING" *ch-last-parts*)
                     (ch:session-header)))
    )
  )
  (princ)
)

(defun ch:delete-live ( / f)
  (setq f (ch:live-file (ch:root)))
  (if (findfile f) (vl-file-delete f))
  (princ)
)

;;; Append the finished session to the monthly file
;;; Returns non-nil only when the row really reached the sessions file.
(defun ch:commit-session (status end-parts / root month file)
  (setq root  (ch:root)
        month (ch:month-str *ch-start-parts*)
        file  (ch:sessions-file root month))
  (ch:safe 'ch:append-locked
           (list file
                 (ch:session-row status end-parts)
                 (ch:session-header)
                 *ch-sid*))
)

;;; Append one line to this session's detailed event log
(defun ch:log-event (event note / root)
  (if (and *ch-sid* (/= *ch-sid* "") (ch:cfg-bool "WriteEventLog" T))
    (progn
      (setq root (ch:root))
      (ch:safe 'ch:append-line
        (list (ch:event-file root)
              (ch:csv-line
                (list (ch:stamp (ch:parts))
                      event
                      *ch-sid*
                      (ch:user)
                      *ch-job*
                      *ch-dwg*
                      (ch:isecs *ch-acc*)
                      (ch:isecs *ch-idle*)
                      (ch:str note)
                      *ch-mgr*))
              (ch:event-header)))
    )
  )
  (princ)
)


;;; ---- session lifecycle ---------------------------------------------

(defun ch:start-session (job task notes mgr / now)
  (if *ch-active* (ch:end-session "SPLIT"))
  (ch:cache-settings)
  (setq now              (ch:now)
        *ch-sid*         (ch:new-sid)
        *ch-job*         (ch:trim (ch:str job))
        *ch-task*        (ch:trim (ch:str task))
        *ch-notes*       (ch:trim (ch:str notes))
        *ch-mgr*         (ch:trim (ch:str mgr))
        *ch-dwg*         (ch:dwg-path)
        *ch-doc-key*     (vl-catch-all-apply 'ch:active-doc-key nil)
        *ch-start-date*  now
        *ch-start-parts* (ch:parts)
        *ch-last*        now
        *ch-last-parts*  (ch:parts)
        *ch-last-beat*   now
        *ch-acc*         0.0
        *ch-idle*        0.0
        *ch-saves*       0
        *ch-cmds*        0
        *ch-obj-count*   0
        *ch-last-save*   0.0
        *ch-cur-checked* nil      ; verified on the first activity event
        *ch-active*      T)
  (if (vl-catch-all-error-p *ch-doc-key*) (setq *ch-doc-key* ""))
  (ch:log-event "SESSION_START" (ch:str (getvar "DWGNAME")))
  (ch:write-live)
  (ch:dbg (strcat "session " *ch-sid* " started on job " *ch-job*))
  (princ)
)

;;; Close the session out.  Sessions shorter than MinSessionSeconds
;;; are dropped so that opening a drawing to glance at it does not
;;; litter the database.
;;;
;;; The live row is refreshed one last time before the commit, and is
;;; only deleted once the session row is safely written - if the share
;;; disappeared mid-session the row stays in live\ and CHRECOVER
;;; re-files it rather than the time being lost.
(defun ch:end-session (status / end-parts)
  (if *ch-active*
    (progn
      (ch:accrue)
      (setq end-parts      (ch:parts)
            *ch-last-parts* end-parts)
      ;; the session is over, so there is nothing left to ask about.
      ;; Without this, a drawing whose prompt was skipped keeps nagging
      ;; after it has been closed - the CLOSE command itself raises a
      ;; commandEnded, which is what brings the pop-up back.
      (setq *ch-active*      nil
            *ch-prompt-due*  nil)
      (ch:log-event "SESSION_END" status)
      ;; commit first, then drop the live row.  The live row is only
      ;; rewritten if the commit failed, which saves a write across the
      ;; share on the path a drawing closes through.
      (if (>= *ch-acc* (float (ch:cfg-int "MinSessionSeconds" 10)))
        (if (ch:commit-session status end-parts)
          (ch:delete-live)
          (progn
            (setq *ch-active* T)
            (ch:write-live)
            (setq *ch-active* nil)
            (ch:say (strcat "Could not write the session row for job "
                            *ch-job* " - it is held in the live folder "
                            "and CHRECOVER will re-file it."))
          )
        )
        (progn
          (ch:dbg "session below MinSessionSeconds - not recorded")
          (ch:delete-live)
        )
      )
    )
  )
  (princ)
)

;;; Change the job mid-session.  Relabelling an unassigned session is
;;; free; moving real billed time to another job splits the session so
;;; each job keeps the minutes actually worked on it.
(defun ch:set-job (job task notes mgr)
  (setq job (ch:trim (ch:str job))
        mgr (ch:trim (ch:str mgr)))
  (cond
    ((= job "") nil)
    ((not *ch-active*) (ch:start-session job task notes mgr))
    ((or (= *ch-job* "") (= (strcase *ch-job*) "UNASSIGNED"))
      (setq *ch-job*   job
            *ch-task*  (ch:trim (ch:str task))
            *ch-notes* (ch:trim (ch:str notes))
            *ch-mgr*   mgr)
      (ch:log-event "JOB_SET" job)
      (ch:write-live)
    )
    ((= (strcase job) (strcase *ch-job*))
      ;; same job: notes, task and manager are just corrections
      (setq *ch-task*  (ch:trim (ch:str task))
            *ch-notes* (ch:trim (ch:str notes))
            *ch-mgr*   mgr)
      (ch:write-live)
    )
    (T
      (ch:end-session "SPLIT")
      (ch:start-session job task notes mgr)
    )
  )
  ;; only once a real job number is on the session - an empty answer
  ;; must not disarm the prompt or write a blank into the recent list
  (if (/= job "")
    (progn
      (setq *ch-prompt-due* nil)
      (ch:mru-add job)
      (ch:job-mgr-remember job mgr)
      (ch:mgr-remember-last mgr)
      (if (ch:cfg-bool "RememberJobInDwg" nil) (ch:dwg-job-put job task))
    )
  )
  (princ)
)


;;; ---- reactor callbacks ---------------------------------------------
;;;
;;; Callbacks must be cheap and must never throw, so each one is a
;;; thin guarded wrapper.  Nothing here calls the command line.

(defun ch:on-command-start (rea args)
  (vl-catch-all-apply
    '(lambda ()
       (if (ch:current-p) (setq *ch-cmds* (1+ *ch-cmds*)))
       (ch:touch))
    nil)
  (princ)
)

(defun ch:on-command-end (rea args)
  (vl-catch-all-apply
    '(lambda () (ch:touch) (ch:maybe-reprompt))
    nil)
  (princ)
)

(defun ch:on-activity (rea args)
  (vl-catch-all-apply 'ch:touch nil)
  (princ)
)

;;; Object events fire once per modified entity, which during a big
;;; edit means thousands of calls.  Only every Nth one does any work;
;;; the command handlers catch anything the stride misses.
(defun ch:on-object (rea args)
  (vl-catch-all-apply
    '(lambda ()
       (setq *ch-obj-count* (1+ *ch-obj-count*))
       (if (>= *ch-obj-count* *ch-obj-stride*)
         (progn (setq *ch-obj-count* 0) (ch:touch))))
    nil)
  (princ)
)

;;; A save.
;;;
;;; beginSave is registered on both the editor and the drawing reactor,
;;; because releases differ in which one raises it.  Where both do, one
;;; save arrives twice - so a second report within a second of the first
;;; is ignored rather than counted.
(defun ch:on-save (rea args)
  (vl-catch-all-apply
    '(lambda ( / now)
       (setq now (ch:now))
       (if (and (ch:current-p)
                (> (abs (ch:secs *ch-last-save* now)) 1.0))
         (progn
           (setq *ch-last-save* now
                 *ch-saves*     (1+ *ch-saves*))
           (ch:touch)
           (ch:log-event "SAVE" (ch:str (getvar "DWGNAME")))
           (ch:write-live))))
    nil)
  (princ)
)

;;; SAVEAS moves the drawing, so the recorded path and the document
;;; identity both have to follow it.
;;; SAVEAS moves the drawing, so the recorded path and this namespace's
;;; idea of its own identity both have to follow it.
;;;
;;; This used to be gated on "is the active document still the one I
;;; think I am" - which is false immediately after a SAVEAS, because the
;;; document's key has already changed.  The update was therefore skipped
;;; exactly when it was needed, leaving the namespace holding a key that
;;; matches nothing: the close handler then never recognised its own
;;; drawing and the session was never committed.  Saving a new drawing
;;; into its job folder is the commonest thing a drafter does, so this
;;; lost whole sessions.
;;;
;;; The name now moves first and the key is re-derived from it.
(defun ch:on-save-complete (rea args)
  (vl-catch-all-apply
    '(lambda ( / new k j)
       (setq new (ch:dwg-path))
       (if (and (/= new "") (/= new *ch-dwg*))
         (progn
           (ch:log-event "PATH_CHANGED" new)
           (setq k (vl-catch-all-apply 'ch:active-doc-key nil))
           (setq *ch-dwg* new)
           (if (not (vl-catch-all-error-p k)) (setq *ch-doc-key* k))
           (ch:recheck-current)

           ;; A drawing saved into its job folder has just told us which
           ;; job it belongs to.  That is evidence, not a guess, so an
           ;; unassigned session adopts it - which is how a template or
           ;; a scratch file ends up on the right job without anybody
           ;; being asked twice.
           (if (or (= *ch-job* "") (= (strcase *ch-job*) "UNASSIGNED"))
             (progn
               (setq j (ch:job-from-path new (ch:trim (ch:cfg "JobPattern"))))
               (if j
                 (progn
                   (ch:set-job j *ch-task* *ch-notes* *ch-mgr*)
                   (ch:say (strcat "saved into job " j
                                   " - this drawing's time is now on it."))
                 )
               )
             )
           )
           (ch:write-live)
         )
       )
     )
    nil)
  (princ)
)

;;; Bank what is known without ending the session.
(defun ch:flush-session ()
  (if *ch-active*
    (progn
      (if (ch:current-p) (ch:accrue))
      (setq *ch-last-parts* (ch:parts))
      (ch:write-live)
    )
  )
  (princ)
)

;;; A drawing closing.
;;;
;;; :vlr-beginClose carries no document, so there is no way to tell from
;;; here WHICH drawing is closing - and the event reaches every open
;;; drawing.  Ending the session on it therefore closed out whichever
;;; drawing happened to be active, splitting the hours of a drawing the
;;; user was still working in, while the drawing actually closing was
;;; skipped.  It also fired on a close the user then cancelled.
;;;
;;; So this only flushes.  Committing is left to
;;; :vlr-documentToBeDestroyed, which does identify the document and
;;; only fires once the drawing really is going away.  The one case that
;;; can be settled here is a single open drawing, where the drawing
;;; closing must be this one.
(defun ch:on-close (rea args)
  (vl-catch-all-apply
    '(lambda ()
       (ch:flush-session)
       (if (and *ch-active* (= (ch:doc-count) 1))
         (ch:end-session "CLOSED")))
    nil)
  (princ)
)

;;; Quitting AutoCAD.  Cancellable, so again only a flush - each
;;; document's own destroy handler commits it as it goes.
(defun ch:on-quit (rea args)
  (vl-catch-all-apply 'ch:flush-session nil)
  (princ)
)

;;; The user switched drawing tabs.  Bank what this drawing has earned
;;; so far, then let ch:current-p re-derive who is current rather than
;;; latching a guess.
(defun ch:on-doc-switch (rea args)
  (vl-catch-all-apply
    '(lambda ()
       (if (and *ch-active* *ch-current*) (ch:accrue))
       (ch:recheck-current)
       ;; coming back: do not bill the time spent in the other drawing
       (if (ch:current-p) (setq *ch-last* (ch:now)) (ch:write-live)))
    nil)
  (princ)
)

(defun ch:on-doc-destroy (rea args)
  (vl-catch-all-apply
    '(lambda ( / doc)
       (setq doc (ch:arg-doc args))
       (if (if doc
             (= (ch:doc-key doc) *ch-doc-key*)
             (<= (ch:doc-count) 1))
         (ch:end-session "CLOSED")))
    nil)
  (princ)
)

;;; First VLA object found in a callback's argument list
(defun ch:arg-doc (args / d a)
  (foreach a (if (listp args) args (list args))
    (if (and (null d) (= (type a) 'VLA-OBJECT)) (setq d a))
  )
  d
)


;;; ---- reactor installation -------------------------------------------

(defun ch:remove-reactors ( / r)
  (foreach r *ch-reactors*
    (vl-catch-all-apply 'vlr-remove (list r))
  )
  (setq *ch-reactors* nil)
  (princ)
)

;;; Register ONE event on its own reactor.
;;;
;;; Event names differ between releases, and a reactor refuses to
;;; construct if any name in its list is unknown.  Grouping events
;;; therefore meant one unsupported name silently removed every handler
;;; beside it - losing close handling, for instance, with no trace
;;; outside a debug line.  One event per reactor keeps a failure to
;;; itself, and the failure is now reported rather than whispered.
;;; Register ONE event on its own reactor.
;;;
;;; EVENT must arrive already quoted.  The alist form this replaced kept
;;; the :vlr-... name inside a quote, so it was never evaluated; passing
;;; it bare would rely on AutoLISP treating a leading colon as
;;; self-evaluating, and if it does not, every name becomes nil and
;;; nothing is tracked at all.
;;;
;;; Event names genuinely differ between releases - AutoCAD 2027 has no
;;; :vlr-beginQuit on the editor reactor, for instance - and a reactor
;;; refuses to construct if a name is unknown.  One event per reactor
;;; keeps such a failure to itself instead of silently removing every
;;; handler beside it.
;;;
;;; REQUIRED says whether losing this event actually costs anything.
;;; Only those are reported; an optional one that a release does not
;;; offer is not a problem the user can act on, and saying so on every
;;; drawing is just noise.
(defun ch:add-event (maker event callback required / r)
  (setq r (vl-catch-all-apply maker (list nil (list (cons event callback)))))
  (if (vl-catch-all-error-p r)
    (progn
      (if required
        (setq *ch-reactor-fails*
              (cons (vl-princ-to-string event) *ch-reactor-fails*))
        (ch:dbg (strcat "optional event not on this release: "
                        (vl-princ-to-string event)))
      )
      nil
    )
    (progn (setq *ch-reactors* (cons r *ch-reactors*)) r)
  )
)

(defun ch:install-reactors ( / ed)
  (ch:remove-reactors)
  (setq *ch-reactor-fails* nil
        ed                 'vlr-editor-reactor)

  ;; commands - the main activity signal
  (ch:add-event ed ':vlr-commandWillStart 'ch:on-command-start T)
  (ch:add-event ed ':vlr-commandEnded     'ch:on-command-end   T)
  (ch:add-event ed ':vlr-commandCancelled 'ch:on-command-end   nil)
  (ch:add-event ed ':vlr-commandFailed    'ch:on-command-end   nil)

  ;; save / close / quit
  (ch:add-event ed ':vlr-beginSave    'ch:on-save          nil)
  (ch:add-event ed ':vlr-saveComplete 'ch:on-save-complete nil)
  (ch:add-event ed ':vlr-beginClose   'ch:on-close         nil)
  (ch:add-event ed ':vlr-beginQuit    'ch:on-quit          nil)  ; absent on 2027

  ;; the same two off the drawing reactor, for releases where the editor
  ;; reactor does not raise them.  ch:on-save debounces the duplicate.
  (ch:add-event 'vlr-dwg-reactor ':vlr-beginSave  'ch:on-save  nil)
  (ch:add-event 'vlr-dwg-reactor ':vlr-beginClose 'ch:on-close nil)

  ;; grip edits and Properties-palette changes raise no command, so the
  ;; database reactor is what catches them
  (if (ch:cfg-bool "TrackObjectEdits" T)
    (progn
      (ch:add-event 'vlr-acdb-reactor ':vlr-objectModified 'ch:on-object nil)
      (ch:add-event 'vlr-acdb-reactor ':vlr-objectAppended 'ch:on-object nil)
      (ch:add-event 'vlr-acdb-reactor ':vlr-objectErased   'ch:on-object nil)
    )
  )

  ;; double-click editing
  (ch:add-event 'vlr-mouse-reactor ':vlr-beginDoubleClick 'ch:on-activity nil)

  ;; optional, off by default: some sysvars change without the user
  ;; doing anything, which would keep the clock running while idle
  (if (ch:cfg-bool "TrackSysVarChanges" nil)
    (ch:add-event 'vlr-sysvar-reactor ':vlr-sysVarChanged 'ch:on-activity nil)
  )

  ;; drawing-tab switches, and the authoritative end-of-drawing event
  (ch:add-event 'vlr-docmanager-reactor
                ':vlr-documentBecameCurrent 'ch:on-doc-switch T)
  (ch:add-event 'vlr-docmanager-reactor
                ':vlr-documentToBeDestroyed 'ch:on-doc-destroy T)

  (ch:dbg (strcat (itoa (length *ch-reactors*)) " reactor(s) active"))
  (if *ch-reactor-fails*
    (progn
      (ch:say (strcat "these events are missing on this release: "
                      (ch:join (reverse *ch-reactor-fails*) " ")))
      (ch:say "time tracking may be incomplete - please report this.")
    )
  )
  (princ)
)


;;; ---- crash recovery --------------------------------------------------
;;;
;;; A live/ row whose file has not been touched for hours belongs to an
;;; AutoCAD that died.  Its last heartbeat is a truthful end time, so
;;; the row is promoted into the sessions table rather than discarded.

(defun ch:recover-live (all / root dir stale mine n sid rows hdr row cols month tgt f)
  (setq root  (ch:root)
        dir   (ch:live-dir root)
        stale (* 3600.0 (float (ch:cfg-int "StaleLiveHours" 8)))
        mine  (strcase (strcat (ch:safe-name (ch:user)) "-"
                               (ch:safe-name (ch:machine)) "-"))
        n     0)
  (foreach f (ch:files dir "*.csv")
    (setq sid (vl-filename-base f))
    (if (and (/= sid *ch-sid*)
             (or all (= (strcase (substr sid 1 (strlen mine))) mine))
             (ch:stale-p f stale))
      (progn
        (setq rows (ch:read-lines f))
        (if (and rows (> (length rows) 1))
          (progn
            (setq hdr   (car rows)
                  row   (ch:replace (cadr rows) "\"RUNNING\"" "\"RECOVERED\"")
                  cols  (ch:csv-parse row)
                  month (ch:fld cols 12))
            (if (= month "") (setq month (ch:month-str (ch:parts))))
            (setq tgt (ch:path+ (ch:sessions-dir root month)
                                (strcat (ch:safe-name (ch:fld cols 2)) "__"
                                        (ch:safe-name (ch:fld cols 3)) "__"
                                        month ".csv")))
            (if (ch:safe 'ch:append-locked (list tgt row hdr sid))
              (progn (vl-file-delete f) (setq n (1+ n)))
            )
          )
          ;; header-only or unreadable: nothing to salvage
          (vl-file-delete f)
        )
      )
    )
  )
  (if (> n 0) (ch:say (strcat (itoa n) " interrupted session(s) recovered.")))
  n
)

;;; True when PATH has not been written to for SECS seconds.  An
;;; unknown age counts as not stale, so a live session is never
;;; harvested out from under a running AutoCAD.
(defun ch:stale-p (path secs / age)
  (setq age (ch:file-age path))
  (and age (> age secs))
)


;;; ---- spool flush ------------------------------------------------------
;;;
;;; Rows written while the share was unreachable live under LocalSpool
;;; in the same layout.  When the share comes back they are appended to
;;; the real files and removed locally.

(defun ch:flush-spool ( / spool root moved lines dir f ln month)
  (setq spool (ch:cfg-path "LocalSpool")
        root  (ch:cfg-path "LogRoot")
        moved 0)
  (if (and (/= root "") (ch:dir-p root) (ch:dir-p spool)
           (/= (strcase spool) (strcase root)))
    (foreach month (ch:subdirs (ch:path+ spool "sessions"))
      (setq dir (ch:path+ (ch:path+ spool "sessions") month))
      (foreach f (ch:files dir "*.csv")
        (setq lines (ch:read-lines f))
        (if (and lines (> (length lines) 1))
          (progn
            (foreach ln (cdr lines)
              (ch:safe 'ch:append-locked
                       (list (ch:path+ (ch:sessions-dir root month)
                                       (strcat (vl-filename-base f) ".csv"))
                             ln (car lines) "spool"))
            )
            (vl-file-delete f)
            (setq moved (1+ moved))
          )
          (vl-file-delete f)
        )
      )
    )
  )
  (if (> moved 0) (ch:dbg (strcat (itoa moved) " spooled file(s) uploaded")))
  moved
)


(ch:module "Session" "1.4.1")

(princ)
;;; ============================================================ EOF
