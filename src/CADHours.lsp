;;; ============================================================
;;; CADHours.lsp  -  Loader, drawing start-up hook and commands
;;;
;;; This is the only file that has to be loaded.  It finds its own
;;; folder, pulls in the other modules, hooks the per-drawing start-up
;;; and defines the user-facing commands.
;;;
;;; Commands
;;;   CHJOB       set or change the job number for this drawing
;;;   CHSTATUS    what is being tracked right now
;;;   CHSTART     start tracking (if it was stopped)
;;;   CHSTOP      stop tracking and bank the time
;;;   CHTODAY     my hours today
;;;   CHWEEK      my hours this week
;;;   CHJOBHOURS  total hours on a job
;;;   CHFIND      search sessions
;;;   CHREPORT    guided report
;;;   CHEXPORT    export matching sessions to CSV
;;;   CHDASH      build and open the HTML dashboard
;;;   CHRECOVER   re-file sessions left behind by a crash
;;;   CHCONFIG    show the active settings
;;;   CADHOURS    command summary
;;;
;;; Requirements : AutoCAD 2019+ full, or AutoCAD LT 2024+
;;;                (LT gained AutoLISP in the 2024 release)
;;; ============================================================

(vl-load-com)


;;; ---- finding our own folder --------------------------------------
;;;
;;; Deliberately self-contained: it runs before the rest of the
;;; tracker exists, so it cannot use anything from CADHours-Core.

(defun ch:boot-env (name / v)
  (setq v (getenv name))
  (if v v "")
)

(defun ch:boot-home ( / cand hit f c)
  (setq cand
    (list
      (getenv "CADHOURS_HOME")
      (vl-registry-read "HKEY_CURRENT_USER\\Software\\CADHours" "Home")
      (strcat (ch:boot-env "APPDATA")
              "\\Autodesk\\ApplicationPlugins\\CADHours.bundle\\Contents")
      (strcat (ch:boot-env "PROGRAMDATA")
              "\\Autodesk\\ApplicationPlugins\\CADHours.bundle\\Contents")
      (strcat (ch:boot-env "PROGRAMFILES") "\\CADHours")
    ))
  (foreach c cand
    (if (and (null hit) c (/= c "") (findfile (strcat c "\\CADHours-Core.lsp")))
      (setq hit c)
    )
  )
  ;; last resort: let AutoCAD's support file search path find it
  (if (null hit)
    (if (setq f (findfile "CADHours-Core.lsp"))
      (setq hit (vl-filename-directory f))
    )
  )
  hit
)

(defun ch:boot-load ( / home ok f)
  (setq home (ch:boot-home) ok T)
  (if (null home)
    (progn
      (princ "\n** CADHours: cannot find CADHours-Core.lsp - nothing loaded.")
      (princ "\n   Set CADHOURS_HOME, or add the install folder to the support file search path.")
      nil
    )
    (progn
      (setq *ch-home* home)
      (foreach f '("CADHours-Core.lsp"
                   "CADHours-Session.lsp"
                   "CADHours-Job.lsp"
                   "CADHours-Report.lsp"
                   "CADHours-Dashboard.lsp")
        (if (findfile (strcat home "\\" f))
          (if (vl-catch-all-error-p
                (vl-catch-all-apply 'load (list (strcat home "\\" f))))
            (progn
              (princ (strcat "\n** CADHours: failed to load " f))
              (setq ok nil)
            )
          )
          (progn
            (princ (strcat "\n** CADHours: missing " f))
            (setq ok nil)
          )
        )
      )
      ok
    )
  )
)


;;; ---- per-drawing start-up ------------------------------------------

;;; Everything that happens when a drawing is ready to be worked in.
;;; Runs once per drawing, from S::STARTUP.
(defun ch:on-doc-load ( / k)
  (if *ch-started*
    nil
    (progn
      (setq *ch-started* T)
      (ch:cfg-load)
      (ch:reset-state)
      (ch:cache-settings)
      (setq k (vl-catch-all-apply 'ch:active-doc-key nil))
      (setq *ch-doc-key* (if (vl-catch-all-error-p k) "" k))

      ;; housekeeping: re-file anything a crash left behind, and push
      ;; up rows written while the share was unreachable
      (if (ch:cfg-bool "AutoRecover" T) (ch:safe 'ch:recover-live (list nil)))
      (ch:safe 'ch:flush-spool nil)

      (if (ch:spooling-p)
        (ch:say (strcat "Log share is not reachable - logging locally to "
                        (ch:cfg-path "LocalSpool")))
      )

      (if (ch:cfg-bool "PromptOnOpen" T)
        (ch:begin-with-prompt)
        (ch:begin-silently)
      )
      (ch:install-reactors)
    )
  )
  (princ)
)

;;; Ask for the job number, then start the clock.  Dismissing the
;;; dialog still starts a session - the time is banked as UNASSIGNED
;;; and CHJOB moves it onto a real job later.
(defun ch:begin-with-prompt ()
  (if (not (ch:safe 'ch:ask-job (list T)))
    (if (not *ch-active*) (ch:start-session "UNASSIGNED" "" ""))
  )
  (princ)
)

;;; PromptOnOpen=0: track quietly against the best guess
(defun ch:begin-silently ( / g)
  (setq g (ch:suggest-job (ch:dwg-path)))
  (ch:start-session (if (= (car g) "") "UNASSIGNED" (car g)) (cadr g) "")
  (if (= (car g) "")
    (setq *ch-prompt-due* (ch:cfg-bool "RequireJobNumber" T))
  )
  (princ)
)

;;; Backstop.  If S::STARTUP never fires - another application replaced
;;; it, or the drawing was opened in a way that skipped it - the first
;;; finished command starts the tracker instead.
;;; Disarming is done with a flag rather than vlr-remove: taking a
;;; reactor apart from inside its own callback is not safe, and a
;;; no-op callback costs nothing.
(defun ch:boot-guard-cb (rea args)
  (vl-catch-all-apply
    '(lambda ()
       (if *ch-boot-guard*
         (progn
           (setq *ch-boot-guard* nil)
           (if (not *ch-started*) (ch:on-doc-load))
         )
       ))
    nil)
  (princ)
)

(defun ch:install-boot-guard ( / r)
  (setq r (vl-catch-all-apply 'vlr-editor-reactor
            (list nil '((:vlr-commandEnded . ch:boot-guard-cb)))))
  (if (not (vl-catch-all-error-p r)) (setq *ch-boot-guard* r))
  (princ)
)


;;; ---- commands --------------------------------------------------------

(defun c:CHJOB ( / *error*)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
  (if (not *ch-started*) (ch:on-doc-load) (ch:ask-job T))
  (princ)
)

(defun c:CHSTART ()
  (cond
    (*ch-active* (ch:say "Already tracking - CHSTATUS shows the detail."))
    ((not *ch-started*) (ch:on-doc-load))
    (T (ch:ask-job T))
  )
  (princ)
)

(defun c:CHSTOP ()
  (if *ch-active*
    (progn
      (ch:accrue)
      (ch:say (strcat "Stopping - " (ch:hhmm *ch-acc*) " banked on job " *ch-job* "."))
      (ch:end-session "STOPPED")
      (ch:say "Tracking stopped.  CHSTART begins a new session.")
    )
    (ch:say "Nothing is being tracked in this drawing.")
  )
  (princ)
)

(defun c:CHSTATUS ( / )
  (if (not *ch-started*)
    (ch:say "The tracker has not started in this drawing.  Type CHSTART.")
    (progn
      (if *ch-active* (ch:accrue))
      (ch:hdr "CAD Hours - status")
      (princ (strcat "\n  State        : "
                     (cond ((not *ch-active*) "stopped")
                           ((not *ch-current*) "paused (another drawing is active)")
                           ((> (ch:secs *ch-last* (ch:now)) *ch-idle-limit*) "paused (idle)")
                           (T "running"))))
      (princ (strcat "\n  Job          : " (if (= *ch-job* "") "UNASSIGNED" *ch-job*)))
      (if (/= *ch-task* "")  (princ (strcat "\n  Task         : " *ch-task*)))
      (if (/= *ch-notes* "") (princ (strcat "\n  Notes        : " *ch-notes*)))
      (princ (strcat "\n  Drawing      : " *ch-dwg*))
      (princ (strcat "\n  User         : " (ch:user) " on " (ch:machine)))
      (if *ch-start-parts*
        (princ (strcat "\n  Started      : " (ch:stamp *ch-start-parts*))))
      (princ (strcat "\n  Billed       : " (ch:hhmm *ch-acc*)
                     "  (" (ch:dec-hours *ch-acc*) " h)"))
      (princ (strcat "\n  Idle dropped : " (ch:hhmm *ch-idle*)))
      (princ (strcat "\n  Saves        : " (itoa *ch-saves*)
                     "    Commands: " (itoa *ch-cmds*)))
      (princ (strcat "\n  Idle limit   : " (itoa (fix *ch-idle-limit*)) " s"
                     "   (credited: " (itoa (fix *ch-idle-credit*)) " s)"))
      (princ (strcat "\n  Log root     : " (ch:root)
                     (if (ch:spooling-p) "   [share unreachable - spooling locally]" "")))
      (princ (strcat "\n  Session id   : " *ch-sid*))
      (princ "\n")
    )
  )
  (princ)
)

(defun c:CHRECOVER ( / *error* ans n)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
  (ch:cfg-load)
  (initget "Mine All")
  (setq ans (getkword "\nRe-file interrupted sessions for [Mine/All] <Mine>: "))
  (setq n (ch:recover-live (= ans "All")))
  (ch:say (strcat (itoa n) " interrupted session(s) re-filed."))
  (princ)
)

(defun c:CHCONFIG ( / file k)
  (ch:cfg-load)
  (setq file (ch:cfg-file))
  (ch:hdr "CAD Hours - settings")
  (princ (strcat "\n  Config file  : " (if file file "(none - built-in defaults)")))
  (princ (strcat "\n  Install home : " (if (ch:home) (ch:home) "(unknown)")))
  (princ (strcat "\n  Log root     : " (ch:cfg-path "LogRoot")
                 (if (ch:dir-p (ch:cfg-path "LogRoot")) "  [reachable]" "  [NOT reachable]")))
  (princ (strcat "\n  Local spool  : " (ch:cfg-path "LocalSpool")))
  (princ "\n")
  (foreach k '("IdleSeconds" "IdleCreditSeconds" "HeartbeatSeconds"
               "MinSessionSeconds" "PromptOnOpen" "RequireJobNumber"
               "RepromptSeconds" "JobPattern" "JobFromPath" "DefaultJob"
               "RememberJobInDwg" "WriteEventLog" "TrackObjectEdits"
               "TrackSysVarChanges" "AutoRecover" "StaleLiveHours" "Debug")
    (princ (strcat "\n  " (ch:rpad k 20) ": " (ch:cfg k)))
  )
  (princ "\n")
  (princ)
)

(defun c:CADHOURS ()
  (ch:hdr (strcat "CAD Hours Tracker " *ch-version*))
  (princ "\n  CHJOB       set or change the job number for this drawing")
  (princ "\n  CHSTATUS    what is being tracked right now")
  (princ "\n  CHSTART     start tracking     CHSTOP   stop and bank the time")
  (princ "\n")
  (princ "\n  CHTODAY     my hours today, by job")
  (princ "\n  CHWEEK      my hours this week, by day and by job")
  (princ "\n  CHJOBHOURS  everything booked to one job")
  (princ "\n  CHFIND      list sessions matching a user / job / period")
  (princ "\n  CHREPORT    guided report with a grouping of your choice")
  (princ "\n  CHEXPORT    write matching sessions to a CSV")
  (princ "\n  CHDASH      build and open the searchable HTML dashboard")
  (princ "\n")
  (princ "\n  CHRECOVER   re-file sessions left behind by a crash")
  (princ "\n  CHCONFIG    show the active settings")
  (princ "\n")
  (princ)
)


;;; ---- install ----------------------------------------------------------

(if (ch:boot-load)
  (progn
    (ch:reset-state)

    ;; chain S::STARTUP rather than replacing it, so any other tool
    ;; that uses the same hook keeps working
    (if (not *ch-startup-hooked*)
      (progn
        (setq *ch-prev-startup* s::startup)
        (defun s::startup ()
          (if *ch-prev-startup* (vl-catch-all-apply *ch-prev-startup* nil))
          (vl-catch-all-apply 'ch:on-doc-load nil)
          (princ)
        )
        (setq *ch-startup-hooked* T)
      )
    )

    (ch:install-boot-guard)
    (princ (strcat "\nCAD Hours Tracker " *ch-version*
                   " loaded.  Type CADHOURS for the command list."))
  )
)

(princ)
;;; ============================================================ EOF
