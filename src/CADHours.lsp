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
;;;   CHDASHALL   refresh it with every session, no prompts
;;;   CHRECOVER   re-file sessions left behind by a crash
;;;   CHCONFIG    show the active settings
;;;   CHDLG       test the pop-up on its own
;;;   CADHOURS    command summary
;;;
;;; Requirements : AutoCAD 2019+ full, or AutoCAD LT 2024+
;;;                (LT gained AutoLISP in the 2024 release)
;;; ============================================================

(vl-load-com)


;;; ---- finding our own folder, and loading fast --------------------
;;;
;;; AutoCAD gives every open drawing its own AutoLISP namespace, so this
;;; code has to be loaded once per drawing - there is no way round that
;;; for a per-document tool.  What matters is where it is loaded FROM.
;;; Read straight off the server, that is 100 kB over SMB every time
;;; somebody opens a file, which is exactly the delay nobody wants.
;;;
;;; So the modules are mirrored into the user's local profile and loaded
;;; from there.  The server copy is still the master: its timestamps are
;;; checked on every drawing (a handful of cheap stat calls) and the
;;; mirror is refreshed only when they change - normally once, after a
;;; deployment.  Settings still come from the server, so changing
;;; cadhours.ini or managers.txt behaves exactly as before.
;;;
;;; Deliberately self-contained: this runs before CADHours-Core exists.

;;; Modules every drawing needs
(setq *ch-core-modules*
  '("CADHours-Core.lsp" "CADHours-Session.lsp" "CADHours-Job.lsp"))

;;; Modules only needed once somebody asks for a report
(setq *ch-report-modules*
  '("CADHours-Report.lsp" "CADHours-Dashboard.lsp"))

(defun ch:boot-env (name / v)
  (setq v (getenv name))
  (if v v "")
)

(defun ch:boot-home ( / cand hit f c)
  (setq cand
    (list
      ;; set by whatever loaded us - this is how a line added to an
      ;; existing acaddoc.lsp points at a folder on the server
      *ch-home*
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

;;; Where the local mirror lives, or nil if we have nowhere to put it
(defun ch:boot-mirror ( / la)
  (setq la (ch:boot-env "LOCALAPPDATA"))
  (if (= la "") nil (strcat la "\\CADHours\\modules"))
)

(defun ch:boot-mkdir (dir / parts cur p)
  (if (vl-file-directory-p dir)
    T
    (progn
      (setq parts (ch:boot-split dir "\\")
            cur   (car parts))
      (foreach p (cdr parts)
        (if (/= p "")
          (progn
            (setq cur (strcat cur "\\" p))
            (if (not (vl-file-directory-p cur)) (vl-mkdir cur))
          )
        )
      )
      (vl-file-directory-p dir)
    )
  )
)

(defun ch:boot-split (str delim / pos res)
  (while (setq pos (vl-string-search delim str))
    (setq res (cons (substr str 1 pos) res)
          str (substr str (+ pos 1 (strlen delim))))
  )
  (reverse (cons str res))
)

;;; One string describing the server copies, so a change is detectable
;;; without reading them.  Stat calls, not reads.
(defun ch:boot-fingerprint (home / s f)
  (setq s "")
  (foreach f (append *ch-core-modules* *ch-report-modules*)
    (setq s (strcat s "|" (vl-princ-to-string
                            (vl-file-systime (strcat home "\\" f)))))
  )
  s
)

(defun ch:boot-read1 (path / f v)
  (if (and (findfile path) (setq f (open path "r")))
    (progn (setq v (read-line f)) (close f) v)
  )
)

(defun ch:boot-write1 (path text / f)
  (if (setq f (open path "w"))
    (progn (write-line text f) (close f) T)
  )
)

(defun ch:boot-copy (src dst / fi fo ln ok)
  (if (setq fi (open src "r"))
    (progn
      (if (setq fo (open dst "w"))
        (progn
          (while (setq ln (read-line fi)) (write-line ln fo))
          (close fo)
          (setq ok T)
        )
      )
      (close fi)
    )
  )
  ok
)

;;; Refresh the mirror.  Each file is written beside its target and
;;; moved into place, and the fingerprint is written last, so a copy
;;; interrupted half way leaves the mirror marked stale rather than
;;; leaving a truncated module for the next drawing to load.
(defun ch:boot-refresh (home mirror fp / ok f src tmp dst)
  (setq ok T)
  (if (ch:boot-mkdir mirror)
    (progn
      (foreach f (append *ch-core-modules* *ch-report-modules*)
        (setq src (strcat home "\\" f)
              tmp (strcat mirror "\\" f ".new")
              dst (strcat mirror "\\" f))
        (if (ch:boot-copy src tmp)
          (progn
            (if (findfile dst) (vl-file-delete dst))
            (if (not (vl-file-rename tmp dst)) (setq ok nil))
          )
          (setq ok nil)
        )
      )
      (if ok (ch:boot-write1 (strcat mirror "\\fingerprint.txt") fp))
      ok
    )
  )
)

;;; Decide where to load modules from, refreshing the mirror if needed.
;;; Returns the folder to load from - the mirror when it is usable, the
;;; server when it is not.
(defun ch:boot-source (home / mirror fp)
  (setq mirror (ch:boot-mirror))
  (if (null mirror)
    home
    (progn
      (setq fp (ch:boot-fingerprint home))
      (if (= fp (ch:boot-read1 (strcat mirror "\\fingerprint.txt")))
        mirror
        (if (ch:boot-refresh home mirror fp)
          (progn
            (princ "\nCAD Hours Tracker: local copy updated.")
            mirror
          )
          home
        )
      )
    )
  )
)

(defun ch:boot-load-file (dir f / r)
  (if (findfile (strcat dir "\\" f))
    (progn
      (setq r (vl-catch-all-apply 'load (list (strcat dir "\\" f))))
      (if (vl-catch-all-error-p r)
        (progn (princ (strcat "\n** CADHours: failed to load " f)) nil)
        T
      )
    )
    (progn (princ (strcat "\n** CADHours: missing " f)) nil)
  )
)

(defun ch:boot-load ( / home ok f t0)
  (setq t0 (getvar "DATE"))
  (setq home (ch:boot-home) ok T)
  (if (null home)
    (progn
      (princ "\n** CADHours: cannot find CADHours-Core.lsp - nothing loaded.")
      (princ "\n   Set CADHOURS_HOME, or add the install folder to the support file search path.")
      nil
    )
    (progn
      (setq *ch-home*    home                    ; settings still come from here
            *ch-mod-dir* (ch:boot-source home))  ; code comes from here
      (foreach f *ch-core-modules*
        (if (not (ch:boot-load-file *ch-mod-dir* f)) (setq ok nil))
      )
      (setq *ch-t-load* (* 86400000.0 (- (getvar "DATE") t0)))
      ok
    )
  )
)

;;; Compare what actually loaded against this version and complain by
;;; name if anything is behind.  A hand-copied deployment makes a stale
;;; file easy to end up with, and the symptom is a command that has
;;; quietly disappeared.
(defun ch:boot-check ( / rep)
  (setq rep (ch:module-report))
  (if (or (car rep) (cadr rep))
    (progn
      (princ "\n** CADHours: the files in the install folder do not match.")
      (if (car rep)
        (princ (strcat "\n   Not loaded : " (ch:join (car rep) ", "))))
      (if (cadr rep)
        (princ (strcat "\n   Out of date: " (ch:join (cadr rep) ", ")
                       "   (expected " *ch-version* ")")))
      (princ "\n   Copy the whole set of .lsp files again, with AutoCAD closed.")
      (princ "\n   CHCONFIG lists them.")
      nil
    )
    T
  )
)


;;; ---- reporting, loaded on demand ----------------------------------
;;;
;;; Nothing in the reporting or dashboard modules is needed to track
;;; time, and together they are about a fifth of the code.  Keeping them
;;; out of every drawing pays for itself; the commands below pull them
;;; in the first time one is actually used, which costs a moment once
;;; per drawing rather than a moment on every drawing.

(defun ch:reports-ready ()
  (if ch:load-rows T nil)
)

(defun ch:need-reports ( / f)
  (if (ch:reports-ready)
    T
    (progn
      (princ "\nLoading reports... ")
      (foreach f *ch-report-modules* (ch:boot-load-file *ch-mod-dir* f))
      (if (ch:reports-ready)
        T
        (progn
          (princ "\n** CADHours: the reporting modules could not be loaded.")
          nil
        )
      )
    )
  )
)

;;; Each stub is replaced by the real command as the module loads, so
;;; the call below lands on the real one.
(defun c:CHREPORT   () (if (ch:need-reports) (c:CHREPORT))   (princ))
(defun c:CHTODAY    () (if (ch:need-reports) (c:CHTODAY))    (princ))
(defun c:CHWEEK     () (if (ch:need-reports) (c:CHWEEK))     (princ))
(defun c:CHJOBHOURS () (if (ch:need-reports) (c:CHJOBHOURS)) (princ))
(defun c:CHFIND     () (if (ch:need-reports) (c:CHFIND))     (princ))
(defun c:CHEXPORT   () (if (ch:need-reports) (c:CHEXPORT))   (princ))
(defun c:CHDASH     () (if (ch:need-reports) (c:CHDASH))     (princ))
(defun c:CHDASHALL  () (if (ch:need-reports) (c:CHDASHALL))  (princ))


;;; ---- per-drawing start-up ------------------------------------------

;;; Everything that happens when a drawing is ready to be worked in.
;;; Runs once per drawing, from S::STARTUP.
(defun ch:on-doc-load (safe-context / k t0 total tsweep)
  (if *ch-started*
    nil
    (progn
      (setq *ch-started* T
            t0           (getvar "DATE")
            *ch-t-ask*   0.0)
      (ch:cfg-load)
      (ch:reset-state)
      (ch:cache-settings)
      (setq k (vl-catch-all-apply 'ch:active-doc-key nil))
      (setq *ch-doc-key* (if (vl-catch-all-error-p k) "" k))

      ;; housekeeping: re-file anything a crash left behind, and push up
      ;; rows written while the share was unreachable.  Throttled - both
      ;; walk the share, and doing that on every drawing open is the
      ;; delay people actually notice.
      (if (ch:sweep-due)
        (progn
          (setq tsweep (getvar "DATE"))
          (if (ch:cfg-bool "AutoRecover" T) (ch:safe 'ch:recover-live (list nil)))
          (ch:safe 'ch:flush-spool nil)
          ;; tidy-ups that used to sit on the path a drawing closes
          ;; through, where they were costing the user a visible pause
          (ch:safe 'ch:clear-stale-locks
                   (list (ch:sessions-dir (ch:root) (ch:month-str (ch:parts)))))
          (ch:safe 'ch:migrate-headers
                   (list (ch:sessions-dir (ch:root) (ch:month-str (ch:parts)))
                         (ch:session-header)
                         (ch:month-str (ch:parts))))
          (ch:safe 'ch:sweep-done nil)
          (setq *ch-t-sweep* (* 86400000.0 (- (getvar "DATE") tsweep)))
          ;; worth saying: this is why one drawing in an hour opens
          ;; slower than the rest, and why the first one after an
          ;; update is the slowest of all
          (if (> *ch-t-sweep* 750.0)
            (ch:say (strcat "housekeeping took " (ch:fmt2 *ch-t-sweep*)
                            " ms - it runs at most every "
                            (itoa (ch:cfg-int "SweepMinutes" 60)) " minutes."))
          )
        )
      )

      (if (ch:spooling-p)
        (ch:say (strcat "Log share is not reachable - logging locally to "
                        (ch:cfg-path "LocalSpool")))
      )

      ;; A never-saved drawing may be one AutoCAD created by itself -
      ;; closing the last file can leave a fresh Drawing1 behind - and
      ;; asking which job that belongs to is a question nobody wants.
      ;; With PromptOnUnsaved = 0 it is tracked quietly and the question
      ;; waits until the user actually does something.
      ;; everything above is our own work; everything below may sit
      ;; waiting on the user
      (setq *ch-t-pre* (* 86400000.0 (- (getvar "DATE") t0)))

      (if (and (ch:cfg-bool "PromptOnOpen" T)
               (or (ch:cfg-bool "PromptOnUnsaved" T)
                   (/= (getvar "DWGTITLED") 0)))
        (ch:begin-with-prompt safe-context)
        (progn
          (ch:begin-silently)
          ;; explain the silence, so "no pop-up" is never a mystery
          (if (and (ch:cfg-bool "PromptOnOpen" T) (= (getvar "DWGTITLED") 0))
            (if (= (strcase (ch:str *ch-job*)) "UNASSIGNED")
              (ch:say (strcat "unsaved drawing - time is UNASSIGNED until you "
                              "type CHJOB, or save it into a job folder."))
              (ch:say (strcat "unsaved drawing, so no pop-up - tracking as "
                              *ch-job* ", taken from the folder path."))
            )
          )
        )
      )
      (ch:install-reactors)
      (setq total        (* 86400000.0 (- (getvar "DATE") t0))
            *ch-t-post*  (- total *ch-t-pre* *ch-t-ask*)
            *ch-t-start* (- total *ch-t-ask*))
    )
  )
  (princ)
)

;;; Ask for the job number, then start the clock.  Dismissing the
;;; dialog still starts a session - the time is banked as UNASSIGNED
;;; and CHJOB moves it onto a real job later.
;;; SAFE-CONTEXT is T only when we are somewhere a modal dialog is
;;; allowed - S::STARTUP or a command the user typed.  It is nil when we
;;; were reached from a reactor callback, where raising a modal DCL
;;; dialog is unsupported and produces a pop-up that will not accept
;;; input.  In that case the drawing is tracked quietly and the user is
;;; told to run CHJOB when it suits them.
;;; A failure here used to be swallowed: ch:safe reported it only under
;;; Debug=1, so if anything went wrong building the dialog the drawing
;;; simply opened with no pop-up and no explanation.  It now says so.
(defun ch:begin-with-prompt (safe-context / r)
  (cond
    (safe-context
      (setq r (vl-catch-all-apply 'ch:ask-job (list T)))
      (if (vl-catch-all-error-p r)
        (progn
          (ch:say (strcat "the job pop-up could not run: "
                          (vl-catch-all-error-message r)))
          (ch:say "Tracking as UNASSIGNED - CHJOB sets the job, CHDLG tests the pop-up.")
          (setq *ch-prompt-due* T)
        )
      )
      (if (not *ch-active*) (ch:start-session "UNASSIGNED" "" "" ""))
    )
    (T (ch:begin-silently))
  )
  (princ)
)

;;; PromptOnOpen=0: track quietly against the best guess
;;; Start tracking without asking.
;;;
;;; Only a job worked out FROM THIS DRAWING is accepted here.  Anything
;;; less - the last job this user happened to have open, the site
;;; default - is a suggestion for the pop-up, not a fact, and booking
;;; time against it unasked puts a wrong number in the timesheet.
;;; Without evidence the session is UNASSIGNED and the user is asked.
(defun ch:begin-silently ( / g job mgr)
  (setq g   (ch:suggest-job (ch:dwg-path))
        job (if (cadddr g) (car g) "")
        mgr (if (and (/= job "") (ch:manager-evidence job))
              (ch:suggest-manager job)
              ""))
  (ch:start-session (if (= job "") "UNASSIGNED" job)
                    (if (= job "") "" (cadr g))
                    ""
                    mgr)
  (if (= job "")
    (setq *ch-prompt-due* (ch:cfg-bool "RequireJobNumber" T))
  )
  ;; Starting without a pop-up must never be silent.  "Nothing said it
  ;; was recording" is indistinguishable from "it is not recording".
  (if (= job "")
    (ch:say "Time is being recorded as UNASSIGNED - type CHJOB to put it on a job.")
    (ch:say (strcat "Tracking time on job " job
                    " (" (caddr g) ")"
                    (if (/= mgr "") (strcat " for " mgr) "")
                    " - CHJOB changes it, CHSTATUS shows the detail."))
  )
  (princ)
)

;;; Backstop, for the case where S::STARTUP never fires - another
;;; application redefined it, or the drawing was opened in a way that
;;; skipped it.  The first command that finishes starts the tracker
;;; instead.
;;;
;;; It has to WAIT, though.  This is an editor reactor, so it is armed
;;; the moment the file loads and hears about commands application-wide
;;; - including the command that is still finishing the job of opening
;;; this very drawing.  Firing on that one meant the backstop beat
;;; S::STARTUP to it: the drawing was started without a safe context, so
;;; no pop-up could be shown, and by the time S::STARTUP arrived the
;;; work was already done and it quietly did nothing.  The second
;;; drawing of a session was the usual victim.
;;;
;;; S::STARTUP follows acaddoc.lsp within milliseconds, so a few seconds
;;; of grace is enough to tell "not yet" from "never coming".
;;;
;;; Disarming is done with a flag rather than vlr-remove: taking a
;;; reactor apart from inside its own callback is not safe, and a no-op
;;; callback costs nothing.
(defun ch:boot-guard-cb (rea args)
  (vl-catch-all-apply
    '(lambda ()
       (if (and *ch-boot-guard*
                (not *ch-started*)
                *ch-loaded-at*
                (> (abs (* 86400.0 (- (getvar "DATE") *ch-loaded-at*))) 3.0))
         (progn
           (setq *ch-boot-guard* nil)
           (ch:say "S::STARTUP did not run for this drawing - starting anyway.")
           ;; inside a reactor, so no dialog and no command line
           (ch:on-doc-load nil)
         )
       )
       ;; started normally: stand down without doing anything
       (if (and *ch-boot-guard* *ch-started*)
         (setq *ch-boot-guard* nil)
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
  (if (not *ch-started*) (ch:on-doc-load T) (ch:ask-job T))
  (princ)
)

(defun c:CHSTART ( / *error*)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
  (cond
    (*ch-active* (ch:say "Already tracking - CHSTATUS shows the detail."))
    ((not *ch-started*) (ch:on-doc-load T))
    (T (ch:ask-job T))
  )
  (princ)
)

(defun c:CHSTOP ( / *error*)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
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

(defun c:CHSTATUS ( / *error*)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
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
      (if (/= (ch:str *ch-mgr*) "")
        (princ (strcat "\n  Manager      : " *ch-mgr*)))
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
  (if (= n 0) (ch:say "Nothing to re-file."))
  (princ)
)

(defun c:CHCONFIG ( / *error* file k hit)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
  (ch:cfg-load)
  (setq file (ch:cfg-file))
  (ch:hdr "CAD Hours - settings")
  (princ (strcat "\n  Version      : " *ch-version*))
  (princ "\n  Modules      : ")
  (foreach k '("Core" "Session" "Job" "Report" "Dashboard")
    (setq hit (assoc k *ch-modules*))
    (princ (strcat "\n    " (ch:rpad k 12)
                   (cond
                     ((null hit)
                       (if (member k '("Report" "Dashboard"))
                         "loads on demand" "NOT LOADED"))
                     ((/= (cdr hit) *ch-version*)
                       (strcat (cdr hit) "  <- OUT OF DATE"))
                     (T (cdr hit)))))
  )
  (princ (strcat "\n  Loaded from  : " (if *ch-mod-dir* *ch-mod-dir* "(unknown)")
                 (if (and *ch-mod-dir* *ch-home*
                          (/= (strcase *ch-mod-dir*) (strcase *ch-home*)))
                   "   [local mirror]" "   [server]")))
  (princ (strcat "\n  Start-up     : "
                 (if *ch-t-load* (ch:fmt2 *ch-t-load*) "?") " ms loading modules"))
  (princ (strcat "\n                 "
                 (if *ch-t-pre*  (ch:fmt2 *ch-t-pre*)  "?") " ms before the pop-up"
                 ",  "
                 (if *ch-t-post* (ch:fmt2 *ch-t-post*) "?") " ms after it"))
  (princ (strcat "\n                 "
                 (if *ch-t-ask*  (ch:fmt2 *ch-t-ask*)  "0") " ms waiting for the pop-up to be answered"
                 "   (that part is not the tracker)"))
  (if *ch-t-sweep*
    (princ (strcat "\n                 " (ch:fmt2 *ch-t-sweep*)
                   " ms housekeeping, in this drawing only")))
  (princ (strcat "\n  Reports      : "
                 (if (ch:reports-ready) "loaded" "load on first use")))
  (princ (strcat "\n  Config file  : " (if file file "(none - built-in defaults)")))
  (princ (strcat "\n  Install home : " (if (ch:home) (ch:home) "(unknown)")))
  (princ (strcat "\n  Log root     : " (ch:cfg-path "LogRoot")
                 (if (ch:dir-p (ch:cfg-path "LogRoot")) "  [reachable]" "  [NOT reachable]")))
  (princ (strcat "\n  Local spool  : " (ch:cfg-path "LocalSpool")))
  (princ "\n")
  (foreach k '("IdleSeconds" "IdleCreditSeconds" "HeartbeatSeconds"
               "MinSessionSeconds" "PromptOnOpen" "PromptOnUnsaved"
               "RequireJobNumber" "RepromptSeconds" "UseDialog" "NoteLines"
               "JobPattern" "JobFromPath" "DefaultJob" "RequireManager"
               "RememberJobInDwg" "WriteEventLog" "TrackObjectEdits"
               "TrackSysVarChanges" "AutoRecover" "SweepMinutes"
               "StaleLiveHours" "Debug")
    (princ (strcat "\n  " (ch:rpad k 20) ": " (ch:cfg k)))
  )
  (princ "\n")
  (princ)
)

;;; Show the pop-up on its own and report exactly what came back,
;;; without starting, stopping or changing any session.  This is the
;;; command to run when the dialog itself is misbehaving.
(defun c:CHDLG ( / *error* res mgrs)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
  (ch:cfg-load)
  (ch:hdr "CAD Hours - dialog test")
  (setq mgrs (ch:managers))
  (princ (strcat "\n  Manager file : "
                 (if (ch:manager-file) (ch:manager-file) "(not found)")))
  (princ (strcat "\n  Names loaded : " (itoa (length mgrs))
                 (if mgrs (strcat "  -> " (ch:join mgrs ", ")) "")))
  (princ (strcat "\n  Notes panel  : " (itoa (ch:note-height)) " rows visible"))
  (princ (strcat "\n  UseDialog    : " (if (ch:cfg-bool "UseDialog" T) "1" "0")))
  (princ "\n\n  Opening the pop-up.  Fill it in and press OK, or press Skip.")

  (setq res (ch:job-dialog "" "" "" "" "dialog test - nothing will be recorded"))

  (princ (strcat "\n\n  Dialog displayed : " (if *ch-dlg-shown* "yes" "NO")))
  (princ (strcat "\n  DCL file         : "
                 (if *ch-dcl-path* *ch-dcl-path* "(none written)")))
  (if res
    (progn
      (princ "\n  Returned         : OK")
      (princ (strcat "\n    job     : \"" (ch:str (car res))    "\""))
      (princ (strcat "\n    task    : \"" (ch:str (cadr res))   "\""))
      (princ (strcat "\n    notes   : \"" (ch:str (caddr res))  "\""))
      (princ (strcat "\n    manager : \"" (ch:str (cadddr res)) "\""))
      (princ (strcat "\n    note length: " (itoa (strlen (ch:str (caddr res))))
                     " characters"))
    )
    (princ (strcat "\n  Returned         : "
                   (if *ch-dlg-shown*
                     "nothing - Skip was pressed, or OK was blocked by validation"
                     "nothing - the dialog could not be displayed at all")))
  )
  (if (and (null *ch-dlg-shown*) *ch-dcl-path*)
    (progn
      (princ "\n\n  The dialog would not display.  Open the DCL file above in")
      (princ "\n  Notepad - that is exactly what AutoCAD was handed.")
    )
  )
  (princ "\n")
  (princ)
)


(defun c:CADHOURS ( / *error*)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
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
  (princ "\n  CHDASHALL   refresh that dashboard with everything, no prompts")
  (princ "\n")
  (princ "\n  CHRECOVER   re-file sessions left behind by a crash")
  (princ "\n  CHCONFIG    show the active settings")
  (princ "\n  CHDLG       test the pop-up on its own and report what it returns")
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
          (vl-catch-all-apply 'ch:on-doc-load (list T))
          (princ)
        )
        (setq *ch-startup-hooked* T)
      )
    )

    (setq *ch-loaded-at* (getvar "DATE"))
    (ch:install-boot-guard)
    (if (ch:boot-check)
      (princ (strcat "\nCAD Hours Tracker " *ch-version*
                     " loaded.  Type CADHOURS for the command list."))
    )
  )
)

(princ)
;;; ============================================================ EOF
