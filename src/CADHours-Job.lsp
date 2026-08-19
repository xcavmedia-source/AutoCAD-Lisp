;;; ============================================================
;;; CADHours-Job.lsp  -  Job number capture
;;;
;;; The pop-up the user sees when a drawing opens, plus everything
;;; that works out what to put in it before they type:
;;;
;;;   1. the job this user last used for this exact drawing
;;;   2. a job number stored inside the drawing (optional)
;;;   3. a job number recognised in the folder path
;;;   4. the last job this user worked on
;;;   5. the configured DefaultJob
;;;
;;; The dialog is DCL, which is part of AutoCAD - there is nothing to
;;; install and no .NET or VBA runtime involved.
;;; ============================================================

(vl-load-com)


;;; ---- job number validation ------------------------------------------

;;; JobPattern is an AutoCAD wildcard pattern: "*" accepts anything,
;;; "##-####" forces two digits, a dash and four digits, "[A-Z]*"
;;; forces a leading letter, and so on.
(defun ch:valid-job (j / pat)
  (setq pat (ch:trim (ch:cfg "JobPattern")))
  (cond
    ((= (ch:trim j) "") nil)
    ((or (= pat "") (= pat "*")) T)
    (T (if (wcmatch (strcase j) (strcase pat)) T nil))
  )
)

(defun ch:job-hint ( / h pat)
  (setq h   (ch:trim (ch:cfg "JobPatternHint"))
        pat (ch:trim (ch:cfg "JobPattern")))
  (cond
    ((/= h "") h)
    ((or (= pat "") (= pat "*")) "")
    (T (strcat "Format: " pat))
  )
)


;;; ---- remembering a job per drawing -----------------------------------
;;;
;;; Kept in this user's registry hive, keyed by the drawing path.  The
;;; drawing itself is deliberately left untouched: writing to the
;;; database would mark a file dirty that the user only opened to
;;; look at, and they would be asked to save it on the way out.

(defun ch:dwg-key () "HKEY_CURRENT_USER\\Software\\CADHours\\Drawings")

(defun ch:dwg-remember (path job task)
  (if (and path (/= path ""))
    (vl-catch-all-apply 'vl-registry-write
      (list (ch:dwg-key)
            (strcase (ch:replace path "\\" "/"))
            (strcat (ch:str job) "|" (ch:str task))))
  )
  (princ)
)

;;; Returns (job task) remembered for PATH, or nil
(defun ch:dwg-recall (path / v parts)
  (if (and path (/= path ""))
    (progn
      (setq v (vl-catch-all-apply 'vl-registry-read
                (list (ch:dwg-key) (strcase (ch:replace path "\\" "/")))))
      (if (and (not (vl-catch-all-error-p v)) v (/= v ""))
        (progn
          (setq parts (ch:split v "|"))
          (list (ch:fld parts 0) (ch:fld parts 1))
        )
      )
    )
  )
)

;;; Optional: keep the job number inside the drawing so it follows the
;;; file between users.  Off by default because it dirties the drawing.
(defun ch:dwg-job-put (job task)
  (if (ch:cfg-bool "RememberJobInDwg" nil)
    (progn
      (vl-catch-all-apply 'vlax-ldata-put (list "CADHOURS" "JOB" (ch:str job)))
      (vl-catch-all-apply 'vlax-ldata-put (list "CADHOURS" "TASK" (ch:str task)))
    )
  )
  (princ)
)

(defun ch:dwg-job-ldata ( / v)
  (setq v (vl-catch-all-apply 'vlax-ldata-get (list "CADHOURS" "JOB")))
  (if (or (vl-catch-all-error-p v) (null v)) "" (ch:str v))
)


;;; ---- reading a job number out of the folder path ----------------------
;;;
;;; Most offices already file drawings under a job folder such as
;;; "P:\Projects\P10432 Smith Residence\CAD".  When JobFromPath is on,
;;; the first path component that contains something matching
;;; JobPattern is offered as the default, so the user usually just
;;; presses Enter.

;;; First boundary-delimited piece of COMP that is a valid job number.
;;;
;;; A candidate has to start at the front of the component or just
;;; after one of " -_.()[]", and end at the back or just before one.  That
;;; is what lets "P10432-Smith", "P10432_Smith" and "P10432 Smith" all
;;; give up P10432, while "P104321" gives up nothing - a job number has
;;; to be a whole token, not a prefix of a longer one.
;;;
;;; Candidates are tried left to right, shortest first, so a pattern
;;; that itself contains a separator (##-####) still matches P10432
;;; rather than stopping at 24.
(defun ch:job-in-token (comp / n seps i c starts ends st en cand hit)
  (setq comp (ch:trim comp)
        n    (strlen comp)
        seps " -_.()[]")
  (if (> n 0)
    (progn
      (setq i 1)
      (while (<= i n)
        (setq c (substr comp i 1))
        (if (vl-string-search c seps)
          (progn
            (if (< i n) (setq starts (cons (1+ i) starts)))
            (if (> i 1) (setq ends   (cons (1- i) ends)))
          )
        )
        (setq i (1+ i))
      )
      (setq starts (cons 1 (reverse starts))
            ends   (append (reverse ends) (list n)))
      (foreach st starts
        (if (null hit)
          (foreach en ends
            (if (and (null hit) (>= en st))
              (progn
                (setq cand (substr comp st (1+ (- en st))))
                (if (ch:valid-job cand) (setq hit cand))
              )
            )
          )
        )
      )
      hit
    )
  )
)

;;; Walk the path from the root down and return the first job number
;;; found.  Shallow-first, so the project folder wins over anything in
;;; a sub-folder or the file name.
(defun ch:job-from-path (path pattern / parts hit p)
  (if (and (ch:cfg-bool "JobFromPath" T)
           path (/= path "")
           (/= pattern "") (/= pattern "*"))
    (progn
      (setq parts (ch:split (ch:norm-path path) "\\"))
      (foreach p parts
        (if (null hit) (setq hit (ch:job-in-token p)))
      )
      hit
    )
  )
)

;;; Best guess at the job number for the drawing at PATH.
;;; Returns (job task source).
(defun ch:suggest-job (path / rec j)
  (cond
    ((setq rec (ch:dwg-recall path))
      (list (car rec) (cadr rec) "last used on this drawing"))
    ((and (ch:cfg-bool "RememberJobInDwg" nil)
          (/= (setq j (ch:dwg-job-ldata)) ""))
      (list j "" "stored in this drawing"))
    ((setq j (ch:job-from-path path (ch:trim (ch:cfg "JobPattern"))))
      (list j "" "from the folder name"))
    ((/= (setq j (ch:mru-last)) "")
      (list j "" "your last job"))
    ((/= (setq j (ch:trim (ch:cfg "DefaultJob"))) "")
      (list j "" "site default"))
    (T (list "" "" ""))
  )
)


;;; ---- the dialog --------------------------------------------------------

;;; DCL source for the pop-up.  It is written to a temporary file at
;;; run time so the tracker does not depend on AutoCAD's support file
;;; search path finding a .dcl.
(defun ch:dcl-source ()
  (list
    "cadhours_job : dialog {"
    "  label = \"CAD Hours Tracker\";"
    "  : boxed_column {"
    "    label = \"Drawing\";"
    "    : text { key = \"dwgname\"; width = 64; }"
    "    : text { key = \"dwgpath\"; width = 64; }"
    "  }"
    "  : boxed_column {"
    "    label = \"Charge this time to\";"
    "    : edit_box   { key = \"job\";    label = \"&Job number:\";  edit_width = 26; }"
    "    : popup_list { key = \"recent\"; label = \"&Recent jobs:\"; edit_width = 26; }"
    "    : edit_box   { key = \"task\";   label = \"&Task / phase:\"; edit_width = 26; }"
    "    : edit_box   { key = \"notes\";  label = \"&Notes:\";        edit_width = 44; }"
    "    : text { key = \"hint\"; width = 64; }"
    "  }"
    "  : errtile { width = 64; }"
    "  : row {"
    "    : spacer { width = 1; }"
    "    : button { key = \"accept\"; label = \"  OK  \"; is_default = true; width = 14; fixed_width = true; }"
    "    : button { key = \"cancel\"; label = \" Skip \"; is_cancel = true; width = 14; fixed_width = true; }"
    "    : spacer { width = 1; }"
    "  }"
    "}"
  )
)

;;; Write the DCL to a temp file and return its path
(defun ch:dcl-file ( / path f ln)
  (if (and *ch-dcl-path* (findfile *ch-dcl-path*))
    *ch-dcl-path*
    (progn
      (setq path (vl-filename-mktemp "cadhours" nil ".dcl"))
      (if (setq f (open path "w"))
        (progn
          (foreach ln (ch:dcl-source) (write-line ln f))
          (close f)
          (setq *ch-dcl-path* path)
        )
      )
    )
  )
)

;;; OK button.  Validates before it lets the dialog close.
(defun ch:dlg-accept ( / j)
  (setq j (ch:trim (get_tile "job")))
  (cond
    ((and (= j "") (ch:cfg-bool "RequireJobNumber" T))
      (set_tile "error" "A job number is required for time tracking."))
    ((and (/= j "") (not (ch:valid-job j)))
      (set_tile "error"
        (strcat "\"" j "\" is not a valid job number.  "
                (ch:job-hint))))
    (T
      (setq *ch-dlg-job*   j
            *ch-dlg-task*  (ch:trim (get_tile "task"))
            *ch-dlg-notes* (ch:trim (get_tile "notes")))
      (done_dialog 1)
    )
  )
  (princ)
)

;;; Show the pop-up.  Returns (job task notes), or nil when the user
;;; skipped it.  EXTRA is appended to the hint line.
;;; Sets *ch-dlg-shown* so the caller can tell "user pressed Skip"
;;; from "the dialog could not be displayed".
(defun ch:job-dialog (job task notes extra / dcl id rc rec hint)
  (setq *ch-dlg-job*    nil
        *ch-dlg-task*   nil
        *ch-dlg-notes*  nil
        *ch-dlg-shown*  nil
        rc              0
        rec             (ch:mru-get)
        hint            (ch:trim (strcat (ch:job-hint) "  " (ch:str extra))))
  (if (and (setq dcl (ch:dcl-file))
           (setq id (load_dialog dcl))
           (> id 0))
    (progn
      (if (new_dialog "cadhours_job" id)
        (progn
          (setq *ch-dlg-shown* T
                *ch-in-dialog* T)
          (set_tile "dwgname" (strcat "  " (ch:str (getvar "DWGNAME"))))
          (set_tile "dwgpath" (strcat "  " (ch:str (ch:dwg-path))))
          (set_tile "job"     (ch:str job))
          (set_tile "task"    (ch:str task))
          (set_tile "notes"   (ch:str notes))
          (set_tile "hint"    (strcat "  " hint))

          (start_list "recent")
          (if rec (mapcar 'add_list rec) (add_list "(none yet)"))
          (end_list)

          (action_tile "recent"
            "(if rec (set_tile \"job\" (nth (atoi $value) rec)))")
          (action_tile "accept" "(ch:dlg-accept)")
          (action_tile "cancel" "(done_dialog 0)")
          (mode_tile "job" 2)                    ; put the caret in the job box

          (setq rc (start_dialog))
          (setq *ch-in-dialog* nil)
        )
      )
      (unload_dialog id)
    )
  )
  (if (and (= rc 1) *ch-dlg-job*)
    (list *ch-dlg-job* (ch:str *ch-dlg-task*) (ch:str *ch-dlg-notes*))
  )
)

;;; Command-line fallback, for the rare case where the dialog cannot
;;; be shown.  Never called from a reactor.
(defun ch:job-getstring (job / s)
  (setq s (getstring T
            (strcat "\nJob number for this drawing"
                    (if (/= job "") (strcat " <" job ">") "")
                    (if (/= (ch:job-hint) "") (strcat "  [" (ch:job-hint) "]") "")
                    ": ")))
  (setq s (ch:trim s))
  (if (= s "") (setq s job))
  (if (ch:valid-job s) (list s "" "") nil)
)


;;; ---- asking, and acting on the answer ----------------------------------

;;; Show the pop-up and apply whatever comes back.
;;; ALLOW-CLI lets the command-line fallback run; pass nil when the
;;; call originates in a reactor.
;;; Returns T when a job number was captured.
(defun ch:ask-job (allow-cli / path guess res)
  (setq path  (ch:dwg-path)
        guess (ch:suggest-job path))
  (if (and *ch-active* (/= *ch-job* "") (/= (strcase *ch-job*) "UNASSIGNED"))
    (setq guess (list *ch-job* *ch-task* "the running session"))
  )
  (setq *ch-last-prompt* (ch:now))
  (setq res (ch:job-dialog (car guess) (cadr guess) *ch-notes*
                           (if (= (caddr guess) "")
                             ""
                             (strcat "Suggested job comes from " (caddr guess) "."))))
  ;; the dialog could not be shown at all - fall back to the prompt
  (if (and (null res) (null *ch-dlg-shown*) allow-cli)
    (setq res (ch:job-getstring (car guess)))
  )
  (cond
    (res
      (ch:set-job (car res) (cadr res) (caddr res))
      (ch:dwg-remember path (car res) (cadr res))
      (ch:say (strcat "Tracking time on job " (car res)
                      " - type CHSTATUS at any time."))
      T
    )
    (T
      (setq *ch-prompt-due* (ch:cfg-bool "RequireJobNumber" T))
      (if *ch-prompt-due*
        (ch:say "No job number yet - time is being held as UNASSIGNED.  Type CHJOB to assign it.")
        (ch:say "Job number skipped.")
      )
      nil
    )
  )
)

;;; Called after every command ends.  Nags for a job number, but only
;;; every RepromptSeconds so it never becomes unusable.
(defun ch:maybe-reprompt ()
  (if (and *ch-prompt-due*
           (not *ch-in-dialog*)
           *ch-current*
           (ch:cfg-bool "RequireJobNumber" T)
           (> (ch:secs *ch-last-prompt* (ch:now))
              (float (ch:cfg-int "RepromptSeconds" 120))))
    (ch:safe 'ch:ask-job (list nil))
  )
  (princ)
)


(princ)
;;; ============================================================ EOF
