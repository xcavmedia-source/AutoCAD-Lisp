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


;;; Best guess at the project manager for JOB.
(defun ch:suggest-manager (job / m)
  (cond
    ((and *ch-active* (/= (ch:str *ch-mgr*) "")) *ch-mgr*)
    ((setq m (ch:job-mgr-recall job)) m)
    (T (ch:mgr-last))
  )
)


;;; ---- the dialog --------------------------------------------------------

;;; ---- guarded tile access ------------------------------------------------
;;;
;;; get_tile and set_tile raise an error when handed a key the loaded
;;; dialog does not contain, and an error thrown inside an action
;;; expression leaves the dialog on screen but unresponsive - the OK
;;; button appears to do nothing.  Everything below goes through these
;;; so a tile mismatch degrades to an empty value instead.

(defun ch:tile-get (key / v)
  (setq v (vl-catch-all-apply 'get_tile (list key)))
  (if (vl-catch-all-error-p v) "" (ch:str v))
)

(defun ch:tile-set (key val)
  (vl-catch-all-apply 'set_tile (list key (ch:str val)))
  (princ)
)

(defun ch:tile-mode (key mode)
  (vl-catch-all-apply 'mode_tile (list key mode))
  (princ)
)

;;; Fill a list_box or popup_list.
;;;
;;; start_list raises on an unknown key exactly as get_tile does, but it
;;; does so during setup - before start_dialog - so an unguarded call
;;; abandons a dialog that is loaded but never shown, leaks the handle
;;; and leaves *ch-in-dialog* latched.  end_list is issued outside the
;;; protected region so the list is never left half-built.
(defun ch:list-fill (key items / r it)
  (setq r (vl-catch-all-apply 'start_list (list key)))
  (if (vl-catch-all-error-p r)
    nil
    (progn
      (foreach it items
        (vl-catch-all-apply 'add_list (list (ch:str it)))
      )
      (vl-catch-all-apply 'end_list nil)
      T
    )
  )
)


;;; ---- the notes panel ----------------------------------------------------
;;;
;;; DCL has no multi-line edit tile; it is simply not in the tile set, and
;;; adding one would mean .NET and an installer.  What DCL does have is a
;;; list_box, which is a single framed panel that scrolls - so the note is
;;; shown there as wrapped lines, which reads as one large box rather than
;;; a stack of separate fields.
;;;
;;; Typing goes through the line below it (Enter appends), and the
;;; Notepad button hands the whole note to a real text editor for
;;; anything substantial.  The note itself is held in *ch-note-lines*
;;; while the dialog is up; the list_box is only the view of it.

(defun ch:note-height ()
  (max 2 (min 12 (ch:cfg-int "NoteLines" 4)))
)

;;; Must not exceed the list_box width below, or wrapped lines are
;;; clipped where the user cannot see them.
(defun ch:note-width () 58)

;;; Break TEXT into display lines of at most WIDTH, on spaces.  COUNT
;;; caps the number of lines; anything past it is crammed into the last.
(defun ch:wrap-note (text count width / words out line w n)
  (setq words (ch:split (ch:trim (ch:str text)) " ")
        out   '()
        line  "")
  (foreach w words
    (cond
      ((= w "") nil)
      ((= line "") (setq line w))
      ((<= (+ (strlen line) 1 (strlen w)) width)
        (setq line (strcat line " " w)))
      (T (setq out (cons line out)) (setq line w))
    )
  )
  (if (/= line "") (setq out (cons line out)))
  (setq out (reverse out))
  (if (and count (> (length out) count))
    (progn
      (setq n    (- count 1)
            line (ch:join (ch:nthcdr n out) " ")
            out  (append (ch:firstn out n) (list line)))
    )
  )
  out
)

(defun ch:firstn (lst n / out)
  (while (and lst (> n 0))
    (setq out (cons (car lst) out)
          lst (cdr lst)
          n   (1- n))
  )
  (reverse out)
)

(defun ch:nthcdr (n lst)
  (while (and lst (> n 0)) (setq lst (cdr lst) n (1- n)))
  lst
)

;;; The note as one value, which is what gets logged
(defun ch:note-text ()
  (ch:join (vl-remove "" *ch-note-lines*) " ")
)

;;; Repaint the panel from *ch-note-lines*
(defun ch:note-refresh ()
  (ch:list-fill "notelist" (if *ch-note-lines* *ch-note-lines* (list "")))
  (princ)
)

;;; Load TEXT into the panel, wrapped
(defun ch:note-load (text)
  (setq *ch-note-lines* (ch:wrap-note text nil (ch:note-width)))
  (ch:note-refresh)
  (princ)
)

;;; Enter in the add line, or focus leaving it, appends what was typed
(defun ch:dlg-note-add ( / v)
  (setq v (ch:trim (ch:tile-get "noteadd")))
  (if (/= v "")
    (progn
      (setq *ch-note-lines*
            (ch:wrap-note (ch:trim (strcat (ch:note-text) " " v))
                          nil (ch:note-width)))
      (ch:tile-set "noteadd" "")
      (ch:note-refresh)
    )
  )
  (princ)
)

;;; Clicking a line lifts it out of the note and into the add box, so it
;;; can be corrected and put back
(defun ch:dlg-note-pick (idx / n out i ln)
  (setq n (atoi (ch:str idx)) i 0 out '())
  (if (and *ch-note-lines* (< n (length *ch-note-lines*)))
    (progn
      (ch:tile-set "noteadd" (nth n *ch-note-lines*))
      (foreach ln *ch-note-lines*
        (if (/= i n) (setq out (cons ln out)))
        (setq i (1+ i))
      )
      (setq *ch-note-lines* (reverse out))
      (ch:note-refresh)
      (ch:tile-mode "noteadd" 2)
    )
  )
  (princ)
)

(defun ch:dlg-note-clear ()
  (setq *ch-note-lines* nil)
  (ch:tile-set "noteadd" "")
  (ch:note-refresh)
  (princ)
)

;;; Notepad button: remember what is on screen and close with code 3, so
;;; the caller can run the editor and bring the dialog straight back.
(defun ch:dlg-notepad ()
  (ch:dlg-note-add)
  (setq *ch-dlg-job*   (ch:trim (ch:tile-get "job"))
        *ch-dlg-task*  (ch:trim (ch:tile-get "task"))
        *ch-dlg-mgr*   (ch:read-manager)
        *ch-dlg-notes* (ch:note-text))
  (done_dialog 3)
  (princ)
)

;;; Hand TEXT to Notepad and return whatever comes back.
;;;
;;; WScript.Shell's Run waits for the editor to close when asked to, so
;;; the note can be read straight back.  Without it there is no way to
;;; wait, so the user is asked to confirm at the command line instead.
(defun ch:notepad-edit (text / path f sh r lines ln)
  (setq path (vl-filename-mktemp "cadnote" nil ".txt"))
  (if (setq f (open path "w"))
    (progn
      (foreach ln (ch:wrap-note text nil 76) (write-line ln f))
      (if (= (ch:trim (ch:str text)) "") (write-line "" f))
      (close f)

      (setq sh (vl-catch-all-apply 'vlax-create-object (list "WScript.Shell")))
      (if (vl-catch-all-error-p sh)
        (setq r (vl-catch-all-error-p sh))
        (progn
          (setq r (vl-catch-all-apply 'vlax-invoke
                    (list sh 'Run (strcat "notepad.exe \"" path "\"") 1 :vlax-true)))
          (vl-catch-all-apply 'vlax-release-object (list sh))
          (setq r (vl-catch-all-error-p r))
        )
      )
      ;; could not wait for the editor - ask the user to tell us instead
      (if r
        (progn
          (startapp "notepad.exe" path)
          (getstring "\nType the notes in Notepad, save and close it, then press Enter: ")
        )
      )

      (setq lines (ch:read-lines path))
      (vl-file-delete path)
      (ch:join (vl-remove "" (mapcar 'ch:trim (if lines lines '()))) " ")
    )
    text
  )
)


;;; ---- the dialog definition ----------------------------------------------

(defun ch:dcl-source ()
  (list
    "cadhours_job : dialog {"
    "  label = \"CAD Hours Tracker\";"
    "  : text { key = \"dwgname\"; width = 64; }"
    "  : text { key = \"dwgpath\"; width = 64; }"
    "  : spacer { height = 0.2; }"
    "  : boxed_column {"
    "    label = \"Charge this time to\";"
    "    : edit_box   { key = \"job\";     label = \"&Job number:\";      edit_width = 26; edit_limit = 64; allow_accept = true; }"
    "    : popup_list { key = \"recent\";  label = \"&Recent jobs:\";     edit_width = 26; }"
    "    : popup_list { key = \"manager\"; label = \"Project &manager:\"; edit_width = 26; }"
    "    : edit_box   { key = \"task\";    label = \"&Task / phase:\";    edit_width = 26; edit_limit = 64; }"
    "    : text { key = \"hint\"; width = 64; }"
    "  }"
    "  : boxed_column {"
    "    label = \"Notes (optional)\";"
    (strcat "    : list_box { key = \"notelist\"; width = 62; height = "
            (itoa (ch:note-height)) "; }")
    "    : row {"
    "      : edit_box { key = \"noteadd\"; label = \"Add:\"; edit_width = 34; edit_limit = 250; }"
    "      : button { key = \"notepad\"; label = \" &Notepad... \"; fixed_width = true; }"
    "      : button { key = \"noteclear\"; label = \" Clear \"; fixed_width = true; }"
    "    }"
    "    : text { width = 62; label = \"  Enter adds a line.  Click a line to edit it.\"; }"
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

;;; Write the DCL to a temp file and return its path.  Rebuilt whenever
;;; the panel height changes.
(defun ch:dcl-file ( / path f ln)
  (if (and *ch-dcl-path* (findfile *ch-dcl-path*)
           (equal *ch-dcl-lines* (ch:note-height)))
    *ch-dcl-path*
    (progn
      (setq path (vl-filename-mktemp "cadhours" nil ".dcl"))
      (if (setq f (open path "w"))
        (progn
          (foreach ln (ch:dcl-source) (write-line ln f))
          (close f)
          (setq *ch-dcl-lines* (ch:note-height))
          (setq *ch-dcl-path* path)
          (ch:dbg (strcat "dialog definition written to " path))
          *ch-dcl-path*
        )
      )
    )
  )
)

;;; The manager the dropdown is currently showing, "" for "(none)"
(defun ch:read-manager ( / i)
  (setq i (atoi (ch:tile-get "manager")))
  (if (and (> i 0) *ch-mgr-list*) (nth i *ch-mgr-list*) "")
)

;;; OK button.  Validates before it lets the dialog close.
(defun ch:dlg-accept ( / j m)
  (ch:dlg-note-add)                     ; do not lose a half-typed line
  (setq j (ch:trim (ch:tile-get "job"))
        m (ch:read-manager))
  (cond
    ((and (= j "") (ch:cfg-bool "RequireJobNumber" T))
      (ch:tile-set "error" "A job number is required for time tracking."))
    ((and (/= j "") (not (ch:valid-job j)))
      (ch:tile-set "error"
        (strcat "\"" j "\" is not a valid job number.  " (ch:job-hint))))
    ((and (= m "") (ch:cfg-bool "RequireManager" nil))
      (ch:tile-set "error" "Please choose the project manager for this job."))
    (T
      (setq *ch-dlg-job*   j
            *ch-dlg-task*  (ch:trim (ch:tile-get "task"))
            *ch-dlg-mgr*   m
            *ch-dlg-notes* (ch:note-text))
      (done_dialog 1)
    )
  )
  (princ)
)

;;; Show the pop-up once.  Returns the start_dialog code: 1 = OK,
;;; 0 = Skip, 3 = the user asked for Notepad, nil = could not display.
(defun ch:job-dialog-once (job task notes mgr extra / dcl id rc rec hint mgrs i)
  (setq *ch-dlg-shown* nil
        rc             nil
        rec            (ch:mru-get)
        mgrs           (ch:managers)
        *ch-mgr-list*  (cons "" mgrs)
        hint           (ch:trim (strcat (ch:job-hint) "  " (ch:str extra))))
  (if (and (setq dcl (ch:dcl-file))
           (setq id (load_dialog dcl))
           (> id 0))
    (progn
      (if (new_dialog "cadhours_job" id)
        (progn
          (setq *ch-dlg-shown* T
                *ch-in-dialog* T)
          (ch:tile-set "dwgname" (strcat "  " (ch:str (getvar "DWGNAME"))))
          (ch:tile-set "dwgpath" (strcat "  " (ch:str (ch:dwg-path))))
          (ch:tile-set "job"  job)
          (ch:tile-set "task" task)
          (ch:tile-set "hint" (strcat "  " hint))

          (ch:list-fill "recent" (if rec rec (list "(none yet)")))
          (ch:list-fill "manager"
            (cons (if mgrs "(none)" "(no managers.txt found)") mgrs))
          (setq i (ch:index-of mgr *ch-mgr-list*))
          (ch:tile-set "manager" (itoa (if i i 0)))

          (ch:note-load notes)

          (action_tile "recent"
            "(if rec (ch:tile-set \"job\" (nth (atoi $value) rec)))")
          (action_tile "noteadd"   "(ch:dlg-note-add)")
          (action_tile "notelist"  "(ch:dlg-note-pick $value)")
          (action_tile "notepad"   "(ch:dlg-notepad)")
          (action_tile "noteclear" "(ch:dlg-note-clear)")
          (action_tile "accept"    "(ch:dlg-accept)")
          (action_tile "cancel"    "(done_dialog 0)")
          (ch:tile-mode "job" 2)               ; put the caret in the job box

          (setq rc (start_dialog))
          (setq *ch-in-dialog* nil)
        )
      )
      (unload_dialog id)
    )
  )
  rc
)

;;; Show the pop-up, looping back if the user goes out to Notepad.
;;; Returns (job task notes manager), or nil when they skipped.
(defun ch:job-dialog (job task notes mgr extra / rc guard)
  (setq guard 0)
  (setq rc (ch:job-dialog-once job task notes mgr extra))
  ;; exhausting the loop must not discard what the user typed: the last
  ;; round's values are still stashed, so treat it as an accept
  (while (and (= rc 3) (< guard 20))
    (setq guard (1+ guard))
    ;; keep whatever they had typed, and take the note out to the editor
    (setq job   (ch:str *ch-dlg-job*)
          task  (ch:str *ch-dlg-task*)
          mgr   (ch:str *ch-dlg-mgr*)
          notes (ch:notepad-edit (ch:str *ch-dlg-notes*)))
    (setq rc (ch:job-dialog-once job task notes mgr extra))
  )
  (if (and (or (= rc 1) (= rc 3)) *ch-dlg-job*)
    (list *ch-dlg-job* (ch:str *ch-dlg-task*)
          (ch:str *ch-dlg-notes*) (ch:str *ch-dlg-mgr*))
  )
)

;;; Position of VALUE in LST, case-insensitive, or nil
(defun ch:index-of (value lst / i hit n)
  (setq value (strcase (ch:trim (ch:str value))) i 0)
  (if (/= value "")
    (foreach n lst
      (if (and (null hit) (= (strcase (ch:str n)) value)) (setq hit i))
      (setq i (1+ i))
    )
  )
  hit
)


;;; ---- command-line prompt ------------------------------------------------
;;;
;;; Used when the dialog cannot be shown, and selectable outright with
;;; UseDialog = 0.  It asks for everything the dialog does, so turning
;;; the dialog off is a real alternative rather than a degraded one.
;;;
;;; Never called from a reactor: getstring would interfere with whatever
;;; the user is in the middle of.

(defun ch:pick-manager (mgr / mgrs i pick n)
  (setq mgrs (ch:managers))
  (cond
    ((null mgrs) (ch:str mgr))
    (T
      (princ "\n  Project managers:")
      (setq i 1)
      (foreach n mgrs
        (princ (strcat "\n    " (itoa i) " = " n))
        (setq i (1+ i))
      )
      (setq pick (ch:trim
                   (getstring (strcat "\n  Number"
                                      (if (/= (ch:str mgr) "")
                                        (strcat " <" mgr ">")
                                        " <none>")
                                      ": "))))
      (cond
        ((= pick "") (ch:str mgr))
        ((and (> (atoi pick) 0) (<= (atoi pick) (length mgrs)))
          (nth (1- (atoi pick)) mgrs))
        ;; a typed name is accepted if it is on the list
        ((ch:known-manager pick))
        (T (ch:say "Not on the list - left unset.") "")
      )
    )
  )
)

;;; Returns (job task notes manager) or nil
(defun ch:job-cli (job mgr / j task notes m)
  (princ "\n")
  (setq j (ch:trim (getstring T
            (strcat "\nJob number"
                    (if (/= (ch:str job) "") (strcat " <" job ">") "")
                    (if (/= (ch:job-hint) "") (strcat "  [" (ch:job-hint) "]") "")
                    ": "))))
  (if (= j "") (setq j (ch:str job)))
  (cond
    ((not (ch:valid-job j))
      (ch:say (strcat "\"" j "\" is not a valid job number.  " (ch:job-hint)))
      nil)
    (T
      (setq m     (ch:pick-manager mgr)
            task  (ch:trim (getstring T "\n  Task / phase <none>: "))
            notes (ch:trim (getstring T "\n  Notes <none>: ")))
      (if (and (= m "") (ch:cfg-bool "RequireManager" nil))
        (progn (ch:say "A project manager is required.") nil)
        (list j task notes m)
      )
    )
  )
)

;;; ---- asking, and acting on the answer ----------------------------------

;;; Show the pop-up and apply whatever comes back.
;;; ALLOW-CLI lets the command-line fallback run; pass nil when the
;;; call originates in a reactor.
;;; Returns T when a job number was captured.
(defun ch:ask-job (allow-cli / path guess mgr res)
  (setq path  (ch:dwg-path)
        guess (ch:suggest-job path))
  (if (and *ch-active* (/= *ch-job* "") (/= (strcase *ch-job*) "UNASSIGNED"))
    (setq guess (list *ch-job* *ch-task* "the running session"))
  )
  (setq mgr (ch:suggest-manager (car guess)))
  ;; the flag is cleared here as well as after the dialog, because an
  ;; error inside start_dialog's action expressions would otherwise
  ;; leave it latched and silence the reminder for good
  (setq *ch-in-dialog* nil)

  ;; UseDialog = 0 skips the pop-up entirely.  Otherwise try the dialog
  ;; and drop to the prompt if it could not be put on screen.
  (if (ch:cfg-bool "UseDialog" T)
    (setq res (ch:job-dialog (car guess) (cadr guess) *ch-notes* mgr
                             (if (= (caddr guess) "")
                               ""
                               (strcat "Suggested job comes from "
                                       (caddr guess) "."))))
  )
  (if (and (null res)
           (or (null *ch-dlg-shown*) (not (ch:cfg-bool "UseDialog" T)))
           allow-cli)
    (setq res (ch:job-cli (car guess) mgr))
  )
  ;; stamped on the way OUT, not on the way in.  Stamping it before the
  ;; dialog meant a pop-up left on screen longer than RepromptSeconds
  ;; had already used up its own quiet period, so pressing Skip brought
  ;; it straight back on the next command.
  (setq *ch-last-prompt* (ch:now))

  (cond
    (res
      (ch:set-job (car res) (cadr res) (caddr res) (cadddr res))
      (ch:dwg-remember path (car res) (cadr res))
      (ch:say (strcat "Tracking time on job " (car res)
                      (if (= (ch:str (cadddr res)) "")
                        ""
                        (strcat " for " (cadddr res)))
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

;;; Called after every command ends.
;;;
;;; This runs inside a reactor callback, and a modal DCL dialog must not
;;; be raised from one: reactors keep firing while the dialog is up, the
;;; state is unsupported, and it is what produced a pop-up that appeared
;;; but would not accept input.  It also fired on things the user would
;;; never call an action - pressing Escape, a pan, a plot, a script
;;; line, or one of the tracker's own report commands.
;;;
;;; So nothing is opened here.  The user is reminded on the command
;;; line, at most once every RepromptSeconds, and types CHJOB when it
;;; suits them.  Time is still being recorded as UNASSIGNED meanwhile,
;;; so nothing is lost by them finishing what they were doing first.
(defun ch:maybe-reprompt ( / wait)
  ;; the cheap flags first: this runs after every single command, and
  ;; ch:cfg-int can fall through to reading the settings file
  (if (and *ch-prompt-due*
           *ch-active*
           (not *ch-in-dialog*)
           (ch:current-p)
           (ch:cfg-bool "RequireJobNumber" T)
           (setq wait (float (max 30 (ch:cfg-int "RepromptSeconds" 120))))
           (> (ch:secs *ch-last-prompt* (ch:now)) wait))
    (progn
      (setq *ch-last-prompt* (ch:now))
      (princ (strcat "\n[CADHours] This drawing's time is still UNASSIGNED"
                     " - type CHJOB to put it on a job."))
      (princ)
    )
  )
  (princ)
)


(ch:module "Job" "1.2.1")

(princ)
;;; ============================================================ EOF
