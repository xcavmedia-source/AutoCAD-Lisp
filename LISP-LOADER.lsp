;;; LISP-LOADER.lsp
;;; Drop-down LISP file loader with toolbar button support.
;;;
;;; COMMANDS
;;;   LISPLOAD  - Opens a dialog listing all .lsp/.fas/.vlx files in the
;;;               configured directory; select one and click Load.
;;;   LISPDIR   - Browse to a different directory (persists for the session).
;;;
;;; TOOLBAR BUTTON (one-time setup)
;;;   1. Type CUI <Enter> in AutoCAD.
;;;   2. In "Customizations in All Files", expand Commands > Custom Commands.
;;;   3. Click "Create a new command" (the star icon).
;;;      Name:    LISP Loader
;;;      Macro:   ^C^CLISPLOAD
;;;      Image:   (pick any icon you like, e.g. RCDATA_16_APPLOAD)
;;;   4. Drag the new command onto any toolbar or ribbon panel.
;;;   5. Click OK / Apply.
;;;
;;; AUTOLOAD ON STARTUP
;;;   Add this line to your acad.lsp or acaddoc.lsp:
;;;     (load "F:\\0000-Drafting\\6-LISP_Routines\\LISP-LOADER.lsp")

(vl-load-com)

;; ── Configuration ────────────────────────────────────────────────────────────

(if (not (boundp '*LL:DIR*))
  (setq *LL:DIR* "F:\\0000-Drafting\\6-LISP_Routines")
)

;; ── Path helpers ─────────────────────────────────────────────────────────────

(defun LL:normalize (path / last)
  ;; Ensure consistent backslashes and a trailing separator.
  (setq path (vl-string-translate "/" "\\" path))
  (setq last (substr path (strlen path) 1))
  (if (not (= last "\\"))
    (setq path (strcat path "\\")))
  path
)

;; ── File scanner (recursive) ─────────────────────────────────────────────────

(defun LL:walk (dir prefix / entry full ext)
  ;; Accumulates relative paths into *LL:SCAN*.  Called recursively.
  (foreach entry (vl-directory-files dir "*")
    (if (not (member entry '("." "..")))
      (progn
        (setq full (strcat dir entry))
        (if (vl-file-directory-p full)
          ;; Sub-folder: recurse, prepending the folder name to the prefix.
          (LL:walk (strcat full "\\") (strcat prefix entry "\\"))
          ;; File: keep if it has a loadable extension.
          (progn
            (setq ext (strcase (vl-filename-extension entry)))
            (if (member ext '(".LSP" ".FAS" ".VLX" ".MNL"))
              (setq *LL:SCAN* (cons (strcat prefix entry) *LL:SCAN*))
            )
          )
        )
      )
    )
  )
)

(defun LL:scan-dir (dir / )
  ;; Returns a sorted list of relative paths (e.g. "SubFolder\tool.lsp"), or nil.
  (setq dir (LL:normalize dir))
  (setq *LL:SCAN* nil)
  (if (vl-file-directory-p dir)
    (progn
      (LL:walk dir "")
      (if *LL:SCAN* (acad-strlsort *LL:SCAN*))
    )
  )
)

;; ── Windows folder browser (Shell.Application COM) ───────────────────────────

(defun LL:browse-folder (prompt / sh fo it path)
  (setq sh (vlax-create-object "Shell.Application"))
  (if sh
    (progn
      (setq fo (vlax-invoke sh 'BrowseForFolder 0 prompt 0 *LL:DIR*))
      (if fo
        (progn
          (setq it (vlax-get-property fo 'Self))
          (setq path (vlax-get-property it 'Path))
          (vlax-release-object it)
          (vlax-release-object fo)
        )
      )
      (vlax-release-object sh)
    )
  )
  path
)

;; ── Secure loader ─────────────────────────────────────────────────────────────

(defun LL:load-file (full-path / sec-was tp dir-of result)
  ;; Temporarily sets SECURELOAD 0 and adds the directory to TRUSTEDPATHS,
  ;; then restores both after loading.  Bypasses the "always load?" prompt.
  (setq sec-was (getvar "SECURELOAD"))
  (setq tp      (getvar "TRUSTEDPATHS"))
  (setq dir-of  (vl-filename-directory full-path))

  (setvar "SECURELOAD" 0)
  (if (not (vl-string-search (strcase dir-of) (strcase tp)))
    (setvar "TRUSTEDPATHS"
      (if (= tp "") dir-of (strcat tp ";" dir-of))
    )
  )

  (setq result (vl-catch-all-apply 'load (list full-path)))

  (setvar "SECURELOAD" sec-was)

  (if (vl-catch-all-error-p result)
    (progn
      (alert (strcat "Error loading:\n" full-path
                     "\n\n" (vl-catch-all-error-message result)))
      nil
    )
    (progn
      (princ (strcat "\n>> Loaded: " full-path))
      t
    )
  )
)

;; ── DCL builder ───────────────────────────────────────────────────────────────

(defun LL:write-dcl (path / fh)
  (setq fh (open path "w"))
  (foreach line
    '("ll_dialog : dialog {"
      "  label = \"LISP File Loader\";"
      "  : text  { key = \"lbl_dir\"; label = \" \"; width = 56; }"
      "  : list_box {"
      "    key = \"lst_files\";"
      "    label = \"Select a file to load:\";"
      "    width = 56; height = 18;"
      "    allow_accept = true;"
      "    multiple_select = false;"
      "  }"
      "  spacer;"
      "  : row {"
      "    : button { label = \"&Load\";       key = \"btn_load\";   is_default = true; width = 14; }"
      "    spacer;"
      "    : button { label = \"&Change Dir\"; key = \"btn_dir\";                       width = 14; }"
      "    spacer;"
      "    : button { label = \"&Cancel\";     key = \"btn_cancel\"; is_cancel  = true; width = 14; }"
      "  }"
      "  errtile;"
      "}"
    )
    (write-line line fh)
  )
  (close fh)
)

;; ── Dialog population (called inside dialog context) ─────────────────────────

(defun LL:populate (dir / files)
  (setq files (LL:scan-dir dir))
  (set_tile "lbl_dir" (strcat " Dir: " dir))
  (start_list "lst_files")
  (if files
    (mapcar 'add_list files)
    (add_list "  [ No .lsp / .fas / .vlx files found in this directory ]")
  )
  (end_list)
  (set_tile "lst_files" "0")
  (setq *LL:FILES* files)
  (setq *LL:SEL*   0)
)

;; ── LISPLOAD command ──────────────────────────────────────────────────────────

(defun C:LISPLOAD ( / dcl-path dcl-id result fname)
  (vl-load-com)
  (setq *LL:DIR* (LL:normalize *LL:DIR*))

  ;; Write DCL to a temp file and load it.
  (setq dcl-path (vl-filename-mktemp "lldr" (getenv "TEMP") ".dcl"))
  (LL:write-dcl dcl-path)
  (setq dcl-id (load_dialog dcl-path))

  (if (< dcl-id 0)
    (progn
      (alert "LISP Loader: could not load dialog definition.")
      (vl-file-delete dcl-path)
      (exit)
    )
  )
  (if (not (new_dialog "ll_dialog" dcl-id))
    (progn
      (alert "LISP Loader: could not create dialog.")
      (unload_dialog dcl-id)
      (vl-file-delete dcl-path)
      (exit)
    )
  )

  ;; Initialise.
  (setq *LL:FILES* nil  *LL:SEL* 0)
  (LL:populate *LL:DIR*)

  ;; Tile callbacks.
  (action_tile "lst_files"
    ;; Single-click updates selection; double-click (reason 4) triggers load.
    "(setq *LL:SEL* (atoi $value)) (if (= $reason 4) (if *LL:FILES* (done_dialog 1)))"
  )
  (action_tile "btn_load"
    "(if *LL:FILES* (done_dialog 1) (done_dialog 0))"
  )
  (action_tile "btn_cancel"
    "(done_dialog 0)"
  )
  (action_tile "btn_dir"
    ;; Opens Windows folder-picker; repopulates list if a new folder is chosen.
    "(setq _nd (LL:browse-folder \"Select LISP Routines Directory\")) (if _nd (progn (setq *LL:DIR* _nd) (LL:populate _nd)))"
  )

  (setq result (start_dialog))
  (unload_dialog dcl-id)
  (vl-file-delete dcl-path)

  (if (and (= result 1) *LL:FILES*)
    (LL:load-file (strcat *LL:DIR* (nth *LL:SEL* *LL:FILES*)))
    (princ "\n>> Load cancelled.")
  )

  (princ)
)

;; ── LISPDIR command ───────────────────────────────────────────────────────────

(defun C:LISPDIR ( / nd)
  (setq nd (LL:browse-folder "Select LISP Routines Directory"))
  (if nd
    (progn
      (setq *LL:DIR* nd)
      (princ (strcat "\n>> LISP directory set to: " nd))
    )
    (princ (strcat "\n>> Directory unchanged: " *LL:DIR*))
  )
  (princ)
)

;; ── Startup message ───────────────────────────────────────────────────────────

(princ (strcat "\n>> LISP Loader ready.  Dir: " *LL:DIR*))
(princ "\n>> Commands: LISPLOAD  |  LISPDIR")
(princ)
