;;; ============================================================
;;; SEQNUM.lsp  -  Sequential Block Numbering Tool
;;;
;;; Renumbers the text (attribute) inside a grid of block
;;; references with a sequential, zero-padded value such as
;;;   P01, P02, P03, ...
;;;
;;; Workflow:
;;;   1. Run the command and select all of the blocks.
;;;   2. A dialog box appears.  Enter the prefix, the start and
;;;      end numbers, the number of digits (zero padding) and an
;;;      optional attribute tag, then press  Apply.
;;;   3. The blocks are numbered in reading order: starting at the
;;;      TOP-LEFT, moving LEFT-to-RIGHT across each row, then down
;;;      to the next row, until either the blocks run out or the
;;;      end number is reached.
;;;
;;; Notes:
;;;   - "Text inside a block" means a block ATTRIBUTE.  Only the
;;;     attribute value can differ from one instance to the next,
;;;     so the blocks must be attributed for this to make sense.
;;;   - Rows are detected automatically using the average block
;;;     height, so the grid does not have to be perfectly aligned.
;;;
;;; Command : SEQNUM
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- internal helpers ------------------------------------------

;;; Left-pad the integer NUM with zeros to a width of DIGITS.
;;;   (sb:pad 7 3)  ->  "007"
(defun sb:pad (num digits / s)
  (setq s (itoa num))
  (while (< (strlen s) digits)
    (setq s (strcat "0" s))
  )
  s
)

;;; Return the bounding-box height (Y extent) of a VLA object.
(defun sb:height (obj / mn mx)
  (vla-getboundingbox obj 'mn 'mx)
  (setq mn (vlax-safearray->list mn)
        mx (vlax-safearray->list mx))
  (abs (- (cadr mx) (cadr mn)))
)

;;; Set the value of a block reference's attribute.
;;; If TAG is "" the first attribute is used, otherwise the
;;; attribute whose tag matches TAG (case-insensitive) is used.
;;; Returns T when an attribute was written, nil otherwise.
(defun sb:setblocktext (obj txt tag / atts att done)
  (setq done nil)
  (if (= (vla-get-hasattributes obj) :vlax-true)
    (progn
      (setq atts (vlax-invoke obj 'GetAttributes))
      (foreach att atts
        (if (and (not done)
                 (or (= tag "")
                     (= (strcase (vla-get-tagstring att))
                        (strcase tag))))
          (progn
            (vla-put-textstring att txt)
            (setq done t)
          )
        )
      )
    )
  )
  done
)

;;; Sort a list of items into reading order (top-left first,
;;; left-to-right across each row, then down to the next row).
;;; Each item is the list  (obj x y).  TOL is the vertical
;;; tolerance used to group items into the same row.
(defun sb:gridsort (items tol / sorted rows cur refy it y ordered row)
  ;; 1. sort everything top-to-bottom (largest Y first)
  (setq sorted (vl-sort items
                        '(lambda (a b) (> (caddr a) (caddr b)))))
  ;; 2. walk down, breaking into rows whenever Y drops past TOL
  (setq rows '() cur '() refy nil)
  (foreach it sorted
    (setq y (caddr it))
    (cond
      ((null refy) (setq cur (list it) refy y))
      ((<= (- refy y) tol) (setq cur (cons it cur)))
      (t (setq rows (cons (reverse cur) rows)
               cur  (list it)
               refy y))
    )
  )
  (if cur (setq rows (cons (reverse cur) rows)))
  (setq rows (reverse rows))
  ;; 3. sort each row left-to-right (smallest X first) and append
  (setq ordered '())
  (foreach row rows
    (setq row (vl-sort row '(lambda (a b) (< (cadr a) (cadr b)))))
    (setq ordered (append ordered row))
  )
  ordered
)

;;; Write the DCL dialog definition to a temporary file and
;;; return its full path.
(defun sb:writedcl (/ f path)
  (setq path (vl-filename-mktemp "seqnum" nil ".dcl"))
  (setq f (open path "w"))
  (write-line "seqnum : dialog {"                                      f)
  (write-line "  label = \"Sequential Block Numbering\";"              f)
  (write-line "  : boxed_column {"                                     f)
  (write-line "    label = \"Numbering\";"                             f)
  (write-line "    : edit_box { key = \"prefix\";"                     f)
  (write-line "        label = \"Prefix:\"; edit_width = 12; }"        f)
  (write-line "    : edit_box { key = \"start\";"                      f)
  (write-line "        label = \"Start number:\"; edit_width = 12; }"  f)
  (write-line "    : edit_box { key = \"end\";"                        f)
  (write-line "        label = \"End number:\"; edit_width = 12; }"    f)
  (write-line "    : edit_box { key = \"digits\";"                     f)
  (write-line "        label = \"Digits (zero pad):\"; edit_width = 12; }" f)
  (write-line "    : edit_box { key = \"tag\";"                        f)
  (write-line "        label = \"Attribute tag (blank = first):\"; edit_width = 18; }" f)
  (write-line "  }"                                                    f)
  (write-line "  : text { key = \"info\"; }"                           f)
  (write-line "  spacer;"                                              f)
  (write-line "  : row {"                                              f)
  (write-line "    : button { key = \"accept\";"                       f)
  (write-line "        label = \"Apply\"; is_default = true; }"        f)
  (write-line "    : button { key = \"cancel\";"                       f)
  (write-line "        label = \"Cancel\"; is_cancel = true; }"        f)
  (write-line "  }"                                                    f)
  (write-line "}"                                                      f)
  (close f)
  path
)


;;; ---- main command ----------------------------------------------

(defun c:SEQNUM
    (/ *error*
       acadobj doc ss i ent obj
       items count heights tol ordered ip
       dclfile dcl_id result
       *sb-prefix* *sb-start* *sb-end* *sb-digits* *sb-tag*
       prefix start end digits tag
       num written txt it)

  ;;; Local error handler
  (defun *error* (msg)
    (if dcl_id (unload_dialog dcl_id))
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** SEQNUM Error: " msg))
    )
    (princ)
  )

  ;;; --- VLA setup ------------------------------------------------
  (setq acadobj (vlax-get-acad-object)
        doc     (vla-get-activedocument acadobj))

  ;;; --- selection ------------------------------------------------
  (princ "\nSelect blocks to number (press ENTER when done): ")
  (setq ss (ssget '((0 . "INSERT"))))

  (if (null ss)
    (progn
      (princ "\nNothing selected - command cancelled.")
      (exit)
    )
  )

  ;;; --- gather block references and their insertion points -------
  (setq items '() heights '() i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent))
    (if (= (vla-get-hasattributes obj) :vlax-true)
      (progn
        (setq ip (vlax-safearray->list
                   (vlax-variant-value (vla-get-insertionpoint obj))))
        (setq items   (cons (list obj (car ip) (cadr ip)) items)
              heights (cons (sb:height obj) heights))
      )
    )
    (setq i (1+ i))
  )

  (setq count (length items))
  (if (= count 0)
    (progn
      (princ "\nNo attributed blocks found in the selection.")
      (exit)
    )
  )

  ;;; --- row tolerance = half the average block height ------------
  (setq tol (/ (apply '+ heights) (float (length heights))))
  (setq tol (* 0.5 tol))
  (if (<= tol 0.0) (setq tol 1.0e-6))

  ;;; --- put blocks into reading order ----------------------------
  (setq ordered (sb:gridsort items tol))

  ;;; --- dialog ---------------------------------------------------
  (setq dclfile (sb:writedcl)
        dcl_id  (load_dialog dclfile))

  (if (not (new_dialog "seqnum" dcl_id))
    (progn
      (princ "\nCould not load the dialog.")
      (exit)
    )
  )

  ;; sensible defaults
  (set_tile "prefix" "P")
  (set_tile "start"  "1")
  (set_tile "end"    (itoa count))
  (set_tile "digits" "2")
  (set_tile "tag"    "")
  (set_tile "info"
            (strcat (itoa count) " attributed block(s) selected."))

  ;; Apply: copy tile values into local vars, then close with 1
  (action_tile "accept"
    (strcat "(setq prefix (get_tile \"prefix\")"
            "      start  (get_tile \"start\")"
            "      end    (get_tile \"end\")"
            "      digits (get_tile \"digits\")"
            "      tag    (get_tile \"tag\"))"
            "(done_dialog 1)"))
  (action_tile "cancel" "(done_dialog 0)")

  (setq result (start_dialog))
  (unload_dialog dcl_id)
  (setq dcl_id nil)
  (vl-file-delete dclfile)

  (if (/= result 1)
    (progn
      (princ "\nCancelled - no blocks changed.")
      (exit)
    )
  )

  ;;; --- validate / normalise the dialog input --------------------
  (setq start  (atoi start)
        end    (atoi end)
        digits (atoi digits))
  (if (< digits 1) (setq digits 1))
  (if (= prefix nil) (setq prefix ""))
  (if (= tag    nil) (setq tag    ""))
  (if (< end start) (setq end start))   ; guard against reversed range

  ;;; --- apply the sequential numbering ---------------------------
  (setq num start written 0)
  (foreach it ordered
    (if (<= num end)
      (progn
        (setq txt (strcat prefix (sb:pad num digits)))
        (if (sb:setblocktext (car it) txt tag)
          (progn
            (vla-update (car it))
            (setq written (1+ written))
          )
        )
        (setq num (1+ num))
      )
    )
  )

  (command "_.REGEN")

  (princ (strcat "\nDone - " (itoa written)
                 " block(s) renumbered ("
                 prefix (sb:pad start digits) " ... "
                 prefix (sb:pad (1- num) digits) ")."))
  (princ)
)


(princ "\nSEQNUM.lsp loaded.  Type  SEQNUM  to run.")
(princ)

;;; ============================================================ EOF
