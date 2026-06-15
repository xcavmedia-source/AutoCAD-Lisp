;;; ============================================================
;;; HOLETOOLS.lsp  -  Hole (Circle) Resize & Re-Grid Tools
;;;
;;; Two complementary commands for cleaning up arrays of holes
;;; that are modelled as AutoCAD CIRCLE objects:
;;;
;;;   HOLESIZE
;;;     Select circles, give a decimal threshold size, and every
;;;     circle whose diameter is UNDER that size is changed to a
;;;     new diameter you specify.  Larger circles are left alone.
;;;
;;;   HOLEGRID
;;;     Re-make the selection and give the X and Y centre-to-centre
;;;     spacing.  The routine finds the top-most / left-most hole,
;;;     groups every hole into rows and columns, and snaps each
;;;     hole to a perfectly regular grid built off that corner
;;;     hole, working in the +X (right) and -Y (down) directions.
;;;     Both straight and staggered (brick-pattern) hole layouts
;;;     are detected and handled automatically.  The last X and Y
;;;     spacing values are remembered for the session.
;;;
;;; Typical workflow:
;;;   1)  HOLESIZE  - normalise all the small holes to one diameter
;;;   2)  HOLEGRID  - normalise their centre-to-centre spacing
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- internal helpers ------------------------------------------

;;; Format VALUE as decimal inches, 4 decimal places, with " suffix
(defun ht:fmtinch (val)
  (strcat (rtos val 2 4) "\"")
)

;;; Return the (x y z) centre point of a circle VLA object as a list
(defun ht:center (obj)
  (vlax-safearray->list
    (vlax-variant-value (vlax-get-property obj 'Center)))
)

;;; Cluster a list of numbers VALS into ordered groups.
;;;   DESC = T  -> sort high-to-low (use for Y / rows, top first)
;;;   DESC = nil-> sort low-to-high (use for X / columns, left first)
;;; A new group starts whenever the gap between neighbouring values
;;; exceeds TOL.  Returns the list of group centroid values in order
;;; (group 0 first).
(defun ht:clusters (vals desc tol / sorted reps accum lastv)
  (setq sorted (vl-sort vals (if desc '> '<))
        reps   '()
        accum  '()
        lastv  nil)
  (foreach v sorted
    (cond
      ((null accum)                         ; very first value
       (setq accum (list v)))
      ((<= (abs (- v lastv)) tol)           ; close enough - same group
       (setq accum (cons v accum)))
      (t                                    ; gap - close current group
       (setq reps  (cons (/ (apply '+ accum) (float (length accum))) reps)
             accum (list v)))
    )
    (setq lastv v)
  )
  ;; flush the final group
  (if accum
    (setq reps (cons (/ (apply '+ accum) (float (length accum))) reps)))
  (reverse reps)                            ; group 0 first
)

;;; Parse a comma-separated string of decimal numbers into a list of reals.
;;; "0.5, 0.75,1.0" -> (0.5 0.75 1.0)
;;; Returns nil if STR is empty or whitespace only.
(defun ht:parse-csv (str / parts tok i ch acc result)
  (setq parts '()  acc ""  i 0)
  (while (< i (strlen str))
    (setq ch (substr str (1+ i) 1))
    (if (= ch ",")
      (progn
        (setq tok (vl-string-trim " \t" acc))
        (if (/= tok "") (setq parts (cons tok parts)))
        (setq acc "")
      )
      (setq acc (strcat acc ch))
    )
    (setq i (1+ i))
  )
  ;; flush last token
  (setq tok (vl-string-trim " \t" acc))
  (if (/= tok "") (setq parts (cons tok parts)))
  (setq result '())
  (foreach p parts
    (setq result (cons (atof p) result))
  )
  result
)

;;; Return T if value V is within TOL of any value in the list EXCL.
(defun ht:excluded-p (v excl tol)
  (vl-some '(lambda (x) (<= (abs (- v x)) tol)) excl)
)

;;; Index of the entry in REPS that is nearest to value V.
;;; Groups are separated by more than TOL, so the nearest centroid
;;; unambiguously identifies the row / column V belongs to.
(defun ht:nearest-index (v reps / i best bestd d)
  (setq i 0  best 0  bestd 1.0e99)
  (foreach r reps
    (setq d (abs (- v r)))
    (if (< d bestd) (setq bestd d  best i))
    (setq i (1+ i))
  )
  best
)


;;; ============================================================
;;; HOLESIZE  -  resize every circle under a threshold diameter
;;; ============================================================
(defun c:HOLESIZE
    (/ *error* ss thresh newdia newrad excl-str excl
       i ent obj dia changed skipped excluded)

  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** HOLESIZE Error: " msg))
    )
    (princ)
  )

  ;;; --- selection (circles only; user may window or type ALL) ----
  (princ "\nSelect circles to evaluate (ENTER when done): ")
  (setq ss (ssget '((0 . "CIRCLE"))))
  (if (null ss)
    (progn (princ "\nNothing selected - command cancelled.") (exit))
  )

  ;;; --- threshold and target diameter ----------------------------
  (initget (+ 1 2 4))   ; no null, no zero, no negative
  (setq thresh (getreal
    "\nChange every circle with a diameter UNDER (less than): "))

  (initget (+ 1 2 4))
  (setq newdia (getreal "\nNew diameter for those circles: "))
  (setq newrad (/ newdia 2.0))

  ;;; --- exclusion list -------------------------------------------
  ;;; User may press ENTER to skip; otherwise enter diameters
  ;;; separated by commas, e.g.:  0.5, 0.75, 1.0
  (princ "\nExclude specific diameters from being changed")
  (setq excl-str (getstring t
    "\n  (enter comma-separated values, or ENTER to skip): "))
  (setq excl (if (= (vl-string-trim " \t" excl-str) "")
               '()
               (ht:parse-csv excl-str)))
  (if excl
    (progn
      (princ "\nExcluding diameters: ")
      (foreach x excl (princ (strcat (ht:fmtinch x) "  ")))
    )
  )

  ;;; --- apply ----------------------------------------------------
  ;;; Tolerance for matching an exclusion: 0.0001 drawing units
  (setq i 0  changed 0  skipped 0  excluded 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent)
          dia (* 2.0 (vlax-get-property obj 'Radius)))
    (cond
      ;;; Diameter matches an exclusion entry - leave it alone
      ((ht:excluded-p dia excl 0.0001)
       (setq excluded (1+ excluded))
      )
      ;;; Diameter is under threshold - resize it
      ((< dia thresh)
       (vlax-put-property obj 'Radius newrad)
       (setq changed (1+ changed))
      )
      ;;; Diameter is at or above threshold - leave it alone
      (t
       (setq skipped (1+ skipped))
      )
    )
    (setq i (1+ i))
  )

  (princ (strcat "\nDone - "
                 (itoa changed)  " circle(s) set to "
                 (ht:fmtinch newdia) " diameter; "
                 (itoa skipped)  " above threshold (unchanged); "
                 (itoa excluded) " excluded by list (unchanged)."))
  (princ)
)


;;; Round REAL value X to the nearest integer (returns an integer).
(defun ht:round (x)
  (fix (+ x (if (>= x 0.0) 0.5 -0.5)))
)

;;; Prompt for a positive distance with an optional remembered default.
;;; PROMPT  - string shown to user
;;; LASTVAR - symbol of the global variable holding the last value (may be nil)
;;; Returns the value chosen.
(defun ht:getsp (prompt lastvar / val defval promptstr)
  (setq defval (eval lastvar))
  (if defval
    (progn
      (setq promptstr (strcat prompt " [" (rtos defval 2 4) "]: "))
      (initget (+ 2 4))   ; no zero, no negative; nil (Enter) = use default
      (setq val (getreal promptstr))
      (if (null val) (setq val defval))
    )
    (progn
      (initget (+ 1 2 4)) ; no null, no zero, no negative
      (setq val (getreal (strcat prompt ": ")))
    )
  )
  (set lastvar val)
  val
)


;;; ============================================================
;;; HOLEGRID  -  snap selected holes onto a regular X / Y grid
;;;              handles both straight and staggered patterns
;;; ============================================================
;;;
;;; Core strategy: never trust absolute X positions to assign columns.
;;; Instead, sort each row's holes left-to-right by their current X
;;; and assign column indices 0,1,2,... in that order.  This is
;;; robust even when holes are far off their ideal positions.
;;;
;;; Stagger detection: compare the leftmost-hole X of row 0 vs row 1.
;;; If they differ by ~xsp/2 the pattern is staggered and odd rows
;;; are offset by exactly xsp/2 from even rows in the output.
;;; ============================================================
(defun c:HOLEGRID
    (/ *error* ss xsp ysp
       i ent obj c objs ys ytol rowreps nrows
       ax ay row row-holes sorted-row
       row0-sorted row1-sorted stagger staggered
       col nx ny moved)

  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** HOLEGRID Error: " msg))
    )
    (princ)
  )

  ;;; --- selection ------------------------------------------------
  (princ "\nSelect holes (circles) to re-grid (ENTER when done): ")
  (setq ss (ssget '((0 . "CIRCLE"))))
  (if (null ss)
    (progn (princ "\nNothing selected - command cancelled.") (exit))
  )

  ;;; --- spacing (remembers last values) --------------------------
  (setq xsp (ht:getsp "\nX spacing, hole centre to hole centre" '*ht:last-xsp*))
  (setq ysp (ht:getsp "\nY spacing, hole centre to hole centre" '*ht:last-ysp*))

  ;;; --- gather centres -------------------------------------------
  (setq i 0  objs '()  ys '())
  (while (< i (sslength ss))
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent)
          c   (ht:center obj)
          objs (cons (list obj (car c) (cadr c)) objs)
          ys   (cons (cadr c) ys))
    (setq i (1+ i))
  )

  ;;; --- cluster rows by Y ----------------------------------------
  ;;; Only Y needs clustering; X assignment is done by sort order.
  ;;; Tolerance 45% of pitch handles misalignment up to nearly half-pitch.
  (setq ytol    (* ysp 0.45)
        rowreps (ht:clusters ys t ytol)   ; top row = index 0
        nrows   (length rowreps))

  (if (< nrows 1)
    (progn (princ "\nCould not detect any rows - command cancelled.") (exit))
  )

  ;;; --- sort each row's holes left-to-right ----------------------
  ;;; row0-sorted, row1-sorted used for stagger detection.
  ;;; All rows processed in the placement loop below.
  (defun ht:holes-in-row (ri)
    (vl-sort
      (vl-remove-if-not
        '(lambda (o) (= ri (ht:nearest-index (caddr o) rowreps)))
        objs)
      '(lambda (a b) (< (cadr a) (cadr b))))   ; sort by X
  )

  (setq row0-sorted (ht:holes-in-row 0)
        row1-sorted (if (>= nrows 2) (ht:holes-in-row 1) '()))

  ;;; --- find anchor: left-most hole of row 0 --------------------
  (setq ax (cadr  (car row0-sorted))
        ay (caddr (car row0-sorted)))

  ;;; --- stagger detection ----------------------------------------
  ;;; Compare the X of each row's leftmost hole.
  ;;; Accept as staggered when the offset is within 20% of xsp/2.
  ;;; Snap to exactly ±xsp/2 so the output is a perfect pattern.
  (setq staggered nil  stagger 0.0)
  (if row1-sorted
    (progn
      (setq stagger (- (cadr (car row1-sorted)) ax))
      (if (< (abs (- (abs stagger) (* xsp 0.5))) (* xsp 0.2))
        (progn
          (setq staggered t
                stagger   (if (>= stagger 0.0)
                            (* xsp 0.5)
                            (* xsp -0.5)))
        )
      )
    )
  )

  (if staggered
    (princ (strcat "\nStaggered pattern detected (offset "
                   (ht:fmtinch (abs stagger)) ")."))
    (princ "\nStraight grid pattern.")
  )

  ;;; --- reposition every hole ------------------------------------
  ;;; For each row, iterate its holes in left-to-right order.
  ;;; Column index = position in that sorted list (0, 1, 2, ...).
  ;;; Even rows (0,2,4...): nx = ax + col * xsp
  ;;; Odd  rows (1,3,5...): nx = ax + stagger + col * xsp
  ;;; All  rows:            ny = ay - row * ysp
  (setq moved 0  row 0)
  (while (< row nrows)
    (setq sorted-row (ht:holes-in-row row)
          col        0)
    (foreach o sorted-row
      (setq obj (car o))
      (if (and staggered (= (rem row 2) 1))
        (setq nx (+ ax stagger (* col xsp)))
        (setq nx (+ ax         (* col xsp)))
      )
      (setq ny (- ay (* row ysp)))
      (vlax-put-property obj 'Center (vlax-3d-point (list nx ny 0.0)))
      (setq col   (1+ col)
            moved (1+ moved))
    )
    (setq row (1+ row))
  )

  (princ (strcat "\nDone - " (itoa moved) " hole(s) re-gridded to "
                 (ht:fmtinch xsp) " x " (ht:fmtinch ysp)
                 " centre spacing across "
                 (itoa nrows) " row(s)"
                 (if staggered " (staggered)." ".")))
  (princ)
)


(princ "\nHOLETOOLS.lsp loaded.")
(princ "\n  HOLESIZE  - resize circles under a threshold diameter")
(princ "\n  HOLEGRID  - snap holes onto a regular X / Y grid (straight or staggered)")
(princ)

;;; ============================================================ EOF
