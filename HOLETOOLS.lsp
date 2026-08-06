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
;;; HOLEGRID  -  rebuild selected holes onto a regular X / Y grid
;;;              handles both straight and staggered patterns and
;;;              fills short columns to match the longest column
;;; ============================================================
;;;
;;; Strategy: index-then-rebuild (immune to error accumulation).
;;;
;;;   1. ROW index comes from COUNTING rows, not dividing distance.
;;;      Holes are clustered by Y (a new row starts wherever there
;;;      is a vertical gap), giving each hole a row number 0,1,2...
;;;      top to bottom.  Because the index is a count, it can never
;;;      drift the way round((anchorY-holeY)/ysp) does down a tall
;;;      sheet - that was what piled holes on top of each other.
;;;
;;;   2. ANCHOR = the top-left hole (row 0, min X).  It stays put;
;;;      every other hole is regenerated from the anchor using the
;;;      X / Y spacing you type.
;;;
;;;   3. STAGGER is detected by comparing row 0 and row 1 left edges.
;;;      If they differ by ~xsp/2 the pattern is treated as a
;;;      staggered (brick) layout: odd rows are offset by exactly
;;;      xsp/2.  Each hole keeps its own diameter (mixed sizes OK).
;;;
;;;   4. FILL - every column that has at least one hole is completed
;;;      down to the longest column, adding circles that copy the
;;;      radius + layer of the nearest existing hole in that column.
;;; ============================================================
(defun c:HOLEGRID
    (/ *error* acadobj doc space
       ss xsp ysp
       i ent obj c objs ys ytol rowreps
       row0 ax ay
       row1 row1min raw-stagger stagger staggered
       ox oy rad lyr row p basex col nx ny
       recs maxrow colkeys ckey pc pcol
       start step r found nrec bestd bestr nradius nlayer
       fx fy new-obj moved added)

  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** HOLEGRID Error: " msg))
    )
    (princ)
  )

  ;;; Return the record (p col row rad lyr) at slot P/COL/ROW, or nil.
  (defun ht:rec-at (p col row lst)
    (vl-some
      '(lambda (e)
         (if (and (= (car e) p) (= (cadr e) col) (= (caddr e) row)) e))
      lst)
  )

  ;;; --- VLA setup ------------------------------------------------
  (setq acadobj (vlax-get-acad-object)
        doc     (vla-get-activedocument acadobj)
        space   (if (and (= (getvar "TILEMODE") 0)
                         (= (getvar "CVPORT") 1))
                  (vla-get-paperspace doc)
                  (vla-get-modelspace doc)))

  ;;; --- selection ------------------------------------------------
  (princ "\nSelect holes (circles) to re-grid (ENTER when done): ")
  (setq ss (ssget '((0 . "CIRCLE"))))
  (if (null ss)
    (progn (princ "\nNothing selected - command cancelled.") (exit))
  )

  ;;; --- spacing (remembers last values) --------------------------
  (setq xsp (ht:getsp "\nX spacing, hole centre to hole centre" '*ht:last-xsp*))
  (setq ysp (ht:getsp "\nY spacing, hole centre to hole centre" '*ht:last-ysp*))

  ;;; --- gather (obj x y radius layer) ----------------------------
  (setq i 0  objs '()  ys '())
  (while (< i (sslength ss))
    (setq ent  (ssname ss i)
          obj  (vlax-ename->vla-object ent)
          c    (ht:center obj)
          objs (cons (list obj (car c) (cadr c)
                           (vlax-get-property obj 'Radius)
                           (vlax-get-property obj 'Layer))
                     objs)
          ys   (cons (cadr c) ys))
    (setq i (1+ i))
  )

  ;;; --- row indices by COUNTING (no accumulation) ----------------
  ;;; Cluster Y values into rows; a new row starts at any vertical
  ;;; gap over half the pitch.  rowreps = ordered row centroids,
  ;;; top (index 0) to bottom.  Each hole's row = nearest centroid.
  (setq ytol    (* ysp 0.5)
        rowreps (ht:clusters ys t ytol))

  ;;; --- anchor: row 0, left-most hole ----------------------------
  (setq row0 (vl-remove-if-not
               '(lambda (o) (= 0 (ht:nearest-index (caddr o) rowreps)))
               objs)
        row0 (vl-sort row0 '(lambda (a b) (< (cadr a) (cadr b))))
        ax   (cadr  (car row0))
        ay   (caddr (car row0)))

  ;;; --- stagger detection ----------------------------------------
  (setq row1 (vl-remove-if-not
               '(lambda (o) (= 1 (ht:nearest-index (caddr o) rowreps)))
               objs)
        staggered nil  stagger 0.0)
  (if row1
    (progn
      (setq row1min     (apply 'min (mapcar 'cadr row1))
            raw-stagger (- row1min ax))
      (if (< (abs (- (abs raw-stagger) (* xsp 0.5))) (* xsp 0.25))
        (setq staggered t
              stagger   (if (>= raw-stagger 0.0) (* xsp 0.5) (- (* xsp 0.5))))
      )
    )
  )

  (if staggered
    (princ (strcat "\nStaggered pattern detected (offset "
                   (ht:fmtinch (abs stagger)) ")."))
    (princ "\nStraight grid pattern.")
  )

  ;;; --- rebuild each hole from its (row,col) index ---------------
  ;;; row  = counted cluster index (immune to drift)
  ;;; p    = row parity used to pick the stagger offset
  ;;; col  = round((ox - basex) / xsp)   (local per-parity origin)
  ;;; recs = (p col row rad lyr) for every placed hole (for fill)
  (setq moved 0  recs '()  maxrow 0)
  (foreach o objs
    (setq obj (car o)
          ox  (cadr  o)
          oy  (caddr o)
          rad (cadddr o)
          lyr (nth 4 o)
          row (ht:nearest-index oy rowreps)
          p   (if staggered (rem row 2) 0)
          basex (if (= p 1) (+ ax stagger) ax)
          col   (ht:round (/ (- ox basex) xsp))
          nx    (+ basex (* col xsp))
          ny    (- ay (* row ysp)))
    (vlax-put-property obj 'Center (vlax-3d-point (list nx ny 0.0)))
    (setq recs (cons (list p col row rad lyr) recs))
    (if (> row maxrow) (setq maxrow row))
    (setq moved (1+ moved))
  )

  ;;; --- fill short columns ---------------------------------------
  ;;; Each distinct column (keyed by parity + col index) is completed
  ;;; from its parity's first row down to MAXROW.  New circles copy
  ;;; the radius + layer of the nearest existing hole in that column.
  (setq colkeys '()  added 0)
  (foreach e recs
    (setq pc (list (car e) (cadr e)))       ; (parity col)
    (if (not (member pc colkeys))
      (setq colkeys (cons pc colkeys))
    )
  )
  (foreach ckey colkeys
    (setq p     (car  ckey)
          pcol  (cadr ckey)
          start (if staggered p 0)
          step  (if staggered 2 1)
          r     start)
    (while (<= r maxrow)
      (if (not (ht:rec-at p pcol r recs))
        (progn
          ;;; nearest existing hole in this column -> radius + layer
          (setq bestd nil  nradius nil  nlayer nil)
          (foreach e recs
            (if (and (= (car e) p) (= (cadr e) pcol))
              (progn
                (setq bestr (abs (- (caddr e) r)))
                (if (or (null bestd) (< bestr bestd))
                  (setq bestd bestr  nradius (cadddr e)  nlayer (nth 4 e)))
              )
            )
          )
          (setq basex   (if (= p 1) (+ ax stagger) ax)
                fx      (+ basex (* pcol xsp))
                fy      (- ay (* r ysp))
                new-obj (vla-addcircle space
                          (vlax-3d-point (list fx fy 0.0))
                          nradius))
          (vlax-put-property new-obj 'Layer nlayer)
          (setq added (1+ added))
        )
      )
      (setq r (+ r step))
    )
  )

  (princ (strcat "\nDone - " (itoa moved) " hole(s) rebuilt on grid, "
                 (itoa added) " added to fill short columns  ("
                 (ht:fmtinch xsp) " x " (ht:fmtinch ysp) " spacing"
                 (if staggered ", staggered)." ").")))
  (princ)
)


(princ "\nHOLETOOLS.lsp loaded.")
(princ "\n  HOLESIZE  - resize circles under a threshold diameter")
(princ "\n  HOLEGRID  - snap holes onto a regular X / Y grid (straight or staggered)")
(princ)

;;; ============================================================ EOF
