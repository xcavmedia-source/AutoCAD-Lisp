;;; ============================================================
;;; PERF.lsp  -  Auto Perforation Pattern Generator
;;;
;;; Fills a closed boundary with a repeating perforation pattern.
;;;
;;; Workflow:
;;;   1. Choose a hole SHAPE  : Circle, Square, Rectangle,
;;;                             Hexagon, Diamond
;;;   2. Choose the SIZE      : one value (Rectangle asks two)
;;;                             - Circle   = diameter
;;;                             - Square   = side length
;;;                             - Rectangle= length x width
;;;                             - Hexagon  = across flats
;;;                             - Diamond  = point-to-point width
;;;   3. Choose the SPACING   : center-to-center distance
;;;   4. Choose the ANGLE     : Straight, 30, 45 or 60 degrees
;;;   All values are in inches.
;;;
;;;   The command REMEMBERS the last pattern (this drawing session
;;;   AND across sessions via the registry).  When run again you can
;;;   [Continue] with the stored pattern or [Redefine] it.
;;;
;;;   Fill area is acquired either by selecting a closed LWPOLYLINE
;;;   boundary, or by picking the lower-left and upper-right corners
;;;   of a rectangular window.
;;;
;;;   The pattern is centered in the boundary and NO hole is allowed
;;;   to cross outside it.
;;;
;;;   Orientation check (staggered 30/60 patterns only):
;;;   The continuous STRAIGHT rows should run along the LONGEST side
;;;   of the boundary, leaving the staggered edge on the short side.
;;;   If the straight rows would land on the short side the command
;;;   PAUSES, explains why, and offers:
;;;       [Continue] - override and keep the current orientation
;;;       [Fix]      - automatically swap 30 <-> 60 to reorient
;;;
;;; Command : PERF
;;; Requires: AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)

;;; ---- persistent setting defaults --------------------------------
;;; Held in globals during a session; mirrored to the registry so
;;; the pattern survives between drawings.
(setq *perf-cfgroot* "AppData/PerfPattern/")

;;; Read all stored settings from the registry into the globals
;;; (only when the globals are not already populated).
(defun pf:loadcfg ( / g)
  (if (null *perf-shape*)
    (progn
      (if (setq g (getcfg (strcat *perf-cfgroot* "Shape")))   (setq *perf-shape*   g))
      (if (setq g (getcfg (strcat *perf-cfgroot* "Size")))    (setq *perf-size*    (atof g)))
      (if (setq g (getcfg (strcat *perf-cfgroot* "Size2")))   (setq *perf-size2*   (atof g)))
      (if (setq g (getcfg (strcat *perf-cfgroot* "Spacing"))) (setq *perf-spacing* (atof g)))
      (if (setq g (getcfg (strcat *perf-cfgroot* "Angle")))   (setq *perf-angle*   g))
    )
  )
  (princ)
)

;;; Write the current globals back to the registry.
(defun pf:savecfg ()
  (setcfg (strcat *perf-cfgroot* "Shape")   *perf-shape*)
  (setcfg (strcat *perf-cfgroot* "Size")    (rtos *perf-size* 2 8))
  (setcfg (strcat *perf-cfgroot* "Size2")   (rtos (cond (*perf-size2*) (0.0)) 2 8))
  (setcfg (strcat *perf-cfgroot* "Spacing") (rtos *perf-spacing* 2 8))
  (setcfg (strcat *perf-cfgroot* "Angle")   *perf-angle*)
  (princ)
)

;;; True when a complete pattern is already stored.
(defun pf:havecfg ()
  (and *perf-shape* *perf-size* *perf-spacing* *perf-angle*)
)

;;; ---- geometry helpers -------------------------------------------

;;; Point-in-polygon test (ray casting).  PT is (x y ..),
;;; POLY is a list of (x y) points.  Returns T when inside.
(defun pf:ptinpoly (pt poly / x y n i j xi yi xj yj inside)
  (setq x (car pt)  y (cadr pt)
        n (length poly)
        inside nil
        i 0  j (1- n))
  (while (< i n)
    (setq xi (car (nth i poly)) yi (cadr (nth i poly))
          xj (car (nth j poly)) yj (cadr (nth j poly)))
    (if (and (not (eq (> yi y) (> yj y)))
             (< x (+ (/ (* (- xj xi) (- y yi)) (- yj yi)) xi)))
      (setq inside (not inside))
    )
    (setq j i  i (1+ i))
  )
  inside
)

;;; Return (minx miny maxx maxy) bounding box of a point list.
(defun pf:bbox (pts / minx miny maxx maxy)
  (setq minx (car  (car pts))  maxx minx
        miny (cadr (car pts))  maxy miny)
  (foreach p pts
    (setq minx (min minx (car  p))  maxx (max maxx (car  p))
          miny (min miny (cadr p))  maxy (max maxy (cadr p)))
  )
  (list minx miny maxx maxy)
)

;;; Local hole vertices (row-aligned frame, centered on origin) as a
;;; list of (x y) points.  Returns NIL for a circle (drawn directly).
(defun pf:localverts (shape size size2 / h hw hh hd r ang pts k)
  (cond
    ((= shape "Circle") nil)
    ((= shape "Square")
     (setq h (* 0.5 size))
     (list (list (- h) (- h)) (list h (- h)) (list h h) (list (- h) h)))
    ((= shape "Rectangle")
     (setq hw (* 0.5 size)  hh (* 0.5 size2))
     (list (list (- hw) (- hh)) (list hw (- hh)) (list hw hh) (list (- hw) hh)))
    ((= shape "Diamond")
     (setq hd (* 0.5 size))
     (list (list hd 0.0) (list 0.0 hd) (list (- hd) 0.0) (list 0.0 (- hd))))
    ((= shape "Hexagon")
     ;; size = across flats -> circumradius R = F / sqrt(3)
     (setq r (/ size (sqrt 3.0))  pts '()  k 0)
     (while (< k 6)
       (setq ang (* k (/ pi 3.0)))
       (setq pts (cons (list (* r (cos ang)) (* r (sin ang))) pts))
       (setq k (1+ k)))
     (reverse pts))
  )
)

;;; Circumradius of a hole - used as a cushion when bounding the lattice.
(defun pf:circumr (shape size size2)
  (cond
    ((= shape "Circle")    (* 0.5 size))
    ((= shape "Square")    (* 0.5 size (sqrt 2.0)))
    ((= shape "Rectangle") (* 0.5 (sqrt (+ (* size size) (* size2 size2)))))
    ((= shape "Diamond")   (* 0.5 size))
    ((= shape "Hexagon")   (/ size (sqrt 3.0)))
    (T (* 0.5 size))
  )
)

;;; Lattice description for an angle keyword.
;;; Returns (Ux Uy Vx Vy rowAngle) where U is the in-row step,
;;; V the row-to-row step and rowAngle the direction holes align to.
(defun pf:lattice (angle s / c s60 c60)
  (setq c   (* s (cos (/ pi 4.0)))   ; S*cos45 = S*sin45
        s60 (* s (sin (/ pi 3.0)))   ; S*sin60 = S*0.8660
        c60 (* s 0.5))               ; S*cos60 = S*0.5
  (cond
    ;; square grid, rows along X
    ((= angle "Straight") (list s 0.0  0.0 s  0.0))
    ;; square grid rotated 45 (diamond layout)
    ((= angle "45")       (list c c  (- c) c  (/ pi 4.0)))
    ;; staggered, straight rows along X
    ((= angle "60")       (list s 0.0  c60 s60  0.0))
    ;; staggered, straight rows along Y (90 deg rotation of the 60 pattern)
    ((= angle "30")       (list 0.0 s  s60 c60  (/ pi 2.0)))
    (T (list s 0.0  0.0 s  0.0))
  )
)

;;; ---- drawing -----------------------------------------------------

;;; Create one hole entity centered at (cx cy).
(defun pf:drawhole (shape size size2 cx cy rowang lverts / ca sa verts)
  (if (= shape "Circle")
    (entmake (list '(0 . "CIRCLE")
                   (list 10 cx cy 0.0)
                   (cons 40 (* 0.5 size))))
    (progn
      (setq ca (cos rowang)  sa (sin rowang))
      (setq verts
            (mapcar
              (lambda (p / lx ly)
                (setq lx (car p)  ly (cadr p))
                (list (+ cx (- (* lx ca) (* ly sa)))
                      (+ cy (+ (* lx sa) (* ly ca)))))
              lverts))
      (entmake
        (append
          (list '(0 . "LWPOLYLINE")
                '(100 . "AcDbEntity")
                '(100 . "AcDbPolyline")
                (cons 90 (length verts))
                '(70 . 1))                 ; closed
          (mapcar (lambda (p) (cons 10 p)) verts)))
    )
  )
)

;;; All test points for the hole footprint at (cx cy); every one must
;;; lie inside the boundary or the hole is rejected.
(defun pf:footprint (shape size cx cy rowang lverts / r pts a k ca sa)
  (if (= shape "Circle")
    (progn
      (setq r (* 0.5 size)  pts '()  k 0)
      (while (< k 16)
        (setq a (* k (/ pi 8.0)))
        (setq pts (cons (list (+ cx (* r (cos a))) (+ cy (* r (sin a)))) pts))
        (setq k (1+ k)))
      pts)
    (progn
      (setq ca (cos rowang)  sa (sin rowang))
      (mapcar
        (lambda (p / lx ly)
          (setq lx (car p)  ly (cadr p))
          (list (+ cx (- (* lx ca) (* ly sa)))
                (+ cy (+ (* lx sa) (* ly ca)))))
        lverts))
  )
)

;;; ---- interactive pattern setup ----------------------------------

(defun pf:setup ( / tmp)
  ;; SHAPE
  (initget "Circle Square Rectangle Hexagon Diamond")
  (setq tmp (getkword
              (strcat "\nHole shape [Circle/Square/Rectangle/Hexagon/Diamond] <"
                      (cond (*perf-shape*) ("Circle")) ">: ")))
  (if tmp (setq *perf-shape* tmp)
          (if (null *perf-shape*) (setq *perf-shape* "Circle")))

  ;; SIZE (one or two values)
  (cond
    ((= *perf-shape* "Rectangle")
     (initget 6)  ; no zero, no negative
     (setq tmp (getdist (strcat "\nHole length (in) <"
                                (rtos (cond (*perf-size*) (1.0)) 2 4) ">: ")))
     (if tmp (setq *perf-size* tmp) (if (null *perf-size*) (setq *perf-size* 1.0)))
     (initget 6)
     (setq tmp (getdist (strcat "\nHole width (in) <"
                                (rtos (cond (*perf-size2*) (0.5)) 2 4) ">: ")))
     (if tmp (setq *perf-size2* tmp) (if (null *perf-size2*) (setq *perf-size2* 0.5))))
    (T
     (initget 6)
     (setq tmp (getdist (strcat "\nHole size (in) <"
                                (rtos (cond (*perf-size*) (0.5)) 2 4) ">: ")))
     (if tmp (setq *perf-size* tmp) (if (null *perf-size*) (setq *perf-size* 0.5))))
  )

  ;; SPACING
  (initget 6)
  (setq tmp (getdist (strcat "\nSpacing center-to-center (in) <"
                             (rtos (cond (*perf-spacing*) (1.0)) 2 4) ">: ")))
  (if tmp (setq *perf-spacing* tmp) (if (null *perf-spacing*) (setq *perf-spacing* 1.0)))

  ;; ANGLE
  (initget "Straight 30 45 60")
  (setq tmp (getkword (strcat "\nPattern angle [Straight/30/45/60] <"
                              (cond (*perf-angle*) ("60")) ">: ")))
  (if tmp (setq *perf-angle* tmp) (if (null *perf-angle*) (setq *perf-angle* "60")))

  (pf:savecfg)
  (princ)
)

;;; ---- boundary acquisition ---------------------------------------
;;; Returns a list of (x y) polygon vertices, or NIL on cancel.

(defun pf:getboundary ( / m ss en ed verts p1 p2 mnx mny mxx mxy)
  (initget "Select Window")
  (setq m (getkword "\nFill area by [Select boundary/Window corners] <Select>: "))
  (if (null m) (setq m "Select"))
  (cond
    ((= m "Select")
     (princ "\nSelect a single closed polyline boundary: ")
     (setq ss (ssget ":S" '((0 . "LWPOLYLINE"))))
     (if (null ss)
       (progn (princ "\nNothing selected.") nil)
       (progn
         (setq en (ssname ss 0)  ed (entget en))
         (if (/= 1 (logand 1 (cdr (assoc 70 ed))))
           (progn (princ "\nThat polyline is not closed.") nil)
           (progn
             ;; group 10 of an LWPOLYLINE is a 2D point (x y)
             (setq verts '())
             (foreach pr ed
               (if (= 10 (car pr))
                 (setq verts (cons (list (car (cdr pr)) (cadr (cdr pr))) verts))))
             (reverse verts)
           )
         )
       )
     )
    )
    ((= m "Window")
     (setq p1 (getpoint "\nLower-left corner of fill area: "))
     (if (null p1) nil
       (progn
         (setq p2 (getcorner p1 "\nUpper-right corner of fill area: "))
         (if (null p2) nil
           (progn
             (setq mnx (min (car p1) (car p2))  mxx (max (car p1) (car p2))
                   mny (min (cadr p1) (cadr p2)) mxy (max (cadr p1) (cadr p2)))
             (list (list mnx mny) (list mxx mny) (list mxx mxy) (list mnx mxy))
           )
         )
       )
     )
    )
  )
)

;;; ---- main command ------------------------------------------------

(defun c:PERF
    (/ *error* poly bb width height cx cy
       lat ux uy vx vy rowang
       longIsX rowsAlongX rowsOnLong dec
       lverts cushion halfdiag nsteps i j px py count
       drawn ans needsetup)

  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** PERF Error: " msg)))
    (princ)
  )

  (pf:loadcfg)

  ;; --- decide whether to reuse or redefine the pattern ------------
  (setq needsetup T)
  (if (pf:havecfg)
    (progn
      (princ (strcat "\nCurrent pattern: " *perf-shape*
                     "  size=" (rtos *perf-size* 2 4)
                     (if (= *perf-shape* "Rectangle")
                       (strcat " x " (rtos (cond (*perf-size2*) (0.0)) 2 4)) "")
                     "  spacing=" (rtos *perf-spacing* 2 4)
                     "  angle=" *perf-angle*))
      (initget "Continue Redefine")
      (setq ans (getkword "\nUse this pattern? [Continue/Redefine] <Continue>: "))
      (if (or (null ans) (= ans "Continue")) (setq needsetup nil))
    )
  )
  (if needsetup (pf:setup))

  ;; --- get the fill boundary --------------------------------------
  (setq poly (pf:getboundary))
  (if (null poly)
    (progn (princ "\nNo fill area - command cancelled.") (exit)))

  (setq bb     (pf:bbox poly)
        width  (- (caddr bb) (car bb))
        height (- (cadddr bb) (cadr bb))
        cx     (* 0.5 (+ (car bb) (caddr bb)))
        cy     (* 0.5 (+ (cadr bb) (cadddr bb))))

  ;; --- orientation check (staggered 30/60 only) -------------------
  (if (member *perf-angle* '("30" "60"))
    (progn
      (setq longIsX    (>= width height)
            rowsAlongX (= *perf-angle* "60")           ; 60 -> rows run along X
            rowsOnLong (or (and longIsX rowsAlongX)
                           (and (not longIsX) (not rowsAlongX))))
      (if (not rowsOnLong)
        (progn
          (princ "\n")
          (princ "\n*** ORIENTATION WARNING ***")
          (princ "\nThe continuous STRAIGHT rows are currently running along the")
          (princ "\nSHORT side of the boundary.  Best practice is to run the straight")
          (princ "\nrows along the LONGEST side, leaving the staggered edge on the")
          (princ "\nshort side (stronger sheet, less waste, cleaner edge).")
          (princ (strcat "\n  Boundary: " (rtos width 2 3) " (X) x "
                         (rtos height 2 3) " (Y)"))
          (princ (strcat "\n  Angle " *perf-angle*
                         " runs straight rows along the "
                         (if rowsAlongX "X" "Y") " axis (short side)."))
          (princ "\n  [Continue] = override, keep this orientation.")
          (princ "\n  [Fix]      = swap 30<->60 to put straight rows on the long side.")
          (initget "Continue Fix")
          (setq dec (getkword "\nChoose [Continue/Fix] <Fix>: "))
          (if (or (null dec) (= dec "Fix"))
            (progn
              (setq *perf-angle* (if (= *perf-angle* "30") "60" "30"))
              (pf:savecfg)
              (princ (strcat "\nReoriented - angle is now " *perf-angle* "."))
            )
            (princ "\nContinuing with the current orientation (override).")
          )
        )
      )
    )
  )

  ;; --- build lattice & local hole geometry ------------------------
  (setq lat    (pf:lattice *perf-angle* *perf-spacing*)
        ux (nth 0 lat) uy (nth 1 lat)
        vx (nth 2 lat) vy (nth 3 lat)
        rowang (nth 4 lat)
        lverts  (pf:localverts *perf-shape* *perf-size* *perf-size2*)
        cushion (pf:circumr *perf-shape* *perf-size* *perf-size2*))

  ;; lattice index range: enough steps to cover the diagonal + cushion
  (setq halfdiag (+ (* 0.5 (sqrt (+ (* width width) (* height height))))
                    *perf-spacing* cushion)
        nsteps   (+ 2 (fix (/ halfdiag *perf-spacing*))))

  ;; --- generate, clip, draw ---------------------------------------
  (setq count 0  i (- nsteps))
  (while (<= i nsteps)
    (setq j (- nsteps))
    (while (<= j nsteps)
      (setq px (+ cx (* i ux) (* j vx))
            py (+ cy (* i uy) (* j vy)))
      ;; quick bbox reject before the full footprint test
      (if (and (>= px (- (car bb) cushion)) (<= px (+ (caddr bb) cushion))
               (>= py (- (cadr bb) cushion)) (<= py (+ (cadddr bb) cushion)))
        (if (vl-every
              (function (lambda (tp) (pf:ptinpoly tp poly)))
              (pf:footprint *perf-shape* *perf-size* px py rowang lverts))
          (progn
            (pf:drawhole *perf-shape* *perf-size* *perf-size2* px py rowang lverts)
            (setq count (1+ count))
          )
        )
      )
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )

  (princ (strcat "\nDone - " (itoa count) " "
                 *perf-shape* " hole(s) placed."))
  (princ)
)

(princ "\nPERF.lsp loaded.  Type  PERF  to run.")
(princ)

;;; ============================================================ EOF
