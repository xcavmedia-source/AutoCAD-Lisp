;;; ============================================================
;;; PERF.lsp  -  Auto Perforation Pattern Generator
;;;
;;; Fills a closed boundary with a repeating perforation pattern.
;;;
;;; Workflow:
;;;   1. Choose a hole SHAPE  : Circle, Square, Rectangle,
;;;                             Hexagon, Diamond, Slot
;;;   2. Choose the SIZE      : one value (Rectangle/Slot ask two)
;;;                             - Circle   = diameter
;;;                             - Square   = side length
;;;                             - Rectangle= length x width
;;;                             - Hexagon  = across flats
;;;                             - Diamond  = point-to-point width
;;;                             - Slot     = overall length x width
;;;                                          (obround, round end caps)
;;;   3. Choose the SPACING   : center-to-center distance
;;;   4. Choose the ANGLE     : Straight, 30, 45 or 60 degrees
;;;   All values are in inches.
;;;
;;;   The command REMEMBERS the last pattern (this drawing session
;;;   AND across sessions via the registry).  When run again you can
;;;   [Continue] with the stored pattern or [Redefine] it.
;;;
;;;   Fill area is acquired either by:
;;;     - selecting ANY closed boundary (polyline w/ arcs, circle,
;;;       ellipse, spline, region-edge polyline ...), or
;;;     - picking the lower-left and upper-right corners of a window.
;;;
;;;   The pattern is centered in the boundary and NO hole is allowed
;;;   to cross outside it.
;;;
;;;   Angled boundary: if the boundary has slanted edges the command
;;;   offers to ALIGN the perforation rows to a boundary edge you
;;;   pick, so the pattern follows that angle.
;;;
;;;   Orientation check (staggered 30/60 patterns, axis-aligned only):
;;;   The continuous STRAIGHT rows should run along the LONGEST side
;;;   of the boundary, leaving the staggered edge on the short side.
;;;   If they would land on the short side the command PAUSES with:
;;;       [Continue] - override, keep the current orientation
;;;       [Fix]      - automatically swap 30 <-> 60 to reorient
;;;
;;; Command : PERF
;;; Requires: AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)

;;; ---- persistent setting defaults --------------------------------
(setq *perf-cfgroot* "AppData/PerfPattern/")

(defun pf:loadcfg ( / g)
  (if (null *perf-shape*)
    (progn
      (if (setq g (getcfg (strcat *perf-cfgroot* "Shape")))   (setq *perf-shape*   g))
      (if (setq g (getcfg (strcat *perf-cfgroot* "Size")))    (setq *perf-size*    (atof g)))
      (if (setq g (getcfg (strcat *perf-cfgroot* "Size2")))   (setq *perf-size2*   (atof g)))
      (if (setq g (getcfg (strcat *perf-cfgroot* "Spacing"))) (setq *perf-spacing* (atof g)))
      (if (setq g (getcfg (strcat *perf-cfgroot* "Angle")))   (setq *perf-angle*   g))
      (if (setq g (getcfg (strcat *perf-cfgroot* "Method")))  (setq *perf-method*  g))
      (if (setq g (getcfg (strcat *perf-cfgroot* "Thick")))   (setq *perf-thick*   (atof g)))
      ;; row pitch: stored 0 means Auto
      (if (setq g (getcfg (strcat *perf-cfgroot* "RowPitch")))
        (setq *perf-rowpitch* (if (> (atof g) 0.0) (atof g) nil)))
    )
  )
  (princ)
)

(defun pf:savecfg ()
  (setcfg (strcat *perf-cfgroot* "Shape")   *perf-shape*)
  (setcfg (strcat *perf-cfgroot* "Size")    (rtos *perf-size* 2 8))
  (setcfg (strcat *perf-cfgroot* "Size2")   (rtos (cond (*perf-size2*) (0.0)) 2 8))
  (setcfg (strcat *perf-cfgroot* "Spacing") (rtos *perf-spacing* 2 8))
  (setcfg (strcat *perf-cfgroot* "Angle")   *perf-angle*)
  (setcfg (strcat *perf-cfgroot* "Method")  (cond (*perf-method*) ("Select")))
  (setcfg (strcat *perf-cfgroot* "Thick")   (rtos (cond (*perf-thick*) (0.0)) 2 8))
  (setcfg (strcat *perf-cfgroot* "RowPitch")(rtos (cond (*perf-rowpitch*) (0.0)) 2 8))
  (princ)
)

(defun pf:havecfg ()
  (and *perf-shape* *perf-size* *perf-spacing* *perf-angle*)
)

;;; ---- geometry helpers -------------------------------------------

;;; Point-in-polygon test (ray casting). PT is (x y ..), POLY a list
;;; of (x y) points.  Returns T when inside.  Walks the list with
;;; foreach (O(n)); the previous nth-indexed loop was O(n^2) and froze
;;; on densely sampled boundaries.
(defun pf:ptinpoly (pt poly / x y inside xi yi xj yj prev)
  (setq x (car pt)  y (cadr pt)
        inside nil  prev (last poly))
  (foreach v poly
    (setq xi (car v) yi (cadr v)
          xj (car prev) yj (cadr prev))
    (if (and (not (eq (> yi y) (> yj y)))
             (< x (+ (/ (* (- xj xi) (- y yi)) (- yj yi)) xi)))
      (setq inside (not inside)))
    (setq prev v))
  inside
)

;;; T when every point of PTS lies inside POLY.  (Explicit loop -
;;; avoids vl-every / (function (lambda ...)) which is unreliable
;;; on some LISP builds and caused a "bad function" error.)
(defun pf:allinside (pts poly / ok)
  (setq ok T)
  (while (and ok pts)
    (if (not (pf:ptinpoly (car pts) poly)) (setq ok nil))
    (setq pts (cdr pts)))
  ok
)

;;; (minx miny maxx maxy) bounding box of a (x y) point list.
(defun pf:bbox (pts / minx miny maxx maxy)
  (setq minx (car (car pts)) maxx minx
        miny (cadr (car pts)) maxy miny)
  (foreach p pts
    (setq minx (min minx (car p))  maxx (max maxx (car p))
          miny (min miny (cadr p)) maxy (max maxy (cadr p))))
  (list minx miny maxx maxy)
)

;;; Distance from point P to segment A-B (all (x y)).
(defun pf:segdist (p a b / ax ay bx by px py dx dy l2 tt qx qy)
  (setq ax (car a) ay (cadr a) bx (car b) by (cadr b)
        px (car p) py (cadr p)
        dx (- bx ax) dy (- by ay)
        l2 (+ (* dx dx) (* dy dy)))
  (if (<= l2 1e-12)
    (distance (list px py) (list ax ay))
    (progn
      (setq tt (/ (+ (* (- px ax) dx) (* (- py ay) dy)) l2)
            tt (max 0.0 (min 1.0 tt))
            qx (+ ax (* tt dx)) qy (+ ay (* tt dy)))
      (sqrt (+ (* (- px qx) (- px qx)) (* (- py qy) (- py qy))))))
)

;;; Local hole vertices (row-aligned frame, centered on origin).
;;; NIL for a circle (drawn directly).
(defun pf:localverts (shape size size2 / h hw hh hd r ang pts k)
  (cond
    ((= shape "Circle") nil)
    ((= shape "Square")
     (setq h (* 0.5 size))
     (list (list (- h) (- h)) (list h (- h)) (list h h) (list (- h) h)))
    ((= shape "Rectangle")
     (setq hw (* 0.5 size) hh (* 0.5 size2))
     (list (list (- hw) (- hh)) (list hw (- hh)) (list hw hh) (list (- hw) hh)))
    ((= shape "Diamond")
     (setq hd (* 0.5 size))
     (list (list hd 0.0) (list 0.0 hd) (list (- hd) 0.0) (list 0.0 (- hd))))
    ((= shape "Hexagon")
     ;; Vertices offset 30 deg so the FLAT sides face the six lattice
     ;; neighbors (honeycomb orientation, pointy-top).  With vertices
     ;; at 0/60/... the points face the neighbors and adjacent holes
     ;; interlock into a star pattern once size approaches spacing.
     (setq r (/ size (sqrt 3.0)) pts '() k 0)
     (while (< k 6)
       (setq ang (+ (* k (/ pi 3.0)) (/ pi 6.0)))
       (setq pts (cons (list (* r (cos ang)) (* r (sin ang))) pts))
       (setq k (1+ k)))
     (reverse pts))
    ((= shape "Slot")
     ;; Obround: size = overall length, size2 = width.  Sampled
     ;; outline (cap arcs in 30-deg steps) for the clip tests; the
     ;; drawn entity uses true arc segments instead of these points.
     (setq hd (- (* 0.5 size) (* 0.5 size2))  ; cap center offset
           r  (* 0.5 size2)
           pts '() k -3)
     (while (<= k 3)                          ; right cap -90..+90
       (setq ang (* k (/ pi 6.0)))
       (setq pts (cons (list (+ hd (* r (cos ang))) (* r (sin ang))) pts))
       (setq k (1+ k)))
     (setq k 3)
     (while (<= k 9)                          ; left cap +90..+270
       (setq ang (* k (/ pi 6.0)))
       (setq pts (cons (list (+ (- hd) (* r (cos ang))) (* r (sin ang))) pts))
       (setq k (1+ k)))
     (reverse pts))
  )
)

(defun pf:circumr (shape size size2)
  (cond
    ((= shape "Circle")    (* 0.5 size))
    ((= shape "Square")    (* 0.5 size (sqrt 2.0)))
    ((= shape "Rectangle") (* 0.5 (sqrt (+ (* size size) (* size2 size2)))))
    ((= shape "Diamond")   (* 0.5 size))
    ((= shape "Hexagon")   (/ size (sqrt 3.0)))
    ((= shape "Slot")      (* 0.5 size))
    (T (* 0.5 size))
  )
)

;;; Lattice description (Ux Uy Vx Vy rowAngle) for an angle keyword.
;;; U = step along a row, V = step to the next row, rowAngle = hole
;;; alignment.  S is the along-row pitch, T the row-to-row pitch; the
;;; two are independent so long holes can sit close together across
;;; rows without forcing a huge gap along them.  With T at its default
;;; (S * sin60 for 30/60, S for straight/45) these reduce exactly to
;;; the classic equilateral / square patterns.
(defun pf:lattice (angle s tt / c d)
  (setq c (* s  (cos (/ pi 4.0)))
        d (* tt (cos (/ pi 4.0))))
  (cond
    ((= angle "Straight") (list s 0.0  0.0 tt  0.0))
    ((= angle "45")       (list c c  (- d) d  (/ pi 4.0)))
    ((= angle "60")       (list s 0.0  (* 0.5 s) tt  0.0))
    ((= angle "30")       (list 0.0 s  tt (* 0.5 s)  (/ pi 2.0)))
    (T (list s 0.0  0.0 tt  0.0))
  )
)

;;; Hole extent across the rows divided by its extent along them.
;;; 1.0 for the symmetric shapes, < 1 for rectangles and slots.
(defun pf:aspect (shape size size2)
  (if (and (member shape '("Rectangle" "Slot")) size2 (> size 0.0))
    (/ size2 size)
    1.0)
)

;;; Row pitch that reproduces the standard pattern proportions.
;;; Scaling by the hole's aspect keeps long holes packed as tightly
;;; across the rows as round ones are, instead of leaving the row gap
;;; driven by the hole's length.
(defun pf:defaultrow (shape size size2 angle s)
  (* s
     (pf:aspect shape size size2)
     (if (member angle '("30" "60")) (sin (/ pi 3.0)) 1.0))
)

;;; ---- drawing -----------------------------------------------------

(defun pf:xform (lx ly cx cy ca sa)
  (list (+ cx (- (* lx ca) (* ly sa)))
        (+ cy (+ (* lx sa) (* ly ca))))
)

(defun pf:drawhole (shape size size2 cx cy rowang lverts / ca sa verts pr lst
                    a w2)
  (cond
    ((= shape "Circle")
     (entmake (list '(0 . "CIRCLE") (list 10 cx cy 0.0) (cons 40 (* 0.5 size)))))
    ((= shape "Slot")
     ;; Obround drawn with true semicircular end caps (bulge = 1).
     (setq a  (- (* 0.5 size) (* 0.5 size2))
           w2 (* 0.5 size2)
           ca (cos rowang) sa (sin rowang))
     (entmake
       (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
             '(90 . 4) '(70 . 1)
             (cons 10 (pf:xform (- a) (- w2) cx cy ca sa)) '(42 . 0.0)
             (cons 10 (pf:xform a     (- w2) cx cy ca sa)) '(42 . 1.0)
             (cons 10 (pf:xform a     w2     cx cy ca sa)) '(42 . 0.0)
             (cons 10 (pf:xform (- a) w2     cx cy ca sa)) '(42 . 1.0))))
    (T
     (progn
      (setq ca (cos rowang) sa (sin rowang) verts '())
      (foreach p lverts
        (setq verts (cons (pf:xform (car p) (cadr p) cx cy ca sa) verts)))
      (setq verts (reverse verts)
            lst (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                      '(100 . "AcDbPolyline") (cons 90 (length verts)) '(70 . 1)))
      (foreach p verts (setq lst (append lst (list (cons 10 p)))))
      (entmake lst))
    )
  )
)

;;; Footprint test points for the hole at (cx cy).
(defun pf:footprint (shape size cx cy rowang lverts / r pts a k ca sa)
  (if (= shape "Circle")
    (progn
      (setq r (* 0.5 size) pts '() k 0)
      (while (< k 16)
        (setq a (* k (/ pi 8.0)))
        (setq pts (cons (list (+ cx (* r (cos a))) (+ cy (* r (sin a)))) pts))
        (setq k (1+ k)))
      pts)
    (progn
      (setq ca (cos rowang) sa (sin rowang) pts '())
      (foreach p lverts
        (setq pts (cons (pf:xform (car p) (cadr p) cx cy ca sa) pts)))
      pts)
  )
)

;;; ---- minimum web (bar) checking ---------------------------------
;;; The web is the clear edge-to-edge material left between adjacent
;;; holes.  Industry practice is that it must be at least as thick as
;;; the material, or the sheet tears / distorts when punched.
;;;
;;; Two holes of a centrally symmetric shape K whose centers differ by
;;; D are separated by exactly the distance from the point D to the
;;; shape scaled 2x.  That identity gives the true clear gap for any
;;; shape and any lattice angle, so one routine covers 30/45/60 and
;;; straight alike.

;;; Distance from point P to convex polygon VERTS (0.0 when inside).
(defun pf:polygap (p verts / n i a b d best)
  (if (pf:ptinpoly p verts)
    0.0
    (progn
      (setq n (length verts) best 1e30 i 0)
      (while (< i n)
        (setq a (nth i verts) b (nth (rem (1+ i) n) verts)
              d (pf:segdist p a b))
        (if (< d best) (setq best d))
        (setq i (1+ i)))
      best)
  )
)

;;; Clear gap between two holes whose centers differ by P, with P
;;; expressed in the hole's own (unrotated) frame.  VERTS2 is the
;;; local outline scaled 2x (unused for circles and slots, which are
;;; handled exactly).
(defun pf:pairgap (shape size size2 p verts2 / a r)
  (cond
    ((= shape "Circle")
     (- (distance p '(0.0 0.0)) size))
    ((= shape "Slot")
     ;; 2x obround: cap centers at +/-2a, radius 2r
     (setq a (- (* 0.5 size) (* 0.5 size2))
           r (* 0.5 size2))
     (- (pf:segdist p (list (* -2.0 a) 0.0) (list (* 2.0 a) 0.0))
        (* 2.0 r)))
    (T (pf:polygap p verts2))
  )
)

;;; Smallest clear web across neighboring holes in the lattice.
;;; Checks every lattice combination within +/-2 steps, which always
;;; contains the closest neighbors.  Because the holes and the lattice
;;; rotate together, working in the hole's local frame makes this
;;; independent of any edge-follow rotation.
;;; MODE 0 = every neighbor, 1 = along-row only (set by S),
;;;      2 = other rows only (set by T).
(defun pf:webscan (shape size size2 s tt angle mode
                   / lat ux uy vx vy ra ca sa lv v2 i j dx dy lx ly g best)
  (setq lat (pf:lattice angle s tt)
        ux (nth 0 lat) uy (nth 1 lat)
        vx (nth 2 lat) vy (nth 3 lat)
        ra (nth 4 lat) ca (cos ra) sa (sin ra)
        lv (pf:localverts shape size size2)
        v2 '())
  (if lv
    (foreach p lv
      (setq v2 (cons (list (* 2.0 (car p)) (* 2.0 (cadr p))) v2))))
  (setq v2 (reverse v2))
  (setq best 1e30 i -2)
  (while (<= i 2)
    (setq j -2)
    (while (<= j 2)
      (if (and (not (and (= i 0) (= j 0)))
               (or (= mode 0)
                   (and (= mode 1) (= j 0))
                   (and (= mode 2) (/= j 0))))
        (progn
          (setq dx (+ (* i ux) (* j vx))
                dy (+ (* i uy) (* j vy))
                lx (+ (* dx ca) (* dy sa))     ; rotate by -rowangle
                ly (- (* dy ca) (* dx sa))
                g  (pf:pairgap shape size size2 (list lx ly) v2))
          (if (< g best) (setq best g))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  best
)

(defun pf:minweb (shape size size2 s tt angle)
  (pf:webscan shape size size2 s tt angle 0)
)

;;; Smallest along-row pitch whose web meets THICK.  The web grows
;;; monotonically with the pitch, so bisection is exact; results are
;;; rounded UP to 4 decimals so they truly satisfy.
(defun pf:minalong (shape size size2 angle thick / s2 lo hi mid k)
  (setq s2 (cond (size2) (0.0))
        lo 1e-6
        hi (max 1e-3 (+ size s2 thick))
        k  0)
  (while (and (< (pf:webscan shape size s2 hi hi angle 1) thick) (< k 60))
    (setq hi (* hi 2.0) k (1+ k)))
  (setq k 0)
  (while (< k 40)
    (setq mid (* 0.5 (+ lo hi)))
    (if (< (pf:webscan shape size s2 mid mid angle 1) thick)
      (setq lo mid)
      (setq hi mid))
    (setq k (1+ k)))
  (/ (float (fix (+ (* hi 10000.0) 0.9999))) 10000.0)
)

;;; Smallest row pitch whose web meets THICK, for a fixed along-row
;;; pitch S.  Same monotonicity, same rounding.
(defun pf:minrow (shape size size2 s angle thick / s2 lo hi mid k)
  (setq s2 (cond (size2) (0.0))
        lo 1e-6
        hi (max 1e-3 (+ size s2 thick))
        k  0)
  (while (and (< (pf:webscan shape size s2 s hi angle 2) thick) (< k 60))
    (setq hi (* hi 2.0) k (1+ k)))
  (setq k 0)
  (while (< k 40)
    (setq mid (* 0.5 (+ lo hi)))
    (if (< (pf:webscan shape size s2 s mid angle 2) thick)
      (setq lo mid)
      (setq hi mid))
    (setq k (1+ k)))
  (/ (float (fix (+ (* hi 10000.0) 0.9999))) 10000.0)
)

;;; Row pitch actually used: the explicit value when the user set one,
;;; otherwise the standard proportion opened up just enough to satisfy
;;; the bar rule.  Auto therefore packs rows as tightly as the material
;;; allows without ever violating the minimum web.
(defun pf:effrow ( / base thk)
  (cond
    (*perf-rowpitch*)
    (T
     (setq base (pf:defaultrow *perf-shape* *perf-size* *perf-size2*
                               *perf-angle* *perf-spacing*)
           thk  (cond (*perf-thick*) (0.0)))
     (max base (pf:minrow *perf-shape* *perf-size* *perf-size2*
                          *perf-spacing* *perf-angle* thk)))
  )
)

;;; Verify the current spacing against the stored material thickness.
;;; Loops until the pattern passes or the user overrides.
(defun pf:checkweb ( / tt web minc ans tmp done s2)
  (if (and *perf-thick* (> *perf-thick* 0.0) (pf:havecfg))
    (progn
      (setq s2 (cond (*perf-size2*) (0.0)) done nil)
      ;; --- along-row pitch --------------------------------------
      (while (not done)
        (setq web (pf:webscan *perf-shape* *perf-size* s2
                              *perf-spacing* *perf-spacing* *perf-angle* 1))
        (if (< web (- *perf-thick* 1e-9))
          (progn
            (setq minc (pf:minalong *perf-shape* *perf-size* *perf-size2*
                                    *perf-angle* *perf-thick*))
            (princ "\n\n*** MINIMUM BAR WARNING ***")
            (princ "\nThe web (bar) is the clear material left between holes.  It")
            (princ "\nshould be at least the material thickness or the sheet tears")
            (princ "\nand distorts when punched.")
            (princ (strcat "\n  Material thickness : " (rtos *perf-thick* 2 4) "\""))
            (princ (strcat "\n  Spacing " (rtos *perf-spacing* 2 4)
                           "\" at " *perf-angle*
                           " leaves only " (rtos web 2 4) "\" along the rows"))
            (princ (strcat "\n  Minimum center-to-center for this pattern: "
                           (rtos minc 2 4) "\""))
            (princ (strcat "\n  [Minimum]  = use " (rtos minc 2 4) "\""))
            (princ "\n  [New]      = type a different center-to-center")
            (princ "\n  [Override] = keep the current spacing anyway")
            (initget "Minimum New Override")
            (setq ans (getkword "\nChoose [Minimum/New/Override] <Minimum>: "))
            (cond
              ((= ans "Override")
               (princ "\nContinuing below minimum bar (override).")
               (setq done T))
              ((= ans "New")
               (initget 6)
               (setq tmp (getdist (strcat "\nNew spacing center-to-center (in) <"
                                          (rtos minc 2 4) ">: ")))
               (setq *perf-spacing* (cond (tmp) (minc))))
              (T
               (setq *perf-spacing* minc)
               (princ (strcat "\nSpacing set to " (rtos minc 2 4) "\"."))
               (setq done T)))
            (pf:savecfg))
          (setq done T)))

      ;; --- row pitch (only an explicit value can violate) --------
      (setq done nil)
      (while (not done)
        (setq tt  (pf:effrow)
              web (pf:webscan *perf-shape* *perf-size* s2
                              *perf-spacing* tt *perf-angle* 2))
        (if (< web (- *perf-thick* 1e-9))
          (progn
            (setq minc (pf:minrow *perf-shape* *perf-size* *perf-size2*
                                  *perf-spacing* *perf-angle* *perf-thick*))
            (princ "\n\n*** MINIMUM BAR WARNING (row spacing) ***")
            (princ (strcat "\n  Row spacing " (rtos tt 2 4)
                           "\" leaves only " (rtos web 2 4)
                           "\" between rows (material "
                           (rtos *perf-thick* 2 4) "\")."))
            (princ (strcat "\n  Minimum row spacing for this pattern: "
                           (rtos minc 2 4) "\""))
            (princ (strcat "\n  [Minimum]  = use " (rtos minc 2 4) "\""))
            (princ "\n  [New]      = type a different row spacing")
            (princ "\n  [Auto]     = let the pattern choose the tightest legal row")
            (princ "\n  [Override] = keep the current row spacing anyway")
            (initget "Minimum New Auto Override")
            (setq ans (getkword "\nChoose [Minimum/New/Auto/Override] <Minimum>: "))
            (cond
              ((= ans "Override")
               (princ "\nContinuing below minimum bar (override).")
               (setq done T))
              ((= ans "Auto")
               (setq *perf-rowpitch* nil)
               (princ "\nRow spacing set to Auto."))
              ((= ans "New")
               (initget 6)
               (setq tmp (getdist (strcat "\nNew row spacing (in) <"
                                          (rtos minc 2 4) ">: ")))
               (setq *perf-rowpitch* (cond (tmp) (minc))))
              (T
               (setq *perf-rowpitch* minc)
               (princ (strcat "\nRow spacing set to " (rtos minc 2 4) "\"."))
               (setq done T)))
            (pf:savecfg))
          (setq done T)))

      ;; --- summary ----------------------------------------------
      (setq tt  (pf:effrow)
            web (pf:minweb *perf-shape* *perf-size* s2
                           *perf-spacing* tt *perf-angle*))
      (princ (strcat "\nPattern: " (rtos *perf-spacing* 2 4)
                     "\" along rows, " (rtos tt 2 4) "\" between rows"
                     (if *perf-rowpitch* "" " (auto)")
                     " - clear bar " (rtos web 2 4)
                     "\" (material " (rtos *perf-thick* 2 4) "\")."))
    )
  )
  (princ)
)

;;; ---- interactive pattern setup ----------------------------------

;;; Prompt text for the single-value shapes.
(defun pf:sizeprompt (shape)
  (cond
    ((= shape "Circle")  "\nCircle diameter (in) <")
    ((= shape "Square")  "\nSquare side length (in) <")
    ((= shape "Hexagon") "\nHexagon size across flats (in) <")
    ((= shape "Diamond") "\nDiamond point-to-point size (in) <")
    (T "\nHole size (in) <")
  )
)

;;; Prompt text for the first of the two-value shapes.
(defun pf:lenprompt (shape)
  (cond
    ((= shape "Rectangle") "\nRectangle length (in) <")
    ((= shape "Slot")      "\nSlot overall length (in, includes end caps) <")
    (T "\nHole length (in) <")
  )
)

;;; Prompt text for the second of the two-value shapes.
(defun pf:widprompt (shape)
  (cond
    ((= shape "Rectangle") "\nRectangle width (in) <")
    ((= shape "Slot")      "\nSlot width (in) <")
    (T "\nHole width (in) <")
  )
)

(defun pf:setup ( / tmp)
  (initget "Circle Square Rectangle Hexagon Diamond SLot")
  (setq tmp (getkword
              (strcat "\nHole shape [Circle/Square/Rectangle/Hexagon/Diamond/SLot] <"
                      (cond (*perf-shape*) ("Circle")) ">: ")))
  (if (= tmp "SLot") (setq tmp "Slot"))
  (if tmp (setq *perf-shape* tmp)
          (if (null *perf-shape*) (setq *perf-shape* "Circle")))

  (cond
    ((member *perf-shape* '("Rectangle" "Slot"))
     (initget 6)
     (setq tmp (getdist (strcat (pf:lenprompt *perf-shape*)
                                (rtos (cond (*perf-size*) (1.0)) 2 4) ">: ")))
     (if tmp (setq *perf-size* tmp) (if (null *perf-size*) (setq *perf-size* 1.0)))
     (initget 6)
     (setq tmp (getdist (strcat (pf:widprompt *perf-shape*)
                                (rtos (cond (*perf-size2*) (0.5)) 2 4) ">: ")))
     (if tmp (setq *perf-size2* tmp) (if (null *perf-size2*) (setq *perf-size2* 0.5)))
     (if (and (= *perf-shape* "Slot") (<= *perf-size* *perf-size2*))
       (progn
         (princ "\nSlot length must exceed its width - using width x 2.")
         (setq *perf-size* (* 2.0 *perf-size2*)))))
    (T
     (initget 6)
     (setq tmp (getdist (strcat (pf:sizeprompt *perf-shape*)
                                (rtos (cond (*perf-size*) (0.5)) 2 4) ">: ")))
     (if tmp (setq *perf-size* tmp) (if (null *perf-size*) (setq *perf-size* 0.5))))
  )

  (initget 6)
  (setq tmp (getdist (strcat "\nMaterial thickness (in) <"
                             (rtos (cond (*perf-thick*) (0.0625)) 2 4) ">: ")))
  (if tmp (setq *perf-thick* tmp) (if (null *perf-thick*) (setq *perf-thick* 0.0625)))

  (initget 6)
  (setq tmp (getdist (strcat "\nSpacing center-to-center (in) <"
                             (rtos (cond (*perf-spacing*) (1.0)) 2 4) ">: ")))
  (if tmp (setq *perf-spacing* tmp) (if (null *perf-spacing*) (setq *perf-spacing* 1.0)))

  (initget "Straight 30 45 60")
  (setq tmp (getkword (strcat "\nPattern angle [Straight/30/45/60] <"
                              (cond (*perf-angle*) ("60")) ">: ")))
  (if tmp (setq *perf-angle* tmp) (if (null *perf-angle*) (setq *perf-angle* "60")))

  ;; Row pitch: Auto keeps the standard pattern proportions (and packs
  ;; long holes tightly across the rows) while honouring the bar rule.
  (initget "Auto")
  (setq tmp (getdist (strcat "\nSpacing between rows (in) or [Auto] <"
                             (if *perf-rowpitch* (rtos *perf-rowpitch* 2 4) "Auto")
                             ">: ")))
  (cond
    ((= tmp "Auto") (setq *perf-rowpitch* nil))
    ((numberp tmp)  (setq *perf-rowpitch* tmp))
  )

  (pf:savecfg)
  ;; angle affects which neighbors are closest, so check once it is known
  (pf:checkweb)
  (princ)
)

;;; ---- boundary acquisition ---------------------------------------
;;; Returns (clippoly realverts) where:
;;;   clippoly  = list of (x y) used for inside/clip tests
;;;   realverts = the boundary's true vertices (for edge following),
;;;               or NIL when the boundary is curved / unavailable.
;;; Returns NIL on cancel.

;;; T when the curve EN is usable as a closed boundary: either its
;;; closed flag is set, or its start and end points coincide.
(defun pf:isclosed (en / sp ep)
  (cond
    ((vlax-curve-isClosed en) T)
    ((and (setq sp (vlax-curve-getStartPoint en))
          (setq ep (vlax-curve-getEndPoint en))
          (< (distance sp ep) 1e-6)) T)
    (T nil)
  )
)

;;; Transform a 3D point P by a nentsel block matrix MAT (a list of
;;; four 3D points: three basis columns + translation).  MAT NIL = WCS.
(defun pf:xfmpt (p mat)
  (if mat
    (list
      (+ (* (car   (nth 0 mat)) (car p)) (* (car   (nth 1 mat)) (cadr p))
         (* (car   (nth 2 mat)) (caddr p)) (car   (nth 3 mat)))
      (+ (* (cadr  (nth 0 mat)) (car p)) (* (cadr  (nth 1 mat)) (cadr p))
         (* (cadr  (nth 2 mat)) (caddr p)) (cadr  (nth 3 mat)))
      (+ (* (caddr (nth 0 mat)) (car p)) (* (caddr (nth 1 mat)) (cadr p))
         (* (caddr (nth 2 mat)) (caddr p)) (caddr (nth 3 mat))))
    p)
)

;;; Sample any closed curve into a polygon of (x y) WCS points.
;;; MAT is the nentsel block transform (NIL when not nested).
(defun pf:curvepts (en mat / p0 p1 n i pt pts param)
  (setq p0 (vlax-curve-getStartParam en)
        p1 (vlax-curve-getEndParam en)
        n  128  pts '()  i 0)
  (while (<= i n)
    (setq param (+ p0 (* (/ (- p1 p0) (float n)) i))
          pt (vlax-curve-getPointAtParam en param))
    (if pt
      (progn (setq pt (pf:xfmpt pt mat))
             (setq pts (cons (list (car pt) (cadr pt)) pts))))
    (setq i (1+ i)))
  (reverse pts)
)

;;; T when EN is an LWPOLYLINE whose segments are all straight
;;; (every bulge is ~zero).  Such boundaries can be clipped against
;;; their true vertices instead of a dense curve sampling.
(defun pf:lwnoarcs (en / ed ok)
  (setq ed (entget en))
  (if (= "LWPOLYLINE" (cdr (assoc 0 ed)))
    (progn
      (setq ok T)
      (foreach pr ed
        (if (and (= 42 (car pr)) (> (abs (cdr pr)) 1e-8))
          (setq ok nil)))
      ok)
    nil
  )
)

;;; True WCS vertices of an LWPOLYLINE, else NIL.  MAT as above.
(defun pf:realverts (en mat / et nv i pt verts)
  (setq et (cdr (assoc 0 (entget en))))
  (if (= et "LWPOLYLINE")
    (progn
      (setq nv (cdr (assoc 90 (entget en))) verts '() i 0)
      (while (< i nv)
        (setq pt (pf:xfmpt (vlax-curve-getPointAtParam en i) mat))
        (setq verts (cons (list (car pt) (cadr pt)) verts))
        (setq i (1+ i)))
      (reverse verts))
    nil
  )
)

(defun pf:getboundary ( / m nsel en mat p1 p2 mnx mny mxx mxy clip)
  (initget "Select Window")
  (setq m (getkword (strcat "\nFill area by [Select boundary/Window corners] <"
                            (cond (*perf-method*) ("Select")) ">: ")))
  (if (null m) (setq m (cond (*perf-method*) ("Select"))))
  (setq *perf-method* m)
  (setcfg (strcat *perf-cfgroot* "Method") m)
  (cond
    ((= m "Select")
     ;; nentsel descends into blocks; returns a block transform matrix
     ;; (3rd element) when the picked object is nested.
     (setq nsel (nentsel "\nSelect a closed boundary (may be nested in a block): "))
     (if (null nsel)
       (progn (princ "\nNothing selected.") nil)
       (progn
         (setq en  (car nsel)
               mat (if (>= (length nsel) 4) (caddr nsel) nil))
         ;; Accept any curve-like object; sample its outline and let
         ;; the point-in-polygon test close the ring.
         (if (not (vl-catch-all-error-p
                    (vl-catch-all-apply 'vlax-curve-getEndParam (list en))))
           (progn
             (if (not (pf:isclosed en))
               (princ "\nNote: boundary not flagged closed - treating its outline as a closed loop."))
             ;; Straight-segment polylines clip against their true
             ;; vertices (few points = fast); curved boundaries fall
             ;; back to dense curve sampling.
             (if (pf:lwnoarcs en)
               (progn
                 (setq clip (pf:realverts en mat))
                 (list clip clip))
               (list (pf:curvepts en mat) (pf:realverts en mat))))
           (progn (princ "\nThat object cannot be used as a boundary.") nil))
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
             (setq mnx (min (car p1) (car p2)) mxx (max (car p1) (car p2))
                   mny (min (cadr p1) (cadr p2)) mxy (max (cadr p1) (cadr p2))
                   clip (list (list mnx mny) (list mxx mny)
                              (list mxx mxy) (list mnx mxy)))
             (list clip clip))))))
  )
)

;;; Does a vertex list contain any non-axis-aligned edge?
(defun pf:hasangle (verts / n i a b dx dy ang m tol found)
  (setq n (length verts) tol 0.0175 found nil i 0)  ; ~1 degree
  (while (and (< i n) (not found))
    (setq a (nth i verts) b (nth (rem (1+ i) n) verts)
          dx (- (car b) (car a)) dy (- (cadr b) (cadr a)))
    (if (> (+ (* dx dx) (* dy dy)) 1e-9)
      (progn
        (setq ang (atan dy dx)
              m   (rem (+ ang (* 2 pi)) (/ pi 2.0)))  ; fold to [0,90)
        (if (and (> m tol) (< m (- (/ pi 2.0) tol)))
          (setq found T))))
    (setq i (1+ i)))
  found
)

;;; Angle (radians) of the boundary edge nearest to picked point PP.
(defun pf:nearestedge-ang (pp verts / n i a b d best ang)
  (setq n (length verts) best 1e30 ang 0.0 i 0)
  (while (< i n)
    (setq a (nth i verts) b (nth (rem (1+ i) n) verts)
          d (pf:segdist pp a b))
    (if (< d best)
      (setq best d
            ang (atan (- (cadr b) (cadr a)) (- (car b) (car a)))))
    (setq i (1+ i)))
  ang
)

;;; T when VERTS is an axis-aligned rectangle (every edge horizontal or
;;; vertical).  Lets the fill optimizer skip the polygon test.
(defun pf:isaxisrect (verts / n i a b dx dy ok)
  (setq verts (pf:dedup verts) n (length verts) ok (= n 4) i 0)
  (while (and ok (< i n))
    (setq a (nth i verts) b (nth (rem (1+ i) n) verts)
          dx (abs (- (car b) (car a))) dy (abs (- (cadr b) (cadr a))))
    (if (and (> dx 1e-6) (> dy 1e-6)) (setq ok nil))
    (setq i (1+ i)))
  ok
)

;;; Drop a trailing vertex that duplicates the first (closing point).
(defun pf:dedup (verts)
  (if (and (cdr verts)
           (< (distance (car verts) (last verts)) 1e-9))
    (reverse (cdr (reverse verts)))
    verts)
)

;;; Evaluate one lattice phase (origin OX OY): returns
;;; (count minCx minCy maxCx maxCy) over centers that fall within the
;;; inset window [IXLO IXHI]x[IYLO IYHI] and (unless ISRECT) inside POLY.
(defun pf:evalphase (ox oy imin imax jmin jmax ux uy vx vy
                     ixlo ixhi iylo iyhi isRect poly
                     / i j px py cnt mnx mny mxx mxy)
  (setq cnt 0 mnx 1e30 mny 1e30 mxx -1e30 mxy -1e30 i imin)
  (while (<= i imax)
    (setq j jmin)
    (while (<= j jmax)
      (setq px (+ ox (* i ux) (* j vx))
            py (+ oy (* i uy) (* j vy)))
      (if (and (>= px ixlo) (<= px ixhi) (>= py iylo) (<= py iyhi)
               (or isRect (pf:ptinpoly (list px py) poly)))
        (progn
          (setq cnt (1+ cnt))
          (if (< px mnx) (setq mnx px)) (if (> px mxx) (setq mxx px))
          (if (< py mny) (setq mny py)) (if (> py mxy) (setq mxy py))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (list cnt mnx mny mxx mxy)
)

;;; ---- main command ------------------------------------------------

(defun c:PERF
    (/ *error* poly realverts bb width height cx cy
       lat ux uy vx vy rowang rowpitch grot ca0 sa0
       longIsX rowsAlongX rowsOnLong dec
       lverts cushion det margin hw2 hh2
       imin imax jmin jmax ii jj dx dy
       hx hy gap ixlo ixhi iylo iyhi isRect
       kbest ksteps ncand res bestox bestoy bres2 sx sy
       i j px py count ans needsetup c bres pp follow)

  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** PERF Error: " msg)))
    (princ)
  )

  (pf:loadcfg)

  ;; --- reuse or redefine the pattern ------------------------------
  (setq needsetup T)
  (if (pf:havecfg)
    (progn
      (princ (strcat "\nCurrent pattern: " *perf-shape*
                     "  size=" (rtos *perf-size* 2 4)
                     (if (member *perf-shape* '("Rectangle" "Slot"))
                       (strcat " x " (rtos (cond (*perf-size2*) (0.0)) 2 4)) "")
                     "  spacing=" (rtos *perf-spacing* 2 4)
                     "  rows=" (if *perf-rowpitch* (rtos *perf-rowpitch* 2 4) "Auto")
                     "  angle=" *perf-angle*
                     (if (and *perf-thick* (> *perf-thick* 0.0))
                       (strcat "  material=" (rtos *perf-thick* 2 4)) "")))
      (initget "Continue Redefine")
      (setq ans (getkword "\nUse this pattern? [Continue/Redefine] <Continue>: "))
      (if (or (null ans) (= ans "Continue")) (setq needsetup nil))
    )
  )
  (if needsetup
    (pf:setup)
    ;; reused pattern still gets verified against the stored thickness
    (pf:checkweb))

  ;; --- get the fill boundary --------------------------------------
  (setq bres (pf:getboundary))
  (if (null bres)
    (progn (princ "\nNo fill area - command cancelled.") (exit)))
  (setq poly (car bres) realverts (cadr bres))

  (setq bb (pf:bbox poly)
        width (- (caddr bb) (car bb))
        height (- (cadddr bb) (cadr bb))
        cx (* 0.5 (+ (car bb) (caddr bb)))
        cy (* 0.5 (+ (cadr bb) (cadddr bb)))
        grot 0.0  follow nil)

  ;; --- offer to follow an angled boundary edge --------------------
  (if (and realverts (pf:hasangle realverts))
    (progn
      (initget "Yes No")
      (setq ans (getkword
                  "\nBoundary has angled edges. Align pattern rows to a boundary edge? [Yes/No] <No>: "))
      (if (= ans "Yes")
        (progn
          (setq pp (getpoint "\nPick a point on the edge to align the rows to: "))
          (if pp
            (progn
              (setq grot (pf:nearestedge-ang pp realverts)
                    follow T)
              (princ (strcat "\nRows aligned to edge at "
                             (angtos grot 0 2) "."))
            )
          )
        )
      )
    )
  )

  ;; --- orientation check (staggered 30/60, axis-aligned only) -----
  (if (and (not follow) (member *perf-angle* '("30" "60")))
    (progn
      (setq longIsX (>= width height)
            rowsAlongX (= *perf-angle* "60")
            rowsOnLong (or (and longIsX rowsAlongX)
                           (and (not longIsX) (not rowsAlongX))))
      (if (not rowsOnLong)
        (progn
          (princ "\n\n*** ORIENTATION WARNING ***")
          (princ "\nThe continuous STRAIGHT rows are currently running along the")
          (princ "\nSHORT side of the boundary.  Best practice is to run the straight")
          (princ "\nrows along the LONGEST side, leaving the staggered edge on the")
          (princ "\nshort side (stronger sheet, less waste, cleaner edge).")
          (princ (strcat "\n  Boundary: " (rtos width 2 3) " (X) x "
                         (rtos height 2 3) " (Y)"))
          (princ (strcat "\n  Angle " *perf-angle* " runs straight rows along the "
                         (if rowsAlongX "X" "Y") " axis (short side)."))
          (princ "\n  [Continue] = override, keep this orientation.")
          (princ "\n  [Fix]      = swap 30<->60 to put straight rows on the long side.")
          (initget "Continue Fix")
          (setq dec (getkword "\nChoose [Continue/Fix] <Fix>: "))
          (if (or (null dec) (= dec "Fix"))
            (progn
              (setq *perf-angle* (if (= *perf-angle* "30") "60" "30"))
              (pf:savecfg)
              (princ (strcat "\nReoriented - angle is now " *perf-angle* ".")))
            (princ "\nContinuing with the current orientation (override)."))
        )
      )
    )
  )

  ;; --- build lattice & local hole geometry, apply global rotation -
  (setq rowpitch (pf:effrow)
        lat (pf:lattice *perf-angle* *perf-spacing* rowpitch)
        ux (nth 0 lat) uy (nth 1 lat)
        vx (nth 2 lat) vy (nth 3 lat)
        rowang (+ (nth 4 lat) grot)
        lverts (pf:localverts *perf-shape* *perf-size* *perf-size2*)
        cushion (pf:circumr *perf-shape* *perf-size* *perf-size2*))
  ;; rotate the lattice vectors by grot (0 unless following an edge)
  (setq ca0 (cos grot) sa0 (sin grot))
  (setq lat (list (- (* ux ca0) (* uy sa0)) (+ (* ux sa0) (* uy ca0))
                  (- (* vx ca0) (* vy sa0)) (+ (* vx sa0) (* vy ca0))))
  (setq ux (nth 0 lat) uy (nth 1 lat) vx (nth 2 lat) vy (nth 3 lat))

  ;; index range: invert [U V] against expanded bbox corners
  (setq det (- (* ux vy) (* uy vx))
        margin (+ cushion (max *perf-spacing* rowpitch))
        hw2 (+ (* 0.5 width) margin)
        hh2 (+ (* 0.5 height) margin))
  (setq imin 1e30 imax -1e30 jmin 1e30 jmax -1e30)
  (foreach c (list (list (- hw2) (- hh2)) (list hw2 (- hh2))
                   (list hw2 hh2) (list (- hw2) hh2))
    (setq dx (car c) dy (cadr c)
          ii (/ (- (* vy dx) (* vx dy)) det)
          jj (/ (- (* ux dy) (* uy dx)) det)
          imin (min imin ii) imax (max imax ii)
          jmin (min jmin jj) jmax (max jmax jj)))
  (setq imin (fix (- imin 3.0)) imax (fix (+ imax 3.0))
        jmin (fix (- jmin 3.0)) jmax (fix (+ jmax 3.0)))

  ;; --- footprint half-extents + inset window ----------------------
  ;; A hole fits when its center is at least (half-extent + gap) from
  ;; the bbox edges; GAP is a hair of clearance so holes never touch.
  (if (= *perf-shape* "Circle")
    (setq hx (* 0.5 *perf-size*) hy hx)
    (progn
      (setq hx 0.0 hy 0.0 ca0 (cos rowang) sa0 (sin rowang))
      (foreach p lverts
        (setq dx (abs (- (* (car p) ca0) (* (cadr p) sa0)))
              dy (abs (+ (* (car p) sa0) (* (cadr p) ca0))))
        (if (> dx hx) (setq hx dx)) (if (> dy hy) (setq hy dy)))))
  (setq gap  (max (* 0.001 *perf-spacing*) 1e-5)
        ixlo (+ (car bb)   hx gap) ixhi (- (caddr bb)  hx gap)
        iylo (+ (cadr bb)  hy gap) iyhi (- (cadddr bb) hy gap)
        isRect (and realverts (= grot 0.0) (pf:isaxisrect realverts)))

  ;; --- optimize lattice phase to maximize the hole count ----------
  ;; Search translations across one unit cell (spanned by U and V);
  ;; the densest fit wins.  Rectangles skip the polygon test (fast),
  ;; so they can afford a finer search.  For polygon boundaries the
  ;; grid coarsens as the candidate count grows so the search stays
  ;; responsive on large parts with tight spacing.
  (setq ncand (* (1+ (- imax imin)) (1+ (- jmax jmin))))
  (if isRect
    (setq ksteps 16)
    (progn
      (setq ksteps (fix (sqrt (/ 3.0e6 (max 1 (* ncand (length poly)))))))
      (setq ksteps (max 2 (min 6 ksteps)))))
  (princ (strcat "\nOptimizing pattern fit (" (itoa ncand)
                 " candidate positions)... "))
  (setq kbest -1)
  (setq i 0)
  (while (< i ksteps)
    (setq j 0)
    (while (< j ksteps)
      (setq px (+ cx (* (/ i (float ksteps)) ux) (* (/ j (float ksteps)) vx))
            py (+ cy (* (/ i (float ksteps)) uy) (* (/ j (float ksteps)) vy))
            res (pf:evalphase px py imin imax jmin jmax ux uy vx vy
                              ixlo ixhi iylo iyhi isRect poly))
      (if (> (car res) kbest)
        (setq kbest (car res) bestox px bestoy py bres2 res))
      (setq j (1+ j)))
    (setq i (1+ i)))

  ;; --- re-center the maximal pattern within the free margin -------
  (if (and bres2 (> kbest 0))
    (progn
      (setq sx (- (- ixhi ixlo) (- (nth 3 bres2) (nth 1 bres2)))
            sy (- (- iyhi iylo) (- (nth 4 bres2) (nth 2 bres2)))
            sx (max sx 0.0) sy (max sy 0.0)
            bestox (+ bestox (- (+ ixlo (* 0.5 sx)) (nth 1 bres2)))
            bestoy (+ bestoy (- (+ iylo (* 0.5 sy)) (nth 2 bres2))))))

  ;; --- generate, clip, draw at the chosen origin ------------------
  (setq count 0 i imin)
  (while (<= i imax)
    (setq j jmin)
    (while (<= j jmax)
      (setq px (+ bestox (* i ux) (* j vx))
            py (+ bestoy (* i uy) (* j vy)))
      (if (and (>= px (- (car bb) cushion)) (<= px (+ (caddr bb) cushion))
               (>= py (- (cadr bb) cushion)) (<= py (+ (cadddr bb) cushion)))
        (if (pf:allinside
              (pf:footprint *perf-shape* *perf-size* px py rowang lverts)
              poly)
          (progn
            (pf:drawhole *perf-shape* *perf-size* *perf-size2* px py rowang lverts)
            (setq count (1+ count)))
        )
      )
      (setq j (1+ j)))
    (setq i (1+ i)))

  (princ (strcat "\nDone - " (itoa count) " " *perf-shape* " hole(s) placed."))
  (princ)
)

(princ "\nPERF.lsp loaded.  Type  PERF  to run.")
(princ)

;;; ============================================================ EOF
