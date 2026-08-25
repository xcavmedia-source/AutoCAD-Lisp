;;; ============================================================
;;; PLBOUND.lsp  -  Outer Boundary Polyline Generator
;;;
;;; Draws a closed LWPOLYLINE around the OUTSIDE edges of a
;;; selection.  Built for artwork made of many small objects (a
;;; logo, a paw print, a hatch that has been exploded, a mass of
;;; tiny shapes): it follows the true silhouette of the selection
;;; instead of just boxing it in, and it never traces the inside.
;;;
;;; Boundary types:
;;;   Outline - ONE polyline around the outside of the whole
;;;             selection.  Gaps between separate parts are
;;;             bridged automatically so the result is a single
;;;             closed loop.
;;;   Regions - one polyline per separate part of the selection
;;;             (5 loops for a paw print with 5 separate pads).
;;;   Hull    - convex hull.  Wraps the selection like a rubber
;;;             band - no concave detail.
;;;   Box     - rectangle covering the full extents.
;;;
;;; Outline and Regions work by stamping the selected geometry
;;; into a grid and walking the edge of that grid.  Cell size sets
;;; the accuracy: smaller follows finer detail, larger smooths
;;; over noise and joins objects that sit close together.  The
;;; default is 1/200 of the selection's longest side.
;;;
;;; Holes and interior detail are ignored on purpose - only the
;;; outside edge is drawn.  The boundary always fully encloses the
;;; selected geometry.
;;;
;;; In Outline and Regions the outward offset is rounded to whole
;;; cells, so use a smaller cell size when an exact offset matters.
;;; Hull and Box offset exactly.
;;;
;;; Geometry read exactly:
;;;   LINE, POINT, LWPOLYLINE (including bulged arc segments),
;;;   CIRCLE, ARC, ELLIPSE, SPLINE, 2D/3D POLYLINE, SOLID, TRACE,
;;;   3DFACE.  Curves are approximated by sampling (*plb-arcres*).
;;; Geometry read as a bounding box:
;;;   TEXT, MTEXT, INSERT, HATCH, IMAGE, WIPEOUT, everything else.
;;;   Explode a hatch or a block first if you need its true shape.
;;; Ignored:
;;;   XLINE and RAY (infinite extents).
;;;
;;; Everything is evaluated in the WCS XY plane and the boundary
;;; is drawn flat at the current ELEVATION.
;;;
;;; Command : PLBOUND
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- configuration ---------------------------------------------

;;; Sample points used to approximate a full 360 deg of curvature
;;; (circles, arcs, ellipses, splines, polyline bulges).
(setq *plb-arcres* 128)

;;; Default trace accuracy: cells across the longest side of the
;;; selection.  200 suits most artwork.  Raise it for finer detail.
(setq *plb-res* 200)

;;; Upper limit on grid cells.  The cell size is coarsened
;;; automatically rather than exceed this.
(setq *plb-maxcells* 400000)

;;; Corner smoothing after tracing, as a multiple of the cell size.
;;; 0 keeps the raw stair-stepped grid outline.
(setq *plb-simp* 1.0)

;;; Regions smaller than this many cells are treated as noise
;;; and skipped in Regions mode.
(setq *plb-mincells* 4)

;;; How far the gap search may reach, in cells, before it gives up
;;; on joining the selection into a single loop.
(setq *plb-maxbridge* 60)

;;; Layer for the new boundary polylines.
;;; nil = current layer.  A string (e.g. "BOUNDARY") is created if
;;; it does not already exist.
(setq *plb-layer* nil)

;;; Remembered answers - updated each run, reused as the defaults.
(setq *plb-mode*   "Outline")
(setq *plb-offset* 0.0)
(setq *plb-nbbox*  0)


;;; ---- small math / point helpers --------------------------------

;;; Strip a point down to a flat 2D (x y) list
(defun plb:2d (p) (list (car p) (cadr p)))

(defun plb:padd (a b) (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))

(defun plb:pscale (p s) (list (* (car p) s) (* (cadr p) s)))

;;; Z of the cross product of (A-O) and (B-O).
;;; > 0 means O, A, B turn counter-clockwise.
(defun plb:cross (o a b)
  (- (* (- (car  a) (car  o)) (- (cadr b) (cadr o)))
     (* (- (cadr a) (cadr o)) (- (car  b) (car  o))))
)

;;; Unit outward normal of edge A->B for a counter-clockwise
;;; polygon: (dy -dx) normalised.  nil for a zero-length edge.
(defun plb:outnorm (a b / dx dy len)
  (setq dx  (- (car  b) (car  a))
        dy  (- (cadr b) (cadr a))
        len (sqrt (+ (* dx dx) (* dy dy))))
  (if (> len 1.0e-10)
    (list (/ dy len) (/ (- dx) len))
  )
)

;;; Format VALUE using the current drawing unit / precision
(defun plb:fmt (val)
  (rtos val (getvar "LUNITS") (getvar "LUPREC"))
)

;;; Number of samples for a curve sweeping SWEEP radians
(defun plb:arcsamples (sweep)
  (max 4 (fix (+ 0.5 (* *plb-arcres* (/ (abs sweep) (* 2.0 pi))))))
)


;;; ---- reading geometry off the selected objects ------------------
;;;
;;; Every object is reduced to one or more CHAINS.  A chain is an
;;; ordered list of 2D points; consecutive points are joined by a
;;; straight line.  Closed shapes repeat their first point at the
;;; end, so a chain always describes the path to be followed.

;;; Point on a curve at parameter P, or nil if it cannot be read
(defun plb:ptat (ent p / pt)
  (setq pt (vl-catch-all-apply 'vlax-curve-getPointAtParam (list ent p)))
  (if (and pt (not (vl-catch-all-error-p pt))) (plb:2d pt))
)

;;; N+1 ordered points spanning parameters P1..P2 inclusive
(defun plb:spanfull (ent p1 p2 n / pts i pt)
  (setq n (max 1 n) pts '() i 0)
  (while (<= i n)
    (if (setq pt (plb:ptat ent (+ p1 (* (- p2 p1) (/ (float i) n)))))
      (setq pts (cons pt pts))
    )
    (setq i (1+ i))
  )
  (reverse pts)
)

;;; The N-1 ordered points strictly between parameters P1 and P2
(defun plb:spaninner (ent p1 p2 n / pts i pt)
  (setq n (max 1 n) pts '() i 1)
  (while (< i n)
    (if (setq pt (plb:ptat ent (+ p1 (* (- p2 p1) (/ (float i) n)))))
      (setq pts (cons pt pts))
    )
    (setq i (1+ i))
  )
  (reverse pts)
)

;;; LWPOLYLINE -> one chain, in vertex order, with bulged segments
;;; expanded into arc samples.  Points come from vlax-curve so they
;;; are WCS whatever the extrusion direction.
(defun plb:lwchain (ent / ed idx nv bulges pr b p closed pts i)
  (setq ed     (entget ent)
        idx    -1
        nv     0
        bulges '()
        closed (= 1 (logand 1 (if (assoc 70 ed)
                                 (cdr (assoc 70 ed))
                                 0))))
  (foreach pr ed
    (cond
      ((= (car pr) 10) (setq idx (1+ idx) nv (1+ nv)))
      ;;; A 42 pair belongs to the segment starting at vertex IDX.
      ;;; Zero bulges are normally left out of the entity data.
      ((= (car pr) 42)
       (if (> (abs (cdr pr)) 1.0e-8)
         (setq bulges (cons (cons idx (cdr pr)) bulges))))
    )
  )
  (setq pts '() i 0)
  (while (< i nv)
    (if (setq p (plb:ptat ent (float i))) (setq pts (cons p pts)))
    ;;; Arc segment starting at this vertex?  The last vertex only
    ;;; starts a segment when the polyline is closed.
    (if (and (or closed (< (1+ i) nv))
             (setq b (cdr (assoc i bulges))))
      (foreach p (plb:spaninner ent (float i) (float (1+ i))
                                ;;; included angle = 4 * atan(bulge)
                                (plb:arcsamples (* 4.0 (atan (abs b)))))
        (setq pts (cons p pts))
      )
    )
    (setq i (1+ i))
  )
  ;;; Repeat the first vertex so the chain closes
  (if (and closed (> nv 2))
    (if (setq p (plb:ptat ent 0.0)) (setq pts (cons p pts)))
  )
  (list (reverse pts))
)

;;; Any other curve - walked one whole parameter span at a time so
;;; that polyline vertices are always hit exactly.
(defun plb:genchain (ent / sp ep spans per pts a b p)
  (setq sp (vl-catch-all-apply 'vlax-curve-getStartParam (list ent))
        ep (vl-catch-all-apply 'vlax-curve-getEndParam   (list ent)))
  (if (or (vl-catch-all-error-p sp) (vl-catch-all-error-p ep)
          (null sp) (null ep) (<= (- ep sp) 1.0e-9))
    nil
    (progn
      (setq spans (max 1 (fix (+ 0.9999 (- ep sp))))
            per   (cond ((> spans 64) 4)
                        ((> spans 16) 8)
                        (T (max 4 (/ *plb-arcres* 4))))
            pts   '()
            a     sp)
      (while (< a (- ep 1.0e-9))
        (setq b (min ep (+ 1.0 (float (fix a)))))
        (if (<= (- b a) 1.0e-9) (setq b ep))          ; always progress
        ;;; first point of each span, then its interior samples
        (if (setq p (plb:ptat ent a)) (setq pts (cons p pts)))
        (foreach p (plb:spaninner ent a b per) (setq pts (cons p pts)))
        (setq a b)
      )
      (if (setq p (plb:ptat ent ep)) (setq pts (cons p pts)))
      (list (reverse pts))
    )
  )
)

;;; A chain straight from a list of DXF group codes
(defun plb:dxfchain (ent codes / ed pts p c)
  (setq ed (entget ent) pts '())
  (foreach c codes
    (if (setq p (cdr (assoc c ed)))
      (setq pts (cons (plb:2d p) pts))
    )
  )
  (if pts (list (reverse pts)))
)

;;; Fallback: the object's bounding box as a closed chain
(defun plb:bboxchain (ent / obj res mn mx)
  (setq obj (vlax-ename->vla-object ent)
        res (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx)))
  (if (vl-catch-all-error-p res)
    nil
    (progn
      (setq mn (vlax-safearray->list mn)
            mx (vlax-safearray->list mx)
            *plb-nbbox* (1+ *plb-nbbox*))
      (list (list (list (car mn) (cadr mn))
                  (list (car mx) (cadr mn))
                  (list (car mx) (cadr mx))
                  (list (car mn) (cadr mx))
                  (list (car mn) (cadr mn))))
    )
  )
)

;;; Every chain that describes ENT
(defun plb:objchains (ent / etype sp ep)
  (setq etype (cdr (assoc 0 (entget ent))))
  (cond
    ((= etype "LINE")       (plb:dxfchain ent '(10 11)))
    ((= etype "POINT")      (plb:dxfchain ent '(10)))
    ((= etype "LWPOLYLINE") (plb:lwchain ent))

    ((member etype '("CIRCLE" "ARC" "ELLIPSE"))
     ;;; Parameters are angles here, so the sweep sets the density
     (setq sp (vlax-curve-getStartParam ent)
           ep (vlax-curve-getEndParam   ent))
     (list (plb:spanfull ent sp ep (plb:arcsamples (- ep sp)))))

    ;;; A SOLID/TRACE quad is drawn 10-11-13-12, not 10-11-12-13
    ((member etype '("SOLID" "TRACE"))
     (plb:dxfchain ent '(10 11 13 12 10)))
    ((= etype "3DFACE")
     (plb:dxfchain ent '(10 11 12 13 10)))

    ;;; Infinite - no meaningful outer edge
    ((member etype '("XLINE" "RAY")) nil)

    (T (or (plb:genchain ent) (plb:bboxchain ent)))
  )
)


;;; ---- convex hull and box ---------------------------------------

;;; Is P strictly inside the counter-clockwise quad Q?
(defun plb:inquad (p q / i inside)
  (setq i 0 inside T)
  (while (and inside (< i 4))
    (if (<= (plb:cross (nth i q) (nth (rem (1+ i) 4) q) p) 0.0)
      (setq inside nil)
    )
    (setq i (1+ i))
  )
  inside
)

;;; Akl-Toussaint discard: drop every point strictly inside the quad
;;; formed by the four extreme points.  Those can never be on the
;;; hull, and this usually removes the great majority of them.
(defun plb:prefilter (pts / pxmin pxmax pymin pymax quad res p)
  (foreach p pts
    (if (or (null pxmin) (< (car  p) (car  pxmin))) (setq pxmin p))
    (if (or (null pxmax) (> (car  p) (car  pxmax))) (setq pxmax p))
    (if (or (null pymin) (< (cadr p) (cadr pymin))) (setq pymin p))
    (if (or (null pymax) (> (cadr p) (cadr pymax))) (setq pymax p))
  )
  (setq quad (list pymin pxmax pymax pxmin)   ; bottom, right, top, left
        res  '())
  (foreach p pts
    (if (not (plb:inquad p quad)) (setq res (cons p res)))
  )
  res
)

;;; Convex hull (Andrew's monotone chain).  Returns the vertices in
;;; counter-clockwise order, or fewer than 3 points when the input
;;; is degenerate (all identical or all collinear).
(defun plb:hull (pts / sorted lower upper p)
  (setq sorted (vl-sort pts
                        '(lambda (a b)
                           (or (< (car a) (car b))
                               (and (= (car a) (car b))
                                    (< (cadr a) (cadr b)))))))
  ;;; LOWER and UPPER are stacks - the newest point is the head, so
  ;;; (cdr stack) is non-nil once two points are on it.
  (setq lower '())
  (foreach p sorted
    (while (and (cdr lower)
                (<= (plb:cross (cadr lower) (car lower) p) 0.0))
      (setq lower (cdr lower))
    )
    (setq lower (cons p lower))
  )
  (setq upper '())
  (foreach p (reverse sorted)
    (while (and (cdr upper)
                (<= (plb:cross (cadr upper) (car upper) p) 0.0))
      (setq upper (cdr upper))
    )
    (setq upper (cons p upper))
  )
  ;;; Drop each chain's final point - it is the other chain's first
  (append (reverse (cdr lower)) (reverse (cdr upper)))
)

;;; Rectangle covering all points, grown outward by DIST
(defun plb:boxpts (pts dist / x0 x1 y0 y1 p)
  (foreach p pts
    (if (or (null x0) (< (car  p) x0)) (setq x0 (car  p)))
    (if (or (null x1) (> (car  p) x1)) (setq x1 (car  p)))
    (if (or (null y0) (< (cadr p) y0)) (setq y0 (cadr p)))
    (if (or (null y1) (> (cadr p) y1)) (setq y1 (cadr p)))
  )
  (setq x0 (- x0 dist) y0 (- y0 dist)
        x1 (+ x1 dist) y1 (+ y1 dist))
  (list (list x0 y0) (list x1 y0) (list x1 y1) (list x0 y1))
)

;;; Push a counter-clockwise CONVEX polygon outward by DIST.  Each
;;; edge is offset along its outward normal and consecutive offset
;;; edges are intersected, which mitres the corners.
(defun plb:offsetpoly (pts dist / n i a b c na nb ip res oa ob)
  (setq n (length pts) i 0 res '())
  (while (< i n)
    (setq a  (nth (rem (+ i (1- n)) n) pts)   ; previous vertex
          b  (nth i pts)                      ; this vertex
          c  (nth (rem (1+ i) n) pts)         ; next vertex
          na (plb:outnorm a b)
          nb (plb:outnorm b c))
    (cond
      ((and na nb)
       (setq oa (plb:pscale na dist)
             ob (plb:pscale nb dist)
             ;;; nil as the 5th argument extends both edges to
             ;;; infinite lines
             ip (inters (plb:padd a oa) (plb:padd b oa)
                        (plb:padd b ob) (plb:padd c ob) nil))
       ;;; Parallel edges (a straight-through corner) - just shift
       (setq res (cons (if ip (plb:2d ip) (plb:padd b oa)) res)))
      (na (setq res (cons (plb:padd b (plb:pscale na dist)) res)))
      (nb (setq res (cons (plb:padd b (plb:pscale nb dist)) res)))
      (T  (setq res (cons b res)))
    )
    (setq i (1+ i))
  )
  (reverse res)
)

;;; Perimeter of a closed point ring
(defun plb:perim (pts / prev total p)
  (setq prev (car pts) total 0.0)
  (foreach p (cdr pts)
    (setq total (+ total (distance prev p))
          prev  p)
  )
  (+ total (distance prev (car pts)))
)

;;; Enclosed area of a closed point ring (shoelace)
(defun plb:area (pts / prev total p)
  (setq prev (car pts) total 0.0)
  (foreach p (cdr pts)
    (setq total (+ total (- (* (car prev) (cadr p)) (* (car p) (cadr prev))))
          prev  p)
  )
  (setq total (+ total (- (* (car prev) (cadr (car pts)))
                          (* (car (car pts)) (cadr prev)))))
  (abs (/ total 2.0))
)


;;; ---- the grid --------------------------------------------------
;;;
;;; The selection is stamped into a grid of square cells.  Cell
;;; values: 0 empty, 1 filled, 2 region being traced, 3 traced,
;;; 9 visited by the region search, 10+ a region label used while
;;; measuring gaps.  Working state is held in globals so the inner
;;; loops stay cheap.
;;;
;;;   *plb-arr*  cell values       *plb-nx* *plb-ny*  size in cells
;;;   *plb-ox* *plb-oy*  grid origin (WCS of cell 0,0 corner)
;;;   *plb-cs*   cell size         *plb-occ*  list of filled cells

(defun plb:cget (i j)
  (if (or (< i 0) (< j 0) (>= i *plb-nx*) (>= j *plb-ny*))
    0
    (vlax-safearray-get-element *plb-arr* (+ i (* j *plb-nx*)))
  )
)

(defun plb:cput (i j v)
  (if (and (>= i 0) (>= j 0) (< i *plb-nx*) (< j *plb-ny*))
    (vlax-safearray-put-element *plb-arr* (+ i (* j *plb-nx*)) v)
  )
)

;;; Cell index of a WCS point
(defun plb:ci (x) (fix (/ (- x *plb-ox*) *plb-cs*)))
(defun plb:cj (y) (fix (/ (- y *plb-oy*) *plb-cs*)))

;;; Build an empty grid spanning EXT ((x0 y0) (x1 y1)) with CELL
;;; sized cells and MARGIN empty cells all round
(defun plb:gridnew (ext cell margin)
  (setq *plb-cs* cell
        *plb-ox* (- (caar  ext) (* cell margin))
        *plb-oy* (- (cadar ext) (* cell margin))
        *plb-nx* (+ (fix (/ (- (caadr  ext) *plb-ox*) cell)) margin 2)
        *plb-ny* (+ (fix (/ (- (cadadr ext) *plb-oy*) cell)) margin 2)
        *plb-arr* (vlax-make-safearray
                    vlax-vbInteger
                    (cons 0 (1- (* *plb-nx* *plb-ny*))))
        *plb-occ* '())
)

;;; Fill one cell, keeping the filled-cell list in step
(defun plb:fill (i j)
  (if (and (>= i 0) (>= j 0) (< i *plb-nx*) (< j *plb-ny*)
           (/= (plb:cget i j) 1))
    (progn
      (plb:cput i j 1)
      (setq *plb-occ* (cons (cons i j) *plb-occ*))
    )
  )
)

;;; Stamp the straight segment P0->P1, stepping half a cell at a
;;; time so the trail of cells is always connected
(defun plb:markseg (p0 p1 / d n i tt)
  (setq d (distance p0 p1)
        n (1+ (fix (/ d (* *plb-cs* 0.5))))
        i 0)
  (while (<= i n)
    (setq tt (/ (float i) n))
    (plb:fill (plb:ci (+ (car  p0) (* (- (car  p1) (car  p0)) tt)))
              (plb:cj (+ (cadr p0) (* (- (cadr p1) (cadr p0)) tt))))
    (setq i (1+ i))
  )
)

;;; Stamp every chain into the grid
(defun plb:markchains (chains / ch prev p)
  (foreach ch chains
    (if (cdr ch)
      (progn
        (setq prev (car ch))
        (foreach p (cdr ch)
          (plb:markseg prev p)
          (setq prev p)
        )
      )
      (plb:fill (plb:ci (caar ch)) (plb:cj (cadar ch)))
    )
  )
)


;;; ---- regions ---------------------------------------------------

;;; Every 8-connected group of filled cells, as a list of cell
;;; lists.  The grid is left exactly as it was found.
(defun plb:regions ( / res cells stack c a b di dj)
  (setq res '())
  (foreach c *plb-occ*
    (if (= (plb:cget (car c) (cdr c)) 1)
      (progn
        (plb:cput (car c) (cdr c) 9)
        (setq stack (list c) cells '())
        (while stack
          (setq a     (caar stack)
                b     (cdar stack)
                stack (cdr stack)
                cells (cons (cons a b) cells)
                dj    -1)
          (while (<= dj 1)
            (setq di -1)
            (while (<= di 1)
              (if (and (or (/= di 0) (/= dj 0))
                       (= (plb:cget (+ a di) (+ b dj)) 1))
                (progn
                  (plb:cput (+ a di) (+ b dj) 9)
                  (setq stack (cons (cons (+ a di) (+ b dj)) stack))
                )
              )
              (setq di (1+ di))
            )
            (setq dj (1+ dj))
          )
        )
        (setq res (cons cells res))
      )
    )
  )
  ;;; put the grid back the way it was
  (foreach cells res
    (foreach c cells (plb:cput (car c) (cdr c) 1))
  )
  (reverse res)
)


;;; ---- measuring the gaps between regions -------------------------

(defun plb:find (a / r nx)
  (setq r a)
  (while (/= (vlax-safearray-get-element *plb-root* r) r)
    (setq r (vlax-safearray-get-element *plb-root* r))
  )
  ;;; flatten the path so later lookups are quick
  (while (/= (vlax-safearray-get-element *plb-root* a) a)
    (setq nx (vlax-safearray-get-element *plb-root* a))
    (vlax-safearray-put-element *plb-root* a r)
    (setq a nx)
  )
  r
)

;;; Grow every region outward one cell at a time until they have all
;;; met.  The number of steps that takes is how far apart the
;;; furthest-separated parts of the selection are, in cells, and so
;;; how much dilation is needed to join them into one loop.
;;; Returns that count, or nil if they never all met.
(defun plb:bridge (regs maxk / n i cells c frontier nxt claimed
                                d remaining a b di dj qv la lb ra rb)
  (setq n         (length regs)
        *plb-root* (vlax-make-safearray vlax-vbInteger (cons 0 (max 0 (1- n))))
        i         0)
  (while (< i n)
    (vlax-safearray-put-element *plb-root* i i)
    (setq i (1+ i))
  )
  ;;; label every region's cells 10, 11, 12 ...
  (setq i 0 frontier '())
  (foreach cells regs
    (foreach c cells
      (plb:cput (car c) (cdr c) (+ 10 i))
      (setq frontier (cons c frontier))
    )
    (setq i (1+ i))
  )
  (setq claimed '() remaining n d 0)
  (while (and (> remaining 1) (< d maxk) frontier)
    (setq d (1+ d) nxt '())
    (foreach c frontier
      (setq a  (car c)
            b  (cdr c)
            la (- (plb:cget a b) 10)
            dj -1)
      (while (<= dj 1)
        (setq di -1)
        (while (<= di 1)
          (if (or (/= di 0) (/= dj 0))
            (progn
              (setq qv (plb:cget (+ a di) (+ b dj)))
              (cond
                ;;; empty ground - claim it for this region
                ((= qv 0)
                 (plb:cput (+ a di) (+ b dj) (+ 10 la))
                 (setq claimed (cons (cons (+ a di) (+ b dj)) claimed)
                       nxt     (cons (cons (+ a di) (+ b dj)) nxt)))
                ;;; already claimed - if by another region, they meet
                ((>= qv 10)
                 (setq lb (- qv 10)
                       ra (plb:find la)
                       rb (plb:find lb))
                 (if (/= ra rb)
                   (progn
                     (vlax-safearray-put-element *plb-root* ra rb)
                     (setq remaining (1- remaining))
                   )
                 ))
              )
            )
          )
          (setq di (1+ di))
        )
        (setq dj (1+ dj))
      )
    )
    (setq frontier nxt)
  )
  ;;; put the grid back the way it was
  (foreach c claimed (plb:cput (car c) (cdr c) 0))
  (foreach cells regs
    (foreach c cells (plb:cput (car c) (cdr c) 1))
  )
  (if (> remaining 1) nil d)
)


;;; ---- growing and shrinking the filled area ----------------------

;;; Fill every cell within K cells of the filled area.
;;; Returns the last layer added (the cells now on the outer edge).
(defun plb:dilate (k / d frontier nxt c a b di dj)
  (setq frontier *plb-occ* d 0)
  (while (< d k)
    (setq d (1+ d) nxt '())
    (foreach c frontier
      (setq a (car c) b (cdr c) dj -1)
      (while (<= dj 1)
        (setq di -1)
        (while (<= di 1)
          (if (and (or (/= di 0) (/= dj 0))
                   (= (plb:cget (+ a di) (+ b dj)) 0))
            (progn
              (plb:cput (+ a di) (+ b dj) 1)
              (setq nxt (cons (cons (+ a di) (+ b dj)) nxt))
            )
          )
          (setq di (1+ di))
        )
        (setq dj (1+ dj))
      )
    )
    (setq *plb-occ* (append nxt *plb-occ*))
    (setq frontier nxt)
  )
  (if (> k 0) frontier *plb-occ*)
)

;;; The empty cells touching CELLS - the seed for shrinking back
(defun plb:outerring (cells / res c a b di dj)
  (setq res '())
  (foreach c cells
    (setq a (car c) b (cdr c) dj -1)
    (while (<= dj 1)
      (setq di -1)
      (while (<= di 1)
        (if (and (or (/= di 0) (/= dj 0))
                 (= (plb:cget (+ a di) (+ b dj)) 0))
          (setq res (cons (cons (+ a di) (+ b dj)) res))
        )
        (setq di (1+ di))
      )
      (setq dj (1+ dj))
    )
  )
  res
)

;;; Empty every filled cell within K cells of SEED (empty ground).
;;; Dilating by K+1 then eroding by K closes the gaps between parts
;;; while leaving the outside edge where it started.
(defun plb:erode (seed k / d frontier nxt removed c a b di dj)
  (setq frontier seed d 0 removed '())
  (while (and (< d k) frontier)
    (setq d (1+ d) nxt '())
    (foreach c frontier
      (setq a (car c) b (cdr c) dj -1)
      (while (<= dj 1)
        (setq di -1)
        (while (<= di 1)
          (if (and (or (/= di 0) (/= dj 0))
                   (= (plb:cget (+ a di) (+ b dj)) 1))
            (progn
              (plb:cput (+ a di) (+ b dj) 4)
              (setq nxt     (cons (cons (+ a di) (+ b dj)) nxt)
                    removed (cons (cons (+ a di) (+ b dj)) removed))
            )
          )
          (setq di (1+ di))
        )
        (setq dj (1+ dj))
      )
    )
    (setq frontier nxt)
  )
  (foreach c removed (plb:cput (car c) (cdr c) 0))
  (setq *plb-occ*
        (vl-remove-if '(lambda (c) (/= (plb:cget (car c) (cdr c)) 1))
                      *plb-occ*))
)


;;; ---- walking the edge of a region -------------------------------
;;;
;;; The outline runs along cell edges, not cell centres.  Every
;;; filled cell contributes the edges that face empty ground,
;;; directed so the filled side is always on the left; chaining them
;;; head to tail therefore walks the region counter-clockwise.
;;; Directions: 0 = +X, 1 = +Y, 2 = -X, 3 = -Y.

(defun plb:vnew ( / n)
  (setq n (* (1+ *plb-nx*) (1+ *plb-ny*))
        *plb-e1* (vlax-make-safearray vlax-vbInteger (cons 0 (1- n)))
        *plb-e2* (vlax-make-safearray vlax-vbInteger (cons 0 (1- n))))
)

(defun plb:vget (arr i j)
  (vlax-safearray-get-element arr (+ i (* j (1+ *plb-nx*))))
)

(defun plb:vput (arr i j v)
  (vlax-safearray-put-element arr (+ i (* j (1+ *plb-nx*))) v)
)

;;; Directions are stored as d+1 so that 0 can mean "no edge here"
(defun plb:addedge (i j d)
  (if (= (plb:vget *plb-e1* i j) 0)
    (plb:vput *plb-e1* i j (1+ d))
    (plb:vput *plb-e2* i j (1+ d))
  )
  (setq *plb-vtx* (cons (cons i j) *plb-vtx*))
)

;;; Trace CELLS (one region) and return its outline as a list of
;;; grid corners.  CELLS are marked 3 (done) on the way out.
(defun plb:trace (cells / c i j s si sj vi vj d dl d1 d2 use loop guard maxg going)
  (foreach c cells (plb:cput (car c) (cdr c) 2))
  (setq *plb-vtx* '())
  (foreach c cells
    (setq i (car c) j (cdr c))
    (if (/= (plb:cget i (1- j)) 2) (plb:addedge i j 0))              ; bottom
    (if (/= (plb:cget (1+ i) j) 2) (plb:addedge (1+ i) j 1))         ; right
    (if (/= (plb:cget i (1+ j)) 2) (plb:addedge (1+ i) (1+ j) 2))    ; top
    (if (/= (plb:cget (1- i) j) 2) (plb:addedge i (1+ j) 3))         ; left
  )
  ;;; Start on the bottom edge of the lowest, then left-most, cell.
  ;;; That edge is always on the outside of the region.
  (setq s (car cells))
  (foreach c cells
    (if (or (< (cdr c) (cdr s))
            (and (= (cdr c) (cdr s)) (< (car c) (car s))))
      (setq s c)
    )
  )
  (setq si (car s) sj (cdr s)
        vi si vj sj
        d  3                      ; pretend we arrived heading -Y ...
        loop '() guard 0 going T
        maxg (+ 1000 (* 8 (length cells))))
  (while going
    ;;; ... so the preferred left turn is +X, the bottom edge
    (setq dl (rem (1+ d) 4)
          d1 (1- (plb:vget *plb-e1* vi vj))
          d2 (1- (plb:vget *plb-e2* vi vj)))
    (cond
      ;;; Where two corners of the region meet at one grid point
      ;;; there are two ways on; the left turn keeps the filled
      ;;; side on the left.
      ((= d1 dl) (setq use 1 d d1))
      ((= d2 dl) (setq use 2 d d2))
      ((>= d1 0) (setq use 1 d d1))
      ((>= d2 0) (setq use 2 d d2))
      (T         (setq use nil))
    )
    (if (null use)
      (setq going nil)
      (progn
        (if (= use 1)
          (plb:vput *plb-e1* vi vj 0)
          (plb:vput *plb-e2* vi vj 0)
        )
        (setq loop  (cons (cons vi vj) loop)
              vi    (+ vi (nth d '(1 0 -1 0)))
              vj    (+ vj (nth d '(0 1 0 -1)))
              guard (1+ guard))
        (if (or (and (= vi si) (= vj sj)) (> guard maxg))
          (setq going nil)
        )
      )
    )
  )
  ;;; clear this region's edges and mark it done
  (foreach c *plb-vtx*
    (plb:vput *plb-e1* (car c) (cdr c) 0)
    (plb:vput *plb-e2* (car c) (cdr c) 0)
  )
  (foreach c cells (plb:cput (car c) (cdr c) 3))
  (reverse loop)
)

;;; Grid corners -> WCS points
(defun plb:toworld (loop / res c)
  (setq res '())
  (foreach c loop
    (setq res (cons (list (+ *plb-ox* (* (car c) *plb-cs*))
                          (+ *plb-oy* (* (cdr c) *plb-cs*)))
                    res))
  )
  (reverse res)
)


;;; ---- tidying the traced outline ---------------------------------

;;; Drop points that sit on a straight line between their
;;; neighbours.  Walks the ring once - no random access.
(defun plb:dropcol (pts / a b c rest out n)
  (if (< (length pts) 4)
    pts
    (progn
      (setq a    (car pts)
            b    (cadr pts)
            rest (append (cddr pts) (list (car pts) (cadr pts)))
            out  '())
      (while rest
        (setq c (car rest) rest (cdr rest))
        (if (> (abs (plb:cross a b c)) 1.0e-9) (setq out (cons b out)))
        (setq a b b c)
      )
      (if (>= (length out) 3) (reverse out) pts)
    )
  )
)

;;; Douglas-Peucker on an open chain.  Uses arrays and its own stack
;;; so a long outline cannot run the interpreter out of depth.
(defun plb:dp (pts tol / n xs ys kp i p stack seg a b ax ay bx by
                         dx dy len worst idx k px py dist res)
  (setq n (length pts))
  (if (or (< n 3) (<= tol 0.0))
    pts
    (progn
      (setq xs (vlax-make-safearray vlax-vbDouble  (cons 0 (1- n)))
            ys (vlax-make-safearray vlax-vbDouble  (cons 0 (1- n)))
            kp (vlax-make-safearray vlax-vbInteger (cons 0 (1- n)))
            i  0)
      (foreach p pts
        (vlax-safearray-put-element xs i (float (car  p)))
        (vlax-safearray-put-element ys i (float (cadr p)))
        (vlax-safearray-put-element kp i 0)
        (setq i (1+ i))
      )
      (vlax-safearray-put-element kp 0 1)
      (vlax-safearray-put-element kp (1- n) 1)
      (setq stack (list (cons 0 (1- n))))
      (while stack
        (setq seg   (car stack)
              stack (cdr stack)
              a     (car seg)
              b     (cdr seg))
        (if (> b (1+ a))
          (progn
            (setq ax    (vlax-safearray-get-element xs a)
                  ay    (vlax-safearray-get-element ys a)
                  bx    (vlax-safearray-get-element xs b)
                  by    (vlax-safearray-get-element ys b)
                  dx    (- bx ax)
                  dy    (- by ay)
                  len   (sqrt (+ (* dx dx) (* dy dy)))
                  worst -1.0
                  idx   -1
                  k     (1+ a))
            (while (< k b)
              (setq px   (vlax-safearray-get-element xs k)
                    py   (vlax-safearray-get-element ys k)
                    dist (if (> len 1.0e-12)
                           (/ (abs (+ (- (* dy px) (* dx py))
                                      (- (* bx ay) (* by ax))))
                              len)
                           (distance (list px py) (list ax ay))))
              (if (> dist worst) (setq worst dist idx k))
              (setq k (1+ k))
            )
            (if (> worst tol)
              (progn
                (vlax-safearray-put-element kp idx 1)
                (setq stack (cons (cons a idx) (cons (cons idx b) stack)))
              )
            )
          )
        )
      )
      (setq res '() i 0)
      (foreach p pts
        (if (= (vlax-safearray-get-element kp i) 1) (setq res (cons p res)))
        (setq i (1+ i))
      )
      (reverse res)
    )
  )
)

;;; Douglas-Peucker on a closed ring: split it at the first point and
;;; the point furthest from it, simplify both halves, join them up.
(defun plb:simpring (pts tol / p0 far fard i d p chaina chainb ra rb res)
  (if (or (<= tol 0.0) (< (length pts) 8))
    pts
    (progn
      (setq p0 (car pts) far 0 fard -1.0 i 0)
      (foreach p pts
        (setq d (distance p0 p))
        (if (> d fard) (setq fard d far i))
        (setq i (1+ i))
      )
      (setq chaina '() chainb '() i 0)
      (foreach p pts
        (if (<= i far) (setq chaina (cons p chaina)))
        (if (>= i far) (setq chainb (cons p chainb)))
        (setq i (1+ i))
      )
      (setq chaina (reverse chaina)
            chainb (append (reverse chainb) (list p0))
            ra     (plb:dp chaina tol)
            rb     (plb:dp chainb tol)
            ;;; each chain ends where the other begins
            res    (append (reverse (cdr (reverse ra)))
                           (reverse (cdr (reverse rb)))))
      (if (>= (length res) 3) res pts)
    )
  )
)


;;; ---- drawing ---------------------------------------------------

;;; Safely get the active space VLA object (model or paper)
(defun plb:activespace (doc)
  (if (and (= (getvar "TILEMODE") 0)
           (= (getvar "CVPORT") 1))
    (vla-get-paperspace doc)
    (vla-get-modelspace doc)
  )
)

;;; Create the closed LWPOLYLINE through PTS
(defun plb:mkpline (space pts elev / flat arr obj p)
  (setq flat '())
  (foreach p pts
    (setq flat (cons (float (cadr p)) (cons (float (car p)) flat)))
  )
  (setq flat (reverse flat)                    ; x1 y1 x2 y2 ...
        arr  (vlax-make-safearray vlax-vbDouble
                                  (cons 0 (1- (length flat)))))
  (vlax-safearray-fill arr flat)
  (setq obj (vla-addlightweightpolyline space arr))
  (vla-put-closed obj :vlax-true)
  (vl-catch-all-apply 'vla-put-elevation (list obj elev))
  obj
)

;;; Move OBJ onto *plb-layer*, creating that layer if needed
(defun plb:setlayer (obj doc / name)
  (setq name *plb-layer*)
  (if (and name (= (type name) 'STR) (/= name ""))
    (progn
      (if (not (tblsearch "LAYER" name))
        (vl-catch-all-apply 'vla-add (list (vla-get-layers doc) name))
      )
      (vl-catch-all-apply 'vla-put-layer (list obj name))
    )
  )
)

;;; ((xmin ymin) (xmax ymax)) over every point of every chain
(defun plb:extents (chains / x0 y0 x1 y1 ch p)
  (foreach ch chains
    (foreach p ch
      (if (or (null x0) (< (car  p) x0)) (setq x0 (car  p)))
      (if (or (null x1) (> (car  p) x1)) (setq x1 (car  p)))
      (if (or (null y0) (< (cadr p) y0)) (setq y0 (cadr p)))
      (if (or (null y1) (> (cadr p) y1)) (setq y1 (cadr p)))
    )
  )
  (if x0 (list (list x0 y0) (list x1 y1)))
)

;;; Every chain point as one flat list (for Hull and Box)
(defun plb:allpoints (chains / res ch p)
  (setq res '())
  (foreach ch chains
    (foreach p ch (setq res (cons p res)))
  )
  res
)


;;; ---- main command ----------------------------------------------

(defun c:PLBOUND
    (/ *error*
       acadobj doc space
       ss i ent chains ch p nobj nskip
       ans dist ext w h cell tol offc margin
       regs k rings ring cells pts bpts guard fit gx gy
       obj objs area elev)

  ;;; Local error handler - cleans up gracefully on cancel/error
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** PLBOUND Error: " msg))
    )
    (princ)
  )

  ;;; --- VLA setup ------------------------------------------------
  (setq acadobj (vlax-get-acad-object)
        doc     (vla-get-activedocument acadobj)
        space   (plb:activespace doc))

  ;;; --- selection ------------------------------------------------
  (princ "\nSelect objects to enclose (press ENTER when done): ")
  (setq ss (ssget))

  (if (null ss)
    (progn
      (princ "\nNothing selected - command cancelled.")
      (exit)
    )
  )

  ;;; --- options --------------------------------------------------
  (initget "Outline Regions Hull Box")
  (setq ans (getkword
              (strcat "\nBoundary type [Outline/Regions/Hull/Box] <"
                      *plb-mode* ">: ")))
  (if ans (setq *plb-mode* ans))

  ;;; --- read the geometry ----------------------------------------
  (princ "\nReading geometry ...")
  (setq i 0 chains '() nobj 0 nskip 0 *plb-nbbox* 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i)
          ch  (plb:objchains ent))
    (if ch
      (progn
        (foreach p ch (setq chains (cons p chains)))
        (setq nobj (1+ nobj))
      )
      (setq nskip (1+ nskip))
    )
    (setq i (1+ i))
  )

  (if (null chains)
    (progn
      (princ "\nNo usable geometry found in the selection.")
      (exit)
    )
  )

  (setq ext (plb:extents chains)
        w   (- (caadr ext) (caar  ext))
        h   (- (cadadr ext) (cadar ext))
        elev (getvar "ELEVATION"))

  (if (< (max w h) 1.0e-9)
    (progn
      (princ "\nThe selection has no size - nothing to enclose.")
      (exit)
    )
  )

  ;;; --- remaining options ----------------------------------------
  (setq cell (/ (max w h) (float *plb-res*)))
  (if (member *plb-mode* '("Outline" "Regions"))
    (progn
      (initget 6)                             ; no zero, no negative
      (setq dist (getdist
                   (strcat "\nTrace cell size <" (plb:fmt cell) ">: ")))
      (if dist (setq cell dist))
    )
  )

  (initget 4)                                 ; no negative
  (setq dist (getdist
               (strcat "\nOutward offset <" (plb:fmt *plb-offset*) ">: ")))
  (if dist (setq *plb-offset* dist))

  ;;; Keep the grid to a workable size.  A big offset needs a wide
  ;;; empty margin, so it counts towards the total as well; growing
  ;;; the cell shrinks both, so this settles after a pass or two.
  (if (member *plb-mode* '("Outline" "Regions"))
    (progn
      (setq guard 0 fit nil)
      (while (and (null fit) (< guard 20))
        (setq guard  (1+ guard)
              offc   (fix (+ 0.5 (/ *plb-offset* cell)))
              margin (+ 12 offc)
              gx     (+ (fix (/ w cell)) (* 2 margin) 2)
              gy     (+ (fix (/ h cell)) (* 2 margin) 2))
        (if (> (* gx gy) *plb-maxcells*)
          (setq cell (* cell (sqrt (/ (float (* gx gy))
                                      (float *plb-maxcells*)))))
          (setq fit T)
        )
      )
      (if (> guard 1)
        (princ (strcat "\nCell size coarsened to " (plb:fmt cell)
                       " to keep the grid workable."))
      )
    )
  )

  (setq rings '() k 0)

  (cond

    ;;; ---------- convex hull / bounding box ----------------------
    ((member *plb-mode* '("Hull" "Box"))
     (setq pts (plb:prefilter (plb:allpoints chains)))
     (if (= *plb-mode* "Box")
       (setq rings (list (plb:boxpts pts *plb-offset*)))
       (progn
         (setq bpts (plb:hull pts))
         (if (< (length bpts) 3)
           (progn
             (princ (strcat "\nThe selection is degenerate - every point"
                            " lies on one straight line."
                            "\nUse the Box option instead."))
             (exit)
           )
         )
         (if (> *plb-offset* 1.0e-9)
           (setq bpts (plb:offsetpoly bpts *plb-offset*))
         )
         (setq rings (list bpts))
       )
     )
    )

    ;;; ---------- traced outline ----------------------------------
    (T
     (setq tol (* *plb-simp* cell))
     (princ "\nBuilding grid ...")
     (plb:gridnew ext cell margin)
     (plb:markchains chains)
     (princ (strcat "\nGrid " (itoa *plb-nx*) " x " (itoa *plb-ny*)
                    ", " (itoa (length *plb-occ*)) " cells filled."))
     (setq regs (plb:regions))
     (princ (strcat "\n" (itoa (length regs)) " separate region"
                    (if (= (length regs) 1) "" "s") " found."))

     ;;; One loop around everything: measure the gaps, then grow the
     ;;; filled area enough to close them and shrink it back again.
     (if (and (= *plb-mode* "Outline") (> (length regs) 1))
       (progn
         (princ "\nMeasuring the gaps between them ...")
         (setq k (plb:bridge regs *plb-maxbridge*))
         (if (null k)
           (progn
             (princ (strcat "\nThose parts are too far apart to join"
                            " (further than " (itoa *plb-maxbridge*)
                            " cells)."
                            "\nDrawing one boundary per part instead -"
                            " use a larger cell size to join them."))
             (setq k 0)
           )
           (princ (strcat "\nBridging gaps of up to "
                          (plb:fmt (* k 2.0 cell)) "."))
         )
         ;;; the grid needs room for the growing
         (if (> (+ k offc 3) margin)
           (progn
             (setq margin (+ k offc 3))
             (plb:gridnew ext cell margin)
             (plb:markchains chains)
           )
         )
       )
     )

     ;;; Grow by one cell more than the bridge needs, so the boundary
     ;;; always sits just outside the geometry, then shrink back.
     (setq cells (plb:dilate (+ 1 offc k)))
     (if (> k 0) (plb:erode (plb:outerring cells) k))

     (setq regs (plb:regions))
     (plb:vnew)
     (princ "\nTracing ...")
     (foreach cells regs
       (if (>= (length cells) *plb-mincells*)
         (setq rings
               (cons (plb:simpring
                       (plb:dropcol (plb:toworld (plb:trace cells)))
                       tol)
                     rings))
       )
     )
     (setq rings (reverse rings))
    )
  )

  (if (null rings)
    (progn
      (princ "\nNothing large enough to enclose.")
      (exit)
    )
  )

  ;;; --- draw the boundaries --------------------------------------
  (setq objs '() area 0.0)
  (foreach ring rings
    (if (>= (length ring) 3)
      (progn
        (setq obj (plb:mkpline space ring elev))
        (plb:setlayer obj doc)
        (setq objs (cons obj objs)
              area (+ area (plb:area ring)))
      )
    )
  )

  ;;; --- report ---------------------------------------------------
  (princ (strcat "\n\n" *plb-mode* " boundary: "
                 (itoa (length objs)) " polyline"
                 (if (= (length objs) 1) "" "s")
                 " around " (itoa nobj) " object"
                 (if (= nobj 1) "" "s")))
  (if (> nskip 0)
    (princ (strcat "\n  " (itoa nskip)
                   " object(s) skipped (no outer edge)")))
  (if (> *plb-nbbox* 0)
    (princ (strcat "\n  " (itoa *plb-nbbox*)
                   " object(s) read as bounding boxes")))
  (if (member *plb-mode* '("Outline" "Regions"))
    (princ (strcat "\n  Cell size  =  " (plb:fmt cell))))
  (if (> *plb-offset* 0.0)
    (princ (strcat "\n  Offset     =  " (plb:fmt *plb-offset*))))
  (setq i 0)
  (foreach ring rings
    (setq i (1+ i))
    (princ (strcat "\n  Boundary " (itoa i) ":  "
                   (itoa (length ring)) " vertices, perimeter "
                   (plb:fmt (plb:perim ring))))
  )
  (princ (strcat "\n  Total area =  " (plb:fmt area)))
  (princ)
)


(princ "\nPLBOUND.lsp loaded.  Type  PLBOUND  to run.")
(princ)

;;; ============================================================ EOF
