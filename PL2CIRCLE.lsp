;;; ============================================================
;;; PL2CIRCLE.lsp  -  Circular Polyline to CIRCLE Converter
;;;
;;; Scans the selected polylines, keeps the ones whose outline is
;;; geometrically a circle, replaces each with a true CIRCLE entity
;;; and deletes the original polyline.
;;;
;;; Two shapes qualify:
;;;   1. Arc based - every segment is an arc (bulge), all of the arcs
;;;      share one centre and radius, and together they sweep a full
;;;      360 degrees.  This is the usual two-arc polyline you get from
;;;      a circle that was converted / imported as a polyline.
;;;   2. Faceted   - every segment is straight, there are at least
;;;      *P2C-MINSEGS* of them, and all vertices sit on one circle.
;;;      This catches circles that were flattened into many short
;;;      chords by an export / import round trip.
;;;
;;; Anything else - rectangles, slots, lens shapes, part circles,
;;; open polylines - is left untouched.
;;;
;;; Each new CIRCLE keeps the polyline's layer, colour, linetype,
;;; linetype scale, lineweight, transparency, thickness, elevation
;;; and extrusion direction.  Polyline width cannot be carried over
;;; (a CIRCLE has none); the summary reports how many were affected.
;;;
;;; Handles LWPOLYLINE and old style 2D POLYLINE entities.  3D
;;; polylines, meshes and curve / spline fitted polylines are skipped.
;;;
;;; The whole run sits inside one undo mark, so a single U undoes it.
;;;
;;; Command : PL2CIRCLE   (alias P2C)
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- user adjustable settings ----------------------------------

;;; Fit tolerance in drawing units.  A vertex or arc centre may sit
;;; this far off the fitted circle and still be accepted.
(if (not *P2C-TOL*) (setq *P2C-TOL* 1.0e-6))

;;; Minimum segment count before an all-straight polyline is accepted
;;; as a faceted circle.  Raise it if the drawing contains regular
;;; polygons you want left alone (a 12 sided polygon and a 12 facet
;;; circle are the same thing to the geometry).  Set it to a very
;;; large number to convert arc based polylines only.
(if (not *P2C-MINSEGS*) (setq *P2C-MINSEGS* 12))

;;; Points closer together than this count as the same point.
(if (not *P2C-FUZZ*) (setq *P2C-FUZZ* 1.0e-10))


;;; ---- small helpers ---------------------------------------------

;;; DXF group CODE from entity data ED, or DFLT when absent
(defun p2c:dxf (code ed dflt / pair)
  (if (setq pair (assoc code ed)) (cdr pair) dflt)
)

;;; Strip a vertex record (x y bulge) down to a plain point (x y)
(defun p2c:xy (v)
  (list (car v) (cadr v))
)

;;; Is layer LNAME locked?
(defun p2c:locked (lname / rec)
  (and (setq rec (tblsearch "LAYER" lname))
       (= 4 (logand 4 (cdr (assoc 70 rec))))
  )
)


;;; ---- vertex extraction -----------------------------------------

;;; Vertices of an LWPOLYLINE as ((x y bulge) ...), in order.
;;; In the entity data each bulge (42) trails the vertex (10) it
;;; belongs to, and is simply absent when the segment is straight.
(defun p2c:lwverts (ed / res pt)
  (setq res '())
  (foreach pair ed
    (cond
      ((= (car pair) 10)
       (setq pt  (cdr pair)
             res (cons (list (car pt) (cadr pt) 0.0) res)))
      ((and (= (car pair) 42) res)
       (setq res (cons (list (car  (car res))
                             (cadr (car res))
                             (cdr pair))
                       (cdr res))))
    )
  )
  (reverse res)
)

;;; Vertices of an old style 2D POLYLINE as ((x y bulge) ...).
;;; They are separate VERTEX entities that follow the header up to
;;; the SEQEND.  Vertices flagged 8 / 16 are spline fit data, not
;;; part of the drawn outline, so they are ignored.
(defun p2c:hvverts (ent / e ed pt flags res)
  (setq e   (entnext ent)
        res '())
  (while (and e
              (setq ed (entget e))
              (= (p2c:dxf 0 ed "") "VERTEX"))
    (setq flags (p2c:dxf 70 ed 0)
          pt    (p2c:dxf 10 ed nil))
    (if (and pt (zerop (logand 24 flags)))
      (setq res (cons (list (car pt) (cadr pt) (p2c:dxf 42 ed 0.0)) res))
    )
    (setq e (entnext e))
  )
  (reverse res)
)

;;; Vertices, elevation, extrusion and closed flag for a supported
;;; polyline: (verts elevation normal closed).  nil for anything this
;;; routine will not touch.
(defun p2c:geom (ent / ed etype flags verts elev nrm closed)
  (setq ed    (entget ent)
        etype (p2c:dxf 0   ed "")
        flags (p2c:dxf 70  ed 0)
        nrm   (p2c:dxf 210 ed '(0.0 0.0 1.0)))
  (cond
    ((= etype "LWPOLYLINE")
     (setq verts  (p2c:lwverts ed)
           elev   (p2c:dxf 38 ed 0.0)
           closed (= 1 (logand 1 flags))))

    ((= etype "POLYLINE")
     ;;; 2 = curve fit, 4 = spline fit, 8 = 3D polyline,
     ;;; 16 = polygon mesh, 64 = polyface mesh  ->  2+4+8+16+64 = 94
     (if (zerop (logand 94 flags))
       (setq verts  (p2c:hvverts ent)
             ;;; the header's point 10 is a dummy - only its Z, the
             ;;; polyline elevation, is meaningful
             elev   (caddr (p2c:dxf 10 ed '(0.0 0.0 0.0)))
             closed (= 1 (logand 1 flags)))))
  )
  (if verts (list verts elev nrm closed))
)


;;; ---- circle fitting --------------------------------------------

;;; Drop a vertex whenever it lands on the next one (cyclically), so
;;; zero length segments cannot upset the segment classification.
;;; The later vertex is the one kept - it carries the bulge that
;;; describes the segment leaving that point.
(defun p2c:dedupe (verts / n i res)
  (setq n   (length verts)
        i   0
        res '())
  (while (< i n)
    (if (> (distance (p2c:xy (nth i verts))
                     (p2c:xy (nth (rem (1+ i) n) verts)))
           *P2C-FUZZ*)
      (setq res (cons (nth i verts) res))
    )
    (setq i (1+ i))
  )
  (reverse res)
)

;;; Centre, radius and signed included angle of the arc that runs
;;; from P1 to P2 with the given BULGE.  nil for a straight or zero
;;; length segment.
;;;
;;;   sagitta s = bulge * chord / 2
;;;   radius  r = ((chord/2)^2 + s^2) / (2 s)          (signed)
;;;   centre    = chord midpoint + left normal * (r - s)
;;;   angle     = 4 * atan(bulge)                      (signed, CCW +)
(defun p2c:arcinfo (p1 p2 bulge / dx dy chord s r mx my ux uy)
  (setq dx    (- (car  p2) (car  p1))
        dy    (- (cadr p2) (cadr p1))
        chord (sqrt (+ (* dx dx) (* dy dy))))
  (if (or (< chord *P2C-FUZZ*) (< (abs bulge) 1.0e-12))
    nil
    (progn
      (setq s  (/ (* bulge chord) 2.0)
            r  (/ (+ (* (/ chord 2.0) (/ chord 2.0)) (* s s)) (* 2.0 s))
            mx (/ (+ (car  p1) (car  p2)) 2.0)
            my (/ (+ (cadr p1) (cadr p2)) 2.0)
            ux (/ (- dy) chord)          ; unit normal, 90 deg left
            uy (/ dx chord))
      (list (list (+ mx (* ux (- r s)))
                  (+ my (* uy (- r s))))
            (abs r)
            (* 4.0 (atan bulge)))
    )
  )
)

;;; Centre of the circle through three points, or nil when they are
;;; collinear.  Worked relative to A so the test stays meaningful
;;; a long way from the origin.
(defun p2c:circumcenter (a b c / bx by cx cy d ub uc)
  (setq bx (- (car  b) (car  a))
        by (- (cadr b) (cadr a))
        cx (- (car  c) (car  a))
        cy (- (cadr c) (cadr a))
        d  (* 2.0 (- (* bx cy) (* by cx)))
        ub (+ (* bx bx) (* by by))
        uc (+ (* cx cx) (* cy cy)))
  (if (< (abs d) (* 1.0e-10 (+ ub uc 1.0)))
    nil
    (list (+ (car  a) (/ (- (* cy ub) (* by uc)) d))
          (+ (cadr a) (/ (- (* bx uc) (* cx ub)) d)))
  )
)

;;; Mean radius of VERTS about CEN when every vertex is within
;;; tolerance of it, otherwise nil.
(defun p2c:onecircle (verts cen / sum rmean ok)
  (setq sum 0.0)
  (foreach v verts
    (setq sum (+ sum (distance cen (p2c:xy v)))))
  (setq rmean (/ sum (float (length verts)))
        ok    T)
  (foreach v verts
    (if (> (abs (- (distance cen (p2c:xy v)) rmean)) *P2C-TOL*)
      (setq ok nil)))
  (if (and ok (> rmean *P2C-TOL*)) rmean)
)

;;; Decide whether a cleaned, closed vertex list draws a circle.
;;; Returns (centre radius) or nil.
(defun p2c:fitcircle (verts / n i v1 v2 info arcs straights sweep
                            cen rad ok)
  (setq n         (length verts)
        arcs      0
        straights 0
        sweep     0.0
        ok        T)
  (if (< n 2)
    nil
    (progn
      ;;; classify every segment; the first arc sets the candidate
      ;;; centre / radius that the rest have to agree with
      (setq i 0)
      (while (< i n)
        (setq v1   (nth i verts)
              v2   (nth (rem (1+ i) n) verts)
              info (p2c:arcinfo v1 v2 (caddr v1)))
        (if info
          (progn
            (setq arcs  (1+ arcs)
                  sweep (+ sweep (caddr info)))
            (if (null cen)
              (setq cen (car info)
                    rad (cadr info))
              (if (or (> (distance cen (car info)) *P2C-TOL*)
                      (> (abs (- rad (cadr info))) *P2C-TOL*))
                (setq ok nil)))
          )
          (setq straights (1+ straights))
        )
        (setq i (1+ i))
      )

      (cond
        ;;; arc based: one common circle, swept exactly once round.
        ;;; The sweep test throws out doubled-back and overlapping
        ;;; outlines that happen to share a centre.
        ((and ok
              (> arcs 0)
              (= straights 0)
              (> rad *P2C-TOL*)
              (< (abs (- (abs sweep) (* 2.0 pi))) 1.0e-4))
         (list cen rad))

        ;;; faceted: fit a circle to three well spread vertices, then
        ;;; make every vertex prove it sits on that circle
        ((and (= arcs 0) (>= n *P2C-MINSEGS*))
         (if (and (setq cen (p2c:circumcenter (nth 0 verts)
                                              (nth (/ n 3) verts)
                                              (nth (/ (* 2 n) 3) verts)))
                  (setq rad (p2c:onecircle verts cen)))
           (list cen rad)))
      )
    )
  )
)


;;; ---- conversion ------------------------------------------------

;;; Entity level properties worth carrying across to the circle
(defun p2c:props (ed / res)
  (setq res '())
  (foreach code '(6 8 48 60 62 67 370 420 430 440)
    (if (assoc code ed)
      (setq res (cons (assoc code ed) res))))
  (reverse res)
)

;;; Did the polyline carry a width the circle cannot keep?
(defun p2c:haswidth (ed / found)
  (setq found nil)
  (foreach pair ed
    (if (and (member (car pair) '(40 41 43))
             (numberp (cdr pair))
             (> (abs (cdr pair)) 1.0e-12))
      (setq found T)))
  found
)

;;; Build the replacement CIRCLE.  Centre is an OCS point, so it goes
;;; in alongside the source elevation and extrusion untouched and the
;;; circle lands exactly where the polyline was.
(defun p2c:mkcircle (cen rad elev nrm ed)
  (entmake
    (append
      (list '(0 . "CIRCLE") '(100 . "AcDbEntity"))
      (p2c:props ed)
      (list '(100 . "AcDbCircle")
            (cons 10 (list (car cen) (cadr cen) elev))
            (cons 40 rad)
            (cons 39 (p2c:dxf 39 ed 0.0))
            (cons 210 nrm))))
)

;;; Convert one polyline.  Returns a status symbol:
;;;   converted / notclosed / notcircle / unsupported / locked / failed
(defun p2c:convert (ent / ed g verts elev nrm closed fit)
  (setq ed (entget ent)
        g  (p2c:geom ent))
  (cond
    ((null g) 'unsupported)
    ((p2c:locked (p2c:dxf 8 ed "0")) 'locked)
    (T
     (setq verts  (car   g)
           elev   (cadr  g)
           nrm    (caddr g)
           closed (nth 3 g))

     ;;; a polyline that merely ends where it started is closed too;
     ;;; p2c:dedupe drops the doubled vertex a moment later
     (if (and (not closed)
              (> (length verts) 2)
              (< (distance (p2c:xy (car verts)) (p2c:xy (last verts)))
                 *P2C-FUZZ*))
       (setq closed T))

     (setq verts (p2c:dedupe verts))

     (cond
       ((not closed) 'notclosed)
       ((null (setq fit (p2c:fitcircle verts))) 'notcircle)
       ((null (p2c:mkcircle (car fit) (cadr fit) elev nrm ed)) 'failed)
       (T (entdel ent) 'converted)
     )
    )
  )
)


;;; ---- main command ----------------------------------------------

(defun c:PL2CIRCLE
    (/ *error*
       doc ss i ent ed res newss
       n-conv n-open n-noncirc n-unsup n-lock n-fail n-width)

  ;;; Local error handler - close the undo mark whatever happens, so
  ;;; the drawing is never left mid-transaction
  (defun *error* (msg)
    (if doc (vla-endundomark doc))
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** PL2CIRCLE Error: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  ;;; --- selection ------------------------------------------------
  (princ "\nSelect polylines to convert (ENTER to process the whole space): ")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))

  ;;; Nothing picked - offer the whole of the current space, but make
  ;;; the user say so out loud first
  (if (null ss)
    (progn
      (initget "Yes No")
      (if (= "Yes" (getkword
                     (strcat "\nNothing selected.  Process ALL polylines in "
                             (getvar "CTAB") "? [Yes/No] <No>: ")))
        (setq ss (ssget "_X" (list '(0 . "LWPOLYLINE,POLYLINE")
                                   (cons 410 (getvar "CTAB")))))
      )
    )
  )

  (if (null ss)
    (progn
      (princ "\nNothing to do - command cancelled.")
      (exit)
    )
  )

  ;;; --- convert --------------------------------------------------
  (vla-startundomark doc)

  (setq n-conv    0
        n-open    0
        n-noncirc 0
        n-unsup   0
        n-lock    0
        n-fail    0
        n-width   0
        newss     (ssadd)
        i         0)

  (while (< i (sslength ss))
    (setq ent (ssname ss i)
          ed  (entget ent)
          res (p2c:convert ent))
    (cond
      ((eq res 'converted)
       (setq n-conv (1+ n-conv))
       (ssadd (entlast) newss)
       (if (p2c:haswidth ed) (setq n-width (1+ n-width))))
      ((eq res 'notclosed)   (setq n-open    (1+ n-open)))
      ((eq res 'notcircle)   (setq n-noncirc (1+ n-noncirc)))
      ((eq res 'unsupported) (setq n-unsup   (1+ n-unsup)))
      ((eq res 'locked)      (setq n-lock    (1+ n-lock)))
      ((eq res 'failed)      (setq n-fail    (1+ n-fail)))
    )
    (setq i (1+ i))
  )

  (vla-endundomark doc)

  ;;; --- report ---------------------------------------------------
  (princ (strcat "\nConverted " (itoa n-conv) " polyline"
                 (if (= n-conv 1) "" "s") " to circles."))
  (if (> n-noncirc 0)
    (princ (strcat "\n  " (itoa n-noncirc) " skipped - not circular.")))
  (if (> n-open 0)
    (princ (strcat "\n  " (itoa n-open) " skipped - not closed.")))
  (if (> n-unsup 0)
    (princ (strcat "\n  " (itoa n-unsup)
                   " skipped - 3D / mesh / fitted polyline.")))
  (if (> n-lock 0)
    (princ (strcat "\n  " (itoa n-lock) " skipped - locked layer.")))
  (if (> n-fail 0)
    (princ (strcat "\n  " (itoa n-fail) " failed - circle not created.")))
  (if (> n-width 0)
    (princ (strcat "\n  Note: " (itoa n-width)
                   " had a polyline width, which a circle cannot keep.")))

  ;;; leave the new circles selected, ready for the next command
  (if (> n-conv 0) (sssetfirst nil newss))

  (princ)
)

;;; Short alias
(defun c:P2C () (c:PL2CIRCLE))


(princ "\nPL2CIRCLE.lsp loaded.  Type  PL2CIRCLE  (or P2C) to run.")
(princ)

;;; ============================================================ EOF
