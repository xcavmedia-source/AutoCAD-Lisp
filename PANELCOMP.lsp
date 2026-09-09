;;; ============================================================
;;; PANELCOMP.lsp  -  Perforated Panel Comparison Tool
;;;
;;; Scans a selection of perforated panels - outlines on a dedicated
;;; "panel" layer, each holding circles, slots, arcs and logo
;;; geometry - and works out which panels are the same part.
;;;
;;; For every panel it builds a fingerprint from:
;;;   - the panel's overall width and height
;;;   - every piece of geometry inside it, described by its type,
;;;     its size, and its centre measured from the panel's own
;;;     lower-left corner
;;; Measuring from the panel corner is what makes the fingerprint
;;; independent of where the panel sits in the drawing, so two panels
;;; match when their patterns line up hole for hole.
;;;
;;; Matching is EXACT: a panel groups only with one carrying the same
;;; pattern in the same orientation. A rotated or mirrored copy is
;;; reported as its own type, which is the safe default when a panel
;;; can carry a logo or a directional pattern.
;;;
;;; Panels sharing a fingerprint get their own AutoCAD colour - the
;;; outline and everything inside it - and an MTEXT summary lists
;;; every type with its colour, size, hole count and quantity.
;;;
;;; PANELCOMP will also, on request, tag each panel with its type number
;;; and gather each type into a block of its own, so a whole type can be
;;; dragged clear of the sheet as one object.
;;;
;;; Panel outlines do NOT have to be closed polylines. Anything on the
;;; panel layer is grouped into outlines by shared endpoints, so a
;;; closed rectangle, an open polyline, a polyline with a gap in it,
;;; or four separate LINEs all resolve to a single panel.
;;;
;;; A panel is measured across the straight axis-aligned edges of its
;;; outline, never its bounding box. Corner treatment - a radius, a
;;; clip, a decorative sweep - and edge lines left overshooting their
;;; neighbour both push a bounding box outward, which would report the
;;; panel too big and, worse, move the corner that every hole position
;;; is measured from. POLYINFO measures the same way.
;;;
;;; Commands : PANELCOMP    compare, colour and report
;;;            PANELRESET   put a selection back to colour ByLayer
;;;            PANELCLOSE   rebuild outlines as closed rectangles
;;;            PANELDIFF    say why two panels did not match
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- configuration ---------------------------------------------

;;; How far two measurements can differ and still count as the same.
;;; 0.001 = one thousandth of an inch: tight enough to keep real part
;;; differences apart, loose enough to absorb rounding in the DWG.
;;; This is a true tolerance - measurements are compared against it, not
;;; rounded to it - so two holes a ten-thousandth of an inch apart are
;;; the same hole however they sit relative to any grid.
(setq *pc:tol* 0.001)

;;; How far apart two pieces of an outline can sit and still be read as
;;; belonging to the same panel. Raise it when one panel comes back as
;;; several - a corner left open, or bridged by a sweep that stands off
;;; from both edges, needs a gap wide enough to reach across. Keep it
;;; well under the space between neighbouring panels, or two panels will
;;; be read as one.
(setq *pc:gap* 0.1)

;;; Layer holding the panel outlines. Leave as "" and PANELCOMP asks
;;; you to pick one outline, then remembers the layer for the rest of
;;; the drawing session.
(setq *pc:panel-layer* "")

;;; Object types that are never part of a perforation pattern. Tags,
;;; dimensions and the like are ignored rather than fingerprinted, so
;;; a panel labelled in the drawing still matches its unlabelled twin.
(setq *pc:ignore*
  '("TEXT" "MTEXT" "ATTDEF" "ATTRIB" "DIMENSION" "LEADER" "MULTILEADER"
    "TOLERANCE" "VIEWPORT" "WIPEOUT"))

;;; ACI colours handed out to panel types, most common type first.
;;; Only the 24 pure hues are used, ordered so each one bisects the gap
;;; left by those before it: the fewer types a job has, the further
;;; apart on the colour wheel their colours land.
;;;
;;; ACI 1-6 are deliberately absent. They are exact duplicates of 10,
;;; 50, 90, 130, 170 and 210 - ACI 1 and ACI 10 are both pure red - so a
;;; list holding both sets gives two different types the same colour.
(setq *pc:colors*
  '( 10 130  70 190  40 160 100 220
     20 140  80 200  50 170 110 230
     30 150  90 210  60 180 120 240))

;;; Layer the type tags are written to, so they can be frozen, isolated
;;; or deleted without touching the panels.
(setq *pc:tag-layer* "PANEL-TYPE")


;;; ---- internal helpers ------------------------------------------

;;; Largest integer <= V. FIX truncates toward zero, which would fold
;;; the grid cells either side of an axis into one.
(defun pc:ifloor (v / f)
  (setq f (fix v))
  (if (and (minusp v) (/= (float f) v)) (1- f) f)
)

;;; Format VALUE as decimal inches, matching POLYINFO's output.
(defun pc:fmtinch (val)
  (strcat (rtos val 2 4) "\"")
)

;;; WCS bounding box of OBJ as ((minx miny minz) (maxx maxy maxz)),
;;; or nil for the few objects that refuse to report one.
(defun pc:bbox (obj / mn mx)
  (if (vl-catch-all-error-p
        (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx)))
    nil
    (list (vlax-safearray->list mn) (vlax-safearray->list mx))
  )
)

;;; Union of two bounding boxes.
(defun pc:bbunion (a b)
  (list (list (min (car  (car a)) (car  (car b)))
              (min (cadr (car a)) (cadr (car b)))
              0.0)
        (list (max (car  (cadr a)) (car  (cadr b)))
              (max (cadr (cadr a)) (cadr (cadr b)))
              0.0))
)

;;; Area of a bounding box.
(defun pc:bbarea (bb)
  (* (- (car  (cadr bb)) (car  (car bb)))
     (- (cadr (cadr bb)) (cadr (car bb))))
)

;;; True when bounding box A encloses bounding box B.
(defun pc:contains (a b / eps)
  (setq eps (* *pc:tol* 10.0))
  (and (<= (- (car  (car a)) eps) (car  (car b)))
       (<= (- (cadr (car a)) eps) (cadr (car b)))
       (>= (+ (car  (cadr a)) eps) (car  (cadr b)))
       (>= (+ (cadr (cadr a)) eps) (cadr (cadr b))))
)

;;; Read PRP off OBJ, returning 0.0 when there is no such property.
(defun pc:prop (obj prp / v)
  (setq v (vl-catch-all-apply 'vlax-get-property (list obj prp)))
  (if (vl-catch-all-error-p v) 0.0 v)
)

;;; Number of DXF group-10 points in ENT - vertex count for a
;;; lightweight polyline, control-point count for a spline.
(defun pc:vcount (ent / n)
  (setq n 0)
  (foreach pair (entget ent)
    (if (= (car pair) 10) (setq n (1+ n)))
  )
  n
)

;;; True when ENT is a shape that already closes on itself.
(defun pc:closedp (ent / ed typ)
  (setq ed  (entget ent)
        typ (cdr (assoc 0 ed)))
  (cond
    ((member typ '("CIRCLE" "ELLIPSE" "REGION")) t)
    ((member typ '("LWPOLYLINE" "POLYLINE" "SPLINE"))
     (and (assoc 70 ed) (= 1 (logand 1 (cdr (assoc 70 ed))))))
    (t nil)
  )
)

;;; True when boxes A and B come within TOL of each other.
(defun pc:boxnear (a b tol)
  (and (<= (- (car  (car a)) tol) (car  (cadr b)))
       (<= (- (car  (car b)) tol) (car  (cadr a)))
       (<= (- (cadr (car a)) tol) (cadr (cadr b)))
       (<= (- (cadr (car b)) tol) (cadr (cadr a))))
)

;;; The places an outline piece can join its neighbours: one box per
;;; straight segment, or the whole entity's box when it has none.
;;; Matching shared endpoints is not enough - corner lines are often
;;; left overshooting each other rather than trimmed, so the pieces of
;;; one panel cross without ever meeting end to end. Comparing segments
;;; catches those, and it still keeps neighbouring panels apart, since
;;; their edges stay a panel gap away from one another.
(defun pc:parts (ent bb / segs out p1 p2)
  (setq segs (pc:segs (list ent)) out '())
  (if segs
    (foreach s segs
      (setq p1  (car s)
            p2  (cadr s)
            out (cons (list (list (min (car  p1) (car  p2))
                                  (min (cadr p1) (cadr p2)))
                            (list (max (car  p1) (car  p2))
                                  (max (cadr p1) (cadr p2))))
                      out)))
    ;;; An arc or spline sets no size, but a corner sweep still has to
    ;;; join the outline it belongs to.
    (setq out (list bb)))
  out
)

;;; True when outline piece IT comes within TOL of any piece in group G.
;;; Both are (ents bb parts).
(defun pc:touches (it g tol / hit)
  (setq hit nil)
  (foreach o g
    (if (not hit)
      (foreach pa (caddr it)
        (if (not hit)
          (foreach pb (caddr o)
            (if (pc:boxnear pa pb tol) (setq hit t)))))))
  hit
)

;;; Walk a list of open outline pieces and gather them into loops by
;;; shared corners. Each returned group is one panel outline that was
;;; drawn as separate pieces rather than a single closed polyline.
(defun pc:chain (items tol / groups hits merged)
  (setq groups '())
  (foreach it items
    (setq hits '())
    (foreach g groups
      (if (pc:touches it g tol) (setq hits (cons g hits))))
    (if hits
      (progn
        ;;; The piece can bridge two groups that had not met yet, so
        ;;; fold every group it touches into one.
        (setq merged (list it))
        (foreach g hits
          (setq merged (append g merged)
                groups (vl-remove g groups)))
        (setq groups (cons merged groups)))
      (setq groups (cons (list it) groups)))
  )
  groups
)

;;; Merge VALS into MAP under key IDX.
(defun pc:merge (map idx vals / rec)
  (if (setq rec (assoc idx map))
    (subst (cons idx (append vals (cdr rec))) rec map)
    (cons (cons idx vals) map)
  )
)

;;; Every straight segment in an outline group, as (p1 p2) pairs.
;;; Arcs, splines and bulged polyline segments are deliberately left
;;; out: a radius, a clip or a decorative sweep at a corner is corner
;;; treatment, not panel edge, and only edges may set the size.
(defun pc:segs (ents / segs ent ed typ verts n i)
  (setq segs '())
  (foreach ent ents
    (setq ed  (entget ent)
          typ (cdr (assoc 0 ed)))
    (cond
      ((= typ "LINE")
       (setq segs (cons (list (cdr (assoc 10 ed)) (cdr (assoc 11 ed))) segs)))
      ((= typ "LWPOLYLINE")
       ;;; Walk the DXF in order, pairing each vertex with the bulge
       ;;; that follows it - a bulge is the arc in an arced segment,
       ;;; and is absent from the list when it is zero.
       (setq verts '())
       (foreach pair ed
         (cond
           ((= (car pair) 10)
            (setq verts (cons (list (cdr pair) 0.0) verts)))
           ((and (= (car pair) 42) verts)
            (setq verts (cons (list (car (car verts)) (cdr pair))
                              (cdr verts))))))
       (setq verts (reverse verts)
             n     (length verts)
             i     0)
       (while (< i (1- n))
         (if (< (abs (cadr (nth i verts))) 1e-8)
           (setq segs (cons (list (car (nth i verts))
                                  (car (nth (1+ i) verts)))
                            segs)))
         (setq i (1+ i)))
       (if (and (pc:closedp ent) (> n 1)
                (< (abs (cadr (nth (1- n) verts))) 1e-8))
         (setq segs (cons (list (car (nth (1- n) verts))
                                (car (car verts)))
                          segs))))
      ;;; Arcs, splines and heavy polylines contribute no straight edge.
    )
  )
  segs
)

;;; The panel rectangle, read off the outline's straight axis-aligned
;;; edges rather than its bounding box. A corner treatment that sweeps
;;; past the corner, or an edge line left overshooting its neighbour,
;;; inflates a bounding box and the panel then measures too big - and
;;; because hole positions are taken from the panel corner, every one of
;;; them shifts with it. Reading the edges instead ignores both.
;;; POLYINFO measures the same way, from orthogonal segments only.
;;; Returns nil when either axis lacks a pair of edges to measure
;;; between, leaving the caller to fall back to the bounding box.
(defun pc:extents (ents / eps xs ys p1 p2 dx dy x0 x1 y0 y1)
  (setq eps (* *pc:tol* 10.0) xs '() ys '())
  (foreach s (pc:segs ents)
    (setq p1 (car s)
          p2 (cadr s)
          dx (abs (- (car  p2) (car  p1)))
          dy (abs (- (cadr p2) (cadr p1))))
    (cond
      ((and (< dx eps) (> dy eps)) (setq xs (cons (car  p1) xs)))
      ((and (< dy eps) (> dx eps)) (setq ys (cons (cadr p1) ys)))
    )
  )
  (if xs (setq x0 (apply 'min xs) x1 (apply 'max xs)))
  (if ys (setq y0 (apply 'min ys) y1 (apply 'max ys)))
  (if (and x0 y0 (> (- x1 x0) eps) (> (- y1 y0) eps))
    (list (list x0 y0) (list x1 y1))
  )
)

;;; Draw a closed rectangle on EXT's corners, on layer LAY and taking
;;; its appearance from that layer. The outline it stands in for may be
;;; carrying a type colour from an earlier PANELCOMP run, which is not
;;; something to copy onto a fresh panel outline.
(defun pc:mkrect (space ext lay / arr obj)
  (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 7)))
  (vlax-safearray-fill arr
    (list (car  (car  ext)) (cadr (car  ext))
          (car  (cadr ext)) (cadr (car  ext))
          (car  (cadr ext)) (cadr (cadr ext))
          (car  (car  ext)) (cadr (cadr ext))))
  (setq obj (vla-addlightweightpolyline space (vlax-make-variant arr)))
  (vla-put-closed obj :vlax-true)
  (if lay (vl-catch-all-apply 'vla-put-layer (list obj lay)))
  (vl-catch-all-apply 'vla-put-color    (list obj 256))
  (vl-catch-all-apply 'vla-put-linetype (list obj "ByLayer"))
  obj
)

;;; Describe one piece of geometry inside a panel: its kind, its size,
;;; and its centre measured from the panel's lower-left corner at OX OY.
;;; Measuring from the panel corner is what makes it independent of
;;; where the panel sits in the drawing.
;;;
;;; The measurements stay as numbers. Rounding them to a grid first and
;;; comparing the results looks like it honours a tolerance but does
;;; not: two holes a ten-thousandth of an inch apart, either side of a
;;; grid line, round to different values and never match, however
;;; generous the tolerance is set. Only KIND is exact - an entity type,
;;; and a vertex count where the type alone says too little.
;;;
;;; Returns (kind width height cx cy a b), where a and b carry whatever
;;; the type needs beyond a bounding box.
(defun pc:sig (ent obj bb ox oy / mn mx typ kind a b)
  (setq mn   (car  bb)
        mx   (cadr bb)
        typ  (cdr (assoc 0 (entget ent)))
        kind typ
        a    0.0
        b    0.0)
  (cond
    ;;; A circle's bounding box already carries its diameter.
    ((= typ "CIRCLE"))
    ;;; Two arcs can share a bounding box and still be different cuts.
    ((= typ "ARC")
     (setq a (pc:prop obj 'Radius)
           b (pc:prop obj 'TotalAngle)))
    ((= typ "ELLIPSE")
     (setq a (pc:prop obj 'MajorRadius)
           b (pc:prop obj 'MinorRadius)))
    ;;; Slots and logo outlines: vertex count and enclosed area pull
    ;;; apart shapes that happen to share a bounding box.
    ((member typ '("LWPOLYLINE" "POLYLINE" "SPLINE"))
     ;;; Held as the side of the equivalent square rather than the area
     ;;; itself, so the one tolerance means the same thing here as it
     ;;; does everywhere else. A thousandth of a square inch on a fifty
     ;;; square inch logo is a far tighter demand than a thousandth of
     ;;; an inch on a length, and would split logos that match.
     (setq kind (strcat typ "|" (itoa (pc:vcount ent)))
           a    (sqrt (abs (pc:prop obj 'Area)))))
  )
  (list kind
        (- (car  mx) (car  mn))
        (- (cadr mx) (cadr mn))
        (- (/ (+ (car  mn) (car  mx)) 2.0) ox)
        (- (/ (+ (cadr mn) (cadr mx)) 2.0) oy)
        a b)
)

;;; True when two measurements are the same to within *pc:tol*.
(defun pc:close (a b)
  (<= (abs (- a b)) *pc:tol*)
)

;;; True when two pieces of geometry are the same piece: same kind, and
;;; every measurement agreeing to within the tolerance.
(defun pc:sigeq (a b)
  (and (= (car a) (car b))
       (pc:close (cadr   a) (cadr   b))
       (pc:close (caddr  a) (caddr  b))
       (pc:close (cadddr a) (cadddr b))
       (pc:close (nth 4  a) (nth 4  b))
       (pc:close (nth 5  a) (nth 5  b))
       (pc:close (nth 6  a) (nth 6  b)))
)

;;; Order two pieces so both panels' lists come out the same way round:
;;; left to right, then bottom to top, then by kind, then by size.
;;;
;;; Every comparison here is EXACT, deliberately. Ordering on "within a
;;; tolerance of each other" reads as the kinder choice and is a trap.
;;; It is not transitive - three holes spaced 0.0008 apart give a level
;;; with b, b level with c, but a below c - so the order comes out
;;; differently depending on which piece the drawing happens to list
;;; first. Worse, VL-SORT DISCARDS one of any two elements its compare
;;; function cannot separate, and a tolerance-based order cannot
;;; separate concentric pieces at all: a counterbore, an annulus, or a
;;; ring in a logo shares a centre and a kind with its neighbour, so one
;;; of the pair was silently deleted from the fingerprint - and which
;;; one depended on drawing order, which splits two identical panels.
;;;
;;; Size is in the order for the same reason: without it, two circles on
;;; one centre are inseparable. Under an exact order two pieces are
;;; level only when every measurement is identical, which is a genuine
;;; stacked duplicate and is meant to collapse.
(defun pc:siglt (a b)
  (cond
    ((/= (cadddr a) (cadddr b)) (< (cadddr a) (cadddr b)))   ; centre x
    ((/= (nth 4  a) (nth 4  b)) (< (nth 4  a) (nth 4  b)))   ; centre y
    ((not (= (car a) (car b)))  (< (car a) (car b)))          ; kind
    ((/= (cadr  a) (cadr  b))   (< (cadr  a) (cadr  b)))      ; width
    ((/= (caddr a) (caddr b))   (< (caddr a) (caddr b)))      ; height
    ((/= (nth 5  a) (nth 5  b)) (< (nth 5  a) (nth 5  b)))
    ((/= (nth 6  a) (nth 6  b)) (< (nth 6  a) (nth 6  b)))
  )
)

;;; True when two panels hold the same pieces in the same places. Both
;;; lists are already ordered, so this walks them in step and stops at
;;; the first real disagreement. Where a pair disagrees it tries them
;;; crossed over as well: two holes sitting within a tolerance of each
;;; other can come out of the sort either way round, and that is a
;;; difference in order, not in the panel.
(defun pc:listeq (a b / ok)
  (setq ok t)
  (while (and ok a b)
    (cond
      ((pc:sigeq (car a) (car b))
       (setq a (cdr a) b (cdr b)))
      ((and (cdr a) (cdr b)
            (pc:sigeq (car a) (cadr b))
            (pc:sigeq (cadr a) (car b)))
       (setq a (cddr a) b (cddr b)))
      (t (setq ok nil))
    )
  )
  (and ok (null a) (null b))
)

;;; Order groups by quantity, largest first. Written out longhand
;;; rather than with VL-SORT, which discards entries its compare
;;; function calls equal - here that would silently lose a panel type.
(defun pc:sortgroups (groups / out best)
  (setq out '())
  (while groups
    (setq best (car groups))
    (foreach g (cdr groups)
      (if (> (length (caddr g)) (length (caddr best))) (setq best g)))
    (setq out    (cons best out)
          groups (vl-remove best groups))
  )
  (reverse out)
)

;;; Name the twelve hues that have names worth printing. The rest are
;;; reported by number, which is what QSELECT and the layer tools want.
(defun pc:colorname (c / hit)
  (if (setq hit (assoc c '(( 10 . "red")          ( 30 . "orange")
                           ( 50 . "yellow")       ( 70 . "chartreuse")
                           ( 90 . "green")        (110 . "spring green")
                           (130 . "cyan")         (150 . "azure")
                           (170 . "blue")         (190 . "violet")
                           (210 . "magenta")      (230 . "rose"))))
    (strcat (itoa c) " (" (cdr hit) ")")
    (itoa c)
  )
)

;;; Make sure layer NAME exists, and return it, or nil if it cannot be
;;; created.
(defun pc:layer (doc name / lay)
  (setq lay (vl-catch-all-apply 'vla-item
                                (list (vla-get-layers doc) name)))
  (if (vl-catch-all-error-p lay)
    (setq lay (vl-catch-all-apply 'vla-add
                                  (list (vla-get-layers doc) name))))
  (if (vl-catch-all-error-p lay) nil name)
)

;;; Write a type tag at the top left of the panel at EXT, sized to the
;;; panel so it stays readable whatever the sheet scale.
(defun pc:tag (space ext txt col lay / h obj)
  (setq h   (max (/ (- (car (cadr ext)) (car (car ext))) 6.0) 0.0625)
        obj (vla-addtext space txt
              (vlax-3d-point (list (+ (car  (car  ext)) (* h 0.25))
                                   (- (cadr (cadr ext)) (* h 1.30))
                                   0.0))
              h))
  (vl-catch-all-apply 'vla-put-color (list obj col))
  (if lay (vl-catch-all-apply 'vla-put-layer (list obj lay)))
  obj
)

;;; Colour ENT, returning nil instead of failing. An object on a
;;; locked layer refuses the change, and an error there would abort the
;;; run part way through with the drawing half recoloured.
(defun pc:setcolor (ent col)
  (not (vl-catch-all-error-p
         (vl-catch-all-apply
           'vla-put-color (list (vlax-ename->vla-object ent) col))))
)

;;; A block name the drawing is not already using. An earlier run's
;;; blocks are left alone rather than redefined under whoever has them.
(defun pc:blockname (base / name n)
  (setq name base n 1)
  (while (tblsearch "BLOCK" name)
    (setq n (1+ n) name (strcat base "-" (itoa n))))
  name
)

;;; Gather ENTS into a block called NAME based at BASE, and drop one
;;; insert of it back exactly where they were, so the drawing looks
;;; unchanged but the whole type now moves as one object.
;;; Returns the name on success, nil if there was nothing left to gather
;;; or AutoCAD refused.
(defun pc:mkblock (name base ents / ss pt)
  (setq ss (ssadd)
        pt (list (car base) (cadr base) 0.0))
  ;;; An entity already consumed by an earlier block, or erased, gives
  ;;; nil from ENTGET and must not go into the selection.
  (foreach e ents (if (entget e) (ssadd e ss)))
  (if (> (sslength ss) 0)
    (if (vl-catch-all-error-p
          (vl-catch-all-apply
            '(lambda (nm p sel)
               (command "_.-BLOCK" nm p sel "")
               (command "_.-INSERT" nm p 1 1 0))
            (list name pt ss)))
      nil
      name)
  )
)

;;; Safely get the active space VLA object (model or paper).
(defun pc:activespace (doc)
  (if (and (= (getvar "TILEMODE") 0)
           (= (getvar "CVPORT") 1))
    (vla-get-paperspace doc)
    (vla-get-modelspace doc)
  )
)

;;; Ask the user which layer the panel outlines are on, by picking
;;; one. The answer is remembered for the rest of the session.
(defun pc:asklayer (/ pick)
  (if (or (null *pc:panel-layer*) (= *pc:panel-layer* ""))
    (progn
      (while (null pick)
        (setq pick (entsel "\nPick any one panel outline so I can learn its layer: "))
        (if (null pick)
          (princ "\n  Nothing under the crosshairs - try again, or Esc to cancel.")))
      (setq *pc:panel-layer* (cdr (assoc 8 (entget (car pick)))))))
  (princ (strcat "\nPanel outlines: layer \"" *pc:panel-layer* "\""))
  *pc:panel-layer*
)

;;; Split selection set SS into panel outlines and panel content.
;;; Returns (outlines content), where each outline is
;;; (list-of-enames bbox) and content is a flat list of enames.
;;; Everything on the panel layer becomes an outline; closed shapes
;;; stand alone, open pieces are chained into loops by their corners.
(defun pc:outlines (ss lay / i ent obj bb content opens g it ents ub outs)
  (setq content '() opens '() outs '() i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent))
    (cond
      ((/= (cdr (assoc 8 (entget ent))) lay)
       (if (not (member (cdr (assoc 0 (entget ent))) *pc:ignore*))
         (setq content (cons ent content))))
      ((null (setq bb (pc:bbox obj))) nil)
      ((pc:closedp ent)
       (setq outs (cons (list (list ent) bb) outs)))
      (t (setq opens (cons (list (list ent) bb (pc:parts ent bb)) opens))))
    (setq i (1+ i)))

  ;;; Fold the open pieces into loops and take each loop's extents.
  (foreach g (pc:chain opens *pc:gap*)
    (setq ents '() ub nil)
    (foreach it g
      (setq ents (append (car it) ents)
            ub   (if ub (pc:bbunion ub (cadr it)) (cadr it))))
    (setq outs (cons (list ents ub) outs)))

  (list outs content)
)


;;; ---- main command ----------------------------------------------

(defun c:PANELCOMP
    (/ *error* acadobj doc space lay ss split outs content
       cands panels recs wrappers idx r p
       sumw sumh cs cells cell k ix iy ix0 ix1 iy0 iy1
       ent obj bb cx cy eps hit orphans dupes empty ext boxed ans tags
       taglay
       hmap emap curidx cursigs curents
       sigs raw pw ph gkey groups g grp col cidx ncol locked nin q
       gents tagobj bx by bans bname bnames blocked oldecho oldsnap
       total ins-pt txtht mtext-obj content-str n)

  ;;; Local error handler - closes the undo group on cancel or error
  ;;; so the drawing is never left mid-transaction.
  (defun *error* (msg)
    ;;; Put back anything the run switched off before it stopped, or a
    ;;; cancel leaves the drawing with no object snaps.
    (if oldecho (setvar "CMDECHO" oldecho))
    (if oldsnap (setvar "OSMODE" oldsnap))
    (if doc (vla-endundomark doc))
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** PANELCOMP Error: " msg))
    )
    (princ)
  )

  ;;; --- VLA setup ------------------------------------------------
  (setq acadobj (vlax-get-acad-object)
        doc     (vla-get-activedocument acadobj)
        space   (pc:activespace doc))
  (vla-startundomark doc)

  ;;; --- which layer are the outlines on? -------------------------
  (setq lay (pc:asklayer))

  ;;; --- selection ------------------------------------------------
  (princ "\nSelect the panels to compare - outlines and perforations together: ")
  (setq ss (ssget))
  (if (null ss)
    (progn (princ "\nNothing selected - command cancelled.") (exit)))

  ;;; --- sort the selection into outlines and contents ------------
  (setq split   (pc:outlines ss lay)
        cands   (car  split)
        content (cadr split))

  (if (null cands)
    (progn
      (princ (strcat "\nNo panel outlines found on layer \"" lay
                     "\" in that selection."))
      (exit)))

  ;;; --- drop any outline that wraps the others -------------------
  ;;; A border drawn around the whole sheet lives on the same layer as
  ;;; the panels, and is recognised by the panels sitting inside it.
  ;;; It takes two: a border holds the whole run, while a panel with one
  ;;; stray piece of line left inside it holds one, and dropping that
  ;;; would quietly lose a real panel - it would come back uncoloured,
  ;;; untagged and uncounted, which is worse than a border kept.
  (setq panels '() wrappers 0)
  (foreach p cands
    (setq nin 0)
    (foreach q cands
      (if (and (not (equal q p))
               (pc:contains (cadr p) (cadr q))
               (< (pc:bbarea (cadr q)) (* 0.99 (pc:bbarea (cadr p)))))
        (setq nin (1+ nin))))
    (if (> nin 1)
      (setq wrappers (1+ wrappers))
      (setq panels (cons p panels))))

  (if (null panels)
    (progn (princ "\nEvery outline found contains another - nothing to compare.")
           (exit)))

  ;;; --- number the panels: (idx enames min max) ------------------
  ;;; The panel rectangle comes from the outline's straight edges, not
  ;;; its bounding box, so corner treatment and overshooting lines do
  ;;; not inflate the size. Everything downstream - the reported size,
  ;;; and the corner every hole position is measured from - rests on it.
  (setq recs '() idx 0 boxed 0)
  (foreach p panels
    (if (setq ext (pc:extents (car p)))
      nil
      (setq ext (cadr p) boxed (1+ boxed)))
    (setq recs (cons (list idx (car p) (car ext) (cadr ext)) recs)
          idx  (1+ idx)))
  (setq recs (reverse recs))

  ;;; --- bucket the panels onto a grid ----------------------------
  ;;; Cells sized to the average panel keep the hole-to-panel lookup
  ;;; near constant time instead of testing every hole against every
  ;;; panel, which matters once a sheet runs to thousands of holes.
  (setq sumw 0.0 sumh 0.0)
  (foreach r recs
    (setq sumw (+ sumw (- (car  (cadddr r)) (car  (caddr r))))
          sumh (+ sumh (- (cadr (cadddr r)) (cadr (caddr r))))))
  (setq cs (max (/ sumw idx) (/ sumh idx)))
  (if (<= cs 0.0) (setq cs 1.0))

  ;;; Widened by the same slack the containment test below allows, or a
  ;;; hole sitting just outside a panel edge - one broken by the edge -
  ;;; can fall in a cell the panel was never registered in and be
  ;;; written off as an orphan.
  (setq cells '() eps (* *pc:tol* 10.0))
  (foreach r recs
    (setq ix0 (pc:ifloor (/ (- (car  (caddr  r)) eps) cs))
          ix1 (pc:ifloor (/ (+ (car  (cadddr r)) eps) cs))
          iy0 (pc:ifloor (/ (- (cadr (caddr  r)) eps) cs))
          iy1 (pc:ifloor (/ (+ (cadr (cadddr r)) eps) cs))
          ix  ix0)
    (while (<= ix ix1)
      (setq iy iy0)
      (while (<= iy iy1)
        (setq k    (strcat (itoa ix) "," (itoa iy))
              cell (assoc k cells))
        (if cell
          (setq cells (subst (cons k (cons r (cdr cell))) cell cells))
          (setq cells (cons (list k r) cells)))
        (setq iy (1+ iy)))
      (setq ix (1+ ix))))

  ;;; --- put every piece of content inside its panel --------------
  (setq hmap '() emap '() orphans 0 curidx nil
        cursigs '() curents '())
  (foreach ent content
    (setq obj (vlax-ename->vla-object ent)
          bb  (pc:bbox obj))
    (if bb
      (progn
        (setq cx  (/ (+ (car  (car bb)) (car  (cadr bb))) 2.0)
              cy  (/ (+ (cadr (car bb)) (cadr (cadr bb))) 2.0)
              k   (strcat (itoa (pc:ifloor (/ cx cs))) ","
                          (itoa (pc:ifloor (/ cy cs))))
              hit nil)
        (foreach r (cdr (assoc k cells))
          (if (and (null hit)
                   (<= (- (car  (caddr  r)) eps) cx)
                   (<= cx (+ (car  (cadddr r)) eps))
                   (<= (- (cadr (caddr  r)) eps) cy)
                   (<= cy (+ (cadr (cadddr r)) eps)))
            (setq hit r)))
        ;;; A border or title block drawn on some other layer can have
        ;;; its centre land inside a panel. Nothing bigger than the
        ;;; panel itself is a hole in it.
        (if (and hit
                 (<= (- (car  (cadr bb)) (car  (car bb)))
                     (+ (- (car  (cadddr hit)) (car  (caddr hit))) eps))
                 (<= (- (cadr (cadr bb)) (cadr (car bb)))
                     (+ (- (cadr (cadddr hit)) (cadr (caddr hit))) eps)))
          (progn
            ;;; Holes come out of the selection panel by panel, so hold
            ;;; the run for the current panel and write it back once
            ;;; the run ends rather than re-keying the map per hole.
            (if (not (equal (car hit) curidx))
              (progn
                (if curidx
                  (setq hmap (pc:merge hmap curidx cursigs)
                        emap (pc:merge emap curidx curents)))
                (setq curidx (car hit) cursigs '() curents '())))
            (setq cursigs (cons (pc:sig ent obj bb
                                        (car  (caddr hit))
                                        (cadr (caddr hit)))
                                cursigs)
                  curents (cons ent curents)))
          (setq orphans (1+ orphans))))
      (setq orphans (1+ orphans))))
  (if curidx
    (setq hmap (pc:merge hmap curidx cursigs)
          emap (pc:merge emap curidx curents)))

  ;;; --- fingerprint each panel and group the matches -------------
  (setq groups '() dupes 0 empty 0)
  (foreach r recs
    (setq sigs (cdr (assoc (car r) hmap))
          raw  (length sigs)
          pw   (- (car  (cadddr r)) (car  (caddr r)))
          ph   (- (cadr (cadddr r)) (cadr (caddr r)))
          ;;; Sorting makes the fingerprint independent of the order
          ;;; the holes were drawn in.
          sigs (if sigs (vl-sort sigs 'pc:siglt) '())
          ;;; Hole count is the only cheap key that can be trusted: it
          ;;; is a whole number, so unlike a rounded measurement it
          ;;; cannot fall either side of anything. Size is checked with
          ;;; the tolerance below, alongside the holes themselves.
          gkey (itoa raw))
    (if (/= raw (length sigs)) (setq dupes (1+ dupes)))
    ;;; A panel with nothing in it is usually not a panel: it is a piece
    ;;; of some outline that failed to join the rest of its own.
    (if (= raw 0) (setq empty (1+ empty)))
    (setq grp nil)
    (foreach g groups
      (if (and (null grp)
               (= (car g) gkey)
               (pc:close pw (car  (cadddr g)))
               (pc:close ph (cadr (cadddr g)))
               (pc:listeq sigs (cadr g)))
        (setq grp g)))
    (if grp
      ;;; The group keeps the fingerprint of its FIRST member. Replacing
      ;;; it with each new arrival lets the yardstick walk: panel 2 a
      ;;; tolerance from panel 1, panel 3 a tolerance from panel 2, and
      ;;; nothing bounds how far the last is from the first. Six panels
      ;;; drifting 0.0008 each ended up 0.004 apart - four times the
      ;;; tolerance - and still counted as one part.
      (setq groups (subst (list gkey (cadr grp) (cons r (caddr grp))
                                (cadddr grp))
                          grp groups))
      (setq groups (cons (list gkey sigs (list r) (list pw ph raw))
                         groups))))

  ;;; --- most common type first, then colour and report -----------
  (setq groups (pc:sortgroups groups)
        ncol   (length *pc:colors*)
        cidx   0
        total  0
        locked 0
        content-str (strcat "{\\H1.25x;\\L;Panel Comparison\\l}\\P" "\\P"))

  ;;; Colour alone stops separating types once there are more types
  ;;; than colours, so the tag is offered as the default at that point.
  (initget "Yes No")
  (setq ans (getkword
              (strcat "\n" (itoa (length groups)) " panel type(s) found."
                      " Tag each panel with its type number? [Yes/No] <"
                      (if (> (length groups) ncol) "Yes" "No") ">: ")))
  (if (null ans)
    (setq ans (if (> (length groups) ncol) "Yes" "No")))
  (setq tags 0)
  (if (= ans "Yes") (setq taglay (pc:layer doc *pc:tag-layer*)))

  (initget "Yes No")
  (setq bans (getkword
               "\nPut each type into its own block, so it moves as one? [Yes/No] <No>: "))
  (if (null bans) (setq bans "No"))
  (setq blocked 0 bnames '())
  ;;; -BLOCK and -INSERT take their points from this code, not the
  ;;; cursor, so running snaps would only drag them off the corner.
  (if (= bans "Yes")
    (progn
      (setq oldecho (getvar "CMDECHO")
            oldsnap (getvar "OSMODE"))
      (setvar "CMDECHO" 0)
      (setvar "OSMODE" 0)))

  (foreach g groups
    (setq col (nth (rem cidx ncol) *pc:colors*)
          pw  (car   (cadddr g))
          ph  (cadr  (cadddr g))
          raw (caddr (cadddr g))
          n   (length (caddr g))
          total (+ total n))
    ;;; Colour the outline and everything inside it, as an object
    ;;; override so the layer's own colour is left alone.
    (setq gents '() bx nil by nil)
    (foreach r (caddr g)
      (foreach ent (cadr r)
        (if (not (pc:setcolor ent col)) (setq locked (1+ locked)))
        (setq gents (cons ent gents)))
      (foreach ent (cdr (assoc (car r) emap))
        (if (not (pc:setcolor ent col)) (setq locked (1+ locked)))
        (setq gents (cons ent gents)))
      (if (= ans "Yes")
        (progn
          (setq tagobj (vl-catch-all-apply
                         'pc:tag (list space (list (caddr r) (cadddr r))
                                       (strcat "T" (itoa (1+ cidx)))
                                       col taglay)))
          (if (not (vl-catch-all-error-p tagobj))
            (setq tags  (1+ tags)
                  gents (cons (vlax-vla-object->ename tagobj) gents)))))
      ;;; The block is based on the lower-left corner of the whole type,
      ;;; so the insert lands exactly over the geometry it replaces.
      (if (or (null bx) (< (car  (caddr r)) bx)) (setq bx (car  (caddr r))))
      (if (or (null by) (< (cadr (caddr r)) by)) (setq by (cadr (caddr r)))))

    (setq bname nil)
    (if (= bans "Yes")
      (if (setq bname (pc:mkblock (pc:blockname
                                    (strcat "PANEL-TYPE-" (itoa (1+ cidx))))
                                  (list bx by) gents))
        (setq blocked (1+ blocked)
              bnames  (cons bname bnames))))

    (setq content-str
          (strcat content-str
                  "{\\H1.0x;\\L;TYPE " (itoa (1+ cidx))
                  "  -  colour " (pc:colorname col) "\\l}\\P"
                  "  Qty    =  " (itoa n) "\\P"
                  "  Size   =  " (pc:fmtinch pw) " x " (pc:fmtinch ph) "\\P"
                  "  Holes  =  " (itoa raw) "\\P"
                  (if bname (strcat "  Block  =  " bname "\\P") "")
                  "\\P")
          cidx (1+ cidx)))

  (if (= bans "Yes")
    (progn (setvar "CMDECHO" oldecho) (setvar "OSMODE" oldsnap)))

  ;;; --- totals and anything worth flagging -----------------------
  (setq content-str
        (strcat content-str
                "{\\H1.0x;\\L;--- TOTAL\\l}\\P"
                "  Panels        =  " (itoa total) "\\P"
                "  Unique types  =  " (itoa (length groups))))
  (if (> cidx ncol)
    (setq content-str
          (strcat content-str "\\P\\P"
                  "  NOTE: " (itoa (length groups)) " types share "
                  (itoa ncol) " colours - colours repeat past type "
                  (itoa ncol) ".")))
  (if (> wrappers 0)
    (setq content-str
          (strcat content-str "\\P\\P"
                  "  NOTE: " (itoa wrappers)
                  " outline(s) skipped as sheet borders - each encloses"
                  " two or more panels. A real panel that came back"
                  " uncoloured and untagged is one of these: something"
                  " else on the panel layer is sitting inside it.")))
  (if (> orphans 0)
    (setq content-str
          (strcat content-str "\\P\\P"
                  "  NOTE: " (itoa orphans)
                  " object(s) fell outside every panel and were ignored.")))
  (if (> empty 0)
    (setq content-str
          (strcat content-str "\\P\\P"
                  "  NOTE: " (itoa empty)
                  " panel(s) hold no perforations at all. If those should"
                  " have holes, their outline was read as more than one"
                  " panel - some piece of it sits further than "
                  (rtos *pc:gap* 2 3) "\" from the rest. Raise *pc:gap*"
                  " at the top of PANELCOMP.lsp and run it again.")))
  (if (> boxed 0)
    (setq content-str
          (strcat content-str "\\P\\P"
                  "  NOTE: " (itoa boxed)
                  " panel(s) had no clear pair of straight edges on one"
                  " axis and were measured by bounding box instead."
                  " Check their size below.")))
  (if (> tags 0)
    (setq content-str
          (strcat content-str "\\P\\P"
                  "  " (itoa tags) " panel(s) tagged on layer "
                  *pc:tag-layer* ".")))
  (if (> blocked 0)
    (setq content-str
          (strcat content-str "\\P\\P"
                  "  " (itoa blocked) " type(s) gathered into blocks."
                  " Each one moves as a single object; explode it to get"
                  " the panels back.")))
  (if (and (= bans "Yes") (< blocked (length groups)))
    (setq content-str
          (strcat content-str "\\P\\P"
                  "  NOTE: " (itoa (- (length groups) blocked))
                  " type(s) could not be blocked - locked layer?")))
  (if (and (= ans "Yes") (< tags total))
    (setq content-str
          (strcat content-str "\\P\\P"
                  "  NOTE: " (itoa (- total tags))
                  " panel(s) could not be tagged.")))
  (if (> locked 0)
    (setq content-str
          (strcat content-str "\\P\\P"
                  "  NOTE: " (itoa locked)
                  " object(s) would not take a colour - locked layer?")))
  (if (> dupes 0)
    (setq content-str
          (strcat content-str "\\P\\P"
                  "  NOTE: " (itoa dupes)
                  " panel(s) hold stacked duplicate geometry and are"
                  " kept as their own type. Worth cleaning up.")))

  ;;; --- place the summary ----------------------------------------
  (initget 1)
  (setq ins-pt (getpoint "\nSpecify insertion point for the summary: ")
        txtht  (max (getvar "TEXTSIZE") 2.5)
        mtext-obj (vla-addmtext space (vlax-3d-point ins-pt) 0.0 content-str))
  (vla-put-height mtext-obj txtht)

  (vla-endundomark doc)
  (princ (strcat "\nDone - " (itoa total) " panel(s), "
                 (itoa (length groups)) " unique type(s)."))
  (if (> orphans 0)
    (princ (strcat "  " (itoa orphans) " object(s) outside any panel.")))
  (princ)
)


;;; ---- put colours back ------------------------------------------

(defun c:PANELRESET (/ *error* acadobj doc ss i n locked erased ent)

  (defun *error* (msg)
    (if doc (vla-endundomark doc))
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** PANELRESET Error: " msg))
    )
    (princ)
  )

  (setq acadobj (vlax-get-acad-object)
        doc     (vla-get-activedocument acadobj))
  (vla-startundomark doc)

  (princ "\nSelect objects to put back to colour ByLayer: ")
  (setq ss (ssget))
  (if (null ss)
    (progn (princ "\nNothing selected - command cancelled.") (exit)))

  (setq i 0 n 0 locked 0 erased 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    ;;; A type tag is PANELCOMP's own annotation, not drawing content,
    ;;; so resetting takes it away rather than colouring it ByLayer.
    (if (= (cdr (assoc 8 (entget ent))) *pc:tag-layer*)
      (if (not (vl-catch-all-error-p
                 (vl-catch-all-apply 'entdel (list ent))))
        (setq erased (1+ erased)))
      (if (pc:setcolor ent 256)
        (setq n (1+ n))
        (setq locked (1+ locked))))
    (setq i (1+ i)))

  (vla-endundomark doc)
  (princ (strcat "\n" (itoa n) " object(s) set back to ByLayer."))
  (if (> erased 0)
    (princ (strcat "  " (itoa erased) " type tag(s) erased.")))
  (if (> locked 0)
    (princ (strcat "  " (itoa locked)
                   " refused the change - locked layer?")))
  (princ)
)


;;; ---- rebuild panel outlines as closed rectangles ---------------
;;; Setting a polyline's Closed flag only joins its last vertex to its
;;; first, which cuts the corner whenever those vertices are not the
;;; corners - the outline then encloses the wrong panel. This draws a
;;; fresh closed rectangle on the panel's real corners instead, taken
;;; from the straight edges of the outline so that corner treatment and
;;; overshooting lines cannot shift them.
;;;
;;; The original geometry is kept unless you answer Yes to replacing it.
;;; Keeping it is the safe answer when the corners carry detail worth
;;; holding on to; the new rectangle is drawn either way.

(defun c:PANELCLOSE (/ *error* acadobj doc space lay ss split outs
                       p ents ext ans built skipped)

  (defun *error* (msg)
    (if doc (vla-endundomark doc))
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** PANELCLOSE Error: " msg))
    )
    (princ)
  )

  (setq acadobj (vlax-get-acad-object)
        doc     (vla-get-activedocument acadobj)
        space   (pc:activespace doc))
  (vla-startundomark doc)

  (setq lay (pc:asklayer))
  (princ "\nSelect the panel outlines to rebuild: ")
  (setq ss (ssget))
  (if (null ss)
    (progn (princ "\nNothing selected - command cancelled.") (exit)))

  (initget "Yes No")
  (setq ans (getkword
              "\nDelete the old outline geometry once rebuilt? [Yes/No] <No>: "))
  (if (null ans) (setq ans "No"))

  (setq split (pc:outlines ss lay)
        outs  (car split)
        built 0
        skipped 0)

  (foreach p outs
    (setq ents (car p)
          ext  (pc:extents ents))
    ;;; No pair of straight edges on one axis means there is nothing
    ;;; solid to put a corner on, and a guessed rectangle would be worse
    ;;; than none.
    (if ext
      (progn
        (if (vl-catch-all-error-p
              (vl-catch-all-apply 'pc:mkrect (list space ext lay)))
          (setq skipped (1+ skipped))
          (progn
            (if (= ans "Yes")
              (foreach e ents (vl-catch-all-apply 'entdel (list e))))
            (setq built (1+ built)))))
      (setq skipped (1+ skipped))))

  (vla-endundomark doc)
  (princ (strcat "\nRebuilt " (itoa built)
                 " outline(s) as closed rectangles on their real corners,"
                 " on layer \"" lay "\"."))
  (if (= ans "Yes")
    (princ " Old geometry deleted.")
    (princ " Old geometry kept - erase it once you have checked the sizes."))
  (if (> skipped 0)
    (princ (strcat "\n" (itoa skipped)
                   " left alone - no clear pair of straight edges to"
                   " measure between, or the outline is locked.")))
  (princ)
)


;;; ---- why two panels did not match ------------------------------
;;; When two panels look identical but PANELCOMP puts them in different
;;; types, this says which piece disagreed and by how much, instead of
;;; leaving it to be guessed at.

;;; Everything one panel's fingerprint is built from, for a selection
;;; holding exactly one panel. Returns (ext from-edges sigs) or nil.
(defun pc:panelinfo (ss lay / split outs content o ext edg ent obj bb
                              cx cy sigs)
  (setq split   (pc:outlines ss lay)
        outs    (car  split)
        content (cadr split))
  (if (/= (length outs) 1)
    nil
    (progn
      (setq o   (car outs)
            ext (pc:extents (car o))
            edg (if ext t nil))
      (if (null ext) (setq ext (cadr o)))
      (setq sigs '())
      (foreach ent content
        (setq obj (vlax-ename->vla-object ent)
              bb  (pc:bbox obj))
        (if bb
          (progn
            (setq cx (/ (+ (car  (car bb)) (car  (cadr bb))) 2.0)
                  cy (/ (+ (cadr (car bb)) (cadr (cadr bb))) 2.0))
            (if (and (>= cx (car  (car ext))) (<= cx (car  (cadr ext)))
                     (>= cy (cadr (car ext))) (<= cy (cadr (cadr ext))))
              (setq sigs (cons (pc:sig ent obj bb
                                       (car  (car ext))
                                       (cadr (car ext)))
                               sigs))))))
      (list ext edg (vl-sort sigs 'pc:siglt))
    )
  )
)

;;; The pieces of A that no piece of B answers to.
(defun pc:unmatched (a b / out hit)
  (setq out '())
  (foreach sg a
    (setq hit nil)
    (foreach o b (if (and (null hit) (pc:sigeq sg o)) (setq hit t)))
    (if (null hit) (setq out (cons sg out))))
  (reverse out)
)

;;; The piece of LST sitting closest to SG, whatever its kind.
(defun pc:nearest (sg lst / best bd d)
  (foreach o lst
    (setq d (+ (abs (- (cadddr sg) (cadddr o)))
               (abs (- (nth 4 sg) (nth 4 o)))))
    (if (or (null best) (< d bd)) (setq best o bd d)))
  best
)

;;; One line describing a piece: kind, size, and where it sits on the
;;; panel.
(defun pc:sigline (sg)
  (strcat (car sg) "  " (pc:fmtinch (cadr sg)) " x " (pc:fmtinch (caddr sg))
          "  at (" (pc:fmtinch (cadddr sg)) ", " (pc:fmtinch (nth 4 sg)) ")")
)

(defun c:PANELDIFF
    (/ *error* acadobj doc space lay ss1 ss2 a b exta extb sga sgb
       ua ub sg near i content-str ins-pt txtht)

  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** PANELDIFF Error: " msg))
    )
    (princ)
  )

  (setq acadobj (vlax-get-acad-object)
        doc     (vla-get-activedocument acadobj)
        space   (pc:activespace doc)
        lay     (pc:asklayer))

  (princ "\nSelect the FIRST panel - outline and perforations: ")
  (setq ss1 (ssget))
  (princ "\nSelect the SECOND panel - outline and perforations: ")
  (setq ss2 (ssget))
  (if (or (null ss1) (null ss2))
    (progn (princ "\nTwo panels are needed - command cancelled.") (exit)))

  (setq a (pc:panelinfo ss1 lay)
        b (pc:panelinfo ss2 lay))
  (if (or (null a) (null b))
    (progn
      (princ (strcat "\nEach selection must hold exactly one panel"
                     " outline on layer \"" lay "\". Select one panel at"
                     " a time."))
      (exit)))

  (setq exta (car a) extb (car b)
        sga  (caddr a) sgb (caddr b)
        ua   (pc:unmatched sga sgb)
        ub   (pc:unmatched sgb sga))

  (setq content-str
    (strcat
      "{\\H1.25x;\\L;Why these two panels differ\\l}\\P\\P"
      "  Tolerance      =  " (pc:fmtinch *pc:tol*) "\\P\\P"
      "  A size         =  "
      (pc:fmtinch (- (car  (cadr exta)) (car  (car exta)))) " x "
      (pc:fmtinch (- (cadr (cadr exta)) (cadr (car exta))))
      (if (cadr a) "   (from edges)" "   (from bounding box)") "\\P"
      "  B size         =  "
      (pc:fmtinch (- (car  (cadr extb)) (car  (car extb)))) " x "
      (pc:fmtinch (- (cadr (cadr extb)) (cadr (car extb))))
      (if (cadr b) "   (from edges)" "   (from bounding box)") "\\P"
      "  A pieces       =  " (itoa (length sga)) "\\P"
      "  B pieces       =  " (itoa (length sgb)) "\\P\\P"))

  ;;; A panel measured off its edges and one measured off its bounding
  ;;; box have their corners in different places, and every position on
  ;;; them is then read from a different origin. That alone will split
  ;;; two identical panels, so it is worth saying loudly.
  (if (not (equal (cadr a) (cadr b)))
    (setq content-str
          (strcat content-str
                  "  ONE PANEL WAS MEASURED OFF ITS EDGES AND THE OTHER"
                  " OFF ITS BOUNDING BOX, so their corners are in"
                  " different places and every hole position is read"
                  " from a different origin. Fix that first.\\P\\P")))

  (if (and (null ua) (null ub))
    (setq content-str
          (strcat content-str
                  "  Every piece matches. These two panels are the same"
                  " part - if PANELCOMP split them, the difference is in"
                  " the panel size above."))
    (progn
      (setq content-str
            (strcat content-str
                    "{\\H1.0x;\\L;" (itoa (length ua))
                    " piece(s) of A unaccounted for in B\\l}\\P"))
      (setq i 0)
      (foreach sg ua
        (if (< i 12)
          (progn
            (setq near (pc:nearest sg sgb)
                  i    (1+ i))
            (setq content-str
                  (strcat content-str
                          "  " (itoa i) ". A: " (pc:sigline sg) "\\P"))
            (if near
              (setq content-str
                (strcat content-str
                  "      B: " (pc:sigline near) "\\P"
                  "      -> "
                  (cond
                    ((not (= (car sg) (car near)))
                     (strcat "different kind: " (car sg) " against "
                             (car near)))
                    (t
                     (strcat "same kind, off by "
                             (pc:fmtinch
                               (max (abs (- (cadddr sg) (cadddr near)))
                                    (abs (- (nth 4 sg) (nth 4 near)))))
                             " in position, "
                             (pc:fmtinch
                               (max (abs (- (cadr  sg) (cadr  near)))
                                    (abs (- (caddr sg) (caddr near)))))
                             " in size")))
                  "\\P"))
              (setq content-str
                    (strcat content-str
                            "      -> nothing comparable in B\\P")))))
      )
      (if (> (length ua) 12)
        (setq content-str
              (strcat content-str "  ... and "
                      (itoa (- (length ua) 12)) " more\\P")))
      (setq content-str
            (strcat content-str "\\P  " (itoa (length ub))
                    " piece(s) of B unaccounted for in A."))))

  (initget 1)
  (setq ins-pt (getpoint "\nSpecify insertion point for the report: ")
        txtht  (max (getvar "TEXTSIZE") 2.5))
  (vla-put-height
    (vla-addmtext space (vlax-3d-point ins-pt) 0.0 content-str) txtht)

  (princ (strcat "\nA has " (itoa (length sga)) " pieces, B has "
                 (itoa (length sgb)) "; " (itoa (length ua))
                 " of A unmatched, " (itoa (length ub)) " of B."))
  (princ)
)


(princ "\nPANELCOMP.lsp loaded.")
(princ "\n  PANELCOMP   compare panels, colour by type, write a summary")
(princ "\n  PANELRESET  put a selection back to colour ByLayer")
(princ "\n  PANELCLOSE  rebuild panel outlines as closed rectangles")
(princ "\n  PANELDIFF   say why two panels did not match")
(princ)

;;; ============================================================ EOF
