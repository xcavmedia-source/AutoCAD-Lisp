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
;;; Panel outlines do NOT have to be closed polylines. Anything on the
;;; panel layer is grouped into outlines by shared endpoints, so a
;;; closed rectangle, an open polyline, a polyline with a gap in it,
;;; or four separate LINEs all resolve to a single panel.
;;;
;;; Commands : PANELCOMP    compare, colour and report
;;;            PANELRESET   put a selection back to colour ByLayer
;;;            PANELCLOSE   close open polyline panel outlines
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- configuration ---------------------------------------------

;;; Geometry is compared after snapping to this many drawing units.
;;; 0.001 = one thousandth of an inch: tight enough to keep real part
;;; differences apart, loose enough to absorb rounding in the DWG.
(setq *pc:tol* 0.001)

;;; How far apart two outline endpoints can be and still count as the
;;; same corner when piecing an outline together.
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
(setq *pc:colors*
  '(1 3 5 4 6 2 30 130 90 190 210 230 40 150 170 20 60 100 140 180
    220 10 50 70 110 160 200 240 14 34 74 114 154 194 234))


;;; ---- internal helpers ------------------------------------------

;;; Snap VAL to the comparison tolerance and return it as a whole
;;; number of tolerance units. Integers compare exactly, which is what
;;; the fingerprints rely on; rounded reals do not.
(defun pc:snap (val / n)
  (setq n (/ val *pc:tol*))
  (if (minusp n) (fix (- n 0.5)) (fix (+ n 0.5)))
)

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

;;; The two loose ends of an open ENT, as 2D points, or nil when the
;;; entity type has no endpoints to chain from.
(defun pc:ends (ent obj / ed typ pts p1 p2)
  (setq ed  (entget ent)
        typ (cdr (assoc 0 ed)))
  (cond
    ((member typ '("LINE" "ARC"))
     (setq p1 (vlax-safearray->list
                (vlax-variant-value (vla-get-startpoint obj)))
           p2 (vlax-safearray->list
                (vlax-variant-value (vla-get-endpoint obj))))
     (list (list (car p1) (cadr p1)) (list (car p2) (cadr p2))))
    ((= typ "LWPOLYLINE")
     (setq pts '())
     (foreach pair ed
       (if (= (car pair) 10) (setq pts (cons (cdr pair) pts))))
     (if pts
       (list (list (car (last pts)) (cadr (last pts)))
             (list (car (car pts))  (cadr (car pts))))))
    (t nil)
  )
)

;;; True when points A and B are the same corner within TOL.
(defun pc:near (a b tol)
  (and (< (abs (- (car  a) (car  b))) tol)
       (< (abs (- (cadr a) (cadr b))) tol))
)

;;; True when outline piece IT shares a corner with any piece in
;;; group G. IT and each member are (ents bb p1 p2).
(defun pc:touches (it g tol / hit)
  (setq hit nil)
  (foreach o g
    (if (and (null hit)
             (or (pc:near (caddr  it) (caddr  o) tol)
                 (pc:near (caddr  it) (cadddr o) tol)
                 (pc:near (cadddr it) (caddr  o) tol)
                 (pc:near (cadddr it) (cadddr o) tol)))
      (setq hit t))
  )
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

;;; Describe one piece of geometry inside a panel as a string: its
;;; type, its size, and its centre measured from the panel's
;;; lower-left corner at OX OY. Because the position is relative, the
;;; same hole in the same place on two panels yields the same string
;;; wherever in the drawing those panels happen to sit.
(defun pc:sig (ent obj bb ox oy / mn mx typ s)
  (setq mn  (car  bb)
        mx  (cadr bb)
        typ (cdr (assoc 0 (entget ent)))
        s   (strcat typ
              "|" (itoa (pc:snap (- (car  mx) (car  mn))))
              "|" (itoa (pc:snap (- (cadr mx) (cadr mn))))
              "|" (itoa (pc:snap (- (/ (+ (car  mn) (car  mx)) 2.0) ox)))
              "|" (itoa (pc:snap (- (/ (+ (cadr mn) (cadr mx)) 2.0) oy)))))
  (cond
    ;;; A circle's bounding box already carries its diameter.
    ((= typ "CIRCLE") s)
    ;;; Two arcs can share a bounding box and still be different cuts.
    ((= typ "ARC")
     (strcat s "|" (itoa (pc:snap (pc:prop obj 'Radius)))
               "|" (itoa (pc:snap (pc:prop obj 'TotalAngle)))))
    ((= typ "ELLIPSE")
     (strcat s "|" (itoa (pc:snap (pc:prop obj 'MajorRadius)))
               "|" (itoa (pc:snap (pc:prop obj 'MinorRadius)))))
    ;;; Slots and logo outlines: vertex count and enclosed area pull
    ;;; apart shapes that happen to share a bounding box.
    ((member typ '("LWPOLYLINE" "POLYLINE" "SPLINE"))
     (strcat s "|" (itoa (pc:vcount ent))
               "|" (itoa (pc:snap (pc:prop obj 'Area)))))
    (t s)
  )
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

;;; Name the handful of ACI colours people read by name.
(defun pc:colorname (c / hit)
  (if (setq hit (assoc c '((1 . "red")   (2 . "yellow") (3 . "green")
                           (4 . "cyan")  (5 . "blue")   (6 . "magenta")
                           (7 . "white"))))
    (strcat (itoa c) " (" (cdr hit) ")")
    (itoa c)
  )
)

;;; Colour ENT, returning nil instead of failing. An object on a
;;; locked layer refuses the change, and an error there would abort the
;;; run part way through with the drawing half recoloured.
(defun pc:setcolor (ent col)
  (not (vl-catch-all-error-p
         (vl-catch-all-apply
           'vla-put-color (list (vlax-ename->vla-object ent) col))))
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
(defun pc:outlines (ss lay / i ent obj bb ends content opens g it ents ub outs)
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
      ((setq ends (pc:ends ent obj))
       (setq opens (cons (list (list ent) bb (car ends) (cadr ends))
                         opens)))
      ;;; No endpoints to chain from - take it as an outline as it is.
      (t (setq outs (cons (list (list ent) bb) outs))))
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
       ent obj bb cx cy eps hit orphans dupes
       hmap emap curidx cursigs curents
       sigs raw pw ph gkey groups g grp col cidx ncol locked
       total ins-pt txtht mtext-obj content-str n)

  ;;; Local error handler - closes the undo group on cancel or error
  ;;; so the drawing is never left mid-transaction.
  (defun *error* (msg)
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

  ;;; --- drop any outline that wraps other outlines ---------------
  ;;; A border drawn around the whole sheet lives on the same layer as
  ;;; the panels; it is recognised by the panels sitting inside it.
  (setq panels '() wrappers 0)
  (foreach p cands
    (if (vl-some
          '(lambda (q)
             (and (not (equal q p))
                  (pc:contains (cadr p) (cadr q))
                  (< (pc:bbarea (cadr q)) (* 0.99 (pc:bbarea (cadr p))))))
          cands)
      (setq wrappers (1+ wrappers))
      (setq panels (cons p panels))))

  (if (null panels)
    (progn (princ "\nEvery outline found contains another - nothing to compare.")
           (exit)))

  ;;; --- number the panels: (idx enames bbmin bbmax) ---------------
  (setq recs '() idx 0)
  (foreach p panels
    (setq recs (cons (list idx (car p) (car (cadr p)) (cadr (cadr p))) recs)
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

  (setq cells '())
  (foreach r recs
    (setq ix0 (pc:ifloor (/ (car  (caddr  r)) cs))
          ix1 (pc:ifloor (/ (car  (cadddr r)) cs))
          iy0 (pc:ifloor (/ (cadr (caddr  r)) cs))
          iy1 (pc:ifloor (/ (cadr (cadddr r)) cs))
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
        cursigs '() curents '() eps (* *pc:tol* 10.0))
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
  (setq groups '() dupes 0)
  (foreach r recs
    (setq sigs (cdr (assoc (car r) hmap))
          raw  (length sigs)
          pw   (- (car  (cadddr r)) (car  (caddr r)))
          ph   (- (cadr (cadddr r)) (cadr (caddr r)))
          ;;; Sorting makes the fingerprint independent of the order
          ;;; the holes were drawn in.
          sigs (if sigs (vl-sort sigs '<) '())
          ;;; Cheap key first: only patterns agreeing on hole count and
          ;;; panel size are worth comparing hole by hole. The raw
          ;;; count is part of it, so a panel carrying stacked
          ;;; duplicate geometry never passes for a clean one.
          gkey (strcat (itoa raw) "|" (itoa (pc:snap pw))
                       "|" (itoa (pc:snap ph))))
    (if (/= raw (length sigs)) (setq dupes (1+ dupes)))
    (setq grp nil)
    (foreach g groups
      (if (and (null grp) (= (car g) gkey) (equal (cadr g) sigs))
        (setq grp g)))
    (if grp
      (setq groups (subst (list gkey sigs (cons r (caddr grp)) (cadddr grp))
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

  (foreach g groups
    (setq col (nth (rem cidx ncol) *pc:colors*)
          pw  (car   (cadddr g))
          ph  (cadr  (cadddr g))
          raw (caddr (cadddr g))
          n   (length (caddr g))
          total (+ total n))
    ;;; Colour the outline and everything inside it, as an object
    ;;; override so the layer's own colour is left alone.
    (foreach r (caddr g)
      (foreach ent (cadr r)
        (if (not (pc:setcolor ent col)) (setq locked (1+ locked))))
      (foreach ent (cdr (assoc (car r) emap))
        (if (not (pc:setcolor ent col)) (setq locked (1+ locked)))))
    (setq content-str
          (strcat content-str
                  "{\\H1.0x;\\L;TYPE " (itoa (1+ cidx))
                  "  -  colour " (pc:colorname col) "\\l}\\P"
                  "  Qty    =  " (itoa n) "\\P"
                  "  Size   =  " (pc:fmtinch pw) " x " (pc:fmtinch ph) "\\P"
                  "  Holes  =  " (itoa raw) "\\P"
                  "\\P")
          cidx (1+ cidx)))

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
                  " outline(s) skipped - they enclose other panels.")))
  (if (> orphans 0)
    (setq content-str
          (strcat content-str "\\P\\P"
                  "  NOTE: " (itoa orphans)
                  " object(s) fell outside every panel and were ignored.")))
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

(defun c:PANELRESET (/ *error* acadobj doc ss i n locked)

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

  (setq i 0 n 0 locked 0)
  (while (< i (sslength ss))
    (if (pc:setcolor (ssname ss i) 256)
      (setq n (1+ n))
      (setq locked (1+ locked)))
    (setq i (1+ i)))

  (vla-endundomark doc)
  (princ (strcat "\n" (itoa n) " object(s) set back to ByLayer."))
  (if (> locked 0)
    (princ (strcat "  " (itoa locked)
                   " refused the change - locked layer?")))
  (princ)
)


;;; ---- close open panel outlines ---------------------------------
;;; PANELCOMP does not need closed outlines, but closed ones give you
;;; working AREA, hatching and boundary picks. This closes the single
;;; open polylines; outlines built from several pieces are reported
;;; instead, since joining those is PEDIT's job.

(defun c:PANELCLOSE (/ *error* acadobj doc lay ss split outs
                       p ents ent obj closed gap pieces skipped)

  (defun *error* (msg)
    (if doc (vla-endundomark doc))
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** PANELCLOSE Error: " msg))
    )
    (princ)
  )

  (setq acadobj (vlax-get-acad-object)
        doc     (vla-get-activedocument acadobj))
  (vla-startundomark doc)

  (setq lay (pc:asklayer))
  (princ "\nSelect the panel outlines to close: ")
  (setq ss (ssget))
  (if (null ss)
    (progn (princ "\nNothing selected - command cancelled.") (exit)))

  (setq split   (pc:outlines ss lay)
        outs    (car split)
        closed 0 gap 0 pieces 0 skipped 0)

  (foreach p outs
    (setq ents (car p))
    (if (= (length ents) 1)
      (progn
        (setq ent (car ents)
              obj (vlax-ename->vla-object ent))
        (cond
          ((pc:closedp ent) (setq skipped (1+ skipped)))
          ((= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")
           ;;; Where the ends already meet, closing only sets the flag.
           ;;; Where they do not, it also draws the missing segment -
           ;;; which is the point of the command.
           (if (apply 'pc:near (append (pc:ends ent obj) (list *pc:gap*)))
             (setq closed (1+ closed))
             (setq gap (1+ gap)))
           (if (vl-catch-all-error-p
                 (vl-catch-all-apply 'vla-put-closed
                                     (list obj :vlax-true)))
             (setq skipped (1+ skipped))))
          (t (setq skipped (1+ skipped)))))
      (setq pieces (1+ pieces))))

  (vla-endundomark doc)
  (princ (strcat "\nClosed " (itoa (+ closed gap)) " outline(s)"))
  (if (> gap 0)
    (princ (strcat " - " (itoa gap)
                   " of them had a real gap and gained a segment")))
  (princ ".")
  (if (> pieces 0)
    (princ (strcat "\n" (itoa pieces)
                   " outline(s) are drawn as separate pieces - join them"
                   " with PEDIT first. PANELCOMP handles them as they are.")))
  (if (> skipped 0)
    (princ (strcat "\n" (itoa skipped) " already closed or not a polyline.")))
  (princ)
)


(princ "\nPANELCOMP.lsp loaded.")
(princ "\n  PANELCOMP   compare panels, colour by type, write a summary")
(princ "\n  PANELRESET  put a selection back to colour ByLayer")
(princ "\n  PANELCLOSE  close open polyline panel outlines")
(princ)

;;; ============================================================ EOF
