;;; ============================================================
;;; MHPLACE.lsp  -  Mounting-Hole / Slot Placement Tool
;;;
;;; Adds the mounting-hole + slot block ("MH" by default) into the
;;; block definition of one or more back panels, along the two
;;; LONGEST opposing edges, and removes any perforations that the
;;; 3/4" mounting holes would collide with.
;;;
;;; Workflow (command  MHPLACE):
;;;   1. Select one or more panel BLOCKS.  The H_Panel Hidden back-
;;;      edge lines and the H_Perf perforations both live inside each
;;;      block, so you pick the blocks themselves.
;;;   2. Pick the mounting-hole block (one MH insert) as the template
;;;      so the tool knows which block to place and can read the 3/4"
;;;      hole radius and slot length from its definition.
;;;   3. Enter the minimum gap, the maximum distance, and the
;;;      perforation-deletion clearance.
;;;
;;; What it does:
;;;   - Reads each panel's four back-panel edges out of its block
;;;     definition (handling rotation / scale / nested blocks) and
;;;     finds the panel's long axis -> the two long opposing edges
;;;     become the runs.
;;;   - Spaces the mounting holes evenly with equal end margins, no
;;;     gap over 12.00".  The hole row-coordinates are computed ONCE
;;;     in world space and shared by every panel, so the holes line
;;;     up across panels; each station is snapped to a perforation
;;;     row so the holes stay IN-LINE with the perforations.
;;;   - Picks the perpendicular set-back (between the min gap and the
;;;     max distance, slot edge -> back edge) that the perforation
;;;     pattern keeps cleanest.
;;;   - Inserts the MH block into each panel's block definition on the
;;;     H_Mounting Holes layer (rotated so a horizontal run's slot
;;;     faces its edge); every instance of that block updates.
;;;   - Deletes perforations within an edge-to-edge clearance of the
;;;     3/4" holes from the block definition.
;;;
;;; Notes / assumptions:
;;;   - Panels are assumed inserted upright (rotation 0 or 90) at
;;;     positive unit scale; mirrored / skewed inserts are not fully
;;;     supported.
;;;   - A block shared by several panels is edited once, so its
;;;     instances must be arranged at the same run-direction position
;;;     to stay aligned.
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- tunable defaults ------------------------------------------

(setq *mh:blockname*  "MH"               ; default block if none picked
      *mh:perflayer*  "H_Perf"           ; perforation layer
      *mh:edgelayer*  "H_Panel Hidden"   ; back-panel edge layer
      *mh:mhlayer*    "H_Mounting Holes"  ; layer the holes land on
      *mh:maxspacing* 12.0               ; max gap between holes (in)
      *mh:tol*        1.0e-6             ; geometric tolerance
)


;;; ---- small helpers ---------------------------------------------

;;; Ceiling of a real number, returned as an integer.
(defun mh:ceil (x / n)
  (setq n (fix x))
  (if (> x (float n)) (1+ n) n)
)

;;; Distance between two 2D points given as (x y ...).
(defun mh:dist2d (a b)
  (sqrt (+ (expt (- (car b) (car a)) 2.0)
           (expt (- (cadr b) (cadr a)) 2.0)))
)

;;; Sorted (ascending) list of unique values, deduped to ~0.01.
(defun mh:uniq (lst / out)
  (setq out '())
  (foreach v lst
    (if (not (vl-some '(lambda (q) (< (abs (- q v)) 0.01)) out))
      (setq out (cons v out))))
  (vl-sort out '<)
)

;;; Nearest value in LST to V (list assumed non-empty).
(defun mh:nearest (v lst / best bd d)
  (setq best (car lst) bd (abs (- v best)))
  (foreach x (cdr lst)
    (setq d (abs (- v x)))
    (if (< d bd) (setq bd d best x)))
  best
)


;;; ---- block-insert transforms -----------------------------------

;;; Build a transform parameter list from an INSERT entity list:
;;;   (basex basey sx sy cos(rot) sin(rot) insx insy rot)
(defun mh:xform (ed / ins base bobj rot)
  (setq ins  (cdr (assoc 10 ed))
        rot  (cond ((cdr (assoc 50 ed))) (0.0))
        bobj (tblobjname "BLOCK" (cdr (assoc 2 ed)))
        base (if bobj (cdr (assoc 10 (entget bobj))) '(0.0 0.0 0.0)))
  (list (car base) (cadr base)
        (cond ((cdr (assoc 41 ed))) (1.0))
        (cond ((cdr (assoc 42 ed))) (1.0))
        (cos rot) (sin rot)
        (car ins) (cadr ins) rot)
)

;;; Local -> world for one transform level.  Returns (x y).
(defun mh:l2w (p pr / lx ly)
  (setq lx (* (- (car  p) (car   pr)) (caddr  pr))
        ly (* (- (cadr p) (cadr  pr)) (cadddr pr)))
  (list (+ (nth 6 pr) (* lx (nth 4 pr)) (* (- ly) (nth 5 pr)))
        (+ (nth 7 pr) (* lx (nth 5 pr)) (*    ly  (nth 4 pr))))
)

;;; World -> local for one transform level.  Returns (x y).
(defun mh:w2l (w pr / dx dy lx ly)
  (setq dx (- (car  w) (nth 6 pr))
        dy (- (cadr w) (nth 7 pr))
        lx (+ (* dx (nth 4 pr)) (* dy (nth 5 pr)))
        ly (+ (* (- dx) (nth 5 pr)) (* dy (nth 4 pr))))
  (list (+ (/ lx (caddr pr)) (car  pr))
        (+ (/ ly (cadddr pr)) (cadr pr)))
)

;;; Apply a stack of transforms (innermost first) to a 2D point.
(defun mh:xfstack (p plist / q)
  (setq q p)
  (foreach pr plist (setq q (mh:l2w q pr)))
  q
)


;;; ---- panel-block geometry --------------------------------------

;;; Recursively walk block BLKNAME, accumulating H_Panel Hidden lines
;;; (transformed by PLIST) into the global *mh:segs* as world (p1 p2).
(defun mh:walkblock (blkname plist hlayer / e ed t0)
  (setq e (tblobjname "BLOCK" blkname))
  (if e (setq e (entnext e)))
  (while (and e (setq ed (entget e))
              (/= (cdr (assoc 0 ed)) "ENDBLK"))
    (setq t0 (cdr (assoc 0 ed)))
    (cond
      ((and (= t0 "LINE")
            (= (strcase (cdr (assoc 8 ed))) (strcase hlayer)))
       (setq *mh:segs*
             (cons (list (mh:xfstack (cdr (assoc 10 ed)) plist)
                         (mh:xfstack (cdr (assoc 11 ed)) plist))
                   *mh:segs*)))
      ((= t0 "INSERT")
       (mh:walkblock (cdr (assoc 2 ed))
                     (cons (mh:xform ed) plist)
                     hlayer))
    )
    (setq e (entnext e))
  )
)

;;; World-space H_Panel Hidden segments under a panel INSERT.
(defun mh:hiddensegs (insent hlayer)
  (setq *mh:segs* '())
  (mh:walkblock (cdr (assoc 2 (entget insent)))
                (list (mh:xform (entget insent)))
                hlayer)
  *mh:segs*
)

;;; Recursively collect H_Perf circles under a panel block into the
;;; global *mh:perfs*, descending through nested blocks so perforations
;;; nested inside sub-blocks are still found.  Each entry is
;;;   (ename worldX worldY radius)
;;; ename is the actual entity (in whatever block holds it) so it can
;;; be deleted later.
(defun mh:collectperfs (blkname plist / e ed t0 c)
  (setq e (tblobjname "BLOCK" blkname))
  (if e (setq e (entnext e)))
  (while (and e (setq ed (entget e))
              (/= (cdr (assoc 0 ed)) "ENDBLK"))
    (setq t0 (cdr (assoc 0 ed)))
    (cond
      ((and (= t0 "CIRCLE")
            (= (strcase (cdr (assoc 8 ed))) (strcase *mh:perflayer*)))
       (setq c (cdr (assoc 10 ed)))
       (setq *mh:perfs*
             (cons (cons e (append (mh:xfstack (list (car c) (cadr c)) plist)
                                   (list (cdr (assoc 40 ed)))))
                   *mh:perfs*)))
      ((= t0 "INSERT")
       (mh:collectperfs (cdr (assoc 2 ed)) (cons (mh:xform ed) plist)))
    )
    (setq e (entnext e))
  )
)

;;; All H_Perf circles under a panel INSERT (world coords + ename).
(defun mh:panelperfs (insent)
  (setq *mh:perfs* '())
  (mh:collectperfs (cdr (assoc 2 (entget insent)))
                   (list (mh:xform (entget insent))))
  *mh:perfs*
)

;;; From a list of (pt1 pt2) line segments, keep only the perfectly
;;; axis-aligned ones (angled corner chamfers are dropped) and return
;;;   (leftX rightX botY topY)
;;; using only the long edges so short lines never skew the result.
(defun mh:paneledges (segs / s a b dx dy mx mn len
                         vlist hlist maxv maxh
                         leftx rightx boty topy)
  (setq vlist '() hlist '() maxv 0.0 maxh 0.0)
  (foreach s segs
    (setq a  (car  s)
          b  (cadr s)
          dx (abs (- (car b) (car a)))
          dy (abs (- (cadr b) (cadr a)))
          mx (max dx dy)
          mn (min dx dy))
    ;; Near-axis = an edge; ~45 deg (ratio near 1) = a corner chamfer.
    (if (and (> mx *mh:tol*) (< (/ mn mx) 0.2))
      (if (< dx dy)
        (setq len dy
              vlist (cons (list (* 0.5 (+ (car a) (car b))) len) vlist)
              maxv  (max maxv len))
        (setq len dx
              hlist (cons (list (* 0.5 (+ (cadr a) (cadr b))) len) hlist)
              maxh  (max maxh len))
      )
    )
  )
  (foreach v vlist
    (if (>= (cadr v) (* 0.5 maxv))
      (progn
        (if (or (null leftx)  (< (car v) leftx))  (setq leftx  (car v)))
        (if (or (null rightx) (> (car v) rightx)) (setq rightx (car v))))))
  (foreach h hlist
    (if (>= (cadr h) (* 0.5 maxh))
      (progn
        (if (or (null boty) (< (car h) boty)) (setq boty (car h)))
        (if (or (null topy) (> (car h) topy)) (setq topy (car h))))))
  (if (and leftx rightx boty topy)
    (list leftx rightx boty topy)
    nil)
)

;;; Walk the mounting-hole block NAME and return (holeRadius slotHalf)
;;;   holeRadius = largest CIRCLE radius  (the 3/4" hole)
;;;   slotHalf   = greatest |x| reached by any non-circle geometry.
(defun mh:blockgeom (name / e ed t0 r maxr maxx pr)
  (setq maxr 0.0 maxx 0.0 e (tblobjname "BLOCK" name))
  (if e (setq e (entnext e)))
  (while (and e (setq ed (entget e))
              (/= (cdr (assoc 0 ed)) "ENDBLK"))
    (setq t0 (cdr (assoc 0 ed)))
    (cond
      ((= t0 "CIRCLE")
       (setq r (cdr (assoc 40 ed)))
       (if (> r maxr) (setq maxr r)))
      (t
       (foreach pr ed
         (if (member (car pr) '(10 11))
           (if (> (abs (cadr pr)) maxx) (setq maxx (abs (cadr pr)))))))
    )
    (setq e (entnext e))
  )
  (list maxr maxx)
)


;;; ---- placement along one edge ----------------------------------

;;; Build the world-space mounting-hole centres for a single edge.
;;; The hole is aligned with the perforation ROW nearest the edge so it
;;; blends into the pattern, clamped so the gap from the slot edge to
;;; the panel edge stays within [mingap, maxdist].
;;;   axis     - 1 vertical run (perp = X) / 0 horizontal run (perp = Y)
;;;   edgeval  - edge coordinate on the perpendicular axis (world)
;;;   nrm      - interior normal sign (+1 / -1) on the perpendicular axis
;;;   perfs    - this panel's world perforations (x y r)
;;;   stations - shared run-axis hole coordinates (world)
;;;   pmin pmax- this panel's run extent
;;;   worldrot - world rotation for the inserted block
;;; Returns a list of (wx wy worldRot).
(defun mh:edgeholes (axis edgeval nrm perfs stations pmin pmax
                     slotHalf mingap maxdist worldrot
                     / dmin dmax cand pc d dfinal used perp s holes)
  ;; Perforation-row distances (inboard from the edge).  d>0 means the
  ;; row is inside the panel; the edge-most row is the smallest d.
  (setq dmin (+ slotHalf mingap)     ; hole-centre distance for min gap
        dmax (+ slotHalf maxdist)    ; hole-centre distance for max dist
        cand '())
  (foreach p perfs
    (setq pc (if (= axis 1) (car p) (cadr p))
          d  (* nrm (- pc edgeval)))
    (if (and (> d *mh:tol*) (<= d (+ dmax 1.0)))
      (setq cand (cons d cand))))
  ;; Align to the edge-most row, but never closer than the min gap nor
  ;; farther than the max distance.
  (if cand
    (setq dfinal (max dmin (min dmax (apply 'min cand))))
    (setq dfinal dmin))
  (setq perp (+ edgeval (* nrm dfinal))
        used (vl-remove-if-not
               '(lambda (v) (and (>= v (- pmin 0.001)) (<= v (+ pmax 0.001))))
               stations)
        holes '())
  (foreach s used
    (setq holes (cons (if (= axis 1) (list perp s worldrot)
                                     (list s perp worldrot))
                      holes)))
  holes
)


;;; ---- edit one block definition ---------------------------------

;;; Add the mounting-hole blocks to definition NAME (converting each
;;; world hole into block-local coordinates via PR) on the mounting-
;;; hole layer, then delete the panel's perforations that collide with
;;; those holes.  PERFS = (ename worldX worldY radius) from the panel's
;;; block tree.  HOLES = world (wx wy worldRot).  Returns (added . deleted).
(defun mh:editblock (doc name pr panelrot holes perfs holeR delclr
                     / blocks blkdef obj lp lr hxy n)
  (setq blocks (vla-get-blocks doc)
        blkdef (vla-item blocks name)
        hxy    (mapcar '(lambda (h) (list (car h) (cadr h))) holes))
  ;; insert the mounting-hole blocks into the definition
  (foreach h holes
    (setq lp (mh:w2l (list (car h) (cadr h)) pr)
          lr (- (caddr h) panelrot)
          obj (vla-insertblock
                blkdef (vlax-3d-point (list (car lp) (cadr lp) 0.0))
                *mh:blockname* 1.0 1.0 1.0 lr))
    (vla-put-layer obj *mh:mhlayer*))
  ;; delete colliding perforations (anywhere in the panel's block tree)
  (setq n 0)
  (foreach p perfs
    (if (vl-some
          '(lambda (q)
             (< (- (mh:dist2d (list (cadr p) (caddr p)) q) holeR (cadddr p))
                delclr))
          hxy)
      (if (not (vlax-erased-p (car p)))
        (progn (vla-delete (vlax-ename->vla-object (car p)))
               (setq n (1+ n))))))
  (cons (length holes) n)
)

;;; Delete LOOSE perforations in model/paper space that collide with
;;; any of the world mounting-hole centres in HOLES (each (x y)).
;;; This complements the in-block deletion so perforations are removed
;;; whether they live inside the panel block or loose in the drawing.
;;; Returns the number deleted.
(defun mh:loosedel (holes holeR delclr / ss i ent ed c r n)
  (setq n 0
        ss (ssget "_X" (list '(0 . "CIRCLE") (cons 8 *mh:perflayer*))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              ed  (entget ent)
              c   (cdr (assoc 10 ed))
              r   (cdr (assoc 40 ed)))
        (if (vl-some
              '(lambda (q)
                 (< (- (mh:dist2d (list (car c) (cadr c)) q) holeR r) delclr))
              holes)
          (progn (entdel ent) (setq n (1+ n))))
        (setq i (1+ i)))))
  n
)


;;; ---- main command ----------------------------------------------

(defun c:MHPLACE
    (/ *error* acadobj doc
       sspan ssblk pent blkname geom holeR slotHalf
       mingap maxdist delclr
       i ed name pr panels seen rec
       vert gmin gmax allperf rows
       ngap spc stations allholes
       totadd totdel
       leftx rightx boty topy axis pmin pmax pxyr holes res)

  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** MHPLACE Error: " msg)))
    (princ)
  )

  (setq acadobj (vlax-get-acad-object)
        doc     (vla-get-activedocument acadobj))

  ;; --- 1. panel blocks ------------------------------------------
  (princ "\nSelect the panel block(s): ")
  (setq sspan (ssget '((0 . "INSERT"))))
  (if (null sspan)
    (progn (princ "\nNo panel blocks selected - cancelled.") (exit)))

  ;; --- 2. mounting-hole block -----------------------------------
  (princ "\nSelect the mounting-hole block: ")
  (setq ssblk (ssget "_+.:E:S" '((0 . "INSERT"))))
  (if ssblk
    (setq pent    (ssname ssblk 0)
          blkname (vla-get-effectivename (vlax-ename->vla-object pent)))
    (setq blkname *mh:blockname*))
  (setq *mh:blockname* blkname)
  (if (null (tblobjname "BLOCK" blkname))
    (progn (princ (strcat "\nBlock \"" blkname "\" not found - cancelled."))
           (exit)))
  (setq geom (mh:blockgeom blkname) holeR (car geom) slotHalf (cadr geom))
  (if (<= holeR 0.0)
    (progn (princ "\nNo circle found in the block - cannot size the hole.")
           (exit)))

  ;; --- 3. numeric prompts ---------------------------------------
  (initget 6)
  (setq mingap (cond ((getdist "\nMinimum gap, slot edge to panel edge <0.1000>: ")) (0.1)))
  (initget 6)
  (setq maxdist (cond ((getdist "\nMaximum distance, slot edge to panel edge <1.0000>: ")) (1.0)))
  (if (< maxdist mingap) (setq maxdist mingap))
  (initget 4)
  (setq delclr (cond ((getdist "\nPerforation deletion clearance, edge to edge <0.0625>: ")) (0.0625)))

  ;; --- build a record per UNIQUE block definition ---------------
  ;; rec = (name pr rot leftx rightx boty topy perfs)
  (setq panels '() seen '() i 0)
  (while (< i (sslength sspan))
    (setq ed   (entget (ssname sspan i))
          name (cdr (assoc 2 ed)))
    (if (not (member name seen))
      (progn
        (setq seen (cons name seen)
              pr   (mh:xform ed)
              rec  (mh:paneledges (mh:hiddensegs (ssname sspan i) *mh:edgelayer*)))
        (if rec
          (setq panels
                (cons (list name pr (nth 8 pr)
                            (car rec) (cadr rec) (caddr rec) (cadddr rec)
                            (mh:panelperfs (ssname sspan i)))
                      panels))
          (princ (strcat "\n  (skipped \"" name "\" - no panel edges found)")))
      )
    )
    (setq i (1+ i))
  )
  (if (null panels)
    (progn (princ "\nNo usable panels - cancelled.") (exit)))

  ;; --- orientation + global run extent --------------------------
  ;; Decide from the first panel; assume all share orientation.
  (setq rec  (car panels)
        vert (>= (- (nth 6 rec) (nth 5 rec))      ; height
                 (- (nth 4 rec) (nth 3 rec))))    ; width
  (setq gmin nil gmax nil allperf '())
  (foreach rec panels
    (setq pmin (if vert (nth 5 rec) (nth 3 rec))
          pmax (if vert (nth 6 rec) (nth 4 rec)))
    (if (or (null gmin) (< pmin gmin)) (setq gmin pmin))
    (if (or (null gmax) (> pmax gmax)) (setq gmax pmax))
    (setq allperf (append (nth 7 rec) allperf)))

  ;; --- shared, in-line hole stations ----------------------------
  (setq ngap (max 1 (mh:ceil (/ (- gmax gmin) *mh:maxspacing*)))
        spc  (/ (- gmax gmin) ngap)
        stations '() i 0)
  (while (< i ngap)
    (setq stations (cons (+ gmin (* spc (+ i 0.5))) stations) i (1+ i)))
  ;; snap every station onto the nearest world perforation row
  ;; (perf entries are (ename x y r), so x = cadr, y = caddr)
  (setq rows (mh:uniq (mapcar (if vert 'caddr 'cadr) allperf)))
  (if rows
    (setq stations (mapcar '(lambda (v) (mh:nearest v rows)) stations)))

  ;; --- place into each block definition -------------------------
  (setq totadd 0 totdel 0 allholes '())
  (foreach rec panels
    (setq name   (nth 0 rec)
          pr     (nth 1 rec)
          leftx  (nth 3 rec) rightx (nth 4 rec)
          boty   (nth 5 rec) topy   (nth 6 rec)
          pxyr   (mapcar 'cdr (nth 7 rec))   ; (x y r) for geometry
          holes  '())
    (if vert
      (progn
        (setq axis 1 pmin boty pmax topy)
        (setq holes
              (append
                (mh:edgeholes 1 leftx  1.0 pxyr stations pmin pmax
                              slotHalf mingap maxdist 0.0)
                (mh:edgeholes 1 rightx -1.0 pxyr stations pmin pmax
                              slotHalf mingap maxdist 0.0))))
      (progn
        (setq axis 0 pmin leftx pmax rightx)
        (setq holes
              (append
                (mh:edgeholes 0 boty 1.0  pxyr stations pmin pmax
                              slotHalf mingap maxdist (/ pi 2.0))
                (mh:edgeholes 0 topy -1.0 pxyr stations pmin pmax
                              slotHalf mingap maxdist (/ pi 2.0)))))
    )
    (setq res (mh:editblock doc name pr (nth 2 rec) holes (nth 7 rec) holeR delclr)
          totadd (+ totadd (car res))
          totdel (+ totdel (cdr res))
          allholes (append (mapcar '(lambda (h) (list (car h) (cadr h))) holes)
                           allholes))
  )

  ;; --- also delete loose perforations colliding with the holes ----
  (if allholes
    (setq totdel (+ totdel (mh:loosedel allholes holeR delclr))))

  (command "_.REGEN")
  (princ (strcat "\nDone - " (itoa (length panels)) " block definition(s); added "
                 (itoa totadd) " mounting hole(s) on the "
                 (if vert "left/right" "top/bottom")
                 " runs; deleted " (itoa totdel) " perforation(s)."))
  (princ)
)


(princ "\nMHPLACE.lsp loaded.  Type  MHPLACE  to run.")
(princ)

;;; ============================================================ EOF
