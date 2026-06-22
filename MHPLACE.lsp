;;; ============================================================
;;; MHPLACE.lsp  -  Mounting-Hole / Slot Placement Tool
;;;
;;; Places the mounting-hole + slot block ("MH" by default) along
;;; the two LONGEST opposing edges of a back panel and cleans up
;;; any perforations that collide with the 3/4" mounting holes.
;;;
;;; Workflow (command  MHPLACE):
;;;   1. Select the panel BLOCK - the H_Panel Hidden back-edge lines
;;;      live inside it, so they can't be picked directly.  The tool
;;;      reads them out of the block definition (handling rotation,
;;;      scale and nested blocks) to get the four back-panel edges,
;;;      and leaves the block itself untouched.
;;;   2. Select the perforations - loose H_Perf circles are filtered
;;;      out of the selection.
;;;   3. Pick the mounting-hole block - one existing insert tells the
;;;      tool which block (and layer) to place.  Its geometry (the
;;;      3/4" hole radius and the slot length) is read straight from
;;;      the block definition, so it adapts if the block changes.
;;;   4. Enter the minimum gap, the maximum distance, and the
;;;      perforation-deletion clearance.
;;;
;;; What it does:
;;;   - Finds the panel's long axis and treats the two long opposing
;;;     edges as the runs (per the "use the longest run" rule).
;;;   - Spaces the mounting holes evenly along each run with equal
;;;     end margins, no gap exceeding 12.00".
;;;   - Snaps every hole's run-position to the nearest perforation
;;;     row so the holes line up IN-LINE with the perforations.
;;;   - Picks the perpendicular set-back (between the min gap and the
;;;     max distance, measured slot-edge -> back-panel edge) that the
;;;     perforation pattern keeps cleanest - i.e. the offset that
;;;     leaves the fewest half-clipped perforations.
;;;   - For a horizontal run the block is rotated 90 deg so the slot
;;;     points at that edge; vertical runs are left at rotation 0.
;;;   - Deletes any perforation whose edge comes within the entered
;;;     clearance of a 3/4" mounting-hole edge (edge-to-edge).
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- tunable defaults ------------------------------------------

(setq *mh:blockname*   "MH"              ; default block if none picked
      *mh:perflayer*   "H_Perf"          ; perforation layer
      *mh:edgelayer*   "H_Panel Hidden"  ; back-panel edge layer
      *mh:maxspacing*  12.0              ; max gap between holes (in)
      *mh:tol*         1.0e-6            ; geometric tolerance
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

;;; Return the active space (model or paper) VLA object.
(defun mh:activespace (doc)
  (if (and (= (getvar "TILEMODE") 0) (= (getvar "CVPORT") 1))
    (vla-get-paperspace doc)
    (vla-get-modelspace doc)
  )
)


;;; ---- panel-block edge extraction -------------------------------

;;; The back-panel edges live on layer H_Panel Hidden INSIDE the panel
;;; block, so they can't be selected directly.  Given a panel INSERT we
;;; walk its block definition (recursing through any nested blocks),
;;; transform every H_Panel Hidden line into world coordinates, and
;;; collect them into the global *mh:segs* as (worldPt1 worldPt2) pairs.

;;; Build a transform parameter list from an INSERT entity list:
;;;   (basex basey sx sy cos(rot) sin(rot) insx insy)
(defun mh:xform (ed / ins base bobj)
  (setq ins  (cdr (assoc 10 ed))
        bobj (tblobjname "BLOCK" (cdr (assoc 2 ed)))
        base (if bobj (cdr (assoc 10 (entget bobj))) '(0.0 0.0 0.0)))
  (list (car base) (cadr base)
        (cond ((cdr (assoc 41 ed))) (1.0))
        (cond ((cdr (assoc 42 ed))) (1.0))
        (cos (cond ((cdr (assoc 50 ed))) (0.0)))
        (sin (cond ((cdr (assoc 50 ed))) (0.0)))
        (car ins) (cadr ins))
)

;;; Apply ONE transform param set to a 2D point.
(defun mh:xf1 (p pr / lx ly)
  (setq lx (* (- (car  p) (car   pr)) (caddr  pr))
        ly (* (- (cadr p) (cadr  pr)) (cadddr pr)))
  (list (+ (nth 6 pr) (* lx (nth 4 pr)) (* (- ly) (nth 5 pr)))
        (+ (nth 7 pr) (* lx (nth 5 pr)) (*    ly  (nth 4 pr)))
        0.0)
)

;;; Apply a stack of transforms (innermost first) to a point.
(defun mh:xfpt (p plist / q)
  (setq q p)
  (foreach pr plist (setq q (mh:xf1 q pr)))
  q
)

;;; Recursively walk block BLKNAME, accumulating H_Panel Hidden lines
;;; (transformed by PLIST) into the global *mh:segs*.
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
             (cons (list (mh:xfpt (cdr (assoc 10 ed)) plist)
                         (mh:xfpt (cdr (assoc 11 ed)) plist))
                   *mh:segs*)))
      ((= t0 "INSERT")
       (mh:walkblock (cdr (assoc 2 ed))
                     (cons (mh:xform ed) plist)
                     hlayer))
    )
    (setq e (entnext e))
  )
)

;;; Collect world-space H_Panel Hidden segments under a panel INSERT.
(defun mh:hiddensegs (insent hlayer)
  (setq *mh:segs* '())
  (mh:walkblock (cdr (assoc 2 (entget insent)))
                (list (mh:xform (entget insent)))
                hlayer)
  *mh:segs*
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
    ;; Real CAD edges are rarely perfectly axis-aligned, so classify by
    ;; ratio rather than an exact tolerance, and use the segment midpoint.
    (if (and (> mx *mh:tol*) (< (/ mn mx) 0.2))
      (if (< dx dy)
        ;; vertical edge : near-constant X
        (setq len dy
              vlist (cons (list (* 0.5 (+ (car a) (car b))) len) vlist)
              maxv  (max maxv len))
        ;; horizontal edge : near-constant Y
        (setq len dx
              hlist (cons (list (* 0.5 (+ (cadr a) (cadr b))) len) hlist)
              maxh  (max maxh len))
      )
    )
  )
  ;; keep only the long edges (>= half the longest of each kind)
  (foreach v vlist
    (if (>= (cadr v) (* 0.5 maxv))
      (progn
        (if (or (null leftx)  (< (car v) leftx))  (setq leftx  (car v)))
        (if (or (null rightx) (> (car v) rightx)) (setq rightx (car v)))
      )
    )
  )
  (foreach h hlist
    (if (>= (cadr h) (* 0.5 maxh))
      (progn
        (if (or (null boty) (< (car h) boty)) (setq boty (car h)))
        (if (or (null topy) (> (car h) topy)) (setq topy (car h)))
      )
    )
  )
  (if (and leftx rightx boty topy)
    (list leftx rightx boty topy)
    nil
  )
)


;;; ---- mounting-hole block geometry ------------------------------

;;; Walk the block definition NAME and return (holeRadius slotHalf)
;;;   holeRadius = largest CIRCLE radius  (the 3/4" hole)
;;;   slotHalf   = greatest |x| reached by any non-circle geometry
;;;                in the block's local frame (half the slot length
;;;                along the slot axis).
(defun mh:blockgeom (name / e ed t0 r maxr maxx pr)
  (setq maxr 0.0 maxx 0.0
        e (tblobjname "BLOCK" name))
  (if e (setq e (entnext e)))
  (while (and e (setq ed (entget e))
              (/= (cdr (assoc 0 ed)) "ENDBLK"))
    (setq t0 (cdr (assoc 0 ed)))
    (cond
      ((= t0 "CIRCLE")
       (setq r (cdr (assoc 40 ed)))
       (if (> r maxr) (setq maxr r)))
      (t
       ;; track |x| of every group-10 / group-11 point
       (foreach pr ed
         (if (member (car pr) '(10 11))
           (if (> (abs (cadr pr)) maxx) (setq maxx (abs (cadr pr))))
         )
       )
      )
    )
    (setq e (entnext e))
  )
  (list maxr maxx)
)


;;; ---- perforation handling --------------------------------------

;;; Build a list of (ename x y radius) for every H_Perf circle in SS.
(defun mh:perflist (ss / lst i ed c)
  (setq lst '() i 0)
  (while (< i (sslength ss))
    (setq ed (entget (ssname ss i)))
    (if (= (cdr (assoc 0 ed)) "CIRCLE")
      (progn
        (setq c (cdr (assoc 10 ed)))
        (setq lst (cons (list (ssname ss i)
                              (car c) (cadr c)
                              (cdr (assoc 40 ed)))
                        lst))
      )
    )
    (setq i (1+ i))
  )
  lst
)

;;; Sorted list of unique perforation run-coordinates (the coordinate
;;; ALONG the run) for perforations lying near a given edge.
;;;   perfs    - full perforation list
;;;   axis     - 1 -> run is vertical (use perf Y), 0 -> run horizontal (perf X)
;;;   perpval  - the edge coordinate on the perpendicular axis
;;;   band     - how far inboard to look for the first rows
(defun mh:rowsnear (perfs axis perpval band / out v p pc rc)
  (setq out '())
  (foreach p perfs
    (if (= axis 1)
      (setq pc (cadr p) rc (caddr p))   ; perp = X, run = Y
      (setq pc (caddr p) rc (cadr p))   ; perp = Y, run = X
    )
    (if (<= (abs (- pc perpval)) band)
      (if (not (vl-some '(lambda (q) (< (abs (- q rc)) 0.01)) out))
        (setq out (cons rc out))
      )
    )
  )
  (vl-sort out '<)
)

;;; Nearest value in SORTEDLIST to V (list assumed non-empty).
(defun mh:nearest (v lst / best bd d)
  (setq best (car lst) bd (abs (- v best)))
  (foreach x (cdr lst)
    (setq d (abs (- v x)))
    (if (< d bd) (setq bd d best x))
  )
  best
)


;;; ---- one run (one edge) ----------------------------------------

;;; Place mounting holes along a single edge and return a list of the
;;; mounting-hole centre points (each a 2D point) that were inserted.
;;;
;;;   space    - active-space vla object
;;;   blkname  - block to insert
;;;   axis     - 1 vertical run / 0 horizontal run
;;;   edgeval  - edge coordinate on the perpendicular axis
;;;   nx ny    - inward unit normal (points into the panel interior)
;;;   r0 r1    - run start / run end coordinate along the edge
;;;   perfs    - perforation list (ename x y r)
;;;   holeR    - 3/4" hole radius
;;;   slotHalf - half slot length along slot axis
;;;   mingap   - min gap, slot edge -> panel edge
;;;   maxdist  - max distance, slot edge -> panel edge
;;;   delclr   - perforation deletion clearance (edge to edge)
(defun mh:placerun (space blkname axis edgeval nx ny r0 r1 perfs
                    holeR slotHalf mingap maxdist delclr
                    / L ngap spc i pos rows runpos
                      band near g bestg bestpen pen step
                      perp ctrs c px py rot p clr nr)
  (setq L    (- r1 r0)
        ngap (max 1 (mh:ceil (/ L *mh:maxspacing*)))
        spc  (/ L ngap))

  ;; --- ideal run positions : even, equal end margins -------------
  ;; ngap evenly sized cells; one hole centred in each cell, so the
  ;; end margins (spc/2) are equal and no gap exceeds the maximum.
  (setq runpos '() i 0)
  (while (< i ngap)
    (setq runpos (cons (+ r0 (* spc (+ i 0.5))) runpos))
    (setq i (1+ i))
  )

  ;; --- snap each run position onto the nearest perforation row ----
  (setq band (+ slotHalf maxdist 1.0)
        rows (mh:rowsnear perfs axis edgeval band))
  (if rows
    (setq runpos (mapcar '(lambda (v) (mh:nearest v rows)) runpos))
  )

  ;; --- choose the cleanest perpendicular set-back (g) -------------
  ;; g runs from the min gap to the max distance.  Centre sits at
  ;;   edge - normal*(slotHalf + g)   (outside the back edge).
  ;; Score = number of surviving perforations left only marginally
  ;; clipped; fewer is cleaner.  Ties favour the smaller g.
  (setq nr (vl-remove-if-not
             '(lambda (p)
                (< (abs (- (if (= axis 1) (cadr p) (caddr p)) edgeval))
                   (+ slotHalf maxdist holeR 1.0)))
             perfs))
  (setq bestg mingap bestpen nil step 0.02 g mingap)
  (while (<= g (+ maxdist *mh:tol*))
    (setq perp (- edgeval (* (if (= axis 1) nx ny) (+ slotHalf g)))
          pen  0)
    (foreach pos runpos
      (if (= axis 1)
        (setq c (list perp pos))          ; perp = X, run = Y
        (setq c (list pos perp))          ; perp = Y, run = X
      )
      (foreach p nr
        (setq clr (- (mh:dist2d c (cdr p)) holeR (cadddr p)))
        ;; surviving but within 0.15" of touching = a sliver
        (if (and (>= clr delclr) (< clr (+ delclr 0.15)))
          (setq pen (1+ pen))
        )
      )
    )
    (if (or (null bestpen) (< pen bestpen))
      (setq bestpen pen bestg g)
    )
    (setq g (+ g step))
  )

  ;; --- insert the blocks -----------------------------------------
  (setq perp (- edgeval (* (if (= axis 1) nx ny) (+ slotHalf bestg)))
        rot  (if (= axis 1) 0.0 (/ pi 2.0))   ; 90 deg for horizontal runs
        ctrs '())
  (foreach pos runpos
    (if (= axis 1)
      (setq px perp  py pos)
      (setq px pos   py perp)
    )
    (vla-insertblock space (vlax-3d-point (list px py 0.0))
                     blkname 1.0 1.0 1.0 rot)
    (setq ctrs (cons (list px py) ctrs))
  )
  ctrs
)


;;; ---- main command ----------------------------------------------

(defun c:MHPLACE
    (/ *error* acadobj doc space
       ssp pansegs ssperf ssblk pent blkname blklayer
       edges leftx rightx boty topy width height
       perfs geom holeR slotHalf
       mingap maxdist delclr
       centres axis r0 r1
       delcount p clr placed)

  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** MHPLACE Error: " msg))
    )
    (princ)
  )

  (setq acadobj (vlax-get-acad-object)
        doc     (vla-get-activedocument acadobj)
        space   (mh:activespace doc))

  ;; --- 1. panel block -> back edges -----------------------------
  ;; The H_Panel Hidden back-edge lines live inside the panel block,
  ;; so the user picks the block itself; we read the edges out of its
  ;; definition (handling rotation / scale / nesting) and leave the
  ;; block untouched.
  (princ "\nSelect the panel block: ")
  (setq ssp (ssget "_+.:E:S" '((0 . "INSERT"))))
  (if (null ssp)
    (progn (princ "\nNo panel block selected - cancelled.")
           (exit)))
  (setq pansegs (mh:hiddensegs (ssname ssp 0) *mh:edgelayer*))
  (if (null pansegs)
    (progn (princ (strcat "\nNo " *mh:edgelayer*
                          " lines found inside that block - cancelled."))
           (exit)))
  (setq edges (mh:paneledges pansegs))
  (if (null edges)
    (progn (princ "\nCould not determine four panel edges - cancelled.")
           (exit)))
  (setq leftx  (car edges)   rightx (cadr edges)
        boty   (caddr edges) topy   (cadddr edges)
        width  (- rightx leftx)
        height (- topy boty))

  ;; --- 2. perforations ------------------------------------------
  (princ "\nSelect the perforations: ")
  (setq ssperf (ssget (list (cons 0 "CIRCLE") (cons 8 *mh:perflayer*))))
  (setq perfs (if ssperf (mh:perflist ssperf) '()))

  ;; --- 3. mounting-hole block -----------------------------------
  (princ "\nSelect the mounting-hole block: ")
  (setq ssblk (ssget "_+.:E:S" '((0 . "INSERT"))))
  (if ssblk
    (progn
      (setq pent (ssname ssblk 0))
      (setq blkname  (vla-get-effectivename (vlax-ename->vla-object pent))
            blklayer (cdr (assoc 8 (entget pent))))
    )
    (setq blkname *mh:blockname*)
  )
  (if (null (tblobjname "BLOCK" blkname))
    (progn (princ (strcat "\nBlock \"" blkname "\" not found - cancelled."))
           (exit)))
  (setq geom     (mh:blockgeom blkname)
        holeR    (car geom)
        slotHalf (cadr geom))
  (if (<= holeR 0.0)
    (progn (princ "\nNo circle found in the block - cannot size the hole.")
           (exit)))

  ;; --- 4. numeric prompts ---------------------------------------
  (initget 6)  ; no zero, no negative
  (setq mingap (cond ((getdist "\nMinimum gap, slot edge to panel edge <0.1000>: ")) (0.1)))
  (initget 6)
  (setq maxdist (cond ((getdist "\nMaximum distance, slot edge to panel edge <1.0000>: ")) (1.0)))
  (if (< maxdist mingap) (setq maxdist mingap))
  (initget 4)  ; no negative (zero allowed)
  (setq delclr (cond ((getdist "\nPerforation deletion clearance, edge to edge <0.0625>: ")) (0.0625)))

  ;; --- decide the run orientation (longest edges win) -----------
  (setq centres '())
  (if (>= height width)
    ;; vertical runs : left & right edges
    (progn
      (setq centres
            (append
              (mh:placerun space blkname 1 leftx  1.0 0.0 boty topy
                           perfs holeR slotHalf mingap maxdist delclr)
              (mh:placerun space blkname 1 rightx -1.0 0.0 boty topy
                           perfs holeR slotHalf mingap maxdist delclr))))
    ;; horizontal runs : bottom & top edges
    (progn
      (setq centres
            (append
              (mh:placerun space blkname 0 boty 0.0  1.0 leftx rightx
                           perfs holeR slotHalf mingap maxdist delclr)
              (mh:placerun space blkname 0 topy 0.0 -1.0 leftx rightx
                           perfs holeR slotHalf mingap maxdist delclr))))
  )
  (setq placed (length centres))

  ;; --- delete colliding perforations ----------------------------
  (setq delcount 0)
  (foreach p perfs
    (if (vl-some
          '(lambda (c)
             (< (- (mh:dist2d c (list (cadr p) (caddr p)))
                   holeR (cadddr p))
                delclr))
          centres)
      (progn (entdel (car p)) (setq delcount (1+ delcount)))
    )
  )

  (princ (strcat "\nDone - placed " (itoa placed)
                 " mounting hole(s) on the "
                 (if (>= height width) "left/right" "top/bottom")
                 " runs; deleted " (itoa delcount)
                 " perforation(s)."))
  (princ)
)


(princ "\nMHPLACE.lsp loaded.  Type  MHPLACE  to run.")
(princ)

;;; ============================================================ EOF
