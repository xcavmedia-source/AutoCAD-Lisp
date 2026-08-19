;;; ============================================================
;;; HOLECHECK.lsp  -  Hole Edge-to-Edge Spacing Checker
;;;
;;; Checks the clear (edge to edge) distance between every pair of
;;; selected CIRCLEs.  The material thickness entered by the user
;;; becomes the minimum acceptable edge distance:
;;;
;;;     gap = centre-to-centre distance - r1 - r2
;;;
;;;     gap <  0          -> the holes overlap
;;;     gap <  thickness  -> the holes are closer together than the
;;;                          material is thick
;;;
;;; Holes sitting exactly at the minimum spacing pass; the comparison
;;; carries a tolerance so rounding noise cannot fail a nominally
;;; correct pattern.
;;;
;;; HOW FAILURES ARE FLAGGED
;;;   - every hole involved in a failure is turned red
;;;   - one revision cloud is drawn around the whole panel, on layer
;;;     H_HoleCheck (red), pointing the checker at the panel so they
;;;     can go find the red holes inside it
;;;
;;; Panel outlines are optional.  Select them and only the panels
;;; holding bad holes get clouded, so a sheet full of panels can be
;;; checked in one run.  Skip them and one cloud is drawn around the
;;; extents of the holes that were checked.
;;;
;;; PUTTING THE DRAWING BACK
;;; Turning a hole red edits real geometry, so every hole this tool
;;; recolours carries hidden XDATA holding the colour it had before.
;;; HOLECHECK undoes the previous run before starting a new one, so
;;; results never pile up and a fixed hole never stays red.  Use
;;; HOLECHECKCLEAR to strip every mark before issuing the drawing.
;;;
;;; The entered settings are remembered.  On each run the tool shows
;;; the stored values and asks whether to Continue with them or to
;;; Redefine them.  They are saved in the drawing (so they survive a
;;; save / reopen) and in memory (so the next drawing opens with the
;;; last values used).
;;;
;;; Commands : HOLECHECK       run the check
;;;            HOLECHECKCLEAR  remove every mark the check left
;;;
;;; Notes : distances are measured in plan (WCS XY); Z is ignored.
;;;         values are reported in decimal drawing units, 4 places.
;;;         a hole on a locked layer cannot be recoloured; the report
;;;         says how many were skipped for that reason.
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- configuration ---------------------------------------------

(setq *hc:layer* "H_HoleCheck")   ; layer the revision clouds go on
(setq *hc:color* 1)               ; 1 = red
(setq *hc:app*   "H_HOLECHECK")   ; XDATA application name

;;; Remembered settings - deliberately NOT reset when this file is
;;; reloaded:
;;;   *hc:thick*   material thickness = minimum edge distance
;;;   *hc:arclen*  revision cloud arc length (0.0 = automatic)


;;; ---- small helpers ---------------------------------------------

;;; Format VAL as a decimal number, 4 places
(defun hc:fmt (val)
  (rtos val 2 4)
)

;;; Pad STR out to LEN characters with dots, for tidy report columns
(defun hc:pad (str len)
  (while (< (strlen str) len)
    (setq str (strcat str "."))
  )
  str
)

;;; Array of N longs, every element 0
(defun hc:zeros (n / arr i)
  (setq arr (vlax-make-safearray vlax-vbLong (cons 0 (1- n)))
        i   0)
  (while (< i n)
    (vlax-safearray-put-element arr i 0)
    (setq i (1+ i))
  )
  arr
)

;;; Every entity of the current space carrying filter FLT, or nil
(defun hc:ss-here (flt)
  (ssget "_X" (append flt (list (cons 410 (getvar "CTAB")))))
)


;;; ---- remembered settings ---------------------------------------
;;; Stored in the drawing so they survive a save / reopen.  Wrapped
;;; so that a read-only (or otherwise unwilling) drawing can never
;;; break the command.

(defun hc:ldget (key / res)
  (setq res (vl-catch-all-apply 'vlax-ldata-get (list "HOLECHECK" key)))
  (if (vl-catch-all-error-p res) nil res)
)

(defun hc:ldput (key val)
  (vl-catch-all-apply 'vlax-ldata-put (list "HOLECHECK" key val))
  val
)

;;; Pull the settings saved in this drawing into the session
;;; variables.  A value stored in the drawing wins; if the drawing
;;; has none, whatever was last typed in this AutoCAD session stays.
(defun hc:restore (/ v)
  (if (and (setq v (hc:ldget "thickness")) (numberp v) (> v 0.0))
    (setq *hc:thick* v)
  )
  (if (and (setq v (hc:ldget "arclen")) (numberp v) (>= v 0.0))
    (setq *hc:arclen* v)
  )
  (princ)
)

(defun hc:store ()
  (hc:ldput "thickness" *hc:thick*)
  (hc:ldput "arclen" (if *hc:arclen* *hc:arclen* 0.0))
  (princ)
)

;;; Ask for the material thickness and the cloud arc length.
;;; Both prompts offer the remembered value as the ENTER default.
(defun hc:ask (/ v)
  (if *hc:thick*
    (progn
      (initget 6)                       ; no zero, no negative
      (setq v (getdist (strcat "\nMaterial thickness (minimum edge distance) <"
                               (hc:fmt *hc:thick*) ">: ")))
      (if (null v) (setq v *hc:thick*))
    )
    (progn
      (initget 7)                       ; no null, no zero, no negative
      (setq v (getdist "\nMaterial thickness (minimum edge distance): "))
    )
  )
  (setq *hc:thick* v)

  (initget 4)                           ; no negative; 0 and ENTER allowed
  (setq v (getdist (strcat "\nRevision cloud arc length, 0 for automatic <"
                           (if (and *hc:arclen* (> *hc:arclen* 0.0))
                             (hc:fmt *hc:arclen*)
                             "automatic")
                           ">: ")))
  (if (null v) (setq v (if *hc:arclen* *hc:arclen* 0.0)))
  (setq *hc:arclen* v)

  (hc:store)
  (princ)
)

;;; Show what is remembered and offer to keep it or redefine it
(defun hc:settings (/ kw)
  (hc:restore)
  (if *hc:thick*
    (progn
      (princ (strcat "\nRemembered settings:"
                     "\n  " (hc:pad "Material thickness " 28) " "
                     (hc:fmt *hc:thick*)
                     "\n  " (hc:pad "Revision cloud arc length " 28) " "
                     (if (and *hc:arclen* (> *hc:arclen* 0.0))
                       (hc:fmt *hc:arclen*)
                       "automatic")))
      (initget "Continue Redefine")
      (setq kw (getkword
                 "\nContinue with these settings or Redefine? [Continue/Redefine] <Continue>: "))
      (if (= kw "Redefine") (hc:ask))
    )
    (progn
      (princ "\nNo previous settings stored - please define them.")
      (hc:ask)
    )
  )
  (princ)
)


;;; ---- result layer ----------------------------------------------

;;; Make sure H_HoleCheck exists and is red, on, thawed and unlocked
;;; so the clouds are actually visible and editable.
(defun hc:ensure-layer (/ ent ed flg)
  (if (setq ent (tblobjname "LAYER" *hc:layer*))
    (progn
      (setq ed (entget ent))
      ;; a negative colour number means the layer is switched off,
      ;; so writing the colour back also turns the layer on
      (setq ed (subst (cons 62 *hc:color*) (assoc 62 ed) ed))
      ;; clear the frozen (1) and locked (4) bits
      (setq flg (logand (cdr (assoc 70 ed)) (~ 5)))
      (setq ed (subst (cons 70 flg) (assoc 70 ed) ed))
      (entmod ed)
    )
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 *hc:layer*)
                   (cons 70 0)
                   (cons 62 *hc:color*)
                   (cons 6 "Continuous")))
  )
  (princ)
)


;;; ---- recolouring the failing holes ------------------------------
;;; The colour a hole had before is kept in XDATA on the hole itself,
;;; so the change can always be undone - including after the drawing
;;; has been saved, closed and reopened.

;;; Turn ENT red, remembering what it looked like.  T when it worked,
;;; nil when the hole could not be edited (a locked layer, usually).
(defun hc:mark-red (ent / ed aci tc res)
  (setq ed (entget ent (list *hc:app*)))
  (if (null (assoc -3 ed))              ; not already marked
    (progn
      (setq aci (cdr (assoc 62 ed))
            tc  (cdr (assoc 420 ed)))
      (setq res (vl-catch-all-apply
                  'entmod
                  (list (append ed
                                (list (list -3
                                            (list *hc:app*
                                                  (cons 1070 (if aci aci 256))
                                                  ;; -1 = there was no
                                                  ;; true-colour override
                                                  (cons 1071 (if tc tc -1)))))))))
      (if (or (vl-catch-all-error-p res) (null res)) (setq res nil) (setq res T))
    )
    (setq res T)
  )
  ;; vla-put-color also clears any true-colour override, which an
  ;; edit of DXF group 62 on its own would not
  (if res
    (setq res (not (vl-catch-all-error-p
                     (vl-catch-all-apply
                       'vla-put-color
                       (list (vlax-ename->vla-object ent) *hc:color*)))))
  )
  res
)

;;; Put ENT back the way it was and drop the XDATA.  T when the
;;; entity carried a mark, nil when there was nothing to undo.
(defun hc:unmark (ent / ed xd lst aci tc)
  (setq ed (entget ent (list *hc:app*))
        xd (cdr (assoc -3 ed)))
  (if xd
    (progn
      (setq lst (cdr (assoc *hc:app* xd))
            aci (cdr (assoc 1070 lst))
            tc  (cdr (assoc 1071 lst)))
      (vl-catch-all-apply 'vla-put-color
                          (list (vlax-ename->vla-object ent)
                                (if aci aci 256)))
      ;; hand back a true-colour override if the hole had one
      (if (and tc (>= tc 0))
        (vl-catch-all-apply 'entmod
                            (list (append (entget ent) (list (cons 420 tc)))))
      )
      ;; an application entry with no data behind it deletes the XDATA
      (setq ed (entget ent (list *hc:app*)))
      (vl-catch-all-apply 'entmod
                          (list (subst (list -3 (list *hc:app*))
                                       (assoc -3 ed)
                                       ed)))
      T
    )
    nil
  )
)

;;; Undo everything an earlier run left behind: holes back to their
;;; own colour, clouds erased.  Returns (holes . clouds).
(defun hc:clear-all (/ ss i n nhole ncloud)
  (setq nhole 0 ncloud 0)
  (if (setq ss (hc:ss-here (list (list -3 (list *hc:app*)))))
    (progn
      (setq i 0 n (sslength ss))
      (while (< i n)
        (if (hc:unmark (ssname ss i)) (setq nhole (1+ nhole)))
        (setq i (1+ i))
      )
    )
  )
  (if (tblsearch "LAYER" *hc:layer*)
    (progn
      (hc:ensure-layer)                 ; unlock it first or ENTDEL fails
      (if (setq ss (hc:ss-here (list (cons 8 *hc:layer*))))
        (progn
          (setq i 0 n (sslength ss))
          (while (< i n)
            (if (not (vl-catch-all-error-p
                       (vl-catch-all-apply 'entdel (list (ssname ss i)))))
              (setq ncloud (1+ ncloud))
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  (cons nhole ncloud)
)


;;; ---- selection --------------------------------------------------

;;; Drop circles sitting on a frozen or switched-off layer.  Only
;;; needed on the "check everything" path - a normal pick never
;;; returns those in the first place.
(defun hc:strip-hidden (ss / i n ent lay rec vis cache hidden)
  (setq i      0
        n      (sslength ss)
        cache  '()
        hidden '())
  (while (< i n)
    (setq ent (ssname ss i)
          lay (cdr (assoc 8 (entget ent))))
    (if (setq rec (assoc lay cache))
      (setq vis (cdr rec))
      (progn
        (setq rec (tblsearch "LAYER" lay)
              vis (and rec
                       (>= (cdr (assoc 62 rec)) 0)              ; on
                       (= 0 (logand 1 (cdr (assoc 70 rec))))))  ; thawed
        (setq cache (cons (cons lay vis) cache))
      )
    )
    (if (not vis) (setq hidden (cons ent hidden)))
    (setq i (1+ i))
  )
  (foreach ent hidden (ssdel ent ss))
  ss
)

;;; Turn a selection set into a list of (x y radius ename seq)
;;; records.  Centres are converted out of the circle's own OCS into
;;; WCS, so circles drawn in a rotated UCS still measure correctly.
;;; The list comes back sorted left to right on X.
(defun hc:collect (ss / i n ent ed ctr rad nrm seq lst)
  (setq i   0
        n   (sslength ss)
        seq 0
        lst '())
  (while (< i n)
    (setq ent (ssname ss i)
          ed  (entget ent)
          rad (cdr (assoc 40 ed))
          ctr (cdr (assoc 10 ed))
          nrm (cdr (assoc 210 ed)))
    (if (null nrm) (setq nrm '(0.0 0.0 1.0)))
    (setq ctr (trans ctr nrm 0))
    (if (and rad (> rad 1.0e-12))
      (setq lst (cons (list (car ctr) (cadr ctr) rad ent seq) lst)
            seq (1+ seq))
    )
    (setq i (1+ i))
  )
  ;; The unique sequence number breaks ties, so vl-sort can never
  ;; decide two circles are "equal" and throw one away.
  (vl-sort lst
           '(lambda (a b)
              (if (= (car a) (car b))
                (< (nth 4 a) (nth 4 b))
                (< (car a) (car b))
              )
            )
  )
)


;;; ---- panel outlines ---------------------------------------------

;;; Panel record: (xmin ymin xmax ymax outline ename seq).  OUTLINE is
;;; the WCS vertex list of an LWPOLYLINE, or nil for anything else -
;;; those fall back to their bounding box.
(defun hc:panels (ss / i n ent ed typ nrm elev pts obj mn mx seq lst)
  (setq i 0 n (sslength ss) seq 0 lst '())
  (while (< i n)
    (setq ent  (ssname ss i)
          ed   (entget ent)
          typ  (cdr (assoc 0 ed))
          pts  nil)
    (if (= typ "LWPOLYLINE")
      (progn
        (setq nrm  (cdr (assoc 210 ed))
              elev (cdr (assoc 38 ed)))
        (if (null nrm)  (setq nrm '(0.0 0.0 1.0)))
        (if (null elev) (setq elev 0.0))
        (foreach g ed
          (if (= 10 (car g))
            (setq pts (cons (trans (list (car (cdr g)) (cadr (cdr g)) elev)
                                   nrm 0)
                            pts))
          )
        )
        (setq pts (reverse pts))
        (if (< (length pts) 3) (setq pts nil))
      )
    )
    ;; AutoCAD's own bounding box - works whatever the entity is
    (setq obj (vlax-ename->vla-object ent))
    (if (not (vl-catch-all-error-p
               (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx))))
      (progn
        (setq mn (vlax-safearray->list mn)
              mx (vlax-safearray->list mx))
        (setq lst (cons (list (car mn) (cadr mn) (car mx) (cadr mx) pts ent seq)
                        lst)
              seq (1+ seq))
      )
    )
    (setq i (1+ i))
  )
  ;; smallest first, so a panel nested inside another one wins the test
  (vl-sort lst
           '(lambda (a b / aa ba)
              (setq aa (* (- (caddr a) (car a)) (- (cadddr a) (cadr a)))
                    ba (* (- (caddr b) (car b)) (- (cadddr b) (cadr b))))
              (if (= aa ba)
                (< (nth 6 a) (nth 6 b))
                (< aa ba)
              )
            )
  )
)

;;; Crossing-number test: T when PX,PY lies inside closed polygon PTS
(defun hc:inside-p (px py pts / a ax ay bx by in)
  (setq in nil
        a  (last pts))
  (foreach b pts
    (setq ax (car a) ay (cadr a)
          bx (car b) by (cadr b))
    ;; only edges that straddle the test line can be crossed, which
    ;; also rules out the horizontal edges that would divide by zero
    (if (or (and (<= ay py) (> by py))
            (and (<= by py) (> ay py)))
      (if (< px (+ ax (/ (* (- bx ax) (- py ay)) (- by ay))))
        (setq in (not in))
      )
    )
    (setq a b)
  )
  in
)

;;; The first panel in PANELS that holds point PX,PY, or nil.  PANELS
;;; arrives smallest first, so the tightest fit wins.
(defun hc:panel-of (px py panels / hit pts)
  (foreach p panels
    (if (and (null hit)
             (>= px (car p)) (<= px (caddr p))
             (>= py (cadr p)) (<= py (cadddr p))
             (or (null (setq pts (nth 4 p)))
                 (hc:inside-p px py pts)))
      (setq hit p)
    )
  )
  hit
)


;;; ---- revision cloud ---------------------------------------------

;;; Points along one edge, spaced about ARCLEN apart.  The far end is
;;; left out because the next edge starts there.
(defun hc:edge-pts (px py qx qy arclen / len nseg i f lst)
  (setq len  (distance (list px py) (list qx qy))
        nseg (max 1 (fix (+ 0.5 (/ len arclen))))
        i    0
        lst  '())
  (while (< i nseg)
    (setq f   (/ (float i) nseg)
          lst (cons (list (+ px (* (- qx px) f))
                          (+ py (* (- qy py) f)))
                    lst)
          i   (1+ i))
  )
  (reverse lst)
)

;;; Draw a rectangular revision cloud around the box X1 Y1 X2 Y2 as a
;;; closed LWPOLYLINE of bulged segments.  Building the cloud from DXF
;;; data instead of calling the REVCLOUD command keeps the result
;;; identical on every AutoCAD release and leaves no command echo.
;;; The corners run counter-clockwise with a positive bulge, so every
;;; arc bows outward and the panel is never covered up.
;;; ARCLEN of 0 (or less) sizes the arcs from the panel itself.
(defun hc:cloud-rect (x1 y1 x2 y2 arclen / per pts data)
  (setq per (* 2.0 (+ (- x2 x1) (- y2 y1))))
  (if (<= arclen 1.0e-9) (setq arclen (/ per 48.0)))
  ;; never let a tiny arc length turn a big panel into a vertex storm
  (setq arclen (max arclen (/ per 400.0)))
  (setq pts (append (hc:edge-pts x1 y1 x2 y1 arclen)    ; bottom, left to right
                    (hc:edge-pts x2 y1 x2 y2 arclen)    ; right, up
                    (hc:edge-pts x2 y2 x1 y2 arclen)    ; top, right to left
                    (hc:edge-pts x1 y2 x1 y1 arclen)))  ; left, down
  (setq data '())
  (foreach p pts
    (setq data (cons (cons 42 0.5)                      ; bulge -> outward arc
                     (cons (cons 10 p) data)))
  )
  (entmakex (append (list '(0 . "LWPOLYLINE")
                          '(100 . "AcDbEntity")
                          (cons 8 *hc:layer*)
                          '(100 . "AcDbPolyline")
                          (cons 90 (length pts))
                          '(70 . 1))                    ; closed
                    (reverse data)))
)

;;; Cloud the box X1 Y1 X2 Y2 after standing it off by PAD
(defun hc:cloud-box (x1 y1 x2 y2 pad arclen)
  (hc:cloud-rect (- x1 pad) (- y1 pad) (+ x2 pad) (+ y2 pad) arclen)
)


;;; ---- main command -----------------------------------------------

(defun c:HOLECHECK
    (/ *error*
       doc undo ss pss panels circles n maxrad thick arclen
       flags i j rest more ci cj xi yi ri xj yj rj
       dx dy dist gap fuzz novl ntight worst
       cleared nred nlock ent p key groups g
       xmin ymin xmax ymax pad diag ndrawn nfree)

  ;;; Local error handler - closes the undo group on cancel / error
  (defun *error* (msg)
    (if undo (vl-catch-all-apply 'vla-endundomark (list doc)))
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** HOLECHECK Error: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (regapp *hc:app*)

  ;;; --- settings -------------------------------------------------
  (hc:settings)
  (setq thick  *hc:thick*
        arclen (if *hc:arclen* *hc:arclen* 0.0))

  ;;; --- selection ------------------------------------------------
  (princ "\nSelect circles to check <ENTER = every circle in the current space>: ")
  (setq ss (ssget '((0 . "CIRCLE"))))

  (if (null ss)
    (progn
      (setq ss (hc:ss-here '((0 . "CIRCLE"))))
      (if ss
        (progn
          (setq ss (hc:strip-hidden ss))
          (princ (strcat "\nChecking every circle in "
                         (if (= 1 (getvar "TILEMODE"))
                           "model space"
                           (strcat "layout " (getvar "CTAB")))
                         "."))
        )
      )
    )
  )

  (if (or (null ss) (= 0 (sslength ss)))
    (progn
      (princ "\nNo circles found - nothing to check.")
      (exit)
    )
  )

  (setq circles (hc:collect ss)
        n       (length circles))

  (if (< n 2)
    (progn
      (princ "\nOnly one circle found - at least two are needed to check spacing.")
      (exit)
    )
  )

  ;;; --- panel outlines (optional) --------------------------------
  (princ "\nSelect panel outline(s) <ENTER = cloud the extents of the holes>: ")
  (setq pss (ssget '((0 . "LWPOLYLINE,POLYLINE,INSERT,REGION,ELLIPSE,SPLINE"))))
  (if pss (setq panels (hc:panels pss)))

  (princ (strcat "\nChecking " (itoa n) " circles against a minimum edge distance of "
                 (hc:fmt thick) " ..."))

  ;;; --- compare every pair ---------------------------------------
  ;;; The list is sorted on X, so once the X gap alone exceeds the
  ;;; allowance the rest of the list cannot fail either and the inner
  ;;; scan stops.  That keeps large hole patterns quick.
  (setq maxrad 0.0)
  (foreach ci circles
    (if (> (caddr ci) maxrad) (setq maxrad (caddr ci)))
  )

  (setq flags  (hc:zeros n)
        novl   0
        ntight 0
        worst  nil
        i      0
        rest   circles)

  (while rest
    (setq ci   (car rest)
          xi   (car ci)
          yi   (cadr ci)
          ri   (caddr ci)
          more (cdr rest)
          j    (1+ i))
    (while more
      (setq cj (car more)
            xj (car cj)
            yj (cadr cj)
            rj (caddr cj)
            dx (- xj xi))
      (if (> (- dx ri maxrad) thick)
        (setq more nil)                 ; nothing further right can fail
        (progn
          ;; FUZZ absorbs rounding noise, so holes drawn at exactly
          ;; the minimum spacing pass instead of failing by 1.0e-15
          (setq dy   (- yj yi)
                dist (sqrt (+ (* dx dx) (* dy dy)))
                gap  (- dist ri rj)
                fuzz (* 1.0e-8 (max 1.0 dist)))
          (if (< gap (- thick fuzz))
            (progn
              (if (< gap (- fuzz))
                (setq novl (1+ novl))
                (setq ntight (1+ ntight))
              )
              (if (or (null worst) (< gap (car worst)))
                (setq worst (list gap
                                  (* 0.5 (+ xi xj))
                                  (* 0.5 (+ yi yj))))
              )
              (vlax-safearray-put-element flags i 1)
              (vlax-safearray-put-element flags j 1)
            )
          )
          (setq more (cdr more)
                j    (1+ j))
        )
      )
    )
    (setq rest (cdr rest)
          i    (1+ i))
  )

  ;;; --- from here on the drawing is modified ---------------------
  (vla-startundomark doc)
  (setq undo T)

  ;;; Undo the previous run first, so a hole that has since been
  ;;; fixed does not stay red and clouds do not pile up
  (setq cleared (hc:clear-all))

  ;;; --- turn the failing holes red -------------------------------
  ;;; and sort them into the panel each one sits in
  (setq nred   0
        nlock  0
        groups '()
        i      0
        rest   circles)
  (while rest
    (if (= 1 (vlax-safearray-get-element flags i))
      (progn
        (setq ci  (car rest)
              ent (nth 3 ci))
        (if (hc:mark-red ent)
          (setq nred (1+ nred))
          (setq nlock (1+ nlock))
        )
        ;; group by panel; nil groups the holes no panel claimed
        ;; "none" collects the holes that no selected outline claimed
        (setq p   (if panels (hc:panel-of (car ci) (cadr ci) panels))
              key (if p (nth 5 p) "none")
              g   (assoc key groups))
        (if g
          (setq groups (subst (cons key (cons ci (cdr g))) g groups))
          (setq groups (cons (cons key (list ci)) groups))
        )
      )
    )
    (setq rest (cdr rest)
          i    (1+ i))
  )

  ;;; --- cloud the panels that failed -----------------------------
  (setq ndrawn 0
        nfree  0)
  (if groups (hc:ensure-layer))
  (foreach g groups
    (setq key (car g)
          p   nil)
    (if (not (equal key "none"))
      (foreach q panels (if (equal (nth 5 q) key) (setq p q)))
    )
    (if p
      ;; a real panel outline - cloud the whole panel
      (setq xmin (car p) ymin (cadr p) xmax (caddr p) ymax (cadddr p))
      ;; no outline claimed these holes - cloud what they cover
      (progn
        (setq nfree (1+ nfree)
              xmin  nil)
        (foreach ci (cdr g)
          (setq xi (car ci) yi (cadr ci) ri (caddr ci))
          (if (null xmin)
            (setq xmin (- xi ri) xmax (+ xi ri)
                  ymin (- yi ri) ymax (+ yi ri))
            (setq xmin (min xmin (- xi ri)) xmax (max xmax (+ xi ri))
                  ymin (min ymin (- yi ri)) ymax (max ymax (+ yi ri)))
          )
        )
      )
    )
    (setq diag (distance (list xmin ymin) (list xmax ymax))
          pad  (max thick (* 0.02 diag)))
    (if (hc:cloud-box xmin ymin xmax ymax pad arclen)
      (setq ndrawn (1+ ndrawn))
    )
  )

  (vla-endundomark doc)
  (setq undo nil)

  ;;; --- report ---------------------------------------------------
  (princ "\n")
  (princ "\n--- HOLECHECK results --------------------------------")
  (princ (strcat "\n  " (hc:pad "Circles checked " 28) " " (itoa n)))
  (princ (strcat "\n  " (hc:pad "Minimum edge distance " 28) " " (hc:fmt thick)))
  (princ (strcat "\n  " (hc:pad "Overlapping pairs " 28) " " (itoa novl)))
  (princ (strcat "\n  " (hc:pad "Pairs closer than minimum " 28) " " (itoa ntight)))
  (if worst
    (princ (strcat "\n  " (hc:pad "Smallest edge distance " 28) " "
                   (hc:fmt (car worst))
                   (if (< (car worst) 0.0) "  (overlap)" "")
                   "  at  " (hc:fmt (cadr worst)) "," (hc:fmt (caddr worst))))
  )
  (if (or (> (car cleared) 0) (> (cdr cleared) 0))
    (princ (strcat "\n  " (hc:pad "Marks cleared from last run " 28) " "
                   (itoa (car cleared)) " hole(s), "
                   (itoa (cdr cleared)) " cloud(s)"))
  )
  (princ (strcat "\n  " (hc:pad "Holes turned red " 28) " " (itoa nred)))
  (if (> nlock 0)
    (princ (strcat "\n  " (hc:pad "Holes on a locked layer " 28) " "
                   (itoa nlock) "  (not recoloured)"))
  )
  (princ (strcat "\n  " (hc:pad "Revision clouds drawn " 28) " " (itoa ndrawn)
                 (cond
                   ((null panels) "")
                   ((> nfree 0)
                    (strcat "  (" (itoa (- ndrawn nfree)) " of "
                            (itoa (length panels))
                            " panel(s), " (itoa nfree) " outside any panel)"))
                   (T (strcat "  of " (itoa (length panels)) " panel(s)")))))
  (princ "\n------------------------------------------------------")

  (if (= 0 (+ novl ntight))
    (princ (strcat "\nPASS - every hole edge is at least " (hc:fmt thick)
                   " from its neighbours."))
    (princ (strcat "\nFAIL - " (itoa (+ novl ntight))
                   " bad hole pair(s).  Red holes inside the cloud(s) on layer "
                   *hc:layer* "."))
  )
  (princ)
)


;;; ---- clean-up command -------------------------------------------

(defun c:HOLECHECKCLEAR (/ *error* doc undo cleared)

  (defun *error* (msg)
    (if undo (vl-catch-all-apply 'vla-endundomark (list doc)))
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** HOLECHECKCLEAR Error: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (regapp *hc:app*)
  (vla-startundomark doc)
  (setq undo T)

  (setq cleared (hc:clear-all))

  (vla-endundomark doc)
  (setq undo nil)

  (princ (strcat "\n" (itoa (car cleared)) " hole(s) put back to their own colour, "
                 (itoa (cdr cleared)) " cloud(s) erased."))
  (princ)
)


(princ "\nHOLECHECK.lsp loaded.  HOLECHECK to run, HOLECHECKCLEAR to remove the marks.")
(princ)

;;; ============================================================ EOF
