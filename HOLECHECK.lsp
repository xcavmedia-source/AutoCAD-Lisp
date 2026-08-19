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
;;; Every failing location is marked with a revision cloud drawn on
;;; layer  H_HoleCheck  (red).  Failures that touch one another are
;;; grouped, so a cluster of bad holes gets one cloud around it
;;; instead of one cloud per pair.
;;;
;;; The entered settings are remembered.  On each run the tool shows
;;; the stored values and asks whether to Continue with them or to
;;; Redefine them.  They are saved in the drawing (so they survive a
;;; save / reopen) and in memory (so the next drawing opens with the
;;; last values used).
;;;
;;; Command : HOLECHECK
;;;
;;; Notes : distances are measured in plan (WCS XY); Z is ignored.
;;;         values are reported in decimal drawing units, 4 places.
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- configuration ---------------------------------------------

(setq *hc:layer* "H_HoleCheck")   ; layer the revision clouds go on
(setq *hc:color* 1)               ; 1 = red

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


;;; ---- selection -------------------------------------------------

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


;;; ---- disjoint sets ---------------------------------------------
;;; Used to gather failures that share a hole into one group, so a
;;; run of bad holes is clouded once rather than once per pair.

(defun hc:uf-make (n / arr i)
  (setq arr (vlax-make-safearray vlax-vbLong (cons 0 (1- n)))
        i   0)
  (while (< i n)
    (vlax-safearray-put-element arr i i)
    (setq i (1+ i))
  )
  arr
)

(defun hc:uf-find (arr i / r p)
  (setq r i)
  (while (/= r (vlax-safearray-get-element arr r))
    (setq r (vlax-safearray-get-element arr r))
  )
  (while (/= i r)                       ; path compression
    (setq p (vlax-safearray-get-element arr i))
    (vlax-safearray-put-element arr i r)
    (setq i p)
  )
  r
)

(defun hc:uf-union (arr a b / ra rb)
  (setq ra (hc:uf-find arr a)
        rb (hc:uf-find arr b))
  (if (/= ra rb) (vlax-safearray-put-element arr ra rb))
  rb
)


;;; ---- revision cloud --------------------------------------------

;;; Draw a circular revision cloud of radius RAD around CTR as a
;;; closed LWPOLYLINE of bulged segments.  Building the cloud from
;;; DXF data instead of calling the REVCLOUD command keeps the result
;;; identical on every AutoCAD release and leaves no command echo.
;;; Vertices run counter-clockwise with a positive bulge, so every
;;; arc bows outward and nothing inside RAD is ever covered up.
;;; ARCLEN of 0 (or less) gives an automatic 12-arc cloud.
(defun hc:cloud (ctr rad arclen / n i step ang data)
  (setq n (if (> arclen 1.0e-9)
            (fix (+ 0.5 (/ (* 2.0 pi rad) arclen)))
            12))
  (setq n    (max 6 (min 60 n))
        step (/ (* 2.0 pi) n)
        i    0
        data '())
  (while (< i n)
    (setq ang  (* i step)
          data (cons (cons 42 0.5)          ; bulge -> outward arc
                     (cons (cons 10 (list (+ (car  ctr) (* rad (cos ang)))
                                          (+ (cadr ctr) (* rad (sin ang)))))
                           data))
          i    (1+ i))
  )
  (entmakex (append (list '(0 . "LWPOLYLINE")
                          '(100 . "AcDbEntity")
                          (cons 8 *hc:layer*)
                          '(100 . "AcDbPolyline")
                          (cons 90 n)
                          '(70 . 1))       ; closed
                    (reverse data)))
)


;;; ---- main command ----------------------------------------------

(defun c:HOLECHECK
    (/ *error*
       doc undo ss circles n maxrad thick arclen
       uf flags i j rest more ci cj xi yi ri xj yj rj
       dx dy dist gap fuzz novl ntight worst
       flagged clusters cl root f fx fy fr
       xmin xmax ymin ymax cx cy brad pad rr
       old nold kw ndrawn)

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

  ;;; --- settings -------------------------------------------------
  (hc:settings)
  (setq thick  *hc:thick*
        arclen (if *hc:arclen* *hc:arclen* 0.0))

  ;;; --- selection ------------------------------------------------
  (princ "\nSelect circles to check <ENTER = every circle in the current space>: ")
  (setq ss (ssget '((0 . "CIRCLE"))))

  (if (null ss)
    (progn
      (setq ss (ssget "_X" (list '(0 . "CIRCLE") (cons 410 (getvar "CTAB")))))
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

  (setq uf     (hc:uf-make n)
        flags  (hc:zeros n)
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
              (hc:uf-union uf i j)
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

  ;;; --- gather the failing holes into groups ---------------------
  (setq flagged '()
        i       0
        rest    circles)
  (while rest
    (if (= 1 (vlax-safearray-get-element flags i))
      ;; (group  x  y  radius  ename  seq)
      (setq flagged (cons (cons (hc:uf-find uf i) (car rest)) flagged))
    )
    (setq rest (cdr rest)
          i    (1+ i))
  )

  ;; sort by group, then by the unique sequence number, and walk the
  ;; sorted list to split it into one list per group
  (setq flagged (vl-sort flagged
                         '(lambda (a b)
                            (if (= (car a) (car b))
                              (< (nth 5 a) (nth 5 b))
                              (< (car a) (car b))
                            )
                          )))
  (setq clusters '()
        cl       '()
        root     nil)
  (foreach f flagged
    (if (and root (/= (car f) root))
      (setq clusters (cons cl clusters)
            cl       '())
    )
    (setq root (car f)
          cl   (cons f cl))
  )
  (if cl (setq clusters (cons cl clusters)))

  ;;; --- from here on the drawing is modified ---------------------
  (vla-startundomark doc)
  (setq undo T)

  ;;; Clear the marks left by an earlier run so results never stack
  (setq nold 0)
  (if (tblsearch "LAYER" *hc:layer*)
    (progn
      (hc:ensure-layer)                 ; on / thawed / unlocked / red
      (if (setq old (ssget "_X" (list (cons 8 *hc:layer*)
                                      (cons 410 (getvar "CTAB")))))
        (progn
          (setq nold (sslength old))
          (initget "Yes No")
          (setq kw (getkword (strcat "\n" (itoa nold)
                                     " mark(s) from an earlier check found on layer "
                                     *hc:layer* " - erase them? [Yes/No] <Yes>: ")))
          (if (= kw "No")
            (setq nold 0)
            (progn
              (setq i 0)
              (while (< i nold)
                (entdel (ssname old i))
                (setq i (1+ i))
              )
            )
          )
        )
      )
    )
  )

  ;;; --- cloud each failing group ---------------------------------
  (setq ndrawn 0)
  (if clusters (hc:ensure-layer))
  (foreach cl clusters
    ;; box that holds every circle in the group, edges included
    (setq xmin nil)
    (foreach f cl
      (setq fx (cadr f)
            fy (caddr f)
            fr (nth 3 f))
      (if (null xmin)
        (setq xmin (- fx fr)
              xmax (+ fx fr)
              ymin (- fy fr)
              ymax (+ fy fr))
        (setq xmin (min xmin (- fx fr))
              xmax (max xmax (+ fx fr))
              ymin (min ymin (- fy fr))
              ymax (max ymax (+ fy fr)))
      )
    )
    (setq cx   (* 0.5 (+ xmin xmax))
          cy   (* 0.5 (+ ymin ymax))
          brad (* 0.5 (distance (list xmin ymin) (list xmax ymax)))
          pad  (max (* 0.25 brad) thick)
          rr   (+ brad pad))
    (if (hc:cloud (list cx cy) rr arclen)
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
  (if (> nold 0)
    (princ (strcat "\n  " (hc:pad "Earlier marks erased " 28) " " (itoa nold)))
  )
  (princ (strcat "\n  " (hc:pad "Revision clouds drawn " 28) " " (itoa ndrawn)))
  (princ "\n------------------------------------------------------")

  (if (= 0 (+ novl ntight))
    (princ (strcat "\nPASS - every hole edge is at least " (hc:fmt thick)
                   " from its neighbours."))
    (princ (strcat "\nFAIL - " (itoa (+ novl ntight))
                   " bad hole pair(s) clouded on layer " *hc:layer* "."))
  )
  (princ)
)


(princ "\nHOLECHECK.lsp loaded.  Type  HOLECHECK  to run.")
(princ)

;;; ============================================================ EOF
