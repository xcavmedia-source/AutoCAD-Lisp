;;; ============================================================
;;; HOLECLEAN.lsp  -  Hole Cleanup by Diameter and Edge Distance
;;;
;;; Workflow:
;;;   1. Select circles and/or block inserts that contain circles.
;;;      Circles nested inside selected blocks are automatically
;;;      included in the analysis.
;;;   2. Enter the target hole diameter (decimal inches).
;;;      Circles whose diameter matches are the "host" holes and
;;;      are never deleted.
;;;   3. Enter the minimum allowable edge distance (decimal inches).
;;;
;;; Delete rules (applied to every non-host circle):
;;;
;;;   Rule A - Inside host:
;;;     If the circle's diameter is smaller than the host AND
;;;     its center lies within the host's radius, it is deleted.
;;;
;;;   Rule B - Outside host, too close:
;;;     If the circle's center is outside every host, the
;;;     edge-to-edge gap to the nearest host is computed.
;;;     If that gap is less than the minimum edge distance,
;;;     the circle is deleted.
;;;
;;; Block handling:
;;;   Nested circles are removed from the block definition,
;;;   which affects EVERY insert of that block.  A warning is
;;;   printed whenever block definitions are modified.
;;;
;;; Command : HOLECLEAN
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP support
;;; ============================================================

(vl-load-com)


;;; ---- 2D affine transform ------------------------------------
;;;
;;; A transform is the list (origin xvec yvec), each a (x y) pair.
;;; Maps local point (a b) -> origin + a*xvec + b*yvec.

(defun hc:xf-make   (o xv yv) (list o xv yv))
(defun hc:xf-origin (xf)      (nth 0 xf))
(defun hc:xf-xvec   (xf)      (nth 1 xf))
(defun hc:xf-yvec   (xf)      (nth 2 xf))

(defun hc:xf-identity ()
  (hc:xf-make '(0.0 0.0) '(1.0 0.0) '(0.0 1.0)))

;;; Build a transform from an INSERT entity's DXF data.
(defun hc:xf-of-insert (ed / o sx sy r c s)
  (setq o  (cdr (assoc 10 ed))
        sx (cond ((cdr (assoc 41 ed))) (T 1.0))
        sy (cond ((cdr (assoc 42 ed))) (T 1.0))
        r  (cond ((cdr (assoc 50 ed))) (T 0.0))
        c  (cos r)
        s  (sin r))
  (hc:xf-make
    (list (car o) (cadr o))
    (list (* sx c) (* sx s))
    (list (* sy (- s)) (* sy c))))

;;; Apply transform to a 2D point (a b).
(defun hc:xf-pt (xf p / o xv yv a b)
  (setq o (hc:xf-origin xf) xv (hc:xf-xvec xf) yv (hc:xf-yvec xf)
        a (car p) b (cadr p))
  (list (+ (car o)  (* a (car xv))  (* b (car yv)))
        (+ (cadr o) (* a (cadr xv)) (* b (cadr yv)))))

;;; Apply only the linear part (no translation) of transform to vector v.
(defun hc:xf-vec (xf v / xv yv a b)
  (setq xv (hc:xf-xvec xf) yv (hc:xf-yvec xf)
        a (car v) b (cadr v))
  (list (+ (* a (car xv))  (* b (car yv)))
        (+ (* a (cadr xv)) (* b (cadr yv)))))

;;; Compose: outer applied after inner.
(defun hc:xf-compose (outer inner / no nx ny)
  (setq no (hc:xf-pt  outer (hc:xf-origin inner))
        nx (hc:xf-vec outer (hc:xf-xvec   inner))
        ny (hc:xf-vec outer (hc:xf-yvec   inner)))
  (hc:xf-make (list (car no) (cadr no)) nx ny))

(defun hc:vec-len (v) (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v)))))


;;; ---- circle records -----------------------------------------
;;;
;;; Each record: (ename world-center world-radius nested?)
;;;   ename        entity name (used for entdel)
;;;   world-center (x y) in model space
;;;   world-radius effective radius after insert scaling
;;;   nested?      T when the entity lives inside a block def

(defun hc:rec-ename  (r) (nth 0 r))
(defun hc:rec-ctr    (r) (nth 1 r))
(defun hc:rec-rad    (r) (nth 2 r))
(defun hc:rec-nested (r) (nth 3 r))


;;; ---- circle gathering ---------------------------------------

;;; Build all circle records from selection set SS.
;;; Top-level CIRCLEs are taken directly.
;;; For each INSERT, blocks are walked recursively.
(defun hc:gather (ss / i n e ed type result)
  (setq result '() i 0 n (sslength ss))
  (while (< i n)
    (setq e    (ssname ss i)
          ed   (entget e)
          type (cdr (assoc 0 ed)))
    (cond
      ((= type "CIRCLE")
       (setq result
         (cons (list e
                     (list (car  (cdr (assoc 10 ed)))
                           (cadr (cdr (assoc 10 ed))))
                     (cdr (assoc 40 ed))
                     nil)
               result)))
      ((= type "INSERT")
       (setq result (append result (hc:walk-insert ed (hc:xf-identity))))))
    (setq i (1+ i)))
  result)

;;; Recursively collect circles from the block referenced by INSERT-ED,
;;; applying the composed world transform XF.
(defun hc:walk-insert (insert-ed xf / composed bname be e ed type result
                                     lc lx ly wc wr sx sy unif)
  (setq composed (hc:xf-compose xf (hc:xf-of-insert insert-ed))
        bname    (cdr (assoc 2 insert-ed))
        be       (tblobjname "BLOCK" bname))
  (if (null be)
    (progn
      (princ (strcat "\n  Warning: block \"" bname "\" not found - skipped."))
      '())
    (progn
      (setq e (entnext be) result '())
      (while (and e (/= (cdr (assoc 0 (setq ed (entget e)))) "ENDBLK"))
        (setq type (cdr (assoc 0 ed)))
        (cond
          ((= type "CIRCLE")
           (setq lc   (cdr (assoc 10 ed))
                 lx   (car lc)
                 ly   (cadr lc)
                 wc   (hc:xf-pt composed (list lx ly))
                 sx   (hc:vec-len (hc:xf-xvec composed))
                 sy   (hc:vec-len (hc:xf-yvec composed))
                 unif (equal sx sy 1.0e-6))
           (if unif
             (setq result
               (cons (list e wc (* sx (cdr (assoc 40 ed))) T) result))
             (princ (strcat "\n  Skipping non-uniform-scaled circle in \""
                            bname "\" (would be ellipse)."))))
          ((= type "INSERT")
           (setq result (append result (hc:walk-insert ed composed)))))
        (setq e (entnext e)))
      result)))


;;; ---- geometry -------------------------------------------------

(defun hc:dist (p q / dx dy)
  (setq dx (- (car p) (car q)) dy (- (cadr p) (cadr q)))
  (sqrt (+ (* dx dx) (* dy dy))))


;;; ---- deletion rules ------------------------------------------

;;; Return T if record R qualifies for deletion relative to HOSTS.
;;; MIN-EDGE is the minimum allowable edge-to-edge gap.
(defun hc:should-delete (r hosts min-edge / pc rc del ph rh d edge)
  (setq pc (hc:rec-ctr r) rc (hc:rec-rad r) del nil)
  (foreach host hosts
    (if (not del)
      (progn
        (setq ph (hc:rec-ctr host)
              rh (hc:rec-rad host)
              d  (hc:dist pc ph))
        (cond
          ;; Rule A: smaller circle, center inside host
          ((and (< rc rh) (<= d rh))
           (setq del T))
          ;; Rule B: center outside host, edge gap below minimum
          ((> d rh)
           (setq edge (- d rh rc))
           (if (< edge min-edge) (setq del T)))))))
  del)


;;; ---- main command -------------------------------------------

(defun c:HOLECLEAN ( / ss target min-edge circles hosts others
                       to-delete seen top-cnt nest-cnt any-nested rec )

  ;; Selection
  (princ "\nSelect circles and/or block inserts containing circles: ")
  (setq ss (ssget '((-4 . "<OR") (0 . "CIRCLE") (0 . "INSERT") (-4 . "OR>"))))

  (if (null ss)
    (princ "\nNothing selected.")

    ;; Target diameter
    (progn
      (initget 6)
      (setq target (getreal "\nEnter target hole diameter (decimal inches): "))

      (if (null target)
        (princ "\nCancelled.")

        ;; Minimum edge distance
        (progn
          (initget 4)
          (setq min-edge (getreal "\nEnter minimum edge distance (decimal inches): "))

          (if (null min-edge)
            (princ "\nCancelled.")

            ;; Main processing
            (progn
              (princ "\nScanning selection...")

              ;; Build unified list of circle records (top-level + nested)
              (setq circles (hc:gather ss))

              (if (null circles)
                (princ "\nNo circles found in selection.")

                (progn
                  ;; Partition into hosts and candidates
                  (setq hosts '() others '())
                  (foreach rec circles
                    (if (equal (* 2.0 (hc:rec-rad rec)) target 1.0e-4)
                      (setq hosts  (cons rec hosts))
                      (setq others (cons rec others))))

                  (if (null hosts)
                    (princ (strcat "\nNo circles match target diameter "
                                   (rtos target 2 4) "\".  Nothing to do."))

                    (progn
                      ;; Determine which candidates to delete
                      (setq to-delete '())
                      (foreach rec others
                        (if (hc:should-delete rec hosts min-edge)
                          (setq to-delete (cons rec to-delete))))

                      (if (null to-delete)
                        (princ "\nNo circles met the deletion criteria.  Drawing unchanged.")

                        (progn
                          ;; Delete, deduped by entity name to avoid toggling entdel
                          (setq seen      '()
                                top-cnt   0
                                nest-cnt  0
                                any-nested nil)
                          (foreach rec to-delete
                            (if (not (member (hc:rec-ename rec) seen))
                              (progn
                                (setq seen (cons (hc:rec-ename rec) seen))
                                (entdel (hc:rec-ename rec))
                                (if (hc:rec-nested rec)
                                  (setq nest-cnt  (1+ nest-cnt)
                                        any-nested T)
                                  (setq top-cnt (1+ top-cnt))))))

                          (command "_.REGEN")

                          ;; Report
                          (princ (strcat
                            "\n\nHOLECLEAN complete."
                            "\n  Target diameter        : " (rtos target 2 4) "\""
                            "\n  Min edge distance      : " (rtos min-edge 2 4) "\""
                            "\n  Host circles (kept)    : " (itoa (length hosts))
                            "\n  Candidates checked     : " (itoa (length others))
                            "\n  Deleted (top-level)    : " (itoa top-cnt)
                            "\n  Deleted (nested/block) : " (itoa nest-cnt)))
                          (if any-nested
                            (princ (strcat
                              "\n\n*** WARNING: Block definitions were modified. "
                              "ALL inserts of those blocks are affected. ***")))
                          ))))))))))))
  (princ))
