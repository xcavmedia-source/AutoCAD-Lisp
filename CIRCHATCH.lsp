;;; ============================================================
;;; CIRCHATCH.lsp  -  Solid-Hatch Circles
;;;
;;; Window-select any number of circles.  Each one is filled with
;;; an associative SOLID hatch, created on the CURRENT layer
;;; (CLAYER) - not on the circle's layer.
;;;
;;; Command : CIRCHATCH
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- internal helpers ------------------------------------------

;;; Safely get the active space VLA object (model or paper)
(defun ch:activespace (doc)
  (if (and (= (getvar "TILEMODE") 0)
           (= (getvar "CVPORT") 1))
    (vla-get-paperspace doc)
    (vla-get-modelspace doc)
  )
)

;;; T when layer NAME exists in DOC and is locked
(defun ch:locked-p (doc name / lay)
  (setq lay (vl-catch-all-apply 'vla-item (list (vla-get-layers doc) name)))
  (and (not (vl-catch-all-error-p lay))
       (= (vla-get-lock lay) :vlax-true))
)

;;; Build one associative SOLID hatch bounded by circle OBJ.
;;; Returns T on success, nil if the hatch could not be created.
(defun ch:hatchcircle (space obj / hatch loop)
  (not
    (vl-catch-all-error-p
      (vl-catch-all-apply
        '(lambda ()
           ;; 0 = acHatchPatternTypePreDefined, :vlax-true = associative
           (setq hatch (vla-addhatch space 0 "SOLID" :vlax-true))
           ;; AppendOuterLoop wants an array of boundary objects
           (setq loop (vlax-make-safearray vlax-vbObject '(0 . 0)))
           (vlax-safearray-put-element loop 0 obj)
           (vla-appendouterloop hatch loop)
           ;; Force the hatch onto the user's current layer
           (vla-put-layer hatch (getvar "CLAYER"))
           (vla-evaluate hatch)
        )
      )
    )
  )
)


;;; ---- main command ----------------------------------------------

(defun c:CIRCHATCH (/ *error* doc space clayer ss i made failed)

  ;;; Local error handler - cleans up gracefully on cancel/error
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** CIRCHATCH Error: " msg))
    )
    (princ)
  )

  (setq doc    (vla-get-activedocument (vlax-get-acad-object))
        space  (ch:activespace doc)
        clayer (getvar "CLAYER"))

  (cond
    ;;; Nothing can be drawn on a locked current layer
    ((ch:locked-p doc clayer)
     (princ (strcat "\nCurrent layer \"" clayer
                    "\" is locked - unlock it and try again."))
    )

    (t
     (princ "\nSelect circles to solid-hatch: ")
     (setq ss (ssget '((0 . "CIRCLE"))))

     (if (null ss)
       (princ "\nNothing selected - command cancelled.")
       (progn
         (setq i 0  made 0  failed 0)
         (while (< i (sslength ss))
           (if (ch:hatchcircle space (vlax-ename->vla-object (ssname ss i)))
             (setq made   (1+ made))
             (setq failed (1+ failed))
           )
           (setq i (1+ i))
         )
         (princ (strcat "\nDone - " (itoa made) " circle"
                        (if (= made 1) "" "s")
                        " hatched on layer \"" clayer "\"."))
         (if (> failed 0)
           (princ (strcat "  " (itoa failed) " could not be hatched."))
         )
       )
     )
    )
  )
  (princ)
)


(princ "\nCIRCHATCH.lsp loaded.  Type  CIRCHATCH  to run.")
(princ)

;;; ============================================================ EOF
