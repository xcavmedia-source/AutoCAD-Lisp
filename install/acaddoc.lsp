;;; ============================================================
;;; acaddoc.lsp  -  alternative loader for the CAD Hours Tracker
;;;
;;; Use this only if you are NOT deploying the CADHours.bundle folder.
;;; AutoCAD runs acaddoc.lsp once for every drawing that is opened, so
;;; it is the other supported way to make the tracker mandatory.
;;;
;;; To use it:
;;;   1. Put the tracker's files in a folder everyone can read, for
;;;      example  \\fileserver\CAD\CADHours
;;;   2. Add that folder to  Options > Files > Support File Search Path
;;;      and to  Options > Files > Trusted Locations
;;;   3. Put this file in the same folder.
;;;
;;; If you already have an acaddoc.lsp, add the (load ...) line below
;;; to it rather than replacing your file.
;;; ============================================================

(
  (lambda ( / cand hit)
    (setq cand
      (list
        ;; the folder this file lives in, if it is on the search path
        (findfile "CADHours.lsp")
        ;; edit these to match your site
        "\\\\fileserver\\CAD\\CADHours\\CADHours.lsp"
        "C:\\CAD\\CADHours\\CADHours.lsp"
      ))
    (foreach c cand
      (if (and (null hit) c (findfile c)) (setq hit c))
    )
    (if hit
      (if (vl-catch-all-error-p (vl-catch-all-apply 'load (list hit)))
        (princ "\n** CADHours: CADHours.lsp found but failed to load.")
      )
      (princ "\n** CADHours: CADHours.lsp not found - time tracking is OFF.")
    )
    (princ)
  )
)
