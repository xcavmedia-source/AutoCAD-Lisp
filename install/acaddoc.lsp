;;; ============================================================
;;; CAD Hours Tracker  -  loader for an existing acaddoc.lsp
;;;
;;; The whole tracker lives in ONE folder on the server.  Each PC just
;;; needs these lines, so updating the tracker later means editing the
;;; server folder once, not visiting every workstation.
;;;
;;; Use it one of two ways:
;;;
;;;   A. The PC already has an acaddoc.lsp
;;;      Paste this whole block at the very END of that file.
;;;
;;;   B. The PC has no acaddoc.lsp
;;;      Save this file, as-is, into a folder that is on
;;;      Options > Files > Support File Search Path.
;;;
;;; >>> IT MUST GO AT THE END OF AN EXISTING FILE. <<<
;;; If your acaddoc.lsp defines S::STARTUP, the tracker chains onto it
;;; so your existing start-up code keeps running - but it can only do
;;; that if it loads AFTER that definition.  Put this block first and
;;; your S::STARTUP will overwrite the tracker's.
;;;
;;; The only thing to edit is the server path on the marked line.
;;; ============================================================

(
  (lambda ( / cand hit c tp)

    ;;-------------------------------------------------------------
    ;; 1.  Where the tracker lives.  The first folder that actually
    ;;     contains CADHours.lsp wins, so a laptop can carry a local
    ;;     copy that is used whenever the server is out of reach.
    ;;-------------------------------------------------------------
    (setq cand
      (list
        "\\\\SERVER\\CAD\\CADHours"      ; <<<<<< EDIT THIS LINE
        "C:\\CAD\\CADHours"              ; optional local fallback
      ))

    (foreach c cand
      (if (and (null hit) (findfile (strcat c "\\CADHours.lsp")))
        (setq hit c)
      )
    )

    (if (null hit)
      (princ "\n** CAD Hours Tracker not found - time is NOT being tracked.")
      (progn

        ;;-----------------------------------------------------------
        ;; 2.  Tell AutoCAD the folder is safe to run code from, so
        ;;     SECURELOAD does not block it.  Harmless if the folder
        ;;     is already trusted, or if the site sets trusted
        ;;     locations by group policy.
        ;;-----------------------------------------------------------
        (setq tp (vl-catch-all-apply 'getvar (list "TRUSTEDPATHS")))
        (if (vl-catch-all-error-p tp) (setq tp nil))
        (if (and tp (null (vl-string-search (strcase hit) (strcase tp))))
          (vl-catch-all-apply 'setvar
            (list "TRUSTEDPATHS"
                  (if (= tp "") hit (strcat tp ";" hit))))
        )

        ;;-----------------------------------------------------------
        ;; 3.  Load it.  Setting *ch-home* first tells the tracker to
        ;;     read cadhours.ini and the dashboard template from this
        ;;     same folder, which is how every PC ends up logging to
        ;;     the one shared location.
        ;;-----------------------------------------------------------
        (setq *ch-home* hit)
        (if (vl-catch-all-error-p
              (vl-catch-all-apply 'load (list (strcat hit "\\CADHours.lsp"))))
          (princ "\n** CAD Hours Tracker failed to load - time is NOT being tracked.")
        )
      )
    )
    (princ)
  )
)

;;; ==================== end CAD Hours Tracker ====================
