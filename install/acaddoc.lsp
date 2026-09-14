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
;;; >>> WRITE PATHS WITH FORWARD SLASHES. <<<
;;; AutoLISP reads a backslash inside quotes as an escape code, so
;;; "F:\0000-Drafting" is NOT that folder - \0 is read as a character
;;; code and the path is quietly mangled.  Forward slashes have no such
;;; problem and are converted for you below, so write:
;;;       "F:/0000-Drafting/21-CADHours"
;;; A UNC path is "//SERVER/CAD/CADHours".
;;; (Doubled backslashes, "F:\\0000-Drafting", also work if you prefer.)
;;; ============================================================

(vl-load-com)

(
  (lambda ( / cand hit c p tp)

    ;;-------------------------------------------------------------
    ;; 1.  Where the tracker lives.  The first folder that actually
    ;;     contains CADHours.lsp wins, so a laptop can carry a local
    ;;     copy that is used whenever the server is out of reach.
    ;;
    ;;     If your drive letters are not mapped the same on every PC,
    ;;     use the UNC form instead - it does not depend on a mapping.
    ;;-------------------------------------------------------------
    (setq cand
      (list
        "F:/0000-Drafting/21-CADHours"   ; <<<<<< the shared folder
        "C:/CAD/CADHours"                ; optional local fallback
      ))

    (foreach c cand
      (setq p (vl-string-translate "/" "\\" c))
      (if (and (null hit) (findfile (strcat p "\\CADHours.lsp")))
        (setq hit p)
      )
    )

    (if (null hit)

      ;;-----------------------------------------------------------
      ;; Not found.  Print the paths exactly as AutoLISP read them,
      ;; because a mangled path is the usual cause and is obvious the
      ;; moment you can see it.
      ;;-----------------------------------------------------------
      (progn
        (princ "\n** CAD Hours Tracker not found - time is NOT being tracked.")
        (princ "\n   Looked for CADHours.lsp in:")
        (foreach c cand
          (princ (strcat "\n     " (vl-string-translate "/" "\\" c)))
        )
        (princ "\n   If a path above is not what you typed, the backslashes were")
        (princ "\n   read as escape codes.  Write it with forward slashes:")
        (princ "\n     \"F:/0000-Drafting/21-CADHours\"")
        (princ "\n   If the path is right, check the folder really holds CADHours.lsp.")
      )

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
