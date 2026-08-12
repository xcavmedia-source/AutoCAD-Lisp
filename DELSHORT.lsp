;;; ============================================================
;;; DELSHORT.lsp  -  Delete Short Lines & Polylines
;;;
;;; Prompts for a maximum length, then finds every LINE,
;;; LWPOLYLINE and 2D/3D POLYLINE whose length is that value or
;;; SMALLER and erases them.
;;;
;;;   - Length of a LINE      = distance between its endpoints.
;;;   - Length of a POLYLINE  = total length of the whole curve,
;;;     following arc bulges and (for closed shapes) the closing
;;;     segment.  It is the path length, not a bounding box.
;;;
;;; The search covers the space you are currently working in
;;; (Model, or the active layout), or just a selection you pick.
;;; Matches are highlighted and counted, and nothing is erased
;;; until you confirm.  The whole run is one undo step, so a
;;; single U puts everything back.
;;;
;;; Objects on locked layers are reported and left alone.
;;; Polygon / polyface meshes are ignored - they share the
;;; POLYLINE entity name but are not curves.
;;;
;;; Command : DELSHORT
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- internal helpers ------------------------------------------

;;; Format VALUE using the current drawing unit / precision settings
(defun ds:fmt (val)
  (rtos val (getvar "LUNITS") (getvar "LUPREC"))
)

;;; Add an "s" to WORD unless N is exactly 1
(defun ds:plural (n word)
  (strcat (itoa n) " " word (if (= n 1) "" "s"))
)

;;; Name of the space the user is currently drawing in, for use as a
;;; DXF 410 filter so only the current tab is ever touched.
(defun ds:curspace ()
  (if (and (= (getvar "TILEMODE") 0)
           (/= (getvar "CVPORT") 1))
    "Model"                          ; inside a floating viewport
    (getvar "CTAB")                  ; Model tab, or a layout
  )
)

;;; T when ENT is a POLYLINE that is really a polygon or polyface
;;; mesh.  Those carry the POLYLINE entity name but are not curves,
;;; so they must never be measured or erased by this command.
;;; Group 70 : bit 16 = polygon mesh, bit 64 = polyface mesh.
(defun ds:meshp (ent / ed flags)
  (setq ed (entget ent))
  (and (= (cdr (assoc 0 ed)) "POLYLINE")
       (setq flags (cdr (assoc 70 ed)))
       (/= 0 (logand flags 80))
  )
)

;;; T when ENT sits on a locked layer, where it cannot be erased.
;;; Layer table group 70 : bit 4 = locked.
(defun ds:lockedp (ent / rec)
  (and (setq rec (tblsearch "LAYER" (cdr (assoc 8 (entget ent)))))
       (= 4 (logand 4 (cdr (assoc 70 rec))))
  )
)

;;; T when ENT sits on a layer that is frozen or turned off, i.e. the
;;; user cannot currently see it.  Layer table group 70 bit 1 =
;;; frozen; a negative group 62 colour means the layer is off.
(defun ds:hiddenp (ent / rec)
  (and (setq rec (tblsearch "LAYER" (cdr (assoc 8 (entget ent)))))
       (or (= 1 (logand 1 (cdr (assoc 70 rec))))
           (minusp (cdr (assoc 62 rec))))
  )
)

;;; Total length of ENT measured along its curve, or nil when it
;;; cannot be measured.
;;; A LINE is measured straight from its DXF endpoints, which stays
;;; reliable for zero-length lines - exactly the junk this command is
;;; usually pointed at, and the case the curve functions can choke on.
;;; Everything else goes through the curve API, using the end
;;; PARAMETER rather than the end point so the result is still correct
;;; for closed polylines, whose start and end points are the same.
(defun ds:curvelen (ent / ed res)
  (setq ed (entget ent))
  (if (= (cdr (assoc 0 ed)) "LINE")
    (distance (cdr (assoc 10 ed)) (cdr (assoc 11 ed)))
    (progn
      (setq res (vl-catch-all-apply
                  '(lambda (e)
                     (vlax-curve-getdistatparam e (vlax-curve-getendparam e)))
                  (list ent)))
      (if (vl-catch-all-error-p res) nil res)
    )
  )
)


;;; ---- main command ----------------------------------------------

(defun c:DELSHORT
    (/ *error*
       doc maxlen opt filt ss i e ent elen
       matches marked nmatch nline npoly
       nlocked nhidden nskipped totlen ans)

  ;;; Local error handler - drops the highlighting and closes the
  ;;; undo group so a cancel cannot leave the drawing half-marked.
  (defun *error* (msg)
    (foreach e marked (redraw e 4))
    (if doc (vla-endundomark doc))
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** DELSHORT Error: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  ;;; --- maximum length -------------------------------------------
  ;;; getdist also accepts two picked points, so the cutoff can be
  ;;; measured off the drawing instead of typed.  The previous value
  ;;; is offered as the default on later runs.
  (if *ds:lastlen*
    (progn
      (initget 6)                    ; no zero, no negative
      (setq maxlen (getdist (strcat "\nMaximum length <"
                                    (ds:fmt *ds:lastlen*) ">: ")))
      (if (null maxlen) (setq maxlen *ds:lastlen*))
    )
    (progn
      (initget 7)                    ; no null, no zero, no negative
      (setq maxlen (getdist "\nMaximum length: "))
    )
  )
  (setq *ds:lastlen* maxlen)

  ;;; --- what to search -------------------------------------------
  (setq filt (list '(0 . "LINE,LWPOLYLINE,POLYLINE")
                   (cons 410 (ds:curspace))))

  (initget "All Select")
  (setq opt (getkword "\nSearch the whole space or a selection? [All/Select] <All>: "))
  (if (null opt) (setq opt "All"))

  (if (= opt "Select")
    (progn
      (princ "\nSelect lines / polylines to check: ")
      (setq ss (ssget filt))
    )
    (setq ss (ssget "_X" filt))
  )

  (if (null ss)
    (progn
      (princ "\nNo lines or polylines to check - command cancelled.")
      (exit)
    )
  )

  (vla-startundomark doc)

  ;;; --- measure everything and collect the matches ---------------
  (setq i        0
        matches  '()
        nline    0
        npoly    0
        nlocked  0
        nhidden  0
        nskipped 0
        totlen   0.0)

  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (cond
      ;;; Mesh disguised as a POLYLINE - not a curve, leave it
      ((ds:meshp ent) (setq nskipped (1+ nskipped)))

      ;;; Anything that refuses to be measured
      ((null (setq elen (ds:curvelen ent))) (setq nskipped (1+ nskipped)))

      ;;; Longer than the cutoff - keep it
      ((> elen maxlen) nil)

      ;;; Short enough, but locked and therefore not erasable
      ((ds:lockedp ent) (setq nlocked (1+ nlocked)))

      ;;; A match
      (T
        (setq matches (cons ent matches)
              totlen  (+ totlen elen))
        (if (= (cdr (assoc 0 (entget ent))) "LINE")
          (setq nline (1+ nline))
          (setq npoly (1+ npoly))
        )
        (if (ds:hiddenp ent) (setq nhidden (1+ nhidden)))
      )
    )
    (setq i (1+ i))
  )
  (setq nmatch (length matches))

  ;;; --- nothing to do --------------------------------------------
  (if (= nmatch 0)
    (progn
      (princ (strcat "\nNo lines or polylines found at or below "
                     (ds:fmt maxlen) "."))
      (if (> nlocked 0)
        (princ (strcat "\n  " (ds:plural nlocked "matching object")
                       " skipped on locked layers."))
      )
      (vla-endundomark doc)
      (exit)
    )
  )

  ;;; --- preview and confirm --------------------------------------
  (if (<= nmatch 1000)
    (progn
      (setq marked matches)
      (foreach e marked (redraw e 3))
    )
    (princ "\n(Too many matches to highlight - preview skipped.)")
  )

  (princ (strcat "\nFound " (ds:plural nmatch "object") " at or below "
                 (ds:fmt maxlen) ":  "
                 (ds:plural nline "line") ", "
                 (ds:plural npoly "polyline") "."))
  (if (> nhidden 0)
    (princ (strcat "\n  Note: " (ds:plural nhidden "matching object")
                   " on frozen or off layers (not visible on screen)."))
  )
  (if (> nlocked 0)
    (princ (strcat "\n  " (ds:plural nlocked "matching object")
                   " skipped on locked layers."))
  )
  (if (> nskipped 0)
    (princ (strcat "\n  " (ds:plural nskipped "object")
                   " ignored (mesh, or length not measurable)."))
  )

  (initget "Yes No")
  (setq ans (getkword "\nDelete these objects? [Yes/No] <Yes>: "))
  (if (null ans) (setq ans "Yes"))

  ;;; Drop the highlighting before erasing - redraw needs live objects
  (foreach e marked (redraw e 4))
  (setq marked nil)

  (if (= ans "Yes")
    (progn
      (foreach e matches (entdel e))
      (princ (strcat "\n" (ds:plural nmatch "object")
                     " deleted - " (ds:fmt totlen)
                     " of total length removed."))
    )
    (princ "\nNothing deleted.")
  )

  (vla-endundomark doc)
  (princ)
)


(princ "\nDELSHORT.lsp loaded.  Type  DELSHORT  to run.")
(princ)

;;; ============================================================ EOF
