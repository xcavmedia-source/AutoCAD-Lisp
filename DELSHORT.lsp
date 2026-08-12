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
;;; NESTED GEOMETRY
;;;   Answer Yes to the block prompt and the command also cleans
;;;   geometry stored inside block definitions, at any depth of
;;;   nesting.  Two things to know before using it:
;;;
;;;     * A block definition is shared.  Erasing a line from it
;;;       changes EVERY insert of that block in the drawing.
;;;     * Geometry in a definition is measured in the block's own
;;;       units, before any insert scale is applied.  A 1" line in
;;;       a block inserted at 0.5 scale draws as 0.5" but is
;;;       measured as 1".
;;;
;;;   With All, every eligible definition in the drawing is
;;;   cleaned.  With Select, only the definitions behind the block
;;;   references you pick are cleaned, plus anything nested inside
;;;   them.
;;;
;;;   Left alone: xrefs and xref-dependent blocks (read-only in
;;;   the host drawing), layout blocks (covered by the normal
;;;   pass), and anonymous blocks other than the *U variants that
;;;   dynamic and exploded blocks use - so dimension blocks keep
;;;   their geometry.
;;;
;;; SETTINGS
;;;   The length, the search scope and the block answer are saved
;;;   in the current AutoCAD profile and offered as defaults on
;;;   the next run, in this drawing or any other, after a restart
;;;   included.  Press ENTER at a prompt to accept what is shown.
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


;;; ---- formatting helpers ----------------------------------------

;;; Format VALUE using the current drawing unit / precision settings
(defun ds:fmt (val)
  (rtos val (getvar "LUNITS") (getvar "LUPREC"))
)

;;; Add an "s" to WORD unless N is exactly 1
(defun ds:plural (n word)
  (strcat (itoa n) " " word (if (= n 1) "" "s"))
)


;;; ---- saved settings --------------------------------------------
;;; Stored with setenv, which keeps them in the current AutoCAD
;;; profile.  That survives closing the drawing and restarting
;;; AutoCAD, and is shared by every drawing opened under the profile.

;;; Read saved setting KEY, or DFLT when it has never been set
(defun ds:getcfg (key dflt / val)
  (if (and (setq val (getenv (strcat "DELSHORT-" key)))
           (/= val ""))
    val
    dflt
  )
)

;;; Save VAL under setting KEY.  Wrapped because a read-only profile
;;; makes setenv fail, and losing a saved default must never take the
;;; whole command down with it.
(defun ds:putcfg (key val)
  (vl-catch-all-apply 'setenv (list (strcat "DELSHORT-" key) val))
  val
)

;;; The saved length as a real, or nil when unset / unusable.
;;; Held as a plain decimal string so it reads back the same way no
;;; matter what UNITS the drawing that saved it was using.
(defun ds:getlen (/ val)
  (if (and (setq val (ds:getcfg "MaxLen" nil))
           (setq val (distof val 2))
           (> val 0.0))
    val
  )
)

(defun ds:putlen (val)
  (ds:putcfg "MaxLen" (rtos val 2 12))
)


;;; ---- drawing / entity helpers ----------------------------------

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

;;; Judge ENT against MAXLEN.  Returns (code length) where code is:
;;;   "SKIP"   - not measurable (mesh, or the curve API refuses)
;;;   "LONG"   - longer than MAXLEN, leave it alone
;;;   "LOCKED" - short enough, but on a locked layer
;;;   "MATCH"  - short enough and erasable
(defun ds:classify (ent maxlen / elen)
  (cond
    ((ds:meshp ent)                       (list "SKIP" 0.0))
    ((null (setq elen (ds:curvelen ent))) (list "SKIP" 0.0))
    ((> elen maxlen)                      (list "LONG" elen))
    ((ds:lockedp ent)                     (list "LOCKED" elen))
    (T                                    (list "MATCH" elen))
  )
)


;;; ---- block definition helpers ----------------------------------

;;; Erase OBJ, a VLA object, and return T when it actually went.
;;; vla-delete is used rather than entdel because it is unambiguous on
;;; geometry held inside a block definition, and because entdel is a
;;; toggle - handed an already-erased entity it brings it back, which
;;; is not a way an erase command should ever be able to fail.
(defun ds:vdel (obj)
  (not (vl-catch-all-error-p
         (vl-catch-all-apply 'vla-delete (list obj))))
)

;;; Read PROP from OBJ, falling back to DFLT when the object does not
;;; publish that property.
(defun ds:propp (obj prop dflt)
  (if (vlax-property-available-p obj prop)
    (vlax-get-property obj prop)
    dflt
  )
)

;;; The definition name behind a block reference.  EffectiveName sees
;;; past the anonymous name a dynamic block reports, back to the block
;;; the user actually inserted.
(defun ds:effname (obj)
  (ds:propp obj 'EffectiveName (vla-get-name obj))
)

;;; T when block definition BLK is one this command may edit.
;;; Anonymous names are refused apart from the *U family, which is
;;; where dynamic block variants and exploded geometry live; that
;;; keeps dimension (*D) and similar generated blocks intact.
(defun ds:blockok (blk / nm)
  (setq nm (vla-get-name blk))
  (and (= (ds:propp blk 'IsXRef :vlax-false) :vlax-false)
       (= (ds:propp blk 'IsLayout :vlax-false) :vlax-false)
       (not (vl-string-search "|" nm))
       (or (/= (substr nm 1 1) "*")
           (= (strcase (substr nm 1 2)) "*U"))
  )
)

;;; The block definition object called NAME, or nil when there is no
;;; such definition.
(defun ds:getblock (doc name / res)
  (setq res (vl-catch-all-apply 'vla-item (list (vla-get-blocks doc) name)))
  (if (vl-catch-all-error-p res) nil res)
)

;;; Walk outwards from the block definition called NAME and return ACC
;;; extended with an entry for it and for every definition nested
;;; inside it, to any depth.  Each entry is
;;;   (UPPERCASE-NAME . block-object)
;;; so the list doubles as the visited set - a block that somehow
;;; ends up referencing itself cannot spin forever - and carries the
;;; resolved objects, so no name has to be looked up twice.
(defun ds:collectdefs (doc name acc / blk obj)
  (if (and name
           (not (assoc (strcase name) acc))
           (setq blk (ds:getblock doc name)))
    (progn
      ;;; Recorded before descending, so self-reference terminates
      (setq acc (cons (cons (strcase name) blk) acc))
      (vlax-for obj blk
        (if (= (vla-get-objectname obj) "AcDbBlockReference")
          (setq acc (ds:collectdefs doc (ds:effname obj) acc))
        )
      )
    )
  )
  acc
)


;;; ---- main command ----------------------------------------------

(defun c:DELSHORT
    (/ *error*
       doc maxlen deflen opt defopt inblocks definb
       etypes filt ss i e ent etype elen verdict
       matches marked bmatches bseed bdefs bhits pair blk obj nm
       nmatch nline npoly nlocked nhidden nskipped ndefs nfail
       totlen ans)

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
  ;;; measured off the drawing instead of typed.
  (setq deflen (ds:getlen))
  (if deflen
    (progn
      (initget 6)                    ; no zero, no negative
      (setq maxlen (getdist (strcat "\nMaximum length <"
                                    (ds:fmt deflen) ">: ")))
      (if (null maxlen) (setq maxlen deflen))
    )
    (progn
      (initget 7)                    ; no null, no zero, no negative
      (setq maxlen (getdist "\nMaximum length: "))
    )
  )
  (ds:putlen maxlen)

  ;;; --- what to search -------------------------------------------
  (setq defopt (ds:getcfg "Scope" "All"))
  (initget "All Select")
  (setq opt (getkword (strcat "\nSearch the whole space or a selection?"
                              " [All/Select] <" defopt ">: ")))
  (if (null opt) (setq opt defopt))
  (ds:putcfg "Scope" opt)

  (setq definb (ds:getcfg "Blocks" "No"))
  (initget "Yes No")
  (setq inblocks (getkword (strcat "\nAlso clean geometry inside block"
                                   " definitions? [Yes/No] <" definb ">: ")))
  (if (null inblocks) (setq inblocks definb))
  (ds:putcfg "Blocks" inblocks)

  ;;; With Select, block references are worth picking up too - they
  ;;; are the handle on which definitions the user wants cleaned.
  (setq etypes (if (and (= inblocks "Yes") (= opt "Select"))
                 "LINE,LWPOLYLINE,POLYLINE,INSERT"
                 "LINE,LWPOLYLINE,POLYLINE"))
  (setq filt (list (cons 0 etypes)
                   (cons 410 (ds:curspace))))

  (if (= opt "Select")
    (progn
      (princ "\nSelect objects to check: ")
      (setq ss (ssget filt))
    )
    (setq ss (ssget "_X" filt))
  )

  ;;; An empty selection is only the end of the road when there is no
  ;;; block pass to follow.  A drawing whose loose geometry is already
  ;;; clean can still have plenty buried inside its definitions, and
  ;;; ssget never sees into those.
  (if (and (null ss)
           (or (= opt "Select") (= inblocks "No")))
    (progn
      (princ "\nNothing to check - command cancelled.")
      (exit)
    )
  )

  (vla-startundomark doc)

  ;;; --- pass 1 : geometry sitting loose in the current space -----
  (setq i        0
        matches  '()
        bmatches '()
        bseed    '()
        bhits    '()
        nline    0
        npoly    0
        nlocked  0
        nhidden  0
        nskipped 0
        ndefs    0
        totlen   0.0)

  (while (and ss (< i (sslength ss)))
    (setq ent   (ssname ss i)
          etype (cdr (assoc 0 (entget ent))))
    (if (= etype "INSERT")
      ;;; Not measurable itself - it is a pointer to a definition
      (setq bseed (cons ent bseed))
      (progn
        (setq verdict (ds:classify ent maxlen)
              elen    (cadr verdict))
        (cond
          ((= (car verdict) "SKIP")   (setq nskipped (1+ nskipped)))
          ((= (car verdict) "LONG")   nil)
          ((= (car verdict) "LOCKED") (setq nlocked (1+ nlocked)))
          (T
            (setq matches (cons ent matches)
                  totlen  (+ totlen elen))
            (if (= etype "LINE")
              (setq nline (1+ nline))
              (setq npoly (1+ npoly))
            )
            (if (ds:hiddenp ent) (setq nhidden (1+ nhidden)))
          )
        )
      )
    )
    (setq i (1+ i))
  )

  ;;; --- pass 2 : geometry stored inside block definitions --------
  (if (= inblocks "Yes")
    (progn
      ;;; Work out which definitions to open.  All means every one in
      ;;; the drawing; Select means only those behind the references
      ;;; picked, plus whatever is nested inside them at any depth.
      ;;; Either way ds:blockok below decides what may be edited.
      (setq bdefs '())
      (if (= opt "Select")
        (foreach e bseed
          (setq bdefs (ds:collectdefs
                        doc
                        (ds:effname (vlax-ename->vla-object e))
                        bdefs))
        )
        (vlax-for blk (vla-get-blocks doc)
          (setq bdefs (cons (cons (strcase (vla-get-name blk)) blk) bdefs))
        )
      )

      ;;; Open each eligible one and measure what is inside it.
      ;;; Layout blocks are refused here, which is what keeps model
      ;;; and paper space geometry from being collected a second time
      ;;; after pass 1 already found it and counted it.
      (foreach pair bdefs
        (setq blk (cdr pair))
        (if (ds:blockok blk)
          (progn
            (setq ndefs (1+ ndefs)
                  nm    (vla-get-name blk))
            (vlax-for obj blk
              (if (member (vla-get-objectname obj)
                          '("AcDbLine" "AcDbPolyline"
                            "AcDb2dPolyline" "AcDb3dPolyline"))
                (progn
                  (setq ent     (vlax-vla-object->ename obj)
                        verdict (ds:classify ent maxlen)
                        elen    (cadr verdict))
                  (cond
                    ((= (car verdict) "SKIP")   (setq nskipped (1+ nskipped)))
                    ((= (car verdict) "LONG")   nil)
                    ((= (car verdict) "LOCKED") (setq nlocked (1+ nlocked)))
                    (T
                      ;;; Kept as the VLA object - it is what erases
                      ;;; cleanly from inside a definition
                      (setq bmatches (cons obj bmatches)
                            totlen   (+ totlen elen))
                      (if (= (cdr (assoc 0 (entget ent))) "LINE")
                        (setq nline (1+ nline))
                        (setq npoly (1+ npoly))
                      )
                      (if (not (member nm bhits))
                        (setq bhits (cons nm bhits))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  (setq nmatch (+ (length matches) (length bmatches)))

  ;;; --- nothing to do --------------------------------------------
  (if (= nmatch 0)
    (progn
      (princ (strcat "\nNo lines or polylines found at or below "
                     (ds:fmt maxlen) "."))
      (if (> ndefs 0)
        (princ (strcat "  (" (ds:plural ndefs "block definition")
                       " searched.)"))
      )
      (if (> nlocked 0)
        (princ (strcat "\n  " (ds:plural nlocked "matching object")
                       " skipped on locked layers."))
      )
      (vla-endundomark doc)
      (exit)
    )
  )

  ;;; --- preview and confirm --------------------------------------
  ;;; Only loose geometry can be highlighted; entities living in a
  ;;; block definition are not drawn anywhere in their own right.
  (if (<= (length matches) 1000)
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

  (if bmatches
    (progn
      (princ (strcat "\n  Inside block definitions: "
                     (ds:plural (length bmatches) "object") " in "
                     (ds:plural (length bhits) "definition") "."))
      (princ "\n  Measured in block units, before insert scaling,")
      (princ " and not highlighted.")
      (setq i 0)
      (foreach nm (reverse bhits)
        (if (< i 12) (princ (strcat "\n      " nm)))
        (setq i (1+ i))
      )
      (if (> i 12)
        (princ (strcat "\n      ... and " (itoa (- i 12)) " more."))
      )
      (princ "\n  ** Erasing these changes every insert of these blocks. **")
    )
  )
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
      (setq nfail 0)
      (foreach e matches
        (if (not (ds:vdel (vlax-ename->vla-object e)))
          (setq nfail (1+ nfail))
        )
      )
      (foreach e bmatches
        (if (not (ds:vdel e)) (setq nfail (1+ nfail)))
      )
      ;;; Inserts keep drawing their old contents until a regen
      (if bmatches (vla-regen doc 1))          ; 1 = acAllViewports
      (if (= nfail 0)
        (princ (strcat "\n" (ds:plural nmatch "object")
                       " deleted - " (ds:fmt totlen)
                       " of total length removed."))
        (progn
          (princ (strcat "\n" (ds:plural (- nmatch nfail) "object")
                         " deleted."))
          (princ (strcat "\n  " (ds:plural nfail "object")
                         " left in place - AutoCAD refused the erase."))
        )
      )
    )
    (princ "\nNothing deleted.")
  )

  (vla-endundomark doc)
  (princ)
)


(princ "\nDELSHORT.lsp loaded.  Type  DELSHORT  to run.")
(princ)

;;; ============================================================ EOF
