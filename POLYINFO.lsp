;;; ============================================================
;;; POLYINFO.lsp  -  Closed Polyline Measurement Tool
;;;
;;; Selects multiple closed LWPOLYLINEs, calculates the bounding-
;;; box Length, Width, and Area of each, then writes all results
;;; plus the average Length / Width to a single MTEXT object.
;;;
;;; Length = longer  bounding-box dimension
;;; Width  = shorter bounding-box dimension
;;; Area   = AutoCAD-reported enclosed area
;;;
;;; Command : POLYINFO
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- internal helpers ------------------------------------------

;;; Format a real number using the current drawing unit & precision
(defun pi:fmt (val)
  (rtos val (getvar "LUNITS") (getvar "LUPREC"))
)

;;; Safely get the active space VLA object (model or paper)
(defun pi:activespace (doc)
  (if (and (= (getvar "TILEMODE") 0)
           (= (getvar "CVPORT") 1))
    (vla-get-paperspace doc)
    (vla-get-modelspace doc)
  )
)


;;; ---- main command ----------------------------------------------

(defun c:POLYINFO
    (/ *error*
       acadobj doc space
       ss i ent obj
       minpt maxpt dx dy len wid area
       total-len total-wid pcount
       avg-len avg-wid
       content ins-pt txtht mtext-obj)

  ;;; Local error handler - cleans up gracefully on cancel/error
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort"
                           "console break")))
      (princ (strcat "\n** POLYINFO Error: " msg))
    )
    (princ)
  )

  ;;; --- VLA setup ------------------------------------------------
  (setq acadobj (vlax-get-acad-object)
        doc     (vla-get-activedocument acadobj)
        space   (pi:activespace doc))

  ;;; --- selection ------------------------------------------------
  (princ "\nSelect closed polylines (press ENTER when done): ")
  (setq ss (ssget '((0 . "LWPOLYLINE"))))

  (if (null ss)
    (progn
      (princ "\nNothing selected - command cancelled.")
      (exit)
    )
  )

  ;;; --- iterate over selection -----------------------------------
  (setq pcount    0
        total-len 0.0
        total-wid 0.0
        ;; MTEXT content string - built up incrementally
        ;; \P = paragraph break (new line) in MTEXT raw text
        content   (strcat
                    "{\\H1.25x;\\L;Polyline Measurements\\l}\\P"
                    "\\P"))

  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent))

    ;;; Only process CLOSED polylines
    (if (equal (vlax-get-property obj 'Closed) :vlax-true)
      (progn
        (setq pcount (1+ pcount))

        ;;; Bounding box gives us the axis-aligned extents
        (vla-getboundingbox obj 'minpt 'maxpt)
        (setq minpt (vlax-safearray->list minpt)
              maxpt (vlax-safearray->list maxpt)
              dx    (abs (- (nth 0 maxpt) (nth 0 minpt)))
              dy    (abs (- (nth 1 maxpt) (nth 1 minpt))))

        ;;; Assign: longer dimension = Length, shorter = Width
        (if (>= dx dy)
          (setq len dx  wid dy)
          (setq len dy  wid dx)
        )

        ;;; AutoCAD-computed enclosed area
        (setq area (vlax-get-property obj 'Area))

        ;;; Accumulate for averages
        (setq total-len (+ total-len len)
              total-wid (+ total-wid wid))

        ;;; Append this polyline's block to the MTEXT content
        (setq content
              (strcat content
                      "{\\H1.0x;\\L;Polyline #" (itoa pcount) "\\l}\\P"
                      "  Length  =  " (pi:fmt len)  "\\P"
                      "  Width   =  " (pi:fmt wid)  "\\P"
                      "  Area    =  " (pi:fmt area) "\\P"
                      "\\P"))
      )
    )
    (setq i (1+ i))
  )

  ;;; --- guard: ensure at least one closed polyline was found -----
  (if (= pcount 0)
    (progn
      (princ "\nNo closed polylines found in the selection.")
      (exit)
    )
  )

  ;;; --- averages -------------------------------------------------
  (setq avg-len (/ total-len pcount)
        avg-wid (/ total-wid pcount))

  (setq content
        (strcat content
                "{\\H1.0x;\\L;--- AVERAGES  ("
                (itoa pcount) " polyline"
                (if (= pcount 1) "" "s") ")\\l}\\P"
                "  Avg Length  =  " (pi:fmt avg-len) "\\P"
                "  Avg Width   =  " (pi:fmt avg-wid)))

  ;;; --- insertion point ------------------------------------------
  (initget 1)  ; no null / no zero input
  (setq ins-pt (getpoint "\nSpecify insertion point for MTEXT: "))

  ;;; --- create the MTEXT object ----------------------------------
  ;;; Text height: honour current TEXTSIZE, enforce a sensible minimum
  (setq txtht (max (getvar "TEXTSIZE") 2.5))

  (setq mtext-obj
        (vla-addmtext space
                      (vlax-3d-point ins-pt)
                      0.0        ; width = 0 → no line-wrap
                      content))

  (vla-put-height mtext-obj txtht)

  (princ
    (strcat "\nDone - " (itoa pcount)
            " closed polyline(s) measured."))
  (princ)
)


(princ "\nPOLYINFO.lsp loaded.  Type  POLYINFO  to run.")
(princ)

;;; ============================================================ EOF
