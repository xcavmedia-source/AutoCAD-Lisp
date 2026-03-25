;;; ============================================================
;;; POLYINFO.lsp  -  Closed Polyline Measurement Tool
;;;
;;; Selects multiple closed LWPOLYLINEs and, for each one:
;;;   - Walks every segment, keeping only perfectly horizontal
;;;     (same Y) and perfectly vertical (same X) segments.
;;;     Any segment that runs at an angle is excluded.
;;;   - Length = longest axis-aligned segment found overall
;;;   - Width  = longest segment on the perpendicular axis
;;;   - Area   = AutoCAD-reported enclosed area
;;;   Length and Width are written in decimal inches.
;;;
;;; A final MTEXT block shows each polyline's results and the
;;; average Length / Width across all selected polylines.
;;;
;;; Command : POLYINFO
;;;
;;; Requirements : AutoCAD 2000+ with Visual LISP / ActiveX support
;;; ============================================================

(vl-load-com)


;;; ---- internal helpers ------------------------------------------

;;; Format VALUE as decimal inches, 4 decimal places, with " suffix
(defun pi:fmtinch (val)
  (strcat (rtos val 2 4) "\"")
)

;;; Format VALUE using the current drawing unit/precision (for area)
(defun pi:fmt (val)
  (rtos val (getvar "LUNITS") (getvar "LUPREC"))
)

;;; Return an ordered list of vertex points for an LWPOLYLINE.
;;; Each point is a (x y z) list read from DXF group code 10.
(defun pi:getverts (ent / ed pts pair)
  (setq ed  (entget ent)
        pts '())
  (foreach pair ed
    (if (= (car pair) 10)
      (setq pts (append pts (list (cdr pair))))
    )
  )
  pts
)

;;; Walk every segment of a closed LWPOLYLINE and return a 2-element
;;; list: (max-horizontal  max-vertical).
;;; A segment is horizontal when |dY| < TOL  (only dX matters).
;;; A segment is vertical   when |dX| < TOL  (only dY matters).
;;; All other (angled) segments are ignored.
(defun pi:hvsizes (ent / verts n i p1 p2 dx dy tol max-h max-v seglen)
  (setq verts (pi:getverts ent)
        n     (length verts)
        tol   1.0e-6
        max-h 0.0
        max-v 0.0)
  (setq i 0)
  (while (< i n)
    (setq p1     (nth i verts)
          p2     (nth (rem (1+ i) n) verts)  ; wraps to v0 on last step
          dx     (abs (- (car  p2) (car  p1)))
          dy     (abs (- (cadr p2) (cadr p1))))
    (cond
      ;;; Horizontal segment (dY ≈ 0)
      ((< dy tol)
       (setq seglen dx)
       (if (> seglen max-h) (setq max-h seglen))
      )
      ;;; Vertical segment (dX ≈ 0)
      ((< dx tol)
       (setq seglen dy)
       (if (> seglen max-v) (setq max-v seglen))
      )
      ;;; Angled - skip
    )
    (setq i (1+ i))
  )
  (list max-h max-v)
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
       ss i ent obj hvsizes max-h max-v len wid area
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
        ;; \P = paragraph break (new line) in raw MTEXT content
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

        ;;; Find longest horizontal and vertical segments.
        ;;; Angled segments are excluded by pi:hvsizes.
        (setq hvsizes (pi:hvsizes ent)
              max-h   (car  hvsizes)   ; longest horizontal segment
              max-v   (cadr hvsizes))  ; longest vertical   segment

        ;;; Assign Length (longer) and Width (shorter)
        (if (>= max-h max-v)
          (setq len max-h  wid max-v)
          (setq len max-v  wid max-h)
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
                      "  Length  =  " (pi:fmtinch len)  "\\P"
                      "  Width   =  " (pi:fmtinch wid)  "\\P"
                      "  Area    =  " (pi:fmt area)      "\\P"
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
                "  Avg Length  =  " (pi:fmtinch avg-len) "\\P"
                "  Avg Width   =  " (pi:fmtinch avg-wid)))

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
