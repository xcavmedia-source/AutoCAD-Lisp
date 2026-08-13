;;***********************************************SheetGen_v1.1 - Brendan Fezuk****************************************************;;
;;                                                                                                                                ;;
;;       -Main Functions:                                                                                                         ;;
;;                                                                                                                                ;;
;;       SheetGen:      Generate and configure a series of layouts (sheets) laid out over a model space grid.                     ;;
;;       SheetGenAdd:   Same dialog, opened straight into "append" mode.                                                          ;;
;;                                                                                                                                ;;
;;                    1. Mode:                                                                                                    ;;
;;                       - Create a new series, or add more sheets after the ones already in the drawing                          ;;
;;                                                                                                                                ;;
;;                    2. Source Layout:                                                                                           ;;
;;                       - Which layout to copy from                                                                              ;;
;;                       - Which grid position that layout is currently looking at                                                ;;
;;                       - Which grid position the first new sheet should look at                                                 ;;
;;                       - Whether the source layout itself becomes the first sheet of the batch                                  ;;
;;                                                                                                                                ;;
;;                    3. Layout Grid Configuration:                                                                               ;;
;;                       - Number of Columns (Whole Number)                                                                       ;;
;;                       - Number of Rows (Whole Number)                                                                          ;;
;;                       - Layouts in Last Row (Optional - defaults to column count)                                              ;;
;;                       - Horizontal Spacing (Decimal Number)                                                                    ;;
;;                       - Vertical Spacing (Decimal Number)                                                                      ;;
;;                                                                                                                                ;;
;;                    4. Part Number and SD Information:                                                                          ;;
;;                       - Part Numbers (Space separated, or single value for sequential)                                         ;;
;;                       - Quantities (Space separated, or single value for all layouts)                                          ;;
;;                       - SD Numbers (Space separated, or single value for sequential)                                           ;;
;;                                                                                                                                ;;
;;                    5. Confirmation Dialog:                                                                                     ;;
;;                       - Review and edit generated sheet names                                                                  ;;
;;                       - Confirm or go back to modify inputs                                                                    ;;
;;                                                                                                                                ;;
;;       Features:    - Automatic layout copying and configuration                                                                ;;
;;                    - Dynamic viewport panning for grid layout                                                                  ;;
;;                    - Append more sheets later without touching or reordering existing sheets                                   ;;
;;                    - Grid position remembered inside the drawing, so a later run picks up where the last one stopped           ;;
;;                    - Automatic sheet naming with part numbers and SD numbers                                                   ;;
;;                    - Support for sequential or custom numbering                                                                ;;
;;                    - Flexible quantity assignment                                                                              ;;
;;                                                                                                                                ;;
;;********************************************************************************************************************************;;

;;************************************************************CHANGELOG***********************************************************;;
;;                                                                                                                                ;;
;;   6/3/24  - v1.0: Initial Release                                                                                              ;;
;;                                                                                                                                ;;
;;   8/13/26 - v1.1: Append mode + bug fixes                                                                                      ;;
;;                   - New "Add more sheets" mode; new sheets are always created after the existing tabs and no                   ;;
;;                     existing tab is renamed, re-panned or moved                                                                ;;
;;                   - Grid position is stored in the drawing (SHEETGEN_STATE dictionary) so a later run defaults                 ;;
;;                     to the next position automatically                                                                         ;;
;;                   - Fixed nested dialog: the confirm dialog was started from inside the first dialog's callback                 ;;
;;                   - Fixed non-existent LAYOUT "Move" option (removed - copies already land in the right order)                 ;;
;;                   - Fixed call to non-existent vl-subseq                                                                       ;;
;;                   - Replaced the hard coded ssget crossing window with a real viewport lookup per layout                       ;;
;;                   - Renames now use ActiveX, so sheet names containing spaces are safe                                         ;;
;;                   - Sequence generator now increments only the trailing digits (RH-100 -> RH-101, not RH100-1)                 ;;
;;                   - Name validation (empty / duplicate / illegal characters / already in use) before anything is created       ;;
;;                   - Confirm dialog splits into multiple columns so 40+ sheets still fit on screen                              ;;
;;                   - (exit) calls inside dialog callbacks replaced with proper dialog flow                                      ;;
;;                   - Whole run wrapped in a single UNDO group with an error handler                                             ;;
;;                                                                                                                                ;;
;;   8/13/26 - v1.2: Fixed "bad argument type: fixnump: nil" raised by the Next button                                            ;;
;;                   - The Next and OK buttons were keyed "accept", a reserved DCL key. Its built in action                       ;;
;;                     closes the dialog by itself, so the action expression never ran and the dialog                             ;;
;;                     reported success with nothing collected. Renamed to "next" and "okbtn"                                     ;;
;;                   - The dialog result is now trusted only when the configuration really came back                              ;;
;;                   - Mode is read from the radio buttons rather than from the radio_column value                                ;;
;;                   - Every integer taken from a tile, the drawing dictionary or a config list goes through                      ;;
;;                     a coercion helper, so nil can no longer reach itoa, nth, setvar or entmake                                 ;;
;;                   - Viewport lookup no longer assumes DXF group 69 is present                                                  ;;
;;                   - Dialog callbacks run under a catch, and any error names the step it happened in                            ;;
;;                                                                                                                                ;;
;;   8/13/26 - v1.3: Fixed the Next button doing nothing at all                                                                   ;;
;;                   - The v1.2 type helpers tested (= (type val) 'STR). In AutoLISP = compares numbers                           ;;
;;                     and strings only, so comparing two symbols raised an error on every single call,                           ;;
;;                     starting with the first line of the Next handler. Now uses eq                                              ;;
;;                   - A rejected Next now raises a message box listing what the dialog actually read,                            ;;
;;                     rather than only writing to the error tile and the command line hidden behind it                           ;;
;;                                                                                                                                ;;
;;   8/13/26 - v1.4: Input handling                                                                                               ;;
;;                   - Values may now be separated by commas as well as spaces, so 1,6,1,1 works                                  ;;
;;                     (a comma is illegal in a layout name, so it is only ever a separator here)                                 ;;
;;                   - An empty grid position box now falls back to the default for the chosen mode                               ;;
;;                     instead of being rejected as "must be 1 or more"                                                           ;;
;;                   - If AutoCAD opens an older sheetgen.dcl that is missing the Mode and Source                                 ;;
;;                     Layout tiles, that is now detected and reported instead of failing obscurely                               ;;
;;                                                                                                                                ;;
;;   8/13/26 - v1.5: Continuing a grid across batches                                                                             ;;
;;                   - Appending now restores Columns and both spacings from the drawing, not just the                            ;;
;;                     position. They describe the physical model space grid, so a value retyped even                             ;;
;;                     slightly differently would have mis-panned every sheet in the new batch                                    ;;
;;                   - Each grid position box now reads out "= row R, col C" as you type, and a summary                           ;;
;;                     line shows the first and last position the run will cover                                                  ;;
;;                                                                                                                                ;;
;;   8/13/26 - v1.6: Confirmed the grid fills continuously                                                                        ;;
;;                   - A batch that stops part way along a row is not a special case. The next batch                              ;;
;;                     carries on at the next column of that same row, so the stored position + 1 is                              ;;
;;                     always the right place to resume and never needs correcting by hand                                        ;;
;;                   - Said so in the dialog, since the position boxes are otherwise easy to misread as                           ;;
;;                     something you are expected to work out yourself                                                            ;;
;;                                                                                                                                ;;
;;   8/13/26 - v1.7: Friendlier DCL version mismatch                                                                              ;;
;;                   - The out of date DCL check no longer counts the three readout tiles, which are                              ;;
;;                     cosmetic. An older dialog file now loses the row/column readout instead of                                 ;;
;;                     refusing to open                                                                                           ;;
;;                   - The message now names the tiles that are actually missing, says the two files are                          ;;
;;                     a matched pair, and explains that a stale copy earlier on the support file search                          ;;
;;                     path will be found first                                                                                   ;;
;;                   - The message now prints the full path of the sheetgen.dcl AutoCAD actually opened                           ;;
;;                   - New SheetGenWhere command reports the loaded version and the dcl path and date                             ;;
;;                                                                                                                                ;;
;;   8/13/26 - v1.8: The setup dialog now lives inside this file                                                                  ;;
;;                   - sheetgen.dcl is no longer needed. SheetGen.lsp writes the dialog out to a                                  ;;
;;                     temporary file and loads that, so there is one file to install and the two can                             ;;
;;                     never drift apart or be mismatched again                                                                   ;;
;;                   - This also takes the support file search path out of the picture entirely.                                  ;;
;;                     SheetGen.lsp can be loaded from any folder; nothing else has to be findable                                ;;
;;                   - A sheetgen.dcl on the search path is now only a fallback, used if the temporary                            ;;
;;                     file cannot be written. Old copies can be deleted                                                          ;;
;;                   - SheetGenWhere reports which dialog is actually in use                                                      ;;
;;                                                                                                                                ;;
;;   8/13/26 - v1.9: Display locked viewports                                                                                     ;;
;;                   - A display locked viewport silently refuses to pan, so sheets came out unpanned and                         ;;
;;                     the run fell over shortly after. Locked viewports are now released before panning                          ;;
;;                     and locked again when the run finishes                                                                     ;;
;;                   - Only the viewports actually unlocked are locked again, tracked by entity name, so                          ;;
;;                     a viewport that was already unlocked stays that way                                                        ;;
;;                   - The error handler re-locks as well, so a run that stops part way cannot leave                              ;;
;;                     viewports unlocked. The whole run including the lock changes is one UNDO step                              ;;
;;                                                                                                                                ;;
;;  8/13/26 - v1.10: Views are set directly instead of by running PAN                                                             ;;
;;                   - Positioning used MSPACE / CVPORT / -PAN / PSPACE. Driving it with commands means                           ;;
;;                     switching spaces and depending on which viewport AutoCAD treats as active, and                             ;;
;;                     when that went wrong the pan did not fail - it landed on paper space or on another                         ;;
;;                     layout, which moved the views in sheets that were already finished                                         ;;
;;                   - The viewport's view centre (DXF group 12) is now edited directly. Nothing outside                          ;;
;;                     the sheet being positioned can be touched, and no space switching is involved                              ;;
;;                   - Each move prints "view A -> B (cols N) dx .. dy .." so a wrong grid step is visible                        ;;
;;                                                                                                                                ;;
;;  8/13/26 - v1.11: Absolute positioning, verified before it acts                                                                ;;
;;                   - entmod on the view centre was accepted but had no effect, so every sheet came out                          ;;
;;                     showing the source view. Back to ZOOM Center, which the command form proved does                           ;;
;;                     work, but aimed at a destination rather than given a displacement                                          ;;
;;                   - Every sheet is now positioned from one reference, the source layout's view centre                          ;;
;;                     and grid position. Sheets no longer chain off each other, so one that fails cannot                         ;;
;;                     drag the rest out of step                                                                                  ;;
;;                   - CTAB and CVPORT are checked after being set. If either is not what was asked for the                       ;;
;;                     view is left alone and it says so, rather than zooming whatever is active - which is                       ;;
;;                     how sheets that were already finished got moved                                                            ;;
;;                   - Each sheet prints the centre it is aimed at                                                                ;;
;;                                                                                                                                ;;
;;  8/13/26 - v1.12: Renaming moved to the end of the run                                                                         ;;
;;                   - Sheets are created and positioned under a plain temporary name, and the real                               ;;
;;                     names are applied once nothing else depends on them. The copy chain has to make                            ;;
;;                     each layout current, and a sheet name carries padding spaces that do not always                            ;;
;;                     survive a round trip through CTAB and the LAYOUT command                                                   ;;
;;                   - A failed rename now reports the reason AutoCAD gave, instead of a bare message                             ;;
;;                   - The name AutoCAD actually stored is read back, and a name it altered is reported                           ;;
;;                                                                                                                                ;;
;;  8/13/26 - v1.13: A sheet that will not position no longer stops the run                                                       ;;
;;                   - Positioning is absolute, so one sheet failing cannot affect another. It now runs                           ;;
;;                     under a catch: the sheets are still built and named and the run finishes, and any                          ;;
;;                     sheet that was missed is reported so its view can be set by hand                                           ;;
;;                   - The sheet viewport is chosen as the largest one on the paper rather than whichever                         ;;
;;                     came back first. A title block sheet often carries small extra viewports, and                              ;;
;;                     driving one of those left the actual drawing view untouched                                                ;;
;;                   - CVPORT alone moves in and out of a viewport, so ZOOM is the only command left in                           ;;
;;                     the positioning path. MSPACE and PSPACE were two more chances for the command                              ;;
;;                     stream to end up somewhere unexpected                                                                      ;;
;;                   - The view height used is printed, so a sheet coming out at the wrong scale shows up                         ;;
;;                                                                                                                                ;;
;;********************************************************************************************************************************;;

(vl-load-com)

(setq sheetgenversion "1.13")


;;;-----------------------------------------------------------------------------------------------;;
;;;                                      String Utilities                                          ;;
;;;-----------------------------------------------------------------------------------------------;;

;;; Coerce to an integer, falling back to DFLT.  Anything handed to itoa, nth,
;;; setvar or entmake goes through here so a nil can never reach them.
(defun sg:Int (val dflt)
  (cond
    ;; note: = compares numbers and strings only, symbols need eq
    ((eq (type val) 'INT)  val)
    ((eq (type val) 'REAL) (fix val))
    ((eq (type val) 'STR)  (atoi val))
    (T                    dflt)
  )
)

(defun sg:Str (val dflt)
  (if (eq (type val) 'STR) val dflt)
)

(defun sg:Num (val dflt)
  (if (numberp val) val dflt)
)

(defun sg:Trim (str)
  (vl-string-trim " \t\r\n" (sg:Str str ""))
)

;;; Index of ITEM in LST, or nil.  Avoids relying on vl-position.
(defun sg:Index (item lst / i n)
  (setq i 0 n nil)
  (while (and lst (null n))
    (if (equal item (car lst)) (setq n i))
    (setq lst (cdr lst)
          i   (1+ i))
  )
  n
)

(defun sg:Last (lst)
  (if lst (nth (1- (length lst)) lst))
)

;;; Run a dialog callback under a catch, so a failure inside it reports what
;;; actually went wrong instead of tearing the dialog down with a bare message.
(defun sg:Guard (label fn / r)
  (setq *sg:step* label)
  (setq r (vl-catch-all-apply fn nil))
  (if (vl-catch-all-error-p r)
    (progn
      (setq r (vl-catch-all-error-message r))
      (princ (strcat "\n** SheetGen failed during " label ": " r))
      ;; the command line is hidden behind a modal dialog, so say it out loud
      (vl-catch-all-apply 'set_tile (list "error" (strcat label ": " r)))
      (alert (strcat "SheetGen hit an error during " label ":\n\n" r
                     "\n\nThe dialog is still open - press Cancel to back out."))
      nil
    )
    r
  )
)

;;; Split STR on spaces, tabs and commas, discarding empty tokens.
;;; Commas count as separators because they are illegal in a layout name
;;; anyway, and typing a comma separated list is the natural thing to do.
(defun sg:Split (str / res tok i ch len)
  (setq res '()
        tok ""
        i   1
        len (strlen str))
  (while (<= i len)
    (setq ch (substr str i 1))
    (if (or (= ch " ") (= ch "\t") (= ch ","))
      (progn
        (if (/= tok "") (setq res (cons tok res)))
        (setq tok "")
      )
      (setq tok (strcat tok ch))
    )
    (setq i (1+ i))
  )
  (if (/= tok "") (setq res (cons tok res)))
  (reverse res)
)

;;; First N elements of LST (fewer if LST is shorter).
(defun sg:Sublist (lst n / res)
  (setq res '())
  (while (and lst (> n 0))
    (setq res (cons (car lst) res)
          lst (cdr lst)
          n   (1- n))
  )
  (reverse res)
)

(defun sg:PadRight (str len)
  (while (< (strlen str) len)
    (setq str (strcat str " "))
  )
  str
)

;;; Split STR into (prefix . trailing-digits).  "RH-100" -> ("RH-" . "100")
;;; A string with no trailing digits comes back as (str . "").
(defun sg:SplitTail (str / i ch pos)
  (setq pos (1+ (strlen str))
        i   (strlen str))
  (while (and (> i 0)
              (setq ch (ascii (substr str i 1)))
              (>= ch 48)
              (<= ch 57))
    (setq pos i
          i   (1- i))
  )
  (if (> pos (strlen str))
    (cons str "")
    (cons (substr str 1 (1- pos)) (substr str pos))
  )
)

;;; Repeat VAL COUNT times.
(defun sg:Repeat (val count / i out)
  (setq i 0 out '())
  (while (< i count)
    (setq out (cons val out)
          i   (1+ i))
  )
  out
)

;;; Build a COUNT-long list from INPUT.
;;;   several tokens   -> used as given (truncated to COUNT)
;;;   one token, digits-> incremented from the trailing digits, zero padding kept
;;;   one token, no digits -> repeated
(defun sg:SeqList (input count / tokens p num width i out s)
  (setq tokens (sg:Split input))
  (cond
    ((null tokens) nil)
    ((> (length tokens) 1)
     (sg:Sublist tokens count)
    )
    (T
     (setq p (sg:SplitTail (car tokens)))
     (if (= (cdr p) "")
       (sg:Repeat (car tokens) count)
       (progn
         (setq num   (atoi (cdr p))
               width (strlen (cdr p))
               i     0
               out   '())
         (while (< i count)
           (setq s (itoa (+ num i)))
           (while (< (strlen s) width) (setq s (strcat "0" s)))
           (setq out (cons (strcat (car p) s) out)
                 i   (1+ i))
         )
         (reverse out)
       )
     )
    )
  )
)

;;; Quantities are never sequenced - a single value is repeated for every sheet.
(defun sg:QtyList (input count / tokens)
  (setq tokens (sg:Split input))
  (cond
    ((null tokens) nil)
    ((= (length tokens) 1) (sg:Repeat (car tokens) count))
    (T (sg:Sublist tokens count))
  )
)


;;;-----------------------------------------------------------------------------------------------;;
;;;                                      Layout Utilities                                          ;;
;;;-----------------------------------------------------------------------------------------------;;

(defun sg:Doc ()
  (vla-get-ActiveDocument (vlax-get-acad-object))
)

;;; Paper space layout names in tab order (Model is tab order 0 and is skipped).
(defun sg:LayoutNames (/ lst)
  (setq lst '())
  (vlax-for l (vla-get-Layouts (sg:Doc))
    (if (> (vla-get-TabOrder l) 0)
      (setq lst (cons (cons (vla-get-TabOrder l) (vla-get-Name l)) lst))
    )
  )
  (mapcar 'cdr (vl-sort lst '(lambda (a b) (< (car a) (car b)))))
)

(defun sg:LayoutExists (nm)
  (if (vl-some '(lambda (x) (= (strcase x) (strcase nm))) (sg:LayoutNames)) T nil)
)

;;; BASE, or BASE with a number appended, whichever is not taken yet.
(defun sg:UniqueName (base / nm i)
  (setq nm base i 0)
  (while (sg:LayoutExists nm)
    (setq i  (1+ i)
          nm (strcat base (itoa i)))
  )
  nm
)

;;; Rename through ActiveX - the LAYOUT command would eat the spaces in a padded
;;; name.  Returns the name AutoCAD actually stored, which is not always the one
;;; asked for, or nil on failure with the reason left in *sg:renameerr*.
(defun sg:Rename (old new / lay r)
  (setq *sg:renameerr* nil)
  (if (= old new)
    new
    (progn
      (setq r (vl-catch-all-apply
                '(lambda ()
                   (setq lay (vla-Item (vla-get-Layouts (sg:Doc)) old))
                   (vla-put-Name lay new)
                   (vla-get-Name lay))))
      (if (vl-catch-all-error-p r)
        (progn
          (setq *sg:renameerr* (vl-catch-all-error-message r))
          nil
        )
        r
      )
    )
  )
)

;;; Some releases expose -LAYOUT, some only LAYOUT.  Work out which once and cache it.
(defun sg:LayoutCmd (/ r)
  (if (not *sg:layoutcmd*)
    (progn
      (setq r (vl-catch-all-apply 'getcname (list "_-LAYOUT")))
      (setq *sg:layoutcmd*
            (if (and (not (vl-catch-all-error-p r)) r) "_.-LAYOUT" "_.LAYOUT"))
    )
  )
  *sg:layoutcmd*
)

;;; Cancel out of anything left running at the command line.
(defun sg:ClearCmd (/ n)
  (setq n 0)
  (while (and (> (getvar "CMDACTIVE") 0) (< n 8))
    (command)
    (setq n (1+ n))
  )
)

;;; Copy layout FROM to a new layout named TO.  Returns T on success.
;;; FROM is selected by making it current and taking the prompt default, so a
;;; name containing spaces never has to travel through (command).
(defun sg:CopyLayoutTo (from to)
  (setvar "CTAB" from)
  (command (sg:LayoutCmd) "_Copy" "" to)
  (sg:ClearCmd)
  (if (not (sg:LayoutExists to))
    (progn
      (command (sg:LayoutCmd) "_Copy" from to)
      (sg:ClearCmd)
    )
  )
  (sg:LayoutExists to)
)

;;; Every real (non paper space) viewport entity in layout LNAME.
;;; Viewport 1 is the paper space view itself and is never one of these.
(defun sg:Viewports (lname / ss i e out)
  (setq out '())
  (if (setq ss (ssget "_X" (list '(0 . "VIEWPORT") (cons 410 lname))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (if (/= 1 (sg:Int (cdr (assoc 69 (entget e))) 1))
          (setq out (cons e out))
        )
        (setq i (1+ i))
      )
    )
  )
  (reverse out)
)

;;; T when viewport entity E has its display locked.
(defun sg:VpLockedP (e / r)
  (setq r (vl-catch-all-apply 'vla-get-DisplayLocked
                              (list (vlax-ename->vla-object e))))
  (if (vl-catch-all-error-p r) nil (equal r :vlax-true))
)

(defun sg:VpSetLock (e state)
  (vl-catch-all-apply 'vla-put-DisplayLocked
                      (list (vlax-ename->vla-object e)
                            (if state :vlax-true :vlax-false)))
)

;;; Re-lock everything sg:PanView released.  Copies of a locked viewport
;;; are themselves locked, so every new sheet gets released and re-locked too.
(defun sg:RelockAll (/ n)
  (setq n (length *sg:unlocked*))
  (foreach e *sg:unlocked* (sg:VpSetLock e T))
  (setq *sg:unlocked* nil)
  n
)

;;; Model space offset of grid position IDX.
;;; Positions are 1 based and fill the grid continuously: row 1 left to right,
;;; then row 2, and so on.  A batch that stops part way along a row is not a
;;; special case - the next batch simply carries on at the next column.  With
;;; 10 columns, position 28 is row 3 column 8 and position 29 is row 3 column 9.
(defun sg:GridOffset (idx cols hspace vspace / n)
  (setq n (max 0 (1- idx)))
  (list (* (rem n cols) hspace)
        (* -1.0 (/ n cols) vspace)
        0.0)
)

;;; The sheet viewport in LNAME: the one with the largest area on the paper.
;;; A title block sheet often carries small extra viewports, and picking
;;; whichever happened to come back first meant sometimes driving one of those
;;; and leaving the actual drawing view untouched.
(defun sg:MainViewport (lname / best ba e ed a)
  (foreach e (sg:Viewports lname)
    (setq ed (entget e)
          a  (* (sg:Num (cdr (assoc 40 ed)) 0.0)
                (sg:Num (cdr (assoc 41 ed)) 0.0)))
    (if (or (null best) (> a ba)) (setq best e ba a))
  )
  best
)

;;; sg:SetView under a catch.  Positioning is absolute, so a sheet that cannot
;;; be positioned does not affect any other sheet - there is no reason to
;;; abandon the run over one.  The sheets still get built and named, and a view
;;; that was missed can be set by hand afterwards.
(defun sg:TrySetView (lname pos basepos basectr cols hspace vspace / r)
  (setq r (vl-catch-all-apply
            'sg:SetView
            (list lname pos basepos basectr cols hspace vspace)))
  (if (vl-catch-all-error-p r)
    (progn
      (sg:ClearCmd)
      (if (/= 1 (sg:Int (getvar "CVPORT") 1)) (setvar "CVPORT" 1))
      (princ (strcat "\n   ** could not position this sheet: "
                     (vl-catch-all-error-message r)))
      nil
    )
    r
  )
)

;;; Model space point the sheet viewport in LNAME is centred on, read from its
;;; view centre (DXF group 12).  nil if unreadable.
(defun sg:ViewCentre (lname / e ctr)
  (if (and (setq e (sg:MainViewport lname))
           (setq ctr (assoc 12 (entget e))))
    (list (car (cdr ctr)) (cadr (cdr ctr)) 0.0)
  )
)

;;; Point layout LNAME's viewport at grid position POS.
;;;
;;; BASECTR is the model point the source layout was centred on and BASEPOS is
;;; the grid position it was showing, so the centre for any sheet is worked out
;;; from those two.  That makes every sheet positioned absolutely and
;;; independently: one sheet failing cannot drag the rest out of step, which a
;;; chain of relative pans does.
;;;
;;; ZOOM Center is used rather than PAN for the same reason - it takes the
;;; destination outright instead of a displacement from wherever the view
;;; happens to be.  CTAB and CVPORT are checked after being set, because if the
;;; layout or the viewport is not the one intended the zoom does not fail, it
;;; just lands somewhere else - which is how sheets that were already finished
;;; ended up being moved.
(defun sg:SetView (lname pos basepos basectr cols hspace vspace / e ed vid hgt tgt ok)
  (setq ok nil)
  (if (and basectr (setq e (sg:MainViewport lname)))
    (progn
      (setq ed  (entget e)
            vid (sg:Int (cdr (assoc 69 ed)) nil)
            hgt (sg:Num (cdr (assoc 45 ed)) nil)   ; view height, keeps the scale
            tgt (mapcar '+
                        basectr
                        (mapcar '-
                                (sg:GridOffset pos     cols hspace vspace)
                                (sg:GridOffset basepos cols hspace vspace))))
      (princ (strcat "\n   position " (itoa pos)
                     " -> centre " (rtos (car tgt) 2 2) "," (rtos (cadr tgt) 2 2)
                     "  height " (if hgt (rtos hgt 2 2) "unknown")))

      ;; ZOOM inside a display locked viewport zooms paper space instead
      (if (sg:VpLockedP e)
        (progn
          (sg:VpSetLock e nil)
          (setq *sg:unlocked* (cons e *sg:unlocked*))
        )
      )

      (setvar "CTAB" lname)
      (cond
        ((/= (strcase (getvar "CTAB")) (strcase lname))
         (princ (strcat "\n   ** could not make layout \"" lname "\" current - view left alone")))
        ((null vid)
         (princ "\n   ** viewport has no ID - view left alone"))
        (T
         ;; CVPORT alone moves in and out of a viewport, so ZOOM is the only
         ;; command involved.  MSPACE and PSPACE are two more chances for the
         ;; command stream to end up somewhere unexpected.
         (setvar "CVPORT" vid)
         (if (/= (getvar "CVPORT") vid)
           (princ "\n   ** could not activate the viewport - view left alone")
           (progn
             (if hgt
               (command "_.ZOOM" "_C" "_non" tgt hgt)
               (command "_.ZOOM" "_C" "_non" tgt "")
             )
             (sg:ClearCmd)
             (setq ok T)
           )
         )
         (setvar "CVPORT" 1)                ; back out to paper space
        )
      )
    )
    (princ (strcat "\n   ** no viewport found in \"" lname "\" - view left alone"))
  )
  ok
)


;;;-----------------------------------------------------------------------------------------------;;
;;;                          Grid State Stored Inside The Drawing                                  ;;
;;;-----------------------------------------------------------------------------------------------;;
;;; Kept in the named object dictionary so appending later works even after the
;;; drawing has been closed and reopened on another machine.

(defun sg:StateSave (cols hspace vspace lastidx lastname / xr)
  (if (setq xr (entmakex (list '(0 . "XRECORD")
                               '(100 . "AcDbXrecord")
                               (cons 1 lastname)
                               (cons 70 cols)
                               (cons 71 lastidx)
                               (cons 40 hspace)
                               (cons 41 vspace))))
    (progn
      (if (dictsearch (namedobjdict) "SHEETGEN_STATE")
        (dictremove (namedobjdict) "SHEETGEN_STATE")
      )
      (dictadd (namedobjdict) "SHEETGEN_STATE" xr)
    )
  )
)

;;; -> (cols hspace vspace lastidx lastname) or nil
(defun sg:StateRead (/ e)
  (if (setq e (dictsearch (namedobjdict) "SHEETGEN_STATE"))
    (list (cdr (assoc 70 e))
          (cdr (assoc 40 e))
          (cdr (assoc 41 e))
          (cdr (assoc 71 e))
          (cdr (assoc 1 e)))
  )
)


;;;-----------------------------------------------------------------------------------------------;;
;;;                                     Name Validation                                            ;;
;;;-----------------------------------------------------------------------------------------------;;

;;; Characters AutoCAD will not accept in a layout name, as found in NM.
(defun sg:BadChars (nm / bad i ch)
  (setq bad "" i 1)
  (while (<= i (strlen nm))
    (setq ch (substr nm i 1))
    (if (and (vl-string-search ch "<>/\\\":;?*|=,")
             (not (vl-string-search ch bad)))
      (setq bad (strcat bad ch))
    )
    (setq i (1+ i))
  )
  bad
)

;;; NAMES is the proposed batch, EXISTING an upper case list of names already in use.
;;; Returns a list of problem descriptions - nil means everything is fine.
(defun sg:ValidateNames (names existing / msgs seen nm i bad)
  (setq msgs '() seen '() i 0)
  (foreach nm names
    (setq i (1+ i))
    (cond
      ((= (sg:Trim nm) "")
       (setq msgs (cons (strcat "Sheet " (itoa i) ": the name is empty.") msgs)))
      ((> (strlen nm) 255)
       (setq msgs (cons (strcat "Sheet " (itoa i) ": longer than 255 characters.") msgs)))
      ((/= "" (setq bad (sg:BadChars nm)))
       (setq msgs (cons (strcat "Sheet " (itoa i) ": illegal character(s) " bad) msgs)))
      ((member (strcase nm) seen)
       (setq msgs (cons (strcat "Sheet " (itoa i) ": \"" nm "\" is used twice in this batch.") msgs)))
      ((member (strcase nm) existing)
       (setq msgs (cons (strcat "Sheet " (itoa i) ": a layout named \"" nm "\" already exists.") msgs)))
    )
    (setq seen (cons (strcase nm) seen))
  )
  (reverse msgs)
)

(defun sg:ReportProblems (msgs / s n)
  (setq s "SheetGen cannot continue:\n\n" n 0)
  (foreach m msgs
    (if (< n 12)
      (setq s (strcat s m "\n"))
    )
    (setq n (1+ n))
  )
  (if (> n 12)
    (setq s (strcat s "...and " (itoa (- n 12)) " more.\n"))
  )
  (alert s)
)


;;;-----------------------------------------------------------------------------------------------;;
;;;                                Confirm / Edit Names Dialog                                     ;;
;;;-----------------------------------------------------------------------------------------------;;

(defun sg:WriteTempDcl (content / f fn)
  (setq fn (vl-filename-mktemp "sheetgen" nil ".dcl"))
  (if (setq f (open fn "w"))
    (progn
      (write-line content f)
      (close f)
      fn
    )
  )
)

;;; Build the confirm dialog on the fly.  Sheets are spread over up to four
;;; columns of 20 so a 40 sheet run still fits on a normal screen.
(defun sg:ConfirmDcl (count / s i ncol rowsper idx ew)
  (setq ncol    (min 4 (max 1 (/ (+ count 19) 20)))
        rowsper (/ (+ count (1- ncol)) ncol)
        ew      (if (> ncol 1) 32 50)
        s       "sheetgen_confirm : dialog {\n  label = \"Confirm and Edit Sheet Names\";\n  : row {\n"
        idx     0)
  (repeat ncol
    (setq s (strcat s "    : column {\n"))
    (setq i 0)
    (while (and (< i rowsper) (< idx count))
      (setq s (strcat s
                      "      : edit_box { key = \"name" (itoa idx)
                      "\"; label = \"Sheet " (itoa (1+ idx))
                      ":\"; edit_width = " (itoa ew)
                      "; edit_limit = 255; }\n"))
      (setq i   (1+ i)
            idx (1+ idx))
    )
    (setq s (strcat s "    }\n"))
  )
  (strcat s
          "  }\n"
          "  spacer;\n"
          "  : row {\n"
          ;; not keyed "accept" - that is a reserved DCL key whose built in
          ;; action closes the dialog before the action expression can run
          "    : button { key = \"okbtn\"; label = \"OK\"; is_default = true; }\n"
          "    : button { key = \"back\"; label = \"< Back\"; is_cancel = true; }\n"
          "  }\n"
          "}\n")
)

(defun sg:GrabNames (/ i out)
  (setq i 0 out '())
  (while (< i *sg:count*)
    (setq out (cons (get_tile (strcat "name" (itoa i))) out)
          i   (1+ i))
  )
  (setq *sg:names* (reverse out))
)

;;; Returns 1 (OK, edited names left in *sg:names*) or 0 (Back / failed).
(defun sg:ShowConfirm (names / dcl-id file res i)
  (setq res 0)
  (if (setq file (sg:WriteTempDcl (sg:ConfirmDcl (length names))))
    (progn
      (setq dcl-id (load_dialog file))
      (if (< dcl-id 0)
        (alert "SheetGen: the temporary DCL file could not be loaded.")
        (if (not (new_dialog "sheetgen_confirm" dcl-id))
          (progn
            (unload_dialog dcl-id)
            (alert "SheetGen: the confirmation dialog could not be opened.")
          )
          (progn
            (setq *sg:count* (length names)
                  *sg:names* nil
                  i          0)
            (foreach n names
              (set_tile (strcat "name" (itoa i)) n)
              (setq i (1+ i))
            )
            (action_tile "okbtn" "(progn (sg:Guard \"OK button\" 'sg:GrabNames) (done_dialog 1))")
            (action_tile "back"  "(done_dialog 0)")
            (setq res (start_dialog))
            (unload_dialog dcl-id)
            ;; if the names were never collected, treat it as Back
            (if (and (= res 1) (null *sg:names*)) (setq res 0))
          )
        )
      )
      (vl-file-delete file)
    )
    (alert "SheetGen: a temporary DCL file could not be written.")
  )
  res
)


;;;-----------------------------------------------------------------------------------------------;;
;;;                                     Main Setup Dialog                                          ;;
;;;-----------------------------------------------------------------------------------------------;;

(defun sg:InitPrefs ()
  (if (not *sg:p-cols*)     (setq *sg:p-cols*     "10"))
  (if (not *sg:p-rows*)     (setq *sg:p-rows*     "4"))
  (if (not *sg:p-lastrow*)  (setq *sg:p-lastrow*  ""))
  (if (not *sg:p-hspace*)   (setq *sg:p-hspace*   "500"))
  (if (not *sg:p-vspace*)   (setq *sg:p-vspace*   "300"))
  (if (not *sg:p-parts*)    (setq *sg:p-parts*    ""))
  (if (not *sg:p-qty*)      (setq *sg:p-qty*      ""))
  (if (not *sg:p-sd*)       (setq *sg:p-sd*       ""))
  (if (not *sg:p-mode*)     (setq *sg:p-mode*     "mode_new"))
  (if (not *sg:p-reuse*)    (setq *sg:p-reuse*    "1"))
)

;;; Sensible source layout / grid positions for MODE.
;;; -> (source-name source-position first-new-position) as strings
(defun sg:SrcDefaults (mode layouts / st idx src)
  (setq st (sg:StateRead))
  (if (= mode "mode_add")
    (progn
      (setq src (sg:Last layouts))
      ;; The state record knows the grid position of the last sheet generated.
      ;; A tab the user copied on to the end still shows that same position.
      (setq idx (sg:Int (nth 3 st) (length layouts)))
      (if (< idx 1) (setq idx (length layouts)))
      (list src (itoa idx) (itoa (1+ idx)))
    )
    (progn
      (setq src (if (sg:LayoutExists "1") "1" (car layouts)))
      (list src "1" "1")
    )
  )
)

;;; "= row 3, col 8" for a grid position, so an abstract sheet number can be
;;; checked against where the part actually sits in model space.
(defun sg:PosHint (pos cols / n)
  (if (or (< pos 1) (< cols 1))
    ""
    (progn
      (setq n (1- pos))
      (strcat "= row " (itoa (1+ (/ n cols))) ", col " (itoa (1+ (rem n cols))))
    )
  )
)

;;; set_tile that tolerates the tile not being there, for cosmetic tiles that
;;; an older DCL will not have.
(defun sg:SetTile (key val)
  (vl-catch-all-apply 'set_tile (list key (sg:Str val "")))
)

;;; Refresh the two position readouts and the run summary.
(defun sg:UpdateHints (/ c rows lastrow total first last)
  (setq c       (sg:Int (get_tile "cols") 0)
        rows    (sg:Int (get_tile "rows") 0)
        lastrow (if (= (sg:Trim (get_tile "lastrow")) "") c (sg:Int (get_tile "lastrow") 0))
        first   (sg:Int (get_tile "firstpos") 0))
  (sg:SetTile "srchint"   (sg:PosHint (sg:Int (get_tile "srcpos") 0) c))
  (sg:SetTile "firsthint" (sg:PosHint first c))
  (if (and (> c 0) (> rows 0) (> lastrow 0) (> first 0))
    (progn
      (setq total (+ (* (- rows 1) c) lastrow)
            last  (+ first total -1))
      (sg:SetTile "summary"
                  (strcat "This run: " (itoa total) " sheet(s), position "
                          (itoa first) " (" (sg:PosHint first c) ") through "
                          (itoa last)  " (" (sg:PosHint last  c) ")"))
    )
    (sg:SetTile "summary" "")
  )
)

;;; T when KEY is a real tile in the dialog that is currently open.
;;; A missing key either errors or hands back a non string, both of which mean
;;; the loaded DCL is not the one this file expects.
(defun sg:TileExists (key / r)
  (setq r (vl-catch-all-apply 'get_tile (list key)))
  (if (vl-catch-all-error-p r)
    nil
    (eq (type r) 'STR)
  )
)

;;; The page 1 dialog, carried inside this file.
;;; It used to live in a separate sheetgen.dcl resolved through the support
;;; file search path, which meant the two files could drift apart or a stale
;;; copy in another folder could be picked up instead. Keeping it here makes
;;; a version mismatch impossible.
(defun sg:Page1Dcl (/ s)
  (setq s "")
  (foreach ln (list
    "sheetgen_page1 : dialog {"
    "  label = \"SheetGen - Layout Grid Setup\";"
    ""
    "  : text {"
    "    label = \"Check that your drawing properties are entered and your title block is set.\";"
    "  }"
    "  : text {"
    "    label = \"Part numbers longer than 4 characters may need the DIESEL expression adjusted.\";"
    "  }"
    ""
    "  spacer;"
    ""
    "  : boxed_column {"
    "    label = \"Mode\";"
    ""
    "    : radio_column {"
    "      key = \"modegrp\";"
    "      : radio_button {"
    "        key = \"mode_new\";"
    "        label = \"Create a new series of sheets\";"
    "      }"
    "      : radio_button {"
    "        key = \"mode_add\";"
    "        label = \"Add more sheets after the tabs already in this drawing\";"
    "      }"
    "    }"
    "  }"
    ""
    "  : boxed_column {"
    "    label = \"Source Layout\";"
    ""
    "    : text {"
    "      label = \"To add sheets: copy your last tab, then run this in Add mode with that tab as the source.\";"
    "    }"
    "    : text {"
    "      label = \"Positions count straight through the grid - row 1 left to right, then row 2, and so on.\";"
    "    }"
    "    : text {"
    "      label = \"They are filled in from where the last run stopped, so normally you can leave them alone.\";"
    "    }"
    ""
    "    : row {"
    "      : column {"
    "        : text { label = \"Copy sheets from layout:\"; }"
    "        : popup_list { key = \"srclayout\"; width = 30; }"
    "      }"
    "      : column {"
    "        : text { label = \"Grid position it shows now:\"; }"
    "        : edit_box { key = \"srcpos\"; width = 6; edit_limit = 6; }"
    "        : text { key = \"srchint\"; label = \"= row 00, col 00\"; width = 18; }"
    "      }"
    "      : column {"
    "        : text { label = \"Grid position of first new sheet:\"; }"
    "        : edit_box { key = \"firstpos\"; width = 6; edit_limit = 6; }"
    "        : text { key = \"firsthint\"; label = \"= row 00, col 00\"; width = 18; }"
    "      }"
    "    }"
    ""
    "    : toggle {"
    "      key = \"reuse\";"
    "      label = \"Reuse the source layout as the first sheet of this batch\";"
    "    }"
    "  }"
    ""
    "  : boxed_column {"
    "    label = \"Layout Grid Configuration\";"
    ""
    "    : row {"
    "      : column {"
    "        : text { label = \"Columns:\"; }"
    "        : edit_box { key = \"cols\"; width = 5; edit_limit = 6; }"
    "      }"
    "      : column {"
    "        : text { label = \"Rows:\"; }"
    "        : edit_box { key = \"rows\"; width = 5; edit_limit = 6; }"
    "      }"
    "      : column {"
    "        : text { label = \"Layouts in Last Row:\"; }"
    "        : edit_box { key = \"lastrow\"; width = 5; edit_limit = 6; }"
    "      }"
    "    }"
    ""
    "    : row {"
    "      : column {"
    "        : text { label = \"Horizontal Spacing:\"; }"
    "        : edit_box { key = \"hspace\"; width = 10; edit_limit = 20; }"
    "      }"
    "      : column {"
    "        : text { label = \"Vertical Spacing:\"; }"
    "        : edit_box { key = \"vspace\"; width = 10; edit_limit = 20; }"
    "      }"
    "    }"
    "  }"
    ""
    "  : boxed_column {"
    "    label = \"Part Number and SD Information\";"
    ""
    "    : text {"
    "      label = \"Separate values with spaces. Enter a single value to run a sequence.\";"
    "    }"
    ""
    "    : column {"
    "      : text { label = \"Part Numbers (first part number ONLY for sequential):\"; }"
    "      : edit_box { key = \"partnums\"; width = 80; edit_limit = 255; }"
    "    }"
    ""
    "    : column {"
    "      : text { label = \"Quantities (single value applies to every sheet):\"; }"
    "      : edit_box { key = \"quantities\"; width = 80; edit_limit = 255; }"
    "    }"
    ""
    "    : column {"
    "      : text { label = \"SD Numbers (first SD number ONLY for sequential):\"; }"
    "      : edit_box { key = \"sdnums\"; width = 80; edit_limit = 255; }"
    "    }"
    "  }"
    ""
    "  spacer;"
    ""
    "  : text {"
    "    key = \"summary\";"
    "    label = \"This run: 000 sheet(s), position 000 (= row 00, col 00) through 000 (= row 00, col 00)\";"
    "    width = 80;"
    "  }"
    ""
    "  : errtile { width = 70; }"
    ""
    "  : row {"
    "    : button { key = \"next\"; label = \"Next >\"; is_default = true; }"
    "    : button { key = \"cancel\"; label = \"Cancel\"; is_cancel = true; }"
    "  }"
    "}"
  )
    (setq s (strcat s ln "\n"))
  )
  s
)

;;; Tiles this version cannot run without, that are absent from the open dialog.
;;; The three readout tiles are deliberately not listed: they are cosmetic, so a
;;; slightly older DCL loses the readout rather than refusing to open at all.
(defun sg:MissingTiles (/ out)
  (foreach k '("mode_new" "mode_add" "srclayout" "srcpos" "firstpos" "reuse"
               "cols" "rows" "lastrow" "hspace" "vspace"
               "partnums" "quantities" "sdnums")
    (if (not (sg:TileExists k)) (setq out (cons k out)))
  )
  (reverse out)
)

;;; Which radio button is lit.  Read from the buttons themselves rather than the
;;; radio_column, whose value is not reliably set by set_tile on a child button.
(defun sg:ReadMode ()
  (cond
    ((= (sg:Str (get_tile "mode_add") "") "1") "mode_add")
    ((= (sg:Str (get_tile "mode_new") "") "1") "mode_new")
    (T (sg:Str *sg:p-mode* "mode_new"))
  )
)

;;; Push the defaults for the currently selected mode into the source tiles.
;;; When appending, the columns and spacing come back from the drawing as well:
;;; they describe the physical model space grid, which does not change between
;;; batches, and a value retyped even slightly differently would mis-pan every
;;; sheet in the new batch.
(defun sg:ApplyModeDefaults (/ d p st)
  (setq d (sg:SrcDefaults (sg:ReadMode) *sg:layouts*)
        p (sg:Index (car d) *sg:layouts*))
  (set_tile "srclayout" (itoa (sg:Int p 0)))
  (set_tile "srcpos"    (sg:Str (cadr d) "1"))
  (set_tile "firstpos"  (sg:Str (caddr d) "1"))

  (if (and (= (sg:ReadMode) "mode_add") (setq st (sg:StateRead)))
    (progn
      (if (and (car st) (> (sg:Int (car st) 0) 0))
        (set_tile "cols" (itoa (sg:Int (car st) 10)))
      )
      (if (cadr st)  (set_tile "hspace" (rtos (cadr st)  2 4)))
      (if (caddr st) (set_tile "vspace" (rtos (caddr st) 2 4)))
    )
  )
  (sg:UpdateHints)
)

;;; Read and check every tile.  Only closes the dialog when the input is usable.
(defun sg:Page1Accept (/ mode cols rows lastrow hspace vspace total
                         src srcpos firstpos reuse msg dflt)
  (setq *sg:p-mode*    (sg:ReadMode)
        *sg:p-cols*    (sg:Str (get_tile "cols")       "")
        *sg:p-rows*    (sg:Str (get_tile "rows")       "")
        *sg:p-lastrow* (sg:Str (get_tile "lastrow")    "")
        *sg:p-hspace*  (sg:Str (get_tile "hspace")     "")
        *sg:p-vspace*  (sg:Str (get_tile "vspace")     "")
        *sg:p-parts*   (sg:Str (get_tile "partnums")   "")
        *sg:p-qty*     (sg:Str (get_tile "quantities") "")
        *sg:p-sd*      (sg:Str (get_tile "sdnums")     "")
        *sg:p-reuse*   (sg:Str (get_tile "reuse")      "0"))

  ;; What the two grid positions should be if the boxes are left empty
  (setq dflt (sg:SrcDefaults *sg:p-mode* *sg:layouts*))

  (setq mode     *sg:p-mode*
        cols     (sg:Int *sg:p-cols* 0)
        rows     (sg:Int *sg:p-rows* 0)
        lastrow  (if (= (sg:Trim *sg:p-lastrow*) "") cols (sg:Int *sg:p-lastrow* 0))
        hspace   (atof *sg:p-hspace*)
        vspace   (atof *sg:p-vspace*)
        src      (nth (sg:Int (get_tile "srclayout") 0) *sg:layouts*)
        srcpos   (if (= (sg:Trim (get_tile "srcpos")) "")
                   (sg:Int (cadr dflt) 1)
                   (sg:Int (get_tile "srcpos") 0))
        firstpos (if (= (sg:Trim (get_tile "firstpos")) "")
                   (sg:Int (caddr dflt) 1)
                   (sg:Int (get_tile "firstpos") 0))
        reuse    (= *sg:p-reuse* "1"))

  (if (null src) (setq src (sg:Str (car dflt) nil)))

  (setq total (+ (* (- rows 1) cols) lastrow))

  (setq msg
        (cond
          ((< cols 1)                 "Columns must be a whole number of 1 or more.")
          ((< rows 1)                 "Rows must be a whole number of 1 or more.")
          ((or (< lastrow 1) (> lastrow cols))
                                      "Layouts in Last Row must be between 1 and the column count.")
          ((< total 1)                "That grid works out to no sheets at all.")
          ((<= hspace 0.0)            "Horizontal Spacing must be greater than zero.")
          ((<= vspace 0.0)            "Vertical Spacing must be greater than zero.")
          ((null src)                 "Pick the layout to copy from.")
          ((< srcpos 1)               "Grid position of the source layout must be 1 or more.")
          ((< firstpos 1)             "Grid position of the first new sheet must be 1 or more.")
          ((= (sg:Trim *sg:p-parts*) "") "Enter at least one part number.")
          ((= (sg:Trim *sg:p-qty*) "")   "Enter at least one quantity.")
          ((= (sg:Trim *sg:p-sd*) "")    "Enter at least one SD number.")
          (T nil)
        ))

  (if msg
    ;; show it in the error tile and in a message box, so a rejected Next can
    ;; never look like a dead button
    (progn
      (vl-catch-all-apply 'set_tile (list "error" msg))
      (alert (strcat "SheetGen cannot continue:\n\n" msg "\n\n"
                     "What the dialog read:\n"
                     "  Mode: "            (sg:Str mode "?") "\n"
                     "  Source layout: "   (sg:Str src "<none>") "\n"
                     "  Source position: " (itoa srcpos)
                     "   First new: "      (itoa firstpos) "\n"
                     "  Columns: "         (itoa cols)
                     "   Rows: "           (itoa rows)
                     "   Last row: "       (itoa lastrow) "\n"
                     "  Spacing H/V: "     (rtos hspace 2 4) " / " (rtos vspace 2 4) "\n"
                     "  Total sheets: "    (itoa total) "\n"
                     "  Parts: ["          *sg:p-parts* "]\n"
                     "  Quantities: ["     *sg:p-qty*   "]\n"
                     "  SD numbers: ["     *sg:p-sd*    "]"))
    )
    (progn
      (setq *sg:cfg*
            (list (cons 'mode     mode)
                  (cons 'src      src)
                  (cons 'srcpos   srcpos)
                  (cons 'firstpos firstpos)
                  (cons 'reuse    reuse)
                  (cons 'cols     cols)
                  (cons 'rows     rows)
                  (cons 'lastrow  lastrow)
                  (cons 'hspace   hspace)
                  (cons 'vspace   vspace)
                  (cons 'total    total)))
      (done_dialog 1)
    )
  )
)

;;; Show the setup dialog.  Returns the config list, or nil if cancelled.
(defun sg:Page1 (start-mode / dcl-id res p miss tmp out)
  (sg:InitPrefs)
  (setq *sg:layouts* (sg:LayoutNames))

  ;; Write the built in dialog out and load that.  Only if the temporary file
  ;; cannot be produced do we fall back to a sheetgen.dcl on the search path.
  (setq dcl-id -1)
  (if (setq tmp (sg:WriteTempDcl (sg:Page1Dcl)))
    (progn
      (setq dcl-id (load_dialog tmp))
      (setq *sg:dclsource* (if (>= dcl-id 0) "built in" nil))
    )
  )
  (if (< dcl-id 0)
    (progn
      (setq dcl-id (load_dialog "sheetgen.dcl"))
      (setq *sg:dclsource* (sg:Str (findfile "sheetgen.dcl") "sheetgen.dcl"))
    )
  )
  ;; NOTE: tmp is deleted at the very end, not here - the file has to survive
  ;; until unload_dialog is done with it.
  (setq out
  (cond
    ((null *sg:layouts*)
     (if (>= dcl-id 0) (unload_dialog dcl-id))
     (alert "SheetGen: this drawing has no paper space layouts to copy from.")
     nil)
    ((< dcl-id 0)
     (alert "SheetGen: the setup dialog could not be created.")
     nil)
    ((not (new_dialog "sheetgen_page1" dcl-id))
     (unload_dialog dcl-id)
     (alert "SheetGen: the sheetgen_page1 dialog could not be opened.")
     nil)
    ;; An older sheetgen.dcl earlier on the support path opens here with tiles
    ;; missing, which is otherwise very confusing to diagnose
    ((setq miss (sg:MissingTiles))
     (unload_dialog dcl-id)
     (setq p "")
     (foreach k miss (setq p (strcat p (if (= p "") "" ", ") k)))
     (alert (strcat
       "SheetGen.lsp version " sheetgenversion " loaded an out of date sheetgen.dcl.\n\n"
       "THIS is the file AutoCAD actually opened:\n\n  "
       (sg:Str (findfile "sheetgen.dcl") "(not found on the search path)") "\n\n"
       "Missing from it:\n  " p "\n\n"
       "SheetGen.lsp and sheetgen.dcl are a matched pair and must be updated\n"
       "together. Overwrite the file named above with the new sheetgen.dcl.\n\n"
       "If that is not where you put the new one, AutoCAD found this copy first:\n"
       "it searches the folders in OPTIONS > Files > Support File Search Path in\n"
       "order and takes the first match, so delete the stale copy."))
     nil)
    (T
     (setq *sg:cfg* nil)
     (if start-mode (setq *sg:p-mode* start-mode))
     (if (not (member *sg:p-mode* '("mode_new" "mode_add")))
       (setq *sg:p-mode* "mode_new")
     )

     (start_list "srclayout")
     (foreach n *sg:layouts* (add_list n))
     (end_list)

     ;; set the button and the group, so get_tile works either way round
     (set_tile *sg:p-mode* "1")
     (vl-catch-all-apply 'set_tile (list "modegrp" *sg:p-mode*))
     (set_tile "cols"       *sg:p-cols*)
     (set_tile "rows"       *sg:p-rows*)
     (set_tile "lastrow"    *sg:p-lastrow*)
     (set_tile "hspace"     *sg:p-hspace*)
     (set_tile "vspace"     *sg:p-vspace*)
     (set_tile "partnums"   *sg:p-parts*)
     (set_tile "quantities" *sg:p-qty*)
     (set_tile "sdnums"     *sg:p-sd*)
     (set_tile "reuse"      *sg:p-reuse*)

     (if (and *sg:p-src* (setq p (sg:Index *sg:p-src* *sg:layouts*)))
       (progn
         (set_tile "srclayout" (itoa p))
         (set_tile "srcpos"    (sg:Str *sg:p-srcpos*   "1"))
         (set_tile "firstpos"  (sg:Str *sg:p-firstpos* "1"))
       )
       (sg:ApplyModeDefaults)
     )

     (sg:UpdateHints)

     (action_tile "modegrp" "(sg:Guard \"mode change\" 'sg:ApplyModeDefaults)")
     (foreach k '("cols" "rows" "lastrow" "srcpos" "firstpos")
       (action_tile k "(sg:Guard \"grid readout\" 'sg:UpdateHints)")
     )
     (action_tile "next"    "(sg:Guard \"Next button\" 'sg:Page1Accept)")
     (action_tile "cancel"  "(done_dialog 0)")

     (setq res (start_dialog))
     (unload_dialog dcl-id)

     ;; res can come back as 1 without *sg:cfg* ever being filled in, so trust
     ;; the config rather than the return code
     (if (and (= res 1) *sg:cfg*)
       (progn
         ;; remember the source choice for the next run in this session
         (setq *sg:p-src*      (sg:Str (cdr (assoc 'src *sg:cfg*)) nil)
               *sg:p-srcpos*   (itoa (sg:Int (cdr (assoc 'srcpos   *sg:cfg*)) 1))
               *sg:p-firstpos* (itoa (sg:Int (cdr (assoc 'firstpos *sg:cfg*)) 1)))
         *sg:cfg*
       )
       nil
     )
    )
  ))
  (if tmp (vl-file-delete tmp))
  out
)


;;;-----------------------------------------------------------------------------------------------;;
;;;                                       Generation                                               ;;
;;;-----------------------------------------------------------------------------------------------;;

(defun sg:err (msg)
  (if (and msg (not (member msg '("Function cancelled" "quit / exit abort" "console break"))))
    (princ (strcat "\n** SheetGen Error: " msg))
  )
  (sg:ClearCmd)
  ;; never leave viewports unlocked because the run died part way through
  (sg:RelockAll)
  (if *sg:undo-open*
    (progn (command "_.UNDO" "_End") (setq *sg:undo-open* nil))
  )
  (if *sg:oldecho* (setvar "CMDECHO" *sg:oldecho*))
  (setq *error* *sg:olderr*)
  (princ)
)

;;; Create the sheets.  Existing layouts are never renamed, re-panned or moved -
;;; the only layout touched outside the new batch is the source, and only when
;;; "reuse" is on (which is what turns the tab you added on the end into sheet 1).
(defun sg:Generate (cfg names / cols hspace vspace src srcpos firstpos reuse
                                cur curpos i n tmp target ok made relocked basectr basepos
                                pending p r lastactual)
  (setq cols     (sg:Int (cdr (assoc 'cols     cfg)) 1)
        hspace   (cdr (assoc 'hspace cfg))
        vspace   (cdr (assoc 'vspace cfg))
        src      (sg:Str (cdr (assoc 'src      cfg)) nil)
        srcpos   (sg:Int (cdr (assoc 'srcpos   cfg)) 1)
        firstpos (sg:Int (cdr (assoc 'firstpos cfg)) 1)
        reuse    (cdr (assoc 'reuse cfg))
        n        (length names)
        ok       T
        made     0)

  (if (or (null src) (< cols 1) (null names))
    (progn
      (alert "SheetGen: the setup came back incomplete. Please run the command again.")
      (setq n 0 ok nil)
    )
  )

  (setq *sg:olderr*  *error*
        *error*      sg:err
        *sg:oldecho* (getvar "CMDECHO")
        *sg:unlocked* nil)
  (setvar "CMDECHO" 0)
  (command "_.UNDO" "_Begin")
  (setq *sg:undo-open* T)

  (setq cur    src
        curpos srcpos
        i      0)

  ;; Where the source layout is looking now.  Every sheet is positioned from
  ;; this one reference, so a sheet that fails cannot pull the others off.
  (setq basectr (sg:ViewCentre src)
        basepos srcpos)
  (if basectr
    (princ (strcat "\nSource \"" src "\" is centred on "
                   (rtos (car basectr) 2 2) "," (rtos (cadr basectr) 2 2)
                   " at grid position " (itoa basepos) "."))
    (princ (strcat "\n** No viewport found in \"" src
                   "\" - sheets will be created but not positioned."))
  )

  ;; Optionally turn the source layout itself into the first sheet of the batch
  (if reuse
    (progn
      (if (/= curpos firstpos)
        (sg:TrySetView cur firstpos basepos basectr cols hspace vspace)
      )
      (setq pending (list (cons src (nth 0 names)))
            made    1
            i       1)
      (princ (strcat "\nSheet 1 of " (itoa n) ": reusing \"" src "\""))
    )
  )

  ;; Create and position every sheet first, under a plain temporary name.
  ;; Renaming is left until the end: the copy chain has to make each layout
  ;; current, and a final sheet name carries padding spaces that do not always
  ;; survive a round trip through CTAB and the LAYOUT command.
  (while (and ok (< i n))
    (setq target (+ firstpos i)
          tmp    (sg:UniqueName "SG_TMP"))
    (if (sg:CopyLayoutTo cur tmp)
      (progn
        (sg:TrySetView tmp target basepos basectr cols hspace vspace)
        (setq pending (append pending (list (cons tmp (nth i names))))
              cur     tmp
              made    (1+ made))
        (princ (strcat "\nSheet " (itoa (1+ i)) " of " (itoa n) " created"))
      )
      (progn
        (setq ok nil)
        (princ (strcat "\n** Could not copy layout \"" cur "\"."))
      )
    )
    (setq i (1+ i))
  )

  ;; Now that nothing else depends on the layout names, apply them
  (setq curpos (+ firstpos (1- (max made 1))))
  (foreach p pending
    (setq r (sg:Rename (car p) (cdr p)))
    (cond
      ((null r)
       (setq ok nil)
       (princ (strcat "\n** Could not rename \"" (car p) "\" to \"" (cdr p)
                      "\"\n   AutoCAD said: " (sg:Str *sg:renameerr* "no reason given"))))
      ((/= r (cdr p))
       (setq lastactual r)
       (princ (strcat "\n** Stored as \"" r "\" rather than \"" (cdr p) "\"")))
      (T (setq lastactual r))
    )
  )

  ;; Lock again exactly the viewports that were unlocked to allow panning
  (setq relocked (sg:RelockAll))

  (if (and (> made 0) lastactual)
    (progn
      (setvar "CTAB" lastactual)
      (sg:StateSave cols hspace vspace curpos lastactual)
      (command "_.REGENALL")
    )
  )

  (command "_.UNDO" "_End")
  (setq *sg:undo-open* nil)
  (setvar "CMDECHO" *sg:oldecho*)
  (setq *error* *sg:olderr*)

  (if ok
    (alert (strcat "Automatic Layout Generation Complete\n\n"
                   (itoa made) " sheet(s) ready, ending at grid position "
                   (itoa curpos) "."
                   (if (> relocked 0)
                     (strcat "\n\n" (itoa relocked)
                             " viewport(s) were display locked. They were released to"
                             "\nallow panning and have been locked again.")
                     "")))
    (alert (strcat "SheetGen stopped after " (itoa made)
                   " sheet(s).\nSee the command line for details.\n"
                   "UNDO once to roll the whole run back."))
  )
  ok
)


;;;-----------------------------------------------------------------------------------------------;;
;;;                                       Commands                                                 ;;
;;;-----------------------------------------------------------------------------------------------;;

(defun sg:Main (start-mode / cfg total parts qty sds names existing problems idx done cr)
  (setq done nil)
  (while (not done)
    (setq *sg:step* "setup dialog")
    (if (setq cfg (sg:Page1 start-mode))
      (progn
        (setq *sg:step* "building sheet names")
        (setq start-mode nil                       ; only force the mode on the first pass
              total      (sg:Int (cdr (assoc 'total cfg)) 0)
              parts      (sg:SeqList *sg:p-parts* total)
              qty        (sg:QtyList *sg:p-qty*   total)
              sds        (sg:SeqList *sg:p-sd*    total))

        (if (not (and (= (length parts) total)
                      (= (length qty)   total)
                      (= (length sds)   total)))
          (alert (strcat "Input count mismatch.\n\n"
                         "Expected " (itoa total) " entries.\n"
                         "Parts: "       (itoa (length parts)) "\n"
                         "Quantities: "  (itoa (length qty))   "\n"
                         "SD Numbers: "  (itoa (length sds))   "\n\n"
                         "Give one value to run a sequence, or one value per sheet."))
          (progn
            ;; Build the sheet names
            (setq names '() idx 0)
            (while (< idx total)
              (setq names (cons (strcat (sg:PadRight (nth idx parts) 4) "_"
                                        (sg:PadRight (nth idx qty)   3) "_"
                                        (nth idx sds))
                                names)
                    idx   (1+ idx))
            )
            (setq names (reverse names))

            ;; Names already in use.  The source is excluded when it is being
            ;; recycled as sheet 1, because it is about to be renamed anyway.
            (setq existing
                  (mapcar 'strcase
                          (if (cdr (assoc 'reuse cfg))
                            (vl-remove (cdr (assoc 'src cfg)) (sg:LayoutNames))
                            (sg:LayoutNames))))

            (setq *sg:step* "confirm names dialog")
            (setq cr (sg:ShowConfirm names))
            (if (= cr 1)
              (progn
                (setq *sg:step* "checking sheet names")
                (setq names *sg:names*)
                (if (setq problems (sg:ValidateNames names existing))
                  (sg:ReportProblems problems)
                  (progn
                    (setq *sg:step* "creating layouts")
                    (sg:Generate cfg names)
                    (setq done T)
                  )
                )
              )
            )
          )
        )
      )
      (setq done T)                                ; cancelled
    )
  )
  (princ)
)

;;; Single entry point.  Anything that escapes is reported with the step it
;;; happened in, rather than as a bare AutoCAD message.
(defun sg:Run (mode / r)
  (setq *sg:step* "startup")
  (setq r (vl-catch-all-apply 'sg:Main (list mode)))
  (if (vl-catch-all-error-p r)
    (progn
      (sg:ClearCmd)
      (if *sg:undo-open*
        (progn (command "_.UNDO" "_End") (setq *sg:undo-open* nil))
      )
      (if *sg:oldecho* (setvar "CMDECHO" *sg:oldecho*))
      (princ (strcat "\n** SheetGen error during " (sg:Str *sg:step* "?") ": "
                     (vl-catch-all-error-message r)))
    )
  )
  (princ)
)

(defun c:SheetGen ()
  (sg:Run nil)
)

;;; Same dialog, opened straight into append mode
(defun c:SheetGenAdd ()
  (sg:Run "mode_add")
)

;;; Reports which files are actually in play.  The two must be a matched pair,
;;; and a stale sheetgen.dcl earlier on the search path is easy to miss.
(defun c:SheetGenWhere (/ f)
  (princ (strcat "\nSheetGen.lsp version " sheetgenversion))
  (princ (strcat "\nSetup dialog: "
                 (sg:Str *sg:dclsource* "not opened yet - run SheetGen once")))
  (princ "\nThe dialog is built into SheetGen.lsp, so there is no sheetgen.dcl to keep in step.")
  (if (setq f (findfile "sheetgen.dcl"))
    (princ (strcat "\nNote: a sheetgen.dcl still exists at " f
                   "\n      It is no longer used and can be deleted."))
  )
  (princ)
)

;;; Kept so old menu macros and scripts still work
(defun c:CopyLayout ()
  (princ "\nCopyLayout is now part of SheetGen - starting SheetGen.")
  (sg:Run nil)
)


;;-----------------------------------------------------------------------------------------------;;
;;                                       File Load Message                                       ;;
;;-----------------------------------------------------------------------------------------------;;

(princ
    (strcat
        "\n\U+25A0"
        "\n\U+25A0\U+25A0"
        "\n\U+25A0\U+25A0\U+25A0"
        "\n\U+25A0\U+25A0\U+25A0\U+25A0 SheetGen.lsp | Version "
        sheetgenversion " | Programmed by Brendan Fezuk | "
        (menucmd "m=$(edtime,0,yyyy)")
        "\n\U+25A0\U+25A0\U+25A0\U+25A0 Type \"SheetGen\" to start the layout generator"
        "\n\U+25A0\U+25A0\U+25A0\U+25A0 Type \"SheetGenAdd\" to add sheets after the existing tabs"
        "\n\U+25A0\U+25A0\U+25A0"
        "\n\U+25A0\U+25A0"
        "\n\U+25A0\n"
    )
)
(princ)

;;-----------------------------------------------------------------------------------------------;;
;;                                         End of File                                           ;;
;;-----------------------------------------------------------------------------------------------;;
