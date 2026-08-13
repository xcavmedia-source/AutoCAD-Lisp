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
;;********************************************************************************************************************************;;

(vl-load-com)

(setq sheetgenversion "1.2")


;;;-----------------------------------------------------------------------------------------------;;
;;;                                      String Utilities                                          ;;
;;;-----------------------------------------------------------------------------------------------;;

;;; Coerce to an integer, falling back to DFLT.  Anything handed to itoa, nth,
;;; setvar or entmake goes through here so a nil can never reach them.
(defun sg:Int (val dflt)
  (cond
    ((= (type val) 'INT)  val)
    ((= (type val) 'REAL) (fix val))
    ((= (type val) 'STR)  (atoi val))
    (T                    dflt)
  )
)

(defun sg:Str (val dflt)
  (if (= (type val) 'STR) val dflt)
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
      ;; the confirm dialog has no error tile, so this is allowed to fail too
      (vl-catch-all-apply 'set_tile (list "error" (strcat label ": " r)))
      nil
    )
    r
  )
)

;;; Split STR on spaces and tabs, discarding empty tokens.
(defun sg:Split (str / res tok i ch len)
  (setq res '()
        tok ""
        i   1
        len (strlen str))
  (while (<= i len)
    (setq ch (substr str i 1))
    (if (or (= ch " ") (= ch "\t"))
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

;;; Rename through ActiveX - the LAYOUT command would eat the spaces in a padded name.
(defun sg:Rename (old new / r)
  (if (= old new)
    T
    (progn
      (setq r (vl-catch-all-apply
                '(lambda ()
                   (vla-put-Name (vla-Item (vla-get-Layouts (sg:Doc)) old) new))))
      (not (vl-catch-all-error-p r))
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

;;; Viewport ID of the first real (non paper space) viewport in layout LNAME, else nil.
(defun sg:VpId (lname / ss i e id)
  (setq id nil)
  (if (setq ss (ssget "_X" (list '(0 . "VIEWPORT") (cons 410 lname))))
    (progn
      (setq i 0)
      (while (and (not id) (< i (sslength ss)))
        (setq e (assoc 69 (entget (ssname ss i))))
        ;; a viewport with no ID, or the paper space viewport (1), is not usable
        (if (and e (/= 1 (cdr e)))
          (setq id (cdr e))
        )
        (setq i (1+ i))
      )
    )
  )
  id
)

;;; Model space offset of grid position IDX (1 based, filling left to right, top to bottom).
(defun sg:GridOffset (idx cols hspace vspace / n)
  (setq n (max 0 (1- idx)))
  (list (* (rem n cols) hspace)
        (* -1.0 (/ n cols) vspace)
        0.0)
)

;;; Pan the viewport of layout LNAME from grid position FROMIDX to TOIDX.
;;; Returns T if a viewport was found (or no pan was needed), nil otherwise.
(defun sg:PanView (lname fromidx toidx cols hspace vspace / vid d)
  (if (= fromidx toidx)
    T
    (progn
      ;; PAN displacement moves the drawing, so it is the negative of the view move
      (setq d (mapcar '-
                      (sg:GridOffset fromidx cols hspace vspace)
                      (sg:GridOffset toidx cols hspace vspace)))
      (if (setq vid (sg:VpId lname))
        (progn
          (command "_.MSPACE")
          (setvar "CVPORT" vid)
          (command "_.-PAN" "_non" '(0.0 0.0 0.0) "_non" d)
          (command "_.PSPACE")
          T
        )
        nil
      )
    )
  )
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
(defun sg:ApplyModeDefaults (/ d p)
  (setq d (sg:SrcDefaults (sg:ReadMode) *sg:layouts*)
        p (sg:Index (car d) *sg:layouts*))
  (set_tile "srclayout" (itoa (sg:Int p 0)))
  (set_tile "srcpos"    (sg:Str (cadr d) "1"))
  (set_tile "firstpos"  (sg:Str (caddr d) "1"))
)

;;; Read and check every tile.  Only closes the dialog when the input is usable.
(defun sg:Page1Accept (/ mode cols rows lastrow hspace vspace total
                         src srcpos firstpos reuse msg)
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

  (setq mode     *sg:p-mode*
        cols     (sg:Int *sg:p-cols* 0)
        rows     (sg:Int *sg:p-rows* 0)
        lastrow  (if (= (sg:Trim *sg:p-lastrow*) "") cols (sg:Int *sg:p-lastrow* 0))
        hspace   (atof *sg:p-hspace*)
        vspace   (atof *sg:p-vspace*)
        src      (nth (sg:Int (get_tile "srclayout") 0) *sg:layouts*)
        srcpos   (sg:Int (get_tile "srcpos")   0)
        firstpos (sg:Int (get_tile "firstpos") 0)
        reuse    (= *sg:p-reuse* "1"))

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
    (set_tile "error" msg)
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
(defun sg:Page1 (start-mode / dcl-id res p)
  (sg:InitPrefs)
  (setq *sg:layouts* (sg:LayoutNames))
  (cond
    ((null *sg:layouts*)
     (alert "SheetGen: this drawing has no paper space layouts to copy from.")
     nil)
    ((< (setq dcl-id (load_dialog "sheetgen.dcl")) 0)
     (alert "SheetGen: sheetgen.dcl was not found on the support file search path.")
     nil)
    ((not (new_dialog "sheetgen_page1" dcl-id))
     (unload_dialog dcl-id)
     (alert "SheetGen: the sheetgen_page1 dialog could not be opened.")
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

     (action_tile "modegrp" "(sg:Guard \"mode change\" 'sg:ApplyModeDefaults)")
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
  )
)


;;;-----------------------------------------------------------------------------------------------;;
;;;                                       Generation                                               ;;
;;;-----------------------------------------------------------------------------------------------;;

(defun sg:err (msg)
  (if (and msg (not (member msg '("Function cancelled" "quit / exit abort" "console break"))))
    (princ (strcat "\n** SheetGen Error: " msg))
  )
  (sg:ClearCmd)
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
                                cur curpos i n tmp target ok made)
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

  (setq *sg:olderr* *error*
        *error*     sg:err
        *sg:oldecho* (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_.UNDO" "_Begin")
  (setq *sg:undo-open* T)

  (setq cur    src
        curpos srcpos
        i      0)

  ;; Optionally turn the source layout itself into the first sheet of the batch
  (if reuse
    (progn
      (setvar "CTAB" cur)
      (if (not (sg:PanView cur curpos firstpos cols hspace vspace))
        (princ (strcat "\nNo viewport found in layout \"" cur "\" - view not panned."))
      )
      (if (sg:Rename cur (nth 0 names))
        (setq cur    (nth 0 names)
              curpos firstpos
              made   1
              i      1)
        (progn
          (setq ok nil)
          (princ (strcat "\nCould not rename layout \"" cur "\"."))
        )
      )
    )
  )

  ;; Every remaining sheet is a copy of the sheet before it, panned one grid step on
  (while (and ok (< i n))
    (setq target (+ firstpos i)
          tmp    (sg:UniqueName "SG_TMP"))
    (if (sg:CopyLayoutTo cur tmp)
      (progn
        (setvar "CTAB" tmp)
        (if (not (sg:PanView tmp curpos target cols hspace vspace))
          (princ (strcat "\nNo viewport found in the copy of \"" cur "\" - view not panned."))
        )
        (if (sg:Rename tmp (nth i names))
          (progn
            (setq cur    (nth i names)
                  curpos target
                  made   (1+ made))
            (princ (strcat "\nSheet " (itoa (1+ i)) " of " (itoa n) ": " (nth i names)))
          )
          (progn
            (setq ok nil)
            (princ (strcat "\nCould not rename layout \"" tmp "\" to \"" (nth i names) "\"."))
          )
        )
      )
      (progn
        (setq ok nil)
        (princ (strcat "\nCould not copy layout \"" cur "\"."))
      )
    )
    (setq i (1+ i))
  )

  (if (> made 0)
    (progn
      (setvar "CTAB" cur)
      (sg:StateSave cols hspace vspace curpos cur)
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
                   (itoa curpos) "."))
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
