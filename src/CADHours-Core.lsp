;;; ============================================================
;;; CADHours-Core.lsp  -  Shared utilities for the CAD Hours Tracker
;;;
;;; Configuration, string / CSV helpers, calendar maths, path and
;;; file-system helpers, and the append-with-lock writer that every
;;; other module builds on.
;;;
;;; Nothing in this file talks to the drawing.  Load it first.
;;;
;;; Requirements : AutoCAD 2019+ (or AutoCAD LT 2024+) with Visual
;;;                LISP / ActiveX support
;;; ============================================================

(vl-load-com)

(setq *ch-version* "1.0.0")


;;; ---- debug / console -------------------------------------------

;;; Print MSG only when Debug=1 in the configuration
(defun ch:dbg (msg)
  (if (= 1 (ch:cfg-int "Debug" 0))
    (princ (strcat "\n[CADHours] " msg))
  )
  (princ)
)

;;; Print MSG unconditionally, prefixed so it is recognisable
(defun ch:say (msg)
  (princ (strcat "\n[CADHours] " msg))
  (princ)
)

;;; Call FN with ARGS, swallowing any error.  Returns the result, or
;;; nil when FN blew up.  Reactor callbacks must never throw, so every
;;; callback body goes through here.
(defun ch:safe (fn args / r)
  (setq r (vl-catch-all-apply fn args))
  (if (vl-catch-all-error-p r)
    (progn
      (ch:dbg (strcat "suppressed error: "
                      (vl-catch-all-error-message r)))
      nil
    )
    r
  )
)


;;; ---- string helpers --------------------------------------------

;;; Split STR on every occurrence of DELIM.  Always returns at least
;;; one element; empty fields are preserved.
(defun ch:split (str delim / pos res)
  (setq res '())
  (while (setq pos (vl-string-search delim str))
    (setq res (cons (substr str 1 pos) res)
          str (substr str (+ pos 1 (strlen delim))))
  )
  (reverse (cons str res))
)

;;; Join LST (a list of strings) with DELIM between elements
(defun ch:join (lst delim / s first x)
  (setq s "" first T)
  (foreach x lst
    (setq s     (if first x (strcat s delim x))
          first nil)
  )
  s
)

;;; Replace every occurrence of OLD with NEW inside STR
(defun ch:replace (str old new / pos res)
  (setq res "")
  (while (setq pos (vl-string-search old str))
    (setq res (strcat res (substr str 1 pos) new)
          str (substr str (+ pos 1 (strlen old))))
  )
  (strcat res str)
)

;;; Strip leading and trailing spaces, tabs, CR and LF
(defun ch:trim (str)
  (if str (vl-string-trim " \t\r\n" str) "")
)

;;; Coerce anything to a string
(defun ch:str (x)
  (cond
    ((null x)             "")
    ((= (type x) 'STR)    x)
    ((= (type x) 'INT)    (itoa x))
    ((= (type x) 'REAL)   (ch:fmt2 x))
    (T                    (vl-princ-to-string x))
  )
)

;;; Format VAL with two decimal places.
;;;
;;; The stock rtos honours LUNITS / DIMZIN and would happily write
;;; "1'-6\"" or strip a trailing zero into a log file, and resetting
;;; those sysvars from a reactor would dirty the drawing.  So the
;;; formatting is done with integer arithmetic instead and never
;;; touches the drawing at all.
(defun ch:fmt2 (val / neg n i f)
  (setq neg (< val 0.0)
        val (abs (float val)))
  (if (> val 2.0e7) (setq val 2.0e7))     ; keep the scaled value in range
  (setq n (fix (+ 0.5 (* 100.0 val)))
        i (/ n 100)
        f (rem n 100))
  (strcat (if neg "-" "") (itoa i) "." (ch:pad2 f))
)

;;; Whole seconds as an integer string
(defun ch:isecs (secs)
  (itoa (fix (+ 0.5 (abs (float secs)))))
)

;;; Zero-padded integers
(defun ch:pad2 (n)
  (if (< n 10) (strcat "0" (itoa n)) (itoa n))
)

(defun ch:pad4 (n)
  (cond
    ((< n 10)   (strcat "000" (itoa n)))
    ((< n 100)  (strcat "00"  (itoa n)))
    ((< n 1000) (strcat "0"   (itoa n)))
    (T          (itoa n))
  )
)

;;; Left-pad / right-pad to WIDTH, for command-line report tables
(defun ch:lpad (s width)
  (while (< (strlen s) width) (setq s (strcat " " s)))
  s
)

(defun ch:rpad (s width)
  (while (< (strlen s) width) (setq s (strcat s " ")))
  (if (> (strlen s) width) (substr s 1 width) s)
)

;;; Expand %ENVVAR% references inside STR
(defun ch:expand-env (str / pos1 pos2 name val res)
  (setq res "")
  (while (setq pos1 (vl-string-search "%" str))
    (setq res (strcat res (substr str 1 pos1))
          str (substr str (+ pos1 2)))
    (if (setq pos2 (vl-string-search "%" str))
      (progn
        (setq name (substr str 1 pos2)
              val  (getenv name))
        (setq res (strcat res (if val val "")))
        (setq str (substr str (+ pos2 2)))
      )
      (setq res (strcat res "%"))
    )
  )
  (strcat res str)
)


;;; ---- CSV ---------------------------------------------------------

;;; Quote one CSV field.  Every field is quoted, so paths containing
;;; commas, quotes or semicolons survive the round trip.
(defun ch:csv-field (x)
  (strcat "\"" (ch:replace (ch:str x) "\"" "\"\"") "\"")
)

;;; Build one CSV record from a list of values
(defun ch:csv-line (lst)
  (ch:join (mapcar 'ch:csv-field lst) ",")
)

;;; Parse one CSV record into a list of strings.  Understands quoted
;;; fields and doubled quotes.  Embedded newlines are not supported -
;;; this tool never writes them.
(defun ch:csv-parse (line / i n c fld fields inq)
  (setq n      (strlen line)
        i      1
        fld    ""
        fields '()
        inq    nil)
  (while (<= i n)
    (setq c (substr line i 1))
    (cond
      (inq
        (cond
          ((= c "\"")
            (if (= (substr line (1+ i) 1) "\"")
              (setq fld (strcat fld "\"") i (1+ i))
              (setq inq nil)
            )
          )
          (T (setq fld (strcat fld c)))
        )
      )
      ((= c "\"") (setq inq T))
      ((= c ",")  (setq fields (cons fld fields) fld ""))
      (T          (setq fld (strcat fld c)))
    )
    (setq i (1+ i))
  )
  (reverse (cons fld fields))
)

;;; Field N (0-based) of a parsed row, "" when the row is short
(defun ch:fld (row n / v)
  (setq v (nth n row))
  (if v v "")
)


;;; ---- calendar ----------------------------------------------------

;;; Current time as a Julian real.  Only ever used for differences -
;;; the integer part is not interpreted as a calendar date.
(defun ch:now () (getvar "DATE"))

;;; Elapsed seconds between two ch:now values
(defun ch:secs (d0 d1) (* 86400.0 (- d1 d0)))

;;; Break CDATE (YYYYMMDD.HHMMSS) into (year month day hour min sec).
;;; CDATE is used for anything calendar-shaped because its meaning is
;;; unambiguous; DATE is used only for elapsed time.
(defun ch:parts ( / c ymd y m d frac hms hh mi ss)
  (setq c    (getvar "CDATE")
        ymd  (fix c)
        y    (/ ymd 10000)
        m    (/ (- ymd (* y 10000)) 100)
        d    (- ymd (* y 10000) (* m 100))
        frac (- c ymd)
        hms  (fix (+ 0.5 (* frac 1000000.0)))
        hh   (/ hms 10000)
        mi   (/ (- hms (* hh 10000)) 100)
        ss   (- hms (* hh 10000) (* mi 100)))
  ;; guard against the rounding above tipping a field over its range
  (if (> ss 59) (setq ss 59))
  (if (> mi 59) (setq mi 59))
  (if (> hh 23) (setq hh 23))
  (list y m d hh mi ss)
)

;;; "YYYY-MM-DD HH:MM:SS" for a (ch:parts) list
(defun ch:stamp (p)
  (strcat (ch:date-str p) " " (ch:time-str p))
)

(defun ch:date-str (p)
  (strcat (ch:pad4 (nth 0 p)) "-" (ch:pad2 (nth 1 p)) "-" (ch:pad2 (nth 2 p)))
)

(defun ch:time-str (p)
  (strcat (ch:pad2 (nth 3 p)) ":" (ch:pad2 (nth 4 p)) ":" (ch:pad2 (nth 5 p)))
)

;;; "YYYY-MM" for a (ch:parts) list
(defun ch:month-str (p)
  (strcat (ch:pad4 (nth 0 p)) "-" (ch:pad2 (nth 1 p)))
)

;;; Compact stamp with no separators, for file names
(defun ch:compact-str (p)
  (strcat (ch:pad4 (nth 0 p)) (ch:pad2 (nth 1 p)) (ch:pad2 (nth 2 p))
          (ch:pad2 (nth 3 p)) (ch:pad2 (nth 4 p)) (ch:pad2 (nth 5 p)))
)

;;; Julian Day Number for a Gregorian Y/M/D (Fliegel-Van Flandern)
(defun ch:jdn (y m d / a y2 m2)
  (setq a  (/ (- 14 m) 12)
        y2 (- (+ y 4800) a)
        m2 (- (+ m (* 12 a)) 3))
  (+ d
     (/ (+ (* 153 m2) 2) 5)
     (* 365 y2)
     (/ y2 4)
     (- (/ y2 100))
     (/ y2 400)
     -32045)
)

;;; Inverse of ch:jdn - returns (year month day)
(defun ch:jdn->ymd (j / a b c e f m2 day mon yr)
  (setq a  (+ j 32044)
        b  (/ (+ (* 4 a) 3) 146097)
        c  (- a (/ (* 146097 b) 4))
        e  (/ (+ (* 4 c) 3) 1461)
        f  (- c (/ (* 1461 e) 4))
        m2 (/ (+ (* 5 f) 2) 153))
  (setq day (+ (- f (/ (+ (* 153 m2) 2) 5)) 1)
        mon (+ m2 3 (* -12 (/ m2 10)))
        yr  (+ (* 100 b) e -4800 (/ m2 10)))
  (list yr mon day)
)

;;; ISO-8601 week label, e.g. "2026-W34".  The ISO week belongs to the
;;; year that owns its Thursday, which is why this is not simply
;;; "day-of-year / 7".
(defun ch:iso-week (y m d / j dow thu ty jan1 n wk)
  (setq j    (ch:jdn y m d)
        dow  (rem j 7)              ; 0 = Monday .. 6 = Sunday
        thu  (+ (- j dow) 3)        ; Thursday of this ISO week
        ty   (car (ch:jdn->ymd thu))
        jan1 (ch:jdn ty 1 1)
        n    (- thu jan1)           ; 0-based day-of-year of that Thursday
        wk   (1+ (/ n 7)))
  (strcat (ch:pad4 ty) "-W" (ch:pad2 wk))
)

;;; ISO week label for a (ch:parts) list
(defun ch:week-str (p)
  (ch:iso-week (nth 0 p) (nth 1 p) (nth 2 p))
)

;;; Monday of the ISO week containing Y/M/D, as "YYYY-MM-DD"
(defun ch:week-start (y m d / j dow ymd)
  (setq j   (ch:jdn y m d)
        dow (rem j 7)
        ymd (ch:jdn->ymd (- j dow)))
  (strcat (ch:pad4 (nth 0 ymd)) "-" (ch:pad2 (nth 1 ymd)) "-" (ch:pad2 (nth 2 ymd)))
)

;;; Seconds since an arbitrary epoch, derived from the local wall
;;; clock.  Used to compare "now" against a file's timestamp.
(defun ch:now-secs ( / p)
  (setq p (ch:parts))
  (+ (* 86400.0 (ch:jdn (nth 0 p) (nth 1 p) (nth 2 p)))
     (* 3600.0 (nth 3 p))
     (* 60.0   (nth 4 p))
     (nth 5 p))
)

;;; Same scale as ch:now-secs, for a vl-file-systime list
;;; (year month day-of-week day hours minutes seconds)
(defun ch:systime-secs (st)
  (if (and st (>= (length st) 7))
    (+ (* 86400.0 (ch:jdn (nth 0 st) (nth 1 st) (nth 3 st)))
       (* 3600.0 (nth 4 st))
       (* 60.0   (nth 5 st))
       (nth 6 st))
  )
)

;;; Age of PATH in seconds, or nil when it cannot be determined
(defun ch:file-age (path / st)
  (if (setq st (ch:systime-secs (vl-file-systime path)))
    (- (ch:now-secs) st)
  )
)

;;; Seconds as "H:MM" - the form that goes on a timesheet
(defun ch:hhmm (secs / h m)
  (setq secs (fix (+ 0.5 secs))
        h    (/ secs 3600)
        m    (/ (rem secs 3600) 60))
  (strcat (itoa h) ":" (ch:pad2 m))
)

;;; Seconds as decimal hours, two places
(defun ch:dec-hours (secs)
  (ch:fmt2 (/ (float secs) 3600.0))
)


;;; ---- paths and directories ---------------------------------------

;;; Normalise separators to backslashes and drop any trailing one
(defun ch:norm-path (p)
  (if (null p)
    ""
    (progn
      (setq p (ch:replace p "/" "\\"))
      (while (and (> (strlen p) 3)
                  (= (substr p (strlen p) 1) "\\"))
        (setq p (substr p 1 (1- (strlen p))))
      )
      p
    )
  )
)

;;; Join two path fragments with a single backslash
(defun ch:path+ (base leaf)
  (setq base (ch:norm-path base)
        leaf (ch:norm-path leaf))
  (cond
    ((= base "") leaf)
    ((= leaf "") base)
    (T (strcat base "\\" leaf))
  )
)

;;; Does DIR exist?
(defun ch:dir-p (dir)
  (and dir (/= dir "") (vl-file-directory-p (ch:norm-path dir)))
)

;;; Create DIR and every missing parent.  Handles both drive-letter
;;; paths and UNC paths - for UNC the \\server\share prefix is taken
;;; as the (already existing) base and never created.
(defun ch:mkpath (path / p parts base cur s)
  (setq p (ch:norm-path path))
  (cond
    ((= p "") nil)
    ((ch:dir-p p) T)
    (T
      (if (= (substr p 1 2) "\\\\")
        (progn
          (setq parts (ch:split (substr p 3) "\\"))
          (if (< (length parts) 2)
            (setq base nil)
            (setq base  (strcat "\\\\" (car parts) "\\" (cadr parts))
                  parts (cddr parts))
          )
        )
        (progn
          (setq parts (ch:split p "\\")
                base  (car parts)
                parts (cdr parts))
        )
      )
      (if base
        (progn
          (setq cur base)
          (foreach s parts
            (if (/= s "")
              (progn
                (setq cur (strcat cur "\\" s))
                (if (not (ch:dir-p cur)) (vl-mkdir cur))
              )
            )
          )
        )
      )
      (ch:dir-p p)
    )
  )
)

;;; Strip characters that are illegal in a Windows file name
(defun ch:safe-name (s / bad out i c)
  (setq bad "\\/:*?\"<>|" out "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (setq out (strcat out (if (vl-string-search c bad) "_" c)))
    (setq i (1+ i))
  )
  (if (= out "") "unknown" out)
)


;;; ---- file locking ------------------------------------------------
;;;
;;; vl-mkdir is the only atomic test-and-set AutoLISP offers: it
;;; returns nil when the directory already exists.  That makes a
;;; directory a usable mutex across machines on a network share.

;;; Busy-wait for SECS.  There is no sleep in AutoLISP, and calling
;;; the DELAY command from inside a reactor is unsafe, so this spins.
;;; Only ever used for waits measured in tens of milliseconds.
(defun ch:spin (secs / t0)
  (setq t0 (ch:now))
  (while (< (ch:secs t0 (ch:now)) secs))
)

;;; Remove a lock left behind by a crashed session
(defun ch:break-lock (lk / age)
  (setq age (ch:file-age (ch:path+ lk "owner.txt")))
  (if (or (null age) (> age 60.0))
    (progn
      (vl-file-delete (ch:path+ lk "owner.txt"))
      (vl-catch-all-apply 'vl-rmdir (list lk))
    )
  )
)

;;; Try to take the lock guarding PATH.  Returns the lock directory on
;;; success, nil when it could not be taken within about two seconds.
(defun ch:lock (path / lk tries got f)
  (setq lk    (strcat (ch:norm-path path) ".lock")
        tries 0
        got   nil)
  (while (and (not got) (< tries 40))
    (if (vl-mkdir lk)
      (setq got T)
      (progn
        (if (= tries 8) (ch:break-lock lk))
        (ch:spin 0.05)
        (setq tries (1+ tries))
      )
    )
  )
  (if got
    (progn
      (if (setq f (open (ch:path+ lk "owner.txt") "w"))
        (progn (write-line (ch:str (getvar "LOGINNAME")) f) (close f))
      )
      lk
    )
  )
)

;;; Release a lock taken by ch:lock
(defun ch:unlock (lk)
  (if lk
    (progn
      (vl-file-delete (ch:path+ lk "owner.txt"))
      (vl-catch-all-apply 'vl-rmdir (list lk))
    )
  )
)


;;; ---- file writing ------------------------------------------------

;;; Append LINE to PATH, creating the file with HEADER when it does
;;; not exist yet.  Returns T on success.
(defun ch:append-line (path line header / f new)
  (ch:mkpath (vl-filename-directory path))
  (setq new (null (findfile path)))
  (if (setq f (open path "a"))
    (progn
      (if (and new header) (write-line header f))
      (write-line line f)
      (close f)
      T
    )
  )
)

;;; Append under a lock.  When the lock cannot be taken - another
;;; AutoCAD instance is mid-write - the line goes to a per-session
;;; sidecar file instead.  Reports glob the whole folder, so a sidecar
;;; is read back exactly like the main file and no data is lost.
(defun ch:append-locked (path line header sidecar-tag / lk ok side)
  (if (setq lk (ch:lock path))
    (progn
      (setq ok (ch:append-line path line header))
      (ch:unlock lk)
      ok
    )
    (progn
      (setq side (strcat (vl-filename-directory path) "\\"
                         (vl-filename-base path)
                         "~" (ch:safe-name sidecar-tag)
                         (vl-filename-extension path)))
      (ch:append-line side line header)
    )
  )
)

;;; Overwrite PATH with the single record LINE (plus HEADER).  Used
;;; for the live/ heartbeat files, which have exactly one writer.
(defun ch:write-line-file (path line header / f)
  (ch:mkpath (vl-filename-directory path))
  (if (setq f (open path "w"))
    (progn
      (if header (write-line header f))
      (write-line line f)
      (close f)
      T
    )
  )
)

;;; Read PATH into a list of lines, nil when it cannot be opened
(defun ch:read-lines (path / f line res)
  (if (and path (findfile path) (setq f (open path "r")))
    (progn
      (while (setq line (read-line f))
        (setq res (cons line res))
      )
      (close f)
      (reverse res)
    )
  )
)

;;; All files under DIR matching PATTERN, as full paths
(defun ch:files (dir pattern)
  (if (ch:dir-p dir)
    (mapcar '(lambda (f) (ch:path+ dir f))
            (vl-directory-files (ch:norm-path dir) pattern 1))
  )
)

;;; Immediate sub-directory names of DIR, "." and ".." removed
(defun ch:subdirs (dir)
  (if (ch:dir-p dir)
    (vl-remove-if
      '(lambda (d) (member d '("." "..")))
      (vl-directory-files (ch:norm-path dir) nil -1))
  )
)


;;; ---- configuration -----------------------------------------------
;;;
;;; Settings come from a plain INI-style file so a CAD manager can
;;; change them without touching LISP.  Search order:
;;;   1. %CADHOURS_CONFIG%
;;;   2. <install folder>\cadhours.ini
;;;   3. %APPDATA%\CADHours\cadhours.ini
;;;   4. the built-in defaults below

(defun ch:cfg-defaults ()
  (list
    (cons "LogRoot"            "%LOCALAPPDATA%\\CADHours\\data")
    (cons "LocalSpool"         "%LOCALAPPDATA%\\CADHours\\spool")
    (cons "IdleSeconds"        "300")
    (cons "IdleCreditSeconds"  "300")
    (cons "HeartbeatSeconds"   "60")
    (cons "MinSessionSeconds"  "10")
    (cons "PromptOnOpen"       "1")
    (cons "RequireJobNumber"   "1")
    (cons "RepromptSeconds"    "120")
    (cons "JobPattern"         "*")
    (cons "JobPatternHint"     "")
    (cons "JobFromPath"        "1")
    (cons "DefaultJob"         "")
    (cons "RememberJobInDwg"   "1")
    (cons "WriteEventLog"      "1")
    (cons "TrackObjectEdits"   "1")
    (cons "TrackSysVarChanges" "0")
    (cons "ObjectEventStride"  "20")
    (cons "AutoRecover"        "1")
    (cons "StaleLiveHours"     "8")
    (cons "MruCount"           "12")
    (cons "Debug"              "0")
  )
)

;;; Where the tracker is installed.  Needed for the HTML dashboard
;;; template and the sample config.
(defun ch:home ( / cand hit c)
  (if (and *ch-home* (ch:dir-p *ch-home*))
    *ch-home*
    (progn
      (setq cand
        (list
          (getenv "CADHOURS_HOME")
          (vl-registry-read "HKEY_CURRENT_USER\\Software\\CADHours" "Home")
          (ch:expand-env "%APPDATA%\\Autodesk\\ApplicationPlugins\\CADHours.bundle\\Contents")
          (ch:expand-env "%PROGRAMDATA%\\Autodesk\\ApplicationPlugins\\CADHours.bundle\\Contents")
          (ch:expand-env "%PROGRAMFILES%\\CADHours")
        ))
      (foreach c cand
        (if (and (null hit) c (ch:dir-p c)) (setq hit (ch:norm-path c)))
      )
      (if (null hit)
        (if (setq hit (findfile "CADHours-Core.lsp"))
          (setq hit (vl-filename-directory hit))
        )
      )
      (setq *ch-home* hit)
    )
  )
)

;;; Locate the configuration file
(defun ch:cfg-file ( / cand hit c)
  (setq cand
    (list
      (getenv "CADHOURS_CONFIG")
      (if (ch:home) (ch:path+ (ch:home) "cadhours.ini"))
      (ch:expand-env "%APPDATA%\\CADHours\\cadhours.ini")
    ))
  (foreach c cand
    (if (and (null hit) c (findfile c)) (setq hit c))
  )
  hit
)

;;; Parse the INI file into an association list.  Blank lines and
;;; lines starting with ; or # are ignored; everything else is
;;; Key=Value.  Section headers are ignored - keys are global.
(defun ch:cfg-load ( / file lines pos k v alist ln)
  (setq alist (ch:cfg-defaults))
  (if (setq file (ch:cfg-file))
    (progn
      (setq lines (ch:read-lines file))
      (foreach ln lines
        (setq ln (ch:trim ln))
        (if (and (/= ln "")
                 (/= (substr ln 1 1) ";")
                 (/= (substr ln 1 1) "#")
                 (/= (substr ln 1 1) "[")
                 (setq pos (vl-string-search "=" ln)))
          (progn
            (setq k (ch:trim (substr ln 1 pos))
                  v (ch:trim (substr ln (+ pos 2))))
            (setq alist (cons (cons k v)
                              (vl-remove-if
                                '(lambda (p) (= (strcase (car p)) (strcase k)))
                                alist)))
          )
        )
      )
    )
  )
  (setq *ch-config* alist)
)

;;; Raw string value for KEY
(defun ch:cfg (key / hit)
  (if (null *ch-config*) (ch:cfg-load))
  (setq hit (assoc key *ch-config*))
  (if (null hit)
    (setq hit (car (vl-remove-if-not
                     '(lambda (p) (= (strcase (car p)) (strcase key)))
                     *ch-config*)))
  )
  (if hit (cdr hit) "")
)

;;; Value for KEY with %ENVVAR% expanded and separators normalised
(defun ch:cfg-path (key)
  (ch:norm-path (ch:expand-env (ch:cfg key)))
)

;;; Integer value for KEY, DEF when unset or unparseable
(defun ch:cfg-int (key def / v n)
  (setq v (ch:trim (ch:cfg key)))
  (if (= v "")
    def
    (progn
      (setq n (vl-catch-all-apply 'atoi (list v)))
      (if (vl-catch-all-error-p n) def n)
    )
  )
)

;;; Real value for KEY, DEF when unset
(defun ch:cfg-real (key def / v n)
  (setq v (ch:trim (ch:cfg key)))
  (if (= v "")
    def
    (progn
      (setq n (vl-catch-all-apply 'atof (list v)))
      (if (vl-catch-all-error-p n) def (float n))
    )
  )
)

;;; True when KEY is 1 / yes / true / on
(defun ch:cfg-bool (key def / v)
  (setq v (strcase (ch:trim (ch:cfg key))))
  (cond
    ((= v "") def)
    ((member v '("1" "Y" "YES" "T" "TRUE" "ON")) T)
    (T nil)
  )
)


;;; ---- log locations -----------------------------------------------

;;; The configured share, or the local spool folder when the share is
;;; not reachable right now.  A laptop that is off the network keeps
;;; logging locally and the rows are pushed up on the next start.
(defun ch:root ( / root)
  (setq root (ch:cfg-path "LogRoot"))
  (cond
    ((and (/= root "") (or (ch:dir-p root) (ch:mkpath root))) root)
    (T
      (setq root (ch:cfg-path "LocalSpool"))
      (ch:mkpath root)
      root
    )
  )
)

;;; True when we are currently writing to the fallback spool
(defun ch:spooling-p ( / root)
  (setq root (ch:cfg-path "LogRoot"))
  (not (and (/= root "") (ch:dir-p root)))
)

(defun ch:sessions-dir (root month) (ch:path+ (ch:path+ root "sessions") month))
(defun ch:live-dir     (root)       (ch:path+ root "live"))
(defun ch:events-dir   (root day)   (ch:path+ (ch:path+ root "events") day))
(defun ch:reports-dir  (root)       (ch:path+ root "reports"))


;;; ---- identity ----------------------------------------------------

;;; Windows user name, falling back to the AutoCAD login name
(defun ch:user ( / u)
  (setq u (getenv "USERNAME"))
  (if (or (null u) (= u "")) (setq u (getvar "LOGINNAME")))
  (if (or (null u) (= u "")) (setq u "unknown"))
  (strcase u)
)

;;; Machine name
(defun ch:machine ( / m)
  (setq m (getenv "COMPUTERNAME"))
  (if (or (null m) (= m "")) (setq m "unknown"))
  (strcase m)
)


;;; ---- most-recently-used job numbers -------------------------------
;;;
;;; Kept in HKCU so a user's recent jobs follow them between drawings
;;; without any file being written.

(defun ch:mru-key () "HKEY_CURRENT_USER\\Software\\CADHours")

(defun ch:mru-get ( / raw)
  (setq raw (vl-registry-read (ch:mru-key) "RecentJobs"))
  (if (and raw (/= raw ""))
    (vl-remove "" (ch:split raw "|"))
    '()
  )
)

(defun ch:mru-add (job / lst max)
  (setq job (ch:trim job))
  (if (/= job "")
    (progn
      (setq max (ch:cfg-int "MruCount" 12))
      (setq lst (cons job (vl-remove-if
                            '(lambda (j) (= (strcase j) (strcase job)))
                            (ch:mru-get))))
      (if (> (length lst) max)
        (setq lst (reverse (cdr (reverse lst))))
      )
      (vl-registry-write (ch:mru-key) "RecentJobs" (ch:join lst "|"))
      (vl-registry-write (ch:mru-key) "LastJob" job)
    )
  )
  job
)

(defun ch:mru-last ( / v)
  (setq v (vl-registry-read (ch:mru-key) "LastJob"))
  (if v v "")
)


(princ)
;;; ============================================================ EOF
