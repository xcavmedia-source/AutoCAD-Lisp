;;; ============================================================
;;; CADHours-Report.lsp  -  Searching and reporting
;;;
;;; The session files under <LogRoot>\sessions are the database.  This
;;; module reads them back, filters by user / job / drawing / date,
;;; and rolls the result up by job, user, day, week, month or drawing.
;;;
;;; Commands
;;;   CHREPORT   guided report - pick a grouping and a filter
;;;   CHTODAY    my hours today, by job
;;;   CHWEEK     my hours this week, by job and by day
;;;   CHJOBHOURS everything booked to one job, by user and by month
;;;   CHFIND     list the individual sessions that match a filter
;;;   CHEXPORT   write the matching sessions out as a CSV
;;;
;;; In-progress sessions (the live\ folder) are included and shown as
;;; RUNNING, so today's numbers include work still on the clock.
;;; ============================================================

(vl-load-com)


;;; ---- record layout ---------------------------------------------------

(setq *ch-col-sid*    0  *ch-col-status* 1  *ch-col-user*   2
      *ch-col-mach*   3  *ch-col-job*    4  *ch-col-task*   5
      *ch-col-dname*  6  *ch-col-dpath*  7  *ch-col-start*  8
      *ch-col-end*    9  *ch-col-date*  10  *ch-col-week*  11
      *ch-col-month* 12  *ch-col-asec*  13  *ch-col-isec*  14
      *ch-col-wsec*  15  *ch-col-ahrs*  16  *ch-col-saves* 17
      *ch-col-cmds*  18  *ch-col-notes* 19  *ch-col-ver*   20)

(defun ch:r-user  (r) (ch:fld r *ch-col-user*))
(defun ch:r-job   (r) (ch:fld r *ch-col-job*))
(defun ch:r-task  (r) (ch:fld r *ch-col-task*))
(defun ch:r-date  (r) (ch:fld r *ch-col-date*))
(defun ch:r-week  (r) (ch:fld r *ch-col-week*))
(defun ch:r-month (r) (ch:fld r *ch-col-month*))
(defun ch:r-dwg   (r) (ch:fld r *ch-col-dname*))
(defun ch:r-path  (r) (ch:fld r *ch-col-dpath*))
(defun ch:r-stat  (r) (ch:fld r *ch-col-status*))

;;; Billed seconds as a number
(defun ch:r-secs (r / v)
  (setq v (ch:fld r *ch-col-asec*))
  (if (= v "") 0.0 (float (atoi v)))
)

(defun ch:r-idle (r / v)
  (setq v (ch:fld r *ch-col-isec*))
  (if (= v "") 0.0 (float (atoi v)))
)


;;; ---- dates -------------------------------------------------------------

(defun ch:today ()
  (ch:date-str (ch:parts))
)

;;; "YYYY-MM-DD" shifted by N days
(defun ch:date-shift (d n / ymd)
  (setq ymd (ch:jdn->ymd
              (+ n (ch:jdn (atoi (substr d 1 4))
                           (atoi (substr d 6 2))
                           (atoi (substr d 9 2))))))
  (strcat (ch:pad4 (nth 0 ymd)) "-" (ch:pad2 (nth 1 ymd)) "-" (ch:pad2 (nth 2 ymd)))
)

(defun ch:monday-of-today ( / p)
  (setq p (ch:parts))
  (ch:week-start (nth 0 p) (nth 1 p) (nth 2 p))
)

(defun ch:first-of-month ( / p)
  (setq p (ch:parts))
  (strcat (ch:pad4 (nth 0 p)) "-" (ch:pad2 (nth 1 p)) "-01")
)

;;; Turn a typed answer into a (from to) pair.  Accepts a date, a
;;; keyword, or a plain number meaning "that many days back".
(defun ch:parse-range (s / u n)
  (setq s (ch:trim s)
        u (strcase s))
  (cond
    ((= s "")            (list (ch:today) (ch:today)))
    ((= u "TODAY")       (list (ch:today) (ch:today)))
    ((= u "YESTERDAY")   (list (ch:date-shift (ch:today) -1)
                               (ch:date-shift (ch:today) -1)))
    ((= u "WEEK")        (list (ch:monday-of-today) (ch:today)))
    ((= u "LASTWEEK")    (list (ch:date-shift (ch:monday-of-today) -7)
                               (ch:date-shift (ch:monday-of-today) -1)))
    ((= u "MONTH")       (list (ch:first-of-month) (ch:today)))
    ((= u "ALL")         (list "0000-01-01" "9999-12-31"))
    ((and (= (strlen s) 10) (= (substr s 5 1) "-")) (list s s))
    ((and (= (strlen s) 7) (= (substr s 5 1) "-"))
      (list (strcat s "-01") (strcat s "-31")))
    ((setq n (atoi s))
      (if (> n 0)
        (list (ch:date-shift (ch:today) (- (1- n))) (ch:today))
        (list (ch:today) (ch:today))))
    (T (list (ch:today) (ch:today)))
  )
)


;;; ---- loading -------------------------------------------------------------

;;; Every month folder that could hold rows between FROM and TO
(defun ch:months-in-range (root from to / dir res mf mt m)
  (setq dir (ch:path+ root "sessions")
        mf  (substr from 1 7)
        mt  (substr to   1 7))
  (foreach m (ch:subdirs dir)
    (if (and (>= m mf) (<= m mt)) (setq res (cons m res)))
  )
  (reverse res)
)

;;; Read every session row between FROM and TO.  Header lines are
;;; dropped by testing the first field for the literal "session_id".
(defun ch:load-rows (from to / root rows cols f ln m)
  (setq root (ch:root) rows '())
  (foreach m (ch:months-in-range root from to)
    (foreach f (ch:files (ch:path+ (ch:path+ root "sessions") m) "*.csv")
      (foreach ln (ch:read-lines f)
        (setq cols (ch:csv-parse ln))
        (if (and (> (length cols) *ch-col-ahrs*)
                 (/= (ch:fld cols 0) "session_id")
                 (>= (ch:r-date cols) from)
                 (<= (ch:r-date cols) to))
          (setq rows (cons cols rows))
        )
      )
    )
  )
  ;; sessions still on the clock
  (foreach f (ch:files (ch:live-dir root) "*.csv")
    (foreach ln (ch:read-lines f)
      (setq cols (ch:csv-parse ln))
      (if (and (> (length cols) *ch-col-ahrs*)
               (/= (ch:fld cols 0) "session_id")
               (>= (ch:r-date cols) from)
               (<= (ch:r-date cols) to))
        (setq rows (cons cols rows))
      )
    )
  )
  (reverse rows)
)

;;; Case-insensitive wildcard test; "" and "*" match everything
(defun ch:match (value pattern)
  (setq pattern (ch:trim pattern))
  (cond
    ((or (= pattern "") (= pattern "*")) T)
    (T (if (wcmatch (strcase (ch:str value)) (strcase pattern)) T nil))
  )
)

;;; Filter loaded rows.  SPEC is an alist of "user" / "job" / "dwg".
(defun ch:filter (rows spec / u j d)
  (setq u (cdr (assoc "user" spec))
        j (cdr (assoc "job"  spec))
        d (cdr (assoc "dwg"  spec)))
  (vl-remove-if-not
    '(lambda (r)
       (and (ch:match (ch:r-user r) (if u u "*"))
            (ch:match (ch:r-job  r) (if j j "*"))
            (or (ch:match (ch:r-dwg r) (if d d "*"))
                (ch:match (ch:r-path r) (if d d "*")))))
    rows)
)


;;; ---- grouping ---------------------------------------------------------

;;; Roll ROWS up by KEYFN.  Returns a list of (key secs idle sessions)
;;; sorted with the biggest total first.
(defun ch:group (rows keyfn / tbl k hit res r)
  (setq tbl '())
  (foreach r rows
    (setq k (apply keyfn (list r)))
    (if (= k "") (setq k "(blank)"))
    (if (setq hit (assoc k tbl))
      (setq tbl (subst (list k
                             (+ (cadr hit)   (ch:r-secs r))
                             (+ (caddr hit)  (ch:r-idle r))
                             (1+ (cadddr hit)))
                       hit tbl))
      (setq tbl (cons (list k (ch:r-secs r) (ch:r-idle r) 1) tbl))
    )
  )
  ;; vl-sort discards elements its comparison function calls equal, so
  ;; ties are broken on the key to keep the ordering total.  Without
  ;; this, two jobs with identical totals would collapse into one row.
  (vl-sort tbl
    '(lambda (a b)
       (cond
         ((> (cadr a) (cadr b)) T)
         ((< (cadr a) (cadr b)) nil)
         (T (< (car a) (car b)))
       )))
)

;;; Same, but ordered by the key instead of by size - what you want
;;; for a day-by-day or week-by-week listing.
(defun ch:group-sorted (rows keyfn)
  (vl-sort (ch:group rows keyfn) '(lambda (a b) (< (car a) (car b))))
)

(defun ch:total-secs (rows / s r)
  (setq s 0.0)
  (foreach r rows (setq s (+ s (ch:r-secs r))))
  s
)


;;; ---- printing -----------------------------------------------------------

(defun ch:rule (n / s)
  (setq s "")
  (repeat n (setq s (strcat s "-")))
  s
)

(defun ch:hdr (title)
  (princ (strcat "\n\n" (ch:rule 68)
                 "\n " title
                 "\n" (ch:rule 68)))
  (princ)
)

;;; Print a grouped table.  LABEL heads the key column.
(defun ch:print-group (label groups / total g)
  (setq total 0.0)
  (foreach g groups (setq total (+ total (cadr g))))
  (if (<= total 0.0) (setq total 1.0))
  (princ (strcat "\n " (ch:rpad label 30)
                 (ch:lpad "Hours" 8)
                 (ch:lpad "Decimal" 10)
                 (ch:lpad "Idle" 8)
                 (ch:lpad "Sess" 6)
                 (ch:lpad "Share" 8)))
  (princ (strcat "\n " (ch:rule 68)))
  (foreach g groups
    (princ (strcat "\n " (ch:rpad (car g) 30)
                   (ch:lpad (ch:hhmm (cadr g)) 8)
                   (ch:lpad (ch:dec-hours (cadr g)) 10)
                   (ch:lpad (ch:hhmm (caddr g)) 8)
                   (ch:lpad (itoa (cadddr g)) 6)
                   (ch:lpad (strcat (ch:fmt2 (* 100.0 (/ (cadr g) total))) "%") 8)))
  )
  (princ (strcat "\n " (ch:rule 68)))
  (princ (strcat "\n " (ch:rpad "TOTAL" 30)
                 (ch:lpad (ch:hhmm total) 8)
                 (ch:lpad (ch:dec-hours total) 10)))
  (princ)
)

;;; Print the individual sessions behind a filter
(defun ch:print-rows (rows / r)
  (princ (strcat "\n " (ch:rpad "Date" 11) (ch:rpad "Start" 9)
                 (ch:rpad "User" 12) (ch:rpad "Job" 14)
                 (ch:rpad "Drawing" 20) (ch:lpad "Hours" 7)))
  (princ (strcat "\n " (ch:rule 74)))
  (foreach r rows
    (princ (strcat "\n " (ch:rpad (ch:r-date r) 11)
                   (ch:rpad (substr (ch:fld r *ch-col-start*) 12 5) 9)
                   (ch:rpad (ch:r-user r) 12)
                   (ch:rpad (ch:r-job r) 14)
                   (ch:rpad (ch:r-dwg r) 20)
                   (ch:lpad (ch:hhmm (ch:r-secs r)) 7)
                   (if (= (ch:r-stat r) "RUNNING") "  <running>" "")))
  )
  (princ (strcat "\n " (ch:rule 74)))
  (princ (strcat "\n " (itoa (length rows)) " session(s), "
                 (ch:hhmm (ch:total-secs rows)) " total ("
                 (ch:dec-hours (ch:total-secs rows)) " h)"))
  (princ)
)


;;; ---- exporting ----------------------------------------------------------

(defun ch:export-rows (rows tag / root file f r)
  (setq root (ch:root)
        file (ch:path+ (ch:reports-dir root)
                       (strcat "CADHours_" (ch:safe-name tag) "_"
                               (ch:compact-str (ch:parts)) ".csv")))
  (ch:mkpath (ch:reports-dir root))
  (if (setq f (open file "w"))
    (progn
      (write-line (ch:session-header) f)
      (foreach r rows (write-line (ch:csv-line r) f))
      (close f)
      file
    )
  )
)

(defun ch:export-group (groups label tag / root file f total g)
  (setq total 0.0)
  (foreach g groups (setq total (+ total (cadr g))))
  (setq root  (ch:root)
        file  (ch:path+ (ch:reports-dir root)
                        (strcat "CADHours_" (ch:safe-name tag) "_"
                                (ch:compact-str (ch:parts)) ".csv")))
  (ch:mkpath (ch:reports-dir root))
  (if (setq f (open file "w"))
    (progn
      (write-line (ch:csv-line (list label "hours_hhmm" "hours_decimal"
                                     "idle_hhmm" "sessions" "share_pct")) f)
      (foreach g groups
        (write-line (ch:csv-line
                      (list (car g) (ch:hhmm (cadr g)) (ch:dec-hours (cadr g))
                            (ch:hhmm (caddr g)) (itoa (cadddr g))
                            (ch:fmt2 (if (> total 0.0)
                                       (* 100.0 (/ (cadr g) total))
                                       0.0))))
                    f)
      )
      (close f)
      file
    )
  )
)


;;; ---- commands -------------------------------------------------------------

(defun c:CHTODAY ( / rows)
  (ch:cfg-load)
  (setq rows (ch:filter (ch:load-rows (ch:today) (ch:today))
                        (list (cons "user" (ch:user)))))
  (ch:hdr (strcat "My hours today - " (ch:today) " - " (ch:user)))
  (ch:print-group "Job" (ch:group rows 'ch:r-job))
  (ch:print-group "Drawing" (ch:group rows 'ch:r-dwg))
  (princ)
)

(defun c:CHWEEK ( / from rows)
  (ch:cfg-load)
  (setq from (ch:monday-of-today)
        rows (ch:filter (ch:load-rows from (ch:today))
                        (list (cons "user" (ch:user)))))
  (ch:hdr (strcat "My hours this week - " from " to " (ch:today)
                  " - " (ch:user)))
  (ch:print-group "Day" (ch:group-sorted rows 'ch:r-date))
  (ch:print-group "Job" (ch:group rows 'ch:r-job))
  (princ)
)

(defun c:CHJOBHOURS ( / *error* job rows)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
  (ch:cfg-load)
  (setq job (ch:trim (getstring T "\nJob number (wildcards allowed): ")))
  (if (= job "") (setq job "*"))
  (setq rows (ch:filter (ch:load-rows "0000-01-01" "9999-12-31")
                        (list (cons "job" job))))
  (ch:hdr (strcat "Job hours - " job))
  (if (null rows)
    (princ "\n No time recorded against that job yet.")
    (progn
      (ch:print-group "User"  (ch:group rows 'ch:r-user))
      (ch:print-group "Month" (ch:group-sorted rows 'ch:r-month))
      (ch:print-group "Drawing" (ch:group rows 'ch:r-dwg))
    )
  )
  (princ)
)

(defun c:CHFIND ( / *error* rng user job rows)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
  (ch:cfg-load)
  (setq rng  (ch:parse-range
               (getstring T "\nPeriod [date | TODAY | WEEK | MONTH | ALL | n days] <TODAY>: "))
        user (ch:trim (getstring T (strcat "\nUser <" (ch:user) ", * for all>: ")))
        job  (ch:trim (getstring T "\nJob number <*>: ")))
  (if (= user "") (setq user (ch:user)))
  (if (= job  "") (setq job  "*"))
  (setq rows (ch:filter (ch:load-rows (car rng) (cadr rng))
                        (list (cons "user" user) (cons "job" job))))
  (ch:hdr (strcat "Sessions " (car rng) " to " (cadr rng)
                  "  user=" user "  job=" job))
  (if (null rows)
    (princ "\n Nothing matched.")
    (ch:print-rows rows)
  )
  (princ)
)

;;; The guided report.  One prompt for the period, one for the filter,
;;; one for the grouping, and an offer to write it out.
(defun c:CHREPORT ( / *error* rng user job grp rows groups label file)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
  (ch:cfg-load)
  (setq rng (ch:parse-range
              (getstring T "\nPeriod [date | TODAY | WEEK | LASTWEEK | MONTH | ALL | n days] <TODAY>: ")))
  (setq user (ch:trim (getstring T (strcat "\nUser <* for everyone, . for " (ch:user) ">: "))))
  (if (= user "")  (setq user "*"))
  (if (= user ".") (setq user (ch:user)))
  (setq job (ch:trim (getstring T "\nJob number <*>: ")))
  (if (= job "") (setq job "*"))

  (initget "Job User Day Week Month Drawing Task")
  (setq grp (getkword "\nGroup by [Job/User/Day/Week/Month/Drawing/Task] <Job>: "))
  (if (null grp) (setq grp "Job"))

  (setq rows (ch:filter (ch:load-rows (car rng) (cadr rng))
                        (list (cons "user" user) (cons "job" job))))

  (setq groups
    (cond
      ((= grp "Job")     (setq label "Job")     (ch:group rows 'ch:r-job))
      ((= grp "User")    (setq label "User")    (ch:group rows 'ch:r-user))
      ((= grp "Day")     (setq label "Day")     (ch:group-sorted rows 'ch:r-date))
      ((= grp "Week")    (setq label "Week")    (ch:group-sorted rows 'ch:r-week))
      ((= grp "Month")   (setq label "Month")   (ch:group-sorted rows 'ch:r-month))
      ((= grp "Drawing") (setq label "Drawing") (ch:group rows 'ch:r-dwg))
      ((= grp "Task")    (setq label "Task")    (ch:group rows 'ch:r-task))
    ))

  (ch:hdr (strcat "CAD hours " (car rng) " to " (cadr rng)
                  "   user=" user "   job=" job "   by " (strcase label)))
  (if (null rows)
    (princ "\n Nothing matched that filter.")
    (progn
      (ch:print-group label groups)
      (initget "Yes No")
      (if (= "Yes" (getkword "\n\nWrite this report to a CSV? [Yes/No] <No>: "))
        (progn
          (setq file (ch:export-group groups label (strcat "by" label)))
          (if file
            (ch:say (strcat "Report written to " file))
            (ch:say "Could not write the report file."))
        )
      )
    )
  )
  (princ)
)

(defun c:CHEXPORT ( / *error* rng user job rows file)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
  (ch:cfg-load)
  (setq rng  (ch:parse-range
               (getstring T "\nPeriod [date | TODAY | WEEK | MONTH | ALL | n days] <TODAY>: "))
        user (ch:trim (getstring T "\nUser <*>: "))
        job  (ch:trim (getstring T "\nJob number <*>: ")))
  (if (= user "") (setq user "*"))
  (if (= job  "") (setq job  "*"))
  (setq rows (ch:filter (ch:load-rows (car rng) (cadr rng))
                        (list (cons "user" user) (cons "job" job))))
  (if (null rows)
    (ch:say "Nothing matched that filter - nothing exported.")
    (progn
      (setq file (ch:export-rows rows "sessions"))
      (if file
        (ch:say (strcat (itoa (length rows)) " session(s) written to " file))
        (ch:say "Could not write the export file."))
    )
  )
  (princ)
)


(princ)
;;; ============================================================ EOF
