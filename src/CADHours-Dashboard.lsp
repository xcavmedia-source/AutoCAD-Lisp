;;; ============================================================
;;; CADHours-Dashboard.lsp  -  Self-contained HTML dashboard
;;;
;;; CHDASH writes every matching session into a single HTML file and
;;; opens it in the default browser.  The page carries its own search,
;;; filtering and pivot logic, so a project manager gets a searchable
;;; view of the database without AutoCAD, without Excel and without
;;; anything being installed.
;;;
;;; The page is built from web\dashboard-template.html when that file
;;; ships alongside the LISP; a plain built-in table is used if it is
;;; missing, so the command always produces something usable.
;;; ============================================================

(vl-load-com)


;;; ---- JSON ---------------------------------------------------------------

(defun ch:json-str (s)
  (setq s (ch:str s))
  (setq s (ch:replace s "\\" "\\\\"))
  (setq s (ch:replace s "\"" "\\\""))
  (setq s (ch:replace s (chr 9) " "))
  (strcat "\"" s "\"")
)

;;; One session as a compact JSON array, matching ch:dash-columns
(defun ch:json-row (r)
  (strcat "["
    (ch:join
      (list
        (ch:json-str (ch:r-user r))
        (ch:json-str (ch:r-job r))
        (ch:json-str (ch:r-task r))
        (ch:json-str (ch:r-date r))
        (ch:json-str (ch:r-week r))
        (ch:json-str (ch:r-month r))
        (ch:json-str (ch:r-dwg r))
        (ch:json-str (ch:r-path r))
        (ch:json-str (ch:fld r *ch-col-start*))
        (ch:json-str (ch:fld r *ch-col-end*))
        (ch:isecs (ch:r-secs r))
        (ch:isecs (ch:r-idle r))
        (ch:json-str (ch:r-stat r))
        (ch:json-str (ch:fld r *ch-col-mach*))
        (ch:json-str (ch:fld r *ch-col-notes*))
      )
      ",")
    "]")
)

(defun ch:dash-columns ()
  "[\"user\",\"job\",\"task\",\"date\",\"week\",\"month\",\"dwg\",\"path\",\"start\",\"end\",\"active\",\"idle\",\"status\",\"machine\",\"notes\"]"
)

(defun ch:dash-marker () "/*__CADHOURS_DATA__*/")

;;; Write the injected data block into an already-open file handle
(defun ch:write-data (f rows from to / first r)
  (write-line "var CADHOURS = {" f)
  (write-line (strcat "  generated: " (ch:json-str (ch:stamp (ch:parts))) ",") f)
  (write-line (strcat "  from: "      (ch:json-str from) ",") f)
  (write-line (strcat "  to: "        (ch:json-str to) ",") f)
  (write-line (strcat "  root: "      (ch:json-str (ch:root)) ",") f)
  (write-line (strcat "  columns: "   (ch:dash-columns) ",") f)
  (write-line "  rows: [" f)
  (setq first T)
  (foreach r rows
    (write-line (strcat (if first "    " "   ,") (ch:json-row r)) f)
    (setq first nil)
  )
  (write-line "  ]" f)
  (write-line "};" f)
  (princ)
)

(defun ch:template-file ( / cand hit c)
  (setq cand
    (list
      (if (ch:home) (ch:path+ (ch:home) "dashboard-template.html"))
      (if (ch:home) (ch:path+ (ch:path+ (ch:home) "web") "dashboard-template.html"))
      (findfile "dashboard-template.html")
    ))
  (foreach c cand
    (if (and (null hit) c (findfile c)) (setq hit c))
  )
  hit
)


;;; ---- fallback page --------------------------------------------------------
;;;
;;; Used only when the template is missing.  Deliberately plain: one
;;; table, one search box, no dependencies.

(defun ch:fallback-html ()
  (list
    "<meta charset=\"utf-8\">"
    "<title>CAD Hours</title>"
    "<style>"
    "body{font:14px/1.5 system-ui,Segoe UI,sans-serif;margin:2rem;color:#111;background:#fff}"
    "h1{font-size:1.3rem} table{border-collapse:collapse;width:100%}"
    "th,td{border-bottom:1px solid #ddd;padding:.4rem .6rem;text-align:left}"
    "th{background:#f4f4f5} td.n{text-align:right;font-variant-numeric:tabular-nums}"
    "input{padding:.5rem;width:22rem;margin-bottom:1rem}"
    "</style>"
    "<h1>CAD Hours</h1>"
    "<input id=\"q\" placeholder=\"Filter by user, job, drawing, date...\">"
    "<table><thead><tr><th>Date</th><th>User</th><th>Job</th><th>Task</th>"
    "<th>Drawing</th><th class=\"n\">Hours</th><th>Status</th></tr></thead>"
    "<tbody id=\"b\"></tbody></table>"
    "<script>"
    (ch:dash-marker)
    "var C=CADHOURS.columns,R=CADHOURS.rows,i=function(n){return C.indexOf(n)};"
    "function hm(s){var h=Math.floor(s/3600),m=Math.round((s%3600)/60);return h+':'+String(m).padStart(2,'0')}"
    "function draw(){var q=document.getElementById('q').value.toLowerCase();"
    "var t=0,h='';R.forEach(function(r){if(q&&r.join(' ').toLowerCase().indexOf(q)<0)return;"
    "t+=r[i('active')];h+='<tr><td>'+r[i('date')]+'</td><td>'+r[i('user')]+'</td><td>'+r[i('job')]"
    "+'</td><td>'+r[i('task')]+'</td><td>'+r[i('dwg')]+'</td><td class=\"n\">'+hm(r[i('active')])"
    "+'</td><td>'+r[i('status')]+'</td></tr>'});"
    "h+='<tr><th>TOTAL</th><th></th><th></th><th></th><th></th><th class=\"n\">'+hm(t)+'</th><th></th></tr>';"
    "document.getElementById('b').innerHTML=h}"
    "document.getElementById('q').addEventListener('input',draw);draw();"
    "</script>"
  )
)


;;; ---- build ----------------------------------------------------------------

;;; Render the dashboard for ROWS and return the file path
(defun ch:build-dashboard (rows from to tag / tpl out f lines wrote ln)
  (setq tpl   (ch:template-file)
        out   (ch:path+ (ch:reports-dir (ch:root))
                        (strcat "CADHours-Dashboard-" (ch:safe-name tag) ".html"))
        lines (if tpl (ch:read-lines tpl) (ch:fallback-html))
        wrote nil)
  (ch:mkpath (ch:reports-dir (ch:root)))
  (if (and lines (setq f (open out "w")))
    (progn
      (foreach ln lines
        (if (vl-string-search (ch:dash-marker) ln)
          (progn (ch:write-data f rows from to) (setq wrote T))
          (write-line ln f)
        )
      )
      (close f)
      (if wrote
        out
        (progn
          (ch:say "The dashboard template has no data marker - page not built.")
          nil
        )
      )
    )
  )
)

;;; Open PATH with whatever the workstation uses for .html
(defun ch:open-file (path / sh)
  (if (findfile path)
    (progn
      (setq sh (vl-catch-all-apply 'vlax-create-object (list "WScript.Shell")))
      (if (vl-catch-all-error-p sh)
        (startapp "explorer.exe" (strcat "\"" path "\""))
        (progn
          (vl-catch-all-apply 'vlax-invoke
            (list sh 'Run (strcat "\"" path "\"") 1 :vlax-false))
          (vl-catch-all-apply 'vlax-release-object (list sh))
        )
      )
    )
  )
  (princ)
)


;;; ---- command ---------------------------------------------------------------

(defun c:CHDASH ( / *error* ans rng user job rows file)
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\n** CADHours: " msg))
    )
    (princ)
  )
  (ch:cfg-load)
  (setq ans (ch:trim
              (getstring T "\nPeriod [date | TODAY | WEEK | MONTH | ALL | n days] <MONTH>: ")))
  (if (= ans "") (setq ans "MONTH"))
  (setq rng (ch:parse-range ans))
  (setq user (ch:trim (getstring T "\nUser <* for everyone>: ")))
  (if (= user "")  (setq user "*"))
  (if (= user ".") (setq user (ch:user)))
  (setq job (ch:trim (getstring T "\nJob number <*>: ")))
  (if (= job "") (setq job "*"))

  (ch:say "Reading sessions...")
  (setq rows (ch:filter (ch:load-rows (car rng) (cadr rng))
                        (list (cons "user" user) (cons "job" job))))
  (if (null rows)
    (ch:say "Nothing matched that filter - no dashboard written.")
    (progn
      (setq file (ch:build-dashboard rows (car rng) (cadr rng)
                                     (strcat (car rng) "_" (cadr rng))))
      (if file
        (progn
          (ch:say (strcat (itoa (length rows)) " session(s) -> " file))
          (ch:open-file file)
        )
        (ch:say "Could not write the dashboard.")
      )
    )
  )
  (princ)
)


(princ)
;;; ============================================================ EOF
