
(require 'tomlparse)

(defun hashtable-to-test-json (hash-table)
 ; (message "hash table is: %s" hash-table)
  (format "{%s}" (walk-hash-table hash-table)))

(defun mask-quote (string)
  (with-temp-buffer (json-insert string) (substring (buffer-string) 1 -1)))

(defun walk-hash-table (hash-table)
  (let (table-list)
    (maphash (lambda (key value)
               (push (format "\"%s\":%s" (mask-quote key) (mangle value)) table-list))
             hash-table)
    (string-join table-list ",")))

(defun mangle (value)
;  (message "mangling %s" value)
  (pcase value
    ((pred hash-table-p) (hashtable-to-test-json value))
    ((pred integerp) (format-value "integer" value))
    ((pred (lambda (v) (and (numberp v) (equal (number-to-string v) "1.0e+INF")))) (format-value "float" "inf"))
    ((pred (lambda (v) (and (numberp v) (equal (number-to-string v) "-1.0e+INF")))) (format-value "float" "-inf"))
    ((pred (lambda (v) (and (numberp v) (equal (number-to-string v) "0.0e+NaN")))) (format-value "float" "nan"))
    ((pred (lambda (v) (and (numberp v) (equal (number-to-string v) "-0.0e+NaN")))) (format-value "float" "nan"))
    ((pred vectorp) (format "[%s]" (string-join (walk-vector value) ",")))
    ((pred numberp) (format-value "float" value))
    ((pred listp) (format-datetime value))
    ((pred stringp) (format-value "string" (mask-quote value)))
    (:false (format-value "bool" "false"))
    (_ (format-value "bool" "true"))))

(defun walk-vector (value)
  (seq-map #'mangle value))

(defun format-datetime (value)
  ; (message "datetime %s" value)
  (let* ((Y (decoded-time-year value))
        (M (decoded-time-month value))
        (D (decoded-time-day value))
        (h (decoded-time-hour value))
        (m (decoded-time-minute value))
        (s (decoded-time-second value))
        (ms "")
        (zone-secs (decoded-time-zone value))
        ;(dst (decoded-time-dst value))
        (zone (when zone-secs
                (if (eq zone-secs 0)
                   "Z"
                 (concat (if (< zone-secs 0) "-" "+")
                         (format-time-string "%H:%M" (abs zone-secs) t))))))

    (when (and s (listp s))
      (setq ms (format ".%d" (/ (* 1000 (- (car s) (* (/ (car s) (cdr s)) (cdr s))  )) (cdr s))))
      (setq s (/ (car s) (cdr s))))

                                  ;      (message "%s %s converted %02.0f%s" s ms s ms)
    (cond
     ((not zone-secs)
      (cond
       ((not h) (format-value "date-local" (format "%04d-%02d-%02d" Y M D)))
       ((not Y) (format-value "time-local" (format "%02d:%02d:%02d%s" h m s ms)))
       (t (format-value "datetime-local" (format "%04d-%02d-%02dT%02d:%02d:%02d%s" Y M D h m s ms)))))
     (t (format-value "datetime" (format "%04d-%02d-%02dT%02d:%02d:%02d%s%s" Y M D h m s ms zone))))))

(defun format-value (type value)
;  (message "type: %s value: `%s'" type value)
  (format "{\"type\":\"%s\",\"value\":\"%s\"}" type (if (and (equal type "float") (not (member value '("inf" "-inf" "nan" "-nan"))))
                                                        (number-to-string value)
                                                      value)))

(defun toml2json (file)
  (princ (hashtable-to-test-json (tomlparse-file file))))
