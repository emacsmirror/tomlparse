;;; tomltest.el --- use toml-test cases for tomlparse.el  -*- lexical-binding: t; -*-

(require 'tomlparse)
(require 'ert)

(defun json-parse-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer)))

(defun compare (toml-file json-file)
  (let ((json-data (json-parse-file json-file))
        (toml-data (tomlparse-file toml-file)))
    (do-compare toml-data json-data)))

(defun do-compare (toml-data json-data)
  ;(message "do-compare: %s %s %s %s" toml-data json-data (type-of toml-data) (type-of json-data))
  (pcase toml-data
    ((pred hash-table-p)
     (should (equal (sort (hash-table-keys toml-data)) (sort (hash-table-keys json-data))))
     (dolist (key (hash-table-keys json-data))
      ;(message "key %s" key)
      (do-compare (gethash key toml-data) (gethash key json-data))))
    ((pred vectorp) (compare-array json-data toml-data))
    (_ (compare-value toml-data json-data))))

(defun compare-value (toml-value json-value)
  ;(message "compare-value: %s %s" toml-value json-value)
  (should (equal (sort (hash-table-keys json-value)) '("type" "value")))
  (let ((type (gethash "type" json-value))
        (json-value (gethash "value" json-value)))
    ;(message "comparing %s %s %s" type toml-value json-value)
    (pcase type
      ("integer" (should (equal toml-value (string-to-number json-value))))
      ("float" (compare-floats toml-value json-value))
      ("string" (should (equal json-value toml-value)))
      ("bool" (if (eq toml-value :false) (should (equal json-value "false")) (should (equal json-value "true"))))
      ("datetime" (should (equal toml-value (iso8601-parse json-value t))))
      ("date-local" (should (equal toml-value (iso8601-parse-date json-value))))
      ("datetime-local" (should (equal toml-value (iso8601-parse json-value t))))
      ("time-local" (should (equal toml-value (iso8601-parse-time json-value t))))
      (_ (should (eq type nil))))))

(defun compare-array (toml-array json-array)
  (should (eq (length toml-array) (length json-array)))
  (cl-mapcar #'do-compare json-array toml-array))

(defun compare-floats (toml-val json-val)
  ;(message "compare-floats: %s %s" toml-val json-val)
  (pcase json-val
    ((or "inf" "+inf") (should (equal toml-val 1.0e+INF)))
    ("-inf" (should (equal toml-val -1.0e+INF)))
    ("nan" (should (or (equal toml-val 0.0e+NaN) (equal toml-val -0.0e+NaN))))
    ("-nan" (should (equal toml-val -0.0e+NaN)))
    ("-0" (should (equal toml-val -0.0)))
    (_ (should (equal toml-val (float (string-to-number json-val)))))))
