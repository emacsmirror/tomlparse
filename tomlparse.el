;;; tomlparse --- A straight-foward tree sitter based parser for toml data -*- lexical-binding: t; -*-

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/tomlparse.el
;; Version: 0.1.0
;; License: GPLv3
;; SPDX-License-Identifier: GPL-3.0-only
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This is a tree sitter based reader for toml data.  It provides three functions
;;
;; * `tomlparse-file' – to read toml data from a file
;; * `tomlparse-buffer' - to read toml data from the current buffer
;; * `tomlparse-string' - to read toml data a string
;;
;; All the functions return the toml data as a hash table similar to
;; `json-parse-string'.
;;
;; In order to use it, the tree sitter module must be compiled into Emacs and the toml
;; language grammar must be installed.

;; To be documented

;;; Code:

(require 'treesit)
(require 'iso8601)
(require 'json)

(defvar tomlparse--current-node nil
  "Internal variable to keep track of the current node for error messages.")

(defvar tomlparse--seen-table-arrays nil
  "Internal variable to keep track of all the table arrays.")

(defvar tomlparse--false-object nil
  "Internal variable store the object to be used for `false'.")

(defvar tomlparse--datetime-object nil
  "Internal variable store the object to be used for `false'.")

(defun tomlparse-file (filename &rest args)
  "Read a toml file FILENAME and return a hash table with its contents.

The arguments ARGS are a list of keyword/argument pairs:

:false-object OBJ -- use TYPE to represent the toml false value.
  It defaults to :false.

:datetime-as SYM -- If SYM equals `string', datetime fields are passed
through as string.  Otherwise they are passed as iso8601 datetime and
put into a list."
  (with-temp-buffer
    (insert-file-contents filename)
    (apply #'tomlparse-buffer args)))

(defun tomlparse-buffer (&rest args)
  "Return toml data from the current buffer as a hash table.

The arguments ARGS are a list of keyword/argument pairs:

:false-object OBJ -- use TYPE to represent the toml false value.
  It defaults to :false.

:datetime-as SYM -- If SYM equals `string', datetime fields are passed
through as string.  Otherwise they are passed as iso8601 datetime and
put into a list."
  (apply #'tomlparse-string (buffer-string) args))

(defun tomlparse-string (string &rest args)
  "Return a hash table with the contents of the toml data STRING.

The arguments ARGS are a list of keyword/argument pairs:

:false-object OBJ -- use TYPE to represent the toml false value.
  It defaults to :false.

:datetime-as SYM -- If SYM equals `string', datetime fields are passed
through as string.  Otherwise they are passed as iso8601 datetime and
put into a list."
  (setq tomlparse--false-object (cadr (or (plist-member args :false-object) '(t :false))))
  (setq tomlparse--datetime-object (plist-get args :datetime-as))
  (catch 'result
    (let ((root (treesit-parse-string string'toml)))
      (setq tomlparse--seen-table-arrays nil)
      (throw 'result (tomlparse--table root)))))


(defun tomlparse--table (root)
  "Analyze the toml table of the node ROOT and return a hash table of its contents."
  (let ((root-hash-table (make-hash-table :test 'equal)))
    (dolist (node (treesit-node-children root))
      (let ((tomlparse--current-node node))
        (pcase (treesit-node-type node)
          ("pair" (tomlparse--pair node root-hash-table))
          ("table" (tomlparse--subtable node root-hash-table))
          ("table_array_element" (tomlparse--table-array-element node root-hash-table))
          ("ERROR" (tomlparse--error)))))
    root-hash-table))

(defun tomlparse--pair (node hash-table)
  "Analyze the toml pair covered by NODE and put it into HASH-TABLE."
  (let* ((key-node (nth 0 (treesit-node-children node)))
         (value (tomlparse--value (nth 2 (treesit-node-children node))))
         (target (tomlparse--climb-tree key-node hash-table))
         (key (tomlparse--key-text (car target)))
         (target-hash-table (or (cdr target) hash-table)))
    (when (gethash key target-hash-table)
      (tomlparse--error (format "duplicate key `%s`" key)))
    (puthash key value target-hash-table)))

(defun tomlparse--subtable (node hash-table)
  "Analyze the toml subtable covered by NODE and put it into HASH-TABLE."
  (let* ((target (tomlparse--climb-tree (cadr (treesit-node-children node)) hash-table))
         (key (tomlparse--key-text (car target)))
         (target-hash-table (or (cdr target) hash-table))
         (value (tomlparse--table node)))
    (when (gethash key target-hash-table)
      (tomlparse--error (format "table `%s` already defined"
                                (treesit-node-text (cadr (treesit-node-children  tomlparse--current-node))))))
    (puthash key value target-hash-table)))

(defun tomlparse--table-array-element (node hash-table)
  "Analyze the toml table array element covered by NODE and put it into HASH-TABLE."
  (let* ((target (tomlparse--climb-tree (cadr (treesit-node-children node)) hash-table))
         (key (tomlparse--key-text (car target)))
         (target-hash-table (or (cdr target) hash-table))
         (value  (tomlparse--table node))
         (old-array (or (tomlparse--table-array key target-hash-table) []))
         (new-array (vconcat old-array `[,value])))
    (add-to-list 'tomlparse--seen-table-arrays key)
    (puthash key new-array target-hash-table)))

(defun tomlparse--table-array (key hash-table)
  "Get KEY from HASH-TABLE if it exists.

If the result is not nil nor a already partially read table array, a
duplication error is raised."
  (let ((candidate (gethash key hash-table)))
    (pcase candidate
      ((pred hash-table-p) (tomlparse--error (format "table `%s` already defined" key)))
      ((pred vectorp) (if (member key tomlparse--seen-table-arrays)
                          candidate
                        (tomlparse--error (format "duplicate key `%s`" key))))
      ((pred identity)  (tomlparse--error (format "duplicate key `%s`" key)))
      (_ candidate))))

(defun tomlparse--climb-tree (key-node hash-table)
  "Climb the tree of key KEY-NODE to the and return the leaf hash table.

The leaf hash table is the hash table in which the value of the pair is
to be put.  It is created as needed and put into HASH-TABLE appropriately."
  (pcase (treesit-node-type key-node)
    ((or "bare_key" "quoted_key")
     (if (equal (treesit-node-type (treesit-node-parent key-node)) "dotted_key")
         (cons key-node hash-table)
       (cons key-node nil)))
    ("dotted_key" (let* ((branch (tomlparse--climb-tree (car (treesit-node-children key-node)) hash-table))
                         (branch-key (tomlparse--key-text (car branch)))
                         (branch-hash-table (tomlparse--hash-table-or-new-one branch-key (cdr branch)))
                         (leaf-key-node (nth 2 (treesit-node-children key-node))))
                    (cons leaf-key-node branch-hash-table)))))

(defun tomlparse--leaf-hash-table (key hash-table)
  "Return the leaf hash table of HASH-TABLE for KEY if it exists.

In case of an array of tables the last table of the array is returned."
  (let ((candidate (gethash key hash-table)))
    (pcase candidate
      ((pred vectorp) (aref candidate (1- (length candidate))))
      ((pred hash-table-p) candidate)
      ((pred not) nil)
      (_ (let ((duplicate-key
                (treesit-node-text (car (treesit-node-children
                                         (car (treesit-node-children tomlparse--current-node)))))))
           (tomlparse--error (format "duplicate key `%s`" duplicate-key)))))))

(defun tomlparse--hash-table-or-new-one (key hash-table)
  "Return the hash table for KEY of HASH-TABLE.  Create it if needed."
  (or (tomlparse--leaf-hash-table key hash-table)
      (let ((new-hash-table (make-hash-table :test 'equal)))
        (puthash key new-hash-table hash-table)
        new-hash-table)))

(defun tomlparse--key-text (key-node)
  "Extract the key string of KEY-NODE."
  (let ((node-text (treesit-node-text key-node)))
    (pcase (treesit-node-type key-node)
      ("bare_key" node-text)
      ("quoted_key" (substring node-text 1 -1)))))

(defun tomlparse--value (node)
  "Parse the pair value of NODE."
  (let ((value (treesit-node-text node)))
    (pcase (treesit-node-type node)
      ("string" (tomlparse--string value))
      ((or "integer" "float") (tomlparse--number-to-string value))
      ("boolean" (or (equal value "true") tomlparse--false-object))
      ((or "offset_date_time" "local_date_time")
       (pcase tomlparse--datetime-object
         ('string value)
         (_ (iso8601-parse  (string-replace " " "T" value)))))
      ("local_date" (pcase tomlparse--datetime-object
         ('string value)
         (_ (iso8601-parse-date (string-replace " " "T" value)))))
      ("local_time" (pcase tomlparse--datetime-object
         ('string value)
         (_ (iso8601-parse-time (string-replace " " "T" value)))))
      ("array" (tomlparse--array node))
      ("inline_table" (tomlparse--table node)))))

(defun tomlparse--string (value)
  "Parse the string and un-escape in VALUE."
  (cond ((eq (string-match "\"\"\"\n*\\(\\(.\\|\n\\)*\\)\"\"\"" value) 0)
         (tomlparse--unmask-triple-quote-string (replace-regexp-in-string "\\\\[ \n\t]+" "" (match-string 1 value))))
        ((eq (string-match "'''\n*\\(\\(.\\|\n\\)*\\)'''" value) 0)
         (tomlparse--unmask-triple-quote-string (match-string 1 value)))
        ((eq (string-match "'\\(.*\\)'" value) 0)
         (match-string 1 value))
        (t (json-parse-string value))))

(defun tomlparse--unmask-triple-quote-string (string)
  "Unmask t \"\"\"triple quoted string\"\"\" STRING."
  (string-replace "\\\"" "\"" string))

(defun tomlparse--number-to-string (value)
  "Parse a number from the value string VALUE."
  (pcase value
    ("inf" 1.0e+INF)
    ("-inf" -1.0e+INF)
    ("nan" 0.0e+NaN)
    ("-nan" -0.0e+NaN)
    (_(let ((base 10))
       (when (string-match "0\\([xob]\\)\\(.*\\)" value)
         (setq base (pcase (match-string 1 value) ("x" 16) ("o" 8) ("b" 2)))
         (setq value (match-string 2 value)))
       (string-to-number (string-replace "_" "" value) base)))))

(defun tomlparse--array (array-node)
  "Parse the array value of ARRAY-NODE."
  (vconcat (remq nil (mapcar #'tomlparse--array-child (treesit-node-children array-node)))))

(defun tomlparse--array-child (node)
  "Parse the value of array element NODE if it is an array element."
  (unless (member (treesit-node-type node) '("[" "]" ","))
    (tomlparse--value node)))

(defun tomlparse--error (&optional msg)
  "Write an error message referencing the line of NODE and maybe MSG."
  (user-error (concat
               (format "Broken toml data: line %s" (line-number-at-pos (treesit-node-start tomlparse--current-node)))
               (when msg (format " (%s)" msg))))
  (throw 'result nil))

(provide 'tomlparse)

;;; tomlparse.el ends here
