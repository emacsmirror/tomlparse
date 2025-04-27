;;; tomlparse.el --- A straight-foward tree sitter based parser for toml data  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Johannes Mueller

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/tomlparse.el
;; Version: 0.0.1
;; License: GPLv3
;; SPDX-License-Identifier: GPL-3.0-only
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This is a tree sitter based reader for TOML data.  It provides three
;; functions
;;
;; * `tomlparse-file' – to read TOML data from a file
;; * `tomlparse-buffer' - to read TOML data from the current buffer
;; * `tomlparse-string' - to read TOML data a string
;;
;; All the functions return the TOML data as a hash table similar to
;; `json-parse-string'.  Just like with `json-parse-string' you can also get the
;; TOML data as a `alist' or `plist' object.  See the documentation of the
;; functions for details.
;;
;; In order to use it, the tree sitter module must be compiled into Emacs and
;; the TOML language grammar must be installed.  Make sure to pick the latest
;; TOML grammar: https://github.com/tree-sitter-grammars/tree-sitter-toml You
;; can install toml grammar by the following snippet in your startup file
;;
;; (add-to-list
;;  'treesit-language-source-alist
;;  '(toml "https://github.com/tree-sitter-grammars/tree-sitter-toml"))
;; (unless (treesit-language-available-p 'toml)
;;   (treesit-install-language-grammar 'toml))

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
  "Internal variable store the object to be used for datetime objects.")

(defconst tomlparse--mangle-function-alist '((alist . tomlparse--hash-table-to-alist)
                                             (plist . tomlparse--hash-table-to-plist)
                                             (nil . identity))
  "Functions to mangle the resulting hash table according to the :object-type arg.")

(defun tomlparse-file (filename &rest args)
  "Read a toml file FILENAME and return a hash table with its contents.

The arguments ARGS are a list of keyword/argument pairs:

:object-type TYPE -- use TYPE to represent TOML tables.
  Type can be hash-table (the default), `alist' or `plist'

:false-object OBJ -- use TYPE to represent the TOML false value.
  It defaults to :false.

:datetime-as SYM -- If SYM equals `string'.
  `datetime' fields are passed through as string.  Otherwise they are
  passed as iso8601 datetime and put into a list."
  (with-temp-buffer
    (insert-file-contents filename)
    (apply #'tomlparse-buffer args)))

(defun tomlparse-buffer (&rest args)
  "Return TOML data from the current buffer as a hash table.

The arguments ARGS are a list of keyword/argument pairs:

:object-type TYPE -- use TYPE to represent TOML tables.
  Type can be hash-table (the default), `alist' or `plist'

:false-object OBJ -- use TYPE to represent the TOML false value.
  It defaults to :false.

:datetime-as SYM -- If SYM equals `string'.
  `datetime' fields are passed through as string.  Otherwise they are
  passed as iso8601 datetime and put into a list."
  (apply #'tomlparse-string (substring-no-properties (buffer-string)) args))

(defun tomlparse-string (string &rest args)
  "Return a hash table with the contents of the toml data STRING.

The arguments ARGS are a list of keyword/argument pairs:

:object-type TYPE -- use TYPE to represent TOML tables.
  Type can be hash-table (the default), `alist' or `plist'

:false-object OBJ -- use TYPE to represent the TOML false value.
  It defaults to :false.

:datetime-as SYM -- If SYM equals `string'.
  `datetime' fields are passed through as string.  Otherwise they are
  passed as iso8601 datetime and put into a list."
  (funcall (alist-get (plist-get args :object-type) tomlparse--mangle-function-alist)
           (tomlparse--parse-string string args)))

(defun tomlparse--parse-string (string args)
  "Do the actual parsing of STRING with keyword/argument pairs ARGS."
  (let ((tomlparse--false-object (cadr (or (plist-member args :false-object) '(t :false))))
        (tomlparse--datetime-object (plist-get args :datetime-as))
        (tomlparse--seen-table-arrays nil))
    (tomlparse--table (treesit-parse-string string 'toml))))

(defun tomlparse--table (root-node)
  "Analyze the toml table of the ROOT-NODE and return a hash table of its contents."
  (let ((root-hash-table (make-hash-table :test 'equal)))
    (dolist (tomlparse--current-node (treesit-node-children root-node))
      (pcase (treesit-node-type tomlparse--current-node)
        ("pair" (tomlparse--pair tomlparse--current-node root-hash-table))
        ("table" (tomlparse--subtable tomlparse--current-node root-hash-table))
        ("table_array_element" (tomlparse--table-array-element tomlparse--current-node root-hash-table))
        ("ERROR" (tomlparse--error "Parser reported an error"))))
    root-hash-table))

(defun tomlparse--pair (node hash-table)
  "Analyze the toml pair covered by NODE and put it into HASH-TABLE."
  (let* ((target (tomlparse--climb-tree (car (treesit-node-children node)) hash-table))
         (key (tomlparse--key-text (car target)))
         (target-hash-table (cdr target))
         (value (tomlparse--value (caddr (treesit-node-children node)))))
    (when (gethash key target-hash-table)
      (tomlparse--error (format "Duplicate key `%s`" key)))
    (puthash key value target-hash-table)))

(defun tomlparse--subtable (node hash-table)
  "Analyze the toml subtable covered by NODE and put it into HASH-TABLE."
  (let* ((target (tomlparse--climb-tree (cadr (treesit-node-children node)) hash-table))
         (key (tomlparse--key-text (car target)))
         (target-hash-table (cdr target))
         (value (tomlparse--table node)))
    (if-let* ((already-existing (gethash key target-hash-table)))
        (if (tomlparse--table-open-for-new-entries-p already-existing)
            (maphash (lambda (key value) (puthash key value already-existing)) value)
          (tomlparse--error (format "Table `%s` already defined"
                                    (treesit-node-text (cadr (treesit-node-children tomlparse--current-node))))))
      (puthash key value target-hash-table))))

(defun tomlparse--table-open-for-new-entries-p (already-existing)
  "Check if the ALREADY-EXISTING object is a hash-table that can accept entries.

If ALREADY-EXISTING has an entry which is itself a hash table we can add
the following entries to it.

If ALREADY-EXISTING has an entry which is a vector, we can add the
follwing entries next to the array.

If none of the two is true, we have simply a duplicated key."
  (and (hash-table-p already-existing)
       (or (cl-find-if #'hash-table-p (hash-table-values already-existing))
           (cl-find-if #'vectorp (hash-table-values already-existing)))))

(defun tomlparse--table-array-element (node hash-table)
  "Analyze the toml table array element covered by NODE and put it into HASH-TABLE."
  (let* ((target (tomlparse--climb-tree (cadr (treesit-node-children node)) hash-table))
         (key (tomlparse--key-text (car target)))
         (target-hash-table (cdr target))
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
      ((pred hash-table-p) (tomlparse--error (format "Table `%s` already defined" key)))
      ((pred vectorp) (if (member key tomlparse--seen-table-arrays)
                          candidate
                        (tomlparse--error (format "Duplicate key `%s`" key))))
      ((pred identity)  (tomlparse--error (format "Duplicate key `%s`" key)))
      (_ candidate))))

(defun tomlparse--climb-tree (key-node hash-table)
  "Climb the tree of key KEY-NODE to the and return the leaf hash table.

The leaf hash table is the hash table in which the value of the pair is
to be put.  It is created as needed and put into HASH-TABLE appropriately."
  (pcase (treesit-node-type key-node)
    ("dotted_key" (let* ((branch (tomlparse--climb-tree (car (treesit-node-children key-node)) hash-table))
                         (branch-key (tomlparse--key-text (car branch)))
                         (branch-hash-table (tomlparse--hash-table-or-new-one branch-key (cdr branch)))
                         (leaf-key-node (caddr (treesit-node-children key-node))))
                    (cons leaf-key-node branch-hash-table)))
    (_ (cons key-node hash-table))))

(defun tomlparse--hash-table-or-new-one (key hash-table)
  "Return the hash table for KEY of HASH-TABLE.  Create it if needed."
  (or (tomlparse--leaf-hash-table key hash-table)
      (let ((new-hash-table (make-hash-table :test 'equal)))
        (puthash key new-hash-table hash-table)
        new-hash-table)))

(defun tomlparse--leaf-hash-table (key hash-table)
  "Return the leaf hash table of HASH-TABLE for KEY if it exists.

In case of an array of tables the last table of the array is returned
because that's the one we will add entries to."
  (let ((candidate (gethash key hash-table)))
    (pcase candidate
      ((pred vectorp) (aref candidate (1- (length candidate))))
      ((pred hash-table-p) candidate)
      ((pred not) nil)
      (_ (let ((duplicate-key
                (treesit-node-text (car (treesit-node-children
                                         (car (treesit-node-children tomlparse--current-node)))))))
           (tomlparse--error (format "Duplicate key `%s`" duplicate-key)))))))

(defun tomlparse--key-text (key-node)
  "Extract the key string of KEY-NODE."
  (let ((node-text (treesit-node-text key-node)))
    (pcase (treesit-node-type key-node)
      ("bare_key" node-text)
      ("quoted_key" (if (equal (treesit-node-type (cadr (treesit-node-children key-node))) "escape_sequence")
                        (tomlparse--string-literal node-text)
                      (substring node-text 1 -1))))))


(defun tomlparse--string-literal (string)
  "Parse the sting literal STRING from TOML data."
  (json-parse-string (tomlparse--capital-unicode-escapes (string-replace "\t" "\\t" string))))

(defun tomlparse--capital-unicode-escapes (string)
  "Unescape unicode sequences in STRING."
  (replace-regexp-in-string
   "\\\\U\\([0-9a-fA-F]\\{8\\}\\)"
   (lambda (match)
     (char-to-string (string-to-number (match-string 1 match) 16)))
   (replace-regexp-in-string "\\\\U0\\{4\\}\\([0-9a-fA-F]\\{4\\}\\)" "\\\\u\\1" string t) t))

(defun tomlparse--value (node)
  "Parse the pair value of NODE."
  (let ((value (treesit-node-text node)))
    (pcase (treesit-node-type node)
      ("string" (tomlparse--string value))
      ((or "integer" "float") (tomlparse--string-to-number value))
      ("boolean" (or (equal value "true") tomlparse--false-object))
      ((or "offset_date_time" "local_date_time")
       (pcase tomlparse--datetime-object
         ('string value)
         (_ (iso8601-parse (string-replace " " "T" value) t))))
      ("local_date"
       (pcase tomlparse--datetime-object
         ('string value)
         (_ (iso8601-parse-date (string-replace " " "T" value)))))
      ("local_time"
       (pcase tomlparse--datetime-object
         ('string value)
         (_ (iso8601-parse-time (string-replace " " "T" value) t))))
      ("array" (tomlparse--array node))
      ("inline_table" (tomlparse--table node)))))

(defun tomlparse--string (value)
  "Parse the string and un-escape in VALUE."
  (cond ((eq (string-match "\"\"\"\n*\\(\\(.\\|\n\\)*\\)\"\"\"" value) 0)
         (let ((string (match-string 1 value)))
           (tomlparse--unmask-triple-quote-string string)))
        ((eq (string-match "'''\n*\\(\\(.\\|\n\\)*\\)'''" value) 0)
         (match-string 1 value))
        ((eq (string-match "'\\(.*\\)'" value) 0)
         (match-string 1 value))
        (t (tomlparse--string-literal value))))

(defun tomlparse--unmask-triple-quote-string (string)
  "Unmask t \"\"\"triple quoted string\"\"\" STRING."
  (tomlparse--string-literal
   (format "\"%s\""
           (string-replace
            "\t" "\\t"
            (string-replace
             "\n" "\\n"
             (tomlparse--remove-umasked-line-breaks
              (string-replace
               "\"" "\\\""
               (string-replace
                "\\\"" "\"" string))))))))

(defconst tomlparse--line-break-regexp "^\\([^\\]*\\)\\(\\(\\\\\\\\\\)*\\)\\\\[ \t]*\n[ \t\n]*")

(defun tomlparse--remove-umasked-line-breaks (string)
  "Remove all unmasked line breaks in STRING."
  (while (string-match tomlparse--line-break-regexp string)
    (setq string
          (replace-match (concat (match-string 1 string) (match-string 2 string) (match-string 2 string))
                         'fixedcase nil string)))
  string)

(defun tomlparse--string-to-number (value)
  "Parse a number from the value string VALUE."
  (pcase value
    ("inf" 1.0e+INF)
    ("+inf" 1.0e+INF)
    ("-inf" -1.0e+INF)
    ("nan" 0.0e+NaN)
    ("+nan" 0.0e+NaN)
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

(defun tomlparse--object-for-alist (object)
  "Process OBJECT to put it into a result alist."
  (pcase object
    ((pred hash-table-p) (tomlparse--hash-table-to-alist object))
    ((pred vectorp) (cl-map 'vector 'tomlparse--object-for-alist object))
    (_ object)))

(defun tomlparse--hash-table-to-alist (hash-table)
  "Recursively convert HASH-TABLE to a nested alist."
  (let (alist)
    (maphash (lambda (key value)
               (let ((value (tomlparse--object-for-alist value)))
                 (setq alist (cons (cons (intern key) value) alist))))
             hash-table)
    alist))

(defun tomlparse--object-for-plist (object)
  "Process OBJECT to put it into a result plist."
  (pcase object
    ((pred hash-table-p) (tomlparse--hash-table-to-plist object))
    ((pred vectorp) (cl-map 'vector 'tomlparse--object-for-plist object))
    (_ object)))

(defun tomlparse--hash-table-to-plist (hash-table)
  "Recursively convert HASH-TABLE to a nested plist."
  (let (plist)
    (maphash (lambda (key value)
               (let ((value (tomlparse--object-for-plist value)))
                 (setq plist (cons (intern key) (cons value plist)))))
             hash-table)
    plist))

(defun tomlparse--error (&optional msg)
  "Write an error message referencing the line of NODE and maybe MSG."
  (user-error (concat
               (format "Broken toml data: line %s" (line-number-at-pos (treesit-node-start tomlparse--current-node)))
               (when msg (format " (%s)" msg)))))

(provide 'tomlparse)

;;; tomlparse.el ends here
