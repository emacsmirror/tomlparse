
(require 'mocker)
(require 'tomlparse)
(require 'subr-x)


(defun hash-equal (hash1 hash2)
  "Compare two hash tables to see whether they are equal."
  ;; (message "comparing:")
  ;; (message "<%s>" hash1)
  ;; (message "<%s>" hash2)
  (and (= (hash-table-count hash1)
          (hash-table-count hash2))
       (catch 'flag (maphash (lambda (key x)
                               (let ((y (gethash key hash2)))
                                 (or (equal x y)
                                     (when (vectorp x)
                                       (and (vectorp y)
                                            (eql (length x) (length y))
                                            (progn (dotimes (i (length x))
                                                     (or (equal (aref x i) (aref y i))
                                                         (when (hash-table-p (aref x i))
                                                           (hash-equal (aref x i) (aref y i)))
                                                         (progn
                                                           (message "element %s different:\n%s\n%s" i (aref x i) (aref y i))
                                                           (throw 'flag nil))))
                                                   t)))
                                     (hash-equal x y)
                                     (throw 'flag nil))))
                             hash1)
              (throw 'flag t))))

(defun external-toml-parser (&rest args)
  (let ((toml-code (buffer-string)))
    (with-temp-buffer
      (insert toml-code)
      (shell-command-on-region (point-min) (point-max) "toml2json" nil 'no-mark)
      (goto-char (point-min))
      (apply #'json-parse-buffer args))))

(ert-deftest empty-buffer ()
  (let ((expected (make-hash-table)))
    (with-temp-buffer
     (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest only-comment ()
  (let ((expected (make-hash-table)))
    (with-temp-buffer
      (insert "# comment")
      (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest key-pair-integer ()
  (with-temp-buffer
    (insert "key = 2342")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest key-pair-string-double-quoted ()
  (with-temp-buffer
    (insert "key = \"value\"")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest key-pair-trailing-comment ()
  (with-temp-buffer
    (insert "key = 2342 # comment")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest key-pair-string-single-quoted ()
  (with-temp-buffer
    (insert "key = 'value'")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest key-pair-float ()
  (with-temp-buffer
    (insert "key = 3.14")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest key-pair-nan ()
  (with-temp-buffer
    (insert "key = nan")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "key" 0.0e+NaN expected)
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest key-pair-minus-nan ()
  (with-temp-buffer
    (insert "key = -nan")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "key" -0.0e+NaN expected)
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest key-pair-inf ()
  (with-temp-buffer
    (insert "key = inf")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "key" 1.0e+INF expected)
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest key-pair-minus-inf ()
  (with-temp-buffer
    (insert "key = -inf")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "key" -1.0e+INF expected)
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest key-pair-bool ()
  (with-temp-buffer
    (insert "key = true")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest simple-struct ()
  (with-temp-buffer
    (insert "
fruit.name = \"banana\"
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer)  expected)))))

(ert-deftest flat-struct ()
    (with-temp-buffer
      (insert "
fruit.name = \"banana\"
fruit . color = \"yellow\"
fruit . flavor = \"banana\"
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest nested-struct ()
  (with-temp-buffer
    (insert "
fruit.apple.smooth = true
fruit.apple.taste = \"sweet\"
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest inline-struct ()
  (with-temp-buffer
    (insert "
struct = { name = \"Baz Qux\", email = \"bazqux@example.com\", url = \"https://example.com/bazqux\" }
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest inline-table-line-break ()
  (skip-when  "The tree sitter gramar does not do TOML 1.1")
  (with-temp-buffer
    (insert "
# TOML 1.1 supports newlines in inline tables and trailing commas.

trailing-comma-1 = {
	c = 1,
}
trailing-comma-2 = { c = 1, }
")
    (let ((expected "{\"trailing-comma-1\":{\"c\":1},\"trailing-comma-2\":{\"c\":1}}"))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest double-quoted-key ()
  (with-temp-buffer
    (insert "\"key\" = 2342")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest single-quoted-key ()
  (with-temp-buffer
    (insert "'key' = 2342")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest quoted-unicode-keys ()
  (with-temp-buffer
    (insert "
\"\\u0000\" = \"null\"
'\\u0000' = \"different key\"
\"\\u0008 \\u000c \\U00000041 \\u007f \\u0080 \\u00ff \\ud7ff \\ue000 \\uffff \\U00010000 \\U0010ffff\" = \"escaped key\"

\"~  ÿ ퟿  ￿ 𐀀 􏿿\" = \"basic key\"
'l ~  ÿ ퟿  ￿ 𐀀 􏿿' = \"literal key\"
")
    (let ((expected (json-parse-string "{\"\\u0000\":\"null\",\"\\b \\f A   ÿ ퟿  ￿ 𐀀 􏿿\":\"escaped key\",\"\\\\u0000\":\"different key\",\"l ~  ÿ ퟿  ￿ 𐀀 􏿿\":\"literal key\",\"~  ÿ ퟿  ￿ 𐀀 􏿿\":\"basic key\"}")))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest one-quoted-unicode-key-first-position ()
  (with-temp-buffer
    (insert "
\"\\u0000\" = \"null\"")
    (let ((expected (json-parse-string "{\"\\u0000\":\"null\"}")))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest one-quoted-unicode-key-last-position ()
  (with-temp-buffer
    (insert "
\"uuu\\u0000\" = \"null\"")
    (let ((expected (json-parse-string "{\"uuu\\u0000\":\"null\"}")))
      (should (hash-equal (tomlparse-buffer) expected)))))



(ert-deftest string-with-masked-characters ()
  (with-temp-buffer
    (insert "str = \"I'm a string. \\\"You can quote me\\\". Name\\tJos\\u00E9\\nLocation\\tSF.\"")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest string-multiline ()
  (with-temp-buffer
    (insert "str1 = \"\"\"\nRoses are red\nViolets are blue\"\"\"")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest string-one-line-multi-lined ()
  (with-temp-buffer
    (insert "
str1 = \"The quick brown fox jumps over the lazy dog.\"
str2 = \"\"\"
The quick brown \\


  fox jumps over \\
    the lazy dog.\"\"\"")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest string-multi-lined-tab ()
  (with-temp-buffer
    (insert "
keep-ws-before = \"\"\"a\t\nb\"\"\"")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest strings-wysiwyg ()
  (with-temp-buffer
    (insert "
# What you see is what you get.
winpath  = 'C:\\Users\\nodejs\\templates'
winpath2 = '\\\\ServerX\\admin$\\system32\\'
quoted   = 'Tom \"Dubs\" Preston-Werner'
regex    = '<\\i\\c*\\s*>'
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest string-triple-single-quoted ()
  (with-temp-buffer
    (insert "
regex2 = '''I [dw]on't need \\d{2} apples'''
lines  = '''
The first newline is
trimmed in raw strings.
   All other whitespace
   is preserved.
'''")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest string-quotes-combined ()
  (with-temp-buffer
    (insert "
quot15 = '''Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"'''

# apos15 = '''Here are fifteen apostrophes: ''''''''''''''''''  # INVALID
apos15 = \"Here are fifteen apostrophes: '''''''''''''''\"

# 'That,' she said, 'is still pointless.'
str = ''''That,' she said, 'is still pointless.''''
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest string-triple-quoted-one-line ()
    (with-temp-buffer
      (insert "
str4 = \"\"\"Here are two quotation marks: \"\". Simple enough.\"\"\"
str5 = \"\"\"Here are three quotation marks: \"\"\\\".\"\"\"
str6 = \"\"\"Here are fifteen quotation marks: \"\"\\\"\"\"\\\"\"\"\\\"\"\"\\\"\"\"\\\".\"\"\"")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest string-quote-inside-triple-quote ()
    (with-temp-buffer
      (insert "
# \"This,\" she said, \"is just a pointless statement.\"
str7 = \"\"\"\"This,\" she said, \"is just a pointless statement.\"\"\"\"
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest string-unicode-escape-normal-quote ()
  (with-temp-buffer
    (insert "
delta-1 = \"\\u03B4\"
delta-2 = \"\\U000003B4\"
a       = \"\\u0061\"
b       = \"\\u0062\"
c       = \"\\U00000063\"
null-1  = \"\\u0000\"
null-2  = \"\\U00000000\"
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest string-unicode-escape-triple-quote ()
  (with-temp-buffer
    (insert "
ml-delta-1 = \"\"\"\\u03B4\"\"\"
ml-delta-2 = \"\"\"\\U000003B4\"\"\"
ml-a       = \"\"\"\\u0061\"\"\"
ml-b       = \"\"\"\\u0062\"\"\"
ml-c       = \"\"\"\\U00000063\"\"\"
ml-null-1  = \"\"\"\\u0000\"\"\"
ml-null-2  = \"\"\"\\U00000000\"\"\"
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest integers ()
  (with-temp-buffer
    (insert "
int1 = +99
int2 = 42
int3 = 0
int4 = -17
int5 = 1_000
int6 = 5_349_221
int7 = 53_49_221  # Indian number system grouping
int8 = 1_2_3_4_5  # VALID but discouraged
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest hex-values ()
  (with-temp-buffer
    (insert "
# hexadecimal with prefix `0x`
hex1 = 0xDEADBEEF
hex2 = 0xdeadbeef
hex3 = 0xdead_beef

# octal with prefix `0o`
oct1 = 0o01234567
oct2 = 0o755 # useful for Unix file permissions

# binary with prefix `0b`
bin1 = 0b11010110")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest floats ()
  (with-temp-buffer
    (insert "
# fractional
flt1 = +1.0
flt2 = 3.1415
flt3 = -0.01

# exponent
flt4 = 5e+22
flt5 = 1e06
flt6 = -2E-2

# both
flt7 = 6.626e-34
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest spec-inf ()
  (with-temp-buffer
    (insert "
# infinity
sf1 = inf  # positive infinity
sf2 = +inf # positive infinity
sf3 = -inf # negative infinity
")
    (let ((expected '("sf3" -1.0e+INF "sf2" 1.0e+INF "sf1" 1.0e+INF)))
      (should (equal (tomlparse-buffer :object-type 'plist) expected)))))

(ert-deftest spec-nan ()
  (with-temp-buffer
    (insert "
# not a number
sf4 = nan  # actual sNaN/qNaN encoding is implementation-specific
sf5 = +nan # same as `nan`
sf6 = -nan # valid, actual encoding is implementation-specific
")
    (let ((expected '("sf6" -0.0e+NaN "sf5" 0.0e+NaN "sf4" 0.0e+NaN)))
      (should (equal (tomlparse-buffer :object-type 'plist) expected)))))


(ert-deftest booleans-false ()
  (with-temp-buffer
    (insert "
bool1 = true
bool2 = false
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest booleans-nil ()
  (with-temp-buffer
    (insert "
bool1 = true
bool2 = false
")
    (let ((expected (external-toml-parser :false-object nil)))
      (should (hash-equal (tomlparse-buffer :false-object nil) expected)))))


(ert-deftest offset-date-time-iso8601 ()
  (with-temp-buffer
    (insert "
odt1 = 1979-05-27T07:32:00Z
odt2 = 1979-05-27T00:32:00-07:00
odt3 = 1979-05-27T00:32:00.999999-07:00
odt4 = 1979-05-27 07:32:00Z
")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "odt1" `(0 32 7 27 5 1979 nil nil 0) expected)
      (puthash "odt2" `(0 32 0 27 5 1979 nil -1 ,(* -7 3600)) expected)
      (puthash "odt3" `((999999 . 1000000) 32 0 27 5 1979 nil -1 ,(* -7 3600)) expected)
      (puthash "odt4" `(0 32 7 27 5 1979 nil nil 0) expected)
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest offset-date-time-string ()
  (with-temp-buffer
    (insert "
odt1 = 1979-05-27T07:32:00Z
odt2 = 1979-05-27T00:32:00-07:00
odt3 = 1979-05-27T00:32:00.999999-07:00
odt4 = 1979-05-27 07:32:00Z
")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "odt1" "1979-05-27T07:32:00Z" expected)
      (puthash "odt2" "1979-05-27T00:32:00-07:00" expected)
      (puthash "odt3" "1979-05-27T00:32:00.999999-07:00" expected)
      (puthash "odt4" "1979-05-27 07:32:00Z" expected)
      (should (hash-equal (tomlparse-buffer :datetime-as 'string) expected)))))


(ert-deftest local-date-time-iso8601 ()
  (with-temp-buffer
    (insert "
ldt1 = 1979-05-27T07:32:00
ldt2 = 1979-05-27T00:32:00.999999
ldt3 = 1979-05-27 07:32:00
")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "ldt1" '(0 32 7 27 5 1979 nil -1 nil) expected)
      (puthash "ldt2" '((999999 . 1000000) 32 0 27 5 1979 nil -1 nil) expected)
      (puthash "ldt3" '(0 32 7 27 5 1979 nil -1 nil) expected)
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest local-date-time-string ()
  (with-temp-buffer
    (insert "
ldt1 = 1979-05-27T07:32:00
ldt2 = 1979-05-27T00:32:00.999999
ldt3 = 1979-05-27 07:32:00
")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "ldt1" "1979-05-27T07:32:00" expected)
      (puthash "ldt2" "1979-05-27T00:32:00.999999" expected)
      (puthash "ldt3" "1979-05-27 07:32:00" expected)
      (should (hash-equal (tomlparse-buffer :datetime-as 'string) expected)))))


(ert-deftest local-date-iso8601 ()
  (with-temp-buffer
    (insert "
ld1 = 1979-05-27
ld2 = 1981-05-27
")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "ld1" '(nil nil nil 27 05 1979 nil -1 nil) expected)
      (puthash "ld2" '(nil nil nil 27 05 1981 nil -1 nil) expected)
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest local-date-string ()
  (with-temp-buffer
    (insert "
ld1 = 1979-05-27
ld2 = 1981-05-27
")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "ld1" "1979-05-27" expected)
      (puthash "ld2" "1981-05-27" expected)
      (should (hash-equal (tomlparse-buffer :datetime-as 'string) expected)))))


(ert-deftest local-time-iso8061 ()
  (with-temp-buffer
    (insert "
lt1 = 07:32:00
lt2 = 00:32:00.999999
")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "lt1" '(0 32 7 nil nil nil nil -1 nil) expected)
      (puthash "lt2" '((999999 . 1000000) 32 0 nil nil nil nil -1 nil) expected)
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest local-time-string ()
  (with-temp-buffer
    (insert "
lt1 = 07:32:00
lt2 = 00:32:00.999999
")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "lt1" "07:32:00" expected)
      (puthash "lt2" "00:32:00.999999" expected)
      (should (hash-equal (tomlparse-buffer :datetime-as 'string) expected)))))


(ert-deftest local-time-no-seconds-iso8061 ()
  (skip-when  "The tree sitter gramar does not do TOML 1.1")
  (with-temp-buffer
    (insert "
lt1 = 07:32
lt2 = 00:32:00.999999
")
    (let ((expected (make-hash-table :test 'equal)))
      (puthash "lt1" '(0 32 7 nil nil nil nil -1 nil) expected)
      (puthash "lt2" '(0 32 0 nil nil nil nil -1 nil) expected)
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest arrays ()
  (with-temp-buffer
    (insert "
integers = [ 1, 2, 3 ]
colors = [ \"red\", \"yellow\", \"green\" ]
nested_arrays_of_ints = [ [ 1, 2 ], [3, 4, 5] ]
nested_mixed_array = [ [ 1, 2 ], [\"a\", \"b\", \"c\"] ]
string_array = [ \"all\", 'strings', \"\"\"are the same\"\"\", '''type''' ]
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest mixed-arrays ()
  (with-temp-buffer
    (insert "
# Mixed-type arrays are allowed
numbers = [ 0.1, 0.2, 0.5, 1, 2, 5 ]
contributors = [
  \"Foo Bar <foo@example.com>\",
  { name = \"Baz Qux\", email = \"bazqux@example.com\", url = \"https://example.com/bazqux\" }
]
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest multiline-arrays ()
  (with-temp-buffer
    (insert "
integers2 = [
  1, 2, 3
]

integers3 = [
  1,
  2, # this is ok
]
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest simple-table ()
  (with-temp-buffer
    (insert "
[table-1]
key1 = \"some string\"
key2 = 123
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest nested-table ()
  (with-temp-buffer
    (insert "
[dog.\"tater.man\"]
type.name = \"pug\"
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest two-tables ()
  (with-temp-buffer
    (insert "
[table-1]
key1 = \"some string\"
key2 = 123

[table-2]
key1 = \"another string\"
key2 = 456")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest two-tables-in-supertable ()
  (with-temp-buffer
    (insert "
[super-table]

[super-table.table-1]
key1 = \"some string\"
key2 = 123

[super-table-2]
key1 = \"another string\"
key2 = 456")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest array-of-tables-simple ()
  (with-temp-buffer
    (insert "
[[products]]
name = \"Hammer\"
sku = 738594937

[[products]]  # empty table within the array

[[products]]
name = \"Nail\"
sku = 284758393

color = \"gray\"
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest table-after-array-of-tables ()
  (with-temp-buffer
    (insert "
[[products]]
name = \"Hammer\"
sku = 738594937

[[products]]  # empty table within the array

[[products]]
name = \"Nail\"
sku = 284758393

color = \"gray\"

[table-1]
key1 = \"some string\"
key2 = 123

[table-2]
key1 = \"another string\"
key2 = 456
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest array-of-tables-complex ()
  (with-temp-buffer
    (insert "
[[fruits]]
name = \"apple\"

[fruits.physical]  # subtable
color = \"red\"
shape = \"round\"

[[fruits.varieties]]  # nested array of tables
name = \"red delicious\"

[[fruits.varieties]]
name = \"granny smith\"


[[fruits]]
name = \"banana\"

[[fruits.varieties]]
name = \"plantain\"
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest object-type-alist-simple ()
  (with-temp-buffer
    (insert "
fruit.apple.smooth = true
fruit.apple.taste = \"sweet\"
")
    (let ((expected '(("fruit" ("apple" ("taste" . "sweet") ("smooth" . t))))))
      (should (equal (tomlparse-buffer :object-type 'alist) expected)))))

(ert-deftest object-type-alist-array-of-table ()
  (with-temp-buffer
    (insert "
authors = [{name = \"Johannes Mueller\", email = \"github@johannes-mueller.org\"}]
")
    (let ((expected '(("authors" . [(("email" . "github@johannes-mueller.org") ("name" . "Johannes Mueller"))]))))
      (should (equal (tomlparse-buffer :object-type 'alist) expected)))))

(ert-deftest object-type-plist ()
  (with-temp-buffer
    (insert "
fruit.apple.smooth = true
fruit.apple.taste = \"sweet\"
")
    (let ((expected '("fruit" ("apple" ("taste" "sweet" "smooth" t)))))
      (should (equal (tomlparse-buffer :object-type 'plist) expected)))))


(ert-deftest object-type-plist-array-of-table ()
  (with-temp-buffer
    (insert "
authors = [{name = \"Johannes Mueller\", email = \"github@johannes-mueller.org\"}]
")
    (let ((expected '("authors" [("email" "github@johannes-mueller.org" "name" "Johannes Mueller")])))
      (should (equal (tomlparse-buffer :object-type 'plist) expected)))))


(ert-deftest inclomplete-pair-line-1 ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 1 (Parser reported an error)") :occur 1))))
    (with-temp-buffer
      (insert "key = # INVALID")
      (tomlparse-buffer))))

(ert-deftest invalid-double-line ()
  (skip-when "Treesitter does not catch this.")
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 1 (Parser reported an error)") :occur 1))))
    (with-temp-buffer
      (insert "first = \"Tom\" last = \"Preston-Werner\" # INVALID")
      (tomlparse-buffer))))

(ert-deftest missing-key ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 1 (Parser reported an error)") :occur 1))))
    (with-temp-buffer
      (insert "= \"no key name\"  # INVALID")
      (tomlparse-buffer))))

(ert-deftest referencing-key ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 1 (Parser reported an error)") :occur 1))))
    (with-temp-buffer
      (insert "foo = bar")
      (tomlparse-buffer))))

(ert-deftest inclomplete-pair-after-valid ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 2 (Parser reported an error)") :occur 1))))
    (with-temp-buffer
      (insert "key = 123\nbroken = # INVALID")
      (tomlparse-buffer))))

(ert-deftest conflicting-key-simple ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 4 (Duplicate key `name`)") :occur 1))))
    (with-temp-buffer
      (insert "
# DO NOT DO THIS
name = \"Tom\"
name = \"Pradyun\"
")
      (tomlparse-buffer))))

(ert-deftest conflicting-table-with-previous-key ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 3 (Duplicate key `fruit.apple`)") :occur 1))))
    (with-temp-buffer
      (insert "
fruit.apple = 1
fruit.apple.smooth = true
")
      (tomlparse-buffer))))

(ert-deftest conflicting-table-array-with-previous-table ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 7 (Table `fruit` already defined)") :occur 1))))
    (with-temp-buffer
      (insert "
# INVALID TOML DOC
[fruit.physical]  # subtable, but to which parent element should it belong?
color = \"red\"
shape = \"round\"

[[fruit]]  # parser must throw an error upon discovering that \"fruit\" is
           # an array rather than a table
name = \"apple\"
")
      (tomlparse-buffer))))


(ert-deftest conflicting-table-array-with-previous-pair ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 5 (Duplicate key `fruits`)") :occur 1))))
    (with-temp-buffer
      (insert "
# INVALID TOML DOC
fruits = \"apple\"

[[fruits]] # Not allowed
")
      (tomlparse-buffer))))

(ert-deftest without-super ()
  (with-temp-buffer
    (insert "
# [x] you
# [x.y] don't
# [x.y.z] need these
[x.y.z.w] # for this to work
[x] # defining a super-table afterwards is ok
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))

(ert-deftest table-implicit-and-explicit-after ()
  (with-temp-buffer
    (insert "
[a.b.c]
answer = 42

[a]
better = 43
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest array-implicit-and-explicit-after ()
  (with-temp-buffer
    (insert "
[[a.b]]
answer = 42

[a]
better = 43
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest without-super-with-values ()
  (with-temp-buffer
    (insert "
# [x] you
# [x.y] don't
# [x.y.z] need these
[x.y.z.w] # for this to work
a = 1
b = 2
[x] # defining a super-table afterwards is ok
c = 3
d = 4
")
    (let ((expected (external-toml-parser)))
      (should (hash-equal (tomlparse-buffer) expected)))))


(ert-deftest table-defined-twice ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 7 (Table `fruit` already defined)") :occur 1))))
    (with-temp-buffer
      (insert "
# DO NOT DO THIS

[fruit]
apple = \"red\"

[fruit]
orange = \"orange\"")
      (tomlparse-buffer))))

(ert-deftest super-table-first ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 7 (Table `fruit.apple` already defined)") :occur 1))))
    (with-temp-buffer
      (insert "
# DO NOT DO THIS EITHER

[fruit]
apple = \"red\"

[fruit.apple]
texture = \"smooth\"")
      (tomlparse-buffer))))


(ert-deftest conflicting-table-with-previous-array-of-tables ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 9 (Table `fruits.varieties` already defined)") :occur 1))))
    (with-temp-buffer
      (insert "
[[fruits]]
name = \"apple\"

[[fruits.varieties]]
name = \"red delicious\"

# INVALID: This table conflicts with the previous array of tables
[fruits.varieties]
name = \"granny smith\"
")
      (tomlparse-buffer))))


(ert-deftest conflicting-array-of-table-with-previous-table ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 5 (Duplicate key `fruits`)") :occur 1))))
    (with-temp-buffer
      (insert "
# INVALID TOML DOC
fruits = []

[[fruits]] # Not allowed
")
      (tomlparse-buffer))))



(ert-deftest list-of-known-table-arrays-is-reset-after-success ()
  (with-temp-buffer
    (insert "[[fruits]]")
    (should (tomlparse-buffer)))
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 5 (Duplicate key `fruits`)") :occur 1))))
    (with-temp-buffer
      (insert "
# INVALID TOML DOC
fruits = []

[[fruits]] # Not allowed
")
      (tomlparse-buffer))))


(ert-deftest list-of-known-table-arrays-is-reset-after-failure ()
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 7 (Parser reported an error)") :occur 1))))
    (with-temp-buffer
      (insert "
# INVALID TOML DOC
[[fruits]] # Not allowed
apple = \"sweet\"

[other]
foo = #error
")
      (tomlparse-buffer)))
  (mocker-let ((user-error (msg) ((:input '("Broken toml data: line 4 (Duplicate key `fruits`)") :occur 1))))
    (with-temp-buffer
      (insert "
fruits = []

[[fruits]]
")
      (tomlparse-buffer))))


;;; tomlparse.el-test.el ends here
