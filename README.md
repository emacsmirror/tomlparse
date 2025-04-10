# tomlparse.el

A straight forward toml parser fore elisp


## Synopsis

This package aims to provide a straight forward feature complete and stable
parser for the wide spread toml config language.  The idea is to provide three
elisp functions `(tomlparse-file)`, `(tomlparse-buffer)` and
`(tomlparse-string)` functions, that behave similar to `(json-parse-string)`
and `(json-parse-buffer)` but read toml rather than json.


## Example

Suppose you want to read the contents of the file `pyproject.toml`:

```toml filename="pyproject.toml"
[project]
name = "tomlparse-demo"
version = "0.1.0"
description = "This is just to demonstrate tomlparse.el"
readme = "README.md"
authors = [
    { name = "Johannes Mueller", email = "github@johannes-mueller.org" }
]
requires-python = ">=3.13.2"
dependencies = [
    "pandas>=2.2.3",
]

[project.optional-dependencies]
jupyter = [
    "jupyter>=1.1.1",
]

[dependency-groups]
dev = [
    "pytest>=8.3.5",
]
```

You can now read the contents of your `pyproject.toml` into a hash-table and
access its items.

```
*** Welcome to IELM ***  Type (describe-mode) or press C-h m for help.
ELISP> (setq projectfile.toml-data (tomlparse-file "pyproject.toml"))
#<hash-table equal 2/6 0x1e60acea9062 ...>

ELISP> (gethash "name" (gethash "project" projectfile.toml-data))
"tomlparse-demo"

ELISP> (gethash "dependencies" (gethash "project" projectfile.toml-data))
["pandas>=2.2.3"]
```

If you prefer you can also read the contents into an `alist` or a `plist`.

```
ELISP> (tomlparse-file "pyproject.toml" :object-type 'alist)
(("dependency-groups" ("dev" . ["pytest>=8.3.5"]))
 ("project" ("optional-dependencies" ("jupyter" . ["jupyter>=1.1.1"]))
  ("dependencies" . ["pandas>=2.2.3"])
  ("requires-python" . ">=3.13.2")
  ("authors"
   . [(("email" . "github@johannes-mueller.org")
       ("name" . "Johannes Mueller"))])
  ("readme" . "README.md")
  ("description" . "This is just to demonstrate tomlparse.el")
  ("version" . "0.1.0") ("name" . "tomlparse-demo")))

ELISP> (tomlparse-file "pyproject.toml" :object-type 'plist)
("dependency-groups" ("dev" ["pytest>=8.3.5"]) "project"
 ("optional-dependencies" ("jupyter" ["jupyter>=1.1.1"])
  "dependencies" ["pandas>=2.2.3"] "requires-python" ">=3.13.2"
  "authors"
  [("email" "github@johannes-mueller.org" "name" "Johannes Mueller")]
  "readme" "README.md" "description"
  "This is just to demonstrate tomlparse.el" "version" "0.1.0" "name"
  "tomlparse-demo"))
```

## Why a new toml parser – isn't there toml.el?

I am aware of toml.el aka [emacs-toml](https://github.com/gongo/emacs-toml) and
I've been using it and I even have contributed to it.  However, the package is
not feature complete, so it cannot parse all valid toml files and it seems to
suffer from maintainer fatigue.  Moreover, now that we have
[Tree-sitter](https://tree-sitter.github.io/tree-sitter/) available in Emacs
29+ we can write parsers way more easy with way less lines of code. (Actually
the name "tomlparse.el" is technically not correct, as the parsing is done by
Tree-sitter.)


## Status

The thing seems to be feature complete (see [Limitations](#limitations)).

After some practice test I will propose it to MELPa.  Be aware that this has
been written during three evenings after work and a couple of hours during the
following weekend..  So there can be issues.  Please report them.


## Installation

At the moment the most convenient method to install it is using
[straight.el](https://github.com/raxod502/straight.el). Put the following lines
into your startup file.

``` elisp
(use-package tomlparse
  :straight (tomlparse :type git :host github :repo "johannes-mueller/tomlparse.el"))

(add-to-list 'treesit-language-source-alist '(toml "https://github.com/tree-sitter-grammars/tree-sitter-toml"))
```

Then you can install the tree-sitter TOML gramar using
`treesit-install-language-grammar`.


## Limitations

So far, the [TOML tree-sitter
gramar](https://github.com/tree-sitter-grammars/tree-sitter-toml) only supports
TOML 1.0.0. So all the things only allowed in TOML 1.1.x are not supported by
this package.  They will be supported as soon as the tree-sitter grammar
supports them.

The read functions read all the example of the [toml
spec](https://toml.io/en/v1.0.0) correctly.  There are a couple of valid test
files in [toml-test](https://github.com/toml-lang/toml-test) that are no
correctly read in or not read in at all.  These are either TOML 1.1.x features
or issues with escape sequences in string literals of the TOML data.  The
latter is especially relevant for Emacs versions < 30.1.


## Future plans

None.  Except maybe fixing the [limitations](#limitations).  I will fix bugs,
of course.  Otherwise we will see, if there will be a new version of the toml
language that becomes relevant.


## Contributing

As usual, issues and pull requests are welcome.
