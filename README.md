[![toml-test](https://github.com/johannes-mueller/tomlparse.el/actions/workflows/toml-test.yaml/badge.svg)](https://github.com/johannes-mueller/tomlparse.el/actions/workflows/toml-test.yaml)
[![Unit
tests](https://github.com/johannes-mueller/tomlparse.el/actions/workflows/unit-tests.yml/badge.svg)](https://github.com/johannes-mueller/tomlparse.el/actions/workflows/unit-tests.yml)
[![melpazoid](https://github.com/johannes-mueller/tomlparse.el/actions/workflows/melpazoid.yml/badge.svg)](https://github.com/johannes-mueller/tomlparse.el/actions/workflows/melpazoid.yml)
# tomlparse.el

A straight forward toml parser fore elisp


## Synopsis

This package aims to provide a straight forward feature complete and stable
parser for the wide spread TOML config language.  The idea is to provide three
elisp functions `(tomlparse-file)`, `(tomlparse-buffer)` and
`(tomlparse-string)` functions, that behave similar to `(json-parse-string)`
and `(json-parse-buffer)` but read TOML rather than JSON.


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
ELISP> (tomlparse-file "pyproject.toml")
#<hash-table equal 2/6 0x1360d8c42522
(("project" . #<hash-table equal 8/24 0x1360d8c41e3c
  (("name" . "tomlparse-demo") ("version" . "0.1.0")
   ("description" . "This is just to demonstrate tomlparse.el")
   ("readme" . "README.md")
   ("authors" . [#<hash-table equal 2/6 0x1360d8dcbb3f
			      (("name" . "Johannes Mueller")
			       ("email"
				. "github@johannes-mueller.org"))
			      >])
   ("requires-python" . ">=3.13.2")
   ("dependencies" . ["pandas>=2.2.3"])
   ("optional-dependencies" . #<hash-table equal 1/6 0x1360d9f91590
    (("jupyter" . ["jupyter>=1.1.1"]))>))
  >)
 ("dependency-groups" . #<hash-table equal 1/6 0x1360d99d52f8
  (("dev" . ["pytest>=8.3.5"]))>))
>
```

If you prefer you can also read the contents into an `alist` or a `plist`.

```
ELISP> (tomlparse-file "pyproject.toml" :object-type 'alist)
((dependency-groups (dev . ["pytest>=8.3.5"]))
 (project (optional-dependencies (jupyter . ["jupyter>=1.1.1"]))
	  (dependencies . ["pandas>=2.2.3"])
	  (requires-python . ">=3.13.2")
	  (authors
	   . [((email . "github@johannes-mueller.org")
	       (name . "Johannes Mueller"))])
	  (readme . "README.md")
	  (description . "This is just to demonstrate tomlparse.el")
	  (version . "0.1.0") (name . "tomlparse-demo")))

ELISP> (tomlparse-file "pyproject.toml" :object-type 'plist)
(dependency-groups (dev ["pytest>=8.3.5"]) project
		   (optional-dependencies (jupyter ["jupyter>=1.1.1"])
					  dependencies
					  ["pandas>=2.2.3"]
					  requires-python ">=3.13.2"
					  authors
					  [(email
					    "github@johannes-mueller.org"
					    name "Johannes Mueller")]
					  readme "README.md"
					  description
					  "This is just to demonstrate tomlparse.el"
					  version "0.1.0" name
					  "tomlparse-demo"))
```

## Why a new TOML parser – isn't there toml.el?

I am aware of toml.el aka [emacs-toml](https://github.com/gongo/emacs-toml).
I've been using it and I even have contributed to it.  However, the package is
not feature complete, so it cannot parse all valid TOML files and it seems to
suffer from maintainer fatigue.  Moreover, now that we have
[Tree-sitter](https://tree-sitter.github.io/tree-sitter/) available in Emacs
29+ we can write parsers way more easily with less lines of code. (Actually the
name "tomlparse.el" is technically not correct, as the parsing is done by
Tree-sitter.)


## Status

The thing seems to be feature complete (see [Limitations](#limitations)).

The read functions read all the example of the [TOML
spec](https://toml.io/en/v1.0.0) correctly.  Furthermore, all valid test files
for TOML-1.0.0 in [toml-test](https://github.com/toml-lang/toml-test) are
correctly read in.  There are some issues with string escaping for emacs<30.1,
though.


## Installation

At the moment the most convenient method to install it is using
[straight.el](https://github.com/raxod502/straight.el). Put the following lines
into your startup file.

``` elisp
(use-package tomlparse
  :straight (tomlparse :type git :host github :repo "johannes-mueller/tomlparse.el")
  :init
  (add-to-list 'treesit-language-source-alist
               '(toml "https://github.com/tree-sitter-grammars/tree-sitter-toml"))
  (unless (treesit-language-available-p 'toml)
    (treesit-install-language-grammar 'toml)))
```

This also installs the TOML language grammar if it is not already installed.


## Limitations

So far, the [TOML tree-sitter
gramar](https://github.com/tree-sitter-grammars/tree-sitter-toml) only supports
TOML 1.0.0. So all the things only allowed in TOML 1.1.x are not supported by
this package.  They will be supported as soon as the Tree-sitter grammar
supports them.

In principle the package works with all Emacs versions that have Tree-sitter
built in.  There are however, issues with string escaping in Emacs<30.1, due to
some changes in the function `json-parse-string`.  As long as you don't have
strings with escape sequences in them, also Emacs=29.1 is fine.


## Future plans

None.  Except maybe fixing the [limitations](#limitations).  I will fix bugs,
of course.  Otherwise we will see, if there will be a new version of the TOML
language that becomes relevant.


## Contributing

As usual, issues and pull requests are welcome.
