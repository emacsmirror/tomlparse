# tomlparse.el

A straight forward toml parser fore elisp


## Synopsis

This package aims to provide a straight forward feature complete and stable
parser for the wide spread toml config language.  The idea is to provide three
elisp functions `(tomlparse-file)`, `(tomlparse-buffer)` and
`(tomlparse-string)` functions, that behave similar to `(json-parse-string)`
and `(json-parse-buffer)` but read toml rather than json.


## Why a new toml parser – isn't there toml.el

I am aware of toml.el aka [emacs-toml](https://github.com/gongo/emacs-toml) and
I've been using it and I even have contributed to it.  However, the package is
not feature complete, so it cannot parse all valid toml files.  The maintainer
does not seem to be interested anymore.  Moreover, now that we have
[Tree-sitter](https://tree-sitter.github.io/tree-sitter/) available in Emacs
29+ we can write parsers way more easy with way less lines of code. (Actually
the name "tomlparse.el" is technically not correct, as the parsing is done by
Tree-sitter.)


## Status

The thing seems to be feature complete, reads all the example of the [toml
spec](https://toml.io/en/v1.0.0) correctly.  After some practice test I will
propose it to MELPa.  Be aware that this has been written during three evenings
after work.  So there can be issues. Please repolrt them


## Installation

At the moment the most convenient method to install it is using
[straight.el](https://github.com/raxod502/straight.el). Put the following lines
into your startup file.

``` elisp
(use-package tomlparse
  :straight (tomlparse :type git :host github :repo "johannes-mueller/tomlparse.el"))
```


## Future plans

None.  I will fix bugs, of course.  Otherwise we will see, if there will be a
new version of the toml language that becomes relevant.



## Contributing

As usual, issues and pull requests are welcome.
