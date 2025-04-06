#!/bin/bash

emacs -Q -batch  -l tomlparse.el -l test/tomltest.el --eval "(toml2json \"$1\")"
