#!/bin/bash

toml_file=$1
json_file=${1%%toml}json

compare_toml_json () {
    local toml_file=$1
    local json_file=${1%%toml}json
    emacs -Q -batch  -l tomlparse.el -l test/tomltest.el --eval "(compare \"$toml_file\" \"$json_file\")"
}

test_toml-1.0 () {
    tmpdir=$(mktemp -d)
    while read -r file
    do
	echo -n testing "$file"
	compare_toml_json "toml-test/tests/${file}" || ((failure++))
	echo " ✔"
    done < <(grep "^valid.*toml$" toml-test/tests/files-toml-1.0.0)
    rm -rf "${tmpdir}"
}

failure=0

if [[ $toml_file ]]
then
    cat "$toml_file"
    cat "$json_file"
    compare_toml_json "$toml_file" || exit 1
else
    test_toml-1.0
fi

exit $failure
