#!/bin/bash

compare_toml_json () {
    toml_file=$1
    json_file=${1%%toml}json
    emacs -Q -batch  -l tomlparse.el -l test/tomltest.el --eval "(compare \"$toml_file\" \"$json_file\")"
}

test_toml-1.0 () {
    tmpdir=$(mktemp -d)
    while read -r file
    do
	echo -n testing "$file"
	compare_toml_json toml-test/tests/${file} || ((failure++))
	echo " ✔"
    done < <(grep "^valid.*toml$" toml-test/tests/files-toml-1.0.0)
    rm -rf ${tmpdir}
}

failure=0

if [[ $1 ]]
then
    cat $1
    cat ${1%%toml}json
    compare_toml_json $1 || exit 1
else
    test_toml-1.0
fi

exit $failure
