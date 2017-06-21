#!/bin/bash

CC="/cygdrive/e/c/dcc/dcc/bin/dcc.exe"
EXE_TEMP="a.exe"

run_one() {
	echo "Testing: '$1'"
	$CC -g -o "$EXE_TEMP" "$1" || exit $?
	"./$EXE_TEMP" || exit $?
}

run_test() {
	for src in $*; do
		run_one "$src"
	done
}

run_test *.c

rm -f "$EXE_TEMP"

