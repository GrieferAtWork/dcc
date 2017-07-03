#!/bin/bash

CC="/cygdrive/e/c/dcc/dcc/bin/dcc.exe"
EXE_TEMP="a.exe"
ALLOW_DRT=$($CC --has-feature drt 2>/dev/null)

run_one() {
	echo "Testing: '$1'"
	if [ "$ALLOW_DRT" == "1" ]; then
		# Use DRT execution to prevent temporary files (Plus: it makes )
		$CC -dg "$1" || exit $?
	else
		$CC -g -o "$EXE_TEMP" "$1" || exit $?
		"./$EXE_TEMP" || {
			E=$?
			echo "ERROR: $E"
			exit $E
		}
	fi
}

run_test() {
	for src in $*; do
		run_one "$src"
	done
}

run_test *.c

rm -f "$EXE_TEMP"

