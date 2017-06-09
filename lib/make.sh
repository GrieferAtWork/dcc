#!/bin/bash

CC=dcc
CC="/cygdrive/e/c/dcc/dcc/bin/dcc.exe"
DEPDIR="./.deps"

src_changed() {
	inf="$1"
	dpf="$2"
	[ -f "$inf" ] || return 1
	[ -f "$dpf" ] || return 1
	ddt=`cat "$dpf" | tr -d '\\\\\n'`
	first=1
	for dep in $ddt; do
		if [ "$first" == 1 ]; then first=0; else
			if [ "$dep" -nt "$inf" ]; then
				echo    "Dependency has changed:"
				echo -e "\tinput file: $inf"
				echo -e "\tdepends on: $dep"
				return 1
			fi
		fi
	done
	return 0
}

obj() {
	local objfile
	local args
	args=()
	while [[ $1 == "-"* ]]; do
		args+=("$1")
		shift
	done
	objfile="$1"
	shift
	depfile="$DEPDIR/$objfile.d"
	if ! src_changed "$objfile" "$depfile"; then
		echo "Compiling: $objfile"
		$CC "${args[@]}" -DNDEBUG -nostdlib -c -MMD -MF "$depfile" -o "$objfile" $*
	fi
	objfile="dbg-$objfile"
	depfile="$DEPDIR/$objfile.d"
	if ! src_changed "$objfile" "$depfile"; then
		echo "Compiling: $objfile"
		$CC "${args[@]}" -g -nostdlib -c -MMD -MF "$depfile" -o "$objfile" $*
	fi
}

mkdir -p "$DEPDIR" || exit $?

obj "crt1.o"   "src/crt1.c"
obj "int64.o"  "src/int64.c"
obj -DDCC_BUILDING_A2L_RUNTIME "addr2line.o" "src/addr2line.c" "src/addr2line-common.c"








