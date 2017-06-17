#!/bin/bash

CC=dcc
CC="/cygdrive/e/c/dcc/dcc/bin/dcc.exe"
DEPDIR=".deps"
OBJDIR=".objs"

src_changed() {
	return 1
#	inf="$1"
#	dpf="$2"
#	[ -f "$inf" ] || return 1
#	[ -f "$dpf" ] || return 1
#	ddt=`cat "$dpf" | tr -d '\\\\\n'`
#	first=1
#	for dep in $ddt; do
#		if [ "$first" == 1 ]; then first=0; else
#			if [ "$dep" -nt "$inf" ]; then
#				echo    "Dependency has changed:"
#				echo -e "\tinput file: $inf"
#				echo -e "\tdepends on: $dep"
#				return 1
#			fi
#		fi
#	done
#	return 0
}

debug_objects=()
ndebug_objects=()

src() {
	local objfile
	local args
	local mode
	mode="$1"
	shift
	args=()
	while [[ $1 == "-"* ]]; do
		args+=("$1")
		shift
	done
	name="$(basename "$1")"
	if [[ "$mode" == *"D"* ]]; then
		objfile="$OBJDIR/dbg-$name.o"
		debug_objects+=("$objfile")
		depfile="$DEPDIR/dbg-$name.d"
		if ! src_changed "$objfile" "$depfile"; then
			echo "Compiling: $objfile"
			$CC "${args[@]}" -g -nostdlib -c -MMD -MF "$depfile" -o "$objfile" $* || exit $?
		fi
	fi
	if [[ "$mode" == *"N"* ]]; then
		objfile="$OBJDIR/ndbg-$name.o"
		ndebug_objects+=("$objfile")
		depfile="$DEPDIR/ndbg-$name.d"
		if ! src_changed "$objfile" "$depfile"; then
			echo "Compiling: $objfile"
			$CC "${args[@]}" -DNDEBUG -nostdlib -c -MMD -MF "$depfile" -o "$objfile" $* || exit $?
		fi
	fi
}

crt-src-c() { src "DN" $*; }
crt-src-n() { src "N" $*; }
crt-src-d() { src "D" $*; }

mkdir -p "$DEPDIR" || exit $?
mkdir -p "$OBJDIR" || exit $?

crt-src-c                            "src/crt/crt1.c"
crt-src-c                            "src/crt/int64.c"
crt-src-d -DDCC_BUILDING_A2L_RUNTIME "src/crt/addr2line.c" "src/a2l/addr2line-common.c"
crt-src-c                            "src/crt/chkstk.S"
crt-src-c                            "src/crt/alloca.S"

$CC -g -c -o dbg-crt.o ${debug_objects[@]}
$CC -c -o crt.o ${ndebug_objects[@]}

# -DDCC_BUILDING_A2L_RUNTIME -g -nostdlib -c -MMD -MF .deps/dbg-addr2line.c.d -o .objs/dbg-addr2line.c.o src/crt/addr2line.c src/a2l/addr2line-common.c



