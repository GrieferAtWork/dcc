#!/bin/bash

CC=dcc
CC="/cygdrive/e/c/dcc/dcc/bin/dcc.exe"

DEPDIR="./.deps"

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
	depfile=""
	
	$CC -nostdlib -c -o objfile $*
}

mkdir -p "$DEPDIR" || exit $?


$CC -nostdlib -c -o crt1.o src/crt1.c
$CC -nostdlib -c -o int64.o src/int64.c
$CC -nostdlib -DDCC_BUILDING_A2L_RUNTIME -c -o addr2line.o \
 src/addr2line.c src/addr2line-common.c
