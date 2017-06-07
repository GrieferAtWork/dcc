#!/bin/bash

CC=dcc
CC="/cygdrive/e/c/dcc/dcc/bin/dcc.exe"

$CC -nostdlib -c -o crt1.o src/crt1.c
$CC -nostdlib -c -o int64.o src/int64.c
$CC -nostdlib -DDCC_BUILDING_A2L_RUNTIME -c -o addr2line.o \
	src/addr2line.c src/addr2line-common.c \
	src/addr2line-tracebacks.S
