#!/bin/bash

CC=dcc
CC="/cygdrive/e/c/dcc/dcc/bin/dcc.exe"

$CC -nostdlib -c -o crt1.o src/crt1.c
$CC -nostdlib -c -o int64.o src/int64.c
$CC -nostdlib -c -o addr2line.o src/addr2line.c
