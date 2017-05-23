#!/bin/bash

CC="/cygdrive/e/c/dcc/dcc/bin/dcc.exe"
F="-ID:\\VisualStudio\\VistualStudio2013\\VC\\include -Iinclude -D_MSC_VER -DDCC_PRIVATE_API"
out() {
	echo "build/dcc/$1"
}
src() {
	$CC $F -c -o $(out $(basename "$1")) $1
}

mkdir $(out "")

src "src/main.c"



