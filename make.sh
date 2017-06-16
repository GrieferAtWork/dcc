#!/bin/bash

CC="gcc"
F=("-g" "-Iinclude" "-DDCC_PRIVATE_API")
out_bin="bin/dcc"
obj_path="build/dcc"

CC_DCC="/cygdrive/e/c/dcc/dcc/bin/dcc.exe"
CC_DDC="/cygdrive/e/c/dcc/dcc/bin/ddc.exe"
if [ -f "$CC_DDC" ]; then
	CC="$CC_DDC";
	obj_path="build/ddd"
	out_bin="bin/ddd.exe"
	F+=("-DDCC_PRIVATE_API")
	F+=("-D_VA_LIST_DEFINED")
	F+=("-D__SSE2__")
	F+=("-ID:/cygwin32/usr/include/w32api")
elif [ -f "$CC_DCC" ]; then
	CC="$CC_DCC";
	obj_path="build/ddc"
	out_bin="bin/ddc.exe"
	F+=("-DDCC_PRIVATE_API")
	F+=("-D_VA_LIST_DEFINED")
	F+=("-D__SSE2__")
	F+=("-ID:/cygwin32/usr/include/w32api")
fi

build() { echo "$obj_path/$1"; }
out() { echo "$(build $1).o"; }
dep() { echo "$(build $1).d"; }

object_list=()
src_changed() {
	inf="$1"
	ouf="$(out $(basename "$inf"))"
	dpf="$(dep $(basename "$inf"))"
	[ -f "$ouf" ] || return 1
	[ -f "$dpf" ] || return 1
	ddt=`cat "$dpf" | tr -d '\\\\\n'`
	first=1
	for dep in $ddt; do
		if [ "$first" == 1 ]; then first=0; else
			if [ "$dep" -nt "$ouf" ]; then
				echo    "Dependency has changed:"
				echo -e "\tinput file: $inf"
				echo -e "\tdepends on: $dep"
				return 1
			fi
		fi
	done
	return 0
}
src() {
	for inf in $*; do
		ouf="$(out $(basename "$inf"))"
		object_list+=("$ouf")
		if ! src_changed "$inf"; then
			dpf="$(dep $(basename "$inf"))"
			echo "Compiling: '$inf'"
			echo $CC "${F[@]}" -MMD -MF "$dpf" -c -o "$ouf" "$inf" || exit $?
			$CC "${F[@]}" -MMD -MF "$dpf" -c -o "$ouf" "$inf" || exit $?
		else
			echo "Unchanged: '$inf'"
		fi
	done
}

mkdir -p $(build "") || exit $?

# Compile DCC source files
src src/*.c
src src/dcc/*.c
src lib/src/addr2line-common.c

echo $CC -g -o "$out_bin" "${object_list[@]}"
$CC -g -o "$out_bin" "${object_list[@]}"






# -g -o ../bin/ddc.exe ../build/dcc/main.c.o ../build/dcc/addr2line.c.o ../build/dcc/assembler.c.o ../build/dcc/cmd-help.c.o ../build/dcc/cmd.c.o ../build/dcc/common.c.o ../build/dcc/compiler.c.o ../build/dcc/fundecl.c.o ../build/dcc/gen.c.o ../build/dcc/lexer.c.o ../build/dcc/linker.c.o ../build/dcc/preprocessor.c.o ../build/dcc/tpp-wrapper.c.o ../build/dcc/type.c.o ../build/dcc/unit-debug.c.o ../build/dcc/unit-export.c.o ../build/dcc/unit-import.c.o ../build/dcc/unit-merge.c.o ../build/dcc/unit.c.o ../build/dcc/vstack-ext.c.o ../build/dcc/vstack.c.o ../build/dcc/x86_util-instrlen.c.o ../build/dcc/addr2line-common.c.o
# -g -I../include -DDCC_PRIVATE_API -DDCC_PRIVATE_API -D_VA_LIST_DEFINED -D__SSE2__ -ID:/cygwin32/usr/include/w32api -MMD -MF ../build/dcc/main.c.d -c -o ../build/dcc/main.c.o ../src/main.c









