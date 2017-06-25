#!/bin/bash

CC="gcc -pthread"
F=("-g" "-Iinclude" "-DDCC_PRIVATE_API")
LF=""
out_bin="bin/dcc"
obj_path="build/dcc"

CC_DCC="/cygdrive/e/c/dcc/dcc/bin/dcc.exe"
CC_DDC="/cygdrive/e/c/dcc/dcc/bin/ddc.exe"
if [ -f "$CC_DDC" ]; then
	CC="$CC_DDC";
	obj_path="build/ddd"
	out_bin="bin/ddd.exe"
	F+=("-D_VA_LIST_DEFINED")
	F+=("-D__SSE2__")
	F+=("-ID:/cygwin32/usr/include/w32api")
elif [ -f "$CC_DCC" ]; then
	CC="$CC_DCC";
	obj_path="build/ddc"
	out_bin="bin/ddc.exe"
	F+=("-D_VA_LIST_DEFINED")
	F+=("-D__SSE2__")
	F+=("-ID:/cygwin32/usr/include/w32api")
else
	LF="$LF -ldl"
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
src src/drt/*.c
src lib/src/a2l/addr2line-common.c

echo $CC -g -o "$out_bin" "${object_list[@]}" $LF
$CC -g -o "$out_bin" "${object_list[@]}" $LF






# -g -o ../bin/ddc.exe ../build/ddc/main.c.o ../build/ddc/addr2line.c.o ../build/ddc/assembler.c.o ../build/ddc/cmd-help.c.o ../build/ddc/cmd.c.o ../build/ddc/common.c.o ../build/ddc/compiler.c.o ../build/ddc/fundecl.c.o ../build/ddc/gen.c.o ../build/ddc/lexer.c.o ../build/ddc/linker.c.o ../build/ddc/preprocessor.c.o ../build/ddc/tpp-wrapper.c.o ../build/ddc/type.c.o ../build/ddc/unit-debug.c.o ../build/ddc/unit-export.c.o ../build/ddc/unit-import.c.o ../build/ddc/unit-merge.c.o ../build/ddc/unit.c.o ../build/ddc/vstack-ext.c.o ../build/ddc/vstack.c.o ../build/ddc/x86_util-instrlen.c.o ../build/ddc/addr2line-common.c.o
# -g -I../include -DDCC_PRIVATE_API -DDCC_PRIVATE_API -D_VA_LIST_DEFINED -D__SSE2__ -ID:/cygwin32/usr/include/w32api -MMD -MF ../build/ddc/assembler.c.d -c -o ../build/ddc/assembler.c.o ../src/dcc/assembler.c
# -g -I../include -DDCC_PRIVATE_API -DDCC_PRIVATE_API -D_VA_LIST_DEFINED -D__SSE2__ -ID:/cygwin32/usr/include/w32api -MMD -MF ../build/ddc/main.c.d -c -o ../build/ddc/main.c.o ../src/main.c
# -g -o bin/ddc.exe build/ddc/main.c.o build/ddc/addr2line.c.o build/ddc/assembler.c.o build/ddc/cmd-help.c.o build/ddc/cmd.c.o build/ddc/common.c.o build/ddc/compiler.c.o build/ddc/fundecl.c.o build/ddc/gen.c.o build/ddc/lexer.c.o build/ddc/linker.c.o build/ddc/preprocessor.c.o build/ddc/tpp-wrapper.c.o build/ddc/type.c.o build/ddc/unit-debug.c.o build/ddc/unit-export.c.o build/ddc/unit-import.c.o build/ddc/unit-merge.c.o build/ddc/unit.c.o build/ddc/vstack-ext.c.o build/ddc/vstack.c.o build/ddc/x86_util-instrlen.c.o build/ddc/addr2line-common.c.o









