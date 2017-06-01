#!/bin/bash

CC="gcc"
F=("-Iinclude" "-DDCC_PRIVATE_API")

CC_DCC="/cygdrive/e/c/dcc/dcc/bin/dcc.exe"
if [ -f "$CC_DCC" ]; then
	CC="$CC_DCC";
	F+=("-D__GNUC__=2")
	F+=("-D__GNUC_MINOR__=0")
	F+=("-D__GNUC_PATCH__=0")
	F+=("-I D:\\cygwin\\root\\usr\\include\\w32api")
fi

build() { echo "build/dcc/$1"; }
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

$CC -o bin/dcc ${object_list[@]}

















