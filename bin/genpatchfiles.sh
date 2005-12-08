#!/bin/sh

if [ $# -ne 1 -o \! -d "$1" ]
then
	echo "usage: `basename $0` <destdir>" >&2
	exit 1
fi

DESTDIR="$1"

find . -name '*.orig' | while read n
do
	[ -e "${n%.orig}" ] || continue

	dir="${n%/*}"
	origdir="$dir"
	name="${n##*/}"
	name="${name%.orig}"
	patchname="$name.patch"

	until [ -d "$DESTDIR/$dir" ]
	do
		subdir="${dir##*/}"
		dir="${dir%/$subdir}"
		patchname="$subdir,$patchname"
	done

	echo "$origdir/$name" >&2
	( cd "$origdir" && echo '$DragonFly$' && diff -u "$name".orig "$name" )  > "$DESTDIR/$dir/$patchname"
done
