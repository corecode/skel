#!/bin/sh
#
# This script enables connected screens.

. /etc/acpi/power-funcs

for x in /tmp/.X11-unix/*; do
	displaynum=`echo $x | sed s#/tmp/.X11-unix/X##`
	getXconsole;
	if [ x"$XAUTHORITY" != x"" ]; then
	    export DISPLAY=":$displaynum"
	    args="--output LVDS1 --auto"
	    xrandr_o=$(/usr/bin/xrandr -q --nograb)
	    for out in $(echo "$xrandr_o" | awk '$2 == "disconnected" && $1 != "LVDS1" { print $1 }')
	    do
		args="$args --output $out --off"
	    done

	    /usr/bin/xrandr $args

	    for out in $(echo "$xrandr_o" | awk '$2 == "connected" && $1 != "LVDS1" { print $1 }')
	    do
		args="$args --output $out --auto --left-of LVDS1"
	    done

	    /usr/bin/xrandr $args
	fi
done
