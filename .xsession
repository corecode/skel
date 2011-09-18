#!/bin/sh
cd $HOME
[ -f .Xresources ] && xrdb .Xresources
[ -f .xsession.local ] && . $HOME/.xsession.local
#setxkbmap -symbols 'us(intl)+us(pc101)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'
#setxkbmap -symbols 'us+pc(pc105)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'
#setxkbmap -symbols 'us(intl)+us(pc104)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'
setxkbmap -symbols 'pc(pc105)+us(altgr-intl)+ctrl(nocaps)+terminate(ctrl_alt_bksp)' &

xset r rate 400 30 &

# disable touchpad
{
	touchpad=$(xinput list | grep "SynPS/2 Synaptics TouchPad")
	case "$touchpad" in
	"") ;;
	*)
	tpid=$(echo "$touchpad" | sed -e 's/.*id=\([[:digit:]]*\).*/\1/')
		xinput set-int-prop "$tpid" "Synaptics Off" 8 1
		;;
	esac
} &

# bell off
xset b off &

# enable dpms, no standby, no suspend, off after 5 min
xset +dpms &
xset dpms 0 0 300

# background and default cursor
xsetroot -solid "#504050" -cursor_name left_ptr &

# go!
exec xmonad
