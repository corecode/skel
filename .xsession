#!/bin/sh
cd $HOME
[ -f .Xresources ] && xrdb .Xresources
[ -f .xsession.local ] && . $HOME/.xsession.local
#setxkbmap -symbols 'us(intl)+us(pc101)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'
#setxkbmap -symbols 'us+pc(pc105)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'
#setxkbmap -symbols 'us(intl)+us(pc104)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'

./bin/init_keyboard.sh &

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

# background and default cursor
xsetroot -solid "#504050" -cursor_name left_ptr &

# for transparency
xcompmgr &

# drop cursor if not needed
unclutter &

# xscreensaver does blanking and locking
xscreensaver -nosplash &

# enable dpms, disable automatic off (xscreensaver handles this instead)
xset +dpms &
xset dpms 0 0 0 &

# go!
exec xmonad
