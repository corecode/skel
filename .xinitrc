#!/bin/zsh
cd $HOME

exec > .xsession-errors 2>&1

# tell xdg-open that we want to use exo-open
export DESKTOP_SESSION=xfce

if [ -z "$ZSH_VERSION" ] && which zsh 2>/dev/null >&2
then
	exec zsh -l "$0" "$@"
fi

if [ -d /etc/X11/xinit/xinitrc.d ]; then
        for f in /etc/X11/xinit/xinitrc.d/*; do
                [ -x "$f" ] && . "$f"
        done
        unset f
fi

[ -f .Xresources ] && xrdb .Xresources
[ -f .xsession.local ] && . $HOME/.xsession.local
#setxkbmap -symbols 'us(intl)+us(pc101)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'
#setxkbmap -symbols 'us+pc(pc105)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'
#setxkbmap -symbols 'us(intl)+us(pc104)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'

for ring in pkcs11 secrets gpg ssh
do
	# start gnome-keyring-daemon for $ring and set the correct vars
	v=$(gnome-keyring-daemon --start --daemonize --components=$ring)
	# change the commands to export these vars as well
        v=$(echo "$v"|sed -Ee 's/^([^=]*)(.*)/\1\2; export \1;/')
	eval $v
done

gsettings-data-convert &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
start-pulseaudio-x11 &

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
unclutter -noevents &

# sync selection and clipboard
autocutsel -selection PRIMARY -buttonup -fork &

# xscreensaver does blanking and locking
xscreensaver -nosplash &

# enable dpms, disable automatic off (xscreensaver handles this instead)
{
	xset +dpms
	xset dpms 0 0 0
} &

# automatic mount + popup
thunar --daemon &

# go!
exec xmonad
