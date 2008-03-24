#!/bin/sh
cd $HOME
[ -f .Xresources ] && xrdb .Xresources
[ -f .xsession.local ] && . $HOME/.xsession.local
#setxkbmap -symbols 'us(intl)+us(pc101)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'
#setxkbmap -symbols 'us+pc(pc104)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'
setxkbmap -symbols 'us(intl)+us(pc104)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'
exec wmaker
