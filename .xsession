#!/bin/sh
cd $HOME
[ -f .Xresources ] && xrdb .Xresources
setxkbmap -symbols 'us(intl)+us(pc101)+ctrl(nocaps)+altwin(meta_win)+group(rwin_switch)'
exec wmaker
