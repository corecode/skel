#!/bin/sh

setxkbmap -symbols 'pc(pc105)+us(altgr-intl)+ctrl(nocaps)+inet(evdev)'

xset r rate 400 30

# turn favorites into play
xmodmap -e 'keycode 164 = XF86AudioPlay'
# turn Lenovo usb keyboard Fn-F12 "explorer" into play
xmodmap -e 'keycode 152 = XF86AudioPlay'
