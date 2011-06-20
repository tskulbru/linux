#!/bin/sh

# Setup keychain
eval `keychain --eval --agents ssh id_dsa`

# Programs to launch at startup
xsetroot -cursor_name left_ptr
sh ~/.fehbg &

#/home/serrghi/.xmonad/dzen/clock &
#stalonetray &
#nm-applet &
# Programs which will run after Xmonad has started
(sleep 2 && stalonetray) &
#(sleep 2 && nm-applet) &
(sleep 2 && dropboxd) &
#(sleep 3 && conky) &

