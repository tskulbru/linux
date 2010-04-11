# Run the system-wide support stuff
. $GLOBALAUTOSTART

# Programs to launch at startup
#nitrogen --restore &
#xcompmgr -cC -t-3 -l-5 -r5 -I0.02 -O0.03 -D4 -fF &
sh ~/.fehbg & 
emesene &
urxvtd -q -f -o &
setxkbmap -option terminate:ctrl_alt_bksp &
#gmixer -d
#wicd-client &
#floamtv.py -D
#bluetooth-applet &
#dropboxd &


# Programs that will run after Openbox has started
#(sleep 5 && bmpanel transpy) 
(sleep 2 && conky) &
(sleep 2 && tint2) &
#(sleep 7 && batterymon) &
#(sleep 2 && bluetooth-applet) &
#(sleep 5 && bmpanel transpy) &
#(sleep 7 && conky -c ~/.conky-weather) &
#(sleep 7 && sudo wlassistant)
