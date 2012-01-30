. ~/.environment

if [ `hostname` = "slk" ] && xrandr | grep "VGA1 connected"; then
    xrandr --output LVDS1 --off --output VGA1 --auto
fi

xmodmap .xmodmaprc &
xautolock -time 30 -locker "i3lock -c 330033 -b -d" &

#eval `gpg-agent --daemon`

awesome &
wmpid=$!

sleep 1

if [ `hostname` = "mebub" ]; then
    xgamma -rgamma 1.2 -bgamma 0.8
fi

xset dpms 3600 3700 3800
xset s 3600
xset m 1 1
wmname compiz

#parcellite &
#blueman-applet &
#pulseaudio --daemonize
#padevchooser &

setxkbmap -variant -option
setxkbmap -layout us,ru -variant ,phonetic -option grp:toggle,grp_led:caps,terminate:ctrl_alt_bksp,ctrl:nocaps,compose:rwin

# trackpoint scrolling according to http://www.thinkwiki.org/wiki/How_to_configure_the_TrackPoint
if [ `hostname` = "mebub" ]; then
    # vertical scrolling
    xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 8 1
    xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 8 2
    #xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 8 200
    # horizontal scrolling
    xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 8 6 7 4 5
fi

wait $wmpid

