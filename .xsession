. ~/.environment

xmodmap .xmodmaprc &
xrdb ~/.Xresources
xautolock -time 30 -locker "i3lock -c 330033 -d" &
xss-lock -v -- i3lock -c 330033 || true &


i3 &
wmpid=$!

sleep 1

xset dpms 3600 3700 3800
xset s 3600
xset m 1 1

pulseaudio --daemonize

arbtt-capture --sample-rate=30 &

setxkbmap -layout us,ru -variant ,phonetic -option grp:menu_toggle,grp_led:caps,terminate:ctrl_alt_bksp,ctrl:nocaps,compose:ralt

case $(hostname) in
    mayo)
        # trackpoint scrolling according to
        # http://www.thinkwiki.org/wiki/How_to_configure_the_TrackPoint
        # vertical scrolling
        xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 8 1
        xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 8 2
        #xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 8 200
        # horizontal scrolling
        xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 8 6 7 4 5

        xgamma -rgamma 1.2 -bgamma 0.8
        ;;
esac

wait $wmpid

