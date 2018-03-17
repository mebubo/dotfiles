xrdb ~/.Xresources

xset s 1200
xset dpms 1800 1800 1800
xset m 1 1
xset r rate 200 60

xss-lock -v -- i3lock -c 330033 &

xmonad &
wmpid=$!

sleep 1


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

        xrandr --output LVDS-1 --gamma 1.2:1:0.8
        ;;
esac

wait $wmpid

