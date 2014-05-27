. ~/.environment

xmodmap .xmodmaprc &
xrdb ~/.Xresources
xautolock -time 30 -locker "i3lock -c 330033 -d" &
xss-lock -v -- i3lock -c 330033 &

#eval `gpg-agent --daemon`

i3 &
wmpid=$!

sleep 1

xset dpms 3600 3700 3800
xset s 3600
xset m 1 1
#wmname compiz

#parcellite &
#blueman-applet &
pulseaudio --daemonize
#padevchooser &

setxkbmap -variant -option
setxkbmap -layout us,ru -variant ,phonetic -option grp:toggle,grp_led:caps,terminate:ctrl_alt_bksp,ctrl:nocaps,compose:menu

case $(hostname) in
    slk)
        if xrandr | grep "VGA1 connected" >/dev/null 2>&1; then
            xrandr --output LVDS1 --off --output VGA1 --auto
        fi
        ;;
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
    britany)
        xrandr --setprovideroutputsource 1 0
        xrandr --output DVI-1-2 --auto --output DVI-1-2 --left-of DVI-0
        xrandr --output DVI-0 --rotate left --output DVI-1 --rotate left --output DVI-1 --right-of DVI-0
        ;;
esac

wait $wmpid

