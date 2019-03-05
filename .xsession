xrdb ~/.Xresources

xset s 1200
xset dpms 1800 1800 1800
xset m 1 1
xset r off

xss-lock -v -- i3lock -c 330033 &

i3 &
wmpid=$!

sleep 1


pulseaudio --daemonize

arbtt-capture --sample-rate=30 &

setxkbmap -layout us,ru -variant ,phonetic -option grp:menu_toggle,grp_led:caps,terminate:ctrl_alt_bksp,ctrl:nocaps,compose:ralt

case $(hostname) in
    laptop)
        xinput set-prop "TPPS/2 IBM TrackPoint" "libinput Accel Speed" 1
        xrandr --output LVDS-1 --gamma 1.2:1:0.8
        ;;
esac

wait $wmpid

