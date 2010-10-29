#. ~/.environment

xmodmap .xmodmaprc &
xautolock -time 30 -locker xlock &

#eval `gpg-agent --daemon`

awesome &
wmpid=$!

sleep 1

#xgamma -rgamma 1.2 -bgamma 0.8

xset dpms 3600 3700 3800
xset s 3600
xset m 1 1
wmname compiz

parcellite &
#blueman-applet &
#pulseaudio --daemonize
#padevchooser &

setxkbmap -variant -option
setxkbmap -layout us,ru -variant ,winkeys -option grp:toggle,grp_led:scroll,terminate:ctrl_alt_bksp,ctrl:nocaps,compose:rwin

wait $wmpid
