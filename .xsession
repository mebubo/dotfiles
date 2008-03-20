. ~/.environment

xsetroot -solid \#3E71AA
xmodmap .xmodmaprc &
xautolock -time 30 -locker xlock &

xmonad | dzen2 -ta l -fn fixed -w 600 -e 'button3=raise' &
wmpid=$!

xgamma -rgamma 1.2 -bgamma 0.8
xset dpms 3600 3700 3800 
xset s 3600
root-tail -g 970x700 -font fixed /var/log/syslog,gray &

nice -14 ~/bin/status.sh | dzen2 -ta r -fn fixed -w 680 -x 600 -e 'button3=raise' &
statuspid=$!

urxvtd -q -f -o
xbindkeys

wait $wmpid
kill -9 $statuspid
