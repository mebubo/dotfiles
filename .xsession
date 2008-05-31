. ~/.environment

#xsetroot -solid \#3E71AA
xmodmap .xmodmaprc &
xautolock -time 30 -locker xlock &

#xmonad | dzen2 -ta l -fn fixed -w 600 -e 'button3=raise' -h 16 &
awesome &
wmpid=$!

sleep 1

#xgamma -rgamma 1.2 -bgamma 0.8
xset dpms 3600 3700 3800 
xset s 3600
#root-tail -g 970x700 -font fixed /var/log/syslog,gray &

#nice -14 ~/bin/status.sh | dzen2 -ta r -fn fixed -w 680 -x 600 -e 'button3=raise' -h 16 &
nice -14 ~/proj/scriplets/status-eee.py awesome | awesome-client &
statuspid=$!

#stalonetray &
#urxvtd -q -f -o

xbindkeys
#xcompmgr -c -t-5 -l-5 -r4.2 -o.55 &

wait $wmpid
kill -9 $statuspid
