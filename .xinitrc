xrdb -merge $HOME/.Xresources
xsetroot -solid darkgray
xmodmap .xmodmaprc &

xmonad | dzen2 -ta l -fn fixed

#while true
#do
#    echo `date` `uptime | sed 's/.*,//'`
#    sleep 1
#done | dwm
