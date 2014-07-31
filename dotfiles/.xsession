# system tray
trayer --edge top --align right --widthtype pixel --width 180 --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x303030 --expand true --heighttype pixel --height 16 &


urxvtd &
if [ -e ~/.screenlayout/layout.sh ] ; then
    ~/.screenlayout/work.sh
fi

xrdb <$HOME/.Xdefaults
# map caps lock as extra escape
#xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'&
#xmodmap -e 'clear Lock' &
#xmodmap -e "keycode 66 = Escape NoSymbol Escape" &
setxkbmap gb 
setxkbmap -option caps:escape

feh --bg-scale /usr/share/backgrounds/warty-final-ubuntu.png

# java fix
_JAVA_AWT_WM_NONREPARENTING=1; export _JAVA_AWT_WM_NONREPARENTING

$HOME/bin/start_irssi.pl

# Start apps
xscreensaver -no-splash &
firefox &
thunderbird &
kill `lsof -t -i:7877`
plackup -l localhost:7877 ~/bin/notifty-service.psgi > /dev/null &

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

orage &

xmodmap -e "keycode 108 = Alt_R"

xcompmgr -c &
pidgin &

# Delayed applications
(sleep 1 && urxvtc -name Tmux1 -title Tmux1 -e sh -c "tmx 1") &
(sleep 2 && urxvtc -name Tmux2 -title Tmux2 -e sh -c "tmx 1") &
(sleep 8 && google-chrome) &
#(sleep 15 && thunar) &
# No need to start spotify now, as alt+4 will load it up when needed
#(sleep 30 && spotify) &
# Annoyling in Ubuntu 14.04 there is a bug in autokey, so disabling - 0.90.4-1 version at time
(sleep 30 && autokey) &

gnome-keyring-daemon --start --components=pkcs11,secrets,ssh,gpg

#killall ibus-daemon

# time to start, the master - the window manager xmonad
exec ck-launch-session xmonad 
