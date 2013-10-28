
if [ -e ~/.screenlayout/layout.sh ] ; then
    ~/.screenlayout/work.sh
fi

xrdb <$HOME/.Xdefaults
feh --bg-scale /usr/share/backgrounds/warty-final-ubuntu.png

# system tray
trayer --edge top --align right --widthtype pixel --width 180 --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x303030 --expand true --heighttype pixel --height 16 &
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
urxvtd &
pidgin &

# Delayed applications
(sleep 1 && urxvtc +bc +uc -cr Green -name Tmux1 -title Tmux1 -e sh -c "tmx 1") &
(sleep 2 && urxvtc +bc +uc -cr Green -name Tmux2 -title Tmux2 -e sh -c "tmx 2") &
(sleep 3 && urxvtc +bc +uc -cr Green -name IRC -title IRC -e sh -c "screen -r") &
(sleep 8 && chromium-browser) &
(sleep 15 && thunar) &
#(sleep 20 && skype) &
(sleep 30 && spotify) &
(sleep 60 && autokey) &

gnome-keyring-daemon --start --components=pkcs11,secrets,ssh,gpg

# time to start, the master - the window manager xmonad
exec ck-launch-session xmonad 
