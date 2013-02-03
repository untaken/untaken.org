sudo apt-get -y install aptitude
sudo aptitude -y install xmonad vim-gtk vim git subversion rxvt-unicode-256color xmobar xscreensaver orage libplack-perl xcompmgr pidgin trayer tmux feh autokey-gtk suckless-tools thunar gtk-chtheme lxappearance gnome-themes-standard chromium-browser
git clone https://github.com/untaken/untaken.org.git ~/untaken
mkdir ~/.xmonad/ ~/bin/
ln -s ~/untaken/backup/.xsession ~/.xsession
ln -s ~/untaken/backup/.Xdefaults ~/.Xdefaults
ln -s ~/untaken/backup/xmonad.hs ~/.xmonad/xmonad.hs
ln -s ~/untaken/backup/.xmobarrc ~/.xmobarrc
ln -s ~/untaken/backup/notifty-service.psgi ~/bin/notifty-service.psgi
cp ~/untaken/backup/default.desktop /usr/share/xsessions/default.desktop
mkdir ~/.fonts
git clone https://gist.github.com/1634235.git ~/.fonts/
fc-cache -vf
