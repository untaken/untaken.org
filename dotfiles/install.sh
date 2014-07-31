sudo apt-get -y install aptitude
sudo aptitude -y install xmonad vim-gtk vim git subversion rxvt-unicode-256color xmobar xscreensaver orage libplack-perl xcompmgr pidgin trayer tmux feh autokey-gtk suckless-tools thunar gtk-chtheme lxappearance gnome-themes-standard chromium-browser
# Additional debs I like installed
sudo aptitude -y install ffmpeg mencoder
sudo aptitude -y ffmpeg mencoder
git clone --recursive https://github.com/untaken/untaken.org.git ~/untaken.org
mkdir ~/.xmonad/ ~/bin/
ln -s ~/untaken.org/dotfiles/.xsession ~/.xsession
ln -s ~/untaken.org/dotfiles/.Xdefaults ~/.Xdefaults
ln -s ~/untaken.org/dotfiles/xmonad.hs ~/.xmonad/xmonad.hs
ln -s ~/untaken.org/dotfiles/.xmobarrc ~/.xmobarrc
ln -s ~/untaken.org/dotfiles/.tmux.conf ~/.tmux.conf
ln -s ~/untaken.org/dotfiles/notifty-service.psgi ~/bin/notifty-service.psgi
ln -s ~/untaken.org/dotfiles/.vimrc ~/.vimrc
ln -s ~/untaken.org/dotfiles/.vim ~/.vim
sudo cp ~/untaken.org/dotfiles/default.desktop /usr/share/xsessions/default.desktop
mkdir ~/.fonts
git clone https://gist.github.com/1634235.git ~/.fonts/
fc-cache -vf
