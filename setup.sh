#!/bin/bash

# create symlinks (sudo ln -s /from/path /to/path)
sudo ln -s ~/Dropbox/unix/.vimrc ~/
sudo ln -s ~/Dropbox/unix/.bashrc ~/
sudo ln -s ~/Dropbox/unix/hosts /etc/

# todo: sites-enabled and mods-enabled

sudo ln -s ~/Dropbox/unix/now.sh ~/ && sudo chmod u+x ~/now.sh

### dependencies (create something to compile this to an .sh script)
# nodejs
python-software-properties
# luakit
git-core curl ruby vim-gnome git gcc make libx11-dev libxtst-dev pkg-config rake autoconf sqlite3 webkit-1.0 dmenu socat glib-2.0 liblua5.1-0-dev liblua5.1-filesystem0 libsqlite3-dev libwebkitgtk-dev libwebkitgtk-3.0-dev unique-1.0 libunique-dev libncurses5-dev libncursesw5-dev luarocks
# github ssh
xclip
# vim-reload-linux
xdotool
# vimdow
wmctrl 

### other
apache2

### afterwards
# node
sudo add-apt-repository ppa:chris-lea/node.js
sudo apt-get update
sudo apt-get install nodejs npm

# ruby
sudo apt-get install ruby1.8-dev rubygems
