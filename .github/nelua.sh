#!/usr/bin/sh

git clone https://github.com/edubart/nelua-lang.git $HOME/nelua-lang
cd $HOME/nelua-lang
make
sudo make install
nelua --version
