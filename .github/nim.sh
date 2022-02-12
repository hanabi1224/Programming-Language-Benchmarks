#!/usr/bin/bash

curl https://nim-lang.org/choosenim/init.sh -sSf > choosenim.sh
sh choosenim.sh -y
sudo ln -sf $HOME/.nimble/bin/choosenim /usr/bin/choosenim
choosenim update stable
echo 'PATH=$HOME/.nimble/bin:$PATH' >>$PROFILE
