#!/usr/bin/bash

curl https://nim-lang.org/choosenim/init.sh -sSf > choosenim.sh
sh choosenim.sh -y
choosenim stable
choosenim update stable
echo 'PATH=$HOME/.nimble/bin:$PATH' >>$PROFILE
