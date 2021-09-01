#!/usr/bin/sh

sudo gu install nodejs || echo 'graal nodejs already installed'
sudo ln -sf $HOME/graalvm-ce-java11/bin/node /usr/bin/graalnode
graalnode --version
