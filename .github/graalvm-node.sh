#!/usr/bin/sh

gu install nodejs || echo 'graal nodejs already installed'
ln -sf $HOME/graalvm-ce-java11/bin/node /usr/bin/graalnode
graalnode --version
