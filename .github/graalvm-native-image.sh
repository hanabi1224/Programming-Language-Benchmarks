#!/usr/bin/sh

sudo gu install native-image
sudo ln -sf $HOME/graalvm-ce-java11/bin/native-image /usr/bin/native-image
native-image --version
