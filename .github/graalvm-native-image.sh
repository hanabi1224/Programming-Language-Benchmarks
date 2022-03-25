#!/usr/bin/sh

sudo gu install native-image || echo 'already installed'
sudo ln -sf $HOME/graalvm-ce-java17/bin/native-image /usr/bin/native-image
native-image --version
