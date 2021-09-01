#!/usr/bin/sh

gu install native-image
ln -sf $HOME/graalvm-ce-java11/bin/native-image /usr/bin/native-image
native-image --version
