#!/usr/bin/sh

sudo gu install python
sudo ln -sf $HOME/graalvm-ce-java17/bin/graalpython /usr/bin/graalpython
graalpython --version
