#!/usr/bin/sh

sudo gu install python
sudo ln -sf $HOME/graalvm-ce-java11/bin/graalpython /usr/bin/graalpython
graalpython --version
