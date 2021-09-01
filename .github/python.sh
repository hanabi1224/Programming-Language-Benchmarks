#!/usr/bin/sh

gu install python
ln -sf $HOME/graalvm-ce-java11/bin/graalpython /usr/bin/graalpython
graalpython --version
