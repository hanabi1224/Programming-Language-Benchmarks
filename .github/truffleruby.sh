#!/usr/bin/sh

gu install ruby
ln -sf $HOME/graalvm-ce-java11/bin/truffleruby /usr/bin/truffleruby
truffleruby --version
