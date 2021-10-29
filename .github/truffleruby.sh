#!/usr/bin/sh

sudo gu install ruby
sudo ln -sf $HOME/graalvm-ce-java17/bin/truffleruby /usr/bin/truffleruby
truffleruby --version
