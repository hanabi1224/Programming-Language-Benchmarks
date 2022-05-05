#!/usr/bin/bash

git clone https://github.com/HaxeFoundation/hashlink.git /tmp/hashlink
cd /tmp/hashlink
make
sudo make install
hl --version
