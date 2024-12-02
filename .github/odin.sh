#!/usr/bin/bash

set -euo pipefail

VERSION=dev-2024-11
FILE_NAME=odin-linux-amd64-$VERSION.zip
sudo apt-get install -y aria2
mkdir /tmp/odin || true
cd /tmp/odin
aria2c -c -o $FILE_NAME https://github.com/odin-lang/Odin/releases/download/$VERSION/$FILE_NAME
unzip -o $FILE_NAME
tar -xvf dist.tar.gz
ODIN_BIN_PATH=$(pwd)/$(find */odin)
sudo ln -sf $ODIN_BIN_PATH /usr/bin/odin
odin version
