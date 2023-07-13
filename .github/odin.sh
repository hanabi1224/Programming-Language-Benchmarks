#!/usr/bin/sh

VERSION=dev-2023-05
FILE_NAME=odin-ubuntu-amd64-$VERSION.zip
sudo apt-get install -y aria2
mkdir /tmp/odin
cd /tmp/odin
aria2c -c -o $FILE_NAME https://github.com/odin-lang/Odin/releases/download/$VERSION/$FILE_NAME
if test -d ubuntu_artifacts; then sudo rm -rf ubuntu_artifacts; fi
unzip -o $FILE_NAME
if test -d ubuntu_artifacts; then ODIN_BIN_PATH=$PWD/ubuntu_artifacts/odin; else ODIN_BIN_PATH=$PWD/odin; fi
sudo chmod +x $ODIN_BIN_PATH
sudo ln -sf $ODIN_BIN_PATH /usr/bin/odin
odin version
