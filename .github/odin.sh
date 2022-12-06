#!/usr/bin/sh

VERSION=dev-2022-12
FILE_NAME=odin-ubuntu-amd64-$VERSION.zip

mkdir /tmp/odin
cd /tmp/odin
wget https://github.com/odin-lang/Odin/releases/download/$VERSION/$FILE_NAME
unzip -o $FILE_NAME
sudo chmod +x $PWD/odin
sudo ln -sf $PWD/odin /usr/bin/odin
odin version
