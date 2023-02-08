#!/usr/bin/sh

VERSION=dev-2023-02
FILE_NAME=odin-ubuntu-amd64-$VERSION.zip
sudo apt-get install -y aria2
mkdir /tmp/odin
cd /tmp/odin
aria2c -c -o $FILE_NAME https://github.com/odin-lang/Odin/releases/download/$VERSION/$FILE_NAME
unzip -o $FILE_NAME
sudo chmod +x $PWD/odin
sudo ln -sf $PWD/odin /usr/bin/odin
odin version
