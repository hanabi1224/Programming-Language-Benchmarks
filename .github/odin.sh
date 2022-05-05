#!/usr/bin/sh

VERSION=dev-2022-05
FILE_NAME=ubuntu_amd64_$VERSION.zip

mkdir /tmp/odin
cd /tmp/odin
wget https://github.com/odin-lang/Odin/releases/download/$VERSION/$FILE_NAME
unzip -o $FILE_NAME
sudo chmod +x odin
sudo ln -sf $PWD/odin /usr/bin/odin
odin version
