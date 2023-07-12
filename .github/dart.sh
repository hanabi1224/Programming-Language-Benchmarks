#!/usr/bin/bash

pushd /tmp
mkdir dartsdk
pushd dartsdk
wget https://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/dartsdk-linux-x64-release.zip -O dartsdk.zip
unzip -o dartsdk.zip
sudo ln -sf $PWD/dart-sdk/bin/dart /usr/local/bin/dart
popd

# dart3
# mkdir dart3sdk
# pushd dart3sdk
# wget https://storage.googleapis.com/dart-archive/channels/dev/release/latest/sdk/dartsdk-linux-x64-release.zip -O dartsdk.zip
# unzip -o dartsdk.zip
# sudo ln -sf $PWD/dart-sdk/bin/dart /usr/local/bin/dart3
# popd
# popd
# dart --version
# dart3 --version
