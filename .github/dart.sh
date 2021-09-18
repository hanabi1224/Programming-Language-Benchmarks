#!/usr/bin/sh

wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
wget -qO- https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list >/etc/apt/sources.list.d/dart_stable.list
apt-get update -y
apt-get install dart -y
echo 'export PATH="$PATH:/usr/lib/dart/bin"' >>$PROFILE
