#!/usr/bin/sh

docker run --rm -v /tmp/hostlib/:/working/ -w /working/ thevlang/vlang:alpine cp /lib/ld-musl-x86_64.so.1 .
cp /tmp/hostlib/ld-musl-x86_64.so.1 /lib/
apt install -y libgc-dev
git clone https://github.com/vlang/v /tmp/vlang
pushd /tmp/vlang
make && ./v -version
./v symlink
popd
v --version
