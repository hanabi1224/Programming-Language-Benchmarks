#!/usr/bin/bash

# docker run --rm -v /tmp/hostlib/:/working/ -w /working/ thevlang/vlang:alpine cp /lib/ld-musl-x86_64.so.1 .
# sudo cp /tmp/hostlib/ld-musl-x86_64.so.1 /lib/
sudo apt-get install -y libgc-dev
git clone https://github.com/vlang/v /tmp/vlang
cd /tmp/vlang
pushd vlib/vweb
cat vweb.v | sed "s/\bprintln('\[Vweb\] Running/eprintln('[Vweb] Running/" > temp.v && mv temp.v vweb.v
popd
make && ./v -version
./v symlink
v --version
v install hanabi1224.biginteger || echo 'ignore failure'
