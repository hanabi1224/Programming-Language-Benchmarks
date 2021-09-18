#!/usr/bin/sh

sudo gu install wasm
sudo ln -sf $HOME/graalvm-ce-java11/bin/wasm /usr/bin/graalwasm
graalwasm --version

wget https://raw.githubusercontent.com/WasmEdge/WasmEdge/master/utils/install.sh
chmod +x ./install.sh
sudo ./install.sh -p /usr/local
wasmedge --version
wasmedgec --version

curl https://get.wasmer.io -sSfL | sh
cat $HOME/.wasmer/wasmer.sh >>$PROFILE

curl https://wasmtime.dev/install.sh -sSf | bash
echo 'PATH=$HOME/.wasmtime/bin:$PATH' >>$PROFILE
cat $PROFILE
