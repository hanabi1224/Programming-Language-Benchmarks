#!/usr/bin/bash

set -euo pipefail

LUA_VERSION="5.4.7"
# https://www.lua.org/ftp/#
pushd /tmp
curl -L -R -O http://www.lua.org/ftp/lua-$LUA_VERSION.tar.gz
tar zxf lua-$LUA_VERSION.tar.gz
pushd lua-$LUA_VERSION
make all test
# sudo ln -sf $PWD/src/luac /usr/bin/luac
# sudo ln -sf $PWD/src/lua /usr/bin/lua
sudo make install
popd
popd
# cp /tmp/lua-$LUA_VERSION/src/lua ./bench/include/lua/out
luac -v
lua -v
# ./bench/include/lua/out/lua -v

LUAJIT_VERSION=2.1.0-beta3
git clone --branch v$LUAJIT_VERSION https://github.com/LuaJIT/LuaJIT.git /tmp/LuaJIT
pushd /tmp/LuaJIT
make
sudo make install
luajit-$LUAJIT_VERSION -v
sudo ln -sf luajit-$LUAJIT_VERSION /usr/local/bin/luajit
popd
luajit -v
# cp /tmp/LuaJIT/src/luajit ./bench/include/lua/out
# ./bench/include/lua/out/luajit -v
