#!/usr/bin/bash

# https://www.lua.org/ftp/#
pushd /tmp
curl -R -O http://www.lua.org/ftp/lua-5.4.4.tar.gz
tar zxf lua-5.4.4.tar.gz
pushd lua-5.4.4
make all test
sudo ln -sf $PWD/src/luac /usr/bin/luac
popd
popd
cp /tmp/lua-5.4.4/src/lua ./bench/include/lua/out
luac -v
./bench/include/lua/out/lua -v

git clone git@github.com:LuaJIT/LuaJIT.git /tmp/LuaJIT
pushd /tmp/LuaJIT
make
popd
cp /tmp/LuaJIT/src/luajit ./bench/include/lua/out
./bench/include/lua/out/luajit -v
