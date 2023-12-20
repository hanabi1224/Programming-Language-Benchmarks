#!/usr/bin/bash
set -ev
# https://www.lua.org/ftp/#
pushd /tmp
curl -R -O http://www.lua.org/ftp/lua-5.4.6.tar.gz
tar zxf lua-5.4.6.tar.gz
pushd lua-5.4.6
make all test
# sudo ln -sf $PWD/src/luac /usr/bin/luac
# sudo ln -sf $PWD/src/lua /usr/bin/lua
sudo make install
popd
popd
# cp /tmp/lua-5.4.6/src/lua ./bench/include/lua/out
luac -v
lua -v
# ./bench/include/lua/out/lua -v

git clone https://github.com/LuaJIT/LuaJIT.git /tmp/LuaJIT
pushd /tmp/LuaJIT
git checkout tags/v2.1.0-beta3
make
sudo make install
luajit-2.1.0-beta3 -v
sudo ln -sf luajit-2.1.0-beta3 /usr/local/bin/luajit
popd
luajit -v
# cp /tmp/LuaJIT/src/luajit ./bench/include/lua/out
# ./bench/include/lua/out/luajit -v
