#!/usr/bin/bash

echo $LD_LIBRARY_PATH
echo 'export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH' >>$PROFILE
source $PROFILE
echo $LD_LIBRARY_PATH

sudo apt-get install -y libpng-dev libturbojpeg-dev libvorbis-dev libopenal-dev libsdl2-dev libmbedtls-dev libuv1-dev libsqlite3-dev
git clone https://github.com/HaxeFoundation/hashlink.git /tmp/hashlink
cd /tmp/hashlink
make
sudo make install
hl --version
