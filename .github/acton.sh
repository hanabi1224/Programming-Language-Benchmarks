#!/usr/bin/sh

wget -q -O - https://apt.acton-lang.io/acton.gpg | sudo apt-key add -
echo "deb [arch=amd64] http://apt.acton-lang.io/ bullseye main" | sudo tee /etc/apt/sources.list.d/acton.list
sudo apt-get update
sudo apt-get install -qy acton
actonc --version
