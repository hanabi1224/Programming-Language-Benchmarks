#!/usr/bin/sh

sudo apt-get install -y software-properties-common apt-transport-https
# sudo apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0xB4112585D386EB94
curl https://dl.hhvm.com/conf/hhvm.gpg.key | sudo apt-key add -
sudo apt-key finger 'opensource+hhvm@fb.com'
sudo add-apt-repository https://dl.hhvm.com/ubuntu
sudo apt-get update
sudo apt-get install -y hhvm
