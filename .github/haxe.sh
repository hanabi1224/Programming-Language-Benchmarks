#!/usr/bin/bash

sudo add-apt-repository ppa:haxe/releases -y
sudo apt-get update
sudo apt-get install haxe -y
haxelib --global update haxelib
mkdir ~/haxelib && haxelib setup ~/haxelib
haxelib install hxcpp
haxelib git hashlink https://github.com/HaxeFoundation/hashlink.git master other/haxelib/
