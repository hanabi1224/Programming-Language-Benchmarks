#!/usr/bin/sh

URL=https://download.java.net/java/early_access/loom/6/openjdk-19-loom+6-625_linux-x64_bin.tar.gz
wget $URL -O loom.tar.gz
tar -xvf loom.tar.gz
sudo ln -sf $PWD/jdk-19/bin/javac /usr/bin/loomjavac
sudo ln -sf $PWD/jdk-19/bin/java /usr/bin/loomjava
loomjavac -version
loomjava -version
