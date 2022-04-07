#!/usr/bin/sh

wget https://download.java.net/java/early_access/loom/5/openjdk-19-loom+5-429_linux-x64_bin.tar.gz
tar -xvf openjdk-19-loom+5-429_linux-x64_bin.tar.gz
sudo ln -sf $PWD/jdk-19/bin/javac /usr/bin/loomjavac
sudo ln -sf $PWD/jdk-19/bin/java /usr/bin/loomjava
loomjavac -version
loomjava -version
