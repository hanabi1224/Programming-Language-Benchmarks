#!/usr/bin/sh

wget https://download.java.net/java/early_access/loom/2/openjdk-18-loom+2-74_linux-x64_bin.tar.gz
tar -xvf openjdk-18-loom+2-74_linux-x64_bin.tar.gz
sudo ln -sf $PWD/jdk-18/bin/javac /usr/bin/loomjavac
sudo ln -sf $PWD/jdk-18/bin/java /usr/bin/loomjava
loomjavac -version
loomjava -version
