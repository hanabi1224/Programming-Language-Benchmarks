#!/usr/bin/sh

# https://crystal-lang.org/install/on_ubuntu/
curl -fsSL https://crystal-lang.org/install.sh | sudo bash
sudo apt install -y libgmp-dev libpcre2-dev libpcre2-8-0 libpcre2-16-0 libpcre2-32-0 libpcre2-posix3
crystal version
