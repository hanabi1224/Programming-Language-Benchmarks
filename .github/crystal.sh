#!/usr/bin/sh

# https://crystal-lang.org/install/on_ubuntu/
curl -fsSL https://crystal-lang.org/install.sh | sudo bash
apt install -y libgmp-dev
crystal version
