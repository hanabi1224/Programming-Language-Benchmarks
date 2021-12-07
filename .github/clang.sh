#!/usr/bin/sh

sudo bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
sudo apt-get install -y clang-13 libapr1-dev libomp-13-dev
