#!/usr/bin/sh

# https://apt.llvm.org/
# sudo bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh 13
sudo apt-get install -y clang-13 libapr1-dev libomp-13-dev
