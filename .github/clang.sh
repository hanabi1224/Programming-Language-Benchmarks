#!/usr/bin/sh

# https://apt.llvm.org/
# sudo bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh 14
sudo apt-get install -y clang-14 libapr1-dev libomp-14-dev
sudo ln -sf /usr/bin/clang-14 /usr/bin/clang
clang --version
