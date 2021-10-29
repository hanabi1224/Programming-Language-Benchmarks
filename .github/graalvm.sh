#!/usr/bin/sh

# https://github.com/actions/virtual-environments/blob/main/images/linux/Ubuntu2004-README.md
# https://www.graalvm.org/docs/getting-started/
echo $GRAALVM_11_ROOT

# sudo ln -sf $GRAALVM_11_ROOT $HOME/graalvm-ce-java11

wget https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-21.3.0/graalvm-ce-java17-linux-amd64-21.3.0.tar.gz
tar -xvf graalvm-ce-java17-linux-amd64-21.3.0.tar.gz
sudo ln -sf $PWD/graalvm-ce-java17-21.3.0 $HOME/graalvm-ce-java17

ls -al $HOME/graalvm-ce-java17
sudo ln -sf $HOME/graalvm-ce-java17/bin/gu /usr/bin/gu
sudo gu upgrade -A || echo ''
gu --version
sudo ln -sf $HOME/graalvm-ce-java17/bin/java /usr/bin/graaljava
graaljava -version
sudo ln -sf $HOME/graalvm-ce-java17/bin/javac /usr/bin/graaljavac
graaljavac -version
