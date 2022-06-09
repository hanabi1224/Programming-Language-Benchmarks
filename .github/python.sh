#!/usr/bin/sh

python3 --version

# sudo apt-get install -y dotnet-sdk-6.0
# python3 -m pip install -U pyjion

# sudo gu install python || echo 'already installed'
# sudo ln -sf $HOME/graalvm-ce-java17/bin/graalpython /usr/bin/graalpython
# graalpython --version

PYSTON_VERSION=2.3.4
PYSTON_ARCHIVE_NAME=pyston_${PYSTON_VERSION}_portable_amd64.tar.gz
cd /tmp
wget https://github.com/pyston/pyston/releases/download/pyston_$PYSTON_VERSION/$PYSTON_ARCHIVE_NAME -O $PYSTON_ARCHIVE_NAME
tar -xvf $PYSTON_ARCHIVE_NAME
cd pyston_${PYSTON_VERSION}
sudo ln -sf $PWD/pyston3 /usr/bin/pyston3
pyston3 --version
