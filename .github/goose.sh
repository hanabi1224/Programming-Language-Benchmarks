#!/usr/bin/bash

set -euo pipefail

VERSION=v2026.2
NAME=goose-$VERSION-linux-x64
mkdir -p /tmp/goose
cd /tmp/goose
curl -sSLf -o $NAME.tar.gz https://github.com/aardappel/goose/releases/download/$VERSION/$NAME.tar.gz
tar -xzf $NAME.tar.gz
sudo ln -sf $(pwd)/$NAME/goose /usr/bin/goose
goose version
