#!/usr/bin/bash

curl https://bun.sh/install | bash
echo 'export PATH=$HOME/.bun/bin:$PATH' >>$PROFILE
