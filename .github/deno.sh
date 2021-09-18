#!/usr/bin/sh

curl -fsSL https://deno.land/x/install/install.sh | sh
echo 'export PATH=$HOME/.deno/bin:$PATH' >>$PROFILE
