#!/usr/bin/bash

/usr/bin/bash -c "$(curl -fsSL https://exaloop.io/install.sh)"
sudo ln -sf ~/.codon/bin/codon /usr/local/bin/codon
codon --version
