#!/usr/bin/bash

curl -sSL https://packages.microsoft.com/keys/microsoft.asc | sudo tee /etc/apt/trusted.gpg.d/microsoft.asc
sudo apt-add-repository https://packages.microsoft.com/ubuntu/22.04/prod
sudo apt-get update
sudo apt-get install -y libmsquic zlib1g-dev clang
