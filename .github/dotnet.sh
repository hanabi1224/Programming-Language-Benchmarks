#!/usr/bin/bash

curl -sSL https://packages.microsoft.com/keys/microsoft.asc | sudo apt-key add -
sudo apt-add-repository https://packages.microsoft.com/debian/10/prod
sudo apt-get update
sudo apt-get install -y libmsquic
