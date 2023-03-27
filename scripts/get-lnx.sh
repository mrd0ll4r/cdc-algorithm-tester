#!/bin/bash -e

source scripts/utils.sh

mkdir -p "$DATA_PATH/lnx"
cd "$DATA_PATH/lnx"

echo "Downloading..."
wget -q https://mirror.kkg.berlin/ubuntu-releases/14.04/ubuntu-14.04.6-desktop-amd64.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/14.04/ubuntu-14.04.6-server-amd64.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/14.04/ubuntu-14.04.6-desktop-i386.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/14.04/ubuntu-14.04.6-server-i386.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/16.04/ubuntu-16.04.7-desktop-amd64.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/16.04/ubuntu-16.04.7-server-amd64.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/16.04/ubuntu-16.04.6-desktop-i386.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/16.04/ubuntu-16.04.6-server-i386.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/18.04/ubuntu-18.04.6-desktop-amd64.iso

# tarring
echo "Creating TAR archive..."
tar -cf ../lnx.tar .
cd ..
rm -r lnx
