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
wget -q https://mirror.kkg.berlin/ubuntu-releases/20.04/ubuntu-20.04.6-desktop-amd64.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/20.04/ubuntu-20.04.6-live-server-amd64.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/22.04/ubuntu-22.04.5-desktop-amd64.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/22.04/ubuntu-22.04.5-live-server-amd64.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/24.04/ubuntu-24.04.2-desktop-amd64.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/24.04/ubuntu-24.04.2-live-server-amd64.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/24.10/ubuntu-24.10-desktop-amd64.iso
wget -q https://mirror.kkg.berlin/ubuntu-releases/24.10/ubuntu-24.10-live-server-amd64.iso
wget -q https://debian.inf.tu-dresden.de/debian-cd/12.9.0-live/amd64/iso-hybrid/debian-live-12.9.0-amd64-gnome.iso
wget -q https://debian.inf.tu-dresden.de/debian-cd/12.9.0-live/amd64/iso-hybrid/debian-live-12.9.0-amd64-kde.iso
wget -q https://debian.inf.tu-dresden.de/debian-cd/12.9.0-live/amd64/iso-hybrid/debian-live-12.9.0-amd64-cinnamon.iso
wget -q https://debian.inf.tu-dresden.de/debian-cd/12.9.0-live/amd64/iso-hybrid/debian-live-12.9.0-amd64-lxde.iso
wget -q https://debian.inf.tu-dresden.de/debian-cd/12.9.0-live/amd64/iso-hybrid/debian-live-12.9.0-amd64-lxqt.iso
wget -q https://debian.inf.tu-dresden.de/debian-cd/12.9.0-live/amd64/iso-hybrid/debian-live-12.9.0-amd64-mate.iso
wget -q https://debian.inf.tu-dresden.de/debian-cd/12.9.0-live/amd64/iso-hybrid/debian-live-12.9.0-amd64-standard.iso
wget -q https://debian.inf.tu-dresden.de/debian-cd/12.9.0-live/amd64/iso-hybrid/debian-live-12.9.0-amd64-xfce.iso
wget -q https://mirror.informatik.tu-freiberg.de/arch/iso/2025.01.01/archlinux-x86_64.iso
wget -q https://mirror.informatik.tu-freiberg.de/arch/iso/2025.01.01/archlinux-x86_64.iso
wget -q https://mirror.informatik.tu-freiberg.de/arch/iso/2025.02.01/archlinux-x86_64.iso
wget -q https://download.fedoraproject.org/pub/fedora/linux/releases/41/Workstation/x86_64/iso/Fedora-Workstation-Live-x86_64-41-1.4.iso
wget -q https://download.fedoraproject.org/pub/fedora-secondary/releases/41/Workstation/ppc64le/iso/Fedora-Workstation-Live-ppc64le-41-1.4.iso
wget -q https://download.fedoraproject.org/pub/fedora/linux/releases/41/Server/x86_64/iso/Fedora-Server-dvd-x86_64-41-1.4.iso
wget -q https://download.fedoraproject.org/pub/fedora/linux/releases/41/Server/aarch64/iso/Fedora-Server-dvd-aarch64-41-1.4.iso
wget -q https://download.fedoraproject.org/pub/fedora/linux/releases/41/Server/aarch64/iso/Fedora-Server-netinst-aarch64-41-1.4.iso
wget -q https://download.fedoraproject.org/pub/fedora-secondary/releases/41/Server/ppc64le/iso/Fedora-Server-dvd-ppc64le-41-1.4.iso
wget -q https://download.fedoraproject.org/pub/fedora-secondary/releases/41/Server/ppc64le/iso/Fedora-Server-netinst-ppc64le-41-1.4.iso
wget -q https://download.fedoraproject.org/pub/fedora-secondary/releases/41/Server/s390x/iso/Fedora-Server-dvd-s390x-41-1.4.iso
wget -q https://download.fedoraproject.org/pub/fedora-secondary/releases/41/Server/s390x/iso/Fedora-Server-netinst-s390x-41-1.4.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/39/Server/aarch64/iso/Fedora-Server-dvd-aarch64-39-1.5.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/39/Server/x86_64/iso/Fedora-Server-dvd-x86_64-39-1.5.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/38/Server/aarch64/iso/Fedora-Server-dvd-aarch64-38-1.6.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/38/Server/x86_64/iso/Fedora-Server-netinst-x86_64-38-1.6.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/37/Server/aarch64/iso/Fedora-Server-dvd-aarch64-37-1.7.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/37/Server/x86_64/iso/Fedora-Server-dvd-x86_64-37-1.7.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/36/Server/aarch64/iso/Fedora-Server-dvd-aarch64-36-1.5.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/36/Server/armhfp/iso/Fedora-Server-dvd-armhfp-36-1.5.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/36/Server/x86_64/iso/Fedora-Server-dvd-x86_64-36-1.5.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/36/Server/x86_64/iso/Fedora-Server-dvd-x86_64-36-1.5.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/35/Server/armhfp/iso/Fedora-Server-dvd-armhfp-35-1.2.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/35/Server/x86_64/iso/Fedora-Server-dvd-x86_64-35-1.2.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/34/Server/aarch64/iso/Fedora-Server-dvd-aarch64-34-1.2.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/34/Server/armhfp/iso/Fedora-Server-dvd-armhfp-34-1.2.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/34/Server/x86_64/iso/Fedora-Server-dvd-x86_64-34-1.2.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/33/Server/aarch64/iso/Fedora-Server-dvd-aarch64-33-1.2.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/33/Server/armhfp/iso/Fedora-Server-dvd-armhfp-33-1.2.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/33/Server/armhfp/iso/Fedora-Server-dvd-armhfp-33-1.3.iso
wget -q https://archives.fedoraproject.org/pub/archive/fedora/linux/releases/33/Server/x86_64/iso/Fedora-Server-dvd-x86_64-33-1.2.iso

# tarring
echo "Creating TAR archive..."
tar -cf ../lnx.tar .
cd ..
rm -r lnx
