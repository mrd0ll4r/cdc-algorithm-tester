#!/bin/bash -e

# Script to generate VM images of a nightly Rust build server.
# This is inteded to be used on a Debian-based host supporting KVM virtualization.
# We will spawn a virtual machine using QEMU and make it do things.

source scripts/utils.sh
mkdir -p "$DATA_PATH/vmb"

# Dependencies
sudo apt install qemu-system-x86 guestfs-tools

# Add your user to the kvm group to be able to use KVM, which speeds things up a lot.
sudo usermod -aG kvm $USER

# Enable ping on SLIRP interfaces, which we need to check for connectivity in the VM.
sudo sysctl -w net.ipv4.ping_group_range='0 2147483647'
# Optionally add that to /etc/sysctl.conf

pushd scripts/vmb

# Download debian base image
wget https://cloud.debian.org/images/cloud/bookworm/20250210-2019/debian-12-nocloud-amd64-20250210-2019.qcow2

# TODO virt-customize reads the host(?!) kernel and... runs it in a VM, and then mounts the VM image, or something?
# Anyway, it complains if /boot/vmlinuz-<current?> is not readable.

./generate_dataset.sh

popd

mv scripts/vmb/disk_images/* "$DATA_PATH/vmb/"
