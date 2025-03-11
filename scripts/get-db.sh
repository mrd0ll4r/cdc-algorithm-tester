#!/bin/bash

source scripts/utils.sh

# Script to generate DB dataset.

chmod +x scripts/db.rc.local
mkdir -p "$DATA_PATH/db"
cd "$DATA_PATH/db" || exit

rm -f *.qcow2
wget -nc -q https://cloud.debian.org/images/cloud/bookworm/20250210-2019/debian-12-nocloud-amd64-20250210-2019.qcow2

qemu-img create -f qcow2 -F qcow2 -b debian-12-nocloud-amd64-20250210-2019.qcow2 root.qcow2 100g
virt-customize -a root.qcow2 --copy-in ../../scripts/db.rc.local:/etc/
virt-customize -a root.qcow2 --run-command "mv /etc/db.rc.local /etc/rc.local"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "1.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "2.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "3.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "4.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "5.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "6.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "7.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "8.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "9.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "10.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "11.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "12.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "13.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "14.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "15.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "16.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "17.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "18.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "19.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "20.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "21.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "22.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "23.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "24.qcow2"

qemu-system-x86_64 -m 4g \
		-enable-kvm  \
		-nographic \
		-serial mon:stdio \
		-drive file=root.qcow2,driver=qcow2 \
		-nic user,model=virtio-net-pci
cp root.qcow2 "25.qcow2"

rm debian-12-nocloud-amd64-20250210-2019.qcow2 root.qcow2