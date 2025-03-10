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

for i in $(seq 1 25); do
	qemu-system-x86_64 -m 4g \
			-enable-kvm  \
			-nographic \
			-serial mon:stdio \
			-drive file=root.qcow2,driver=qcow2 \
			-nic user,model=virtio-net-pci
	cp root.qcow2 "$i.qcow2"
done

rm debian-12-nocloud-amd64-20250210-2019.qcow2 root.qcow2