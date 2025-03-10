#!/bin/bash -e

day="$1"
build_date="$2"

if [ -f "disk_images/$day.qcow2" ]; then
	echo "Day $day: already done, skipping."
	exit 0

	#echo "disk image already exists, skipping execution for day $day, reusing image..."
	#cp "disk_images/$day.qcow2" root.qcow2
	#exit 0
fi

echo "Working on day $day..."
target_script=""
if [ "$day" = "00" ]; then
	target_script="boot_scripts/00.sh"
else
	# We use the image of day zero for each build day.
	# This means that builds take longer and can't use the cached dependencies of earlier builds,
	# but each image is smaller.
	# This also corresponds to a real build system, which has a clean slate for every build.
	cp "disk_images/00.qcow2" root.qcow2
	target_script="boot_scripts/01.sh"
fi
chmod +x "$target_script"
cp "$target_script" ./daily_job.sh
echo "$build_date" > build.date
virt-customize -a root.qcow2 \
	--copy-in daily_job.sh:/root/ \
	--copy-in build.date:/root/

qemu-system-x86_64 \
  -m 64g \
  -enable-kvm \
  -smp $(nproc) \
  -nographic \
  -serial mon:stdio \
  -drive file=root.qcow2,driver=qcow2 \
  -nic user,model=virtio-net-pci \
  -no-reboot \
  -cpu host | ./ignore-stdin-for-secs.sh 10

# Save current disk
cp root.qcow2 disk_images/$day.qcow2
