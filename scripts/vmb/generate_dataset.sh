#!/bin/bash -e

# Script to build VM backups of a nightly Rust build server.
# Disk images are saved at disk_images/$day.qcow2

mkdir -p disk_images

# Create a clean base image
qemu-img create -f qcow2 -F qcow2 -b debian-12-nocloud-amd64-20250210-2019.qcow2 root.qcow2 35g

# Add first boot script and nightly commit hashes etc.
virt-customize -a root.qcow2 --firstboot boot_scripts/firstboot.sh \
	--copy-in boot_scripts/build_repo_commit.sh:/root/ \
	--copy-in repo_uris.csv:/root/ \
	--copy-in commit_hashes.csv:/root/

# For each day...
# TODO for some arcane reason it is impossible to run this as a while loop.
# Somehow QEMU messes with it.

./simulate_single_day.sh "00" "2020-01-01"
./simulate_single_day.sh "01" "2025-01-06"
./simulate_single_day.sh "02" "2025-01-07"
./simulate_single_day.sh "03" "2025-01-08"
./simulate_single_day.sh "04" "2025-01-09"
./simulate_single_day.sh "05" "2025-01-10"
./simulate_single_day.sh "06" "2025-01-11"
./simulate_single_day.sh "07" "2025-01-12"
./simulate_single_day.sh "08" "2025-01-13"
./simulate_single_day.sh "09" "2025-01-14"
./simulate_single_day.sh "10" "2025-01-15"
./simulate_single_day.sh "11" "2025-01-16"
./simulate_single_day.sh "12" "2025-01-17"
./simulate_single_day.sh "13" "2025-01-18"
./simulate_single_day.sh "14" "2025-01-19"
./simulate_single_day.sh "15" "2025-01-20"
./simulate_single_day.sh "16" "2025-01-21"
./simulate_single_day.sh "17" "2025-01-22"
./simulate_single_day.sh "18" "2025-01-23"
./simulate_single_day.sh "19" "2025-01-24"
./simulate_single_day.sh "20" "2025-01-25"
./simulate_single_day.sh "21" "2025-01-26"
./simulate_single_day.sh "22" "2025-01-27"
./simulate_single_day.sh "23" "2025-01-28"
./simulate_single_day.sh "24" "2025-01-29"
./simulate_single_day.sh "25" "2025-01-30"
./simulate_single_day.sh "26" "2025-01-31"
./simulate_single_day.sh "27" "2025-02-01"
./simulate_single_day.sh "28" "2025-02-02"
./simulate_single_day.sh "29" "2025-02-03"
./simulate_single_day.sh "30" "2025-02-04"
./simulate_single_day.sh "31" "2025-02-05"
