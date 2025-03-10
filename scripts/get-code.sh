#!/bin/bash

source scripts/utils.sh

# Downloads source code tarballs of gcc for 2019-2022 releases, extracts them, and collects the results into a tarball.

download_extract_archive() {
  local url=$1
  local name=$2
  echo "Downloading $url"
  if ! wget -q "$url"; then
    echo "Failed to download $url"
    return 1
  fi
  gunzip -d "$name".tar.gz
}

mkdir -p "$DATA_PATH/code"
cd "$DATA_PATH/code" || exit

# gcc
echo "Downloading GCC..."
declare -a gcc_versions=(
  "12.2.0" "12.1.0" "11.3.0" "11.2.0" "11.1.0" "10.4.0" "10.3.0" "10.2.0" "10.1.0" "9.5.0" "9.4.0" "9.3.0" "9.2.0"
  "9.1.0" "8.5.0" "8.4.0" "8.3.0" "7.5.0" "7.4.0" "7.3.0" "7.2.0" "7.1.0" "6.5.0" "6.4.0" "6.3.0" "6.2.0" "6.1.0"
  "5.5.0" "5.4.0" "5.3.0" "5.2.0" "5.1.0" "4.9.4" "4.9.3" "4.9.2" "4.9.1" "4.9.0" "4.8.5" "4.8.4" "4.8.3" "4.8.2"
  "4.8.1" "4.8.0" "4.7.4" "4.7.3" "4.7.2" "4.7.1" "4.7.0" "4.6.4" "4.6.3" "4.6.2" "4.6.1" "4.6.0" "4.5.4" "4.5.3"
  "4.5.2" "4.5.1" "4.5.0" "4.4.7" "4.4.6" "4.4.5" "4.4.4" "4.4.3" "4.4.2" "4.4.1" "4.4.0" "4.3.6" "4.3.5" "4.3.4"
  "4.3.3" "4.3.2" "4.3.1" "4.3.0" "4.2.4" "4.2.3" "4.2.2" "4.2.1" "4.2.0" "4.1.2" "4.1.1" "4.1.0" "4.0.4" "4.0.3"
  "4.0.2" "4.0.1" "4.0.0" "3.4.6" "3.4.5" "3.4.4" "3.4.3" "3.4.2" "3.4.1" "3.4.0" "3.3.6" "3.3.5" "3.3.4" "3.3.3"
  "3.3.2" "3.3.1" "3.3.0" "3.2.3" "3.2.2" "3.2.1" "3.2.0" "3.1.1" "3.1.0" "3.0.4" "3.0.3" "3.0.2" "3.0.1" "3.0.0"
  "2.95.3" "2.95.2" "2.95.1"
)
for version in "${gcc_versions[@]}"; do
  if ! download_extract_archive "http://ftpmirror.gnu.org/gcc/gcc-$version/gcc-$version.tar.gz" "gcc-$version"; then
    echo "Warning: Failed to process gcc-$version, continuing with next version..."
  fi
done

# emacs
echo "Downloading Emacs..."
declare -a emacs_versions=(
  "29.1" "21.4" "24.4" "22.1" "24.5" "27.1" "22.2" "22.3" "23.1" "25.3" "23.2" "27.2" "23.3" "26.1" "23.4" "24.1" 
  "28.1" "26.2" "24.2" "24.3" "26.3" "28.2" "25.1" "25.2" "29.2" "29.3" "29.4" "30.1"
)
for version in "${emacs_versions[@]}"; do
  if ! download_extract_archive "https://mirror.ihost.md/gnu/emacs/emacs-$version.tar.gz" "emacs-$version"; then
    echo "Warning: Failed to process emacs-$version, continuing with next version..."
  fi
done

# gdb
echo "Downloading GDB..."
declare -a gdb_versions=(
  "10.1" "7.10.1" "5.2.1" "7.10" "5.3" "6.0" "7.11.1" "6.1.1" "6.7" "7.11" "6.1" "7.12.1" "6.2.1" "6.2" "7.12" "6.3" 
  "7.1" "6.4" "7.2" "6.5" "7.3.1" "6.6" "7.3" "6.7.1" "6.8" "7.4.1" "7.0.1" "7.0" "7.4" "7.5.1" "7.5" "7.6.1" "7.6.2" 
  "7.6" "7.7.1" "7.7" "7.8.1" "7.8.2" "7.8" "7.9.1" "7.9" "8.0.1" "8.0" "8.1.1" "8.1" "8.2.1" "8.2" "8.3.1" "8.3" "9.1" 
  "9.2" "10.2" "11.1" "11.2" "12.1" "13.1" "13.2" "14.1" "14.2" "15.1" "15.2" "16.1" "16.2"
)
for version in "${gdb_versions[@]}"; do
  if ! download_extract_archive "https://mirror.ihost.md/gnu/gdb/gdb-$version.tar.gz" "gdb-$version"; then
    echo "Warning: Failed to process gdb-$version, continuing with next version..."
  fi
done

# linux kernel
declare -a linux6_links=(
  "6.0.1" "6.0.10" "6.0.11" "6.0.12" "6.0.13" "6.0.14" "6.0.15" "6.0.16" "6.0.17" "6.0.18" "6.0.19" "6.0.2" "6.0.3" "6.0.4" 
  "6.0.5" "6.0.6" "6.0.7" "6.0.8" "6.0.9" "6.0" "6.1.1" "6.1.10" "6.1.100" "6.1.101" "6.1.102" "6.1.103" "6.1.104" "6.1.105"
   "6.1.106" "6.1.107" "6.1.108" "6.1.109" "6.1.11" "6.1.110" "6.1.111" "6.1.112" "6.1.113" "6.1.114" "6.1.115" "6.1.116" 
   "6.1.117" "6.1.118" "6.1.119" "6.1.12"
)
for version in "${linux6_links[@]}"; do
  if ! download_extract_archive "https://mirror.ihost.md/kernel/v6.x/linux-$version.tar.gz" "linux-$version"; then
    echo "Warning: Failed to process linux-$version, continuing with next version..."
  fi
done
