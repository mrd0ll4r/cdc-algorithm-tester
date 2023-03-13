#!/bin/bash

download_extract_archive() {
  local url=$1
  local name=$2
  echo "Downloading $name..."
  wget -q $url
  tar -xf $name.tar.gz
  rm -f $name.tar.gz
}

path=data/code
mkdir -p $path
cd $path

# gcc
declare -a gcc_versions=(
  "11.1.0" "10.3.0" "10.2.0" "10.1.0" "9.4.0" "9.3.0" "9.2.0" "9.1.0" "8.5.0" "8.4.0" "8.3.0" "8.2.0" "8.1.0" "7.5.0"
  "7.4.0" "7.3.0" "7.2.0" "7.1.0" "6.5.0" "6.4.0"
)
for version in "${gcc_versions[@]}"
do
   download_extract_archive "https://mirror.linux-ia64.org/gnu/gcc/releases/gcc-$version/gcc-$version.tar.gz" "gcc-$version"
done

# gdb
declare -a gdb_versions=(
  "10.1" "10.2" "11.1" "11.2" "12.1" "6.6a" "6.7.1a" "6.7a" "6.8a" "7.0.1a" "7.0a" "7.10.1" "7.10" "7.11.1",
  "7.11" "7.12.1" "7.12" "7.1a" "7.2a" "7.3.1" "7.3a" "7.4.1" "7.4" "7.5.1" "7.5" "7.6.1" "7.6.2" "7.6",
  "7.7.1" "7.7" "7.8.1" "7.8.2" "7.8" "7.9.1" "7.9" "8.0.1" "8.0" "8.1.1" "8.1" "8.2.1" "8.2" "8.3.1" "8.3" "9.1" "9.2"
)
for version in "${gdb_versions[@]}"
do
   download_extract_archive "http://ftp.gwdg.de/pub/linux/sources.redhat.com/gdb/releases/gdb-${version}.tar.gz" "gdb-$version"
done

# emacs
echo "Downloading Emacs..."
emacs_tarball_urls=$(curl -s https://api.github.com/repos/emacs-mirror/emacs/tags \
| grep "tarball_url" \
| cut -d : -f 2,3 \
| tr -d \" \
| sed 's/,$//')

for u in $emacs_tarball_urls; do
    tag=$(echo $u | awk -F'/' '{print $NF}')
    wget "$u" -O "$tag"
    tar -xzf $tag
    rm $tag
done

# tarring
echo "Creating TAR archive..."
tar -cf ../code.tar .
cd .. && rm -r code
