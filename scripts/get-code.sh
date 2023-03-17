#!/bin/bash -e
# Downloads source code tarballs of gcc, gdb, and emacs of 2019-2022 releases, extracts them, and collects the results
# into a tarball.

download_extract_archive() {
  local url=$1
  local name=$2
  echo "Downloading $name..."
  wget -q "$url"
  tar -xf "$name".tar.gz
  rm -f "$name".tar.gz
}

path=data/code
mkdir -p $path
cd $path

# gcc
echo "Downloading GCC..."
declare -a gcc_versions=(
  "12.2.0" "12.1.0" "11.3.0" "11.2.0" "11.1.0" "10.4.0" "10.3.0" "10.2.0" "10.1.0" "9.5.0" "9.4.0" "9.3.0" "9.2.0"
  "9.1.0" "8.5.0" "8.4.0" "8.3.0" "7.5.0"
)
for version in "${gcc_versions[@]}"; do
  download_extract_archive "http://ftpmirror.gnu.org/gcc/gcc-$version/gcc-$version.tar.gz" "gcc-$version"
done

# gdb
echo "Downloading gdb..."
declare -a gdb_versions=(
  "10.1" "10.2" "11.1" "11.2" "12.1" "8.3.1" "8.3" "9.1" "9.2"
)
for version in "${gdb_versions[@]}"; do
  download_extract_archive "http://ftpmirror.gnu.org/gdb/gdb-${version}.tar.gz" "gdb-$version"
done

# emacs
echo "Downloading emacs..."
declare -a emacs_versions=(
  "26.2" "26.3" "27.1" "27.2" "28.1" "28.2"
)
for version in "${emacs_versions[@]}"; do
  download_extract_archive "http://ftpmirror.gnu.org/emacs/emacs-${version}.tar.gz" "emacs-$version"
done

# tarring
echo "Creating TAR archive..."
tar -cf ../code.tar .
cd ..
rm -r code
