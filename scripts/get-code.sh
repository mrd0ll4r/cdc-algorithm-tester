#!/bin/bash -e

source scripts/utils.sh

# Downloads source code tarballs of gcc for 2019-2022 releases, extracts them, and collects the results into a tarball.

download_extract_archive() {
  local url=$1
  local name=$2
  echo "Downloading $name..."
  wget -q "$url"
  tar -xf "$name".tar.gz
  rm -f "$name".tar.gz
}

mkdir -p "$DATA_PATH/code"
cd "$DATA_PATH/code"

# gcc
echo "Downloading GCC..."
declare -a gcc_versions=(
  "12.2.0" "12.1.0" "11.3.0" "11.2.0" "11.1.0" "10.4.0" "10.3.0" "10.2.0" "10.1.0" "9.5.0" "9.4.0" "9.3.0" "9.2.0"
  "9.1.0" "8.5.0" "8.4.0" "8.3.0" "7.5.0"
)
for version in "${gcc_versions[@]}"; do
  download_extract_archive "http://ftpmirror.gnu.org/gcc/gcc-$version/gcc-$version.tar.gz" "gcc-$version"
done

# tarring
echo "Creating TAR archive..."
tar -cf ../code.tar .
cd ..
rm -r code
