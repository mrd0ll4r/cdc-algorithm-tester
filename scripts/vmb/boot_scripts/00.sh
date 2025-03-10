#!/bin/bash -e

# Day zero: Install OS-level dependencies and set up Rust.

# Wait for network to be up
while ! ping -c1 dns.google; do
  echo "waiting for network"
  sleep 1
done

# Dependencies for the actual builds
echo "Installing dependencies..."
apt-get -y update
DEBIAN_FRONTEND=noninteractive apt-get -y install \
    git \
    curl \
    build-essential \
    cmake

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

. "$HOME/.cargo/env"

rustup install 1.83

# Create builds directory
mkdir -p build

poweroff
