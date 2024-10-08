# Docker environment to build CDC Algorithm Tester binary in a stable
# environment.
# Compiled binaries can be found in /cdc-algo-tester/target/release/.
# Sources are copied into /cdc-algo-tester/.

FROM ubuntu:focal AS chef

# Get build dependencies for Rust itself.
RUN apt-get update && apt-get install -y \
  curl \
  build-essential \
  git

# Install Rust.
ENV RUSTUP_HOME=/usr/local/rustup CARGO_HOME=/usr/local/cargo PATH=/usr/local/cargo/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > install-rust.sh
RUN chmod +x install-rust.sh
RUN ./install-rust.sh -y
RUN rustup install nightly-2024-08-21

# Cargo-chef is used to build dependencies and cache them, for faster
# incremental builds.
RUN cargo install cargo-chef
WORKDIR cdc-algo-tester

FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder

# Install OS-level dependencies.
#RUN apt-get update && apt-get install -y \
#  libssl-dev \
#  protobuf-compiler \
#  pkg-config \
#  libpq-dev \
#  libmagic-dev

# Get a list of Rust dependencies to build.
COPY --from=planner /cdc-algo-tester/recipe.json recipe.json

# Build dependencies - this should be cached by docker.
RUN cargo +nightly-2024-08-21 chef cook --release --recipe-path recipe.json

# Build our project.
COPY . .
ENV RUSTFLAGS="-C target-cpu=native"
RUN cargo +nightly-2024-08-21 build --release --locked
