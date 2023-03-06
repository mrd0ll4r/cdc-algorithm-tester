#!/bin/bash -e

mkdir -p out

# Build binary.
docker build -t cdc-algo-tester-builder -f Dockerfile.builder .

# Extract binaries to host.
docker create --name extract cdc-algo-tester-builder
docker cp extract:/cdc-algo-tester/target/release/cdc-algorithm-tester ./out/

docker rm extract