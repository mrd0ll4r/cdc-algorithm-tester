#!/bin/bash -e

# Script to build a project at a given commit hash.
# This fetches the repository at the given commit and executes cargo build --locked.

REPO_URL=$1
COMMIT_HASH=$2
PROJECT_NAME=$3

echo "building project $PROJECT_NAME ($REPO_URL) at commit hash $COMMIT_HASH"

cd ~/build

if [ ! -d "$PROJECT_NAME" ]; then
	# does not exist yet, clone
	first_checkout=TRUE
	git clone --depth=1 "$REPO_URL"
fi

cd "$PROJECT_NAME"
git fetch --depth=1 "$REPO_URL" "$COMMIT_HASH"
git checkout "$COMMIT_HASH"
if [ "$first_checkout" = "TRUE" ]; then
	git branch -D @{-1}
fi
cargo build --locked
