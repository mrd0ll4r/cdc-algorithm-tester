#!/bin/bash -e

# Day n: Build all projects on a given date.
# The date is provided via /root/build.date
# Commit hashes and repo URIs are read from /root/commit_hashes.csv and /root/repo_uris.csv, respectively.

# Wait for network to be up
while ! ping -c1 dns.google; do
  echo "waiting for network"
  sleep 1
done

# Read current "date"
today=$(cat /root/build.date)
echo "Today is $today"

# For each project to be built today...
cat commit_hashes.csv |
	grep -F "$today" |
	while read -r line; do
		echo "Working on $line..."

		project_name=$(echo $line | cut -d',' -f2)
		commit_hash=$(echo $line | cut -d',' -f3)

		# Get project repo URI
		repo_uri=$(cat repo_uris.csv | grep -F "$project_name" | cut -d',' -f2)

		# Build
		/root/build_repo_commit.sh "$repo_uri" "$commit_hash" "$project_name"
	done

poweroff
