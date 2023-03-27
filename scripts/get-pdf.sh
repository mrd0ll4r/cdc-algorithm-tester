#!/bin/bash -e

source scripts/utils.sh

mkdir -p "$DATA_PATH/pdf"
cd "$DATA_PATH/pdf"

for i in $(seq 1 20); do
  f=$(printf "arXiv_pdf_1801_%03d" "$i")
  aws s3 cp --request-payer requester "s3://arxiv/pdf/$f" .
  tar -xf "$f"
  rm "$f"
done

echo "Creating TAR archive..."
tar -cf ../pdf.tar .
cd ..
rm -r pdf