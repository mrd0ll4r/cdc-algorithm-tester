#!/bin/bash -e

source scripts/utils.sh

mkdir -p "$DATA_PATH/pdf"
cd "$DATA_PATH/pdf"

for i in {01..29}; do
  aws s3 cp --request-payer requester s3://arxiv/pdf/arXiv_pdf_1801_0$i.tar .
done

echo "Creating TAR archive..."
cat *.tar > ../pdf.tar
cd ..
rm -r pdf