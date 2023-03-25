#!/bin/bash -e

source scripts/utils.sh

for i in $(seq -w 1 29); do
  aws s3 cp --request-payer requester "s3://arxiv/pdf/arXiv_pdf_1801_0$i.tar" "$DATA_PATH/pdf/."
done

echo "Creating TAR archive..."
cat "$DATA_PATH/pdf/*.tar" > "$DATA_PATH/pdf.tar"
rm -r "$DATA_PATH/pdf"
