#!/bin/bash

DIR="${1:-.}"

find "$DIR" -type f -name "*.hs" -print0 | while IFS= read -r -d '' file; do
    lines=$(wc -l < "$file")
    echo -e "$lines\t$file"
done | sort -rn
