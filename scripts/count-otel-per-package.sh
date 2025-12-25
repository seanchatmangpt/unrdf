#!/bin/bash

echo "OTEL Contamination by Package:"
echo "=============================="

for pkg_dir in packages/*; do
  if [ -d "$pkg_dir/src" ]; then
    pkg_name=$(basename "$pkg_dir")
    count=$(find "$pkg_dir/src" -name "*.mjs" -exec grep -l "@opentelemetry" {} \; 2>/dev/null | wc -l)
    if [ "$count" -gt 0 ]; then
      echo "$pkg_name: $count files"
    fi
  fi
done
