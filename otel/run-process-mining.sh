#!/usr/bin/env bash
# Run PM4Py process mining notebook via papermill
#
# Usage:
#   ./run-process-mining.sh                    # Default: last 24h
#   ./run-process-mining.sh --hours 6          # Last 6 hours
#   ./run-process-mining.sh --hours 1 --limit 500
#   ./run-process-mining.sh --output report.ipynb

set -euo pipefail

HOURS=24
LIMIT=1000
OUTPUT="/tmp/pm4py-output-$(date +%Y%m%d-%H%M%S).ipynb"
NOTEBOOK_DIR="$(cd "$(dirname "$0")" && pwd)"
NOTEBOOK="$NOTEBOOK_DIR/pm4py-process-mining.ipynb"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --hours)  HOURS="$2"; shift 2 ;;
    --limit)  LIMIT="$2"; shift 2 ;;
    --output) OUTPUT="$2"; shift 2 ;;
    --help)
      echo "Usage: $0 [--hours N] [--limit N] [--output FILE]"
      echo ""
      echo "Options:"
      echo "  --hours N     Time range in hours (default: 24)"
      echo "  --limit N     Max traces/logs to fetch (default: 1000)"
      echo "  --output F    Output notebook path (default: /tmp/pm4py-output-<timestamp>.ipynb)"
      exit 0
      ;;
    *) echo "Unknown option: $1"; exit 1 ;;
  esac
done

if [[ ! -f "$NOTEBOOK" ]]; then
  echo "Error: Notebook not found at $NOTEBOOK"
  exit 1
fi

echo "Running process mining analysis..."
echo "  Time range: last ${HOURS}h"
echo "  Limit: ${LIMIT} traces/logs"
echo "  Output: ${OUTPUT}"

docker exec unrdf-pm4py \
  papermill \
    "$NOTEBOOK" \
    "$OUTPUT" \
    -p hours "$HOURS" \
    -p limit "$LIMIT" \
    2>&1

if [[ $? -eq 0 ]]; then
  echo ""
  echo "Process mining complete. Output: $OUTPUT"
  echo ""
  echo "To view results:"
  echo "  docker cp unrdf-pm4py:$OUTPUT ./pm4py-results.ipynb"
  echo "  open ./pm4py-results.ipynb"
else
  echo "Process mining failed. Check the container logs:"
  echo "  docker logs unrdf-pm4py --tail 50"
fi
