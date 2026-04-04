#!/usr/bin/env bash
# PM4Py conformance gate for CI/CD
#
# Runs process mining and fails if conformance drops below threshold.
# Designed for use as a CI check step.
#
# Usage:
#   ./scripts/pm4py-conformance-gate.sh --min-fitness 0.8 --min-precision 0.5
#
# Exit codes:
#   0 - Conformance passes
#   1 - Conformance below threshold
#   2 - Error running analysis

set -euo pipefail

MIN_FITNESS="${MIN_FITNESS:-0.8}"
MIN_PRECISION="${MIN_PRECISION:-0.5}"
HOURS="${HOURS:-24}"
LIMIT="${LIMIT:-1000}"

# Parse flags
while [[ $# -gt 0 ]]; do
    case "$1" in
        --min-fitness)  MIN_FITNESS="$2"; shift 2 ;;
        --min-precision) MIN_PRECISION="$2"; shift 2 ;;
        --hours)        HOURS="$2"; shift 2 ;;
        --limit)        LIMIT="$2"; shift 2 ;;
        --help)
            echo "Usage: pm4py-conformance-gate.sh [--min-fitness F] [--min-precision P] [--hours N] [--limit N]"
            echo ""
            echo "Options:"
            echo "  --min-fitness F     Minimum fitness threshold (default: 0.8)"
            echo "  --min-precision P   Minimum precision threshold (default: 0.5)"
            echo "  --hours N           Time range in hours (default: 24)"
            echo "  --limit N           Max traces to analyze (default: 1000)"
            echo ""
            echo "Exit codes: 0=pass, 1=below threshold, 2=error"
            exit 0
            ;;
        *) echo "Unknown option: $1"; exit 2 ;;
    esac
done

echo "PM4Py Conformance Gate"
echo "  Min fitness:    ${MIN_FITNESS}"
echo "  Min precision:  ${MIN_PRECISION}"
echo "  Time range:     last ${HOURS}h"
echo ""

# Run analysis
RESULT=$(docker run --rm \
    --network otel_default \
    -e TEMPO_URL=http://tempo:3200 \
    -e LOKI_URL=http://loki:3100 \
    -e MIN_FITNESS="${MIN_FITNESS}" \
    -e MIN_PRECISION="${MIN_PRECISION}" \
    ostar-pm4py \
    python3 -c "
import json, sys, os
sys.path.insert(0, '/app')
from pm4py_analyze import fetch_traces_from_tempo, discover_process_model, conformance_check
import pandas as pd

df = fetch_traces_from_tempo(hours=${HOURS}, limit=${LIMIT})
if len(df) == 0:
    print(json.dumps({'error': 'no_traces'}))
    sys.exit(0)

net, im, fm = discover_process_model(df)
result = conformance_check(df, net, im, fm)

output = {'trace_events': len(df), 'unique_traces': df['case:concept:name'].nunique()}
if result:
    output['fitness'] = result.get('fitness', {}).get('average_trace_fitness', 0)
    output['precision'] = result.get('precision', 0)
else:
    output['fitness'] = 0
    output['precision'] = 0

print(json.dumps(output))
" 2>&1) || {
    echo "ERROR: Failed to run PM4Py analysis (exit code $?)"
    exit 2
}

echo "Result: $RESULT"

# Parse result
FITNESS=$(echo "$RESULT" | python3 -c "import json,sys; print(json.load(sys.stdin).get('fitness', 0))" 2>/dev/null || echo "0")
PRECISION=$(echo "$RESULT" | python3 -c "import json,sys; print(json.load(sys.stdin).get('precision', 0))" 2>/dev/null || echo "0")

echo ""
echo "  Fitness:   ${FITNESS} (threshold: ${MIN_FITNESS})"
echo "  Precision: ${PRECISION} (threshold: ${MIN_PRECISION})"
echo ""

PASS=true
if (( $(echo "$FITNESS < $MIN_FITNESS" | bc -l 2>/dev/null || echo 1) )); then
    echo "FAIL: Fitness ${FITNESS} below threshold ${MIN_FITNESS}"
    PASS=false
fi

if (( $(echo "$PRECISION < $MIN_PRECISION" | bc -l 2>/dev/null || echo 1) )); then
    echo "FAIL: Precision ${PRECISION} below threshold ${MIN_PRECISION}"
    PASS=false
fi

if $PASS; then
    echo "PASS: Conformance gate cleared"
    exit 0
else
    exit 1
fi
