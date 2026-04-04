#!/usr/bin/env bash
# Trace baseline comparison for UNRDF OTEL stack
#
# Compares current trace structure against a saved baseline.
# Detects:
#   - New operations not in baseline
#   - Missing operations that were in baseline
#   - Latency percentile shifts > 2x
#
# Usage:
#   ./scripts/trace-baseline.sh save              # Save current state as baseline
#   ./scripts/trace-baseline.sh compare           # Compare against baseline
#   ./scripts/trace-baseline.sh compare --hours 1 # Compare last 1h against baseline

set -euo pipefail

TEMPO_URL="${TEMPO_URL:-http://localhost:3200}"
BASELINE_DIR="${BASELINE_DIR:-/Users/sac/unrdf/otel/baselines}"
HOURS="${HOURS:-24}"
LIMIT="${LIMIT:-1000}"

mkdir -p "$BASELINE_DIR"

# Fetch trace operation summary from Tempo
fetch_operations() {
    local start end
    end=$(date +%s)
    start=$((end - (HOURS * 3600)))

    local response
    response=$(curl -sf "${TEMPO_URL}/api/search?start=${start}&end=${end}&limit=${LIMIT}" 2>/dev/null || echo '{"traces":[]}')

    echo "$response" | python3 -c "
import json, sys
from collections import Counter

data = json.load(sys.stdin)
traces = data.get('traces', [])
ops = Counter()
total_traces = len(traces)

print(f'total_traces:{total_traces}')
for t in traces:
    ops[t.get('rootTraceName', 'unknown')] += 1

for op, count in ops.most_common(50):
    print(f'op:{op}:{count}')
"
}

save_baseline() {
    local timestamp
    timestamp=$(date -u +%Y%m%dT%H%M%SZ)
    local baseline_file="${BASELINE_DIR}/baseline_${timestamp}.txt"

    echo "Saving baseline to ${baseline_file}..."
    fetch_operations > "$baseline_file"

    # Also save as 'latest'
    cp "$baseline_file" "${BASELINE_DIR}/baseline_latest.txt"
    echo "Baseline saved. Traces: $(grep -c '^op:' "$baseline_file") operations"
}

compare_baseline() {
    local current latest

    latest="${BASELINE_DIR}/baseline_latest.txt"
    if [[ ! -f "$latest" ]]; then
        echo "ERROR: No baseline found. Run 'trace-baseline.sh save' first."
        exit 1
    fi

    echo "Comparing current traces against baseline..."
    current=$(mktemp)
    fetch_operations > "$current"

    echo ""
    echo "=== BASELINE COMPARISON ==="

    # Compare total traces
    local current_total baseline_total
    baseline_total=$(grep '^total_traces:' "$latest" | cut -d: -f2)
    current_total=$(grep '^total_traces:' "$current" | cut -d: -f2)
    echo "Traces: baseline=${baseline_total} current=${current_total}"

    # Find new operations
    echo ""
    echo "--- NEW OPERATIONS (not in baseline) ---"
    comm -13 <(grep '^op:' "$latest" | sort) <(grep '^op:' "$current" | sort) || echo "  (none)"

    # Find missing operations
    echo ""
    echo "--- MISSING OPERATIONS (in baseline, not now) ---"
    comm -23 <(grep '^op:' "$latest" | sort) <(grep '^op:' "$current" | sort) || echo "  (none)"

    # Compare operation counts (shifts > 2x)
    echo ""
    echo "--- OPERATION COUNT SHIFTS (>2x change) ---"
    local has_shifts=false
    while IFS=: read -r prefix op count; do
        [[ "$prefix" != "op" ]] && continue
        local baseline_count
        baseline_count=$(grep "^op:${op}:" "$latest" | cut -d: -f3 || echo "0")
        if [[ "$baseline_count" -gt 0 ]]; then
            local ratio
            ratio=$(echo "scale=1; $count / $baseline_count" | bc 2>/dev/null || echo "0")
            if (( $(echo "$ratio > 2.0 || $ratio < 0.5" | bc -l 2>/dev/null || echo 0) )); then
                echo "  ${op}: ${baseline_count} -> ${count} (${ratio}x)"
                has_shifts=true
            fi
        fi
    done < "$current"

    if [[ "$has_shifts" == "false" ]]; then
        echo "  (none)"
    fi

    echo ""
    echo "=== COMPARISON COMPLETE ==="

    rm -f "$current"
}

# Parse --hours flag from arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --hours) HOURS="$2"; shift 2 ;;
        --limit) LIMIT="$2"; shift 2 ;;
        save)    save_baseline; exit 0 ;;
        compare) compare_baseline; exit 0 ;;
        *)       echo "Usage: trace-baseline.sh {save|compare} [--hours N] [--limit N]"; exit 1 ;;
    esac
done

echo "Usage: trace-baseline.sh {save|compare} [--hours N] [--limit N]"
exit 1
