#!/bin/bash
# UNRDF Multiverse Beta Test Harness
# 7-Day Soak Test - Daily Execution Script

set -e

BETA_DAY="${1:-1}"
# Resolve from this script, not from a hardcoded checkout location
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
LOG_DIR="$SCRIPT_DIR/logs"
RESULTS_FILE="$LOG_DIR/beta-day-${BETA_DAY}-$(date +%Y%m%d-%H%M%S).json"

mkdir -p "$LOG_DIR"

echo "=== UNRDF MULTIVERSE BETA TEST - DAY $BETA_DAY ==="
echo "Start Time: $(date --iso-8601=seconds)"
echo "Results: $RESULTS_FILE"
echo ""

# Track metrics
FAILED_TESTS=0
START_TIME=$(date +%s)

# Helper function
FIRST_RESULT=1
record_result() {
  # Emit a comma between array entries so the results file is valid JSON
  if [ "$FIRST_RESULT" -eq 1 ]; then FIRST_RESULT=0; else echo "  ," >> "$RESULTS_FILE"; fi
  echo "  $1" >> "$RESULTS_FILE"
}

run_test() {
  local name="$1"
  local cmd="$2"
  local timeout_sec="${3:-30}"

  echo "[$name] Starting..."
  local test_start=$(date +%s)

  if timeout ${timeout_sec}s bash -c "$cmd" > "$LOG_DIR/${name}.log" 2>&1; then
    local test_end=$(date +%s)
    local duration=$((test_end - test_start))
    echo "[$name] ✅ PASSED (${duration}s)"
    record_result "{\"test\": \"$name\", \"status\": \"PASS\", \"duration\": ${duration}}"
  else
    local test_end=$(date +%s)
    local duration=$((test_end - test_start))
    echo "[$name] ❌ FAILED (${duration}s)"
    record_result "{\"test\": \"$name\", \"status\": \"FAIL\", \"duration\": ${duration}}"
    FAILED_TESTS=$((FAILED_TESTS + 1))
  fi
}

# Start results JSON
echo "{\"beta_day\": $BETA_DAY, \"timestamp\": \"$(date --iso-8601=seconds)\", \"results\": [" > "$RESULTS_FILE"

# Test 1: 10k Universe Creation Stress Test
run_test "10k-universe-stress" \
  "cd \"$ROOT\" && node benchmarks/10k-system.mjs" \
  60

# Test 2: 10k Morphism Applications
run_test "10k-morphism-stress" \
  "cd \"$ROOT\" && pnpm --filter @unrdf/kgc-multiverse exec vitest run -t 'applies morphism to 1000' --reporter json" \
  30

# Test 3: Concurrent Receipt Generation
run_test "concurrent-receipts" \
  "cd \"$ROOT\" && pnpm --filter @unrdf/kgc-multiverse exec vitest run -t 'receipt' --reporter json" \
  20

# Test 4: Worker Pool Scaling (2-12 workers)
run_test "worker-pool-scaling" \
  "cd \"$ROOT\" && for workers in 2 4 8 12; do WORKER_COUNT=\$workers node benchmarks/10k-system.mjs --workers \$workers || exit 1; done" \
  90

# Test 5: Memory Stability Monitoring
run_test "memory-stability" \
  "cd \"$ROOT\" && node beta-test/memory-monitor.mjs" \
  15

# Test 6: Error Injection & Recovery
run_test "error-injection" \
  "cd \"$ROOT\" && node beta-test/error-injection.mjs" \
  20

# Test 7: Performance Consistency Checks
run_test "performance-consistency" \
  "cd \"$ROOT\" && node beta-test/performance-check.mjs" \
  25

# Test 8: All Unit Tests Pass
run_test "unit-tests-full" \
  "cd \"$ROOT\" && timeout 5s npm test" \
  5

# Close results JSON
echo "  ], \"failed_count\": $FAILED_TESTS, \"end_timestamp\": \"$(date --iso-8601=seconds)\"}" >> "$RESULTS_FILE"

END_TIME=$(date +%s)
TOTAL_DURATION=$((END_TIME - START_TIME))

echo ""
echo "=== BETA TEST SUMMARY ==="
echo "Total Duration: ${TOTAL_DURATION}s"
echo "Failed Tests: $FAILED_TESTS"
echo "Results: $RESULTS_FILE"

if [ $FAILED_TESTS -eq 0 ]; then
  echo "✅ ALL TESTS PASSED"
  exit 0
else
  echo "❌ $FAILED_TESTS TESTS FAILED"
  exit 1
fi
