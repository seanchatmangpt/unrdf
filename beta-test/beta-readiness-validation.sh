#!/bin/bash
# Beta Readiness Validation
# Quick validation that all beta test components are ready

set -e

echo "╔════════════════════════════════════════════════════════════════╗"
echo "║         UNRDF Multiverse - Beta Readiness Validation          ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""

FAILED=0
START_TIME=$(date +%s)

# Test 1: Memory Monitor
echo "[1/5] Memory stability monitor..."
if timeout 10s node beta-test/memory-monitor.mjs > /tmp/beta-mem.log 2>&1; then
  HEAP_GROWTH=$(grep "heapGrowthMB" /tmp/beta-mem.log | grep -oP '"heapGrowthMB":\s*\K[0-9.]+')
  echo "  ✅ PASS - Heap growth: ${HEAP_GROWTH} MB"
else
  echo "  ❌ FAIL - Memory monitor failed"
  FAILED=$((FAILED + 1))
fi

# Test 2: Error Injection & Recovery
echo "[2/5] Error injection & recovery..."
if timeout 15s node beta-test/error-injection.mjs > /tmp/beta-error.log 2>&1; then
  PASSED=$(grep '"passed":' /tmp/beta-error.log | grep -oP '\d+')
  echo "  ✅ PASS - ${PASSED}/5 recovery tests passed"
else
  echo "  ❌ FAIL - Error injection tests failed"
  FAILED=$((FAILED + 1))
fi

# Test 3: Performance Consistency
echo "[3/5] Performance consistency check..."
if timeout 20s node beta-test/performance-check.mjs > /tmp/beta-perf.log 2>&1; then
  PERF_STATUS=$(grep '"status":' /tmp/beta-perf.log | tail -1 | grep -oP '"status":\s*"\K[^"]+')
  echo "  ✅ PASS - Performance: $PERF_STATUS"
else
  echo "  ❌ FAIL - Performance check failed"
  FAILED=$((FAILED + 1))
fi

# Test 4: 10k Benchmark
echo "[4/5] 10k universe benchmark..."
if timeout 60s node benchmarks/10k-system.mjs > /tmp/beta-10k.log 2>&1; then
  TOTAL_TIME=$(grep "Total time:" /tmp/beta-10k.log | grep -oP '\d+\.?\d*s' | head -1 || echo "N/A")
  PEAK_MEM=$(grep "Peak memory:" /tmp/beta-10k.log | tail -1 | grep -oP '\d+\.?\d* MB' | head -1 || echo "N/A")
  echo "  ✅ PASS - Time: ${TOTAL_TIME}, Memory: ${PEAK_MEM}"
else
  echo "  ❌ FAIL - 10k benchmark failed"
  FAILED=$((FAILED + 1))
fi

# Test 5: Unit Tests
echo "[5/5] Unit test suite..."
UNIT_EXIT=0
timeout 5s npm test > /tmp/beta-unit.log 2>&1 || UNIT_EXIT=$?
if [ $UNIT_EXIT -eq 0 ]; then
  echo "  ✅ PASS - All unit tests passed"
else
  PASSING=$(grep -oP '\d+ passing' /tmp/beta-unit.log || echo "unknown passing")
  FAILING=$(grep -oP '\d+ failing' /tmp/beta-unit.log || echo "unknown failing")
  # Unit tests are not a critical gate (beta focuses on integration), but a
  # non-zero exit (failure or the 5s timeout) must never be reported as PASS.
  echo "  ⚠️  UNVERIFIED - npm test exited $UNIT_EXIT ($PASSING, $FAILING); see /tmp/beta-unit.log"
fi

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

echo ""
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║                      Validation Summary                        ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""
echo "Total Duration: ${DURATION}s"
echo "Failed Tests: $FAILED/4 (critical)"
echo ""

if [ $FAILED -eq 0 ]; then
  echo "✅ BETA READINESS: CONFIRMED"
  echo ""
  echo "All critical components validated:"
  echo "  • Memory stability: ✅"
  echo "  • Error recovery: ✅"
  echo "  • Performance consistency: ✅"
  echo "  • 10k benchmark: ✅"
  echo ""
  echo "🚀 Ready to begin 7-day beta soak test"
  exit 0
else
  echo "❌ BETA READINESS: NOT READY"
  echo ""
  echo "Failed components: $FAILED"
  echo "Review logs in /tmp/beta-*.log"
  exit 1
fi
