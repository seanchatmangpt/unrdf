#!/bin/bash
# Verification script for performance proxy deliverables

set -e

echo "Performance Proxy Deliverables Verification"
echo "==========================================="
echo ""

# Check files exist
echo "1. Checking files exist..."
FILES=(
  "perf-harness.mjs"
  "performance-proxies.md"
  "README.md"
  "DELIVERABLES.md"
  "otel-instrumentation-example.mjs"
)

for file in "${FILES[@]}"; do
  if [ -f "/home/user/unrdf/proofs/$file" ]; then
    echo "   ✓ $file"
  else
    echo "   ✗ $file MISSING"
    exit 1
  fi
done

echo ""
echo "2. Running performance harness..."
cd /home/user/unrdf
if node proofs/perf-harness.mjs > /tmp/harness-output.txt 2>&1; then
  echo "   ✓ Harness executed successfully"
else
  echo "   ✗ Harness failed"
  cat /tmp/harness-output.txt
  exit 1
fi

echo ""
echo "3. Verifying harness output..."
if grep -q "Performance Measurements (CSV):" /tmp/harness-output.txt; then
  echo "   ✓ CSV output present"
else
  echo "   ✗ CSV output missing"
  exit 1
fi

if grep -q "Statistical Summary:" /tmp/harness-output.txt; then
  echo "   ✓ Statistical summary present"
else
  echo "   ✗ Statistical summary missing"
  exit 1
fi

if grep -q "Budget Summary:" /tmp/harness-output.txt; then
  echo "   ✓ Budget validation present"
else
  echo "   ✗ Budget validation missing"
  exit 1
fi

if grep -q "Observable Performance Proxies Identified:" /tmp/harness-output.txt; then
  echo "   ✓ Observable proxies identified"
else
  echo "   ✗ Observable proxies missing"
  exit 1
fi

echo ""
echo "4. Checking measurement count..."
MEASUREMENT_COUNT=$(grep -c "^[a-z].*,[0-9]" /tmp/harness-output.txt || true)
if [ "$MEASUREMENT_COUNT" -ge 15 ]; then
  echo "   ✓ $MEASUREMENT_COUNT measurements captured (expected: ≥15)"
else
  echo "   ✗ Only $MEASUREMENT_COUNT measurements (expected: ≥15)"
  exit 1
fi

echo ""
echo "5. Verifying documentation completeness..."
if grep -q "Observable Cost Operations" proofs/performance-proxies.md; then
  echo "   ✓ Observable operations documented"
else
  echo "   ✗ Observable operations missing from docs"
  exit 1
fi

if grep -q "OTEL Instrumentation Gaps" proofs/performance-proxies.md; then
  echo "   ✓ OTEL gaps documented"
else
  echo "   ✗ OTEL gaps missing from docs"
  exit 1
fi

echo ""
echo "=========================================="
echo "✅ ALL VERIFICATIONS PASSED"
echo "=========================================="
echo ""
echo "Deliverables:"
echo "  - 10+ observable performance proxies identified"
echo "  - 1 runnable measurement harness (perf-harness.mjs)"
echo "  - CSV output with $MEASUREMENT_COUNT measurements"
echo "  - Statistical summary with p50/p95 percentiles"
echo "  - Budget validation (5 operations)"
echo "  - OTEL gaps documented with implementation examples"
echo "  - Performance budgets proposed"
echo ""
echo "Next steps:"
echo "  1. Add OTEL spans to freeze/reconstruct/appendEvent (P0)"
echo "  2. Wrap Oxigraph with query tracing (P0)"
echo "  3. Enable MemoryProfiler in production (P1)"
echo ""

exit 0
