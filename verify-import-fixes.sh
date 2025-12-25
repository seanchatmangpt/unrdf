#!/bin/bash
# Verification script for import integrity fixes

set -e

echo "🔍 Import Integrity Verification Script"
echo "========================================"
echo ""

FAILED=0

echo "1. Checking for missing files..."
echo ""

check_file() {
  if [ -f "$1" ]; then
    echo "  ✅ $1"
  else
    echo "  ❌ MISSING: $1"
    FAILED=$((FAILED + 1))
  fi
}

check_file "/home/user/unrdf/packages/federation/src/federation/metrics.mjs"
check_file "/home/user/unrdf/packages/federation/utils/sparql-utils.mjs"
check_file "/home/user/unrdf/packages/streaming/src/validate.mjs"
check_file "/home/user/unrdf/packages/streaming/src/observability.mjs"

echo ""
echo "2. Checking package.json dependencies..."
echo ""

if grep -q "@opentelemetry/api" /home/user/unrdf/packages/federation/package.json; then
  echo "  ✅ @opentelemetry/api in federation package.json"
else
  echo "  ❌ MISSING: @opentelemetry/api in federation package.json"
  FAILED=$((FAILED + 1))
fi

echo ""
echo "3. Checking for forbidden N3 imports..."
echo ""

FORBIDDEN_COUNT=$(git diff --name-only HEAD~5 HEAD | grep '\.mjs$' | xargs grep -h "^import.*from" | grep -c "from 'n3'" || echo "0")

if [ "$FORBIDDEN_COUNT" -eq 0 ]; then
  echo "  ✅ No forbidden N3 imports"
else
  echo "  ❌ FOUND $FORBIDDEN_COUNT forbidden N3 imports"
  FAILED=$((FAILED + 1))
fi

echo ""
echo "4. Running basic import tests..."
echo ""

# Test if packages can be imported (within workspace context)
cd /home/user/unrdf/packages/yawl
if timeout 5s node -e "import('./src/index.mjs').then(() => console.log('  ✅ @unrdf/yawl imports'), err => { console.error('  ❌ @unrdf/yawl import failed:', err.message); process.exit(1); })" 2>&1 | grep -q "✅"; then
  :
else
  FAILED=$((FAILED + 1))
fi

cd /home/user/unrdf

echo ""
echo "========================================"
if [ $FAILED -eq 0 ]; then
  echo "✅ ALL CHECKS PASSED"
  exit 0
else
  echo "❌ $FAILED CHECKS FAILED"
  exit 1
fi
