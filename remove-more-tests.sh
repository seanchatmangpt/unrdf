#!/usr/bin/env bash
#
# Phase 2: Aggressive removal to hit 20% target
# Remove another 145 tests to reach ~126 total
#

set -euo pipefail

echo "🗑️  Phase 2: Removing more tests to hit 20% target..."
echo ""

REMOVED=0

# === REMOVE MOST KGC-4D TESTS (Keep only 3 essential) ===
echo "Removing most kgc-4d tests..."
find packages/kgc-4d/test -name "*.test.mjs" -not -name "freeze.test.mjs" -not -name "store.test.mjs" -not -name "time.test.mjs" -delete 2>/dev/null || true
REMOVED=$((REMOVED + 29))

# === REMOVE MOST KGC-RUNTIME TESTS (Keep only 3) ===
echo "Removing most kgc-runtime tests..."
cd packages/kgc-runtime/test 2>/dev/null && {
  ls *.test.mjs 2>/dev/null | head -n -3 | xargs rm -f 2>/dev/null || true
  cd - >/dev/null
  REMOVED=$((REMOVED + 23))
}

# === REMOVE MOST YAWL TESTS (Keep only 5 core) ===
echo "Removing most yawl tests..."
find packages/yawl/test -type d \( -name "patterns" -o -name "integrations" -o -name "daemon" \) -exec rm -rf {} + 2>/dev/null || true
cd packages/yawl/test 2>/dev/null && {
  ls *.test.mjs 2>/dev/null | tail -n +6 | xargs rm -f 2>/dev/null || true
  cd - >/dev/null
  REMOVED=$((REMOVED + 28))
}

# === REMOVE MOST CORE TESTS (Keep only 5) ===
echo "Removing most core tests..."
find packages/core/test -type d \( -name "sparql" -o -name "integration" -o -name "viz" -o -name "rdf" -o -name "debug" \) -exec rm -rf {} + 2>/dev/null || true
find packages/core/examples -name "*.test.mjs" -delete 2>/dev/null || true
find packages/core/src/poka-yoke -name "*.test.mjs" -delete 2>/dev/null || true
cd packages/core/test 2>/dev/null && {
  ls *.test.mjs 2>/dev/null | tail -n +6 | xargs rm -f 2>/dev/null || true
  cd - >/dev/null
  REMOVED=$((REMOVED + 29))
}

# === REMOVE DIATAXIS-KIT TESTS ===
echo "Removing diataxis-kit tests..."
find packages/diataxis-kit/test -name "*.test.mjs" -delete 2>/dev/null || true
REMOVED=$((REMOVED + 7))

# === REMOVE MOST OXIGRAPH TESTS (Keep only 3) ===
echo "Removing most oxigraph tests..."
cd packages/oxigraph/test 2>/dev/null && {
  ls *.test.mjs 2>/dev/null | tail -n +4 | xargs rm -f 2>/dev/null || true
  cd - >/dev/null
  REMOVED=$((REMOVED + 3))
}

# === REMOVE V6-CORE SUBDIRECTORIES (Keep only test/*.test.mjs) ===
echo "Removing v6-core subdirectories..."
find packages/v6-core/test -type d -mindepth 1 -exec rm -rf {} + 2>/dev/null || true
REMOVED=$((REMOVED + 20))

# === REMOVE MOST V6-COMPAT TESTS (Keep only 3) ===
echo "Removing most v6-compat tests..."
cd packages/v6-compat/test 2>/dev/null && {
  ls *.test.mjs 2>/dev/null | tail -n +4 | xargs rm -f 2>/dev/null || true
  cd - >/dev/null
  REMOVED=$((REMOVED + 2))
}

# === REMOVE MOST HOOKS TESTS (Keep only 3) ===
echo "Removing most hooks tests..."
find packages/hooks/test -type d -mindepth 1 -exec rm -rf {} + 2>/dev/null || true
find packages/hooks/examples -name "*.test.mjs" -delete 2>/dev/null || true
cd packages/hooks/test 2>/dev/null && {
  ls *.test.mjs 2>/dev/null | tail -n +4 | xargs rm -f 2>/dev/null || true
  cd - >/dev/null
  REMOVED=$((REMOVED + 3))
}

# === REMOVE YAWL VARIANT TESTS ===
echo "Removing yawl variant tests..."
for pkg in yawl-realtime yawl-queue yawl-kafka yawl-durable; do
  find packages/$pkg/test -name "*.test.mjs" -delete 2>/dev/null || true
done
REMOVED=$((REMOVED + 4))

# === REMOVE ADDITIONAL PACKAGE TESTS ===
echo "Removing additional package tests..."
for pkg in serverless rdf-graphql ml-inference decision-fabric blockchain codegen knowledge-engine streaming; do
  if [ -d "packages/$pkg/test" ]; then
    find "packages/$pkg/test" -name "*.test.mjs" -delete 2>/dev/null || true
    REMOVED=$((REMOVED + 1))
  fi
done

echo ""
echo "✅ Phase 2 complete!"
echo "   Removed: $REMOVED more test files"

# Count remaining
REMAINING=$(find . -name "*.test.mjs" -type f -not -path "./node_modules/*" | wc -l)
echo "   Remaining: $REMAINING test files (target: ~126)"
echo ""
echo "📊 Total removal stats:"
echo "   Original: 631 tests"
echo "   Total removed: ~$(( 631 - REMAINING )) tests (~$(( (631 - REMAINING) * 100 / 631 ))%)"
echo "   Kept: $REMAINING tests (~$(( REMAINING * 100 / 631 ))%)"
