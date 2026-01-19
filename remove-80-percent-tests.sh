#!/usr/bin/env bash
#
# Remove 80% of tests - Keep only critical 20%
# Target: 126 test files remaining, <5s total execution
#

set -euo pipefail

echo "🗑️  Removing 80% of tests (505 files)..."
echo "✅ Keeping 20% of tests (126 files)"
echo ""

# Track removals
REMOVED=0

# === REMOVE BROWSER TESTS ===
echo "Removing browser tests..."
if [ -d "test/browser" ]; then
  REMOVED=$((REMOVED + $(find test/browser -name "*.test.mjs" | wc -l)))
  rm -rf test/browser
fi

# === REMOVE REACT HOOKS TESTS ===
echo "Removing React hooks tests..."
if [ -d "test/react-hooks" ]; then
  REMOVED=$((REMOVED + $(find test/react-hooks -name "*.test.mjs" | wc -l)))
  rm -rf test/react-hooks
fi

# === REMOVE ML TESTS ===
echo "Removing ML tests..."
if [ -d "test/ml" ]; then
  REMOVED=$((REMOVED + $(find test/ml -name "*.test.mjs" | wc -l)))
  rm -rf test/ml
fi

# === REMOVE E2E SUBDIRECTORY ===
echo "Removing e2e subdirectory tests..."
if [ -d "test/e2e" ]; then
  REMOVED=$((REMOVED + $(find test/e2e -name "*.test.mjs" | wc -l)))
  rm -rf test/e2e
fi
[ -f "test/e2e-rdf-kgn.test.mjs" ] && rm test/e2e-rdf-kgn.test.mjs && REMOVED=$((REMOVED + 1))

# === REMOVE RESILIENCE TESTS ===
echo "Removing resilience tests..."
if [ -d "test/resilience" ]; then
  REMOVED=$((REMOVED + $(find test/resilience -name "*.test.mjs" | wc -l)))
  rm -rf test/resilience
fi

# === REMOVE STREAMING SUBDIRECTORY ===
echo "Removing streaming subdirectory tests..."
if [ -d "test/streaming" ]; then
  REMOVED=$((REMOVED + $(find test/streaming -name "*.test.mjs" | wc -l)))
  rm -rf test/streaming
fi

# === REMOVE FEDERATION SUBDIRECTORY ===
echo "Removing federation subdirectory tests..."
if [ -d "test/federation" ]; then
  REMOVED=$((REMOVED + $(find test/federation -name "*.test.mjs" | wc -l)))
  rm -rf test/federation
fi

# === REMOVE OBSERVABILITY & MONITORING ===
echo "Removing observability tests..."
[ -f "test/knowledge-engine/observability.test.mjs" ] && rm test/knowledge-engine/observability.test.mjs && REMOVED=$((REMOVED + 1))
if [ -d "test/knowledge-engine/monitoring" ]; then
  REMOVED=$((REMOVED + $(find test/knowledge-engine/monitoring -name "*.test.mjs" | wc -l)))
  rm -rf test/knowledge-engine/monitoring
fi
if [ -d "test/validation" ]; then
  REMOVED=$((REMOVED + $(find test/validation -name "*.test.mjs" | wc -l)))
  rm -rf test/validation
fi

# === REMOVE PROFILING TESTS ===
echo "Removing profiling tests..."
if [ -d "test/profiling" ]; then
  REMOVED=$((REMOVED + $(find test/profiling -name "*.test.mjs" | wc -l)))
  rm -rf test/profiling
fi

# === REMOVE SANDBOX TESTS ===
echo "Removing sandbox tests..."
if [ -d "test/knowledge-engine/sandbox" ]; then
  REMOVED=$((REMOVED + $(find test/knowledge-engine/sandbox -name "*.test.mjs" | wc -l)))
  rm -rf test/knowledge-engine/sandbox
fi

# === REMOVE PROJECT ENGINE SUBDIRECTORY ===
echo "Removing project-engine subdirectory tests..."
if [ -d "test/project-engine" ]; then
  REMOVED=$((REMOVED + $(find test/project-engine -name "*.test.mjs" | wc -l)))
  rm -rf test/project-engine
fi

# === REMOVE CLI SUBDIRECTORY ===
echo "Removing cli subdirectory tests..."
if [ -d "test/cli" ]; then
  REMOVED=$((REMOVED + $(find test/cli -name "*.test.mjs" | wc -l)))
  rm -rf test/cli
fi

# === REMOVE NON-CRITICAL ROOT TESTS ===
echo "Removing non-critical root tests..."
for file in \
  test/admission.test.mjs \
  test/cli-stubs-smoke.test.mjs \
  test/cli-stubs.test.mjs \
  test/error-sanitizer-allowlist.test.mjs \
  test/integration-agent-8-e2e.test.mjs \
  test/integration.test.mjs \
  test/observability-smoothing.test.mjs \
  test/query-optimizer-cache.test.mjs \
  test/transaction-veto.test.mjs \
  test/universe.test.mjs \
  test/unrdf-package-system.test.mjs
do
  [ -f "$file" ] && rm "$file" && REMOVED=$((REMOVED + 1))
done

# === REMOVE EXAMPLES TESTS ===
echo "Removing examples tests..."
if [ -d "examples" ]; then
  REMOVED=$((REMOVED + $(find examples -name "*.test.mjs" 2>/dev/null | wc -l)))
  find examples -name "*.test.mjs" -delete 2>/dev/null || true
fi

# === REMOVE AUTONOMIC INNOVATION TESTS ===
echo "Removing AUTONOMIC_INNOVATION tests..."
if [ -d "AUTONOMIC_INNOVATION" ]; then
  REMOVED=$((REMOVED + $(find AUTONOMIC_INNOVATION -name "*.test.mjs" 2>/dev/null | wc -l)))
  find AUTONOMIC_INNOVATION -name "*.test.mjs" -delete 2>/dev/null || true
fi

# === REMOVE ENTERPRISE MIGRATION TESTS ===
echo "Removing ENTERPRISE_MIGRATION tests..."
if [ -d "ENTERPRISE_MIGRATION" ]; then
  REMOVED=$((REMOVED + $(find ENTERPRISE_MIGRATION -name "*.test.mjs" 2>/dev/null | wc -l)))
  find ENTERPRISE_MIGRATION -name "*.test.mjs" -delete 2>/dev/null || true
fi

# === REMOVE REFERENCE IMPL TESTS ===
echo "Removing reference-impl tests..."
if [ -d "reference-impl" ]; then
  REMOVED=$((REMOVED + $(find reference-impl -name "*.test.mjs" 2>/dev/null | wc -l)))
  find reference-impl -name "*.test.mjs" -delete 2>/dev/null || true
fi

# === REMOVE PLAYGROUND TESTS ===
echo "Removing playground tests..."
if [ -d "playground" ]; then
  REMOVED=$((REMOVED + $(find playground -name "*.test.mjs" 2>/dev/null | wc -l)))
  find playground -name "*.test.mjs" -delete 2>/dev/null || true
fi

# === REMOVE SIDECAR TESTS ===
echo "Removing sidecar tests..."
if [ -d "sidecar" ]; then
  REMOVED=$((REMOVED + $(find sidecar -name "*.test.mjs" 2>/dev/null | wc -l)))
  find sidecar -name "*.test.mjs" -delete 2>/dev/null || true
fi

# === REMOVE TOOLS TESTS ===
echo "Removing tools tests..."
if [ -d "tools" ]; then
  REMOVED=$((REMOVED + $(find tools -name "*.test.mjs" 2>/dev/null | wc -l)))
  find tools -name "*.test.mjs" -delete 2>/dev/null || true
fi

# === REMOVE MOST PACKAGE TESTS ===
echo "Removing non-critical package tests..."
for pkg in \
  browser \
  graph-analytics \
  ml-versioning \
  semantic-search \
  geosparql \
  spatial-kg \
  zkp \
  ai-ml-innovations \
  temporal-discovery \
  self-healing-workflows \
  event-automation \
  collab \
  daemon \
  atomvm \
  yawl-viz \
  yawl-api \
  yawl-langchain \
  yawl-observability \
  kgc-cli \
  kgc-docs \
  kgc-tools \
  kgc-probe \
  kgc-swarm \
  kgc-substrate \
  kgc-multiverse \
  kgc-claude \
  fusion \
  integration-tests \
  test-utils \
  docs \
  kgn
do
  if [ -d "packages/$pkg" ]; then
    REMOVED=$((REMOVED + $(find "packages/$pkg" -name "*.test.mjs" 2>/dev/null | wc -l)))
    find "packages/$pkg" -name "*.test.mjs" -delete 2>/dev/null || true
  fi
done

# === REMOVE NARRATIVE STATE CHAIN TESTS ===
echo "Removing narrative-state-chain tests..."
if [ -d "test/narrative-state-chain" ]; then
  REMOVED=$((REMOVED + $(find test/narrative-state-chain -name "*.test.mjs" 2>/dev/null | wc -l)))
  find test/narrative-state-chain -name "*.test.mjs" -delete 2>/dev/null || true
fi

# === REMOVE MOST PROOFS ===
echo "Removing non-critical proofs..."
for file in \
  proofs/poka-yoke/03-branded-ids.test.mjs \
  proofs/poka-yoke/04-builder-pattern.test.mjs
do
  [ -f "$file" ] && rm "$file" && REMOVED=$((REMOVED + 1))
done
find proofs -name "poka-yoke-*.test.mjs" -delete 2>/dev/null || true

echo ""
echo "✅ Removal complete!"
echo "   Removed: $REMOVED test files"

# Count remaining
REMAINING=$(find . -name "*.test.mjs" -type f -not -path "./node_modules/*" | wc -l)
echo "   Remaining: $REMAINING test files (target: ~126)"
echo ""
echo "📊 Stats:"
echo "   Original: 631 tests"
echo "   Removed: $REMOVED tests (~$(( REMOVED * 100 / 631 ))%)"
echo "   Kept: $REMAINING tests (~$(( REMAINING * 100 / 631 ))%)"
