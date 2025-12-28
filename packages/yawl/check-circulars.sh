#!/bin/bash
# Simple circular dependency checker for YAWL package

echo "🔍 Checking for circular dependencies in YAWL..."
echo ""

cd /home/user/unrdf/packages/yawl/src

# Function to check if A imports B and B imports A
check_bidirectional() {
  local fileA="$1"
  local fileB="$2"
  local nameA=$(basename "$fileA" .mjs)
  local nameB=$(basename "$fileB" .mjs)

  # Check if A imports B
  local a_imports_b=$(grep -c "from.*['\"]\..*${nameB}\.mjs['\"]" "$fileA" 2>/dev/null || echo "0")

  # Check if B imports A
  local b_imports_a=$(grep -c "from.*['\"]\..*${nameA}\.mjs['\"]" "$fileB" 2>/dev/null || echo "0")

  if [ "$a_imports_b" -gt 0 ] && [ "$b_imports_a" -gt 0 ]; then
    echo "🔴 CIRCULAR: $fileA ↔ $fileB"
    return 0
  fi
  return 1
}

# Check specific suspected pairs from gap analysis
echo "Checking suspected pairs..."
echo ""

CIRCULAR_COUNT=0

# Check engine ↔ workflow
if check_bidirectional "engine.mjs" "workflow.mjs"; then
  ((CIRCULAR_COUNT++))
fi

if check_bidirectional "engine-core.mjs" "workflow.mjs"; then
  ((CIRCULAR_COUNT++))
fi

# Check case-lifecycle ↔ resources
if check_bidirectional "case-lifecycle.mjs" "resources/yawl-resources.mjs"; then
  ((CIRCULAR_COUNT++))
fi

# Check cancellation ↔ case-lifecycle
if check_bidirectional "cancellation/yawl-cancellation.mjs" "case-lifecycle.mjs"; then
  ((CIRCULAR_COUNT++))
fi

# Check workflow-class ↔ engine
if check_bidirectional "workflow/workflow-class.mjs" "engine.mjs"; then
  ((CIRCULAR_COUNT++))
fi

# Check api ↔ engine
if check_bidirectional "api/workflow-api-core.mjs" "engine.mjs"; then
  ((CIRCULAR_COUNT++))
fi

echo ""
echo "Checking all file pairs (this may take a moment)..."
echo ""

# Get all .mjs files
FILES=($(find . -name "*.mjs" -type f | sort))

# Check each pair (only upper triangle to avoid duplicates)
for ((i=0; i<${#FILES[@]}; i++)); do
  for ((j=i+1; j<${#FILES[@]}; j++)); do
    if check_bidirectional "${FILES[$i]}" "${FILES[$j]}"; then
      ((CIRCULAR_COUNT++))
    fi
  done
done

echo ""
if [ $CIRCULAR_COUNT -eq 0 ]; then
  echo "✅ No circular dependencies found!"
  exit 0
else
  echo "❌ Found $CIRCULAR_COUNT circular dependency pair(s)"
  exit 1
fi
