#!/bin/bash
# Test validation script for sidecar graceful degradation

set -e

echo "🧪 Testing Sidecar Graceful Degradation"
echo "========================================"
echo ""

# Test 1: Sidecar status command
echo "Test 1: sidecar status (sidecar not running)"
output=$(node cli/unrdf.mjs sidecar status 2>&1)
exit_code=$?

if [ $exit_code -eq 0 ]; then
  echo "✅ Exit code is 0 (success)"
else
  echo "❌ Exit code is $exit_code (expected 0)"
  exit 1
fi

if echo "$output" | grep -q "Sidecar not available"; then
  echo "✅ Warning message displayed"
else
  echo "❌ Warning message not found"
  exit 1
fi

echo ""

# Test 2: Sidecar health command
echo "Test 2: sidecar health (sidecar not running)"
output=$(node cli/unrdf.mjs sidecar health 2>&1)
exit_code=$?

if [ $exit_code -eq 0 ]; then
  echo "✅ Exit code is 0 (success)"
else
  echo "❌ Exit code is $exit_code (expected 0)"
  exit 1
fi

if echo "$output" | grep -q "Sidecar not available"; then
  echo "✅ Warning message displayed"
else
  echo "❌ Warning message not found"
  exit 1
fi

echo ""
echo "========================================"
echo "✅ All tests passed!"
echo ""
echo "Summary:"
echo "- sidecar status: Exit 0, warning message shown"
echo "- sidecar health: Exit 0, warning message shown"
echo ""
echo "Tests can now run without sidecar process!"
