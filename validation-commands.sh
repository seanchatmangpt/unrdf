#!/bin/bash
# Adversarial Validation Commands - Reproducible Test Suite
# Run this script to reproduce all validation results

set -e

echo "=== FILE INVENTORY ==="
find /home/user/unrdf -maxdepth 1 -name "*.mjs" | grep -E "(max|combo|microfw)" | sort
wc -l /home/user/unrdf/{max-combo-10-mega-framework.mjs,max-combo-10-mega-framework-standalone.mjs,microfw-9-graph-routing.mjs}

echo -e "\n=== EXECUTION TEST 1: mega-framework ==="
timeout 5s node /home/user/unrdf/max-combo-10-mega-framework.mjs --help 2>&1 || echo "FAILED: Missing dependencies"

echo -e "\n=== EXECUTION TEST 2: standalone ==="
timeout 10s node /home/user/unrdf/max-combo-10-mega-framework-standalone.mjs

echo -e "\n=== EXECUTION TEST 3: microfw-9 ==="
timeout 5s node /home/user/unrdf/microfw-9-graph-routing.mjs

echo -e "\n=== PACKAGE COUNT ==="
grep -E "@unrdf/|from 'vue'" /home/user/unrdf/max-combo-10-mega-framework.mjs | grep -oE "@unrdf/[a-z0-9-]+|'vue'" | sort -u | tee /tmp/packages.txt
echo "Total packages: $(wc -l < /tmp/packages.txt)"

echo -e "\n=== EXTERNAL DEPENDENCIES ==="
grep -E "^import.*from ['\"]" /home/user/unrdf/max-combo-10-mega-framework.mjs | grep -v "@unrdf/" || echo "Only @unrdf packages"

echo -e "\n=== JSDOC COVERAGE ==="
echo "JSDoc blocks: $(grep -c '^\s*/\*\*' /home/user/unrdf/max-combo-10-mega-framework-standalone.mjs)"
echo "Classes/Functions: $(grep -cE 'class |function ' /home/user/unrdf/max-combo-10-mega-framework-standalone.mjs)"

echo -e "\n=== TEST FILES ==="
find /home/user/unrdf -name "*.test.mjs" -o -name "*.spec.mjs" | grep -i "max\|combo\|microfw" || echo "No test files found"

echo -e "\n=== VALIDATION COMPLETE ==="
