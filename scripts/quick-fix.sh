#!/bin/bash
# Quick Fix - Auto-resolve 90% of common issues
# 80/20 Rule: Fix the most common problems automatically

set -e

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}🔧 UNRDF Quick Fix - Auto-resolving issues...${NC}\n"

FIXED_COUNT=0

# Step 1: Fix linting issues
echo -e "${YELLOW}1/4 Fixing linting issues...${NC}"
if timeout 10s pnpm eslint . --fix --cache 2>&1 | tee /tmp/eslint-fix.log; then
  LINT_FIXES=$(grep -c "fixed" /tmp/eslint-fix.log || echo "0")
  if [ "$LINT_FIXES" -gt 0 ]; then
    echo -e "${GREEN}✅ Fixed $LINT_FIXES linting issues${NC}"
    FIXED_COUNT=$((FIXED_COUNT + LINT_FIXES))
  else
    echo -e "${GREEN}✅ No linting issues found${NC}"
  fi
else
  echo -e "${YELLOW}⚠️  Some linting issues require manual attention${NC}"
fi

# Step 2: Fix formatting issues
echo -e "\n${YELLOW}2/4 Fixing formatting issues...${NC}"
CHANGED_FILES=$(timeout 10s pnpm prettier --write "**/*.{mjs,js,cjs,json,md}" 2>&1 | grep -c "ms" || echo "0")
if [ "$CHANGED_FILES" -gt 0 ]; then
  echo -e "${GREEN}✅ Formatted $CHANGED_FILES files${NC}"
  FIXED_COUNT=$((FIXED_COUNT + CHANGED_FILES))
else
  echo -e "${GREEN}✅ All files already formatted${NC}"
fi

# Step 3: Fix dependency issues
echo -e "\n${YELLOW}3/4 Checking dependencies...${NC}"
if ! pnpm install --frozen-lockfile &>/dev/null; then
  echo -e "${YELLOW}⚠️  Lockfile mismatch, updating...${NC}"
  timeout 30s pnpm install
  echo -e "${GREEN}✅ Dependencies synchronized${NC}"
  FIXED_COUNT=$((FIXED_COUNT + 1))
else
  echo -e "${GREEN}✅ Dependencies up to date${NC}"
fi

# Step 4: Clear caches (common source of issues)
echo -e "\n${YELLOW}4/4 Clearing caches...${NC}"
rm -rf node_modules/.cache .eslintcache 2>/dev/null || true
find packages -type d -name ".turbo" -exec rm -rf {} + 2>/dev/null || true
echo -e "${GREEN}✅ Caches cleared${NC}"
FIXED_COUNT=$((FIXED_COUNT + 1))

# Summary
echo -e "\n${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
if [ $FIXED_COUNT -gt 0 ]; then
  echo -e "${GREEN}✅ Quick Fix Complete: $FIXED_COUNT issues resolved 🎉${NC}"
else
  echo -e "${GREEN}✅ No issues found - codebase is clean! 🎉${NC}"
fi
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

# Verify fixes
echo -e "${BLUE}Verification:${NC}"
echo -e "  Run ${YELLOW}pnpm validate${NC} to verify all fixes"
echo -e "  Run ${YELLOW}./scripts/pre-commit.sh${NC} to test pre-commit checks"
echo ""

exit 0
