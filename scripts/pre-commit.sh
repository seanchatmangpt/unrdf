#!/bin/bash
# Fast Pre-Commit Validation
# Target: <10s total execution time
# 80/20 Rule: Catch 80% of issues in 20% of the time

set -e

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}🔍 Pre-Commit Validation (Target: <10s)${NC}\n"

START_TIME=$(date +%s)

# Step 1: Lint changed files only (fastest check)
echo -e "${YELLOW}1/3 Linting changed files...${NC}"
CHANGED_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(mjs|js|cjs)$' || true)

if [ -n "$CHANGED_FILES" ]; then
  # Only lint changed files
  timeout 5s pnpm eslint $CHANGED_FILES --cache --max-warnings 0 || {
    echo -e "${RED}❌ Linting failed. Run: pnpm fix${NC}"
    exit 1
  }
  echo -e "${GREEN}✅ Linting passed${NC}"
else
  echo -e "${YELLOW}⚠️  No changed files to lint${NC}"
fi

# Step 2: Type check (fast with cache)
echo -e "\n${YELLOW}2/3 Type checking...${NC}"
timeout 3s pnpm tsc --noEmit --pretty || {
  echo -e "${RED}❌ Type check failed${NC}"
  exit 1
}
echo -e "${GREEN}✅ Type check passed${NC}"

# Step 3: Fast tests only (affected packages)
echo -e "\n${YELLOW}3/3 Running fast tests...${NC}"
timeout 5s pnpm test:fast 2>&1 | tail -10 || {
  echo -e "${RED}❌ Tests failed. Run: pnpm test${NC}"
  exit 1
}
echo -e "${GREEN}✅ Tests passed${NC}"

# Calculate duration
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

echo -e "\n${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
if [ $DURATION -lt 10 ]; then
  echo -e "${GREEN}✅ All checks passed in ${DURATION}s (Target: <10s) 🎉${NC}"
else
  echo -e "${YELLOW}⚠️  All checks passed in ${DURATION}s (Target: <10s)${NC}"
  echo -e "${YELLOW}   Consider optimizing test suite${NC}"
fi
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

exit 0
