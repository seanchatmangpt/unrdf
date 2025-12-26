#!/bin/bash
# Interactive Test Debugger
# Debug failing tests with Chrome DevTools integration

set -e

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

TEST_FILE=$1

if [ -z "$TEST_FILE" ]; then
  echo -e "${RED}❌ Usage: ./scripts/debug-test.sh <test-file>${NC}"
  echo -e "\nExamples:"
  echo -e "  ${YELLOW}./scripts/debug-test.sh packages/core/tests/core.test.mjs${NC}"
  echo -e "  ${YELLOW}./scripts/debug-test.sh packages/hooks/tests/hooks.test.mjs${NC}"
  echo ""
  echo -e "${BLUE}Available test files:${NC}"
  find packages/*/tests -name "*.test.mjs" -type f | head -10
  exit 1
fi

if [ ! -f "$TEST_FILE" ]; then
  echo -e "${RED}❌ Test file not found: $TEST_FILE${NC}"
  exit 1
fi

echo -e "${BLUE}🐛 Debugging Test: $TEST_FILE${NC}\n"

# Step 1: Run test with verbose output
echo -e "${YELLOW}Step 1: Running test with verbose output...${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

DEBUG=* timeout 30s pnpm vitest run "$TEST_FILE" --reporter=verbose 2>&1 || {
  echo -e "\n${YELLOW}⚠️  Test failed. Starting debugger...${NC}"
}

# Step 2: Offer debugging options
echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Debugging Options:${NC}\n"
echo -e "  ${GREEN}1)${NC} Run with Chrome DevTools (node --inspect-brk)"
echo -e "  ${GREEN}2)${NC} Run with coverage to see uncovered lines"
echo -e "  ${GREEN}3)${NC} Run in watch mode for iterative debugging"
echo -e "  ${GREEN}4)${NC} Show test file content"
echo -e "  ${GREEN}5)${NC} Exit"
echo ""
read -p "Select option (1-5): " OPTION

case $OPTION in
  1)
    echo -e "\n${YELLOW}Starting Chrome DevTools debugger...${NC}"
    echo -e "${BLUE}Open Chrome and navigate to: ${YELLOW}chrome://inspect${NC}\n"
    node --inspect-brk ./node_modules/.bin/vitest run "$TEST_FILE"
    ;;
  2)
    echo -e "\n${YELLOW}Running with coverage...${NC}"
    pnpm vitest run "$TEST_FILE" --coverage
    ;;
  3)
    echo -e "\n${YELLOW}Starting watch mode...${NC}"
    pnpm vitest watch "$TEST_FILE"
    ;;
  4)
    echo -e "\n${YELLOW}Test file content:${NC}"
    cat -n "$TEST_FILE" | head -50
    ;;
  5)
    echo -e "\n${GREEN}Exiting...${NC}"
    exit 0
    ;;
  *)
    echo -e "${RED}Invalid option${NC}"
    exit 1
    ;;
esac
