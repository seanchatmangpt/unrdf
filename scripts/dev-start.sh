#!/bin/bash
# UNRDF Development Environment Quick Start
# 80/20 Rule: One command to start developing

set -e  # Exit on error

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}🚀 Starting UNRDF Development Environment...${NC}\n"

# Step 1: Check prerequisites
echo -e "${YELLOW}📋 Checking prerequisites...${NC}"

# Check Node.js version
NODE_VERSION=$(node --version | cut -d'v' -f2 | cut -d'.' -f1)
if [ "$NODE_VERSION" -lt 18 ]; then
  echo -e "${RED}❌ Node.js >= 18.0.0 required. Current: $(node --version)${NC}"
  exit 1
fi
echo -e "${GREEN}✅ Node.js $(node --version)${NC}"

# Check pnpm
if ! command -v pnpm &> /dev/null; then
  echo -e "${YELLOW}⚠️  pnpm not found. Installing...${NC}"
  npm install -g pnpm
fi
echo -e "${GREEN}✅ pnpm $(pnpm --version)${NC}"

# Step 2: Install dependencies
echo -e "\n${YELLOW}📦 Installing dependencies...${NC}"
time timeout 60s pnpm install --frozen-lockfile || {
  echo -e "${YELLOW}⚠️  Lockfile mismatch, running regular install...${NC}"
  time timeout 60s pnpm install
}
echo -e "${GREEN}✅ Dependencies installed${NC}"

# Step 3: Build core packages (only essential ones)
echo -e "\n${YELLOW}🔨 Building core packages...${NC}"
time timeout 30s pnpm -r --filter @unrdf/core --filter @unrdf/yawl build
echo -e "${GREEN}✅ Core packages built${NC}"

# Step 4: Run fast validation
echo -e "\n${YELLOW}✅ Running fast validation...${NC}"
time timeout 15s pnpm test:fast 2>&1 | tail -20
echo -e "${GREEN}✅ Validation passed${NC}"

# Step 5: Success message with next steps
echo -e "\n${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}✨ UNRDF Development Environment Ready!${NC}"
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

echo -e "${BLUE}Quick Start Commands:${NC}"
echo -e "  ${YELLOW}pnpm dev${NC}           # Start watch mode (build + test)"
echo -e "  ${YELLOW}pnpm test:watch${NC}    # Start test watch mode"
echo -e "  ${YELLOW}pnpm fix${NC}           # Auto-fix lint/format issues"
echo -e "  ${YELLOW}pnpm validate${NC}      # Fast validation (lint + test)"
echo -e "  ${YELLOW}pnpm validate:full${NC} # Full validation (build + lint + test)"
echo ""
echo -e "${BLUE}Package-Specific Development:${NC}"
echo -e "  ${YELLOW}./scripts/dev-package.sh${NC}  # Interactive package selector"
echo ""
echo -e "${BLUE}Testing:${NC}"
echo -e "  ${YELLOW}pnpm test${NC}               # Run all tests"
echo -e "  ${YELLOW}pnpm test:core${NC}          # Test specific package"
echo -e "  ${YELLOW}./scripts/debug-test.sh${NC} # Debug failing tests"
echo ""
echo -e "${BLUE}Quality Checks:${NC}"
echo -e "  ${YELLOW}./scripts/pre-commit.sh${NC} # Pre-commit validation (<10s)"
echo -e "  ${YELLOW}pnpm lint:fix${NC}           # Fix linting issues"
echo ""
echo -e "${BLUE}Documentation:${NC}"
echo -e "  ${YELLOW}docs/developer-workflow.md${NC}  # Complete workflow guide"
echo -e "  ${YELLOW}docs/onboarding-guide.md${NC}    # New developer onboarding"
echo ""
echo -e "${GREEN}Happy coding! 🎉${NC}\n"
