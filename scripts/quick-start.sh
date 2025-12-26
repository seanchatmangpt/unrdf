#!/usr/bin/env bash
# Quick Start Script - One-command developer setup
# Saves 10-15 minutes per developer by automating environment validation and setup

set -euo pipefail

# Color output for better readability
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Project root detection
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}UNRDF Quick Start - Developer Setup${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Function to print status messages
print_step() {
    echo -e "${BLUE}→${NC} $1"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

# Track failures
FAILURES=()

# 1. Environment Detection
print_step "Step 1/5: Detecting environment..."

# Check OS
OS="$(uname -s)"
case "$OS" in
    Linux*)     OS_TYPE="Linux";;
    Darwin*)    OS_TYPE="macOS";;
    CYGWIN*)    OS_TYPE="Windows";;
    MINGW*)     OS_TYPE="Windows";;
    *)          OS_TYPE="Unknown";;
esac
print_success "Operating System: $OS_TYPE"

# Check Node.js
if ! command -v node &> /dev/null; then
    print_error "Node.js not found. Please install Node.js >= 18.0.0"
    FAILURES+=("Node.js not installed")
else
    NODE_VERSION=$(node --version)
    NODE_MAJOR=$(echo "$NODE_VERSION" | cut -d'.' -f1 | sed 's/v//')
    if [ "$NODE_MAJOR" -lt 18 ]; then
        print_error "Node.js version $NODE_VERSION is too old. Required: >= 18.0.0"
        FAILURES+=("Node.js version too old")
    else
        print_success "Node.js: $NODE_VERSION"
    fi
fi

# Check pnpm
if ! command -v pnpm &> /dev/null; then
    print_error "pnpm not found. Installing pnpm..."
    npm install -g pnpm
    if [ $? -eq 0 ]; then
        print_success "pnpm installed successfully"
    else
        print_error "Failed to install pnpm"
        FAILURES+=("pnpm installation failed")
    fi
else
    PNPM_VERSION=$(pnpm --version)
    print_success "pnpm: $PNPM_VERSION"
fi

echo ""

# 2. Install Dependencies
print_step "Step 2/5: Installing dependencies..."
START_TIME=$(date +%s)

if timeout 60s pnpm install --frozen-lockfile 2>&1 | tee /tmp/pnpm-install.log; then
    END_TIME=$(date +%s)
    DURATION=$((END_TIME - START_TIME))
    print_success "Dependencies installed in ${DURATION}s"
else
    print_error "Dependency installation failed"
    FAILURES+=("pnpm install failed")
fi

echo ""

# 3. Build All Packages
print_step "Step 3/5: Building all packages..."
START_TIME=$(date +%s)

if timeout 120s pnpm build 2>&1 | tee /tmp/pnpm-build.log; then
    END_TIME=$(date +%s)
    DURATION=$((END_TIME - START_TIME))
    print_success "Build completed in ${DURATION}s"
else
    print_error "Build failed - check /tmp/pnpm-build.log"
    FAILURES+=("Build failed")
fi

echo ""

# 4. Run Quick Validation
print_step "Step 4/5: Running quick validation..."
START_TIME=$(date +%s)

# Lint check
if timeout 30s pnpm lint 2>&1 | grep -q "error" && [ ${PIPESTATUS[0]} -ne 0 ]; then
    print_warning "Linting found issues (non-blocking)"
else
    print_success "Linting passed"
fi

# Quick tests
if timeout 120s pnpm test:fast 2>&1 | tee /tmp/test-fast.log; then
    END_TIME=$(date +%s)
    DURATION=$((END_TIME - START_TIME))
    print_success "Quick tests passed in ${DURATION}s"
else
    print_error "Quick tests failed - check /tmp/test-fast.log"
    FAILURES+=("Quick tests failed")
fi

echo ""

# 5. Health Checks
print_step "Step 5/5: Running health checks..."

# Check package count
PACKAGE_COUNT=$(ls -1 packages | wc -l | tr -d ' ')
print_success "Found $PACKAGE_COUNT packages"

# Check if critical packages exist
CRITICAL_PACKAGES=("core" "hooks" "streaming" "cli")
for pkg in "${CRITICAL_PACKAGES[@]}"; do
    if [ -d "packages/$pkg" ]; then
        print_success "Critical package '$pkg' found"
    else
        print_warning "Critical package '$pkg' not found"
    fi
done

# Check for node_modules in packages
NODE_MODULES_COUNT=$(find packages -name "node_modules" -type d | wc -l | tr -d ' ')
if [ "$NODE_MODULES_COUNT" -gt 0 ]; then
    print_success "Dependencies installed in $NODE_MODULES_COUNT packages"
fi

echo ""
echo -e "${BLUE}========================================${NC}"

# Final Status
if [ ${#FAILURES[@]} -eq 0 ]; then
    echo -e "${GREEN}✓ Quick Start SUCCESSFUL${NC}"
    echo ""
    echo "Next steps:"
    echo "  1. Run 'npm run dev' to start development servers"
    echo "  2. Run 'npm run test:watch' for test-driven development"
    echo "  3. See DEVELOPER-GUIDE.md for full workflow documentation"
    echo ""
    exit 0
else
    echo -e "${RED}✗ Quick Start FAILED${NC}"
    echo ""
    echo "Failures encountered:"
    for failure in "${FAILURES[@]}"; do
        echo -e "  ${RED}✗${NC} $failure"
    done
    echo ""
    echo "Check log files in /tmp/ for details"
    exit 1
fi
