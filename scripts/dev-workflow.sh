#!/usr/bin/env bash
# Development Workflow Scripts
# Common development tasks with sensible defaults

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

print_info() {
    echo -e "${BLUE}→${NC} $1"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

# Parse command
COMMAND="${1:-help}"

case "$COMMAND" in
    dev)
        print_info "Starting all development servers with hot reload..."
        print_info "Press Ctrl+C to stop all servers"
        echo ""

        # Start parallel dev servers
        pnpm run dev
        ;;

    test:watch)
        print_info "Starting test watch mode..."
        echo ""

        # Allow package selection
        if [ -n "${2:-}" ]; then
            print_info "Running tests for package: $2"
            pnpm -C "packages/$2" test:watch
        else
            print_info "Running tests for all packages"
            pnpm test:watch
        fi
        ;;

    validate)
        print_info "Running quick validation (lint + type-check + test)..."
        START_TIME=$(date +%s)

        FAILED=0

        # Lint
        print_info "1/3: Linting..."
        if timeout 30s pnpm lint; then
            print_success "Linting passed"
        else
            print_error "Linting failed"
            FAILED=1
        fi

        # Type check (via build which includes type checking)
        print_info "2/3: Type checking..."
        if timeout 60s pnpm build; then
            print_success "Type checking passed"
        else
            print_error "Type checking failed"
            FAILED=1
        fi

        # Fast tests
        print_info "3/3: Running tests..."
        if timeout 120s pnpm test:fast; then
            print_success "Tests passed"
        else
            print_error "Tests failed"
            FAILED=1
        fi

        END_TIME=$(date +%s)
        DURATION=$((END_TIME - START_TIME))

        echo ""
        if [ $FAILED -eq 0 ]; then
            print_success "Validation completed in ${DURATION}s - ALL PASSED"
            exit 0
        else
            print_error "Validation completed in ${DURATION}s - FAILURES DETECTED"
            exit 1
        fi
        ;;

    fix)
        print_info "Auto-fixing linting and formatting issues..."

        # Fix linting
        print_info "1/2: Fixing lint issues..."
        if timeout 30s pnpm lint:fix; then
            print_success "Lint fixes applied"
        else
            print_error "Some lint issues could not be auto-fixed"
        fi

        # Fix formatting
        print_info "2/2: Fixing formatting..."
        if timeout 20s pnpm format; then
            print_success "Formatting applied"
        else
            print_error "Formatting failed"
        fi

        print_success "Auto-fix complete"
        ;;

    validate:commit)
        print_info "Running pre-commit validation (fast)..."
        START_TIME=$(date +%s)

        if timeout 150s pnpm precommit; then
            END_TIME=$(date +%s)
            DURATION=$((END_TIME - START_TIME))
            print_success "Pre-commit validation passed in ${DURATION}s"
            exit 0
        else
            END_TIME=$(date +%s)
            DURATION=$((END_TIME - START_TIME))
            print_error "Pre-commit validation failed in ${DURATION}s"
            exit 1
        fi
        ;;

    build:watch)
        print_info "Starting build in watch mode..."
        echo ""

        # Use nodemon or similar for watch mode
        if command -v nodemon &> /dev/null; then
            nodemon --watch packages --ext mjs,js,ts --exec "pnpm build"
        else
            print_error "nodemon not found. Install with: npm install -g nodemon"
            exit 1
        fi
        ;;

    help|*)
        echo "UNRDF Development Workflow Commands"
        echo ""
        echo "Usage: $0 <command> [options]"
        echo ""
        echo "Commands:"
        echo "  dev                Start all dev servers with hot reload"
        echo "  test:watch [pkg]   Start test watch mode (optionally for specific package)"
        echo "  validate           Quick validation: lint + type-check + test"
        echo "  fix                Auto-fix linting and formatting issues"
        echo "  validate:commit    Pre-commit validation (lint + fast tests)"
        echo "  build:watch        Build in watch mode (requires nodemon)"
        echo "  help               Show this help message"
        echo ""
        echo "Examples:"
        echo "  $0 dev                     # Start all dev servers"
        echo "  $0 test:watch core         # Watch tests for @unrdf/core"
        echo "  $0 validate                # Run full validation before commit"
        echo "  $0 fix                     # Auto-fix code style issues"
        echo ""
        ;;
esac
