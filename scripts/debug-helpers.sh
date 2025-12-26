#!/usr/bin/env bash
# Debug Helpers - Tools for debugging and profiling
# Specialized tools for troubleshooting issues

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

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

# Parse command
COMMAND="${1:-help}"

case "$COMMAND" in
    debug:test)
        PACKAGE_NAME="${2:-}"

        if [ -z "$PACKAGE_NAME" ]; then
            print_error "Package name required"
            echo "Usage: $0 debug:test <package-name> [test-file]"
            exit 1
        fi

        TEST_FILE="${3:-}"

        print_info "Starting debugger for package: $PACKAGE_NAME"
        print_warning "Set breakpoints using 'debugger;' statement in your code"
        echo ""

        cd "packages/$PACKAGE_NAME"

        if [ -n "$TEST_FILE" ]; then
            print_info "Running test file: $TEST_FILE"
            node --inspect-brk ./node_modules/.bin/vitest run "$TEST_FILE"
        else
            print_info "Running all tests with debugger"
            node --inspect-brk ./node_modules/.bin/vitest run
        fi
        ;;

    trace:otel)
        print_info "Viewing OpenTelemetry traces..."
        echo ""

        # Check if validation output exists
        if [ -f "validation-output.log" ]; then
            print_info "Recent OTEL validation results:"
            echo ""

            # Show scores
            grep "Score:" validation-output.log | tail -5
            echo ""

            # Show any failures
            FAILURES=$(grep -c "FAILED\|Error" validation-output.log || echo "0")
            if [ "$FAILURES" -gt 0 ]; then
                print_warning "Found $FAILURES failures/errors"
                grep "FAILED\|Error" validation-output.log | tail -10
            else
                print_success "No failures detected"
            fi
        else
            print_warning "No validation output found"
            print_info "Run OTEL validation with: node validation/run-all.mjs comprehensive"
        fi

        # Check for OTEL spans in recent logs
        if [ -d "logs" ]; then
            print_info "Searching for recent OTEL spans..."
            find logs -name "*.log" -mtime -1 -exec grep -l "span" {} \; | head -5
        fi
        ;;

    profile:perf)
        PACKAGE_NAME="${2:-}"

        if [ -z "$PACKAGE_NAME" ]; then
            print_error "Package name required"
            echo "Usage: $0 profile:perf <package-name> [script]"
            exit 1
        fi

        SCRIPT="${3:-test}"

        print_info "Profiling package: $PACKAGE_NAME"
        print_info "Script: $SCRIPT"
        echo ""

        cd "packages/$PACKAGE_NAME"

        # Create profile output directory
        mkdir -p ../../profiles
        PROFILE_FILE="../../profiles/${PACKAGE_NAME}-$(date +%s).cpuprofile"

        print_info "Generating CPU profile..."
        node --cpu-prof --cpu-prof-dir=../../profiles npm run "$SCRIPT"

        print_success "Profile saved to: $PROFILE_FILE"
        print_info "Open in Chrome DevTools: chrome://inspect -> Load profile"
        ;;

    trace:deps)
        PACKAGE_NAME="${2:-}"

        if [ -z "$PACKAGE_NAME" ]; then
            print_error "Package name required"
            echo "Usage: $0 trace:deps <package-name>"
            exit 1
        fi

        print_info "Tracing dependencies for: $PACKAGE_NAME"
        echo ""

        if [ ! -d "packages/$PACKAGE_NAME" ]; then
            print_error "Package not found: $PACKAGE_NAME"
            exit 1
        fi

        # Direct dependencies
        print_info "Direct dependencies:"
        node -p "JSON.stringify(require('./packages/$PACKAGE_NAME/package.json').dependencies || {}, null, 2)"
        echo ""

        # Dev dependencies
        print_info "Dev dependencies:"
        node -p "JSON.stringify(require('./packages/$PACKAGE_NAME/package.json').devDependencies || {}, null, 2)"
        echo ""

        # Workspace dependencies
        print_info "Workspace dependencies:"
        grep -r "workspace:\*" "packages/$PACKAGE_NAME/package.json" || print_warning "No workspace dependencies"
        ;;

    analyze:bundle)
        PACKAGE_NAME="${2:-}"

        if [ -z "$PACKAGE_NAME" ]; then
            print_error "Package name required"
            echo "Usage: $0 analyze:bundle <package-name>"
            exit 1
        fi

        print_info "Analyzing bundle for: $PACKAGE_NAME"

        DIST_DIR="packages/$PACKAGE_NAME/dist"

        if [ ! -d "$DIST_DIR" ]; then
            print_error "No build artifacts found. Run 'pnpm build' first"
            exit 1
        fi

        echo ""
        print_info "Bundle files:"
        ls -lh "$DIST_DIR"

        echo ""
        print_info "Total bundle size:"
        du -sh "$DIST_DIR"

        echo ""
        print_info "File breakdown:"
        find "$DIST_DIR" -type f -exec ls -lh {} \; | awk '{print $5, $9}' | sort -hr
        ;;

    check:memory)
        print_info "Checking memory usage..."
        echo ""

        # Run tests with memory profiling
        print_info "Running tests with memory tracking..."
        NODE_OPTIONS="--expose-gc --max-old-space-size=512" timeout 120s pnpm test:fast 2>&1 | tee /tmp/memory-test.log

        # Check for memory warnings
        if grep -q "out of memory\|heap" /tmp/memory-test.log; then
            print_warning "Memory warnings detected"
            grep -i "memory\|heap" /tmp/memory-test.log
        else
            print_success "No memory warnings"
        fi
        ;;

    trace:imports)
        PACKAGE_NAME="${2:-}"

        if [ -z "$PACKAGE_NAME" ]; then
            print_error "Package name required"
            echo "Usage: $0 trace:imports <package-name>"
            exit 1
        fi

        print_info "Tracing imports for: $PACKAGE_NAME"
        echo ""

        PACKAGE_DIR="packages/$PACKAGE_NAME"

        if [ ! -d "$PACKAGE_DIR" ]; then
            print_error "Package not found: $PACKAGE_NAME"
            exit 1
        fi

        # Find all imports
        print_info "ES6 imports:"
        grep -r "^import" "$PACKAGE_DIR/src" --include="*.mjs" --include="*.js" | cut -d: -f2 | sort | uniq

        echo ""
        print_info "Require statements:"
        grep -r "require(" "$PACKAGE_DIR/src" --include="*.mjs" --include="*.js" | cut -d: -f2 | sort | uniq || print_info "No require() statements found"

        echo ""
        print_info "External dependencies used:"
        grep -r "from ['\"]" "$PACKAGE_DIR/src" --include="*.mjs" --include="*.js" | grep -v "^['\"./@]" | sed "s/.*from ['\"]//;s/['\"].*//" | sort | uniq
        ;;

    watch:logs)
        LOG_FILE="${2:-validation-output.log}"

        print_info "Watching log file: $LOG_FILE"
        print_info "Press Ctrl+C to stop"
        echo ""

        if [ ! -f "$LOG_FILE" ]; then
            print_warning "Log file not found: $LOG_FILE"
            print_info "Waiting for file to be created..."
            touch "$LOG_FILE"
        fi

        tail -f "$LOG_FILE"
        ;;

    help|*)
        echo "UNRDF Debug Helpers"
        echo ""
        echo "Usage: $0 <command> [options]"
        echo ""
        echo "Commands:"
        echo "  debug:test <pkg> [file]    Run tests with Node.js debugger"
        echo "  trace:otel                 View OpenTelemetry trace results"
        echo "  profile:perf <pkg>         Generate CPU profile for package"
        echo "  trace:deps <pkg>           Show dependency tree for package"
        echo "  analyze:bundle <pkg>       Analyze bundle size and composition"
        echo "  check:memory               Check memory usage during tests"
        echo "  trace:imports <pkg>        Show all imports in package"
        echo "  watch:logs [file]          Watch log file in real-time"
        echo "  help                       Show this help message"
        echo ""
        echo "Examples:"
        echo "  $0 debug:test core                    # Debug @unrdf/core tests"
        echo "  $0 trace:otel                         # View OTEL results"
        echo "  $0 profile:perf streaming             # Profile streaming package"
        echo "  $0 analyze:bundle hooks               # Analyze hooks bundle"
        echo "  $0 trace:deps federation              # Show federation deps"
        echo ""
        ;;
esac
