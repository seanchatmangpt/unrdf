#!/bin/bash
# Install Git hooks for UNRDF
# DfLSS: Prevent defects by ensuring hooks are always installed
#
# This script installs pre-commit and pre-push hooks:
# - Pre-commit: Fast validation (<5s) - format and lint checks
# - Pre-push: Full validation (<60s) - format, lint, build, and tests
# Run this once after cloning the repository: ./scripts/install-hooks.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
HOOKS_DIR="$REPO_ROOT/.git/hooks"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

print_header() {
    echo ""
    echo -e "${BOLD}========================================${NC}"
    echo -e "${BOLD}   UNRDF Git Hooks Installation${NC}"
    echo -e "${BOLD}   DfLSS: Defect Prevention System${NC}"
    echo -e "${BOLD}========================================${NC}"
    echo ""
}

print_success() {
    echo -e "${GREEN}[OK]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

verify_prerequisites() {
    print_info "Verifying prerequisites..."

    # Check git directory
    if [ ! -d "$REPO_ROOT/.git" ]; then
        print_error "Not in a git repository (.git directory not found)"
        echo "Run this script from the repository root: ./scripts/install-hooks.sh"
        exit 1
    fi
    print_success "Git repository found"

    # Check timeout command
    if ! command -v timeout &> /dev/null; then
        print_warning "'timeout' command not found"
        echo "Install coreutils:"
        echo "  macOS: brew install coreutils"
        echo "  Linux: apt-get install coreutils"
        echo ""
        echo "Note: Hooks will still work but without timeout protection"
    else
        print_success "timeout command available"
    fi

    # Check hook scripts exist
    if [ ! -f "$SCRIPT_DIR/hooks/pre-commit" ]; then
        print_error "pre-commit hook not found at $SCRIPT_DIR/hooks/"
        exit 1
    fi
    if [ ! -f "$SCRIPT_DIR/hooks/pre-push" ]; then
        print_error "pre-push hook not found at $SCRIPT_DIR/hooks/"
        exit 1
    fi
    print_success "Hook scripts found"

    # Check pnpm
    if ! command -v pnpm &> /dev/null; then
        print_warning "pnpm not found - hooks may fail"
        echo "Install pnpm: npm install -g pnpm"
    else
        print_success "pnpm available"
    fi
}

backup_existing_hooks() {
    print_info "Backing up existing hooks..."

    for hook in pre-commit pre-push; do
        if [ -f "$HOOKS_DIR/$hook" ] && [ ! -L "$HOOKS_DIR/$hook" ]; then
            BACKUP_NAME="$hook.bak.$(date +%Y%m%d%H%M%S)"
            mv "$HOOKS_DIR/$hook" "$HOOKS_DIR/$BACKUP_NAME"
            print_warning "Backed up existing $hook to $BACKUP_NAME"
        elif [ -L "$HOOKS_DIR/$hook" ]; then
            rm "$HOOKS_DIR/$hook"
            print_info "Removed old symlink for $hook"
        fi
    done
}

install_hooks() {
    print_info "Installing hooks..."

    # Create hooks directory if needed
    mkdir -p "$HOOKS_DIR"

    # Install pre-commit hook
    cp "$SCRIPT_DIR/hooks/pre-commit" "$HOOKS_DIR/pre-commit"
    chmod +x "$HOOKS_DIR/pre-commit"
    print_success "Installed pre-commit hook (fast tier: <5s)"

    # Install pre-push hook
    cp "$SCRIPT_DIR/hooks/pre-push" "$HOOKS_DIR/pre-push"
    chmod +x "$HOOKS_DIR/pre-push"
    print_success "Installed pre-push hook (full tier: <60s)"
}

run_self_test() {
    print_info "Running self-test..."

    # Test hook syntax
    if sh -n "$SCRIPT_DIR/hooks/pre-commit" 2>/dev/null; then
        print_success "pre-commit syntax OK"
    else
        print_error "pre-commit has syntax errors"
        return 1
    fi

    if sh -n "$SCRIPT_DIR/hooks/pre-push" 2>/dev/null; then
        print_success "pre-push syntax OK"
    else
        print_error "pre-push has syntax errors"
        return 1
    fi

    print_success "Self-test passed"
}

print_summary() {
    echo ""
    echo -e "${BOLD}========================================${NC}"
    echo -e "${GREEN}${BOLD}   Installation Complete!${NC}"
    echo -e "${BOLD}========================================${NC}"
    echo ""
    echo "Hooks installed:"
    echo -e "  ${GREEN}pre-commit${NC}: Fast validation (<5s)"
    echo -e "    - Format check (2s timeout, auto-fix)"
    echo -e "    - Lint check (3s timeout)"
    echo ""
    echo -e "  ${GREEN}pre-push${NC}: Full validation (<60s)"
    echo -e "    - Format check (5s timeout)"
    echo -e "    - Lint check (10s timeout)"
    echo -e "    - Build check (15s timeout)"
    echo -e "    - Unit tests (30s timeout)"
    echo ""
    echo -e "${YELLOW}Important:${NC}"
    echo "  - Never use --no-verify to bypass hooks"
    echo "  - Fix all issues before committing/pushing"
    echo "  - Run 'pnpm format' and 'pnpm lint:fix' to auto-fix issues"
    echo ""
    echo "To bypass (DISCOURAGED):"
    echo "  - Pre-commit: SKIP_PRE_COMMIT=1 git commit"
    echo "  - Pre-push: SKIP_PRE_PUSH=1 git push"
    echo ""
    echo "To uninstall: rm .git/hooks/pre-commit .git/hooks/pre-push"
    echo ""
}

uninstall_hooks() {
    print_header
    print_info "Uninstalling hooks..."

    for hook in pre-commit pre-push; do
        if [ -f "$HOOKS_DIR/$hook" ]; then
            rm "$HOOKS_DIR/$hook"
            print_success "Removed $hook hook"
        fi
    done

    echo ""
    print_success "Hooks uninstalled successfully"
    echo ""
}

# ============================================================
# MAIN
# ============================================================

main() {
    # Handle uninstall flag
    if [ "${1:-}" == "--uninstall" ]; then
        uninstall_hooks
        exit 0
    fi

    print_header
    verify_prerequisites
    backup_existing_hooks
    install_hooks
    run_self_test
    print_summary
}

main "$@"

