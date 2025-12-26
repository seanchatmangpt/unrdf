#!/usr/bin/env bash
# Common Tasks Automation
# Frequently used operations automated for developer productivity

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
    new:package)
        PACKAGE_NAME="${2:-}"

        if [ -z "$PACKAGE_NAME" ]; then
            print_error "Package name required"
            echo "Usage: $0 new:package <package-name>"
            exit 1
        fi

        print_info "Scaffolding new package: $PACKAGE_NAME"

        PACKAGE_DIR="packages/$PACKAGE_NAME"

        if [ -d "$PACKAGE_DIR" ]; then
            print_error "Package '$PACKAGE_NAME' already exists"
            exit 1
        fi

        # Create package structure
        mkdir -p "$PACKAGE_DIR"/{src,test,examples,docs}

        # Create package.json
        cat > "$PACKAGE_DIR/package.json" << EOF
{
  "name": "@unrdf/$PACKAGE_NAME",
  "version": "5.0.0",
  "description": "UNRDF $PACKAGE_NAME package",
  "type": "module",
  "main": "./dist/index.mjs",
  "types": "./dist/index.d.ts",
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "import": "./dist/index.mjs"
    }
  },
  "files": [
    "dist",
    "README.md"
  ],
  "scripts": {
    "build": "unbuild",
    "test": "vitest run",
    "test:fast": "vitest run --reporter=dot",
    "test:watch": "vitest",
    "lint": "eslint src test",
    "lint:fix": "eslint src test --fix",
    "format": "prettier --write src test",
    "format:check": "prettier --check src test",
    "clean": "rm -rf dist .turbo node_modules"
  },
  "keywords": ["rdf", "unrdf", "knowledge-graph"],
  "author": "UNRDF Team",
  "license": "MIT",
  "dependencies": {
    "@unrdf/core": "workspace:*",
    "zod": "^4.1.13"
  },
  "devDependencies": {
    "vitest": "^4.0.15",
    "unbuild": "^3.6.1",
    "eslint": "^9.39.1",
    "prettier": "^3.7.4"
  }
}
EOF

        # Create build.config.ts
        cat > "$PACKAGE_DIR/build.config.ts" << 'EOF'
import { defineBuildConfig } from 'unbuild';

export default defineBuildConfig({
  entries: ['src/index'],
  declaration: true,
  clean: true,
  rollup: {
    emitCJS: false,
  },
});
EOF

        # Create index.mjs
        cat > "$PACKAGE_DIR/src/index.mjs" << 'EOF'
/**
 * @fileoverview UNRDF Package Entry Point
 */

/**
 * Example function
 * @param {string} input - Input value
 * @returns {string} Output value
 */
export function hello(input) {
  return `Hello, ${input}!`;
}
EOF

        # Create basic test
        cat > "$PACKAGE_DIR/test/index.test.mjs" << 'EOF'
import { describe, it, expect } from 'vitest';
import { hello } from '../src/index.mjs';

describe('Package Tests', () => {
  it('should export hello function', () => {
    expect(hello('world')).toBe('Hello, world!');
  });
});
EOF

        # Create README
        cat > "$PACKAGE_DIR/README.md" << EOF
# @unrdf/$PACKAGE_NAME

UNRDF $PACKAGE_NAME package.

## Installation

\`\`\`bash
pnpm add @unrdf/$PACKAGE_NAME
\`\`\`

## Usage

\`\`\`javascript
import { hello } from '@unrdf/$PACKAGE_NAME';

console.log(hello('world'));
\`\`\`

## Documentation

See [docs](./docs) for detailed documentation.
EOF

        print_success "Package scaffolded: $PACKAGE_DIR"
        print_info "Next steps:"
        echo "  1. cd $PACKAGE_DIR"
        echo "  2. Implement your package in src/"
        echo "  3. Add tests in test/"
        echo "  4. Run 'pnpm install' from project root"
        ;;

    clean:all)
        print_info "Cleaning all build artifacts and dependencies..."

        # Clean dist folders
        print_info "Removing dist folders..."
        find packages -type d -name "dist" -exec rm -rf {} + 2>/dev/null || true
        print_success "Dist folders removed"

        # Clean node_modules
        print_info "Removing node_modules..."
        find packages -type d -name "node_modules" -exec rm -rf {} + 2>/dev/null || true
        rm -rf node_modules
        print_success "Node modules removed"

        # Clean turbo cache
        print_info "Removing .turbo cache..."
        find packages -type d -name ".turbo" -exec rm -rf {} + 2>/dev/null || true
        print_success "Turbo cache removed"

        # Clean test coverage
        print_info "Removing coverage reports..."
        find packages -type d -name "coverage" -exec rm -rf {} + 2>/dev/null || true
        print_success "Coverage reports removed"

        print_success "Clean complete - run 'pnpm install' to reinstall"
        ;;

    update:deps)
        print_info "Updating all dependencies safely..."

        # Update pnpm itself
        print_info "Updating pnpm..."
        pnpm self-update || print_warning "Could not update pnpm (may need sudo)"

        # Update dependencies
        print_info "Updating workspace dependencies..."
        if timeout 60s pnpm update; then
            print_success "Dependencies updated"
        else
            print_error "Dependency update failed"
            exit 1
        fi

        # Rebuild
        print_info "Rebuilding after update..."
        if timeout 120s pnpm build; then
            print_success "Build successful after update"
        else
            print_error "Build failed after update"
            exit 1
        fi

        # Run tests
        print_info "Running tests after update..."
        if timeout 120s pnpm test:fast; then
            print_success "Tests passed after update"
        else
            print_error "Tests failed after update - consider reverting"
            exit 1
        fi

        print_success "Dependencies updated and validated"
        ;;

    check:health)
        print_info "Running system health check..."
        echo ""

        WARNINGS=0
        ERRORS=0

        # Check Node version
        print_info "Checking Node.js version..."
        NODE_VERSION=$(node --version | sed 's/v//')
        NODE_MAJOR=$(echo "$NODE_VERSION" | cut -d'.' -f1)
        if [ "$NODE_MAJOR" -ge 18 ]; then
            print_success "Node.js $NODE_VERSION (OK)"
        else
            print_error "Node.js $NODE_VERSION (Required: >= 18.0.0)"
            ERRORS=$((ERRORS + 1))
        fi

        # Check pnpm
        print_info "Checking pnpm..."
        if command -v pnpm &> /dev/null; then
            PNPM_VERSION=$(pnpm --version)
            print_success "pnpm $PNPM_VERSION (OK)"
        else
            print_error "pnpm not found"
            ERRORS=$((ERRORS + 1))
        fi

        # Check package count
        print_info "Checking packages..."
        PACKAGE_COUNT=$(ls -1 packages | wc -l | tr -d ' ')
        print_success "$PACKAGE_COUNT packages found"

        # Check for broken symlinks in node_modules
        print_info "Checking for broken symlinks..."
        BROKEN_SYMLINKS=$(find packages -type l ! -exec test -e {} \; -print 2>/dev/null | wc -l | tr -d ' ')
        if [ "$BROKEN_SYMLINKS" -eq 0 ]; then
            print_success "No broken symlinks"
        else
            print_warning "$BROKEN_SYMLINKS broken symlinks found"
            WARNINGS=$((WARNINGS + 1))
        fi

        # Check disk space
        print_info "Checking disk space..."
        DISK_USAGE=$(df -h . | awk 'NR==2 {print $5}' | sed 's/%//')
        if [ "$DISK_USAGE" -lt 90 ]; then
            print_success "Disk usage: ${DISK_USAGE}% (OK)"
        else
            print_warning "Disk usage: ${DISK_USAGE}% (Consider cleaning)"
            WARNINGS=$((WARNINGS + 1))
        fi

        # Check if build artifacts exist
        print_info "Checking build artifacts..."
        DIST_COUNT=$(find packages -type d -name "dist" | wc -l | tr -d ' ')
        if [ "$DIST_COUNT" -gt 0 ]; then
            print_success "$DIST_COUNT packages built"
        else
            print_warning "No build artifacts found - run 'pnpm build'"
            WARNINGS=$((WARNINGS + 1))
        fi

        # Summary
        echo ""
        echo -e "${BLUE}========================================${NC}"
        if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
            print_success "System health: EXCELLENT"
        elif [ $ERRORS -eq 0 ]; then
            print_warning "System health: GOOD ($WARNINGS warnings)"
        else
            print_error "System health: POOR ($ERRORS errors, $WARNINGS warnings)"
        fi
        echo -e "${BLUE}========================================${NC}"

        [ $ERRORS -eq 0 ]
        ;;

    list:packages)
        print_info "Listing all packages..."
        echo ""

        for pkg_dir in packages/*; do
            if [ -f "$pkg_dir/package.json" ]; then
                PKG_NAME=$(node -p "require('./$pkg_dir/package.json').name" 2>/dev/null || echo "unknown")
                PKG_VERSION=$(node -p "require('./$pkg_dir/package.json').version" 2>/dev/null || echo "unknown")
                echo -e "  ${GREEN}✓${NC} $PKG_NAME@$PKG_VERSION"
            fi
        done
        ;;

    help|*)
        echo "UNRDF Common Tasks Automation"
        echo ""
        echo "Usage: $0 <command> [options]"
        echo ""
        echo "Commands:"
        echo "  new:package <name>    Scaffold a new package"
        echo "  clean:all             Clean all build artifacts and dependencies"
        echo "  update:deps           Update all dependencies safely"
        echo "  check:health          Run system health check"
        echo "  list:packages         List all packages in workspace"
        echo "  help                  Show this help message"
        echo ""
        echo "Examples:"
        echo "  $0 new:package my-feature      # Create new package"
        echo "  $0 clean:all                   # Deep clean"
        echo "  $0 update:deps                 # Safe dependency update"
        echo "  $0 check:health                # Validate environment"
        echo ""
        ;;
esac
