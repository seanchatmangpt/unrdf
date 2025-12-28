#!/bin/bash
# Session start hook for UNRDF development
# Sets up development environment

echo "=================================="
echo "UNRDF v6.0.0 Development Session"
echo "=================================="

# Check Node.js version
if command -v node &> /dev/null; then
  NODE_VERSION=$(node --version)
  echo "Node.js: $NODE_VERSION"

  # Verify minimum version (18.0.0)
  MAJOR_VERSION=$(echo "$NODE_VERSION" | sed 's/v//' | cut -d. -f1)
  if [ "$MAJOR_VERSION" -lt 18 ]; then
    echo "WARNING: Node.js 18+ required (found $NODE_VERSION)"
  fi
else
  echo "ERROR: Node.js not found"
fi

# Check pnpm
if command -v pnpm &> /dev/null; then
  PNPM_VERSION=$(pnpm --version)
  echo "pnpm: v$PNPM_VERSION"
else
  echo "WARNING: pnpm not found - install with 'npm install -g pnpm'"
fi

# Check Git status
if command -v git &> /dev/null; then
  if git rev-parse --is-inside-work-tree &> /dev/null; then
    BRANCH=$(git branch --show-current 2>/dev/null || echo "unknown")
    echo "Git branch: $BRANCH"

    # Check for uncommitted changes
    CHANGES=$(git status --porcelain 2>/dev/null | wc -l)
    if [ "$CHANGES" -gt 0 ]; then
      echo "Uncommitted changes: $CHANGES files"
    fi
  fi
fi

# Quick health check
echo ""
echo "Quick Health Check:"

# Check if dependencies installed
if [ -d "node_modules" ]; then
  echo "  Dependencies: installed"
else
  echo "  Dependencies: NOT INSTALLED - run 'pnpm install'"
fi

# Check test status (quick)
if [ -f "package.json" ]; then
  if grep -q '"test"' package.json; then
    echo "  Test script: available"
  fi
fi

# Reminder of key rules
echo ""
echo "Key Rules:"
echo "  - Use Oxigraph, not N3 directly"
echo "  - Max 500 lines per file"
echo "  - 5s timeout default for commands"
echo "  - ZERO TODOs in production code"
echo "  - ZERO skipped tests"

echo ""
echo "Ready for development!"
echo "=================================="
