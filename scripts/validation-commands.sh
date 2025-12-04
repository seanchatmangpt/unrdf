#!/bin/bash
# UNRDF Example Validation - Quick Reference Commands

echo "🔍 UNRDF Example Validation Commands"
echo "====================================="
echo ""

case "${1:-help}" in
  "full")
    echo "Running full validation on all 21 examples..."
    node scripts/validate-all-examples.mjs
    ;;

  "summary")
    echo "Validation Summary:"
    node scripts/validate-all-examples.mjs 2>&1 | tail -30
    ;;

  "passing")
    echo "Currently passing examples:"
    node scripts/validate-all-examples.mjs 2>&1 | grep "VALIDATION PASSED" | wc -l
    echo "examples fully validated"
    ;;

  "failing")
    echo "Currently failing examples:"
    node scripts/validate-all-examples.mjs 2>&1 | grep "VALIDATION FAILED" | wc -l
    echo "examples need work"
    ;;

  "report")
    echo "Opening detailed validation report..."
    cat docs/VALIDATION_REPORT.md | less
    ;;

  "quick")
    echo "Opening summary report..."
    cat docs/VALIDATION_SUMMARY.md | less
    ;;

  "memory")
    echo "Retrieving validation state from memory..."
    npx claude-flow@alpha memory retrieve "unrdf/vitest/validation" --reasoningbank
    ;;

  "test")
    if [ -z "$2" ] || [ -z "$3" ]; then
      echo "Usage: $0 test <package> <example>"
      echo "Example: $0 test core basic-store"
      exit 1
    fi
    echo "Testing $2/$3..."
    cd "packages/$2/examples/$3" && pnpm test
    ;;

  "coverage")
    if [ -z "$2" ] || [ -z "$3" ]; then
      echo "Usage: $0 coverage <package> <example>"
      echo "Example: $0 coverage core basic-store"
      exit 1
    fi
    echo "Generating coverage for $2/$3..."
    cd "packages/$2/examples/$3" && pnpm test:coverage
    ;;

  "watch")
    if [ -z "$2" ] || [ -z "$3" ]; then
      echo "Usage: $0 watch <package> <example>"
      echo "Example: $0 watch core basic-store"
      exit 1
    fi
    echo "Watching $2/$3 tests..."
    cd "packages/$2/examples/$3" && pnpm test:watch
    ;;

  "fix")
    echo "Running automated fixes..."
    node scripts/fix-all-examples.mjs
    ;;

  "stats")
    echo "📊 Validation Statistics:"
    echo ""
    echo "Total Examples: 21"
    echo "Validated: $(node scripts/validate-all-examples.mjs 2>&1 | grep "Validated:" | awk '{print $2}')"
    echo "Failed: $(node scripts/validate-all-examples.mjs 2>&1 | grep "^Failed:" | awk '{print $2}')"
    echo "Total Tests: $(node scripts/validate-all-examples.mjs 2>&1 | grep "Total tests found:" | awk '{print $4}')"
    echo "Pass Rate: $(node scripts/validate-all-examples.mjs 2>&1 | grep "Pass rate:" | awk '{print $3}')"
    ;;

  "packages")
    echo "📦 Validation by Package:"
    echo ""
    echo "✅ COMPLETE (100%):"
    echo "  - @unrdf/core (3/3)"
    echo "  - @unrdf/federation (2/2)"
    echo "  - @unrdf/cli (2/2)"
    echo "  - @unrdf/dark-matter (2/2)"
    echo ""
    echo "⚠️  PARTIAL:"
    echo "  - @unrdf/hooks (1/2)"
    echo ""
    echo "❌ BLOCKED:"
    echo "  - @unrdf/streaming (0/2) - Missing implementations"
    echo "  - @unrdf/browser (0/2) - Test execution errors"
    echo "  - @unrdf/knowledge-engine (0/2) - Test execution errors"
    echo "  - @unrdf/composables (0/2) - Below minimum tests"
    echo "  - full-stack (0/2) - Missing configs"
    ;;

  "next")
    echo "🎯 Next Steps (Priority Order):"
    echo ""
    echo "Phase 1: Quick Wins (1-2 hours)"
    echo "  1. Add vitest.config.mjs to full-stack/server"
    echo "  2. Add vitest.config.mjs to full-stack/web"
    echo "  3. Add 1 test to hooks/policy-hooks"
    echo "  → Result: 11/21 passing (52%)"
    echo ""
    echo "Phase 2: Implementation Fixes (4-6 hours)"
    echo "  1. Fix @unrdf/streaming missing methods"
    echo "  2. Debug browser examples"
    echo "  3. Debug knowledge-engine examples"
    echo "  4. Debug composables reactive-graphs"
    echo "  → Result: 18/21 passing (86%)"
    echo ""
    echo "Phase 3: Test Expansion (2-3 hours)"
    echo "  1. Add missing tests to reach minimums"
    echo "  → Result: 21/21 passing (100%) 🎯"
    ;;

  "help"|*)
    echo "Available commands:"
    echo ""
    echo "  full              Run complete validation on all examples"
    echo "  summary           Show validation summary"
    echo "  passing           Count passing examples"
    echo "  failing           Count failing examples"
    echo "  report            View detailed validation report"
    echo "  quick             View summary report"
    echo "  memory            Retrieve validation state from memory"
    echo "  test <pkg> <ex>   Test specific example"
    echo "  coverage <pkg> <ex> Generate coverage for example"
    echo "  watch <pkg> <ex>  Watch mode for example"
    echo "  fix               Run automated fixes"
    echo "  stats             Show validation statistics"
    echo "  packages          Show status by package"
    echo "  next              Show next steps"
    echo "  help              Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 full"
    echo "  $0 test core basic-store"
    echo "  $0 coverage federation peer-discovery"
    echo "  $0 packages"
    echo "  $0 next"
    ;;
esac
