#!/bin/bash
# Correction Validation Suite
# Validates that refuted claims have been corrected

PHASE=${1:-all}
FAILED=0

echo "╔═══════════════════════════════════════════════════════════╗"
echo "║     ADVERSARIAL VALIDATION - CORRECTION CHECKER           ║"
echo "╚═══════════════════════════════════════════════════════════╝"
echo ""
echo "Phase: $PHASE"
echo "Date: $(date)"
echo ""

# P0-1: Zero defects
if [[ "$PHASE" == "all" || "$PHASE" == "phase1" ]]; then
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "P0-1: Checking 'Zero Defects' claims..."
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  COUNT=$(grep -ri "zero defects\|0 defects" /home/user/unrdf/docs/*.md 2>/dev/null | \
    grep -v "ADVERSARIAL\|VALIDATION\|CORRECTION\|90.4%\|measured\|test pass" | wc -l)

  if [ $COUNT -eq 0 ]; then
    echo "✅ PASS: No unjustified 'zero defects' claims"
  else
    echo "❌ FAIL: $COUNT unjustified 'zero defects' claims remain"
    echo ""
    echo "Examples:"
    grep -ri "zero defects\|0 defects" /home/user/unrdf/docs/*.md 2>/dev/null | \
      grep -v "ADVERSARIAL\|VALIDATION\|90.4%" | head -5
    FAILED=$((FAILED + 1))
  fi
  echo ""
fi

# P0-2: 99.997%
if [[ "$PHASE" == "all" || "$PHASE" == "phase1" ]]; then
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "P0-2: Checking '99.997% correctness' claims..."
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  COUNT=$(grep -r "99\.997" /home/user/unrdf/docs/*.md 2>/dev/null | \
    grep -v "theoretical\|ADVERSARIAL\|VALIDATION\|CORRECTION\|bound" | wc -l)

  if [ $COUNT -eq 0 ]; then
    echo "✅ PASS: All 99.997% claims include 'theoretical' qualifier"
  else
    echo "❌ FAIL: $COUNT claims missing 'theoretical' qualifier"
    echo ""
    echo "Examples:"
    grep -r "99\.997" /home/user/unrdf/docs/*.md 2>/dev/null | \
      grep -v "theoretical\|ADVERSARIAL\|bound" | head -5
    FAILED=$((FAILED + 1))
  fi
  echo ""
fi

# P0-3: Timeline
if [[ "$PHASE" == "all" || "$PHASE" == "phase1" ]]; then
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "P0-3: Checking Timeline (Nov 2024 → Dec 2025)..."
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  # Check critical thesis files
  CRITICAL_FILES=(
    "/home/user/unrdf/docs/PHD-THESIS-UNRDF-2028-REVOLUTION.md"
    "/home/user/unrdf/docs/HTF-HYPER-THESIS-FRAMEWORK.md"
  )

  TIMELINE_FAIL=0
  for file in "${CRITICAL_FILES[@]}"; do
    if [ -f "$file" ]; then
      if grep -q "November 18, 2024" "$file" | grep -v "Original Date\|Updated"; then
        echo "❌ CRITICAL: $file still has Nov 2024 date"
        TIMELINE_FAIL=$((TIMELINE_FAIL + 1))
      fi
    fi
  done

  COUNT=$(grep -r "November 18, 2024" /home/user/unrdf/docs/*.md 2>/dev/null | \
    grep -v "Original Date\|Updated\|ADVERSARIAL\|VALIDATION" | wc -l)

  if [ $COUNT -eq 0 ] && [ $TIMELINE_FAIL -eq 0 ]; then
    echo "✅ PASS: No Nov 2024 dates in active claims"
  else
    echo "❌ FAIL: $COUNT total Nov 2024 dates remain ($TIMELINE_FAIL critical)"
    echo ""
    echo "Examples:"
    grep -r "November 18, 2024" /home/user/unrdf/docs/*.md 2>/dev/null | \
      grep -v "Original\|Updated\|VALIDATION" | head -5
    FAILED=$((FAILED + 1))
  fi
  echo ""
fi

# P1-1: 13,027 LOC
if [[ "$PHASE" == "all" || "$PHASE" == "phase2" ]]; then
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "P1-1: Checking Microframework LOC (13,027 → 1,856)..."
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  COUNT=$(grep -r "13,027\|13027" /home/user/unrdf/docs/*.md 2>/dev/null | \
    grep -v "ADVERSARIAL\|VALIDATION\|CORRECTION\|EXCERPTS" | wc -l)

  if [ $COUNT -eq 0 ]; then
    echo "✅ PASS: No 13,027 LOC claims remain"
  else
    echo "❌ FAIL: $COUNT instances of 13,027 remain"
    grep -r "13,027" /home/user/unrdf/docs/*.md 2>/dev/null | \
      grep -v "VALIDATION\|CORRECTION" | head -3
    FAILED=$((FAILED + 1))
  fi
  echo ""
fi

# P1-2: Production-ready YAWL
if [[ "$PHASE" == "all" || "$PHASE" == "phase2" ]]; then
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "P1-2: Checking Production-ready YAWL..."
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  COUNT=$(grep -ri "production-ready.*yawl\|yawl.*production-ready" /home/user/unrdf/docs/*.md 2>/dev/null | \
    grep -v "prototype\|research\|ADVERSARIAL\|VALIDATION\|pending" | wc -l)

  if [ $COUNT -eq 0 ]; then
    echo "✅ PASS: No unjustified 'production-ready YAWL' claims"
  else
    echo "❌ FAIL: $COUNT 'production-ready YAWL' claims without qualifiers"
    grep -ri "production-ready.*yawl" /home/user/unrdf/docs/*.md 2>/dev/null | \
      grep -v "prototype\|VALIDATION" | head -3
    FAILED=$((FAILED + 1))
  fi
  echo ""
fi

# P1-3: Receipt throughput
if [[ "$PHASE" == "all" || "$PHASE" == "phase2" ]]; then
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "P1-3: Checking Receipt throughput (>100K)..."
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  COUNT=$(grep -ri ">100,000 receipts\|>100K receipts" /home/user/unrdf/docs/*.md 2>/dev/null | \
    grep -v "target\|optimize\|projected\|ADVERSARIAL\|VALIDATION" | wc -l)

  if [ $COUNT -eq 0 ]; then
    echo "✅ PASS: No unjustified >100K receipt claims"
  else
    echo "⚠️  WARNING: $COUNT >100K receipt claims (manual review recommended)"
    grep -ri ">100,000 receipts" /home/user/unrdf/docs/*.md 2>/dev/null | \
      grep -v "target\|VALIDATION" | head -3
    # Not counting as failure - may be intentional with context
  fi
  echo ""
fi

# P2-1: 700 LOC
if [[ "$PHASE" == "all" || "$PHASE" == "phase3" ]]; then
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "P2-1: Checking KGC-4D LOC (700 → 5,465)..."
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  COUNT=$(grep -r "\b700 LoC\|\b700 LOC" /home/user/unrdf/docs/*.md 2>/dev/null | \
    grep -vi "validation\|correction\|5,465" | wc -l)

  if [ $COUNT -eq 0 ]; then
    echo "✅ PASS: No uncorrected 700 LOC claims"
  else
    echo "⚠️  WARNING: $COUNT instances of 700 LOC (verify context)"
    grep -r "700 LoC" /home/user/unrdf/docs/*.md 2>/dev/null | head -3
  fi
  echo ""
fi

# P2-2: Workflow patterns
if [[ "$PHASE" == "all" || "$PHASE" == "phase3" ]]; then
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "P2-2: Checking Workflow patterns (20 → 14)..."
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  COUNT=$(grep -r "20 workflow patterns" /home/user/unrdf/docs/*.md 2>/dev/null | \
    grep -v "defines 20\|registry\|original YAWL\|VALIDATION\|14" | wc -l)

  if [ $COUNT -eq 0 ]; then
    echo "✅ PASS: Workflow pattern claims accurate (14 implemented)"
  else
    echo "⚠️  WARNING: $COUNT instances of '20 patterns' without qualification"
    grep -r "20 workflow patterns" /home/user/unrdf/docs/*.md 2>/dev/null | \
      grep -v "registry\|VALIDATION" | head -3
  fi
  echo ""
fi

# P2-3: Package count
if [[ "$PHASE" == "all" || "$PHASE" == "phase3" ]]; then
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "P2-3: Checking Package count (32 → 20)..."
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  COUNT=$(grep -r "32 packages" /home/user/unrdf/docs/*.md 2>/dev/null | \
    grep -v "ADVERSARIAL\|VALIDATION\|CORRECTION" | wc -l)

  if [ $COUNT -eq 0 ]; then
    echo "✅ PASS: No 32 package claims remain"
  else
    echo "❌ FAIL: $COUNT instances of '32 packages' remain"
    grep -r "32 packages" /home/user/unrdf/docs/*.md 2>/dev/null | head -3
    FAILED=$((FAILED + 1))
  fi
  echo ""
fi

# P3-1: Total LOC
if [[ "$PHASE" == "all" || "$PHASE" == "phase3" ]]; then
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "P3-1: Checking Total LOC (192,332 → 269,806)..."
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  COUNT=$(grep -r "192,332\|192332" /home/user/unrdf/docs/*.md 2>/dev/null | \
    grep -v "ADVERSARIAL\|VALIDATION\|CORRECTION" | wc -l)

  if [ $COUNT -eq 0 ]; then
    echo "✅ PASS: No 192,332 LOC claims remain"
  else
    echo "❌ FAIL: $COUNT instances of 192,332 remain"
    grep -r "192,332" /home/user/unrdf/docs/*.md 2>/dev/null | head -3
    FAILED=$((FAILED + 1))
  fi
  echo ""
fi

# Summary
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "VALIDATION SUMMARY"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ $FAILED -eq 0 ]; then
  echo "✅ ALL CHECKS PASSED"
  echo ""
  echo "Status: Ready for publication review"
  exit 0
else
  echo "❌ $FAILED CHECKS FAILED"
  echo ""
  echo "Status: Corrections incomplete - review failures above"
  exit 1
fi
