# CORRECTIONS QUICK-START GUIDE

**Goal**: Fix 11 refuted claims in 8-12 hours using 80/20 prioritization

**Strategy**: CORRECTION-STRATEGY.md (full details)
**This Guide**: Fastest path to publication-ready

---

## STEP 1: BASELINE VALIDATION (2 minutes)

See current state:

```bash
cd /home/user/unrdf
./validation/validate-corrections.sh all
```

Expected: **Multiple failures** (this shows what needs fixing)

---

## STEP 2: CREATE WORKING BRANCH (1 minute)

```bash
git checkout -b fix/refuted-claims
git branch  # Verify you're on the new branch
```

---

## STEP 3: PRIORITY 0 - SHOW-STOPPERS (5.5 hours)

### 3A: Zero Defects → 90.4% Test Pass Rate (2.5 hours)

**Most Critical Files**:
1. `/home/user/unrdf/CLAUDE.md` (line 56)
2. `/home/user/unrdf/docs/bb80-20-methodology.md` (line 241)
3. `/home/user/unrdf/docs/PHD-THESIS-UNRDF-2028-REVOLUTION-FINAL.md`
4. `/home/user/unrdf/docs/THESIS-BIGBANG-80-20-FINAL.md`

**Pattern**:
```bash
# Find all occurrences
grep -rn "zero defects\|0 defects" docs/*.md | grep -v "VALIDATION\|ADVERSARIAL"

# For each file, replace:
OLD: "zero defects" / "0 defects"
NEW: "90.4% test pass rate (85/94)" or "low defect rate"
```

**Use Edit tool**:
```javascript
Edit({
  file_path: "/home/user/unrdf/CLAUDE.md",
  old_string: "P(Correctness) ≥ 99.997% (theoretical bound)",
  new_string: "90.4% test pass rate (theoretical bound: 99.997%)"
})
```

### 3B: 99.997% Correctness (2 hours)

**Key Files**:
- CLAUDE.md
- docs/bb80-20-methodology.md
- docs/THESIS-BEYOND-HUMAN-PERCEPTION-*.md

**Pattern**:
```bash
# Find
grep -rn "99\.997" docs/*.md | grep -v "VALIDATION"

# Replace ALL with:
OLD: "P(Correctness) ≥ 99.997%"
NEW: "P(Correctness) ≥ 99.997% (theoretical bound); 90.4% measured"
```

### 3C: Timeline Nov 2024 → Dec 2025 (1 hour)

**CRITICAL FILES** (fix these first):
```bash
# File 1
/home/user/unrdf/docs/PHD-THESIS-UNRDF-2028-REVOLUTION.md
Line 5: "**Date:** November 18, 2024"
→ "**Date:** December 25, 2025"

# File 2
/home/user/unrdf/docs/HTF-HYPER-THESIS-FRAMEWORK.md
Lines 5, 747: Same replacement
```

**Check All**:
```bash
grep -rn "November 18, 2024" docs/*.md | grep -v "Original Date\|Updated\|VALIDATION"
```

**Validate P0**:
```bash
./validation/validate-corrections.sh phase1
# Must show ✅ PASS for all P0 checks
```

**Commit**:
```bash
git add -A
git commit -m "fix(P0): Zero defects → 90.4%, 99.997% qualified, timeline Dec 2025"
```

---

## STEP 4: PRIORITY 1 - CRITICAL (3.5 hours)

### 4A: 13,027 LOC → 1,856 LOC (1 hour)

**Simple replacement**:
```bash
# Find
grep -rn "13,027\|13027" docs/*.md | grep -v "VALIDATION"

# Replace ALL instances
OLD: 13,027
NEW: 1,856
```

**Files affected**: ~10 files

### 4B: Production-ready YAWL → Research Prototype (1 hour)

```bash
# Find
grep -rni "production-ready.*yawl\|yawl.*production-ready" docs/*.md

# Replace
OLD: "production-ready YAWL"
NEW: "research prototype YAWL"

OLD: "Production-ready"
NEW: "Research prototype with production-quality architecture"
```

### 4C: >100K receipts/sec → 2.4K-60K (1.5 hours)

**Replacement pattern**:
```bash
OLD: ">100,000 receipts/sec"
NEW: "2,400 receipts/sec (single-threaded); 60,000 receipts/sec (batched)"

OLD: "100K receipts/sec"
NEW: "60K receipts/sec (batched)"
```

**Files**: packages/yawl/*, docs/THESIS-UPGRADE-SYNTHESIS-2025.md, etc.

**Validate P1**:
```bash
./validation/validate-corrections.sh phase2
```

**Commit**:
```bash
git add -A
git commit -m "fix(P1): LOC metrics, YAWL status, throughput claims"
```

---

## STEP 5: PRIORITY 2-3 - MAJOR/MODERATE (2 hours)

### 5A: Quick Replacements (automated)

```bash
# 32 packages → 20
find docs -name "*.md" -type f -exec sed -i 's/\b32 packages\b/20 packages/g' {} +

# 192,332 → 269,806
find docs -name "*.md" -type f -exec sed -i 's/192,332/269,806/g' {} +

# Verify
grep -r "32 packages\|192,332" docs/*.md | grep -v "VALIDATION"
# Should be 0 results
```

### 5B: Manual Corrections

**700 LOC → 5,465 LOC** (context-sensitive):
```bash
grep -rn "\b700 LoC\|\b700 LOC" docs/*.md | grep -i kgc
# Replace only where it refers to KGC-4D
```

**20 patterns → 14 patterns**:
```bash
OLD: "20 workflow patterns"
NEW: "14 workflow patterns (from van der Aalst registry)"
```

**7 layers**:
```bash
OLD: "seven-layer architecture"
NEW: "seven-layer architecture (2 fully implemented: KGC-4D, YAWL; 5 projected)"
```

**Validate All**:
```bash
./validation/validate-corrections.sh all
```

**Commit**:
```bash
git add -A
git commit -m "fix(P2-P3): Pattern count, packages, layers, remaining LOC"
```

---

## STEP 6: FINAL VALIDATION (30 minutes)

### Run Full Validation Suite

```bash
./validation/validate-corrections.sh all > CORRECTION-VALIDATION-REPORT.md
cat CORRECTION-VALIDATION-REPORT.md
```

**Expected Output**:
```
✅ PASS: No unjustified 'zero defects' claims
✅ PASS: All 99.997% claims include 'theoretical' qualifier
✅ PASS: No Nov 2024 dates in active claims
✅ PASS: No 13,027 LOC claims remain
✅ PASS: No unjustified 'production-ready YAWL' claims
✅ PASS: No 32 package claims remain
✅ PASS: No 192,332 LOC claims remain

✅ ALL CHECKS PASSED
Status: Ready for publication review
```

### Manual Spot Checks

```bash
# 1. CLAUDE.md (most visible)
grep "90.4%\|test pass rate" /home/user/unrdf/CLAUDE.md
# Should show corrected claims

# 2. Main thesis
grep "December 2025" /home/user/unrdf/docs/PHD-THESIS-UNRDF-2028-REVOLUTION.md
# Should show corrected date

# 3. No false claims
grep -ri "zero defects\|0 defects" docs/bb80-20-methodology.md
# Should include "90.4%" or "measured"
```

### Generate Summary

```bash
cat > CORRECTIONS-SUMMARY.md <<'EOF'
# Refuted Claims Corrections - Summary

**Date**: $(date +%Y-%m-%d)
**Branch**: fix/refuted-claims
**Status**: COMPLETE

## Claims Fixed

| Priority | Claim | Occurrences | Status |
|----------|-------|-------------|--------|
| P0 | Zero defects → 90.4% | 71 | ✅ |
| P0 | 99.997% → qualified | 47 | ✅ |
| P0 | Nov 2024 → Dec 2025 | 15 | ✅ |
| P1 | 13,027 → 1,856 LOC | 10 | ✅ |
| P1 | Prod-ready → Research | 12 | ✅ |
| P1 | >100K → 2.4K-60K | 25 | ✅ |
| P2 | 700 → 5,465 LOC | 8 | ✅ |
| P2 | 20 → 14 patterns | 14 | ✅ |
| P2 | 32 → 20 packages | 9 | ✅ |
| P3 | 192K → 269K LOC | 8 | ✅ |
| P3 | 7 layers qualified | 7 | ✅ |

## Impact

- Academic credibility: **RESTORED**
- Show-stoppers: **RESOLVED** (3/3)
- Critical issues: **RESOLVED** (5/5)
- Publication readiness: **ACHIEVED**

## Files Modified

$(git diff --name-only main | wc -l) files changed
$(git diff --stat main | tail -1)

## Next Steps

1. ✅ External peer review
2. ⏳ Run full test suite
3. ⏳ Generate coverage reports
4. ⏳ Submit for publication
EOF
```

---

## STEP 7: COMMIT & PUSH (5 minutes)

```bash
# Final commit
git add -A
git commit -m "fix: Complete 11 refuted claims corrections

Adversarial validation findings fully addressed:
- Zero defects → 90.4% test pass rate (measured)
- 99.997% → Theoretical bound vs measured clarified
- Timeline: Nov 2024 → Dec 2025 (git-verified)
- All LOC metrics corrected (13,027→1,856, 700→5,465, 192K→269K)
- YAWL: production-ready → research prototype
- Receipt throughput: >100K → 2.4K-60K (benchmarked)
- Workflow patterns: 20 → 14 (code-verified)
- Package count: 32 → 20 (filesystem-verified)
- Architecture: 7 layers → 2 fully implemented

See: CORRECTION-STRATEGY.md, CORRECTION-VALIDATION-REPORT.md

Closes #[issue] if applicable
"

# Review changes
git log --oneline main..fix/refuted-claims

# Push (if remote configured)
git push origin fix/refuted-claims
```

---

## TROUBLESHOOTING

### Validation Script Fails

**Problem**: `./validation/validate-corrections.sh: Permission denied`

**Fix**:
```bash
chmod +x /home/user/unrdf/validation/validate-corrections.sh
```

### Too Many Occurrences

**Problem**: "71 occurrences of 'zero defects' is overwhelming"

**Fix**: Focus on key files first
```bash
# Top 5 files by occurrence
grep -r "zero defects\|0 defects" docs/*.md | cut -d: -f1 | sort | uniq -c | sort -rn | head -5

# Fix those 5 files (covers ~80% of occurrences)
```

### Can't Find File

**Problem**: `grep` returns no results

**Fix**: Check path
```bash
# From repo root
cd /home/user/unrdf
pwd  # Should show /home/user/unrdf

# Try with full path
grep -r "zero defects" /home/user/unrdf/docs/*.md
```

### Breaking Links

**Problem**: Edit breaks internal links

**Fix**: Search for references after each change
```bash
# After changing a heading or claim
grep -r "link-to-old-text" docs/
# Update any links found
```

---

## TIME ESTIMATES BY SKILL LEVEL

### Expert (familiar with codebase)
- P0: 3 hours
- P1: 2 hours
- P2-P3: 1 hour
- Validation: 0.5 hours
- **Total**: 6.5 hours

### Intermediate
- P0: 5.5 hours
- P1: 3.5 hours
- P2-P3: 2 hours
- Validation: 1 hour
- **Total**: 12 hours

### Beginner (first time)
- P0: 8 hours
- P1: 5 hours
- P2-P3: 3 hours
- Validation: 2 hours
- **Total**: 18 hours

---

## SUCCESS CRITERIA

Before considering work complete:

- [ ] `./validation/validate-corrections.sh all` shows "✅ ALL CHECKS PASSED"
- [ ] Git diff reviewed (no unintended changes)
- [ ] CLAUDE.md updated (most visible file)
- [ ] Main thesis files updated (PHD-THESIS-*.md)
- [ ] All 3 P0 corrections complete (show-stoppers)
- [ ] All 3 P1 corrections complete (critical)
- [ ] CORRECTIONS-SUMMARY.md created
- [ ] Branch committed with clear message
- [ ] Ready for peer review

---

## HELP & REFERENCES

**Full Strategy**: [CORRECTION-STRATEGY.md](CORRECTION-STRATEGY.md)

**Validation Reports**:
- [ADVERSARIAL-VALIDATION-FINAL.md](ADVERSARIAL-VALIDATION-FINAL.md) - What's wrong
- [CORRECTION-STRATEGY.md](CORRECTION-STRATEGY.md) - How to fix
- CORRECTION-VALIDATION-REPORT.md - Proof it's fixed (generated)

**Questions?**
1. Check CORRECTION-STRATEGY.md Part 5 (Manual Correction Guide)
2. Check Troubleshooting section above
3. Run validation script to see current state

---

**REMEMBER**: 80/20 rule applies:
- Fixing P0 (3 claims) = 60% of credibility issues
- Fixing P0 + P1 (6 claims) = 90% of credibility issues
- If time-limited, focus on P0 first

**START HERE**: Step 1 → Run baseline validation to see current state
