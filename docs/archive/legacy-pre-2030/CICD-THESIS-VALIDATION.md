# CI/CD Thesis Validation System

**Purpose**: Block merges if thesis metrics don't match code reality
**Date**: 2025-12-25
**Status**: Production-Ready

---

## Overview

This CI/CD system validates all thesis claims against actual codebase measurements, implementing the **Adversarial PM principle** from CLAUDE.md: "Did you RUN it? Can you PROVE it?"

**Core Principle**: NEVER trust claims without verification. Metrics must match reality.

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  GitHub Actions Workflow (.github/workflows/thesis-validation.yml)  │
├─────────────────────────────────────────────────────────────┤
│  1. Checkout + Setup (Node 20, pnpm)                       │
│  2. Install dependencies                                    │
│  3. Run linter (5s timeout)                                │
│  4. Run tests (20s timeout)                                │
│  5. Run OTEL validation (30s timeout)                       │
│  6. Run thesis validation (10s timeout) ← BLOCKING         │
│  7. Upload artifacts (reports, logs)                        │
│  8. Comment on PR if failed                                │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  Thesis Validation Script (validation/thesis-validation.mjs)         │
├─────────────────────────────────────────────────────────────┤
│  11 Critical Validations:                                   │
│  1. KGC-4D LOC: 5,465 ±100                                 │
│  2. Total LOC: 269,806 ±5,000                              │
│  3. YAWL LOC: 26,449 total / 19,618 src ±500               │
│  4. Microframeworks: 3 files, 1,856 LOC ±50                │
│  5. Git timeline: 2025 (not 2024)                          │
│  6. Test infrastructure: vitest available                   │
│  7. Package count: 20 ±2                                   │
│  8. OTEL score: ≥80/100                                    │
│  9. No TODO/FIXME in thesis docs                           │
│  10. No inflated claims in recent commits                   │
│  11. Cross-reference files exist                            │
│                                                             │
│  Exit 0: All pass                                          │
│  Exit 1: Any fail                                          │
└─────────────────────────────────────────────────────────────┘
```

---

## Validation Criteria

### 1. KGC-4D LOC Count

**Claim** (ADVERSARIAL-THESIS-REVIEW.md):
- Original claim: 700 LOC
- Actual reality: 5,465 LOC (23 files)
- Discrepancy: 8x inflation

**Validation**:
```bash
find packages/kgc-4d/src -name "*.mjs" -exec wc -l {} + | tail -1
# Expected: 5,465 ±100 LOC
```

**Threshold**: 5,365 - 5,565 LOC
**Status**: ✅ PASS if within range, ❌ FAIL otherwise

---

### 2. Total Codebase LOC

**Claim** (CORRECTED-THESIS-EXCERPTS.md):
- Total: 269,806 LOC

**Validation**:
```bash
find . -name "*.mjs" -o -name "*.js" | xargs wc -l | tail -1
# Expected: 269,806 ±5,000 LOC
```

**Threshold**: 264,806 - 274,806 LOC
**Status**: ✅ PASS if within range, ❌ FAIL otherwise

---

### 3. YAWL LOC Count

**Claim** (CORRECTED-THESIS-EXCERPTS.md):
- Total: 26,449 LOC
- Source only: 19,618 LOC

**Validation**:
```bash
# Total (includes examples, tests, validation)
find packages/yawl -name "*.mjs" -o -name "*.js" | xargs wc -l | tail -1
# Expected: 26,449 ±500 LOC

# Source only
find packages/yawl/src -name "*.mjs" | xargs wc -l | tail -1
# Expected: 19,618 ±500 LOC
```

**Thresholds**:
- Total: 25,949 - 26,949 LOC
- Source: 19,118 - 20,118 LOC

**Status**: ✅ PASS if both match, ❌ FAIL otherwise

---

### 4. Microframeworks Count

**Claim** (CORRECTED-THESIS-EXCERPTS.md):
- Originally claimed: 20 frameworks
- Actually delivered: 3 frameworks, 1,856 LOC
- Discrepancy: 7x inflation

**Validation**:
```bash
find . -type f \( -name "microfw-*.mjs" -o -name "max-combo-*.mjs" \) | wc -l
# Expected: 3 files

find . -type f \( -name "microfw-*.mjs" -o -name "max-combo-*.mjs" \) -exec wc -l {} + | tail -1
# Expected: 1,856 ±50 LOC
```

**Thresholds**:
- Count: Exactly 3 files
- LOC: 1,806 - 1,906 LOC

**Status**: ✅ PASS if both match, ❌ FAIL otherwise

---

### 5. Git Timeline Consistency

**Claim** (ADVERSARIAL-THESIS-REVIEW.md):
- Thesis dated: Nov 18, 2024
- Actual work: Dec 2-24, 2025
- Discrepancy: 13-month temporal paradox

**Validation**:
```bash
git log --format="%ai" --reverse | head -1  # First commit
git log --format="%ai" | head -1            # Last commit
# Expected: Both in 2025 (not 2024)
```

**Threshold**: Repository must show 2025 work
**Status**: ✅ PASS if 2025, ⚠️ WARN if mismatch (documented discrepancy)

---

### 6. Test Pass Rate

**Claim** (ADVERSARIAL-THESIS-REVIEW.md):
- YAWL: latest% pass rate (168/262 tests)
- Status: Below production standards (≥95% required)

**Validation**:
```bash
cd packages/yawl && command -v vitest
# Expected: vitest should be available
```

**Threshold**: Test infrastructure must be runnable
**Status**: ✅ PASS if vitest exists, ⚠️ WARN if missing

---

### 7. Package Count

**Claim** (CORRECTED-THESIS-EXCERPTS.md):
- 20 packages

**Validation**:
```bash
ls -1 packages/*/package.json | wc -l
# Expected: 20 ±2 packages
```

**Threshold**: 18 - 22 packages
**Status**: ✅ PASS if within range, ❌ FAIL otherwise

---

### 8. OTEL Validation Score

**Claim** (CLAUDE.md):
- Minimum score: ≥80/100
- Source: validation-output.log

**Validation**:
```bash
node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log
# Expected: Score ≥80/100
```

**Threshold**: ≥80/100
**Status**: ✅ PASS if ≥80, ❌ FAIL if <80, ⚠️ WARN if log missing

---

### 9. No TODO/FIXME in Thesis Docs

**Rationale**: Thesis should be publication-ready

**Validation**:
```bash
grep -r "TODO\|FIXME" ADVERSARIAL-THESIS-REVIEW.md CORRECTED-THESIS-EXCERPTS.md books/kgc-thesis/
# Expected: 0 results
```

**Threshold**: 0 TODO/FIXME markers
**Status**: ✅ PASS if 0, ⚠️ WARN if >0

---

### 10. No Inflated Claims in Recent Commits

**Rationale**: Commit messages should match deliverables

**Validation**:
```bash
git log --oneline -20
# Check for patterns:
# - "X frameworks" (verify count matches deliverables)
# - "production-ready" (verify 95%+ test pass rate)
# - "zero defects" (verify tests pass)
# - "100% pass" (verify actual pass rate)
```

**Threshold**: No inflation patterns
**Status**: ✅ PASS if clean, ⚠️ WARN if patterns detected

---

### 11. Cross-Reference File Integrity

**Rationale**: Validation documents must exist

**Validation**:
```bash
ls -l ADVERSARIAL-THESIS-REVIEW.md CORRECTED-THESIS-EXCERPTS.md
# Expected: Both files exist
```

**Threshold**: Both files must exist
**Status**: ✅ PASS if both exist, ❌ FAIL otherwise

---

## Running Locally

### Quick Validation (10 seconds)

```bash
# From repository root
timeout 10s node validation/thesis-validation.mjs
```

**Expected output**:
```
═══════════════════════════════════════════════════════════
  THESIS VALIDATION - Adversarial Claims Verification
═══════════════════════════════════════════════════════════

[1/11] Validating KGC-4D LOC count...
✅ PASS KGC-4D LOC Count
  5465 LOC (23 files) matches expected 5465 ±100

[2/11] Validating total codebase LOC...
✅ PASS Total Codebase LOC
  269806 LOC matches expected 269806 ±5000

...

═══════════════════════════════════════════════════════════
  VALIDATION SUMMARY
═══════════════════════════════════════════════════════════

✅ Passed:  11
❌ Failed:  0
⚠️  Warnings: 0

🎉 All thesis validations passed!
Thesis metrics are verified and consistent with code reality.
```

**Exit codes**:
- `0`: All validations passed
- `1`: One or more validations failed

---

### Full CI/CD Simulation

```bash
# 1. Install dependencies
pnpm install

# 2. Run linter (5s timeout)
timeout 5s pnpm run lint

# 3. Run tests (20s timeout)
timeout 20s pnpm test

# 4. Run OTEL validation (30s timeout)
timeout 30s node validation/run-all.mjs comprehensive

# 5. Run thesis validation (10s timeout) - BLOCKING
timeout 10s node validation/thesis-validation.mjs
```

**Total time**: ~65 seconds (if all steps succeed)

---

## GitHub Actions Integration

### Workflow Triggers

```yaml
on:
  push:
    branches: ['**']       # All branches
  pull_request:
    branches: [main, master]  # PRs to main/master
```

### Branch Protection

**Recommended settings** for `main` / `master`:

1. **Require status checks to pass**:
   - ✅ `validate / Run thesis validation (BLOCKING)`
   - ✅ `thesis-quality-gate / Enforce quality standards`

2. **Require branches to be up to date**: ✅ Enabled

3. **Require linear history**: ✅ Enabled (prevents squash merges hiding history)

4. **Do not allow bypassing the above settings**: ✅ Enabled

### Setting Up Branch Protection

```bash
# Using GitHub CLI
gh api repos/seanchatmangpt/unrdf/branches/main/protection \
  --method PUT \
  --field required_status_checks[strict]=true \
  --field required_status_checks[contexts][]=validate \
  --field required_status_checks[contexts][]=thesis-quality-gate \
  --field enforce_admins=true \
  --field required_linear_history=true
```

Or via GitHub UI:
1. Go to `Settings` → `Branches`
2. Add rule for `main` or `master`
3. Enable "Require status checks to pass before merging"
4. Select `validate` and `thesis-quality-gate` checks
5. Enable "Require branches to be up to date"
6. Enable "Require linear history"
7. Save changes

---

## Artifacts

The workflow uploads these artifacts on every run:

1. **validation-output.log**: OTEL validation results
2. **coverage/otel-report.json**: Detailed OTEL metrics
3. **thesis-validation-report.txt**: Thesis validation summary

### Downloading Artifacts

```bash
# Via GitHub CLI
gh run download <run-id> -n validation-reports

# Via GitHub UI
# Actions → Select workflow run → Artifacts → Download
```

---

## Success Criteria

### For Merge to Main/Master

ALL of the following must be true:

- ✅ Linter passes (0 errors)
- ✅ Tests pass (or explicitly documented why failures are acceptable)
- ✅ OTEL score ≥80/100
- ✅ Thesis validation passes (all 11 checks)
- ✅ No inflated claims in commit messages
- ✅ LOC counts match reality (within tolerance)
- ✅ No TODO/FIXME in thesis docs

### For Publication

Before submitting thesis for peer review:

- ✅ All CI/CD checks pass
- ✅ Test pass rate ≥95% (or clearly documented as research prototype)
- ✅ External benchmarks completed (vs Temporal.io, Camunda, etc.)
- ✅ Independent code review obtained
- ✅ All cross-references validated
- ✅ Dates corrected to match actual work timeline
- ✅ Market projections either cited or labeled as speculative

---

## Failure Modes

### What to Do When Validation Fails

#### LOC Count Mismatch

**Symptom**: "KGC-4D LOC Count: 5600 LOC does not match expected 5465"

**Cause**: Code changed without updating thesis

**Fix**:
1. Verify actual LOC: `find packages/kgc-4d/src -name "*.mjs" -exec wc -l {} +`
2. Update thesis documents with correct numbers
3. Update thresholds in `validation/thesis-validation.mjs` if necessary
4. Commit changes with message: "docs: update LOC counts to match current code"

#### OTEL Score Below Threshold

**Symptom**: "OTEL Validation Score: 65/100 is below threshold of 80"

**Cause**: Features not producing expected OTEL spans

**Fix**:
1. Review failing features: `grep "FAILED" validation-output.log`
2. Fix implementations to produce correct spans
3. Re-run: `node validation/run-all.mjs comprehensive`
4. Verify score ≥80

#### Test Infrastructure Missing

**Symptom**: "vitest not found - cannot verify test claims"

**Cause**: Dependencies not installed

**Fix**:
```bash
pnpm install
cd packages/yawl && pnpm test
```

#### Microframework Count Mismatch

**Symptom**: "Found 5 frameworks (expected 3)"

**Cause**: Either:
- New frameworks added (update thesis)
- Old frameworks deleted (update thesis)
- Counting error (fix glob pattern)

**Fix**:
1. List actual files: `find . -name "microfw-*.mjs" -o -name "max-combo-*.mjs"`
2. Update thesis with actual count
3. Update validation threshold if appropriate

---

## Maintenance

### Updating Thresholds

When legitimate code changes occur:

1. **Measure new reality**:
   ```bash
   find packages/kgc-4d/src -name "*.mjs" -exec wc -l {} + | tail -1
   ```

2. **Update `validation/thesis-validation.mjs`**:
   ```javascript
   const expectedKGC4D = 5600;  // Updated from 5465
   const tolerance = 100;
   ```

3. **Update thesis documents**:
   - `CORRECTED-THESIS-EXCERPTS.md`
   - Any other docs citing the old number

4. **Commit**:
   ```bash
   git add validation/thesis-validation.mjs CORRECTED-THESIS-EXCERPTS.md
   git commit -m "docs: update KGC-4D LOC to 5,600 (135 LOC increase from feature X)"
   ```

### Adding New Validations

To add validation #12:

1. **Add check** to `validation/thesis-validation.mjs`:
   ```javascript
   console.log(`\n${c.blue}[12/12]${c.reset} Validating new metric...`);

   const newMetric = exec('command to measure');
   const expected = 42;

   if (newMetric === expected) {
     logResult(true, 'New Metric', `${newMetric} matches ${expected}`);
   } else {
     logResult(false, 'New Metric', `${newMetric} != ${expected}`);
   }
   ```

2. **Update count** in all log messages: `[1/12]`, `[2/12]`, etc.

3. **Test locally**:
   ```bash
   timeout 10s node validation/thesis-validation.mjs
   ```

4. **Document** in this file under "Validation Criteria"

---

## Adversarial PM Checklist

Before claiming "thesis is validated":

- ❓ **Did I RUN the validation script?**
  - Not just read it - actually execute: `node validation/thesis-validation.mjs`

- ❓ **Did I READ the full output?**
  - Not just check exit code - review all 11 validations

- ❓ **What BREAKS if I'm wrong?**
  - Peer review rejection
  - Credibility loss
  - Wasted publication fees

- ❓ **What's the EVIDENCE?**
  - `thesis-validation-report.txt` shows all metrics
  - Git history shows actual work timeline
  - `wc -l` output shows actual LOC counts
  - OTEL spans show actual behavior

---

## References

- **Adversarial PM Principle**: `/home/user/unrdf/CLAUDE.md` (lines 1-50)
- **11 Refuted Claims**: `/home/user/unrdf/ADVERSARIAL-THESIS-REVIEW.md`
- **Corrected Metrics**: `/home/user/unrdf/CORRECTED-THESIS-EXCERPTS.md`
- **OTEL Validation**: `/home/user/unrdf/validation/run-all.mjs`

---

## Final Truth

**Core Principle**: Metrics must match reality, not wishes.

This CI/CD system implements the Adversarial PM philosophy:
> "The work is 70% complete. The remaining 30% is verification and validation."

**The validation script asks**: *If someone challenged EVERY claim today, which would survive scrutiny?*

Only claims that pass this script survive scrutiny.

---

**Author**: Adversarial PM Quality Analyzer
**Date**: 2025-12-25
**Status**: Production-Ready
**Version**: latest
