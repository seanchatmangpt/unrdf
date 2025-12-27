# UNRDF v6 CI/CD Pipeline Documentation

**Version:** 6.0.0-alpha.1
**Last Updated:** 2025-12-27
**Audience:** Maintainers, Release Engineers

---

## Overview

The UNRDF v6 CI/CD pipeline enforces sophisticated invariants including determinism, receipts, and L5 maturity requirements. It consists of three primary workflows:

1. **v6-validate.yml** - Runs on every PR to validate v6 release criteria
2. **v6-release.yml** - Automates releases when tags are pushed
3. **v6-regression.yml** - Continuous integration with regression detection

---

## Table of Contents

- [Pipeline Architecture](#pipeline-architecture)
- [Workflows](#workflows)
  - [1. v6-validate.yml](#1-v6-validateyml)
  - [2. v6-release.yml](#2-v6-releaseyml)
  - [3. v6-regression.yml](#3-v6-regressionyml)
- [Supporting Scripts](#supporting-scripts)
- [Release Promotion Path](#release-promotion-path)
- [Troubleshooting](#troubleshooting)
- [Maintenance Tasks](#maintenance-tasks)

---

## Pipeline Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Pull Request                            │
└───────────────────┬─────────────────────────────────────────┘
                    │
                    ▼
        ┌───────────────────────┐
        │  v6-validate.yml      │
        │  ─────────────────    │
        │  ✓ 14 criteria        │
        │  ✓ OTEL ≥80/100      │
        │  ✓ Coverage ≥80%     │
        │  ✓ Perf <10% regr    │
        │  ✓ ESLint 0 errors   │
        └───────────┬───────────┘
                    │
                    ▼
            ┌───────────────┐
            │ PR Comment    │
            │ with Report   │
            └───────────────┘

┌─────────────────────────────────────────────────────────────┐
│                  Tag Push (v6.*)                            │
└───────────────────┬─────────────────────────────────────────┘
                    │
                    ▼
        ┌───────────────────────┐
        │  v6-release.yml       │
        │  ─────────────────    │
        │  ✓ Pre-checks         │
        │  ✓ Build packages     │
        │  ✓ Generate notes     │
        │  ✓ Publish npm        │
        │  ✓ GitHub release     │
        └───────────┬───────────┘
                    │
                    ▼
            ┌───────────────┐
            │ npm + GitHub  │
            │ Release       │
            └───────────────┘

┌─────────────────────────────────────────────────────────────┐
│             Push to main/develop + Daily                    │
└───────────────────┬─────────────────────────────────────────┘
                    │
                    ▼
        ┌───────────────────────┐
        │  v6-regression.yml    │
        │  ─────────────────    │
        │  ✓ Lint (5s)          │
        │  ✓ Tests (30s)        │
        │  ✓ Integration (60s)  │
        │  ✓ Build (10s)        │
        │  ✓ Compare baseline   │
        └───────────┬───────────┘
                    │
                    ▼
        ┌───────────────────────┐
        │ Baseline Metrics      │
        │ Storage               │
        └───────────────────────┘
```

---

## Workflows

### 1. v6-validate.yml

**Trigger:** Pull requests to any branch affecting v6 packages
**Purpose:** Validate all 14 v6 release criteria before merge
**Duration:** ~5-10 minutes

#### Jobs

1. **validation-criteria**
   - Runs `scripts/v6-validate.mjs --comprehensive`
   - Validates directory structure, files, module imports, smoke tests
   - Outputs: passed/failed/warnings counts

2. **performance-benchmarks**
   - Runs `pnpm benchmark:core`
   - Compares against baseline (downloads from artifacts)
   - Flags regressions >10%
   - Stores current metrics as new baseline

3. **package-tests**
   - Runs `pnpm test` across all packages
   - Reports PASS count
   - Enforces 100% pass rate

4. **eslint-validation**
   - Runs `pnpm lint` with 5s timeout
   - Enforces 400+ rules
   - Must have 0 violations

5. **coverage-check**
   - Runs `pnpm test:coverage`
   - Checks threshold ≥80%
   - Fails if below threshold

6. **breaking-changes**
   - Tests v6-compat package
   - Ensures v5 API compatibility maintained
   - Reports breaking change count

7. **otel-validation**
   - Runs `validation/run-all.mjs comprehensive`
   - Extracts OTEL score
   - Must be ≥80/100

8. **pr-comment**
   - Downloads all artifacts
   - Generates formatted markdown report
   - Posts to PR using sticky comment

9. **validation-complete**
   - Final status check
   - Displays badge (✅ v6-ready / ❌ needs work)

#### Configuration

```yaml
env:
  NODE_VERSION: '18'
  PNPM_VERSION: '10.15.0'
  VALIDATION_TIMEOUT: '5s'
  BENCHMARK_TIMEOUT: '30s'
  TEST_TIMEOUT: '30s'
```

#### Artifacts

- `v6-validation-report` (30 days)
- `v6-baseline-metrics` (90 days)
- `v6-test-results` (30 days)
- `v6-coverage-report` (30 days)
- `v6-otel-report` (30 days)

---

### 2. v6-release.yml

**Trigger:** Tag push matching `v6.*.*` pattern or manual dispatch
**Purpose:** Automated release with validation and npm publishing
**Duration:** ~10-15 minutes

#### Jobs

1. **pre-release-checks**
   - Extracts version from tag
   - Determines release type (alpha/beta/rc/stable)
   - Runs all 14 validation checks
   - Validates OTEL score ≥80/100
   - Checks performance regression <10%
   - Verifies no breaking changes
   - Ensures all tests pass
   - Must ALL pass before proceeding

2. **build-packages**
   - Builds `@unrdf/v6-core`
   - Builds `@unrdf/v6-compat`
   - Verifies dist/ directories exist
   - Creates npm pack tarballs
   - Uploads as artifacts

3. **generate-release-notes**
   - Runs `.github/scripts/release-notes.mjs`
   - Generates comprehensive markdown
   - Includes receipt with signature
   - Categorizes commits (features/fixes/breaking)
   - Uploads release notes artifact

4. **publish-npm**
   - Publishes to npm registry
   - Uses appropriate tag (alpha/beta/rc/latest)
   - Requires `npm-publish` environment
   - Uses `NPM_TOKEN` secret

5. **create-github-release**
   - Creates GitHub release
   - Attaches package tarballs
   - Generates SHA256SUMS.txt
   - Marks as prerelease if not stable

6. **announce-release**
   - Posts to GitHub Discussions (if stable)
   - Prints announcement to logs
   - Includes migration guide link

7. **release-metrics**
   - Records release metadata
   - Stores promotion path info
   - Uploads metrics (365 days retention)

#### Release Types

| Type | Tag Pattern | npm Tag | Requirements |
|------|-------------|---------|--------------|
| alpha | `v6.0.0-alpha.1` | `alpha` | Rolling, any PR merged |
| beta | `v6.0.0-beta.1` | `beta` | Consensus + 7-day soak |
| rc | `v6.0.0-rc.1` | `rc` | 3x external user testing |
| stable | `v6.0.0` | `latest` | Final with guarantees |

#### Required Secrets

- `NPM_TOKEN` - npm authentication token with publish permissions
- `GITHUB_TOKEN` - Auto-provided by Actions (for releases)

#### Manual Release

```bash
# Create and push tag
git tag v6.0.0-alpha.2
git push origin v6.0.0-alpha.2

# Or use workflow dispatch in GitHub UI
```

---

### 3. v6-regression.yml

**Trigger:** Push to main/develop, PRs, daily at 2 AM UTC
**Purpose:** Continuous integration with regression detection
**Duration:** ~3-5 minutes

#### Jobs

1. **lint-check** (5s timeout)
   - Fast linting with strict SLA
   - Measures actual duration
   - Warns if >5s
   - Fails if violations found

2. **unit-tests** (30s timeout)
   - Matrix: Node 18, 20, 22
   - Runs full test suite
   - Coverage check on Node 18 only
   - Must be ≥80%

3. **integration-tests** (60s timeout)
   - Checks for integration test files
   - Runs with timeout enforcement
   - Measures duration

4. **build-check** (10s timeout)
   - Builds v6-core
   - Verifies dist/ artifact
   - Measures build time

5. **regression-detection**
   - Downloads previous baseline
   - Runs current benchmarks
   - Compares metrics
   - Flags >10% slowdown
   - Stores new baseline (main branch only)

6. **test-pass-rate**
   - Calculates test success %
   - Tracks over time
   - Alerts if drops

7. **benchmark-tracking**
   - Compares against last green build
   - Detects >10% slowdown
   - Stores successful benchmarks

8. **ci-summary**
   - Aggregates all job results
   - Prints comprehensive report

#### Baseline Metrics

Stored in artifacts, compared on each run:

```json
{
  "timestamp": "2025-12-27T11:00:00Z",
  "commit": "abc123",
  "branch": "main",
  "metrics": {
    "lint_duration": "2",
    "test_duration": "25",
    "build_duration": "8",
    "coverage": "85.5"
  }
}
```

#### Andon Principle

If any timeout fires → STOP and fix root cause. Don't just increase timeout.

---

## Supporting Scripts

### .github/scripts/pr-comment.mjs

Generates formatted PR comments with validation results.

**Usage:**
```bash
node .github/scripts/pr-comment.mjs \
  --validation-report validation.json \
  --coverage coverage-summary.json \
  --output pr-comment.md
```

**Output Example:**
```markdown
## v6 Validation Report

### ✅ Overall Status: **PASS**

### 📋 Validation Summary
| Metric | Count |
|--------|-------|
| ✅ Passed | 28 |
| ❌ Failed | 0 |
| ⚠️ Warnings | 2 |

### 📊 Test Coverage
| Metric | Coverage |
|--------|----------|
| ✅ Lines | 85.2% |
```

---

### .github/scripts/baseline-metrics.mjs

Compares performance metrics and detects regressions.

**Usage:**
```bash
node .github/scripts/baseline-metrics.mjs compare \
  .baseline/baseline.json \
  .current/baseline.json \
  --threshold 10
```

**Output:**
```
═══════════════════════════════════════
Performance Regression Analysis
═══════════════════════════════════════

Threshold: 10%
Baseline: abc123 (2025-12-26T10:00:00Z)
Current:  def456 (2025-12-27T11:00:00Z)

Summary:
  Total Metrics: 4
  Regressions:   0
  Improvements:  1
  Unchanged:     3

✅ No regressions detected
```

---

### .github/scripts/release-notes.mjs

Generates comprehensive release notes with receipt.

**Usage:**
```bash
node .github/scripts/release-notes.mjs \
  --version 6.0.0-alpha.2 \
  --type alpha \
  --output release-notes.md
```

**Output:** Full markdown with:
- Release type badge
- Categorized commits
- Installation instructions
- Migration guide link
- Cryptographic receipt
- Validation checklist
- Artifact digests

---

## Release Promotion Path

```
┌─────────┐     Consensus     ┌──────┐     3x External    ┌────┐     Final
│  Alpha  │  ──────────────>  │ Beta │  ───────────────>  │ RC │  ──────────>  │ Stable │
│ Rolling │   + 7-day soak    │      │      Testing       │    │   Review      │  v6.0  │
└─────────┘                   └──────┘                    └────┘               └────────┘
   |                             |                          |                       |
   v                             v                          v                       v
Any PR                      Stabilization             Pre-release             Production
Merged                      Period                    Validation              Guarantee
```

### Stage Requirements

1. **Alpha (v6.0.0-alpha.N)**
   - Requirements: PR merged to main
   - Frequency: Rolling releases
   - Stability: Expect breaking changes
   - Audience: Early adopters, testing

2. **Beta (v6.0.0-beta.N)**
   - Requirements: Team consensus + 7-day soak time
   - Frequency: Milestone-based
   - Stability: API stabilizing, fewer breaks
   - Audience: Integration testing

3. **RC (v6.0.0-rc.N)**
   - Requirements: 3+ external user tests
   - Frequency: Pre-release only
   - Stability: Production-candidate
   - Audience: Pre-production validation

4. **Stable (v6.0.0)**
   - Requirements: RC validation + final review
   - Frequency: Major/minor releases
   - Stability: Full guarantees
   - Audience: Production users

---

## Troubleshooting

### Workflow Failures

#### v6-validate.yml fails

**Problem:** Validation criteria not met

**Solutions:**
1. Check validation report artifact
2. Run `node scripts/v6-validate.mjs --comprehensive` locally
3. Review failed checks in PR comment
4. Fix issues and push update

**Common Issues:**
- Missing directory structure → Create expected dirs
- Module import failures → Check exports in index.mjs
- OTEL score <80 → Review validation/run-all.mjs output
- Coverage <80% → Add more tests

---

#### v6-release.yml fails

**Problem:** Pre-release checks fail

**Solutions:**
1. Verify tag format: `v6.X.Y` or `v6.X.Y-{alpha|beta|rc}.N`
2. Ensure package.json version matches tag
3. Check all validation criteria pass
4. Review pre-release-checks job logs

**Common Issues:**
- Version mismatch → Update package.json
- NPM_TOKEN expired → Regenerate in npm settings
- OTEL validation fails → Run locally first
- Tests fail → Fix and retag

---

#### v6-regression.yml fails

**Problem:** Performance regression detected

**Solutions:**
1. Download regression-report.json artifact
2. Review which metrics regressed
3. Investigate performance changes
4. Optimize or update baseline if intentional

**Common Issues:**
- Lint timeout → Investigate slow linter rules
- Test timeout → Parallelize or optimize tests
- Build timeout → Check for new dependencies
- >10% slowdown → Profile and optimize

---

### Artifact Issues

**Problem:** Baseline metrics not found

**Solution:**
```bash
# Manually create baseline on main branch
git checkout main
pnpm benchmark:core > baseline.json
# Upload via workflow dispatch or commit
```

**Problem:** Artifacts expired

**Solution:**
- v6-baseline-metrics: 90-day retention (extend if needed)
- Others: 30 days is intentional for cleanup

---

## Maintenance Tasks

### Weekly

- [ ] Review open PRs with failed validation
- [ ] Check for stuck workflows
- [ ] Monitor artifact storage usage

### Monthly

- [ ] Review baseline metrics accuracy
- [ ] Update dependencies in workflows
- [ ] Audit timeout SLAs (are they still appropriate?)
- [ ] Check npm package downloads

### Quarterly

- [ ] Review release promotion criteria
- [ ] Update documentation
- [ ] Audit GitHub Actions minutes usage
- [ ] Test disaster recovery (restore from artifacts)

### Before Major Release

- [ ] Verify all 14 criteria documented
- [ ] Test release workflow on RC branch
- [ ] Prepare migration guide
- [ ] Coordinate with users for RC testing
- [ ] Generate comprehensive changelog

---

## Metrics & Observability

### Key Metrics

1. **Validation Success Rate**
   - Target: >95% on first run
   - Alert: <80% indicates process issues

2. **OTEL Score**
   - Target: ≥80/100
   - Alert: <80 blocks release

3. **Test Coverage**
   - Target: ≥80%
   - Alert: Declining trend

4. **Performance Regression**
   - Target: <10% slowdown
   - Alert: >10% requires investigation

5. **Workflow Duration**
   - Validate: ~5-10 min
   - Release: ~10-15 min
   - Regression: ~3-5 min

### Monitoring

```bash
# Check recent workflow runs
gh run list --workflow=v6-validate.yml --limit 10

# View workflow status
gh run view <run-id>

# Download artifacts
gh run download <run-id> --name v6-validation-report

# Check npm package
npm view @unrdf/v6-core versions
```

---

## References

- [GitHub Actions Documentation](https://docs.github.com/actions)
- [v6 Validation Script](../../scripts/v6-validate.mjs)
- [Migration Plan](./MIGRATION_PLAN.md)
- [Program Charter](./PROGRAM_CHARTER.md)
- [Maturity Ladder](./MATURITY_LADDER.md)

---

## Support

For issues with CI/CD pipeline:

1. Check this documentation
2. Review workflow logs in GitHub Actions
3. Test scripts locally
4. Open issue with `ci/cd` label
5. Tag @maintainers for urgent issues

---

**Document Maintenance:**
- Review quarterly
- Update on workflow changes
- Keep examples current
- Version with v6 releases
