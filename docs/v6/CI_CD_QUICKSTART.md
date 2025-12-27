# v6 CI/CD Pipeline - Quick Start Guide

Get started with the UNRDF v6 automated CI/CD pipeline in 5 minutes.

---

## For Contributors (Making a PR)

### 1. Create Your Branch
```bash
git checkout -b feature/my-v6-feature
# Make your changes to packages/v6-core or packages/v6-compat
```

### 2. Push and Create PR
```bash
git add .
git commit -m "feat: add new v6 feature"
git push origin feature/my-v6-feature
# Create PR on GitHub
```

### 3. Watch Validation Run
The `v6-validate.yml` workflow will automatically:
- ✅ Run all 14 v6 validation criteria
- ✅ Check OTEL score (must be ≥80/100)
- ✅ Verify test coverage (must be ≥80%)
- ✅ Run performance benchmarks
- ✅ Check for breaking changes
- ✅ Post results as PR comment

### 4. Review PR Comment
You'll get a comment like this:

```markdown
## v6 Validation Report

### ✅ Overall Status: **PASS**

### 📋 Validation Summary
| Metric | Count |
|--------|-------|
| ✅ Passed | 28 |
| ❌ Failed | 0 |
| ⚠️ Warnings | 2 |

### 🚀 Release Readiness
✅ **Ready for v6 release**
```

### 5. Fix Issues (if needed)
If validation fails:
```bash
# Run validation locally first
node scripts/v6-validate.mjs --comprehensive

# Fix issues
# ...

# Push again
git push
```

---

## For Maintainers (Creating a Release)

### Alpha Release (Rolling)
```bash
# Update version in package.json
npm version 6.0.0-alpha.2 --no-git-tag-version

# Commit and create tag
git add packages/v6-core/package.json packages/v6-compat/package.json
git commit -m "chore: bump to v6.0.0-alpha.2"
git tag v6.0.0-alpha.2
git push origin main --tags
```

The `v6-release.yml` workflow will:
- ✅ Validate all pre-release checks
- ✅ Build packages
- ✅ Generate release notes with receipt
- ✅ Publish to npm with `--tag alpha`
- ✅ Create GitHub release

### Beta Release (Consensus + 7-day soak)
```bash
# After team consensus and 7-day testing period
git tag v6.0.0-beta.1
git push origin --tags
```

### RC Release (3x external testing)
```bash
# After successful external user testing
git tag v6.0.0-rc.1
git push origin --tags
```

### Stable Release (Production)
```bash
# Final release with guarantees
git tag v6.0.0
git push origin --tags
```

---

## For Monitoring (Regression Detection)

The `v6-regression.yml` workflow runs automatically:
- On every push to main/develop
- On every pull request
- Daily at 2 AM UTC (catch slow regressions)

### Check Workflow Status
```bash
# View recent runs
gh run list --workflow=v6-regression.yml --limit 5

# View specific run
gh run view <run-id>

# Download artifacts
gh run download <run-id> --name v6-baseline-metrics
```

### Review Regression Reports
If >10% performance regression detected:

1. Download `regression-report.json` artifact
2. Review which metrics regressed
3. Investigate and optimize
4. Update baseline if change is intentional

---

## Troubleshooting

### Validation Fails Locally But Passes in CI
```bash
# Ensure clean state
pnpm install --frozen-lockfile

# Run exactly what CI runs
timeout 5s node scripts/v6-validate.mjs --comprehensive
```

### OTEL Score <80
```bash
# Run OTEL validation locally
node validation/run-all.mjs comprehensive

# Check for failures
grep "FAILED\|Error" validation-output.log
```

### Coverage <80%
```bash
# Run tests with coverage
pnpm test:coverage

# View HTML report
open coverage/index.html
```

### Workflow Doesn't Trigger
Check trigger paths in workflow file:
```yaml
on:
  pull_request:
    paths:
      - 'packages/v6-core/**'  # Only triggers on v6 changes
      - 'packages/v6-compat/**'
```

---

## Key Files

| File | Purpose |
|------|---------|
| `.github/workflows/v6-validate.yml` | PR validation |
| `.github/workflows/v6-release.yml` | Automated releases |
| `.github/workflows/v6-regression.yml` | CI + regression tracking |
| `.github/scripts/pr-comment.mjs` | PR comment generator |
| `.github/scripts/baseline-metrics.mjs` | Performance comparison |
| `.github/scripts/release-notes.mjs` | Release notes generator |
| `scripts/v6-validate.mjs` | Core validation script |
| `docs/v6/CI_CD_PIPELINE.md` | Full documentation |

---

## Common Commands

```bash
# Run v6 validation locally
node scripts/v6-validate.mjs --comprehensive

# Run specific package tests
pnpm --filter @unrdf/v6-core test

# Run benchmarks
pnpm benchmark:core

# Check lint
timeout 5s pnpm lint

# Check coverage
pnpm test:coverage

# Generate release notes (maintainers)
node .github/scripts/release-notes.mjs \
  --version 6.0.0-alpha.2 \
  --type alpha \
  --output release-notes.md
```

---

## Need Help?

1. Read [Full CI/CD Documentation](./CI_CD_PIPELINE.md)
2. Check workflow logs in GitHub Actions
3. Open issue with `ci/cd` label
4. Tag @maintainers for urgent issues

---

**Quick Links:**
- [Migration Plan](./MIGRATION_PLAN.md)
- [Program Charter](./PROGRAM_CHARTER.md)
- [Maturity Ladder](./MATURITY_LADDER.md)
