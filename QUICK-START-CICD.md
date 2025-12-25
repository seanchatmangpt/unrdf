# CI/CD Quick Start Guide

Get the production-grade CI/CD pipeline running in 5 minutes.

## Step 1: Install Git Hooks (30 seconds)

```bash
cd /home/user/unrdf
node scripts/install-git-hooks.mjs
```

This installs:
- **pre-commit** - Fast quality checks (<5s)
- **pre-push** - Comprehensive validation (<2min)
- **commit-msg** - Conventional commit validation

## Step 2: Test Quality Gates Locally (2 minutes)

### Pre-commit (test with staged files)
```bash
# Stage some files
git add .

# Run pre-commit check
node scripts/quality-gates/pre-commit.mjs
```

**Expected:** File size checks, linting on staged files, TypeScript artifact detection

### Pre-push (full validation)
```bash
# Run pre-push check
node scripts/quality-gates/pre-push.mjs
```

**Expected:** Full test suite, coverage check, build verification

### PR Validation (comprehensive)
```bash
# Run PR validation
node scripts/quality-gates/pr-validation.mjs
```

**Expected:** All tests, coverage, security, performance, OTEL validation

## Step 3: Configure GitHub Secrets (1 minute)

Go to: `Settings > Secrets and variables > Actions`

Add secrets:
- **NPM_TOKEN** - Your npm publish token (for releases)
  - Get from: https://www.npmjs.com/settings/YOUR_USERNAME/tokens
  - Scope: Publish
- **GITHUB_TOKEN** - Auto-provided (no action needed)

## Step 4: Test Workflows (1 minute)

### Option A: Push to trigger CI
```bash
git add .
git commit -m "feat: add CI/CD pipeline"
git push
```

### Option B: Test locally with 'act'
```bash
# Install act (if not installed)
# brew install act  # macOS
# or download from https://github.com/nektos/act

# List workflows
act -l

# Test push workflow
act push
```

## Step 5: Verify Workflows Run

Go to GitHub Actions tab:
- https://github.com/YOUR_ORG/unrdf/actions

You should see workflows running:
1. CI/CD Pipeline
2. Security Scanning
3. Performance Tracking
4. Code Quality

## What You Get

### Automated on Every Push
- ✅ Multi-version testing (Node 18, 20, 22)
- ✅ Code coverage tracking (≥80%)
- ✅ Security scanning
- ✅ Performance benchmarks
- ✅ Code quality analysis

### Automated on Pull Requests
- ✅ All CI checks
- ✅ Performance regression detection
- ✅ Coverage comparison
- ✅ Security review
- ✅ Auto-comments with results

### Automated Weekly
- ✅ Dependency updates (patch versions)
- ✅ Security patches
- ✅ Cache optimization

### Automated on Release (git tag)
- ✅ Full validation
- ✅ npm publish
- ✅ Docker images
- ✅ GitHub release
- ✅ Changelog generation

## Common Commands

### Quality Gates
```bash
# Pre-commit (fast)
node scripts/quality-gates/pre-commit.mjs

# Pre-push (comprehensive)
node scripts/quality-gates/pre-push.mjs

# PR validation (full)
node scripts/quality-gates/pr-validation.mjs
```

### Documentation
```bash
# Generate API docs
node scripts/documentation/generate-api-docs.mjs

# Generate changelog
node scripts/documentation/generate-changelog.mjs
```

### Monitoring
```bash
# Collect CI metrics
node scripts/monitoring/ci-metrics.mjs

# Collect benchmark metrics
node scripts/monitoring/collect-benchmark-metrics.mjs

# Compare benchmarks
node scripts/monitoring/compare-benchmarks.mjs
```

## Troubleshooting

### Pre-commit is slow
**Issue:** Pre-commit taking >5 seconds

**Solution:**
```bash
# Check staged file count
git diff --cached --name-only | wc -l

# If >50 files, commit in smaller batches
```

### Tests failing in CI but pass locally
**Issue:** Tests pass locally but fail in CI

**Debug:**
```bash
# Run with same Node version as CI
nvm use 18
pnpm test

# Check for environment-specific issues
pnpm test -- --reporter=verbose
```

### Coverage below threshold
**Issue:** Coverage check fails

**Solution:**
```bash
# Generate coverage report
pnpm test:coverage

# View detailed report
open coverage/lcov-report/index.html

# Add tests for uncovered code
```

### Workflow not running
**Issue:** Push but no workflow triggered

**Check:**
1. Verify `.github/workflows/*.yml` files exist
2. Check branch protection rules
3. Review GitHub Actions settings
4. Check workflow logs for errors

## Next Steps

1. Review full documentation: `/home/user/unrdf/docs/CI-CD-PIPELINE.md`
2. Customize thresholds in scripts as needed
3. Configure branch protection rules in GitHub
4. Set up required status checks
5. Enable Dependabot (optional, complements dependency-update workflow)

## Performance SLAs

### Local Quality Gates
- Pre-commit: <5 seconds
- Pre-push: <2 minutes
- PR validation: <5 minutes

### CI Workflows
- Main CI: <10 minutes
- Security: <5 minutes
- Performance: <15 minutes
- Code Quality: <8 minutes

### Release
- Full release: <20 minutes

## Support

- **Documentation:** `/home/user/unrdf/docs/CI-CD-PIPELINE.md`
- **Implementation Summary:** `/home/user/unrdf/CI-CD-SUMMARY.md`
- **Scripts:** `/home/user/unrdf/scripts/*/`
- **Workflows:** `/home/user/unrdf/.github/workflows/`

---

**Ready to go?** Run `node scripts/install-git-hooks.mjs` to start!
