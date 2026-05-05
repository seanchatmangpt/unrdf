# CI/CD Pipeline Documentation

**Production-Grade Continuous Integration and Deployment**

## Overview

The UNRDF CI/CD pipeline provides comprehensive automation for testing, validation, security scanning, performance tracking, and deployment. Built on GitHub Actions with advanced quality gates and monitoring.

## Pipeline Architecture

```
┌─────────────────┐
│   Code Commit   │
└────────┬────────┘
         │
         v
┌─────────────────┐
│  Pre-commit     │ ← Fast checks (<5s)
│  Quality Gate   │   - File size
└────────┬────────┘   - Linting
         │            - Formatting
         v
┌─────────────────┐
│   Git Commit    │
└────────┬────────┘
         │
         v
┌─────────────────┐
│  Pre-push       │ ← Comprehensive (<2min)
│  Quality Gate   │   - Full test suite
└────────┬────────┘   - Coverage check
         │            - Build verification
         v
┌─────────────────┐
│   Git Push      │
└────────┬────────┘
         │
         v
┌─────────────────────────────────────────┐
│         GitHub Actions Workflows         │
├─────────────────────────────────────────┤
│  • CI Pipeline        (5-10 min)        │
│  • Security Scan      (3-5 min)         │
│  • Performance        (10-15 min)       │
│  • Code Quality       (5-8 min)         │
└────────┬────────────────────────────────┘
         │
         v
┌─────────────────┐
│  PR Validation  │ ← Full validation
│  (Pull Request) │   - All tests
└────────┬────────┘   - OTEL validation
         │            - Performance check
         v
┌─────────────────┐
│   Merge PR      │
└────────┬────────┘
         │
         v
┌─────────────────┐
│  Release Flow   │ ← Tag-triggered
│  (on git tag)   │   - Build & publish
└─────────────────┘   - npm publish
                      - Docker build
                      - GitHub release
```

## Workflows

### 1. CI Pipeline (`.github/workflows/ci.yml`)

**Triggers:** Push to main/develop, Pull Requests

**Jobs:**
- TypeScript Gate - Ensures no TS artifacts (fail fast)
- Lint & Format - ESLint + Prettier validation
- Test Suite - Multi-version testing (Node 18, 20, 22)
- Security Audit - Dependency vulnerability scanning
- Build & Package - Build verification
- Documentation - API docs generation
- Performance Benchmark - Performance regression detection
- Integration Tests - End-to-end validation
- Final Validation - ESM, package.json, imports

**Duration:** ~5-10 minutes

**Critical Checks:**
- ✅ 100% test pass rate
- ✅ 80%+ code coverage
- ✅ Zero linting errors
- ✅ Build succeeds

### 2. Security Scanning (`.github/workflows/security.yml`)

**Triggers:** Push, Pull Requests, Daily cron (2 AM UTC)

**Jobs:**
- Dependency Audit - pnpm audit
- SAST Analysis - CodeQL static analysis
- Secrets Detection - TruffleHog + Gitleaks
- License Compliance - License checker
- Supply Chain Analysis - Package integrity
- Container Scanning - Trivy + Docker Scout
- Security Best Practices - Pattern detection

**Duration:** ~3-5 minutes

**Critical Checks:**
- ✅ Zero critical/high vulnerabilities
- ✅ No secrets in code
- ✅ License compliance
- ✅ No eval() or unsafe patterns

### 3. Performance Tracking (`.github/workflows/performance-tracking.yml`)

**Triggers:** Push, Pull Requests, Daily cron (3 AM UTC)

**Jobs:**
- Benchmark Suite - Core, Streaming, Federation, YAWL
- Memory Profiling - Heap usage tracking
- Load Testing - Concurrent operation stress testing
- Performance Summary - Trend analysis

**Duration:** ~10-15 minutes

**Critical Checks:**
- ✅ No regressions >5%
- ✅ Memory within limits
- ✅ Throughput targets met

**Regression Detection:**
```javascript
// Automatic comparison against baseline
if (currentDuration > baseline * 1.05) {
  // Flag as regression
  // Comment on PR with details
}
```

### 4. Code Quality (`.github/workflows/code-quality.yml`)

**Triggers:** Push, Pull Requests

**Jobs:**
- Complexity Analysis - Cyclomatic complexity
- Duplication Detection - Code duplication <5%
- Type Coverage - JSDoc coverage ≥90%
- File Size Validation - Files <500 lines
- Dependency Analysis - Circular dependencies
- Bundle Size Analysis - Track bundle growth

**Duration:** ~5-8 minutes

**Critical Checks:**
- ✅ Complexity <10 per function
- ✅ Duplication <5%
- ✅ JSDoc coverage ≥90%

### 5. Dependency Updates (`.github/workflows/dependency-update.yml`)

**Triggers:** Weekly (Monday 9 AM UTC), Manual

**Jobs:**
- Check Outdated - Identify updates
- Update Patch - Auto-update patch versions
- Update Minor - Manual minor updates
- Security Updates - Auto-fix vulnerabilities

**Duration:** ~5-10 minutes

**Automation:**
- Patch versions: Automatic PR
- Security fixes: High-priority PR
- Minor versions: Manual approval
- Major versions: Manual only

### 6. Release Pipeline (`.github/workflows/release.yml`)

**Triggers:** Git tag (`v*`), Manual

**Jobs:**
- Validate Release - Version checks
- Test Suite - Full matrix (Node 18, 20, 22)
- Build Release - Artifact creation
- Publish npm - Package publication
- Docker Build - Container images
- GitHub Release - Release notes
- Post-validation - Verify deployment

**Duration:** ~15-20 minutes

**Critical Checks:**
- ✅ All tests pass
- ✅ Version consistency
- ✅ Build artifacts verified
- ✅ npm publish succeeds

### 7. Cache Optimization (`.github/workflows/cache-optimization.yml`)

**Triggers:** Weekly (Sunday 1 AM UTC), Manual

**Jobs:**
- Warmup Dependencies - Pre-cache for all Node versions
- Warmup Build - Pre-build artifacts
- Cleanup Caches - Remove caches >7 days old
- Cache Statistics - Usage reporting

**Benefits:**
- 3-5x faster CI runs
- Reduced GitHub Actions minutes
- Consistent build times

## Quality Gates

### Pre-commit (`scripts/quality-gates/pre-commit.mjs`)

**SLA:** <5 seconds

**Checks:**
```javascript
// Fast checks on staged files only
1. File size <500 lines
2. ESLint on changed files
3. No TypeScript artifacts
4. Basic formatting
```

**Usage:**
```bash
# Runs automatically on git commit
# Or manually:
node scripts/quality-gates/pre-commit.mjs
```

### Pre-push (`scripts/quality-gates/pre-push.mjs`)

**SLA:** <120 seconds

**Checks:**
```javascript
1. Full test suite (100% pass)
2. Code coverage ≥80%
3. Build succeeds
4. No broken imports
5. No N3 direct imports
6. Performance benchmarks
```

**Usage:**
```bash
# Runs automatically on git push
# Or manually:
node scripts/quality-gates/pre-push.mjs
```

### PR Validation (`scripts/quality-gates/pr-validation.mjs`)

**SLA:** <300 seconds

**Checks:**
```javascript
1. All tests pass (100%)
2. Coverage maintained ≥80%
3. Security scan clean
4. No performance regressions
5. OTEL validation ≥80/100
6. Documentation updated
```

**Usage:**
```bash
# Runs on PR creation
# Or manually:
node scripts/quality-gates/pr-validation.mjs
```

## Monitoring & Metrics

### CI Metrics Tracking (`scripts/monitoring/ci-metrics.mjs`)

Tracks:
- Build duration trends
- Test pass rates
- Coverage trends
- Failure patterns

**Output:** `ci-metrics.json` with historical data

### Performance Tracking (`scripts/monitoring/`)

Scripts:
- `collect-benchmark-metrics.mjs` - Aggregate performance data
- `compare-benchmarks.mjs` - Regression detection
- `memory-profiler.mjs` - Memory usage tracking
- `load-tester.mjs` - Concurrent operation testing

**Output:** JSON reports + PR comments

### JSDoc Coverage (`scripts/quality-gates/check-jsdoc-coverage.mjs`)

Checks:
- Function documentation
- Type annotations
- Parameter descriptions
- Return value docs

**Target:** ≥90% coverage

## Documentation Generation

### API Documentation (`scripts/documentation/generate-api-docs.mjs`)

**Features:**
- Extracts JSDoc from all packages
- Generates markdown per package
- Creates searchable index
- Updates automatically in CI

**Usage:**
```bash
node scripts/documentation/generate-api-docs.mjs
# Output: docs/api/*.md
```

### Changelog Generation (`scripts/documentation/generate-changelog.mjs`)

**Features:**
- Parses conventional commits
- Categorizes by type (feat, fix, etc.)
- Identifies breaking changes
- Suggests version bumps

**Usage:**
```bash
node scripts/documentation/generate-changelog.mjs
# Updates: CHANGELOG.md
```

## Git Hooks

### Installation

```bash
node scripts/install-git-hooks.mjs
```

### Installed Hooks

1. **pre-commit** - Quality checks on staged files
2. **pre-push** - Comprehensive validation
3. **commit-msg** - Conventional commit format validation

### Conventional Commits Format

```
type(scope): description

Types: feat, fix, docs, style, refactor, perf, test, chore, build, ci, revert

Examples:
  feat(core): add RDF streaming parser
  fix(federation): resolve timeout issues
  docs: update API documentation
  perf(yawl): optimize pattern matching
```

## Bypassing Checks

**Not recommended, but available:**

```bash
# Skip pre-commit
git commit --no-verify

# Skip pre-push
git push --no-verify
```

**When to bypass:**
- Emergency hotfix (still requires PR review)
- WIP commits on feature branch
- Known false positives (fix immediately)

## Troubleshooting

### CI Failures

#### Tests Failing

```bash
# Run locally
pnpm test

# Check specific package
pnpm -C packages/core test

# With coverage
pnpm test:coverage
```

#### Coverage Below Threshold

```bash
# Generate coverage report
pnpm test:coverage

# View report
open coverage/lcov-report/index.html

# Add tests for uncovered code
```

#### Build Failures

```bash
# Clean and rebuild
pnpm clean
pnpm install
pnpm build

# Check for syntax errors
pnpm lint
```

#### Security Vulnerabilities

```bash
# Check vulnerabilities
pnpm audit

# Auto-fix if possible
pnpm audit --fix

# Review manual fixes
pnpm audit --audit-level moderate
```

### Performance Issues

#### Slow CI Runs

1. Check cache utilization
2. Review parallel job configuration
3. Optimize test execution
4. Use test sharding

#### Cache Misses

```bash
# Manually warmup cache
gh workflow run cache-optimization.yml
```

### Quality Gate Failures

#### Pre-commit Too Slow

```bash
# Check file count
git diff --cached --name-only | wc -l

# If >50 files, consider splitting commit
```

#### Pre-push Timeout

```bash
# Run individual checks
pnpm test
pnpm lint
pnpm build

# Identify slow tests
pnpm test -- --reporter=verbose
```

## Best Practices

### Commit Workflow

1. **Stage changes**
   ```bash
   git add <files>
   ```

2. **Pre-commit runs** (automatic)
   - Fix any issues
   - Re-stage if needed

3. **Commit with conventional format**
   ```bash
   git commit -m "feat(scope): description"
   ```

4. **Pre-push runs** (automatic)
   - Fix any failures
   - Review test output

5. **Push to remote**
   ```bash
   git push
   ```

### Pull Request Workflow

1. **Create feature branch**
   ```bash
   git checkout -b feature/your-feature
   ```

2. **Make changes** with frequent commits

3. **Run PR validation locally**
   ```bash
   node scripts/quality-gates/pr-validation.mjs
   ```

4. **Push and create PR**
   ```bash
   git push -u origin feature/your-feature
   gh pr create
   ```

5. **Monitor CI** - All workflows must pass

6. **Address feedback** - Reviewers + automated checks

7. **Merge** - Squash or rebase as configured

### Release Workflow

1. **Prepare release**
   ```bash
   # Generate changelog
   node scripts/documentation/generate-changelog.mjs

   # Update version in package.json
   npm version [patch|minor|major]
   ```

2. **Create git tag**
   ```bash
   git tag -a v5.0.2 -m "Release v5.0.2"
   git push --tags
   ```

3. **Monitor release workflow**
   - All tests pass
   - npm publish succeeds
   - Docker images built
   - GitHub release created

4. **Verify deployment**
   ```bash
   npm view @unrdf/core@latest
   ```

## Metrics & KPIs

### Pipeline Performance

- **CI Duration:** <10 minutes (target: 5 minutes)
- **Security Scan:** <5 minutes (target: 3 minutes)
- **Performance Tests:** <15 minutes (target: 10 minutes)
- **Total PR Validation:** <30 minutes

### Quality Metrics

- **Test Pass Rate:** 100% (no failures allowed)
- **Code Coverage:** ≥80% (target: 90%)
- **JSDoc Coverage:** ≥90% (target: 100%)
- **Duplication:** <5% (target: <3%)
- **Complexity:** <10 per function (target: <7)

### Security Metrics

- **Critical Vulnerabilities:** 0 (block merge)
- **High Vulnerabilities:** 0 (block merge)
- **Moderate Vulnerabilities:** Review required
- **License Compliance:** 100%

### Performance Metrics

- **Regression Threshold:** 5% (warning)
- **Regression Limit:** 10% (block merge)
- **Memory Growth:** <10% per release
- **Bundle Size:** Track trends

## Advanced Features

### Parallel Testing

Workflows use matrix strategy for parallel execution:

```yaml
strategy:
  matrix:
    node-version: [18, 20, 22]
```

### Incremental Builds

Smart caching avoids rebuilding unchanged packages:

```yaml
- uses: actions/cache@v4
  with:
    path: packages/*/dist
    key: ${{ hashFiles('packages/*/src/**') }}
```

### Performance Tracking

Automatic baseline comparison with PR comments:

```yaml
- name: Comment PR with benchmark results
  uses: actions/github-script@v7
  # Generates table of regressions
```

### Security Automation

Multiple scanning layers:

1. Dependency audit (pnpm)
2. SAST (CodeQL)
3. Secrets detection (TruffleHog + Gitleaks)
4. Container scanning (Trivy + Docker Scout)
5. License compliance

### Auto-fixing

Some workflows auto-fix and create PRs:

- Dependency security patches
- Patch version updates
- Formatting issues (optional)

## Integration with External Tools

### Codecov

Coverage reports uploaded automatically:

```yaml
- uses: codecov/codecov-action@v4
```

### GitHub Security

SARIF reports uploaded to Security tab:

```yaml
- uses: github/codeql-action/upload-sarif@v3
```

### npm Registry

Automated publishing on release:

```yaml
- run: pnpm publish
  env:
    NODE_AUTH_TOKEN: ${{ secrets.NPM_TOKEN }}
```

### Docker Registry

Multi-platform images to GitHub Container Registry:

```yaml
platforms: linux/amd64,linux/arm64
```

## Maintenance

### Weekly Tasks

- Review dependency updates (automated PR)
- Check cache statistics
- Review security scan results
- Monitor performance trends

### Monthly Tasks

- Audit workflow efficiency
- Review quality gate thresholds
- Update documentation
- Clean up old branches/tags

### Quarterly Tasks

- Review and update CI/CD strategy
- Evaluate new tools/actions
- Performance optimization
- Team training on new features

## Support

### Documentation

- This guide: `docs/CI-CD-PIPELINE.md`
- GitHub Actions: `.github/workflows/*.yml`
- Quality Gates: `scripts/quality-gates/*.mjs`
- Monitoring: `scripts/monitoring/*.mjs`

### Getting Help

1. Check this documentation
2. Review workflow logs in GitHub Actions
3. Run scripts locally for debugging
4. Check GitHub Actions marketplace for action docs
5. Open issue with `ci/cd` label

## Appendix

### File Structure

```
.github/workflows/
  ├── ci.yml                    # Main CI pipeline
  ├── security.yml              # Security scanning
  ├── performance-tracking.yml  # Performance benchmarks
  ├── code-quality.yml          # Quality analysis
  ├── dependency-update.yml     # Auto-updates
  ├── release.yml               # Release automation
  └── cache-optimization.yml    # Cache management

scripts/
  ├── quality-gates/
  │   ├── pre-commit.mjs        # Pre-commit checks
  │   ├── pre-push.mjs          # Pre-push validation
  │   ├── pr-validation.mjs     # PR validation
  │   └── check-jsdoc-coverage.mjs
  ├── monitoring/
  │   ├── ci-metrics.mjs
  │   ├── collect-benchmark-metrics.mjs
  │   ├── compare-benchmarks.mjs
  │   ├── memory-profiler.mjs
  │   └── load-tester.mjs
  ├── documentation/
  │   ├── generate-api-docs.mjs
  │   └── generate-changelog.mjs
  └── install-git-hooks.mjs

docs/
  ├── CI-CD-PIPELINE.md         # This file
  └── api/                      # Generated API docs
```

### Environment Variables

GitHub Actions provides these automatically:

- `GITHUB_SHA` - Commit hash
- `GITHUB_REF_NAME` - Branch name
- `GITHUB_WORKFLOW` - Workflow name
- `GITHUB_RUN_ID` - Run ID
- `GITHUB_EVENT_NAME` - Trigger event

Custom secrets (configure in repo settings):

- `NPM_TOKEN` - npm publish token
- `GITHUB_TOKEN` - Provided automatically

### Useful Commands

```bash
# Run all quality gates locally
node scripts/quality-gates/pre-commit.mjs
node scripts/quality-gates/pre-push.mjs
node scripts/quality-gates/pr-validation.mjs

# Generate documentation
node scripts/documentation/generate-api-docs.mjs
node scripts/documentation/generate-changelog.mjs

# Run monitoring scripts
node scripts/monitoring/ci-metrics.mjs
node scripts/monitoring/collect-benchmark-metrics.mjs

# Install git hooks
node scripts/install-git-hooks.mjs

# Test workflows locally (requires 'act')
act -l                          # List workflows
act push                        # Test push event
act pull_request                # Test PR event
```

---

**Version:** 1.0.0
**Last Updated:** 2025-12-25
**Maintained By:** UNRDF Team
