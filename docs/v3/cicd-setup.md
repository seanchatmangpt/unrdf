# CI/CD Setup Documentation

## Overview

UNRDF v3 uses GitHub Actions for automated testing, security scanning, and deployment. This document explains how the CI/CD pipelines work and how to use them.

## Architecture

The CI/CD system consists of three main workflows:

1. **CI Pipeline** (`.github/workflows/ci.yml`) - Continuous Integration
2. **Release Pipeline** (`.github/workflows/release.yml`) - Automated Releases
3. **Security Pipeline** (`.github/workflows/security.yml`) - Security Scanning

## CI Pipeline

### Triggers

The CI pipeline runs on:
- Push to `main` or `develop` branches
- Pull requests to `main` or `develop` branches

### Jobs

#### 1. TypeScript Gate
Ensures no TypeScript files are committed (project uses pure ESM + JSDoc).

**Validation:**
```bash
find . -name "*.ts" -o -name "*.tsx" -o -name "*.d.ts" | grep -v node_modules
```

**Why:** UNRDF v3 is ESM-only with JSDoc for type hints.

#### 2. Lint & Format
Checks code quality using ESLint and Prettier.

**Commands:**
```bash
pnpm lint              # ESLint check
pnpm format:check      # Prettier check
```

**Fix locally:**
```bash
pnpm lint:fix          # Auto-fix ESLint issues
pnpm format            # Auto-format with Prettier
```

#### 3. Test Suite (Matrix)
Runs tests across Node.js versions 18, 20, and 22.

**Test commands:**
```bash
pnpm test              # All tests with coverage
pnpm test:e2e          # E2E tests
pnpm test:dark-matter  # Dark matter 80/20 tests
```

**Coverage:**
- Uploaded to Codecov automatically
- Minimum coverage: 80%
- Report: `coverage/lcov-report/index.html`

#### 4. Security Audit
Scans dependencies for vulnerabilities.

**Commands:**
```bash
pnpm audit --audit-level moderate
```

**Fix vulnerabilities:**
```bash
pnpm audit --fix       # Auto-fix when possible
pnpm update            # Update dependencies
```

#### 5. Build & Package
Builds the project and verifies artifacts.

**Build process:**
```bash
pnpm install --frozen-lockfile
pnpm build
```

**Artifacts verified:**
- `dist/index.mjs` - Main entry point
- `dist/knowledge-engine.mjs` - Knowledge engine module
- All built modules load successfully

#### 6. Documentation
Generates API documentation using JSDoc.

**Command:**
```bash
pnpm docs              # Generate docs
pnpm docs:serve        # Serve locally
```

**Output:** `docs/api/`

#### 7. Integration Tests
Runs comprehensive integration tests.

**Command:**
```bash
pnpm test -- --testNamePattern="Integration"
```

#### 8. Final Validation
Validates project structure and configuration.

**Checks:**
- `package.json` has `"type": "module"`
- Main entry is `.mjs` file
- No `require()` in ESM modules
- No TypeScript files

## Release Pipeline

### Triggers

Releases are triggered by:
1. **Git tags** matching `v*` (e.g., `v2.2.0`)
2. **Manual workflow dispatch** with version input

### Creating a Release

#### Automated (Recommended)

1. Update version in `package.json`:
   ```bash
   pnpm version patch   # 2.1.1 -> 2.1.2
   pnpm version minor   # 2.1.1 -> 2.2.0
   pnpm version major   # 2.1.1 -> 3.0.0
   ```

2. Push the tag:
   ```bash
   git push origin main --tags
   ```

3. GitHub Actions will:
   - Run full test suite
   - Build release artifacts
   - Publish to npm
   - Build and push Docker image
   - Create GitHub release
   - Validate deployment

#### Manual

1. Go to Actions → Release & Publish
2. Click "Run workflow"
3. Enter version (e.g., `2.2.0`)
4. Select release type (patch/minor/major)
5. Click "Run workflow"

### Release Process

#### Phase 1: Validation
- Extract version from tag
- Verify version format (X.Y.Z or X.Y.Z-alpha.N)
- Check tag matches `package.json`
- Determine if prerelease (contains alpha/beta/rc)

#### Phase 2: Testing
- Run tests on Node 18, 20, 22
- Run linter
- Run E2E tests
- Upload coverage to Codecov

#### Phase 3: Build
- Install dependencies
- Build project (`pnpm build`)
- Verify build artifacts
- Create tarball
- Upload artifacts

#### Phase 4: Publish to npm
- Dry-run publish
- Publish with `--access public`
- Stable releases: `latest` tag
- Prereleases: `next` tag
- Verify published package

#### Phase 5: Docker
- Build multi-platform image (amd64, arm64)
- Push to GitHub Container Registry
- Tag: version, major.minor, major, latest
- Cache layers for faster builds

#### Phase 6: GitHub Release
- Generate changelog from commits
- Create release (draft=false)
- Attach tarball
- Auto-generate release notes

#### Phase 7: Post-Release Validation
- Install from npm globally
- Verify CLI works (`unrdf --version`)
- Pull and test Docker image
- Send notification

### Version Formats

**Stable releases:**
```
v2.1.1
v2.2.0
v3.0.0
```

**Prereleases:**
```
v2.2.0-alpha.1
v2.2.0-beta.1
v2.2.0-rc.1
```

### Required Secrets

Configure in **Settings → Secrets and variables → Actions**:

| Secret | Description | How to get |
|--------|-------------|------------|
| `NPM_TOKEN` | npm publish token | [npmjs.com/settings/tokens](https://www.npmjs.com/settings/tokens) (Automation token) |
| `GITHUB_TOKEN` | GitHub API token | Auto-provided by GitHub Actions |

### Docker Images

Published to GitHub Container Registry:

```bash
# Pull latest
docker pull ghcr.io/unrdf/unrdf:latest

# Pull specific version
docker pull ghcr.io/unrdf/unrdf:2.1.1

# Pull major version
docker pull ghcr.io/unrdf/unrdf:2

# Run
docker run -p 3000:3000 ghcr.io/unrdf/unrdf:latest
```

## Security Pipeline

### Triggers

Security scans run on:
- Push to `main` or `develop`
- Pull requests to `main` or `develop`
- **Daily at 2 AM UTC** (scheduled)
- Manual workflow dispatch

### Security Scans

#### 1. Dependency Audit
Uses `pnpm audit` to scan for vulnerabilities.

**Severity levels:**
- Critical: Immediate failure
- High: Immediate failure
- Moderate: Warning
- Low: Info only

**Reports:** `audit-report.json`

#### 2. SAST (Static Analysis)
Uses GitHub CodeQL for code analysis.

**Checks:**
- SQL injection vulnerabilities
- XSS vulnerabilities
- Path traversal
- Command injection
- Cryptographic issues
- Authentication bypass

**Results:** GitHub Security tab

#### 3. Secrets Detection
Scans for leaked secrets using:
- **TruffleHog** - Detects secrets in code history
- **Gitleaks** - Scans commits for secrets

**Patterns detected:**
- API keys
- Passwords
- Private keys
- Tokens
- Connection strings

#### 4. License Compliance
Verifies dependency licenses.

**Allowed licenses:**
- MIT
- Apache-2.0
- BSD-2-Clause, BSD-3-Clause
- ISC, 0BSD
- CC0-1.0
- Unlicense

**Blocked licenses:**
- GPL-2.0, GPL-3.0 (copyleft)
- LGPL-2.0, LGPL-3.0
- AGPL-3.0

**Reports:** `license-report.json`, `license-report.csv`

#### 5. Dependency Review (PRs only)
Reviews new dependencies in pull requests.

**Checks:**
- New vulnerabilities introduced
- License compatibility
- Malicious packages

#### 6. Supply Chain Analysis
Checks for:
- Known malicious packages
- Package integrity
- Post-install scripts
- Unexpected dependencies

#### 7. Container Security
Scans Docker images with:
- **Trivy** - Vulnerability scanner
- **Docker Scout** - Supply chain analysis

**Checks:**
- OS vulnerabilities
- Application vulnerabilities
- Misconfigurations
- Exposed secrets

#### 8. Security Best Practices
Custom checks for:
- Hardcoded secrets/passwords
- `eval()` usage (dangerous)
- `child_process.exec()` (command injection risk)
- Unsafe file operations
- Missing security headers

### Viewing Security Results

**GitHub Security Tab:**
1. Go to repository → Security
2. View:
   - Code scanning alerts (CodeQL)
   - Secret scanning alerts
   - Dependency alerts (Dependabot)

**Workflow Artifacts:**
- `audit-report.json` - Dependency vulnerabilities
- `license-report.json` - License inventory
- `trivy-results.sarif` - Container scan results

## Local Testing

### Run CI checks locally

```bash
# Lint
pnpm lint

# Tests
pnpm test
pnpm test:e2e

# Build
pnpm build

# Security audit
pnpm audit

# Full CI suite
pnpm ci:test
```

### Test with act (GitHub Actions locally)

Install [act](https://github.com/nektos/act):
```bash
brew install act  # macOS
```

Run workflows:
```bash
# Run CI workflow
act push

# Run specific job
act push -j test

# Run release workflow (requires secrets)
act push -e .github/workflows/release.yml -s NPM_TOKEN=xxx
```

### Validate workflow syntax

```bash
# Install actionlint
brew install actionlint

# Validate all workflows
actionlint .github/workflows/*.yml
```

## Debugging Failures

### Test failures

1. **Check test output:**
   ```bash
   Actions → CI/CD Pipeline → Test Suite → View logs
   ```

2. **Run locally:**
   ```bash
   pnpm test -- --reporter=verbose
   ```

3. **Check coverage:**
   ```bash
   pnpm test
   open coverage/lcov-report/index.html
   ```

### Build failures

1. **Check build logs:**
   ```bash
   Actions → CI/CD Pipeline → Build & Package → View logs
   ```

2. **Build locally:**
   ```bash
   pnpm install
   pnpm build
   ls -la dist/
   ```

3. **Test built module:**
   ```bash
   node -e "import('./dist/index.mjs').then(m => console.log(m))"
   ```

### Release failures

1. **NPM_TOKEN invalid:**
   - Regenerate at [npmjs.com/settings/tokens](https://www.npmjs.com/settings/tokens)
   - Update in GitHub Settings → Secrets

2. **Version mismatch:**
   ```bash
   # Ensure tag matches package.json
   git tag v2.1.1
   cat package.json | grep version
   ```

3. **Build artifacts missing:**
   - Check build job logs
   - Ensure `pnpm build` completes successfully

### Security scan failures

1. **High/Critical vulnerabilities:**
   ```bash
   pnpm audit --audit-level moderate
   pnpm audit --fix
   pnpm update
   ```

2. **Secrets detected:**
   - Remove secrets from code
   - Rotate compromised credentials
   - Use environment variables

3. **License issues:**
   ```bash
   npx license-checker --production --summary
   ```

## Performance Optimization

### Cache Strategy

GitHub Actions caches:
- `node_modules` (via `cache: 'pnpm'`)
- Docker build layers (`type=gha`)
- Test coverage reports

**Cache invalidation:**
- `pnpm-lock.yaml` changes → node_modules cache invalidated
- Dockerfile changes → Docker cache invalidated

### Build Time

**Current benchmarks:**
- Lint: ~30s
- Tests (single Node version): ~2 min
- Build: ~45s
- Docker build: ~3 min
- Total CI time: ~8-10 min

**Optimization tips:**
1. Use `--frozen-lockfile` (faster, reproducible)
2. Run jobs in parallel
3. Cache node_modules
4. Use Docker layer caching

## Best Practices

### Branch Protection

Configure in **Settings → Branches → Branch protection rules**:

- [x] Require status checks to pass
  - [x] TypeScript Gate
  - [x] Lint & Format
  - [x] Test Suite (Node 18)
  - [x] Security Audit
  - [x] Build & Package
- [x] Require pull request reviews (1 approver)
- [x] Dismiss stale reviews
- [x] Require linear history
- [x] Include administrators

### Commit Messages

Use [Conventional Commits](https://www.conventionalcommits.org/):

```
feat: add Knowledge Hook validation
fix: resolve SPARQL query memory leak
docs: update CI/CD documentation
test: add dark matter 80/20 tests
chore: upgrade dependencies
```

### Release Cadence

- **Patch** (2.1.1 → 2.1.2): Bug fixes, weekly
- **Minor** (2.1.0 → 2.2.0): New features, monthly
- **Major** (2.0.0 → 3.0.0): Breaking changes, quarterly

### Security Updates

- **Critical vulnerabilities:** Release patch within 24h
- **High vulnerabilities:** Release patch within 1 week
- **Moderate vulnerabilities:** Include in next minor release

## Monitoring

### Status Badges

Add to README.md:

```markdown
[![CI](https://github.com/unrdf/unrdf/actions/workflows/ci.yml/badge.svg)](https://github.com/unrdf/unrdf/actions/workflows/ci.yml)
[![Security](https://github.com/unrdf/unrdf/actions/workflows/security.yml/badge.svg)](https://github.com/unrdf/unrdf/actions/workflows/security.yml)
[![codecov](https://codecov.io/gh/unrdf/unrdf/branch/main/graph/badge.svg)](https://codecov.io/gh/unrdf/unrdf)
```

### Notifications

Configure in **Settings → Notifications**:
- Email on workflow failures
- Slack/Discord webhooks (optional)

## Troubleshooting

### Common Issues

**Issue:** "frozen-lockfile" error
```bash
# Solution: Commit pnpm-lock.yaml
git add pnpm-lock.yaml
git commit -m "chore: update lockfile"
```

**Issue:** Docker build OOM
```bash
# Solution: Reduce build concurrency
docker build --memory=4g --memory-swap=4g .
```

**Issue:** Tests timeout
```bash
# Solution: Increase timeout
vitest.config.mjs:
  testTimeout: 30000  // 30 seconds
```

**Issue:** NPM publish 403 Forbidden
```bash
# Solution: Verify NPM_TOKEN has publish permission
npm whoami  # Check logged-in user
npm access ls-packages  # Check package access
```

## Support

- **Documentation:** [docs/v3/](./README.md)
- **Issues:** [GitHub Issues](https://github.com/unrdf/unrdf/issues)
- **Discussions:** [GitHub Discussions](https://github.com/unrdf/unrdf/discussions)

## Changelog

### v3.0.0 (2025-01-15)
- Initial CI/CD pipeline setup
- GitHub Actions workflows (ci, release, security)
- Automated testing across Node 18, 20, 22
- npm publishing automation
- Docker multi-platform builds
- Comprehensive security scanning
- Documentation generation

---

**Last updated:** 2025-10-01
**Maintained by:** UNRDF Team
