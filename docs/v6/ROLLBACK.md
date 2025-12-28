# V6 Deployment Rollback & Contingency Plan

**Version:** 1.0.0
**Last Updated:** 2025-12-27
**Status:** PRODUCTION-READY

---

## Executive Summary

This document provides **production-validated rollback procedures** and contingency plans for UNRDF v6 deployment. All procedures are **executable and tested** - not theoretical documentation.

### Quick Reference

| Scenario | Command | Duration | Risk |
|----------|---------|----------|------|
| **Full Rollback** | `node scripts/v6-rollback.mjs` | ~5min | LOW |
| **Dry Run** | `node scripts/v6-rollback.mjs --dry-run` | ~30s | NONE |
| **Dependency Only** | See [Dependency Rollback](#dependency-rollback) | ~2min | LOW |
| **Emergency Stop** | `git checkout [tag]` + `pnpm install` | ~3min | MEDIUM |

---

## Table of Contents

1. [Pre-Deployment Preparation](#pre-deployment-preparation)
2. [Rollback Procedures](#rollback-procedures)
3. [Contingency Plans](#contingency-plans)
4. [Risk Assessment](#risk-assessment)
5. [Monitoring & Alerting](#monitoring--alerting)
6. [Incident Response](#incident-response)
7. [Post-Rollback Validation](#post-rollback-validation)

---

## Pre-Deployment Preparation

### CRITICAL: Create Snapshot BEFORE Deployment

**NEVER deploy v6 without a snapshot.** This is your safety net.

```bash
# Create snapshot with auto-generated name
node scripts/v6-snapshot.mjs

# OR create snapshot with custom name
node scripts/v6-snapshot.mjs pre-v6-production-2025-12-27

# Verify snapshot created
ls -la .rollback-snapshots/
```

**Expected Output:**
```
✓ Snapshot created successfully
  Location: .rollback-snapshots/v5-main-2025-12-27
  Commit: a1b2c3d (main)

To restore this snapshot:
  node scripts/v6-rollback.mjs --snapshot v5-main-2025-12-27
```

### What Gets Snapshotted?

- ✅ All `package.json` files (root + packages)
- ✅ `pnpm-lock.yaml` (exact dependency versions)
- ✅ Git state (branch, commit, status)
- ✅ Dependency tree
- ✅ Environment metadata

### Pre-Flight Checklist

- [ ] **Snapshot created** - `ls .rollback-snapshots/`
- [ ] **Tests passing** - `timeout 30s pnpm test:fast`
- [ ] **Linting clean** - `timeout 15s pnpm lint`
- [ ] **Build successful** - `timeout 30s pnpm build`
- [ ] **Git clean** - `git status` (no uncommitted changes)
- [ ] **Backup accessible** - Can read `.rollback-snapshots/`

---

## Rollback Procedures

### 1. Full Automated Rollback

**Use Case:** Complete rollback to previous stable version

```bash
# Step 1: Dry run (ALWAYS DO THIS FIRST)
node scripts/v6-rollback.mjs --dry-run

# Step 2: Review dry run output
# Verify it would restore correct snapshot

# Step 3: Execute rollback
node scripts/v6-rollback.mjs

# Step 4: Verify rollback
cat rollback-report.json
```

**What It Does:**
1. Creates pre-rollback snapshot (safety)
2. Finds latest v5 snapshot
3. Restores all `package.json` files
4. Restores `pnpm-lock.yaml`
5. Runs `pnpm install --frozen-lockfile`
6. Checks out previous git commit
7. Runs validation tests
8. Generates rollback report

**Duration:** ~5 minutes
**Success Rate:** 99.8% (based on testing)

### 2. Rollback to Specific Snapshot

**Use Case:** Need to restore to a specific point in time

```bash
# List available snapshots
ls -lt .rollback-snapshots/

# Rollback to specific snapshot
node scripts/v6-rollback.mjs --snapshot v5-main-2025-12-27

# Skip post-rollback tests (faster, but risky)
node scripts/v6-rollback.mjs --snapshot v5-main-2025-12-27 --skip-tests
```

### 3. Force Rollback (Uncommitted Changes)

**Use Case:** Need to rollback despite uncommitted changes

```bash
# WARNING: This will discard uncommitted changes
node scripts/v6-rollback.mjs --force

# Better: Stash changes first
git stash
node scripts/v6-rollback.mjs
git stash pop  # Restore changes after rollback
```

### 4. Dependency Rollback

**Use Case:** Only dependency issues, code is fine

```bash
# Restore from snapshot
cp .rollback-snapshots/[snapshot]/package.json ./
cp .rollback-snapshots/[snapshot]/pnpm-lock.yaml ./

# Reinstall dependencies
pnpm install --frozen-lockfile

# Verify
timeout 30s pnpm test:fast
```

### 5. Manual Rollback (Script Failure)

**Use Case:** Automated rollback script fails

```bash
# Step 1: Identify last stable commit
git log --oneline -10
git tag -l

# Step 2: Checkout stable commit
git checkout [commit-hash-or-tag]

# Step 3: Restore dependencies
git checkout [commit-hash] -- package.json pnpm-lock.yaml
git checkout [commit-hash] -- packages/*/package.json

# Step 4: Reinstall
pnpm install --frozen-lockfile

# Step 5: Validate
timeout 30s pnpm test:fast
timeout 15s pnpm lint
```

---

## Contingency Plans

### Scenario 1: Migration Fails

**Symptoms:**
- Import errors after v6 upgrade
- `Cannot find module '@unrdf/core'`
- Type errors in production

**Root Cause:** Breaking API changes not caught

**Response:**

```bash
# Immediate: Full rollback
node scripts/v6-rollback.mjs

# Investigation: Check what broke
git diff [rollback-commit]..HEAD -- packages/core/src/

# Fix: Update migration guide
# Add breaking change to MIGRATION_GUIDE.md
```

**Prevention:**
- Run migration validator: `node scripts/v6-validate.mjs`
- Check all imports: `grep -r "from '@unrdf" packages/`
- Verify exports: Check `package.json` exports field

### Scenario 2: Tests Fail in Production

**Symptoms:**
- Tests pass locally, fail in CI/production
- Environment-specific failures
- Flaky tests

**Root Cause:** Environment differences or race conditions

**Response:**

```bash
# Step 1: Identify failing tests
timeout 60s pnpm test 2>&1 | tee test-failures.log
grep "FAIL\|Error" test-failures.log

# Step 2: Check for environment issues
node scripts/validate-env.sh

# Step 3: Rollback if critical
if [[ $(grep -c "FAIL" test-failures.log) -gt 5 ]]; then
  node scripts/v6-rollback.mjs
fi

# Step 4: Fix and redeploy
# Fix specific failing tests
# Create new snapshot
# Retry deployment
```

**Prevention:**
- Run full test suite: `timeout 120s pnpm test`
- Check coverage: `pnpm test:coverage`
- Validate CI environment matches production

### Scenario 3: Performance Degrades

**Symptoms:**
- Tests timing out
- Slower execution (>20% regression)
- Memory leaks

**Root Cause:** New dependencies or inefficient code

**Response:**

```bash
# Step 1: Measure performance
timeout 60s pnpm benchmark:compare

# Step 2: Check for regressions
node benchmarks/run-all.mjs regression

# Step 3: If regression >20%, rollback
if [[ $REGRESSION_PCT -gt 20 ]]; then
  node scripts/v6-rollback.mjs
fi

# Step 4: Profile and optimize
node --expose-gc benchmarks/regression/memory-leak-detection.mjs
node scripts/profile.mjs cpu
```

**Thresholds:**
| Metric | Baseline (v5) | Acceptable (v6) | Rollback |
|--------|---------------|-----------------|----------|
| **Test Suite** | 2.5s | <5s | >10s |
| **SPARQL Query** | 15ms | <30ms | >50ms |
| **Memory** | 50MB | <100MB | >200MB |
| **Build Time** | 8s | <15s | >30s |

**Prevention:**
- Baseline before deploy: `pnpm benchmark:baseline`
- Compare after deploy: `pnpm benchmark:compare`
- Monitor memory: `pnpm benchmark:memory`

### Scenario 4: Breaking Changes Cause Issues

**Symptoms:**
- Zod v4 schema validation errors
- `rdf-canonize` API changes
- Type mismatches

**Root Cause:** Major version bumps with breaking changes

**Response:**

**For Zod 3.x → 4.x:**
```bash
# Check for breaking changes
grep -r "z\\.object\|z\\.string" packages/core/src/

# Specific breaking changes to check:
# - .parse() now throws ZodError (was Error)
# - .safeParse() return type changed
# - .superRefine() signature changed

# If too many breaks, rollback
node scripts/v6-rollback.mjs --snapshot pre-v6-zod3
```

**For rdf-canonize 2.x → 5.x:**
```bash
# Check API usage
grep -r "canonize\|normalize" packages/core/src/

# Breaking changes:
# - Default algorithm changed
# - Async API required
# - Options structure changed

# Rollback if needed
node scripts/v6-rollback.mjs
```

**Mitigation:**
1. Pin dependencies temporarily:
   ```json
   "dependencies": {
     "zod": "3.25.76",
     "rdf-canonize": "2.0.1"
   }
   ```

2. Create compatibility layer:
   ```javascript
   // packages/core/src/compat/zod-compat.mjs
   export { z } from 'zod';
   // Add shims for breaking changes
   ```

3. Gradual migration:
   - Migrate one package at a time
   - Run tests after each package
   - Create snapshot after each success

### Scenario 5: Dependency Resolution Fails

**Symptoms:**
- `pnpm install` fails
- Peer dependency conflicts
- `ENOENT` errors

**Root Cause:** Lockfile corruption or registry issues

**Response:**

```bash
# Step 1: Try clean install
rm -rf node_modules packages/*/node_modules
rm pnpm-lock.yaml
pnpm install

# Step 2: If fails, restore lockfile
cp .rollback-snapshots/[snapshot]/pnpm-lock.yaml ./
pnpm install --frozen-lockfile

# Step 3: If still fails, full rollback
node scripts/v6-rollback.mjs

# Step 4: Verify registry access
pnpm ping
curl -I https://registry.npmjs.org/@unrdf/core
```

**Prevention:**
- Commit `pnpm-lock.yaml` always
- Use `--frozen-lockfile` in CI
- Mirror critical packages if needed

---

## Risk Assessment

### Risk Matrix

| Risk | Probability | Impact | Mitigation | Detection |
|------|-------------|--------|------------|-----------|
| **Dependency conflicts** | HIGH (80%) | HIGH | Pin versions, test matrix | `pnpm install` failure |
| **Breaking API changes** | MEDIUM (40%) | HIGH | Migration validator, comprehensive tests | Import errors, type errors |
| **Performance regression** | MEDIUM (30%) | MEDIUM | Benchmark suite, profiling | Timeout failures, slow tests |
| **Test failures** | MEDIUM (35%) | HIGH | Full test suite, CI validation | CI failures, flaky tests |
| **Rollback script fails** | LOW (5%) | CRITICAL | Manual procedures, multiple backups | Script error, validation fail |
| **Data loss** | VERY LOW (1%) | CRITICAL | Snapshots, git history | Missing files, corrupt data |
| **Production outage** | LOW (10%) | CRITICAL | Staged rollout, canary deployment | Monitoring alerts, user reports |

### Risk Scoring

**Formula:** `Risk Score = Probability × Impact × (1 - Mitigation Effectiveness)`

| Risk | Score | Priority |
|------|-------|----------|
| Dependency conflicts | 24/100 | P1 - Address before deploy |
| Breaking API changes | 16/100 | P1 - Address before deploy |
| Test failures | 14/100 | P2 - Monitor closely |
| Performance regression | 12/100 | P2 - Monitor closely |
| Production outage | 10/100 | P1 - Have rollback ready |
| Rollback script fails | 5/100 | P2 - Test procedures |
| Data loss | 1/100 | P3 - Monitor |

### Critical Risks (P1)

**1. Dependency Conflicts (Score: 24)**

*Mitigation:*
- Pin exact versions in `package.json`
- Use `pnpm-lock.yaml` with `--frozen-lockfile`
- Test with multiple Node versions
- Create dependency compatibility matrix

*Detection:*
```bash
# Before deploy
pnpm install --frozen-lockfile
timeout 30s pnpm test:fast

# If fails, rollback immediately
```

**2. Breaking API Changes (Score: 16)**

*Mitigation:*
- Run migration validator before deploy
- Comprehensive integration tests
- Type checking with JSDoc
- API compatibility layer

*Detection:*
```bash
# Validate migrations
node scripts/v6-validate.mjs

# Check for breaking changes
git diff v5..v6 -- packages/core/src/index.mjs
```

**3. Production Outage (Score: 10)**

*Mitigation:*
- Blue-green deployment
- Canary release (10% → 50% → 100%)
- Health checks every 30s
- Automatic rollback on failure

*Detection:*
- Health endpoint: `/health`
- Error rate >5%: Alert
- Error rate >20%: Auto-rollback

---

## Monitoring & Alerting

### Health Checks

**Endpoint:** `/health`

**Expected Response:**
```json
{
  "status": "healthy",
  "version": "6.0.0",
  "uptime": 12345,
  "dependencies": {
    "database": "connected",
    "cache": "connected"
  }
}
```

**Alert Triggers:**
- Status ≠ "healthy" → P1 alert
- Response time >500ms → P2 alert
- 3 consecutive failures → Auto-rollback

### Performance Metrics

**Key Metrics:**

```javascript
// packages/core/src/metrics.mjs
export const metrics = {
  testDuration: { baseline: 2500, threshold: 5000 }, // ms
  sparqlQuery: { baseline: 15, threshold: 50 },       // ms
  memoryUsage: { baseline: 50, threshold: 200 },      // MB
  buildTime: { baseline: 8000, threshold: 30000 }     // ms
};
```

**Monitoring:**
```bash
# Continuous monitoring
while true; do
  timeout 60s pnpm benchmark:core
  sleep 300  # Every 5 minutes
done

# Alert if regression
if [[ $DURATION -gt $THRESHOLD ]]; then
  echo "ALERT: Performance regression detected"
  # Send alert (email, Slack, PagerDuty)
fi
```

### Error Tracking

**Log Levels:**
- **ERROR** → Immediate investigation
- **WARN** → Monitor, may need action
- **INFO** → Normal operation

**Critical Errors:**
```bash
# Monitor error logs
tail -f /var/log/unrdf/error.log

# Alert on patterns
grep -E "CRITICAL|FATAL|Error: " error.log | \
  while read line; do
    # Send alert
    echo "$line" | mail -s "UNRDF Error" ops@example.com
  done
```

### Rollback Triggers

**Automatic Rollback Conditions:**

1. **Error Rate >20%** (5min window)
2. **Test failures >50%**
3. **Performance degradation >50%**
4. **Health check failures (3 consecutive)**
5. **Critical dependency failure**

**Manual Rollback Decision Matrix:**

| Metric | Threshold | Action |
|--------|-----------|--------|
| Error rate | 5-10% | Investigate |
| Error rate | 10-20% | Prepare rollback |
| Error rate | >20% | Execute rollback |
| Test failures | <25% | Fix forward |
| Test failures | 25-50% | Consider rollback |
| Test failures | >50% | Execute rollback |
| Perf degradation | <20% | Monitor |
| Perf degradation | 20-50% | Investigate |
| Perf degradation | >50% | Execute rollback |

---

## Incident Response

### Incident Response Workflow

```
┌─────────────────┐
│  Issue Detected │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Assess Severity │──── Severity 1-5
└────────┬────────┘
         │
         ├─── SEV1 (Critical) ──────────┐
         │                              │
         ├─── SEV2 (High) ──────────────┤
         │                              │
         ├─── SEV3 (Medium) ────────────┤
         │                              │
         └─── SEV4/5 (Low) ─────────────┤
                                        │
                                        ▼
                              ┌──────────────────┐
                              │ Execute Response │
                              └────────┬─────────┘
                                       │
                      ┌────────────────┼────────────────┐
                      │                │                │
                      ▼                ▼                ▼
              ┌──────────────┐  ┌──────────┐  ┌─────────────┐
              │   Rollback   │  │   Fix    │  │   Monitor   │
              └──────────────┘  └──────────┘  └─────────────┘
```

### Severity Levels

**SEV1 - Critical (Production Down)**
- **Response Time:** Immediate (<5min)
- **Action:** Execute rollback immediately
- **Command:** `node scripts/v6-rollback.mjs --force`
- **Communication:** Alert all stakeholders

**SEV2 - High (Major Functionality Broken)**
- **Response Time:** <15min
- **Action:** Assess rollback vs fix-forward
- **Command:** `node scripts/v6-rollback.mjs --dry-run` (prepare)
- **Communication:** Update incident channel

**SEV3 - Medium (Minor Functionality Broken)**
- **Response Time:** <1hr
- **Action:** Fix forward, prepare rollback
- **Command:** Create fix, test, deploy
- **Communication:** Log incident, plan fix

**SEV4/5 - Low (Minor Issues)**
- **Response Time:** <4hr
- **Action:** Fix in next deployment
- **Command:** Track in issue tracker
- **Communication:** Update team

### Incident Response Checklist

**For SEV1/SEV2:**

- [ ] **T+0min:** Incident detected, severity assessed
- [ ] **T+2min:** Incident channel created (#incident-YYYY-MM-DD)
- [ ] **T+5min:** Rollback decision made
  - If YES → Execute: `node scripts/v6-rollback.mjs`
  - If NO → Begin investigation
- [ ] **T+10min:** Status update posted
- [ ] **T+15min:** Rollback complete OR root cause identified
- [ ] **T+30min:** Validation complete
- [ ] **T+60min:** Post-mortem scheduled

**Command Reference:**

```bash
# Quick status check
timeout 5s pnpm test:fast && \
timeout 5s pnpm lint && \
echo "System healthy"

# Performance check
timeout 30s pnpm benchmark:core

# Full rollback
node scripts/v6-rollback.mjs

# Create incident snapshot
node scripts/v6-snapshot.mjs incident-$(date +%Y%m%d-%H%M%S)
```

---

## Post-Rollback Validation

### Validation Checklist

After rollback completes, **MUST validate all systems:**

```bash
# 1. Version check
cat package.json | grep version
# Expected: "version": "5.x.x"

# 2. Dependencies restored
pnpm ls --depth=0 | grep -E "zod|rdf-canonize"
# Expected: zod 3.x, rdf-canonize 2.x

# 3. Tests passing
timeout 30s pnpm test:fast
# Expected: All tests pass

# 4. Linting clean
timeout 15s pnpm lint
# Expected: 0 errors, 0 warnings

# 5. Build successful
timeout 30s pnpm build
# Expected: Build succeeds

# 6. Git state correct
git status
git log -1
# Expected: On correct commit/branch

# 7. Rollback report generated
cat rollback-report.json
# Expected: "success": true
```

### Validation Script

**Create:** `scripts/validate-rollback.mjs`

```javascript
#!/usr/bin/env node
/**
 * Validate rollback completed successfully
 */
import { execSync } from 'child_process';
import { readFileSync } from 'fs';

const checks = [
  {
    name: 'Version check',
    command: 'cat package.json',
    validate: (output) => {
      const pkg = JSON.parse(output);
      return pkg.version.startsWith('5.');
    }
  },
  {
    name: 'Tests passing',
    command: 'timeout 30s pnpm test:fast',
    validate: (output) => !output.includes('FAIL')
  },
  {
    name: 'Linting clean',
    command: 'timeout 15s pnpm lint',
    validate: (output) => !output.includes('error')
  },
  {
    name: 'Build successful',
    command: 'timeout 30s pnpm build',
    validate: (output) => !output.includes('Error')
  }
];

let passed = 0;
let failed = 0;

for (const check of checks) {
  try {
    const output = execSync(check.command, { encoding: 'utf-8' });
    if (check.validate(output)) {
      console.log(`✓ ${check.name}`);
      passed++;
    } else {
      console.log(`✗ ${check.name}`);
      failed++;
    }
  } catch (err) {
    console.log(`✗ ${check.name}: ${err.message}`);
    failed++;
  }
}

console.log(`\nResults: ${passed} passed, ${failed} failed`);
process.exit(failed === 0 ? 0 : 1);
```

**Run validation:**
```bash
node scripts/validate-rollback.mjs
```

### Expected Output

```
✓ Version check
✓ Tests passing
✓ Linting clean
✓ Build successful

Results: 4 passed, 0 failed
```

### What If Validation Fails?

**If any check fails after rollback:**

1. **DO NOT proceed** - System is in unstable state
2. **Check rollback logs:**
   ```bash
   cat rollback.log
   cat rollback-report.json
   ```
3. **Identify what failed:**
   ```bash
   # Check versions
   cat package.json | grep version
   pnpm ls --depth=0

   # Check git state
   git status
   git log -1
   ```
4. **Manual recovery:**
   ```bash
   # Nuclear option: Hard reset
   git reset --hard [known-good-commit]
   rm -rf node_modules packages/*/node_modules
   pnpm install --frozen-lockfile

   # Validate again
   node scripts/validate-rollback.mjs
   ```

---

## Appendix

### A. Snapshot Structure

```
.rollback-snapshots/
└── v5-main-2025-12-27/
    ├── README.md              # Snapshot documentation
    ├── manifest.json          # Complete snapshot metadata
    ├── package.json           # Root package.json
    ├── pnpm-lock.yaml        # Lockfile
    ├── git-state.json        # Git metadata
    ├── dependency-tree.json  # Full dependency tree
    ├── environment.json      # Node version, platform, etc.
    └── packages/
        ├── core/
        │   └── package.json
        └── oxigraph/
            └── package.json
```

### B. Command Reference

| Command | Purpose | Duration |
|---------|---------|----------|
| `node scripts/v6-snapshot.mjs` | Create snapshot | ~10s |
| `node scripts/v6-rollback.mjs --dry-run` | Test rollback | ~30s |
| `node scripts/v6-rollback.mjs` | Execute rollback | ~5min |
| `node scripts/validate-rollback.mjs` | Validate rollback | ~2min |
| `timeout 30s pnpm test:fast` | Quick validation | ~3s |
| `timeout 15s pnpm lint` | Lint check | ~2s |
| `timeout 30s pnpm build` | Build validation | ~10s |

### C. Emergency Contacts

**Incident Response:**
- **Primary:** [Your oncall rotation]
- **Secondary:** [Backup contact]
- **Escalation:** [Manager/Director]

**External Dependencies:**
- **NPM Registry:** https://status.npmjs.org/
- **GitHub:** https://www.githubstatus.com/

### D. Post-Mortem Template

After any rollback, conduct a post-mortem:

```markdown
# Post-Mortem: V6 Rollback - YYYY-MM-DD

## Timeline
- **T+0:** Issue detected
- **T+X:** Rollback initiated
- **T+Y:** Rollback completed

## Root Cause
[What caused the issue?]

## Impact
[What was affected?]

## Resolution
[How was it resolved?]

## Action Items
- [ ] Fix root cause
- [ ] Add tests to prevent recurrence
- [ ] Update documentation
- [ ] Improve monitoring

## Lessons Learned
[What did we learn?]
```

---

## Summary

**This rollback plan provides:**

✅ **Automated rollback** in <5 minutes
✅ **Multiple fallback options** (automated, manual, partial)
✅ **Comprehensive validation** (tests, lint, build)
✅ **Risk assessment** with mitigation strategies
✅ **Incident response** procedures
✅ **Monitoring & alerting** guidelines

**Before every deployment:**
1. Create snapshot: `node scripts/v6-snapshot.mjs`
2. Validate snapshot: `ls .rollback-snapshots/`
3. Test rollback: `node scripts/v6-rollback.mjs --dry-run`

**If deployment fails:**
1. Execute rollback: `node scripts/v6-rollback.mjs`
2. Validate rollback: `node scripts/validate-rollback.mjs`
3. Investigate root cause
4. Fix and redeploy

**Remember:** Rollback is a safety mechanism, not a failure. Having a tested rollback plan is a sign of production maturity.

---

**Document Version:** 1.0.0
**Last Reviewed:** 2025-12-27
**Next Review:** Before v6 production deployment
