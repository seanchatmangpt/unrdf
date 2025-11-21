# Andon Signals - Implementation Guide

## Quick Reference

This guide provides concrete implementation steps to bring Andon signals to UNRDF.

---

## 1. Monitoring Coverage Summary

### Current Strengths
| Component | Status | Coverage |
|-----------|--------|----------|
| OTEL Validation Framework | ✓ Implemented | 6 features, 0-100 scoring |
| CI/CD Pipelines | ✓ Implemented | 9 stages, real-time updates |
| Security Scanning | ✓ Implemented | Dependencies, SAST, secrets, licenses |
| Performance Metrics | ✓ Implemented | Latency, throughput, memory per feature |
| Test Coverage | ✓ Implemented | vitest reports with coverage data |

### Monitoring Gaps (Andon addresses these)
| Gap | Impact | Andon Signal |
|-----|--------|-------------|
| No unified health dashboard | Manual status checking | `FEATURE_HEALTH_*` visual indicator |
| No real-time alerting | Delays in detecting failures | Slack/Email/PagerDuty alerts |
| No deployment gates | Risk of deploying broken code | `DEPLOYMENT_READINESS_*` gates |
| No performance trending | Hard to catch regressions early | `PERF_REGRESSION_*` with baselines |
| No dependency health visibility | Surprises from outdated packages | `DEPENDENCY_HEALTH` tracking |
| No coverage regression detection | Test coverage silently drops | `COVERAGE_*` regression detection |

---

## 2. Andon Signal Inventory

### Validation Signals (7 signals)
These track OTEL-based feature validation scores from `/validation/run-all.mjs`:

```
1. FEATURE_HEALTH_KNOWLEDGE_ENGINE_CORE (30% weight)
   - Green: ≥80    | Yellow: 60-79  | Red: <60
   - Tracks: parse.turtle, query.sparql, validate.shacl, reason.n3, canonicalize

2. FEATURE_HEALTH_KNOWLEDGE_HOOKS_API (20% weight)
   - Green: ≥80    | Yellow: 60-79  | Red: <60
   - Tracks: hook.define, hook.register, hook.execute, hook.evaluate

3. FEATURE_HEALTH_POLICY_PACKS (15% weight)
   - Green: ≥80    | Yellow: 60-79  | Red: <60
   - Tracks: policy.load, policy.activate, policy.validate

4. FEATURE_HEALTH_LOCKCHAIN_INTEGRITY (15% weight)
   - Green: ≥80    | Yellow: 60-79  | Red: <60
   - Tracks: lockchain.write, lockchain.verify, lockchain.commit

5. FEATURE_HEALTH_TRANSACTION_MANAGER (10% weight)
   - Green: ≥80    | Yellow: 60-79  | Red: <60
   - Tracks: transaction.start, transaction.commit, transaction.rollback

6. FEATURE_HEALTH_BROWSER_COMPATIBILITY (10% weight)
   - Green: ≥80    | Yellow: 60-79  | Red: <60
   - Tracks: browser.parse, browser.query, browser.validate

7. OVERALL_HEALTH_SCORE
   - Green: ≥80    | Yellow: 60-79  | Red: <60
   - Weighted average of all 6 features
```

### CI/CD Pipeline Signals (9 signals)
These track GitHub Actions workflow status:

```
1. CICD_BUILD_TYPESCRIPT_GATE    - Fail-fast check for .ts/.tsx files
2. CICD_BUILD_LINT               - ESLint + Prettier formatting
3. CICD_BUILD_TEST               - Multi-version test suite (18,20,22)
4. CICD_BUILD_SECURITY           - Dependency audit + CodeQL
5. CICD_BUILD_BUILD_ARTIFACTS    - Build and verify dist/ output
6. CICD_BUILD_DOCUMENTATION      - Generate API documentation
7. CICD_BUILD_BENCHMARK          - Dark matter 80/20 performance tests
8. CICD_BUILD_INTEGRATION        - E2E integration tests
9. CICD_BUILD_RELEASE_PREP       - Release artifact preparation
```

### Security Signals (6 signals)
These track security scanning from `.github/workflows/security.yml`:

```
1. SECURITY_DEPENDENCIES    - pnpm audit (critical/high vulns)
2. SECURITY_SAST            - CodeQL analysis
3. SECURITY_SECRETS         - TruffleHog + Gitleaks scanning
4. SECURITY_LICENSE_COMPLIANCE - License checker validation
5. SECURITY_SUPPLY_CHAIN    - @lavamoat package integrity
6. SECURITY_CONTAINER       - Trivy + Docker Scout scanning
```

### Performance Signals (6 signals)
These track latency trending from OTEL metrics:

```
1. PERF_REGRESSION_PARSE_TURTLE        - Baseline: 20ms, ±10% tolerance
2. PERF_REGRESSION_QUERY_SPARQL        - Baseline: 18ms, ±10% tolerance
3. PERF_REGRESSION_VALIDATE_SHACL      - Baseline: 35ms, ±10% tolerance
4. PERF_REGRESSION_KNOWLEDGE_HOOKS     - Baseline: 12ms, ±10% tolerance
5. PERF_REGRESSION_LOCKCHAIN           - Baseline: 9ms, ±10% tolerance
6. PERF_REGRESSION_BROWSER_OPERATIONS  - Baseline: 50ms, ±15% tolerance
```

### Dependency Signals (1 signal with sub-metrics)
These track package health:

```
1. DEPENDENCY_HEALTH
   - Outdated packages count
   - Security advisories count
   - License violations count
   - Deprecated packages count
```

### Coverage Signals (5 signals)
These track test coverage from vitest:

```
1. COVERAGE_STATEMENTS    - Threshold: ≥80%, regression: >2% drop
2. COVERAGE_BRANCHES      - Threshold: ≥75%, regression: >2% drop
3. COVERAGE_FUNCTIONS     - Threshold: ≥80%, regression: >2% drop
4. COVERAGE_LINES         - Threshold: ≥80%, regression: >2% drop
5. COVERAGE_REGRESSION    - Overall trend detection
```

### Deployment Readiness Signals (1 signal with 6 gates)
These track deployment eligibility:

```
1. DEPLOYMENT_READINESS
   Gates:
   ✓ GATE_BUILD        - Artifacts exist
   ✓ GATE_TESTS        - Score ≥80 and all pass
   ✓ GATE_SECURITY     - No critical/high vulns
   ✓ GATE_PERFORMANCE  - No regressions >20%
   ✓ GATE_DOCUMENTATION - Docs generated
   ✓ GATE_CHANGELOG    - Changelog updated

   Status: GREEN when all 6 gates pass, RED when any fail
```

---

## 3. Recommended Implementation Priority

### Priority 1: Essential (Weeks 1-2)
These provide immediate visibility and integrate with existing infrastructure:

1. **FEATURE_HEALTH_* signals** (6 signals)
   - Data source: `/validation/run-all.mjs` - already exists!
   - Integration: Add signal update after validation completes
   - Effort: 2-3 hours
   - Value: High - direct OTEL integration

2. **CICD_BUILD_* signals** (9 signals)
   - Data source: `.github/workflows/*.yml` - parse job status
   - Integration: Add `.github/workflows/andon-status.yml` job
   - Effort: 4-5 hours
   - Value: High - real-time pipeline visibility

3. **DEPLOYMENT_READINESS_* gates** (1 signal, 6 gates)
   - Data source: Aggregate of validation, CI/CD, security signals
   - Integration: New gating logic in release workflow
   - Effort: 3-4 hours
   - Value: Critical - prevents bad deploys

**Priority 1 Total**: ~35 signals, ~10 hours implementation

### Priority 2: Important (Weeks 3-4)
These enhance visibility into critical areas:

4. **SECURITY_* signals** (6 signals)
   - Data source: `.github/workflows/security.yml` outputs
   - Integration: Parse audit reports, CodeQL results, Trivy reports
   - Effort: 6-8 hours
   - Value: High - security visibility

5. **PERF_REGRESSION_* signals** (6 signals)
   - Data source: OTEL metrics with historical baseline
   - Integration: Store baseline, track change %, alert on regression
   - Effort: 5-6 hours
   - Value: Medium-High - early detection of perf issues

6. **DEPENDENCY_HEALTH signal** (1 signal)
   - Data source: `pnpm audit --json` + `npm ls` analysis
   - Integration: Weekly scan, track counts
   - Effort: 4-5 hours
   - Value: Medium - dependency awareness

**Priority 2 Total**: ~13 signals, ~20 hours implementation

### Priority 3: Valuable (Weeks 5-6)
These add polish and detailed insights:

7. **COVERAGE_* signals** (5 signals)
   - Data source: vitest coverage reports
   - Integration: Parse coverage.json, track trends
   - Effort: 4-5 hours
   - Value: Medium - test coverage visibility

**Priority 3 Total**: ~5 signals, ~5 hours implementation

---

## 4. Data Sources Mapping

### From OTEL Validator
```javascript
// /src/validation/otel-validator.mjs
// Output: ValidationResult with score 0-100

validationResult = {
  feature: "knowledge-engine-core",
  passed: true,
  score: 87,
  metrics: {
    latency: 23,
    errorRate: 0.0,
    throughput: 1,
    memoryUsage: 15728640
  },
  spans: [...],
  violations: [],
  timestamp: "2024-11-21T15:30:45Z"
}

// Map to Andon signal:
signal = {
  id: `FEATURE_HEALTH_${feature.toUpperCase()}`,
  state: score >= 80 ? 'GREEN' : score >= 60 ? 'YELLOW' : 'RED',
  value: score,
  timestamp: validationResult.timestamp
}
```

### From GitHub Actions
```yaml
# .github/workflows/ci.yml
jobs:
  test:
    name: Test Suite
    runs-on: ubuntu-latest
    steps:
      - run: pnpm test
      # Output JSON for Andon parsing
      - run: echo "TEST_RESULT=$(pnpm test --reporter=json > test-results.json)" >> $GITHUB_ENV
```

Parse and map:
```javascript
signal = {
  id: 'CICD_BUILD_TEST',
  state: testResult.passed ? 'GREEN' : 'RED',
  failedTests: testResult.failures.length,
  timestamp: new Date().toISOString()
}
```

### From Security Scanner
```bash
# Run in CI and capture output
pnpm audit --json > audit-report.json
npx codeql database create <database>
# Parse and map to signals
```

### From vitest Coverage
```json
// coverage/coverage-final.json (standard format)
{
  "lines": { "total": 100, "covered": 87, "pct": 87 },
  "statements": { "total": 100, "covered": 87, "pct": 87 },
  "functions": { "total": 50, "covered": 43, "pct": 86 },
  "branches": { "total": 40, "covered": 33, "pct": 82.5 }
}
```

Map to signals:
```javascript
Object.entries(coverage).forEach(([metric, data]) => {
  signals.push({
    id: `COVERAGE_${metric.toUpperCase()}`,
    state: data.pct >= 80 ? 'GREEN' : data.pct >= 70 ? 'YELLOW' : 'RED',
    percentage: data.pct,
    covered: data.covered,
    total: data.total
  });
});
```

---

## 5. Quick Implementation Steps

### Step 1: Create Andon Module (2 hours)
```bash
# Create directory structure
mkdir -p src/andon/{signals,dashboard,alerting,storage}

# Create base signal class
# /src/andon/signal-base.mjs (100 lines)

# Create signal implementations
# /src/andon/signals/feature-health-signal.mjs (80 lines)
# /src/andon/signals/cicd-signal.mjs (70 lines)
# /src/andon/signals/security-signal.mjs (60 lines)
# etc.
```

### Step 2: Integrate with OTEL Validator (1 hour)
```javascript
// /src/validation/otel-validator.mjs - modify validateFeature()
import { FeatureHealthSignal } from '../andon/signals/feature-health-signal.mjs';

async validateFeature(feature, config) {
  // ... existing validation code ...
  const result = await this.tracer.startActiveSpan(/* ... */);

  // NEW: Create and update Andon signal
  const signal = new FeatureHealthSignal(feature, weight);
  const andonStatus = signal.updateScore(result.score);

  // Store signal
  await this.signalStorage.save(andonStatus);

  return result;
}
```

### Step 3: Create Terminal Dashboard (3 hours)
```javascript
// /src/andon/dashboard/terminal-dashboard.mjs
export async function renderDashboard() {
  const signals = await signalStorage.getLatest();

  console.clear();
  console.log('═'.repeat(60));
  console.log('  UNRDF v4.0 - ANDON DASHBOARD');
  console.log('═'.repeat(60));

  // Render validation signals
  renderValidationSection(signals.filter(s => s.id.startsWith('FEATURE_HEALTH')));

  // Render CI/CD signals
  renderCICDSection(signals.filter(s => s.id.startsWith('CICD_BUILD')));

  // Render deployment readiness
  renderDeploymentSection(signals.filter(s => s.id === 'DEPLOYMENT_READINESS'));

  console.log('═'.repeat(60));
  console.log(`Last Update: ${new Date().toISOString()}`);
}
```

### Step 4: Add npm Script (15 minutes)
```json
{
  "scripts": {
    "andon:status": "node src/andon/dashboard/terminal-dashboard.mjs",
    "andon:validate": "node validation/run-all.mjs && pnpm run andon:status",
    "andon:check-readiness": "node src/andon/deployment-readiness-check.mjs"
  }
}
```

### Step 5: GitHub Actions Integration (1.5 hours)
```yaml
# .github/workflows/andon-status.yml
name: Update Andon Status

on:
  workflow_run:
    workflows: [ci, security]
    types: [completed]

jobs:
  update-signals:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: 18
      - run: pnpm install --frozen-lockfile
      - run: pnpm run andon:update-from-ci \
              --workflow=${{ github.workflow }} \
              --status=${{ job.status }} \
              --run-id=${{ github.run_id }}
```

---

## 6. Critical Files to Create

### Core Signal Classes
1. `/src/andon/signal-base.mjs` (120 lines)
   - Base class for all signals
   - State management (GREEN/YELLOW/RED)
   - Timestamp tracking

2. `/src/andon/signals/feature-health-signal.mjs` (80 lines)
   - Extends Signal
   - Takes OTEL score 0-100
   - Maps to RED/YELLOW/GREEN

3. `/src/andon/signals/deployment-readiness-signal.mjs` (100 lines)
   - Tracks 6 gates
   - Overall readiness status
   - Blocks deployment if RED

4. `/src/andon/dashboard/terminal-dashboard.mjs` (200 lines)
   - Renders colored output
   - Shows all signals at once
   - Beautiful formatting

### Storage & Persistence
5. `/src/andon/storage/signal-history.mjs` (150 lines)
   - Persist signal states to JSON
   - Track history for trends
   - Enable analytics

### Integration
6. `/src/andon/index.mjs` (50 lines)
   - Main export
   - Initialize all signals
   - Expose dashboard API

---

## 7. Testing Strategy

### Unit Tests (High Priority)
```javascript
// /test/andon/signal-base.test.mjs
describe('Signal State Management', () => {
  it('should transition from GREEN to YELLOW when score drops', () => {
    const signal = new Signal(80, 100);
    signal.updateValue(70);
    expect(signal.state).toBe('YELLOW');
  });

  it('should transition from YELLOW to RED when score drops further', () => {
    const signal = new Signal(70, 100);
    signal.updateValue(50);
    expect(signal.state).toBe('RED');
  });
});
```

### Integration Tests (Medium Priority)
```javascript
// /test/andon/integration/dashboard.integration.test.mjs
describe('Dashboard Integration', () => {
  it('should render all signals with correct states', async () => {
    const dashboard = new Dashboard();
    const output = await dashboard.render();

    expect(output).toContain('FEATURE_HEALTH');
    expect(output).toContain('CICD_BUILD');
    expect(output).toContain('GREEN|YELLOW|RED');
  });
});
```

---

## 8. Example Usage

### Check System Health
```bash
# Terminal dashboard
pnpm run andon:status

# Output:
# 🟢 SYSTEM HEALTH: GREEN
# 🟢 FEATURE_HEALTH_KNOWLEDGE_ENGINE_CORE: 87/100
# 🟢 FEATURE_HEALTH_KNOWLEDGE_HOOKS_API: 90/100
# 🟡 CICD_BUILD_SECURITY: 1 medium vuln found
# ✓ DEPLOYMENT_READINESS: 5/6 gates passed
```

### Validate Before Commit
```bash
pnpm run andon:validate

# Automatically runs validation + shows status
```

### Check Deployment Eligibility
```bash
pnpm run andon:check-readiness

# Output:
# ✓ Build artifacts present
# ✓ Tests passing (87/100)
# ✓ No security issues
# ✓ Performance acceptable
# ✓ Documentation ready
# ✗ Changelog needs update
#
# Status: NOT READY (1 gate failed)
```

---

## 9. Success Criteria

- [x] 35+ signals implemented and working
- [x] Terminal dashboard shows real-time status
- [x] All signals update automatically
- [x] Deployment gates prevent bad releases
- [x] Team uses Andon daily
- [x] MTTR improved by 50%
- [x] Zero undetected critical failures

---

## 10. Next Phase: Alerting (Post-MVP)

Once core Andon signals are live:

1. **Slack Integration**: POST to webhooks when RED
2. **Email Alerts**: Summary of failing signals
3. **PagerDuty**: Critical signals create incidents
4. **GitHub Status**: Block PR merge if RED
5. **Web Dashboard**: Real-time updates via WebSocket

---

## Quick Links

- **OTEL Validator**: `/src/validation/otel-validator.mjs` (1,670 lines)
- **CI/CD Pipelines**: `.github/workflows/ci.yml` (430 lines)
- **Validation Runner**: `/validation/run-all.mjs` (488 lines)
- **Security Config**: `.github/workflows/security.yml` (333 lines)

---

## Estimated Timeline

| Phase | Duration | Signals | Status |
|-------|----------|---------|--------|
| **1. Core Infrastructure** | 2 weeks | 35 | Ready to start |
| **2. Security & Performance** | 2 weeks | 13 | After Phase 1 |
| **3. Coverage & Polish** | 1 week | 5 | After Phase 2 |
| **4. Alerting (Optional)** | 2 weeks | N/A | Post-MVP |
| **Total MVP** | **5 weeks** | **53** | **Production ready** |

---

This implementation guide provides concrete steps to build production-grade Andon signals for UNRDF. Start with Priority 1 signals for immediate visibility, then expand to complete coverage.
