# Andon Signals - Complete Matrix Reference

## Full Signal Inventory (53 Signals)

### SECTION 1: VALIDATION SIGNALS (7 signals)
Source: `/validation/run-all.mjs` OTEL validator
Update Frequency: Per test run
Weight: Determines overall health score

| Signal ID | Feature | Weight | Green | Yellow | Red | Description |
|-----------|---------|--------|-------|--------|-----|-------------|
| FEATURE_HEALTH_KNOWLEDGE_ENGINE_CORE | Knowledge Engine | 30% | ≥80 | 60-79 | <60 | Core RDF operations (parse, query, validate, reason) |
| FEATURE_HEALTH_KNOWLEDGE_HOOKS_API | Knowledge Hooks | 20% | ≥80 | 60-79 | <60 | Hook lifecycle (define, register, execute, evaluate) |
| FEATURE_HEALTH_POLICY_PACKS | Policy Packs | 15% | ≥80 | 60-79 | <60 | Policy pack operations (load, activate, validate) |
| FEATURE_HEALTH_LOCKCHAIN_INTEGRITY | Lockchain | 15% | ≥80 | 60-79 | <60 | Cryptographic audit trail (write, verify, commit) |
| FEATURE_HEALTH_TRANSACTION_MANAGER | Transactions | 10% | ≥80 | 60-79 | <60 | ACID transaction guarantees |
| FEATURE_HEALTH_BROWSER_COMPATIBILITY | Browser | 10% | ≥80 | 60-79 | <60 | Browser shims & polyfills (parse, query, validate) |
| OVERALL_HEALTH_SCORE | System Overall | 100% | ≥80 | 60-79 | <60 | Weighted average of all 6 features |

**Metrics Tracked Per Signal**:
- Validation score (0-100)
- Expected spans present
- Latency (ms)
- Error rate (%)
- Throughput (ops/run)
- Memory usage (MB)
- Violations count

---

### SECTION 2: CI/CD PIPELINE SIGNALS (9 signals)
Source: `.github/workflows/*.yml` GitHub Actions
Update Frequency: Real-time (job completion)
Criticality: High (blocks release)

| Signal ID | Job Name | Purpose | Green | Yellow | Red | Time Limit |
|-----------|----------|---------|-------|--------|-----|-----------|
| CICD_BUILD_TYPESCRIPT_GATE | TypeScript Gate | Fail-fast .ts/.tsx check | No TS files | — | TS files found | 1 min |
| CICD_BUILD_LINT | Lint & Format | ESLint + Prettier | All pass | — | Failures | 2 min |
| CICD_BUILD_TEST | Test Suite | Multi-version (18,20,22) | All pass | — | Failures | 5 min |
| CICD_BUILD_SECURITY | Security Audit | pnpm audit + CodeQL | 0 high/crit | Medium found | High/Crit found | 3 min |
| CICD_BUILD_BUILD_ARTIFACTS | Build & Package | Verify dist/ artifacts | All present | — | Missing files | 2 min |
| CICD_BUILD_DOCUMENTATION | Generate Docs | JSDoc API docs | Docs exist | — | Generation failed | 2 min |
| CICD_BUILD_BENCHMARK | Performance Benchmark | Dark matter 80/20 tests | All pass | — | Failures | 3 min |
| CICD_BUILD_INTEGRATION | Integration Tests | E2E integration tests | All pass | — | Failures | 5 min |
| CICD_BUILD_RELEASE_PREP | Release Preparation | Artifact & changelog prep | Ready | — | Failed | 2 min |

**State Mapping**:
- GREEN = Job passed ✓
- YELLOW = Job skipped or has warnings ⚠
- RED = Job failed ✗

---

### SECTION 3: SECURITY SIGNALS (6 signals)
Source: `.github/workflows/security.yml` scanners
Update Frequency: Daily (scheduled) + on-demand
Criticality: Critical (blocks deployment)

| Signal ID | Scanner | Checks | Green | Yellow | Red | Tool |
|-----------|---------|--------|-------|--------|-----|------|
| SECURITY_DEPENDENCIES | Dependency Audit | Vulnerability count | 0 high/crit | >0 medium | 1+ high/crit | pnpm audit |
| SECURITY_SAST | SAST Analysis | Code quality issues | 0 issues | Warnings | 1+ errors | CodeQL |
| SECURITY_SECRETS | Secret Detection | Hardcoded secrets | 0 found | — | 1+ found | TruffleHog + Gitleaks |
| SECURITY_LICENSE_COMPLIANCE | License Check | Approved licenses | All approved | Unknown | 1+ forbidden | license-checker |
| SECURITY_SUPPLY_CHAIN | Supply Chain | Package integrity | Verified ✓ | — | Failed ✗ | @lavamoat |
| SECURITY_CONTAINER | Container Scan | Image CVEs | 0 found | Low CVEs | High/Crit CVEs | Trivy + Docker Scout |

**Vulnerability Thresholds**:
- RED = Critical or High severity vulnerabilities
- YELLOW = Medium severity vulnerabilities
- GREEN = None or Low only

---

### SECTION 4: PERFORMANCE SIGNALS (6 signals)
Source: OTEL latency metrics with baseline tracking
Update Frequency: Per validation run
Criticality: Medium (warning, no block)

| Signal ID | Operation | Baseline | ±Tolerance | Green | Yellow | Red | Unit |
|-----------|-----------|----------|------------|-------|--------|-----|------|
| PERF_REGRESSION_PARSE_TURTLE | Parse RDF | 20ms | ±10% | <22ms | 22-24ms | >24ms | ms |
| PERF_REGRESSION_QUERY_SPARQL | SPARQL Query | 18ms | ±10% | <20ms | 20-22ms | >22ms | ms |
| PERF_REGRESSION_VALIDATE_SHACL | SHACL Validate | 35ms | ±10% | <39ms | 39-42ms | >42ms | ms |
| PERF_REGRESSION_KNOWLEDGE_HOOKS | Hook Execute | 12ms | ±10% | <13ms | 13-15ms | >15ms | ms |
| PERF_REGRESSION_LOCKCHAIN | Lockchain Write | 9ms | ±10% | <10ms | 10-11ms | >11ms | ms |
| PERF_REGRESSION_BROWSER_OPERATIONS | Browser Ops | 50ms | ±15% | <58ms | 58-65ms | >65ms | ms |

**Regression Calculation**:
```
percentChange = ((current - baseline) / baseline) * 100

GREEN  = percentChange ≤ +10%   (no significant regression)
YELLOW = 10% < percentChange ≤ +20%  (moderate regression - investigate)
RED    = percentChange > +20%   (major regression - fix required)
```

---

### SECTION 5: DEPENDENCY HEALTH SIGNAL (1 signal)
Source: `pnpm audit --json`, npm deprecation API, license-checker
Update Frequency: Weekly automated check
Criticality: Medium

| Signal ID | Metric | Count | Green | Yellow | Red |
|-----------|--------|-------|-------|--------|-----|
| DEPENDENCY_HEALTH | Outdated packages | — | 0-2 | 3-5 | >5 |
| | Security advisories | — | 0 | — | ≥1 |
| | License violations | — | 0 | 1 | >1 |
| | Deprecated packages | — | 0 | 1 | >1 |

**Overall State Logic**:
```
RED    = Any security advisory OR any license violation
YELLOW = 3+ outdated packages OR any deprecated package
GREEN  = <3 outdated, 0 advisories, 0 violations, 0 deprecated
```

**Sub-metrics**:
- Outdated packages: Count from `pnpm outdated`
- Security advisories: From `pnpm audit --json` metadata
- License violations: Non-whitelisted licenses
- Deprecated packages: npm deprecation checks

---

### SECTION 6: TEST COVERAGE SIGNALS (5 signals)
Source: `vitest` coverage reports (coverage-final.json)
Update Frequency: Per test run
Criticality: Low (warning only)

| Signal ID | Metric | Threshold | Green | Yellow | Red | Regression Alert |
|-----------|--------|-----------|-------|--------|-----|------------------|
| COVERAGE_STATEMENTS | Statement % | 80% | ≥80% | 70-79% | <70% | >2% drop |
| COVERAGE_BRANCHES | Branch % | 75% | ≥75% | 65-74% | <65% | >2% drop |
| COVERAGE_FUNCTIONS | Function % | 80% | ≥80% | 70-79% | <70% | >2% drop |
| COVERAGE_LINES | Line % | 80% | ≥80% | 70-79% | <70% | >2% drop |
| COVERAGE_REGRESSION | Trend | — | Stable | <2% drop | >2% drop | Auto-detect |

**Thresholds**:
```
GREEN  = ≥ threshold AND no regression
YELLOW = Below threshold OR regression 0.5-2%
RED    = Well below threshold (<70%) OR regression >2%
```

---

### SECTION 7: DEPLOYMENT READINESS SIGNAL (1 signal with 6 gates)
Source: Aggregate of validation, CI/CD, security, performance signals
Update Frequency: On release attempt
Criticality: Critical (prevents deployment)

| Gate | Signal Group | Checks | Pass Condition | Fail Condition |
|------|--------------|--------|----------------|----------------|
| GATE_BUILD | CI/CD Pipeline | Artifact existence | All artifacts present | Missing files |
| GATE_TESTS | Validation | Score threshold | OVERALL_HEALTH ≥80 | Score <80 |
| GATE_SECURITY | Security | Vulnerability count | 0 critical/high vulns | 1+ critical/high |
| GATE_PERFORMANCE | Performance | Regression limit | All <20% regression | Any >20% regression |
| GATE_DOCUMENTATION | CI/CD Pipeline | Doc generation | Docs successfully generated | Doc generation failed |
| GATE_CHANGELOG | Manual/Git | Version update | Changelog has version entry | Missing changelog entry |

**Overall Deployment State**:
```
Passed gates: 6/6   → 🟢 GREEN  (READY TO DEPLOY)
Passed gates: 5/6   → 🟡 YELLOW (BLOCKED - fix failing gate)
Passed gates: <5/6  → 🔴 RED    (CRITICAL - multiple gates failed)

Example:
✓ Build ✓ Tests ✓ Security ✓ Performance ✓ Docs ✗ Changelog
→ 5/6 gates = 🟡 YELLOW (Update changelog to proceed)
```

---

## Cross-Reference: Signal to Data Source

| Signal Group | Data Source | Command | Output Format | Parse Time |
|--------------|-------------|---------|----------------|-----------|
| Validation | `/validation/run-all.mjs` | `node validation/run-all.mjs` | JSON + Console | ~60s |
| CI/CD | GitHub Actions API | `gh run list` + `gh run view` | JSON | <5s |
| Security | `.github/workflows/security.yml` | Multiple tools | JSON reports | ~120s |
| Performance | OTEL metrics | Via validation spans | Extracted from spans | Included in validation |
| Dependencies | Package manager | `pnpm audit --json` | JSON | ~10s |
| Coverage | vitest reports | `pnpm test --coverage` | JSON coverage file | ~45s |
| Deployment | All above signals | Aggregation | Calculated | <1s |

---

## Alert Severity Mapping

### Signal to Alert Type

| Signal | Green | Yellow | Red |
|--------|-------|--------|-----|
| FEATURE_HEALTH_* | Dashboard | Slack + Email | Email + Slack + PagerDuty |
| CICD_BUILD_* | Dashboard | Dashboard | Email + Slack + PagerDuty |
| SECURITY_* | Dashboard | Email + Slack | Email + Slack + PagerDuty + Block PR |
| PERF_REGRESSION_* | Dashboard | Slack (#perf) | Slack + Email |
| DEPENDENCY_HEALTH | Dashboard | Email (weekly) | Email + Slack |
| COVERAGE_* | Dashboard | Email (weekly) | Email + Slack |
| DEPLOYMENT_READINESS | Dashboard | Slack | Block release |

### Alert Escalation Timeline

| Severity | Channel | Timer | Escalation | Action Required |
|----------|---------|-------|-----------|-----------------|
| RED | Immediate | 5 min | PagerDuty incident | Yes (critical) |
| YELLOW | Standard | 24 hr | Daily summary | Yes (within 24h) |
| GREEN | None | — | Dashboard only | No |

---

## Metrics Collection Points

### Real-Time Collection (Continuous)
```
CI/CD Signals (9)        ← GitHub Actions webhooks
+ Validation Signals (7) ← OTEL tracer on each run
= 16 signals, <1s latency
```

### Scheduled Collection (Daily/Weekly)
```
Security Signals (6)     ← Daily cron job
+ Dependency Health (1)  ← Weekly cron job
= 7 signals, scheduled
```

### On-Demand Collection (Per Release)
```
Deployment Readiness (1) ← Manual release trigger
+ All above signals      ← Aggregate status
= 1 aggregated signal
```

---

## State Transitions Example

### Scenario: Feature Score Degradation
```
Time    Signal                           State  Action
─────────────────────────────────────────────────────────
T0      FEATURE_HEALTH_KNOWLEDGE_HOOKS   🟢 90  Operating
T1      (Test failure detected)          🟡 75  Yellow alert
T2      (More failures)                  🔴 45  Red alert + Slack
T3      (Developer investigating)        🔴 45  PagerDuty escalated
T4      (Root cause: PR #123)            🔴 45  PR reverted
T5      (Tests re-run)                   🟡 62  Yellow (improving)
T6      (Fix deployed)                   🟢 88  Green, back to normal
```

---

## Dashboard Display Order

When rendering the Andon dashboard, display signals in this priority order:

1. **Overall Health** (summary)
   - OVERALL_HEALTH_SCORE
   - DEPLOYMENT_READINESS

2. **Validation Health** (functional)
   - FEATURE_HEALTH_* (all 6)

3. **Pipeline Status** (process)
   - CICD_BUILD_* (all 9)

4. **Security Posture** (protection)
   - SECURITY_* (all 6)

5. **Performance Health** (efficiency)
   - PERF_REGRESSION_* (all 6)

6. **Dependencies** (sustainability)
   - DEPENDENCY_HEALTH

7. **Quality Metrics** (assurance)
   - COVERAGE_* (all 5)

---

## Key Numbers Summary

| Category | Count | Critical | Status |
|----------|-------|----------|--------|
| Total Signals | 53 | 7 | Ready |
| Validation | 7 | 7 | From OTEL validator ✓ |
| CI/CD | 9 | 9 | From GH Actions ✓ |
| Security | 6 | 6 | From security scanners ✓ |
| Performance | 6 | 0 | New, needs baseline |
| Dependencies | 1 | 0 | From pnpm audit ✓ |
| Coverage | 5 | 0 | From vitest ✓ |
| Deployment | 1 | 1 | New, aggregation needed |
| **TOTAL** | **53** | **23** | **70% ready** |

---

## Signal Configuration Template

```javascript
// Template for adding new signal
{
  id: "SIGNAL_NAME",                    // Unique identifier
  group: "category",                    // validation|cicd|security|performance|dependency|coverage|deployment
  name: "Human readable name",
  description: "What this signal measures",
  dataSource: "/path/to/data",          // Where data comes from
  updateFrequency: "per-run|real-time|daily|weekly",
  criticalPath: true|false,             // Blocks deployment?
  thresholds: {
    green: { operator: "≥", value: 80 },
    yellow: { operator: ">=", value: 60 },
    red: { operator: "<", value: 60 }
  },
  metrics: ["metric1", "metric2"],      // Sub-metrics tracked
  alertChannels: {
    red: ["email", "slack", "pagerduty"],
    yellow: ["slack", "email"],
    green: []
  },
  historicalTracking: true|false,       // Store history for trends?
  regressionDetection: true|false       // Track changes?
}
```

---

## Quick Lookup: Which Signal Answers...?

| Question | Signal(s) |
|----------|-----------|
| Is the system healthy? | OVERALL_HEALTH_SCORE |
| Can we deploy? | DEPLOYMENT_READINESS |
| Are tests passing? | FEATURE_HEALTH_* + COVERAGE_* |
| Is security OK? | SECURITY_* |
| Is performance degrading? | PERF_REGRESSION_* |
| Are dependencies outdated? | DEPENDENCY_HEALTH |
| Is the build working? | CICD_BUILD_* |
| Are knowledge hooks working? | FEATURE_HEALTH_KNOWLEDGE_HOOKS_API |
| Is lockchain integrity OK? | FEATURE_HEALTH_LOCKCHAIN_INTEGRITY |
| Are there memory issues? | FEATURE_HEALTH_* (memory metrics) |
| Do we have coverage regression? | COVERAGE_REGRESSION |
| Are there critical vulns? | SECURITY_DEPENDENCIES |

---

This matrix serves as the complete reference for all 53 Andon signals. Use it for:
- Signal configuration during implementation
- Alert routing setup
- Dashboard layout planning
- Team training and onboarding
- Troubleshooting and diagnosis
