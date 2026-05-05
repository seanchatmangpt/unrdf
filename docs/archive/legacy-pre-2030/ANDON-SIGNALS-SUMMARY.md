# Andon Signals Design - Executive Summary

## What is Andon for UNRDF?

**Andon** (visual management system) brings enterprise-grade real-time health monitoring and alerting to UNRDF. It translates complex metrics into simple **RED/YELLOW/GREEN** status indicators that enable rapid problem detection and response.

---

## Current State vs. Proposed State

### Before Andon (Current)
```
Team member needs to know system health:
1. Run manual validation: pnpm run validation
2. Check CI/CD pipeline: GitHub Actions tab
3. Review security scan: Separate security.yml output
4. Check coverage: Find coverage report
5. Compile findings: Mental aggregation
6. Time spent: 15-20 minutes
7. Status visibility: Low
8. Problem detection: Reactive (after failures)
```

### With Andon (Proposed)
```
Team member runs:
$ pnpm run andon:status

Output:
┌──────────────────────────────────────────────────┐
│ UNRDF vlatest ANDON STATUS                          │
├──────────────────────────────────────────────────┤
│ 🟢 SYSTEM HEALTH: GREEN                          │
│ 🟢 Validation: 87/100                            │
│ 🟢 CI/CD: All passing                            │
│ 🟢 Security: No critical vulns                   │
│ 🟢 Performance: No regressions                   │
│ ✓ Deploy Ready: YES                              │
└──────────────────────────────────────────────────┘

Time spent: <1 second
Status visibility: Complete
Problem detection: Proactive (real-time alerts)
```

---

## Andon Signal Overview

### 53 Total Signals Across 7 Signal Groups

```
┌─────────────────────────────────────────────────────────┐
│         ANDON SIGNAL GROUPS (53 Total)                  │
├──────────────────┬──────────┬────────────────────────────┤
│ Group            │ Count    │ Status Update              │
├──────────────────┼──────────┼────────────────────────────┤
│ Validation       │ 7        │ After each test run        │
│ CI/CD Pipeline   │ 9        │ Real-time from GH Actions  │
│ Security         │ 6        │ Daily scheduled + on-demand│
│ Performance      │ 6        │ Per validation run         │
│ Dependencies     │ 1        │ Weekly automated check     │
│ Test Coverage    │ 5        │ Per test run               │
│ Deployment       │ 1 (6 gates) │ On release attempt      │
├──────────────────┼──────────┼────────────────────────────┤
│ TOTAL            │ 53       │ Real-time or scheduled     │
└──────────────────┴──────────┴────────────────────────────┘
```

---

## 7 Signal Groups Explained

### 1. Validation Signals (7 signals)
**What**: OTEL-based feature health scores (0-100)
**Why**: Direct measure of core functionality
**From**: `/validation/run-all.mjs` OTEL validator
**Threshold**: Green ≥80, Yellow 60-79, Red <60

```
🟢 FEATURE_HEALTH_KNOWLEDGE_ENGINE_CORE        85/100
🟢 FEATURE_HEALTH_KNOWLEDGE_HOOKS_API          90/100
🟢 FEATURE_HEALTH_POLICY_PACKS                 88/100
🟢 FEATURE_HEALTH_LOCKCHAIN_INTEGRITY          82/100
🟡 FEATURE_HEALTH_TRANSACTION_MANAGER          75/100 ⚠️
🟢 FEATURE_HEALTH_BROWSER_COMPATIBILITY        91/100
   OVERALL_HEALTH_SCORE                        87/100
```

### 2. CI/CD Pipeline Signals (9 signals)
**What**: GitHub Actions workflow job status
**Why**: Early detection of build failures
**From**: `.github/workflows/*.yml` job results
**Threshold**: Green (pass), Yellow (warning), Red (fail)

```
🟢 CICD_BUILD_TYPESCRIPT_GATE ........... PASS (fail-fast)
🟢 CICD_BUILD_LINT ..................... PASS
🟢 CICD_BUILD_TEST ..................... PASS (18,20,22)
🟡 CICD_BUILD_SECURITY ................. WARN (2 deps outdated)
🟢 CICD_BUILD_BUILD_ARTIFACTS .......... PASS
🟢 CICD_BUILD_DOCUMENTATION ............ PASS
🟢 CICD_BUILD_BENCHMARK ................ PASS
🟢 CICD_BUILD_INTEGRATION .............. PASS
🟢 CICD_BUILD_RELEASE_PREP ............. PASS
```

### 3. Security Signals (6 signals)
**What**: Vulnerability and compliance scanning results
**Why**: Prevent security regressions
**From**: `.github/workflows/security.yml` scanner outputs
**Threshold**: Red (critical/high), Yellow (medium), Green (none)

```
🟢 SECURITY_DEPENDENCIES ............... 0 critical, 0 high ✓
🟢 SECURITY_SAST ....................... No issues (CodeQL)
🟢 SECURITY_SECRETS .................... No secrets detected
🟢 SECURITY_LICENSE_COMPLIANCE ......... All approved licenses
🟢 SECURITY_SUPPLY_CHAIN ............... Integrity verified
🟢 SECURITY_CONTAINER .................. No CVEs found
```

### 4. Performance Signals (6 signals)
**What**: Operation latency trending against baselines
**Why**: Catch performance regressions early
**From**: OTEL latency metrics with historical baselines
**Threshold**: Red >20% regression, Yellow 10-20%, Green <10%

```
🟢 PERF_REGRESSION_PARSE_TURTLE ........ 23ms (baseline: 20ms) +15%
🟢 PERF_REGRESSION_QUERY_SPARQL ........ 18ms (baseline: 18ms) ±0%
🟢 PERF_REGRESSION_VALIDATE_SHACL ...... 34ms (baseline: 35ms) -3%
🟢 PERF_REGRESSION_KNOWLEDGE_HOOKS ..... 12ms (baseline: 12ms) ±0%
🟢 PERF_REGRESSION_LOCKCHAIN ........... 8ms  (baseline: 9ms)  -11%
🟢 PERF_REGRESSION_BROWSER_OPERATIONS .. 48ms (baseline: 50ms) -4%
```

### 5. Dependency Health Signal (1 signal)
**What**: Aggregated package health metrics
**Why**: Prevent dependency-related surprises
**From**: `pnpm audit`, npm deprecation API, license checker
**Threshold**: Red (vulns or violations), Yellow (updates), Green (current)

```
🟢 DEPENDENCY_HEALTH
   ├─ Outdated packages: 2 (minor updates available)
   ├─ Security advisories: 0
   ├─ License violations: 0
   └─ Deprecated packages: 0
```

### 6. Test Coverage Signals (5 signals)
**What**: Code coverage percentage trends
**Why**: Prevent test coverage from silently degrading
**From**: vitest coverage reports (coverage-final.json)
**Threshold**: Red <72%, Yellow 72-80%, Green ≥80%

```
🟢 COVERAGE_STATEMENTS ................. 87% (threshold: 80%) ✓
🟢 COVERAGE_BRANCHES ................... 82% (threshold: 75%) ✓
🟢 COVERAGE_FUNCTIONS .................. 85% (threshold: 80%) ✓
🟢 COVERAGE_LINES ...................... 86% (threshold: 80%) ✓
   COVERAGE_REGRESSION ................. -latest% (acceptable)
```

### 7. Deployment Readiness Signal (1 signal, 6 gates)
**What**: Multi-gate deployment eligibility check
**Why**: Prevent broken releases
**From**: Aggregate of validation, CI/CD, security, performance signals
**Threshold**: Green (all gates), Yellow (some gates), Red (any fail)

```
🟢 DEPLOYMENT_READINESS (Release vlatest)
   ✓ GATE_BUILD ................. Artifacts present
   ✓ GATE_TESTS ................. Score 87/100 (passing)
   ✓ GATE_SECURITY .............. No critical issues
   ✓ GATE_PERFORMANCE ........... No major regressions
   ✓ GATE_DOCUMENTATION ......... API docs generated
   ✗ GATE_CHANGELOG ............. Manual update needed

   Status: 🟡 YELLOW (5/6 gates passed)
   Action: Update changelog to proceed
```

---

## Alert Channels by Severity

### RED (Critical - Stop Everything)
```
Affected Signals:
  • FEATURE_HEALTH < 60
  • CICD_BUILD any failure
  • SECURITY critical/high vuln
  • DEPLOYMENT_READINESS gate failure

Alert Channels:
  → Email (immediate)
  → Slack @channel (mention team)
  → PagerDuty (create incident)
  → GitHub (block PR merge)

Response Time: 5 minutes max
Action: Rollback or fix immediately
```

### YELLOW (Warning - Attention Needed)
```
Affected Signals:
  • FEATURE_HEALTH 60-79
  • PERF_REGRESSION 10-20%
  • COVERAGE regression >2%
  • DEPENDENCY updates >5
  • SECURITY medium vulns

Alert Channels:
  → Slack #alerts
  → Email summary

Response Time: 24 hours
Action: Investigate and plan fixes
```

### GREEN (All Clear)
```
Status: Operating normally
Dashboard: Real-time updates only
No alerts sent
```

---

## Implementation Roadmap

### Phase 1: Core Signals (Weeks 1-2)
- ✓ 35 signals (validation + CI/CD + deployment)
- ✓ Terminal dashboard (`pnpm run andon:status`)
- ✓ Signal aggregation and scoring
- Effort: ~10 hours
- Value: Immediate visibility

### Phase 2: Expanded Coverage (Weeks 3-4)
- ✓ 13 more signals (security + performance + dependencies)
- ✓ Signal history and trending
- ✓ Regression detection
- Effort: ~20 hours
- Value: Proactive problem detection

### Phase 3: Polish & Testing (Weeks 5-6)
- ✓ 5 coverage signals
- ✓ Unit and integration tests
- ✓ Documentation
- Effort: ~10 hours
- Value: Production ready

### Phase 4: Alerting (Weeks 7-8, Optional)
- Slack integration
- Email alerts
- PagerDuty incidents
- GitHub status checks
- Effort: ~15 hours
- Value: Automatic escalation

**Total MVP Effort: 5-6 weeks, 53 signals, production-grade**

---

## Data Sources

Each signal pulls from existing UNRDF infrastructure:

```
Signal Group              Data Source                              Effort
────────────────────────────────────────────────────────────────────────
Validation               /validation/run-all.mjs OTEL output      Already exists ✓
CI/CD Pipeline           .github/workflows/*.yml job status       Webhook/parser needed
Security                 .github/workflows/security.yml outputs   Parser needed
Performance              OTEL latency metrics + baseline tracking Need baseline DB
Dependencies             pnpm audit --json output                 Already available
Test Coverage            vitest coverage-final.json report        Already available
Deployment Readiness     Aggregate of above signals               Orchestration needed
```

---

## Key Metrics

### Before Andon
- Problem detection: Reactive (after failures)
- Mean time to detect (MTTD): 30+ minutes
- Manual status checks: 15+ minutes each
- Deployment safety: Manual review gates
- Team visibility: Low

### With Andon
- Problem detection: Proactive (real-time alerts)
- MTTD: <5 minutes (automated)
- Status checks: <1 second (automated)
- Deployment safety: Automated gates
- Team visibility: Complete

### Success Targets
- Signal coverage: 95%+ of critical systems
- Alert response time: <5 minutes for RED
- False positive rate: <5%
- Dashboard uptime: latest%
- MTTR improvement: 50%+ reduction
- Team adoption: 100% daily usage

---

## Example Scenarios

### Scenario 1: Feature Degradation
```
Event: FEATURE_HEALTH_KNOWLEDGE_HOOKS_API drops from 90 to 55
State: RED (critical)
Alert: Slack @devops, Email, PagerDuty incident created
Dashboard: Shows 🔴 signal, lists affected spans
Team action:
  1. View Andon dashboard → see which spans are failing
  2. Check recent commits → identify problematic change
  3. Roll back or fix
  4. Re-validate → signal turns 🟡 YELLOW
  5. Deploy fix → signal turns 🟢 GREEN
Time to resolution: 15 minutes (vs. 2+ hours manually)
```

### Scenario 2: Security Alert
```
Event: SECURITY_DEPENDENCIES detects new CVE
State: RED (critical severity)
Alert: Email + Slack #security-alerts + GitHub
Dashboard: Shows 🔴 SECURITY_DEPENDENCIES with details
Team action:
  1. Update vulnerable dependency
  2. Re-run security scan
  3. Verify no new issues
  4. Deploy patch
Time to fix: 1 hour (vs. 4+ hours discovery + assessment)
```

### Scenario 3: Performance Regression
```
Event: PERF_REGRESSION_QUERY_SPARQL exceeds 20% baseline
State: YELLOW → RED if exceeds 30%
Alert: Slack #performance-team
Dashboard: Shows 🟡 signal with trend graph
Team action:
  1. Profile the operation
  2. Identify optimization opportunity
  3. Implement and test
  4. Verify regression resolved
Early detection prevents: User complaints, SLA breaches
```

### Scenario 4: Release Gate
```
Event: Release workflow starts for vlatest
Dashboard checks DEPLOYMENT_READINESS gates:
  ✓ Build artifacts present
  ✓ Tests: 87/100 (passing)
  ✓ Security: No critical issues
  ✓ Performance: No major regression
  ✗ Changelog: Needs manual update
  ✓ Docs: Generated
Result: 🟡 YELLOW - 5/6 gates pass
Action: Changelog updated → all gates ✓ → 🟢 GREEN → Deploy!
Safety: Prevents releasing without proper documentation
```

---

## Dashboard Example

```
$ pnpm run andon:status

┌────────────────────────────────────────────────────────┐
│  UNRDF vlatest ANDON DASHBOARD                            │
│  Last Update: 2024-11-21 15:30:45 UTC                  │
├────────────────────────────────────────────────────────┤
│                                                        │
│  🟢 SYSTEM HEALTH: GREEN (87/100)                      │
│  📊 All 53 signals monitored                           │
│                                                        │
├────────────────────────────────────────────────────────┤
│  VALIDATION SIGNALS (7) - Overall: 87/100 🟢           │
│  ├─ 🟢 Knowledge Engine Core ..... 85/100              │
│  ├─ 🟢 Knowledge Hooks API ....... 90/100              │
│  ├─ 🟢 Policy Packs ............. 88/100              │
│  ├─ 🟢 Lockchain Integrity ....... 82/100              │
│  ├─ 🟡 Transaction Manager ....... 75/100 ⚠️            │
│  ├─ 🟢 Browser Compatibility ..... 91/100              │
│  └─ OVERALL SCORE ............... 87/100 ✓             │
│                                                        │
├────────────────────────────────────────────────────────┤
│  CI/CD PIPELINE (9) - Status: All Green 🟢             │
│  ├─ 🟢 TypeScript Gate (1s)                           │
│  ├─ 🟢 Lint (45s)                                      │
│  ├─ 🟢 Test Suite (2m 34s)                            │
│  ├─ 🟡 Security Audit (1m 12s) [2 outdated deps]      │
│  ├─ 🟢 Build Artifacts (1m 5s)                        │
│  ├─ 🟢 Documentation (45s)                             │
│  ├─ 🟢 Benchmarks (2m 15s)                            │
│  ├─ 🟢 Integration Tests (3m)                          │
│  └─ 🟢 Release Prep (1m 30s)                          │
│                                                        │
├────────────────────────────────────────────────────────┤
│  SECURITY SIGNALS (6) - Overall: Secure 🟢             │
│  ├─ 🟢 Dependencies ........... 0 crit, 0 high        │
│  ├─ 🟢 SAST (CodeQL) ......... No issues              │
│  ├─ 🟢 Secrets Detection ..... Clean                   │
│  ├─ 🟢 Licenses ............. All compliant            │
│  ├─ 🟢 Supply Chain .......... Verified                │
│  └─ 🟢 Container Scan ....... No CVEs                  │
│                                                        │
├────────────────────────────────────────────────────────┤
│  DEPLOYMENT READINESS - Release vlatest 🟢              │
│  ├─ ✓ Build Complete                                   │
│  ├─ ✓ Tests Pass (87/100)                             │
│  ├─ ✓ Security Clear                                  │
│  ├─ ✓ Performance OK                                  │
│  ├─ ✓ Docs Ready                                      │
│  ├─ ✓ Changelog Updated                               │
│  └─ STATUS: 🟢 READY TO DEPLOY                         │
│                                                        │
└────────────────────────────────────────────────────────┘

Learn more: pnpm run andon:help
Detailed view: pnpm run andon:detailed
```

---

## Key Files

**Design Documents**:
- `/docs/ANDON-SIGNALS-DESIGN.md` - Complete technical design (2,500+ lines)
- `/docs/ANDON-SIGNALS-IMPLEMENTATION-GUIDE.md` - Concrete implementation steps
- `/docs/ANDON-SIGNALS-SUMMARY.md` - This document (quick reference)

**Data Sources**:
- `/validation/run-all.mjs` - OTEL validation framework
- `.github/workflows/ci.yml` - CI/CD pipeline definitions
- `.github/workflows/security.yml` - Security scanning
- `/src/validation/otel-validator.mjs` - OTEL span validation

---

## Next Steps

1. **Review** this summary and full design
2. **Approve** signal design and implementation plan
3. **Start Phase 1** with validation + CI/CD + deployment signals
4. **Iterate** with team feedback
5. **Expand** to full 53-signal coverage
6. **Integrate** alerting channels (Slack, PagerDuty)
7. **Deploy** to production

---

## Questions?

Refer to:
- **Detailed Design**: `ANDON-SIGNALS-DESIGN.md`
- **Implementation Steps**: `ANDON-SIGNALS-IMPLEMENTATION-GUIDE.md`
- **Data Sources**: Cross-reference section above
- **Scenarios**: Real-world examples section

**Estimated Implementation**: 5-6 weeks for production-ready 53-signal system with complete dashboard and alerting.
