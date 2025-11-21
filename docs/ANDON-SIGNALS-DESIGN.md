# Andon Signals Design - UNRDF v4.0

## Executive Summary

Andon (Visual Management/Alert System) is a Lean manufacturing concept for real-time status visibility and rapid problem escalation. This document designs Andon signals for the UNRDF project to provide production-grade monitoring, alerting, and visual indicators across all critical systems.

**Current State**: UNRDF has excellent OTEL-based validation (`validation/run-all.mjs`, OTELValidator) and comprehensive CI/CD pipelines, but lacks integrated Andon visualization and real-time alerting.

**Goal**: Design Andon signals that translate OTEL metrics, validation scores, and CI/CD status into actionable red/yellow/green indicators for development teams and operations.

---

## 1. Current Monitoring Coverage Analysis

### 1.1 Existing Observability Infrastructure

**OTEL-Based Validation (Implemented)**
- Location: `/validation/run-all.mjs`, `/src/validation/otel-validator.mjs`
- Coverage: 6 core features with span-based validation
- Metrics: Validation scores (0-100), latency, error rates, memory usage
- Status: Active - v3.1.0 validation framework in place

**Feature Validation Scoring**
```
Knowledge Engine Core (30%)     - Expected spans: parse.turtle, query.sparql, validate.shacl, reason.n3
Knowledge Hooks API (20%)       - Expected spans: hook.define, hook.register, hook.execute, hook.evaluate
Policy Packs (15%)              - Expected spans: policy.load, policy.activate, policy.validate
Lockchain Integrity (15%)       - Expected spans: lockchain.write, lockchain.verify, lockchain.commit
Transaction Manager (10%)       - Expected spans: transaction.start, transaction.commit, transaction.rollback
Browser Compatibility (10%)     - Expected spans: browser.parse, browser.query, browser.validate
```

**Performance Thresholds (Implemented)**
- Knowledge Engine: Max 1000ms latency, max 1% error rate
- Knowledge Hooks: Max 500ms latency, max 1% error rate
- Policy Packs: Max 800ms latency, max 1% error rate
- Lockchain: Max 600ms latency, max 1% error rate
- Transaction Manager: Max 500ms latency, max 1% error rate
- Browser: Max 1200ms latency, max 5% error rate (lenient for browser)

**CI/CD Pipeline (Implemented)**
- Location: `.github/workflows/`
- Stages: TypeScript Gate, Lint, Test, Security, Build, Docs, Release, Benchmark, Integration
- Security: Dependency scanning, SAST (CodeQL), secrets scanning, license compliance
- Performance: Dark matter 80/20 benchmarks

**Security Scanning (Implemented)**
- Dependency vulnerabilities (pnpm audit)
- SAST analysis (CodeQL)
- Secrets detection (TruffleHog, Gitleaks)
- License compliance checking
- Container scanning (Trivy, Docker Scout)
- Supply chain analysis

### 1.2 Monitoring Gaps

**What's Missing**:
1. No unified Andon dashboard/visualization
2. No real-time alerting channels (Slack, PagerDuty, email)
3. No health check endpoints beyond build status
4. No performance trend tracking
5. No dependency deprecation warnings
6. No test coverage regression detection
7. No build time trend analysis
8. No deployment readiness gates with visual indicators
9. No SLA/SLO monitoring for core operations
10. No custom metric aggregation across pipelines

---

## 2. Andon Signal Design

### 2.1 Signal State Model

Each Andon signal uses a **3-state traffic light model**:

```
GREEN (✓)   = All thresholds met, system healthy
YELLOW (⚠)  = Warning threshold reached, attention needed
RED (✗)     = Critical threshold exceeded, immediate action required
```

---

### 2.2 Core Andon Signal Groups

#### A. VALIDATION SIGNALS (OTEL Score-Based)

**Andon Signal: Feature Health Score**
```
Signal ID:     FEATURE_HEALTH_{FEATURE_NAME}
Source:        /validation/run-all.mjs OTEL validation scores
Metric:        Weighted feature validation score (0-100)
Update Freq:   On CI/CD run, hourly health check
```

**State Thresholds**:
| State  | Threshold | Action Required |
|--------|-----------|-----------------|
| GREEN  | ≥ 80      | System operational |
| YELLOW | 60-79     | Investigate violations |
| RED    | < 60      | Critical issues, manual intervention |

**Features Monitored**:
1. `knowledge-engine-core` (30% weight) - Validates parse, query, validate, reason spans
2. `knowledge-hooks-api` (20% weight) - Validates hook lifecycle
3. `policy-packs` (15% weight) - Validates policy operations
4. `lockchain-integrity` (15% weight) - Validates cryptographic audit trail
5. `transaction-manager` (10% weight) - Validates ACID guarantees
6. `browser-compatibility` (10% weight) - Validates browser shims/polyfills

**Implementation**:
```javascript
// Proposed: andon/feature-health-signal.mjs
export class FeatureHealthSignal {
  constructor(featureName, weight) {
    this.featureName = featureName;
    this.weight = weight;
    this.score = 0;
    this.state = 'GREEN';
  }

  updateScore(newScore) {
    this.score = newScore;
    this.state = newScore >= 80 ? 'GREEN'
               : newScore >= 60 ? 'YELLOW'
               : 'RED';
    return {
      signal: `FEATURE_HEALTH_${this.featureName.toUpperCase()}`,
      state: this.state,
      score: this.score,
      weight: this.weight,
      timestamp: new Date().toISOString()
    };
  }

  getOverallHealth(allFeatures) {
    const weightedScore = allFeatures.reduce((sum, f) =>
      sum + (f.score * f.weight), 0) / allFeatures.reduce((s, f) => s + f.weight, 0);
    return weightedScore >= 80 ? 'GREEN'
         : weightedScore >= 60 ? 'YELLOW'
         : 'RED';
  }
}
```

---

#### B. CI/CD PIPELINE SIGNALS

**Andon Signal: Build Pipeline Status**
```
Signal ID:     CICD_BUILD_{PIPELINE_NAME}
Source:        .github/workflows/ GitHub Actions status
Metric:        Pipeline completion status
Update Freq:   Real-time on job status change
```

**Signals by Pipeline**:
```
CICD_BUILD_TYPESCRIPT_GATE    - TypeScript artifact check (fail-fast)
CICD_BUILD_LINT               - ESLint + Prettier check
CICD_BUILD_TEST               - Multi-version test suite (18, 20, 22)
CICD_BUILD_SECURITY           - Security audit + CodeQL
CICD_BUILD_BUILD_ARTIFACTS    - Build and artifact verification
CICD_BUILD_DOCUMENTATION      - API docs generation
CICD_BUILD_BENCHMARK          - Dark matter 80/20 performance tests
CICD_BUILD_INTEGRATION        - E2E integration tests
CICD_BUILD_RELEASE_PREP       - Release artifact preparation
```

**State Logic**:
```
GREEN  = All steps passed ✓
YELLOW = Warning (deprecated deps, license issues) ⚠
RED    = Failed step, cannot proceed ✗
```

**Implementation**:
```javascript
// Proposed: andon/cicd-signal.mjs
export class CICDSignal {
  constructor(pipelineName) {
    this.pipelineName = pipelineName;
    this.jobs = new Map();
    this.state = 'GREEN';
  }

  updateJobStatus(jobName, status) {
    // status: 'success' | 'failure' | 'skipped' | 'cancelled'
    this.jobs.set(jobName, {
      name: jobName,
      status: status,
      timestamp: new Date().toISOString()
    });

    // Recalculate overall state
    const failed = Array.from(this.jobs.values()).filter(j => j.status === 'failure');
    const warnings = Array.from(this.jobs.values()).filter(j => j.status === 'skipped');

    this.state = failed.length > 0 ? 'RED'
               : warnings.length > 0 ? 'YELLOW'
               : 'GREEN';
  }

  getSignal() {
    return {
      signal: `CICD_BUILD_${this.pipelineName.toUpperCase()}`,
      state: this.state,
      failedJobs: Array.from(this.jobs.values()).filter(j => j.status === 'failure'),
      timestamp: new Date().toISOString()
    };
  }
}
```

---

#### C. SECURITY SIGNALS

**Andon Signal: Security Scan Status**
```
Signal ID:     SECURITY_{SCAN_TYPE}
Source:        .github/workflows/security.yml
Metric:        Vulnerability count by severity
Update Freq:   Daily (scheduled) or on-demand
```

**Security Signals**:
```
SECURITY_DEPENDENCIES         - Dependency audit (critical/high vulns)
SECURITY_SAST                 - CodeQL SAST analysis
SECURITY_SECRETS              - Secret scanning (TruffleHog, Gitleaks)
SECURITY_LICENSE_COMPLIANCE   - License compliance check
SECURITY_SUPPLY_CHAIN         - Supply chain analysis (@lavamoat)
SECURITY_CONTAINER            - Container image scanning (Trivy)
```

**State Thresholds**:
```
GREEN  = No critical or high severity vulnerabilities
YELLOW = Moderate vulnerabilities (> 0 moderate) or warnings
RED    = Critical or high severity vulnerabilities found
```

**Vulnerability Count Tracking**:
```javascript
export class SecuritySignal {
  constructor(scanType) {
    this.scanType = scanType;
    this.vulnerabilities = { critical: 0, high: 0, medium: 0, low: 0 };
    this.state = 'GREEN';
  }

  updateVulnerabilities(critical, high, medium, low) {
    this.vulnerabilities = { critical, high, medium, low };

    if (critical > 0 || high > 0) {
      this.state = 'RED';
    } else if (medium > 0) {
      this.state = 'YELLOW';
    } else {
      this.state = 'GREEN';
    }
  }

  getSignal() {
    return {
      signal: `SECURITY_${this.scanType.toUpperCase()}`,
      state: this.state,
      vulnerabilities: this.vulnerabilities,
      timestamp: new Date().toISOString()
    };
  }
}
```

---

#### D. PERFORMANCE SIGNALS

**Andon Signal: Performance Regression**
```
Signal ID:     PERF_REGRESSION_{OPERATION}
Source:        /validation/run-all.mjs latency metrics
Metric:        Latency change from baseline
Update Freq:   Per test run
```

**Performance Signals**:
```
PERF_REGRESSION_PARSE_TURTLE          - Parse latency trend
PERF_REGRESSION_QUERY_SPARQL          - Query latency trend
PERF_REGRESSION_VALIDATE_SHACL        - Validation latency trend
PERF_REGRESSION_KNOWLEDGE_HOOKS       - Hook execution latency
PERF_REGRESSION_LOCKCHAIN_OPERATIONS  - Lockchain write/verify latency
PERF_REGRESSION_BROWSER_OPERATIONS    - Browser operation latency
```

**Baseline & Thresholds**:
```javascript
export class PerformanceSignal {
  constructor(operation, baselineMs, tolerancePercent = 10) {
    this.operation = operation;
    this.baselineMs = baselineMs;
    this.tolerancePercent = tolerancePercent;
    this.currentMs = baselineMs;
    this.state = 'GREEN';
  }

  updateLatency(newLatencyMs) {
    this.currentMs = newLatencyMs;
    const percentChange = ((newLatencyMs - this.baselineMs) / this.baselineMs) * 100;

    if (percentChange > this.tolerancePercent * 2) {
      this.state = 'RED';      // >20% regression
    } else if (percentChange > this.tolerancePercent) {
      this.state = 'YELLOW';   // 10-20% regression
    } else {
      this.state = 'GREEN';    // <10% regression
    }
  }

  getSignal() {
    return {
      signal: `PERF_REGRESSION_${this.operation.toUpperCase()}`,
      state: this.state,
      baselineMs: this.baselineMs,
      currentMs: this.currentMs,
      percentChange: ((this.currentMs - this.baselineMs) / this.baselineMs) * 100,
      timestamp: new Date().toISOString()
    };
  }
}
```

---

#### E. DEPENDENCY SIGNALS

**Andon Signal: Dependency Health**
```
Signal ID:     DEPENDENCY_{HEALTH_TYPE}
Source:        package.json version scanning, GitHub dependabot
Metric:        Count of outdated/deprecated/vulnerable dependencies
Update Freq:   Weekly automated check
```

**Dependency Signals**:
```
DEPENDENCY_UPDATES_AVAILABLE     - Outdated packages
DEPENDENCY_SECURITY_ADVISORIES   - Known vulnerabilities
DEPENDENCY_LICENSE_VIOLATIONS    - Non-compliant licenses
DEPENDENCY_DEPRECATION_WARNING   - Deprecated packages
```

**State Logic**:
```javascript
export class DependencySignal {
  constructor() {
    this.outdated = 0;
    this.vulnerabilities = 0;
    this.licenseViolations = 0;
    this.deprecated = 0;
    this.state = 'GREEN';
  }

  updateCounts(outdated, vulns, licenses, deprecated) {
    this.outdated = outdated;
    this.vulnerabilities = vulns;
    this.licenseViolations = licenses;
    this.deprecated = deprecated;

    if (vulns > 0 || licenses > 0) {
      this.state = 'RED';
    } else if (outdated > 5 || deprecated > 0) {
      this.state = 'YELLOW';
    } else {
      this.state = 'GREEN';
    }
  }

  getSignal() {
    return {
      signal: 'DEPENDENCY_HEALTH',
      state: this.state,
      outdated: this.outdated,
      vulnerabilities: this.vulnerabilities,
      licenseViolations: this.licenseViolations,
      deprecated: this.deprecated,
      timestamp: new Date().toISOString()
    };
  }
}
```

---

#### F. TEST COVERAGE SIGNALS

**Andon Signal: Test Coverage Regression**
```
Signal ID:     COVERAGE_{METRIC}
Source:        vitest coverage reports
Metric:        Code coverage percentage
Update Freq:   Per test run
```

**Coverage Signals**:
```
COVERAGE_STATEMENTS   - Statement coverage percentage
COVERAGE_BRANCHES     - Branch coverage percentage
COVERAGE_FUNCTIONS    - Function coverage percentage
COVERAGE_LINES        - Line coverage percentage
COVERAGE_REGRESSION   - Coverage trending
```

**Thresholds**:
```javascript
export class CoverageSignal {
  constructor(metric, minThreshold = 80) {
    this.metric = metric;
    this.minThreshold = minThreshold;
    this.currentCoverage = 100;
    this.previousCoverage = 100;
    this.state = 'GREEN';
  }

  updateCoverage(currentPercent) {
    this.previousCoverage = this.currentCoverage;
    this.currentCoverage = currentPercent;

    if (currentPercent < this.minThreshold * 0.9) {
      this.state = 'RED';       // <72% (below 90% of min)
    } else if (currentPercent < this.minThreshold) {
      this.state = 'YELLOW';    // 72-80% (below min)
    } else if (currentPercent < this.previousCoverage - 2) {
      this.state = 'YELLOW';    // Regression >2%
    } else {
      this.state = 'GREEN';
    }
  }

  getSignal() {
    return {
      signal: `COVERAGE_${this.metric.toUpperCase()}`,
      state: this.state,
      currentCoverage: this.currentCoverage,
      previousCoverage: this.previousCoverage,
      regression: this.previousCoverage - this.currentCoverage,
      threshold: this.minThreshold,
      timestamp: new Date().toISOString()
    };
  }
}
```

---

#### G. DEPLOYMENT READINESS SIGNALS

**Andon Signal: Release Readiness**
```
Signal ID:     DEPLOYMENT_READINESS_{STAGE}
Source:        All prior signals + artifact availability
Metric:        Gate status (pass/fail)
Update Freq:   On release attempt
```

**Deployment Signals**:
```
DEPLOYMENT_READINESS_BUILD        - All artifacts built successfully
DEPLOYMENT_READINESS_TESTS        - All tests passing (80%+ score)
DEPLOYMENT_READINESS_SECURITY     - No critical/high vulnerabilities
DEPLOYMENT_READINESS_PERFORMANCE  - No major regressions (>20%)
DEPLOYMENT_READINESS_DOCS         - Documentation generated
DEPLOYMENT_READINESS_CHANGELOG    - Changelog updated
```

**Implementation - Deployment Gate**:
```javascript
export class DeploymentReadinessSignal {
  constructor() {
    this.gates = {
      build: false,
      tests: false,
      security: false,
      performance: false,
      docs: false,
      changelog: false
    };
    this.state = 'RED';
  }

  updateGate(gateName, passed) {
    this.gates[gateName] = passed;

    const allPassed = Object.values(this.gates).every(g => g);
    const somePassed = Object.values(this.gates).some(g => g);

    this.state = allPassed ? 'GREEN'
               : somePassed ? 'YELLOW'
               : 'RED';
  }

  getSignal() {
    return {
      signal: 'DEPLOYMENT_READINESS',
      state: this.state,
      gates: this.gates,
      passedGates: Object.values(this.gates).filter(g => g).length,
      totalGates: Object.keys(this.gates).length,
      timestamp: new Date().toISOString()
    };
  }

  canDeploy() {
    return this.state === 'GREEN';
  }
}
```

---

## 3. Andon Signal Summary Matrix

| Signal Group | Signal ID | Source | Metric | Update Freq | Critical? |
|---|---|---|---|---|---|
| Validation | `FEATURE_HEALTH_*` | OTEL validation scores | Feature score 0-100 | Per run | Yes |
| CI/CD | `CICD_BUILD_*` | GitHub Actions | Job status | Real-time | Yes |
| Security | `SECURITY_*` | Security scanners | Vuln count | Daily | Yes |
| Performance | `PERF_REGRESSION_*` | Latency metrics | Latency trend | Per run | No |
| Dependencies | `DEPENDENCY_*` | npm/pnpm audit | Outdated/vuln count | Weekly | No |
| Coverage | `COVERAGE_*` | vitest reports | % coverage | Per run | No |
| Deployment | `DEPLOYMENT_READINESS_*` | All gates | Gate status | On release | Yes |

---

## 4. Andon Dashboard Design

### 4.1 Real-Time Dashboard Layout

```
┌─────────────────────────────────────────────────────────────────┐
│  UNRDF v4.0 - ANDON DASHBOARD (Live Status)                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  🟢 SYSTEM HEALTH: GREEN  │  Last Update: 2024-11-21 15:30:45  │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│  VALIDATION SIGNALS (Overall Score: 87/100 - GREEN)            │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ 🟢 Knowledge Engine Core        85/100 (30% weight)     │   │
│  │ 🟢 Knowledge Hooks API          90/100 (20% weight)     │   │
│  │ 🟢 Policy Packs                 88/100 (15% weight)     │   │
│  │ 🟢 Lockchain Integrity          82/100 (15% weight)     │   │
│  │ 🟡 Transaction Manager          75/100 (10% weight)     │   │
│  │ 🟢 Browser Compatibility        91/100 (10% weight)     │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│  CI/CD PIPELINE STATUS                                          │
│  ┌──────────────┬──────────┬────────────────────────────────┐   │
│  │ Pipeline     │ Status   │ Details                        │   │
│  ├──────────────┼──────────┼────────────────────────────────┤   │
│  │ TypeScript   │ 🟢 PASS  │ All checks passed              │   │
│  │ Lint         │ 🟢 PASS  │ ESLint + Prettier              │   │
│  │ Test         │ 🟢 PASS  │ 18,20,22 Node versions         │   │
│  │ Security     │ 🟡 WARN  │ 1 medium vuln, 2 outdated deps │   │
│  │ Build        │ 🟢 PASS  │ Artifacts verified             │   │
│  │ Docs         │ 🟢 PASS  │ API docs generated             │   │
│  │ Benchmark    │ 🟢 PASS  │ No regressions                 │   │
│  │ Integration  │ 🟢 PASS  │ E2E tests passed               │   │
│  └──────────────┴──────────┴────────────────────────────────┘   │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│  SECURITY SIGNALS (Overall: GREEN)                              │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │ 🟢 Dependencies      0 critical, 0 high vulns              │ │
│  │ 🟢 SAST (CodeQL)     No issues found                       │ │
│  │ 🟢 Secrets           No secrets detected                   │ │
│  │ 🟢 Licenses          All compliant (MIT, Apache, BSD)      │ │
│  │ 🟢 Supply Chain      ✓ Integrity verified                  │ │
│  │ 🟢 Container         No CVEs found                         │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│  PERFORMANCE SIGNALS (Overall: GREEN)                           │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │ 🟢 Parse Turtle         23ms   (baseline: 20ms, +15%)      │ │
│  │ 🟢 Query SPARQL         18ms   (baseline: 18ms, ±0%)       │ │
│  │ 🟢 Validate SHACL       34ms   (baseline: 35ms, -3%)       │ │
│  │ 🟢 Knowledge Hooks      12ms   (baseline: 12ms, ±0%)       │ │
│  │ 🟢 Lockchain            8ms    (baseline: 9ms, -11%)       │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│  DEPLOYMENT READINESS (Release v4.0.1)                          │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │ ✓ Build Complete          │ Build artifacts present     │    │
│  │ ✓ Tests Pass (87/100)      │ All suites passing          │    │
│  │ ✓ Security Clear           │ No critical issues          │    │
│  │ ✓ Performance Verified     │ No major regressions        │    │
│  │ ✓ Docs Generated          │ API documentation ready     │    │
│  │ ✓ Changelog Updated       │ Release notes prepared      │    │
│  │                                                          │    │
│  │ STATUS: 🟢 READY TO DEPLOY                             │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 4.2 Dashboard Implementation Path

**Phase 1: Command-Line Andon (Immediate)**
```bash
# Proposed: `pnpm run andon:status`
# Outputs current dashboard to terminal with color coding
```

**Phase 2: Web Dashboard (Short-term)**
```
GET /api/andon/signals
GET /api/andon/signals/{signalGroup}
GET /api/andon/health-check

Returns JSON with all signal states for custom dashboards
```

**Phase 3: Integration (Medium-term)**
- Slack webhook integration for signal state changes
- PagerDuty integration for critical signals (RED)
- GitHub status checks for deployment readiness
- Custom metrics export to Prometheus/Grafana

---

## 5. Alerting Rules

### 5.1 Alert Channels by Severity

**RED Alert (Critical - Immediate Escalation)**
```
Channels:  Email + Slack @channel + PagerDuty incident
Signals:   FEATURE_HEALTH < 60
           CICD_BUILD failure (blocking pipeline)
           SECURITY critical/high vulnerabilities
           DEPLOYMENT_READINESS gate failure
Timeout:   5 minutes max response time
```

**YELLOW Alert (Warning - Team Review)**
```
Channels:  Slack #unrdf-alerts + Email
Signals:   FEATURE_HEALTH 60-79
           PERF_REGRESSION > 10%
           COVERAGE regression > 2%
           DEPENDENCY updates > 5
           SECURITY medium vulnerabilities
Timeout:   24 hours to review
```

**GREEN Status (Informational - Dashboard Only)**
```
Channels:  Dashboard only
Signals:   All systems normal
Update:    Real-time refresh
```

### 5.2 Alert Escalation Rules

```javascript
// Proposed: andon/alerting-rules.mjs
export const alertingRules = {
  RED: {
    maxRetries: 3,
    retryInterval: 5 * 60 * 1000,     // 5 minutes
    escalateTo: ['email', 'slack', 'pagerduty'],
    onCall: true,
    requiresAcknowledgment: true
  },
  YELLOW: {
    maxRetries: 1,
    retryInterval: 24 * 60 * 60 * 1000, // 24 hours
    escalateTo: ['slack', 'email'],
    onCall: false,
    requiresAcknowledgment: false
  },
  GREEN: {
    maxRetries: 0,
    escalateTo: [],
    onCall: false
  }
};
```

---

## 6. Andon Signal Implementation Roadmap

### Phase 1: Core Infrastructure (Weeks 1-2)
```
✓ Create /src/andon/ module directory
✓ Implement signal classes (FeatureHealth, CICD, Security, etc.)
✓ Add signal state management
✓ Integrate with existing OTEL validator
✓ Create signal aggregator
```

### Phase 2: Dashboard & Persistence (Weeks 3-4)
```
✓ Implement terminal-based dashboard (`pnpm run andon:status`)
✓ Create signal history tracking
✓ Build signal trend analysis
✓ Add signal export to JSON/CSV
```

### Phase 3: Alerting (Weeks 5-6)
```
✓ Slack webhook integration
✓ Email alerting
✓ PagerDuty integration
✓ Custom alert rules engine
```

### Phase 4: Web Dashboard (Weeks 7-8)
```
✓ REST API endpoints for signals
✓ Web UI dashboard
✓ Real-time WebSocket updates
✓ Signal filter/search
```

### Phase 5: Integration & Automation (Weeks 9-10)
```
✓ GitHub Actions integration
✓ Automated deployment gates
✓ SLA/SLO tracking
✓ Custom metrics API
```

---

## 7. File Structure

```
/src/andon/
├── index.mjs                          # Main export
├── signal-base.mjs                    # Base Signal class
├── signals/
│   ├── feature-health-signal.mjs       # OTEL-based validation
│   ├── cicd-signal.mjs                 # CI/CD pipeline status
│   ├── security-signal.mjs             # Security scan results
│   ├── performance-signal.mjs          # Latency trending
│   ├── dependency-signal.mjs           # Dependency health
│   ├── coverage-signal.mjs             # Test coverage
│   └── deployment-readiness-signal.mjs # Release gates
├── aggregator.mjs                     # Signal aggregation
├── dashboard/
│   ├── terminal-dashboard.mjs          # CLI dashboard
│   ├── formatter.mjs                   # Output formatting
│   └── colors.mjs                      # Terminal colors
├── alerting/
│   ├── alert-engine.mjs                # Alert rules & dispatch
│   ├── channels/
│   │   ├── slack-channel.mjs
│   │   ├── email-channel.mjs
│   │   ├── pagerduty-channel.mjs
│   │   └── github-channel.mjs
│   └── rules.mjs                       # Alert thresholds
├── storage/
│   ├── signal-history.mjs              # Persistent storage
│   └── analytics.mjs                   # Trend analysis
└── api/
    ├── signal-endpoints.mjs            # REST API
    └── websocket-server.mjs            # Real-time updates

/test/andon/
├── signal-*.test.mjs                   # Unit tests
├── integration/
│   └── dashboard.integration.test.mjs
└── fixtures/
    └── sample-signals.mjs              # Test data

/.github/workflows/
└── andon-status.yml                    # Andon status job
```

---

## 8. Metrics Export Format

### 8.1 Signal JSON Schema

```json
{
  "signal": "FEATURE_HEALTH_KNOWLEDGE_ENGINE_CORE",
  "state": "GREEN",
  "timestamp": "2024-11-21T15:30:45Z",
  "value": 87,
  "weight": 0.30,
  "metadata": {
    "spans": [
      {
        "name": "parse.turtle",
        "status": "ok",
        "duration": 23,
        "attributes": {
          "service.name": "unrdf",
          "parse.format": "turtle"
        }
      }
    ],
    "violations": [],
    "metrics": {
      "latency": 23,
      "errorRate": 0.0,
      "throughput": 1,
      "memoryUsage": 15728640
    }
  },
  "thresholds": {
    "green": 80,
    "yellow": 60,
    "red": 0
  },
  "trend": {
    "previous": 85,
    "change": 2,
    "direction": "up"
  }
}
```

### 8.2 Health Check Endpoint

```javascript
// GET /api/andon/health
export const healthCheckSchema = z.object({
  overall: z.enum(['GREEN', 'YELLOW', 'RED']),
  signals: z.array(SignalSchema),
  gates: z.object({
    canDeploy: z.boolean(),
    canCommit: z.boolean()
  }),
  timestamp: z.string().datetime(),
  uptime: z.number(),
  dependencies: z.object({
    database: z.enum(['connected', 'disconnected']),
    cache: z.enum(['connected', 'disconnected']),
    messageQueue: z.enum(['connected', 'disconnected'])
  })
});
```

---

## 9. Integration Points

### 9.1 With Existing OTEL Validator

```javascript
// Existing: /src/validation/otel-validator.mjs
// New integration:

import { FeatureHealthSignal } from '../andon/signals/feature-health-signal.mjs';

export class OTELValidator {
  // ... existing code ...

  async validateFeature(feature, config) {
    const result = await this.tracer.startActiveSpan(/* ... */);

    // NEW: Update Andon signal
    const signal = new FeatureHealthSignal(feature, weight);
    signal.updateScore(result.score);

    return { ...result, andonSignal: signal };
  }
}
```

### 9.2 With CI/CD Pipelines

```yaml
# .github/workflows/andon-status.yml (NEW)
name: Andon Status Update

on:
  workflow_run:
    workflows: [ci, security, benchmark]
    types: [completed]

jobs:
  update-andon:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Update Andon Signals
        run: |
          pnpm install
          pnpm run andon:update-signals \
            --pipeline=${{ github.workflow }} \
            --status=${{ job.status }}

      - name: Check Deployment Readiness
        run: pnpm run andon:check-readiness

      - name: Send Alerts if Needed
        if: failure()
        run: pnpm run andon:alert --severity=RED
```

---

## 10. Success Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| Signal Coverage | 95%+ of critical systems | Signal list vs system inventory |
| Alert Response Time | <5min for RED alerts | Alert log analysis |
| False Positive Rate | <5% | Alert audit over 30 days |
| Dashboard Availability | 99.9% uptime | Endpoint monitoring |
| MTTR Improvement | 50% reduction | Incident timeline analysis |
| Team Adoption | 100% daily usage | Dashboard access logs |

---

## 11. Example Andon Scenarios

### Scenario 1: Feature Validation Degradation
```
TRIGGER:  FEATURE_HEALTH_KNOWLEDGE_HOOKS_API drops from 90 to 55
STATE:    RED (critical)
ALERT:    Slack @devops, Email, PagerDuty incident
RESPONSE:
  1. Check latest commits (hook API changes)
  2. Review OTEL spans for missing/failing traces
  3. Run manual validation/debug
  4. Rollback if needed
  5. Resolve and re-validate
```

### Scenario 2: Security Vulnerability Discovered
```
TRIGGER:  SECURITY_DEPENDENCIES detects CVE in dependency
STATE:    RED (critical if high/critical severity)
ALERT:    Email + Slack #security-alerts
RESPONSE:
  1. Assess vulnerability impact
  2. Update dependency version
  3. Run security scans again
  4. Verify no regression
  5. Deploy patch
```

### Scenario 3: Performance Regression
```
TRIGGER:  PERF_REGRESSION_QUERY_SPARQL exceeds 20% baseline
STATE:    YELLOW (warning) then RED if >30%
ALERT:    Slack #performance-team
RESPONSE:
  1. Profile query execution
  2. Compare with previous commit
  3. Optimize query or indexes
  4. Re-benchmark to verify
  5. Add regression test
```

### Scenario 4: Deployment Gate
```
TRIGGER:  Release workflow attempts to deploy
STATE:    Checks all DEPLOYMENT_READINESS gates
ACTION:
  - ✓ Build: 2 artifacts verified
  - ✓ Tests: 87/100 score, all pass
  - ✓ Security: No critical vulns
  - ✓ Performance: No major regression
  - ✓ Docs: API docs generated
  - ✗ Changelog: Manual update needed
RESULT:   YELLOW - 5/6 gates passed, waiting for changelog
PROCEED:  After changelog update + re-validation
```

---

## 12. Future Enhancements

1. **SLA/SLO Tracking**: Auto-calculate service level agreements
2. **Predictive Alerts**: ML-based anomaly detection
3. **Custom Signal Types**: User-defined health indicators
4. **Mobile Andon**: Mobile app for on-the-go visibility
5. **Audit Trail**: Full Andon state history with annotations
6. **Post-mortems**: Automated incident documentation
7. **Team Notifications**: Per-team filtering and routing
8. **Andon Takt Time**: Measure time-to-detection and resolution

---

## Conclusion

This Andon signals design provides UNRDF with enterprise-grade visual management and alerting. By integrating with the existing OTEL validation framework and CI/CD pipelines, Andon creates a unified, real-time view of system health that enables rapid problem detection and escalation.

**Key Benefits**:
- Visibility: Know system health at a glance
- Responsiveness: Alerts ensure rapid response
- Accountability: Clear ownership and escalation
- Improvement: Trends guide optimization efforts
- Confidence: Deployment readiness gates prevent incidents

**Next Steps**:
1. Review and approve signal design
2. Implement Phase 1 infrastructure
3. Integrate with OTEL validator
4. Deploy terminal dashboard
5. Set up alerting channels
6. Train team on Andon usage
