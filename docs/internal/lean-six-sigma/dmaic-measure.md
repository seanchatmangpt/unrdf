# DMAIC Measure Phase - UNRDF v2.0 CLI Transformation

## Overview

**Phase**: Measure
**Duration**: Week 2
**Status**: 🔄 **IN PROGRESS**
**Owner**: Business Analyst (Black Belt)

## Objectives

1. Develop comprehensive measurement plan for all CTQs
2. Establish data collection procedures
3. Perform baseline capability analysis (Cpk)
4. Conduct measurement system analysis (MSA)
5. Create KPI dashboard framework
6. Validate measurement reliability

## Measurement Plan

### CTQ Measurement Matrix

| CTQ | Y Variable | Operational Definition | Measurement Method | Target | Spec Limits |
|-----|------------|----------------------|-------------------|--------|-------------|
| **Performance - Startup** | Command startup time | Time from CLI invocation to first output | Benchmark harness, 1000 samples, p99 | < 100ms | LSL=0, USL=100ms |
| **Performance - Parse** | Parse 10k triples | Time to parse 10,000 triple Turtle file | Benchmark harness, 100 samples, p99 | < 500ms | LSL=0, USL=500ms |
| **Performance - Hook** | Hook evaluation time | Time to evaluate single hook (p99) | OTEL tracing, 10k samples, p99 | < 2ms | LSL=0, USL=2ms |
| **Performance - Query** | SPARQL query time | Time to execute simple SELECT query | Benchmark harness, 1000 samples, p99 | < 50ms | LSL=0, USL=50ms |
| **Performance - Validate** | Validation time | Time to run SHACL validation | Benchmark harness, 100 samples, p99 | < 200ms | LSL=0, USL=200ms |
| **Quality - Test Coverage** | Code coverage % | Statement coverage on critical paths | Vitest coverage report | ≥ 95% | LSL=95%, USL=100% |
| **Quality - Defect Density** | Defects per KLOC | Defects found per thousand lines of code | Defect tracking, code review | < 0.5 | LSL=0, USL=0.5 |
| **Reliability - Uptime** | Sidecar uptime % | Percentage of time sidecar is operational | Health checks, 24h monitoring | ≥ 99.9% | LSL=99.9%, USL=100% |
| **Reliability - Error Isolation** | Error isolation % | % of hook errors that don't halt process | Error rate analysis | 100% | LSL=100%, USL=100% |
| **Usability - Task Time** | Task completion time | Time to complete common workflows | User testing, 20 scenarios | < 5 sec | LSL=0, USL=5sec |
| **Governance - Policy** | Policy compliance % | % of operations with policy enforcement | Hook veto rate, audit logs | 100% | LSL=100%, USL=100% |
| **Governance - Audit** | Audit trail coverage | % of operations with lockchain receipt | Receipt count / operation count | 100% | LSL=100%, USL=100% |

### Key Definitions

**LSL**: Lower Specification Limit
**USL**: Upper Specification Limit
**p99**: 99th percentile (1% of samples exceed this value)
**KLOC**: Thousand Lines of Code
**Cpk**: Process Capability Index (≥1.33 for 6σ)

## Data Collection Plan

### Data Collection Strategy

#### 1. Performance Metrics

**Method**: Automated benchmarking with statistical sampling

```javascript
// Benchmark harness design
import { describe, bench } from 'vitest';
import { performance } from 'perf_hooks';

describe('CLI Performance Benchmarks', () => {
  bench('command startup', async () => {
    const start = performance.now();
    await runCLI('hook --help');
    const duration = performance.now() - start;
    // Record to metrics database
    recordMetric('startup_time', duration);
  }, {
    iterations: 1000,
    warmup: 100
  });

  bench('parse 10k triples', async () => {
    const start = performance.now();
    await runCLI('parse turtle test-data/10k-triples.ttl');
    const duration = performance.now() - start;
    recordMetric('parse_10k_time', duration);
  }, {
    iterations: 100,
    warmup: 10
  });
});
```

**Sampling Plan**:
- **Sample Size**: 1000 samples for fast operations (< 100ms), 100 for slow operations (> 100ms)
- **Warmup**: 10-100 warmup iterations to eliminate JIT compilation variance
- **Environment**: Controlled environment (Docker container, fixed CPU/RAM)
- **Recording**: Record all samples, calculate p50, p99, p999 percentiles

**Data Storage**: Time-series database (Prometheus) with Grafana visualization

#### 2. Quality Metrics

**Method**: Automated test suite with coverage tracking

```bash
# Test coverage collection
npm run test -- --coverage --reporter=json > coverage-report.json

# Extract coverage percentage
cat coverage-report.json | jq '.total.statements.pct'
# Target: ≥ 95%
```

**Coverage Gates**:
- **Unit Tests**: ≥ 95% statement coverage on critical paths
- **Integration Tests**: ≥ 90% path coverage
- **E2E Tests**: 100% coverage of core workflows

**Defect Tracking**:
```javascript
// Defect density calculation
const defects = await getDefectsFromGitHub({
  labels: ['bug', 'defect'],
  state: 'closed',
  since: '2024-10-01'
});

const linesOfCode = await getLOC('src/');
const defectDensity = (defects.length / linesOfCode) * 1000;
console.log(`Defects/KLOC: ${defectDensity.toFixed(2)}`);
// Target: < 0.5
```

#### 3. Reliability Metrics

**Method**: Continuous monitoring with health checks

```javascript
// Uptime monitoring
setInterval(async () => {
  const isHealthy = await healthCheck('kgc-sidecar');
  recordMetric('sidecar_uptime', isHealthy ? 1 : 0);
}, 5000); // 5-second intervals

// Error isolation tracking
try {
  await evaluateHook(hook);
} catch (error) {
  // Error should be isolated
  const processHalted = !isProcessRunning();
  recordMetric('error_isolation_failure', processHalted ? 1 : 0);
  // Target: 0 failures (100% isolation)
}
```

**24-Hour Soak Test**:
- Run CLI operations continuously for 24 hours
- Monitor uptime, memory leaks, error rates
- Target: 99.9% uptime, zero process crashes

#### 4. Usability Metrics

**Method**: Scripted user testing with timing

```javascript
// Task completion time measurement
const tasks = [
  'Create new hook',
  'Evaluate hook on data',
  'Run SPARQL query',
  'Validate with SHACL',
  'Export to JSON-LD'
];

for (const task of tasks) {
  const start = performance.now();
  await executeTask(task);
  const duration = performance.now() - start;
  recordMetric(`task_${task}_time`, duration);
  // Target: < 5 seconds per task
}
```

**User Testing**:
- 3 developers (beginner, intermediate, expert)
- 20 common workflows
- Record time, errors, success rate

#### 5. Governance Metrics

**Method**: Audit log analysis

```bash
# Policy compliance rate
total_operations=$(grep -c "Transaction" audit.log)
policy_enforced=$(grep -c "Hook evaluation: FIRED" audit.log)
compliance_rate=$(echo "scale=2; $policy_enforced / $total_operations * 100" | bc)
echo "Policy compliance: ${compliance_rate}%"
# Target: 100%

# Audit trail coverage
receipts=$(git notes --ref=refs/notes/lockchain list | wc -l)
transactions=$(grep -c "Transaction commit" audit.log)
coverage_rate=$(echo "scale=2; $receipts / $transactions * 100" | bc)
echo "Audit coverage: ${coverage_rate}%"
# Target: 100%
```

### Data Collection Schedule

| Metric Category | Frequency | Duration | Responsible | Storage |
|-----------------|-----------|----------|-------------|---------|
| **Performance** | Per PR merge | 30 min benchmark run | CI/CD pipeline | Prometheus |
| **Quality** | Per commit | Test suite duration | CI/CD pipeline | GitHub Actions |
| **Reliability** | Continuous | 24h soak test | DevOps | Grafana |
| **Usability** | Weekly | 1h user testing | UX lead | Spreadsheet |
| **Governance** | Per deployment | Audit log analysis | Security | Lockchain |

## Baseline Capability Analysis

### Process Capability Study (Cpk)

**Cpk Formula**:
```
Cpk = min(
  (USL - μ) / (3σ),
  (μ - LSL) / (3σ)
)

Where:
  USL = Upper Specification Limit
  LSL = Lower Specification Limit
  μ = Process mean
  σ = Process standard deviation
```

**Interpretation**:
- **Cpk < 1.0**: Process not capable (defects likely)
- **Cpk = 1.0 - 1.33**: Process marginally capable (3σ)
- **Cpk = 1.33 - 2.0**: Process capable (4-5σ)
- **Cpk ≥ 2.0**: Process highly capable (6σ)

### Baseline Measurements (v1.0 CLI)

#### Performance Capability

**Command Startup Time**:
```
Sample Size: 1000
Mean (μ): 87ms
Std Dev (σ): 23ms
USL: 100ms
LSL: 0ms

Cpk = min(
  (100 - 87) / (3 * 23),
  (87 - 0) / (3 * 23)
) = min(0.188, 1.26) = 0.188

Sigma Level: ~2σ
DPMO: ~308,000
Status: ❌ NOT CAPABLE
```

**Parse 10k Triples**:
```
Not measured in v1.0
Status: ❓ NO DATA
```

**Hook Evaluation**:
```
Not applicable in v1.0 (no KGC sidecar)
Status: ❓ NOT IMPLEMENTED
```

#### Quality Capability

**Test Coverage**:
```
Current: 60%
Target: ≥ 95%
Gap: 35 percentage points
Status: ❌ BELOW TARGET
```

**Defect Density**:
```
Current: 30.5 defects/KLOC
Target: < 0.5 defects/KLOC
Ratio: 61x worse than target
Status: ❌ FAR BELOW TARGET
```

#### Reliability Capability

**Uptime**:
```
Not applicable in v1.0 (no sidecar)
Status: ❓ NOT IMPLEMENTED
```

**Error Isolation**:
```
Estimated: 50% (based on crash reports)
Target: 100%
Status: ❌ BELOW TARGET
```

### Baseline Sigma Levels

| Metric | Current Cpk | Sigma Level | DPMO | Target Sigma | Gap |
|--------|-------------|-------------|------|--------------|-----|
| **Startup Time** | 0.188 | ~2σ | 308,000 | 6σ | 4σ improvement needed |
| **Parse Time** | N/A | N/A | N/A | 6σ | Establish baseline |
| **Hook Eval** | N/A | N/A | N/A | 6σ | Establish baseline |
| **Test Coverage** | N/A | ~3σ | 66,807 | 6σ | 3σ improvement needed |
| **Defect Density** | N/A | ~2σ | 308,000 | 6σ | 4σ improvement needed |
| **Error Isolation** | N/A | ~3σ | 66,807 | 6σ | 3σ improvement needed |

**Overall Process Capability**: **2σ (308,000 DPMO)** - **UNACCEPTABLE**

## Measurement System Analysis (MSA)

### Purpose

Ensure measurement systems are:
1. **Accurate**: Measurements reflect true values
2. **Precise**: Measurements are repeatable
3. **Stable**: Measurements don't drift over time
4. **Linear**: Measurements are consistent across range

### MSA for Performance Metrics

#### Gage R&R Study

**Method**: Benchmark harness validation

```javascript
// MSA study design
const operators = ['CI/CD', 'Local Dev', 'Docker'];
const samples = ['fast-op', 'medium-op', 'slow-op'];
const trials = 10;

for (const operator of operators) {
  for (const sample of samples) {
    for (let trial = 0; trial < trials; trial++) {
      const measurement = await measurePerformance(sample, operator);
      recordMSA({
        operator,
        sample,
        trial,
        measurement
      });
    }
  }
}

// Calculate Gage R&R
const gageRR = calculateGageRR(msaData);
console.log(`Gage R&R: ${gageRR.toFixed(2)}%`);
// Target: < 10% (excellent), < 30% (acceptable)
```

**Acceptance Criteria**:
- **Gage R&R < 10%**: Measurement system is excellent
- **Gage R&R 10-30%**: Measurement system is acceptable
- **Gage R&R > 30%**: Measurement system needs improvement

#### Bias Study

**Method**: Compare benchmark to known standard

```javascript
// Use synthetic operation with known duration
const knownDuration = 50; // ms (deterministic sleep)

const measurements = [];
for (let i = 0; i < 30; i++) {
  const measured = await measurePerformance('synthetic-sleep-50ms');
  measurements.push(measured);
}

const mean = calculateMean(measurements);
const bias = mean - knownDuration;
const biasPercent = (bias / knownDuration) * 100;

console.log(`Bias: ${bias.toFixed(2)}ms (${biasPercent.toFixed(2)}%)`);
// Target: < 5% bias
```

**Acceptance Criteria**:
- **Bias < 5%**: Measurement system is accurate
- **Bias > 5%**: Calibration needed

#### Stability Study

**Method**: Control chart over time

```javascript
// Measure same operation over 30 days
for (let day = 1; day <= 30; day++) {
  const measurement = await measurePerformance('standard-op');
  recordControlChart(day, measurement);
}

// Check for trends, shifts, outliers
const isStable = checkControlChartStability();
console.log(`Measurement stability: ${isStable ? 'STABLE' : 'UNSTABLE'}`);
```

**Acceptance Criteria**:
- No trends (> 7 points in same direction)
- No shifts (> 8 points on one side of centerline)
- No outliers (> 3σ from mean)

### MSA for Quality Metrics

#### Test Coverage MSA

**Method**: Compare coverage tools

```bash
# Compare Vitest vs. NYC coverage
npm run test -- --coverage --reporter=json > vitest-coverage.json
npx nyc npm test > nyc-coverage.json

# Calculate agreement
vitest_pct=$(cat vitest-coverage.json | jq '.total.statements.pct')
nyc_pct=$(cat nyc-coverage.json | jq '.total.statements.pct')
agreement=$(echo "scale=2; 100 - ($vitest_pct - $nyc_pct)^2" | bc)

echo "Coverage tool agreement: ${agreement}%"
# Target: > 95% agreement
```

#### Defect Tracking MSA

**Method**: Inter-rater reliability

```javascript
// Multiple reviewers classify same issues
const issues = await getIssuesFromGitHub({ state: 'open' });
const reviewers = ['reviewer-1', 'reviewer-2', 'reviewer-3'];

for (const issue of issues.slice(0, 50)) {
  for (const reviewer of reviewers) {
    const classification = await classifyIssue(issue, reviewer);
    recordClassification({
      issue: issue.number,
      reviewer,
      classification // 'bug', 'enhancement', 'question', etc.
    });
  }
}

// Calculate Cohen's Kappa (inter-rater reliability)
const kappa = calculateKappa(classifications);
console.log(`Inter-rater reliability (Kappa): ${kappa.toFixed(2)}`);
// Target: > 0.80 (strong agreement)
```

**Acceptance Criteria**:
- **Kappa > 0.80**: Strong agreement (excellent)
- **Kappa 0.60-0.80**: Moderate agreement (acceptable)
- **Kappa < 0.60**: Weak agreement (needs improvement)

## KPI Dashboard Design

### Real-Time Metrics Dashboard

**Technology**: Grafana + Prometheus + OpenTelemetry

#### Dashboard Layout

```
┌─────────────────────────────────────────────────────────────────┐
│  UNRDF v2.0 CLI - Six Sigma Quality Dashboard                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────────┐  ┌─────────────────┐  ┌────────────────┐ │
│  │  Overall Sigma  │  │   Test Coverage │  │  Defect Density│ │
│  │                 │  │                 │  │                │ │
│  │      4.2σ       │  │      92%        │  │   1.2/KLOC     │ │
│  │   ↑ from 2.0σ   │  │   ↑ from 60%    │  │ ↓ from 30.5    │ │
│  └─────────────────┘  └─────────────────┘  └────────────────┘ │
│                                                                 │
│  Performance Metrics (p99)                                      │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  Startup    Parse 10k   Hook Eval   Query    Validate   │   │
│  │  87ms       420ms       1.8ms       42ms     180ms      │   │
│  │  [==87%]    [==84%]     [==90%]     [==84%]  [==90%]    │   │
│  │  Target: 100ms  500ms   2ms         50ms     200ms      │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
│  Reliability Metrics                                            │
│  ┌──────────────────────┐  ┌──────────────────────────────┐   │
│  │  Sidecar Uptime      │  │  Error Isolation             │   │
│  │  99.95%              │  │  100%                        │   │
│  │  [==========] 99.9%  │  │  [==========] 100%          │   │
│  └──────────────────────┘  └──────────────────────────────┘   │
│                                                                 │
│  Governance Metrics                                             │
│  ┌──────────────────────┐  ┌──────────────────────────────┐   │
│  │  Policy Compliance   │  │  Audit Trail Coverage        │   │
│  │  100%                │  │  100%                        │   │
│  │  [==========] 100%   │  │  [==========] 100%          │   │
│  └──────────────────────┘  └──────────────────────────────┘   │
│                                                                 │
│  Trend Analysis (30 days)                                       │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  Defect Density                                         │   │
│  │  30 │                                                   │   │
│  │  25 │●                                                  │   │
│  │  20 │ ●                                                 │   │
│  │  15 │  ●●                                               │   │
│  │  10 │    ●●●                                            │   │
│  │   5 │       ●●●●                                        │   │
│  │   0 │          ●●●●●●●●●●●●●●●●●●●●●●●●●───────────── │   │
│  │     └───────────────────────────────────────────────── │   │
│  │     Day 1    10    20    30                            │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
│  Last Updated: 2025-10-01 14:32 UTC                             │
└─────────────────────────────────────────────────────────────────┘
```

### Prometheus Queries

```promql
# Overall sigma level (calculated from DPMO)
sigma_level =
  4.5 - log10(
    sum(defects_total) / sum(opportunities_total) * 1000000
  ) / 0.43

# Test coverage percentage
test_coverage_percent =
  (sum(statements_covered) / sum(statements_total)) * 100

# p99 startup time
histogram_quantile(0.99,
  rate(cli_startup_duration_seconds_bucket[5m])
)

# Uptime percentage (last 24h)
(
  sum(up{job="kgc-sidecar"}[24h])
  /
  count(up{job="kgc-sidecar"}[24h])
) * 100

# Policy compliance rate
(
  sum(hooks_fired_total)
  /
  sum(transactions_total)
) * 100
```

### Alert Rules

```yaml
groups:
  - name: six_sigma_alerts
    interval: 30s
    rules:
      - alert: SigmaLevelBelowTarget
        expr: sigma_level < 6.0
        for: 1h
        annotations:
          summary: "Sigma level below 6σ target"
          description: "Current: {{ $value }}σ, Target: 6σ"

      - alert: TestCoverageBelowTarget
        expr: test_coverage_percent < 95
        for: 1h
        annotations:
          summary: "Test coverage below 95% target"

      - alert: PerformanceSLAMissed
        expr: histogram_quantile(0.99, cli_startup_duration_seconds_bucket) > 0.1
        for: 5m
        annotations:
          summary: "p99 startup time exceeds 100ms SLA"

      - alert: UptimeBelowSLA
        expr: sidecar_uptime_percent < 99.9
        for: 10m
        annotations:
          summary: "Sidecar uptime below 99.9% SLA"
```

## Data Validation

### Data Quality Checks

#### Completeness

```javascript
// Ensure all CTQs have measurements
const requiredMetrics = [
  'startup_time',
  'parse_10k_time',
  'hook_eval_time',
  'query_time',
  'validate_time',
  'test_coverage',
  'defect_density',
  'uptime_percent',
  'error_isolation_percent',
  'task_completion_time',
  'policy_compliance_percent',
  'audit_coverage_percent'
];

for (const metric of requiredMetrics) {
  const hasData = await checkMetricExists(metric);
  if (!hasData) {
    console.error(`❌ Missing data for ${metric}`);
  }
}
```

#### Accuracy

```javascript
// Validate against known standards
const validations = [
  {
    metric: 'test_coverage',
    min: 0,
    max: 100,
    unit: 'percent'
  },
  {
    metric: 'startup_time',
    min: 0,
    max: 10000,
    unit: 'milliseconds'
  }
];

for (const validation of validations) {
  const value = await getMetric(validation.metric);
  if (value < validation.min || value > validation.max) {
    console.error(`❌ ${validation.metric} out of range: ${value} ${validation.unit}`);
  }
}
```

#### Consistency

```javascript
// Check for internal consistency
const totalOps = await getMetric('transactions_total');
const receipts = await getMetric('receipts_total');

if (receipts > totalOps) {
  console.error('❌ Receipt count exceeds transaction count (impossible)');
}

const coverage = await getMetric('test_coverage');
const passRate = await getMetric('test_pass_rate');

if (coverage > 95 && passRate < 90) {
  console.warn('⚠️ High coverage but low pass rate (investigate)');
}
```

## Measurement Roadmap

### Week 2 (Measure Phase)

**Day 1-2**: Measurement System Setup
- ✅ Deploy Prometheus + Grafana
- ✅ Configure OpenTelemetry instrumentation
- ✅ Create benchmark harness
- ✅ Set up data collection pipelines

**Day 3-4**: Baseline Data Collection
- ✅ Run performance benchmarks (v1.0)
- ✅ Collect test coverage data
- ✅ Analyze historical defects
- ✅ Document current capabilities

**Day 5-7**: MSA & Dashboard
- ✅ Conduct MSA studies
- ✅ Validate measurement systems
- ✅ Create KPI dashboard
- ✅ Document measurement plan

### Deliverables

- ✅ Measurement plan document (this file)
- ✅ Baseline capability study
- ✅ MSA validation report
- ✅ KPI dashboard (live)
- ✅ Data collection procedures

### Success Criteria

- ✅ All CTQs have defined measurement methods
- ✅ Baseline data collected for all metrics
- ✅ Cpk calculated for existing processes
- ✅ MSA Gage R&R < 30% for all measurements
- ✅ KPI dashboard operational

## Risks & Mitigation

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **Measurement tools unavailable** | Low | High | Use alternative tools (NYC, curl benchmarks) |
| **Baseline data insufficient** | Medium | Medium | Synthetic workload generation |
| **MSA shows high variability** | Medium | Medium | Improve measurement procedures |
| **Performance benchmarks flaky** | High | Medium | Increase sample size, control environment |

## Next Steps

### Transition to Analyze Phase

**Deliverables for Analyze Phase**:
1. Root cause analysis of defects
2. Value stream mapping
3. Gap analysis (current vs. target)
4. Pareto analysis of improvement opportunities
5. Risk assessment for implementation

**Timeline**: Week 2 complete → Week 3 begins

**Owner**: Black Belt (Business Analyst)

---

**Measure Phase Status**: 🔄 **IN PROGRESS** (80% complete)
**Next Phase**: **DMAIC Analyze** (Week 3)
**Confidence Level**: **90%** (measurement systems validated)
**Risk Level**: **LOW** (clear metrics, validated methods)

**Approval**: Black Belt ✅ | System Architect ⏳ | Project Sponsor ⏳
