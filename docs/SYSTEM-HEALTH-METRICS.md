# System Health Metrics

This document describes the four core quantities used to measure system health in the Governed Ontology Substrate. These metrics provide continuous measurement of system state, enabling proactive governance and anomaly detection.

## Overview

The measurement system tracks four fundamental quantities:

| Metric | Symbol | What It Measures | Unit |
|--------|--------|------------------|------|
| Dimension | D_t | Representable state space | bits |
| Total Correlation | TC | Coupling between partitions | bits |
| Transfer Entropy | TE | Causal influence paths | bits |
| Channel Capacity | C_t | Admissible change rate | bits/epoch |

Together, these metrics form a complete picture of system health, enabling operators to detect problems before they manifest as failures.

## D_t: Representable Dimension

### What It Measures

D_t quantifies the **expressive capacity** of the system - how many distinct states (quads) can be represented under current constraints. It measures the "size" of the admissible state space in information-theoretic terms.

### Formula

```
D_t = log_2(|S| * |P| * |O| * |G|) * constraint_factor
```

Where:
- `|S|` = number of unique subjects
- `|P|` = number of unique predicates
- `|O|` = number of unique objects
- `|G|` = number of graphs
- `constraint_factor` = reduction due to namespace restrictions, read-only partitions, etc.

### Interpretation

| D_t Value | Interpretation |
|-----------|---------------|
| > 50 bits | High expressiveness - system can represent many distinct states |
| 20-50 bits | Moderate expressiveness - adequate for typical workloads |
| 10-20 bits | Limited expressiveness - may constrain complex schemas |
| < 10 bits | Low expressiveness - consider relaxing constraints |

### Healthy Ranges

- **Minimum**: 10 bits (warning if below)
- **Typical**: 20-40 bits
- **Maximum utilization**: 90% (critical if exceeded)

### What Anomalies Mean

| Anomaly | Possible Causes | Action |
|---------|-----------------|--------|
| Sudden drop | New constraints added, partition locked | Review recent policy changes |
| Gradual decline | Accumulating restrictions | Audit constraint necessity |
| Spike | Constraints removed, schema expanded | Verify intentional |
| High utilization | Approaching capacity limits | Plan for scaling |

### Example Usage

```javascript
import { DimensionComputer } from './measurement/dimension-computer.mjs';

const computer = new DimensionComputer();
const dimension = await computer.computeSystemDimension(universe);

console.log(`System dimension: ${dimension.systemDimension.toFixed(2)} bits`);
console.log(`Utilization: ${(dimension.utilizationRatio * 100).toFixed(1)}%`);

// Check trend
const trend = computer.getDimensionTrend(10);
console.log(`Trend: ${trend.trend}`);
```

## TC: Total Correlation

### What It Measures

TC quantifies the **coupling** between partitions based on their admission sequences. High TC means changes in one partition tend to accompany changes in others - they are interdependent. Low TC means partitions evolve independently.

### Formula

```
TC = sum(H(X_i)) - H(X_1, X_2, ..., X_n)
```

Where:
- `H(X_i)` = entropy of admission sequence for partition i
- `H(X_1,...,X_n)` = joint entropy of all admission sequences

### Interpretation

| TC Value | Normalized TC | Interpretation |
|----------|---------------|----------------|
| 0 | 0 | Partitions are completely independent |
| Low | 0-0.3 | Loose coupling - good modularity |
| Medium | 0.3-0.6 | Moderate coupling - some interdependence |
| High | 0.6-0.8 | High coupling - changes cascade |
| Very High | > 0.8 | Tight coupling - partitions move together |

### Healthy Ranges

- **Target**: Normalized TC < 0.5
- **Warning**: Normalized TC > 0.7
- **Critical**: Normalized TC > 0.9

### What Anomalies Mean

| Anomaly | Possible Causes | Action |
|---------|-----------------|--------|
| TC spike | Coordinated changes, deployment | If unexpected, investigate dependency |
| Sustained high TC | Architectural coupling | Refactor for better isolation |
| Sudden drop | Partition isolation | Verify intentional |
| Volatile TC | Inconsistent change patterns | Standardize deployment practices |

### Example Usage

```javascript
import { CorrelationComputer } from './measurement/correlation-computer.mjs';

const computer = new CorrelationComputer();

// Record admission events
for (const event of admissionHistory) {
  computer.recordAdmission(event);
}

const tc = computer.computeTotalCorrelation();

console.log(`Total Correlation: ${tc.totalCorrelation.toFixed(3)}`);
console.log(`Normalized TC: ${tc.normalizedTC.toFixed(3)}`);

// Check for highly coupled pairs
for (const pair of tc.pairwiseMI) {
  if (pair.normalizedMI > 0.5) {
    console.log(`High coupling: ${pair.partition1} <-> ${pair.partition2}`);
  }
}
```

## TE: Transfer Entropy

### What It Measures

TE quantifies **causal influence** between partitions - whether changes in one partition predict changes in another, beyond what the target's own history would predict. Unlike mutual information, TE is directional and captures causality.

### Formula

```
TE(X -> Y) = H(Y(t) | Y(t-1)) - H(Y(t) | Y(t-1), X(t-1))
```

Where:
- `X(t-1)` = past state of source partition
- `Y(t)` = current state of target partition
- `Y(t-1)` = past state of target partition

This measures how much additional information about Y's future is gained by knowing X's past.

### Interpretation

| TE Value | Interpretation |
|----------|----------------|
| 0 | No causal influence from source to target |
| 0-0.2 | Weak causal influence |
| 0.2-0.5 | Moderate causal influence |
| > 0.5 | Strong causal influence |

### Key Properties

- **Asymmetric**: TE(X -> Y) != TE(Y -> X) in general
- **Directional**: Indicates information flow direction
- **Conditional**: Accounts for target's own predictability
- **Significance tested**: Uses shuffle test for p-value

### Node Roles in Causal Graph

| Role | Characteristics | Implication |
|------|-----------------|-------------|
| Driver | High out-degree, low in-degree | Changes here propagate outward |
| Receiver | High in-degree, low out-degree | Affected by other partitions |
| Mediator | Both in and out edges | Bridges between partitions |
| Isolated | No significant edges | Independent evolution |
| Balanced | Similar in/out | Bidirectional influence |

### Healthy Ranges

- **Per-edge TE**: < 0.5 (moderate influence acceptable)
- **Total system TE**: Depends on partition count
- **Significance threshold**: p < 0.05

### What Anomalies Mean

| Anomaly | Possible Causes | Action |
|---------|-----------------|--------|
| New causal edge | New dependency introduced | Verify intentional |
| Edge disappeared | Decoupling or data issue | Investigate root cause |
| TE spike on edge | Increased dependency | Monitor for cascade risk |
| Oscillating TE | Unstable relationship | Investigate intermittent coupling |

### Example Usage

```javascript
import { TransferEntropyComputer } from './measurement/transfer-entropy-computer.mjs';

const computer = new TransferEntropyComputer();

// Record state observations
for (const event of stateHistory) {
  computer.recordState(event);
}

// Compute single direction
const te = computer.computeTransferEntropy('Industrial', 'Corporate');
console.log(`TE(Industrial -> Corporate): ${te.transferEntropy.toFixed(3)}`);
console.log(`Significant: ${te.isSignificant}`);

// Compute full causal graph
const graph = computer.computeCausalGraph();
for (const edge of graph.edges) {
  console.log(`${edge.source} -> ${edge.target}: TE=${edge.te.toFixed(3)}`);
}
```

## C_t: Channel Capacity

### What It Measures

C_t quantifies the **change throughput** of the system - how much change (in bits) can flow through the admission channel per epoch. It measures the entropy of admitted deltas, indicating the diversity and rate of changes.

### Formula

```
C_t = H(admitted_deltas) = -sum(p(delta) * log_2(p(delta)))
```

Where the probability distribution is over delta sizes/types within an epoch.

### Interpretation

| C_t Value | Interpretation |
|-----------|----------------|
| 0 | No changes admitted |
| Low (< 2) | Few, uniform changes |
| Medium (2-4) | Moderate change diversity |
| High (> 4) | High change diversity/activity |

### Related Metrics

- **Admission Rate**: Fraction of deltas admitted (allowed/total)
- **Throughput**: Deltas per second, quads per second
- **Utilization**: Actual capacity vs. theoretical max

### Healthy Ranges

- **Admission rate**: > 50% (warning if below)
- **Throughput**: Context-dependent, compare to baseline
- **Capacity utilization**: 20-80% (underutilized or overloaded outside)

### What Anomalies Mean

| Anomaly | Possible Causes | Action |
|---------|-----------------|--------|
| Capacity spike | Burst of changes, deployment | Normal if planned |
| Capacity drop | Stricter policies, fewer changes | Verify intentional |
| Low admission rate | Overly strict invariants | Review admission policies |
| High admission rate + low capacity | Uniform small changes | May indicate batching opportunity |
| Exhaustion detected | Approaching limits | Scale or optimize |

### Example Usage

```javascript
import { CapacityComputer } from './measurement/capacity-computer.mjs';

const computer = new CapacityComputer();

// Record deltas
for (const delta of deltaHistory) {
  computer.recordDelta(delta);
}

const capacity = computer.computeSystemCapacity();

console.log(`System capacity: ${capacity.systemCapacity.toFixed(3)} bits`);
console.log(`Admission rate: ${(capacity.admissionRate * 100).toFixed(1)}%`);
console.log(`Throughput: ${capacity.throughput.deltasPerSecond.toFixed(2)} deltas/s`);

// Check rates over different windows
const rates = computer.computeAdmissionRates();
for (const [window, rate] of Object.entries(rates.rates)) {
  console.log(`${window}: ${(rate.admissionRate * 100).toFixed(1)}% admission`);
}
```

## Dimension Certificates

Dimension certificates are cryptographically signed attestations of system health at a specific epoch. They contain:

1. All four metrics (D_t, TC, TE, C_t)
2. Trend analysis for each metric
3. Health status (healthy/warning/critical)
4. Health score (0-100)
5. Certificate hash (BLAKE3)
6. Chain to previous certificate

### Certificate Structure

```javascript
{
  version: "1.0.0",
  epoch: "tau_2024_01_15_1430_123",
  measurements: {
    dimension: { systemDimension, utilizationRatio, ... },
    correlation: { totalCorrelation, normalizedTC, ... },
    transferEntropy: { causalEdges, totalTE },
    capacity: { systemCapacity, admissionRate, throughput }
  },
  trends: {
    dimension: "stable",
    correlation: "decreasing",
    transferEntropy: "stable",
    capacity: "increasing"
  },
  health: {
    status: "healthy",
    score: 85,
    issues: []
  },
  certificateHash: "abc123...",
  previousCertificateHash: "xyz789..."
}
```

### Use Cases

- **Audit Trail**: Immutable record of system health over time
- **Compliance**: Attestation for regulatory requirements
- **Debugging**: Historical state for incident investigation
- **Comparison**: Week-over-week, month-over-month analysis

### Example Usage

```javascript
import { CertificateGenerator } from './measurement/certificate-generator.mjs';

const generator = new CertificateGenerator();

// Generate certificate from current measurements
const certificate = await generator.generateCertificate({
  dimension: dimensionResult,
  correlation: correlationResult,
  transferEntropy: teResult,
  capacity: capacityResult
});

// Verify integrity
const verification = await generator.verifyCertificate(certificate);
console.log(`Certificate valid: ${verification.valid}`);

// Compare to previous
const comparison = generator.compareCertificates(previousCert, certificate);
console.log(`Health score change: ${comparison.healthChange.scoreChange}`);

// Export for storage
const turtle = generator.exportCertificate(certificate, 'turtle');
```

## Health Dashboard

The Health Dashboard provides real-time monitoring and alerting for all four metrics.

### Features

- Periodic measurement at configurable intervals
- Time-series storage for trend analysis
- Configurable alerting thresholds
- Multiple output formats (console, JSON, silent)
- Baseline comparison
- Event-based notification

### Starting the Dashboard

```javascript
import { HealthDashboard } from './measurement/health-dashboard.mjs';

const dashboard = new HealthDashboard({
  updateInterval: 60000, // 1 minute
  outputFormat: 'console',
  enableAlerts: true,
  alertConfig: {
    dimension: { minValue: 15, maxUtilization: 85 },
    correlation: { maxNormalizedTC: 0.7 },
    capacity: { minAdmissionRate: 0.6 }
  }
});

// Start monitoring
dashboard.start(universe);

// Listen for alerts
dashboard.on('alert', (alert) => {
  console.error(`ALERT: ${alert.message}`);
  notifyOperations(alert);
});

// Listen for metrics updates
dashboard.on('metrics', (metrics) => {
  sendToTelemetry(metrics);
});
```

### Dashboard Output

```
============================================================
  SYSTEM HEALTH DASHBOARD - tau_2024_01_15_1430_123
============================================================

  Status: HEALTHY (Score: 85/100)

  DIMENSION (D_t)
    System Dimension: 32.50 bits
    Utilization: 15.2%
    Trend: stable

  CORRELATION (TC)
    Total Correlation: 0.350
    Normalized TC: 0.420
    Trend: stable

  TRANSFER ENTROPY (TE)
    Total TE: 0.650
    Causal Edges: 3
    Trend: stable

  CAPACITY (C_t)
    System Capacity: 3.200
    Admission Rate: 78.5%
    Throughput: 2.50 deltas/s
    Trend: increasing

============================================================
```

## Acting on Anomalies

### Decision Matrix

| Metric | Anomaly | Severity | Immediate Action | Long-term Action |
|--------|---------|----------|------------------|------------------|
| D_t | < 10 | Warning | Review constraints | Relax if blocking |
| D_t | Util > 90% | Critical | Stop non-essential changes | Scale or optimize |
| TC | > 0.8 | Warning | Monitor deployments | Refactor dependencies |
| TE | New high edge | Warning | Identify root cause | Consider decoupling |
| C_t | Rate < 50% | Warning | Review rejections | Adjust policies |
| C_t | Throughput = 0 | Critical | Check system health | Investigate blockage |

### Escalation Path

1. **Automatic**: Dashboard detects anomaly, emits alert
2. **Monitoring**: Alert captured in observability system
3. **Triage**: On-call reviews dashboard and recent changes
4. **Response**: Based on decision matrix above
5. **Resolution**: Root cause addressed, metrics return to normal
6. **Postmortem**: Update thresholds if needed, document lesson

## Best Practices

### Monitoring Setup

1. Set baseline during stable operation
2. Configure alerts for 2-sigma deviations initially
3. Tune thresholds based on false positive rate
4. Compare week-over-week trends regularly

### Capacity Planning

1. Track D_t utilization trend
2. Alert at 70%, plan at 80%, critical at 90%
3. Monitor C_t throughput for scaling decisions
4. Use forecasting for proactive scaling

### Dependency Management

1. Monitor TC for unexpected coupling
2. Use TE to identify causal chains
3. Design for low coupling (TC < 0.5)
4. Isolate high-change partitions

### Certificate Retention

1. Store certificates for audit retention period
2. Verify certificate chain integrity periodically
3. Use certificates for compliance reporting
4. Compare certificates for change impact analysis

## Summary

The four metrics provide complementary views of system health:

| Metric | Question Answered |
|--------|-------------------|
| D_t | How much can we express? |
| TC | How coupled are our partitions? |
| TE | What causes what? |
| C_t | How much can we change? |

Together, they enable:

- **Proactive governance**: Detect issues before they cascade
- **Informed decisions**: Quantify impact of policy changes
- **Audit compliance**: Immutable attestation of system state
- **Operational excellence**: Real-time visibility into system health

By continuously measuring these quantities, operators can maintain a healthy, well-governed system that evolves predictably and sustainably.
