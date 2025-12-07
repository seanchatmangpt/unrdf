# SLA for JS→Erlang→JS Roundtrips

## Overview

Strict Service Level Agreement (SLA) for JavaScript→Erlang→JavaScript roundtrips with poka-yoke enforcement to prevent violations.

## SLA Thresholds

**Strict Requirements**:
- **Latency**: <10ms per roundtrip (end-to-end)
- **Error Rate**: <0.1% (1 error per 1000 roundtrips)

## Roundtrip Definition

A roundtrip consists of:
1. **JS → Erlang**: JavaScript calls Erlang process (via AtomVM runtime)
2. **Erlang Processing**: Erlang module executes
3. **Erlang → JS**: Erlang calls back to JavaScript (via bridge interceptor)
4. **JS Response**: JavaScript bridge method completes and returns

**Total Latency**: Time from JS call start to JS response completion.

## Measurement Methodology

### Roundtrip Tracking

Roundtrips are tracked using `roundtrip-sla.mjs`:

- **Start**: `startRoundtrip(operationType)` called when JS initiates call
- **End**: `endRoundtrip(operationId, success)` called when JS receives response
- **Latency**: Calculated as `endTime - startTime` using `performance.now()`

### Operation Types

- `emit_event`: Erlang emits event to KGC-4D
- `register_hook`: Erlang registers knowledge hook
- `process_intent`: Erlang processes intent (Λ → A)
- `execute_beam`: JS executes Erlang module

### Metrics Storage

Per-operation-type statistics:
- `count`: Total roundtrips
- `totalLatency`: Sum of all latencies
- `errorCount`: Number of failed roundtrips
- `lastLatency`: Last roundtrip latency
- `lastError`: Last error message (if any)

Calculated metrics:
- `averageLatency = totalLatency / count`
- `errorRate = errorCount / count`

## Poka-Yoke Enforcement

### Pre-Operation Check

Before starting a roundtrip, `canStartRoundtrip()` checks:
- If error rate > 0.1%, **reject operation** with error:
  ```
  SLA violation prevented: Error rate X% exceeds 0.1% threshold
  ```

This prevents operations that would violate SLA.

### Post-Operation Validation

After roundtrip completes:
- If latency > 10ms, **log warning** but allow (may be transient)
- Record metrics for monitoring

### Error Handling

- Failed roundtrips increment `errorCount`
- Error rate calculated as `errorCount / count`
- If error rate exceeds threshold, future operations rejected

## OTEL Integration

Roundtrip metrics are added to OTEL spans:

- `roundtrip.operation_id`: Unique roundtrip identifier
- `roundtrip.operation_type`: Operation type
- `roundtrip.latency`: Roundtrip duration in ms
- `roundtrip.sla_met`: Boolean (latency < 10ms && no error)
- `roundtrip.error_rate`: Current error rate for operation type

## Validation

SLA validation is integrated into the validation suite:

- Checks roundtrip latency < 10ms
- Checks error rate < 0.1%
- Adds violations if thresholds exceeded
- Included in overall validation score (20% weight)

## Monitoring

### SLA Report

Use `getSLAReport()` to get compliance report:

```javascript
import { getSLAReport } from '@unrdf/atomvm/src/roundtrip-sla.mjs';

const report = getSLAReport();
console.log(report.overall.overallCompliant); // true/false
console.log(report.violations); // List of violations
```

### Per-Operation Stats

Use `getSLAStats(operationType)` to get stats:

```javascript
import { getSLAStats, OPERATION_TYPES } from '@unrdf/atomvm/src/roundtrip-sla.mjs';

const stats = getSLAStats(OPERATION_TYPES.EMIT_EVENT);
console.log(stats.averageLatency); // Average latency
console.log(stats.errorRate); // Error rate
console.log(stats.slaCompliant); // Compliance status
```

## Controls

### Code Review

- Verify roundtrip tracking is added to all bridge methods
- Verify OTEL span attributes include roundtrip metrics
- Verify poka-yoke enforcement prevents SLA violations

### Automated Checks

- Validation suite checks SLA compliance
- OTEL spans include roundtrip metrics
- Poka-yoke rejects operations when error rate too high

### Monitoring

- Track SLA compliance over time
- Alert when violations occur
- Review trends weekly

## Examples

### Successful Roundtrip

```javascript
// JS calls Erlang
const runtime = new AtomVMRuntime('my-module');
await runtime.loadWASM();
const result = await runtime.executeBeam('my-module.avm');

// Roundtrip tracked automatically:
// - Start: When executeBeam() called
// - End: When execution completes
// - Latency: <10ms (SLA met)
// - Error rate: <0.1% (SLA met)
```

### SLA Violation Prevention

```javascript
// If error rate > 0.1%, operation rejected:
try {
  await bridge.emitEvent('TEST', {});
} catch (error) {
  // Error: "SLA violation prevented: Error rate 0.15% exceeds 0.1% threshold"
}
```

## Rationale

**Why <10ms?**
- Erlang processes are lightweight
- Bridge operations are synchronous
- KGC-4D operations are fast
- Sub-10ms ensures real-time responsiveness

**Why <0.1%?**
- Production systems require high reliability
- 0.1% = 1 error per 1000 operations
- Allows for transient errors while maintaining quality
- Strict enough to catch systemic issues

## Poka-Yoke Benefits

- **Prevents violations**: Operations rejected before SLA violated
- **Fail fast**: Errors caught immediately, not after accumulation
- **Clear feedback**: Error messages explain why operation rejected
- **Type safety**: Invalid states unrepresentable

