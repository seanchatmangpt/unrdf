# SLA Implementation for JS→Erlang→JS Roundtrips

## Objective
Implement strict SLA tracking (<10ms latency, <0.1% error rate) for JS→Erlang→JS roundtrips with poka-yoke prevention of SLA violations.

## Scope
- Roundtrip measurement: JS calls Erlang (via AtomVM) → Erlang processes → Erlang calls back to JS (via bridge) → JS responds
- Metrics: Latency (end-to-end) + error rate
- Thresholds: <10ms latency, <0.1% error rate
- Poka-yoke: Prevent operations that would violate SLA

## Implementation Plan

### 1. Create Roundtrip Measurement Infrastructure

**File**: `packages/atomvm/src/roundtrip-sla.mjs` (new)
- Track roundtrip start time when JS calls Erlang
- Track roundtrip end time when JS receives response from Erlang
- Calculate latency (end - start)
- Track errors (failed roundtrips)
- Store metrics per roundtrip type (emit_event, register_hook, process_intent)

**Key functions**:
- `startRoundtrip(operationType, operationId)` - Start timing, returns roundtrip context
- `endRoundtrip(operationId, success)` - End timing, record metrics
- `getSLAStats(operationType)` - Get current SLA stats (latency, error rate, count)
- `checkSLACompliance(operationType)` - Check if current metrics meet SLA (<10ms, <0.1%)
- `canStartRoundtrip(operationType)` - Poka-yoke: Check if operation can start without violating SLA

**Data structure**:
```javascript
{
  roundtrips: Map<operationId, { startTime, operationType }>,
  stats: Map<operationType, { count, totalLatency, errorCount, lastLatency, lastError }>
}
```

### 2. Instrument Bridge Methods with Roundtrip Tracking

**File**: `packages/atomvm/playground/src/kgc4d-bridge.mjs`
- Import `roundtrip-sla.mjs`
- Wrap `emitEvent()`, `registerHook()`, `processIntent()` with roundtrip tracking
- Start roundtrip timer at method entry (when called from Erlang)
- End roundtrip timer when bridge method completes
- Record success/failure for error rate calculation
- Add OTEL span attributes:
  - `roundtrip.latency` - Roundtrip duration in ms
  - `roundtrip.sla_met` - Boolean indicating if SLA was met
  - `roundtrip.error_rate` - Current error rate for operation type
  - `roundtrip.operation_id` - Unique roundtrip identifier

**Changes**:
- Add `import { startRoundtrip, endRoundtrip, getSLAStats } from '../../src/roundtrip-sla.mjs'`
- Wrap each async method with roundtrip tracking
- Calculate and record latency
- Set OTEL span attributes

### 3. Instrument Bridge Interceptor for Erlang→JS Calls

**File**: `packages/atomvm/playground/src/bridge-interceptor.mjs`
- Track when Erlang calls JS (via `KGC4D_BRIDGE:` commands)
- Link to parent roundtrip if initiated from JS
- Track completion time when JS responds
- Record in roundtrip SLA tracker

**Changes**:
- Import `roundtrip-sla.mjs`
- In `handleBridgeCommand()`, start roundtrip when command received
- End roundtrip when bridge method completes
- Add OTEL span attributes for roundtrip metrics

### 4. Instrument AtomVM Runtime for JS→Erlang Calls

**File**: `packages/atomvm/src/atomvm-runtime.mjs`
- Track when JS calls Erlang (via `executeBeam()` or `runExample()`)
- Start roundtrip timer at execution start
- Link to bridge roundtrip when Erlang calls back

**File**: `packages/atomvm/src/node-runtime.mjs`
- Same as browser runtime - track JS→Erlang calls

**Changes**:
- Import `roundtrip-sla.mjs`
- In `executeBeam()` and `runExample()`, start roundtrip
- Pass roundtrip context to bridge interceptor
- End roundtrip when execution completes

### 5. Add Poka-Yoke SLA Enforcement

**File**: `packages/atomvm/src/roundtrip-sla.mjs`
- Before starting roundtrip, check if current error rate would exceed threshold
- If error rate > 0.1%, reject operation (prevent SLA violation)
- If average latency > 10ms, log warning but allow (latency is per-operation, not pre-checkable)
- Add validation functions:
  - `canStartRoundtrip(operationType)` - Check if operation can start without violating SLA
  - `validateRoundtripLatency(latency)` - Validate latency meets SLA (<10ms)
  - `validateErrorRate(errorRate)` - Validate error rate meets SLA (<0.1%)
  - `enforceSLA(operationType)` - Poka-yoke: Throw error if SLA would be violated

**Implementation**:
```javascript
function canStartRoundtrip(operationType) {
  const stats = getSLAStats(operationType);
  if (stats.count === 0) return true; // No history, allow
  const errorRate = stats.errorCount / stats.count;
  if (errorRate > 0.001) { // 0.1%
    throw new Error(`SLA violation prevented: Error rate ${(errorRate * 100).toFixed(2)}% exceeds 0.1% threshold`);
  }
  return true;
}
```

### 6. Integrate with OTEL Tracing

**File**: `packages/atomvm/playground/src/kgc4d-bridge.mjs`
- Add OTEL span attributes for roundtrip metrics:
  - `roundtrip.latency` - Roundtrip duration in ms
  - `roundtrip.sla_met` - Boolean (latency < 10ms && no error)
  - `roundtrip.error_rate` - Current error rate for operation type
  - `roundtrip.operation_id` - Unique roundtrip identifier
  - `roundtrip.operation_type` - Operation type (emit_event, register_hook, process_intent)

**File**: `packages/atomvm/playground/src/bridge-interceptor.mjs`
- Add OTEL span attributes for Erlang→JS portion of roundtrip
- Link spans to parent roundtrip span using `traceId` and `spanId`

### 7. Add SLA Validation to Validation Suite

**File**: `validation/atomvm-playground.mjs`
- Add SLA-specific validation rules
- Check that roundtrip latency < 10ms
- Check that error rate < 0.1%
- Add to `performanceThresholds`:
  - `maxRoundtripLatency: 10`
  - `maxRoundtripErrorRate: 0.001`

**File**: `packages/validation/src/otel-validator-core.mjs`
- Add `_validateRoundtripSLA()` method
- Check OTEL span attributes for roundtrip metrics
- Validate against strict thresholds (<10ms, <0.1%)
- Add violations if thresholds exceeded

### 8. Add SLA Monitoring and Reporting

**File**: `packages/atomvm/src/roundtrip-sla.mjs`
- Add `getSLAReport()` - Generate SLA compliance report
  - Per-operation-type stats
  - Overall compliance status
  - Violations list
- Add `resetSLAStats()` - Reset stats for testing
- Add `getViolations()` - Get list of SLA violations
- Add alerting when SLA thresholds exceeded (console.warn)

### 9. Update Documentation

**File**: `packages/atomvm/README.md`
- Document SLA requirements (<10ms latency, <0.1% error rate)
- Document roundtrip measurement methodology
- Document poka-yoke enforcement (operations rejected if error rate too high)

**File**: `docs/SLA-ROUNDTRIP.md` (new)
- Document SLA thresholds and rationale
- Document measurement methodology
- Document poka-yoke controls
- Document monitoring and alerting

## Implementation Details

### Roundtrip Measurement Flow

1. **JS → Erlang**: 
   - JS calls AtomVM runtime `executeBeam()` or `runExample()`
   - `startRoundtrip('execute_beam', operationId)` called
   - Roundtrip context stored with start time

2. **Erlang Processing**:
   - Erlang module executes
   - May call back to JS via `KGC4D_BRIDGE:` commands

3. **Erlang → JS**:
   - Bridge interceptor receives command
   - Links to parent roundtrip if exists
   - Bridge method executes (emitEvent, registerHook, processIntent)
   - Bridge method records its portion of roundtrip

4. **JS Response**:
   - Bridge method returns result
   - `endRoundtrip(operationId, success)` called
   - Calculate latency (endTime - startTime)
   - Record success/failure
   - Update stats: count++, totalLatency += latency, errorCount += (success ? 0 : 1)

### Poka-Yoke Enforcement

- **Pre-operation check**: Before starting roundtrip, check if error rate would exceed 0.1%
  - If yes, throw error: "SLA violation prevented: Error rate X% exceeds 0.1% threshold"
  - This prevents operations that would violate SLA
- **Post-operation validation**: After roundtrip, validate latency < 10ms
  - If exceeded, log warning but don't reject (may be transient)
  - Record in stats for monitoring
- **Rejection**: If pre-check fails, reject operation with clear error message
- **Warning**: If latency exceeds threshold, log warning but allow (may be transient)

### SLA Metrics Storage

- Use in-memory Map for real-time tracking
- Store per operation type (emit_event, register_hook, process_intent, execute_beam)
- Track per operation type:
  - `count` - Total roundtrips
  - `totalLatency` - Sum of all latencies (for average calculation)
  - `errorCount` - Number of failed roundtrips
  - `lastLatency` - Last roundtrip latency
  - `lastError` - Last error message (if any)
- Calculate:
  - `averageLatency = totalLatency / count`
  - `errorRate = errorCount / count`

### OTEL Integration

- Add span attributes to existing OTEL spans
- Use existing tracer infrastructure
- Link spans using trace context
- Export metrics via OTEL for external monitoring

## Testing

1. Test roundtrip measurement accuracy
   - Verify start/end times recorded correctly
   - Verify latency calculated correctly
2. Test error rate calculation
   - Verify error count increments on failure
   - Verify error rate calculated correctly
3. Test poka-yoke rejection when error rate too high
   - Simulate high error rate
   - Verify operation rejected
4. Test SLA validation in validation suite
   - Verify validation passes when metrics meet thresholds
   - Verify validation fails when thresholds exceeded
5. Test OTEL span attributes
   - Verify roundtrip metrics in spans
   - Verify SLA compliance flag in spans

## Success Criteria

- Roundtrip latency measured accurately (JS→Erlang→JS)
- Error rate calculated correctly (failures / total)
- Poka-yoke prevents operations when error rate > 0.1%
- SLA validation passes when metrics meet thresholds (<10ms, <0.1%)
- OTEL spans include roundtrip metrics
- Documentation complete
- All tests pass

## Files to Create/Modify

**New Files**:
- `packages/atomvm/src/roundtrip-sla.mjs` - Core SLA tracking and enforcement
- `docs/SLA-ROUNDTRIP.md` - SLA documentation

**Modified Files**:
- `packages/atomvm/playground/src/kgc4d-bridge.mjs` - Add roundtrip tracking
- `packages/atomvm/playground/src/bridge-interceptor.mjs` - Add roundtrip tracking
- `packages/atomvm/src/atomvm-runtime.mjs` - Track JS→Erlang calls
- `packages/atomvm/src/node-runtime.mjs` - Track JS→Erlang calls
- `validation/atomvm-playground.mjs` - Add SLA validation thresholds
- `packages/validation/src/otel-validator-core.mjs` - Add SLA validation method
- `packages/atomvm/README.md` - Document SLA requirements

