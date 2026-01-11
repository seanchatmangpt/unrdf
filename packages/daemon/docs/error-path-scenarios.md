# Error Path Validation Scenarios

## Format Guide

Each error path scenario follows this structure:

```
JTBD #N: [Failure Mode Title]

Error Scenario:
[What breaks and why]

Expected Error Handling:
- [System should do X]
- [System should do Y]
- [System should NOT do Z]

How to Trigger Error:
[Concrete steps to reproduce]

Proof of Correct Handling:
[Observable evidence]

Test Assertion:
[Testable condition]
```

---

## JTBD #1.2: Concurrent Job Timeout

**Failure Mode**: One job exceeds timeout threshold while 99 others continue executing

### Error Scenario:
100 concurrent jobs are scheduled. Job #50 is configured to run for 600ms, but the timeout threshold is 500ms. Other 99 jobs complete quickly (10-100ms each). System must detect timeout for job #50 while allowing other 99 jobs to complete normally.

### Expected Error Handling:
- Timeout error is caught for job #50 (not uncaught exception)
- Other 99 jobs continue executing independently
- Error context available: operationId='job-50', errorType='timeout', duration=600ms
- Metrics updated: totalOperations=100, failedOperations=1, successfulOperations=99
- System remains operational: daemon.isRunning=true

### How to Trigger Error:
1. Create daemon with concurrency=100
2. Schedule 100 jobs with handlers that complete in 10-100ms, except job-50 (600ms)
3. Set timeout threshold to 500ms
4. Execute all 100 jobs via `daemon.execute(jobId)` in parallel
5. Observe job-50 fails with timeout, others succeed
6. Verify daemon.getHealth().isRunning=true

### Proof of Correct Handling:
1. Try-catch captures timeout error (not thrown uncaught)
2. Completed operations metric shows: 99 success + 1 timeout
3. Error message contains: "timeout"
4. Health check shows: {isRunning: true, activeOperations: 0, completedOperations: 100}
5. New operations can be scheduled and executed after timeout

### Test Assertion:
```javascript
expect(results.success).toBe(99);         // 99 jobs succeeded
expect(results.timeout).toBe(1);          // 1 job timed out
expect(daemon.isRunning).toBe(true);      // System still alive
expect(metrics.failedOperations).toBe(1); // Metrics accurate
```

---

## JTBD #2.2: Receipt Replication Failure

**Failure Mode**: Receipt verification fails (hash mismatch, storage unavailable, checksum error)

### Error Scenario:
Operation completes successfully and result is recorded. However, when attempting to verify or replicate the receipt (for audit purposes), the verification fails. The receipt hash doesn't match expected value, or receipt data is corrupted. System must detect this error, log it, but NOT lose the operation record or prevent future operations.

### Expected Error Handling:
- Receipt verification error is caught (not uncaught exception)
- Original operation still recorded in completedOperations cache
- Error context logged: receipt_id, expected_hash, actual_hash, operationId, timestamp
- Audit trail remains complete and accessible
- System continues accepting new operations

### How to Trigger Error:
1. Execute 5 operations that generate receipts
2. Simulate receipt verification by comparing hashes
3. For operation #2, introduce a mismatch: stored_hash='abc123' vs computed_hash='xyz789'
4. Attempt to verify receipt for operation #2
5. Observe error is caught and logged
6. Verify operations #1,3,4,5 are still in cache
7. Schedule and execute new operations

### Proof of Correct Handling:
1. Receipt error thrown and caught (not silent)
2. completedOperations.get('op-2') returns the operation record
3. Error logged with diagnostic context
4. health.completedOperations shows 5 (not reduced)
5. New operations execute successfully after error
6. Metrics still accurate: totalOperations=6, successfulOperations=6

### Test Assertion:
```javascript
expect(result).toBeDefined();             // Operation completed despite receipt error
expect(health.completedOperations).toBe(5); // All operations still in cache
expect(daemon.isRunning).toBe(true);      // System operational
expect(newOpResult.status).toBe('ok');    // New operations work
```

---

## JTBD #3.1: Primary Node Crash During Execution

**Failure Mode**: Primary node process crashes unexpectedly mid-operation (simulated via handler rejection with crash context)

### Error Scenario:
An operation is executing when the primary node crashes (SIGTERM, out of memory, or segfault). The executing operation fails with an error like "Node crashed: SIGTERM" or "Unexpected shutdown". System must detect this crash, log it with diagnostic context, prevent cascading failures, and allow recovery.

### Expected Error Handling:
- Crash is detected and caught (not uncaught exception)
- Error context logged: nodeId, operationId, crash_timestamp, crash_reason, activeOperations_count
- Affected operation marked as failed in metrics
- Recovery mechanisms triggered: restart policy, health check
- Subsequent operations can execute after crash
- System remains operational: daemon.isRunning=true

### How to Trigger Error:
1. Create daemon and start it
2. Schedule 10 operations with handlers that complete quickly, except operation #5
3. Operation #5 handler rejects with error message containing "crashed"
4. Execute all 10 operations
5. Observe operation #5 fails while others complete
6. Check error logs for crash context
7. Schedule and execute recovery operation
8. Verify it succeeds

### Proof of Correct Handling:
1. Error thrown and caught (try-catch succeeds)
2. Crash context logged: {level: 'error', message: contains 'crashed'}
3. Metrics show: failedOperations=1, totalOperations=10
4. Recovery operation executes: {status: 'recovered'}
5. Health check returns: {isRunning: true, completedOperations: 10}
6. Error message preserved in metrics

### Test Assertion:
```javascript
expect(operationFailed).toBe(true);       // Crash detected
expect(crashLogs.length).toBeGreaterThan(0); // Logged
expect(metrics.failedOperations).toBe(1); // Marked as failure
expect(recoveryResult.status).toBe('recovered'); // Recovery works
expect(daemon.isRunning).toBe(true);      // System stable
```

---

## JTBD #4.2: Invalid Operation (Constraint Violation)

**Failure Mode**: Operation submitted with missing required fields or violating schema constraints

### Error Scenario:
Application attempts to schedule an operation that violates the Daemon schema:
- Missing required 'id' field
- Handler is not a function (e.g., string, null, undefined)
- Operation is null or undefined

System must reject the invalid operation synchronously (at schedule time), not allow it to be queued, and provide a descriptive error message. The rejection must happen BEFORE the operation is added to the queue.

### Expected Error Handling:
- Validation error thrown synchronously at schedule() call
- Error message is descriptive: "Invalid operation: must have id and handler function"
- Invalid operation is NOT added to queue
- operationQueue length unchanged
- operations.size unchanged
- Valid operations can still be scheduled after validation error

### How to Trigger Error:
1. Create daemon
2. Attempt to schedule operation without 'id': `daemon.schedule({name: 'op', handler: fn})`
3. Catch error and verify message
4. Check daemon.operationQueue.length is unchanged
5. Schedule valid operation: `daemon.schedule({id: 'valid-op', handler: fn})`
6. Verify it's in the queue

### Proof of Correct Handling:
1. Error thrown synchronously (not silent)
2. Error message contains: "Invalid operation"
3. Queue inspection shows no invalid operation: operationQueue.length = initialLength
4. operations.size = 0 after error
5. Valid operation queued successfully after error
6. Valid operation can be executed

### Test Assertion:
```javascript
expect(() => daemon.schedule(invalidOp)).toThrow(
  'Invalid operation: must have id and handler function'
);
expect(daemon.operationQueue.length).toBe(initialLength); // Queue unchanged
expect(daemon.operations.size).toBe(0); // Not added
// After scheduling valid operation:
expect(daemon.operations.has('valid-op')).toBe(true);
```

---

## JTBD #5.2: Memory Pressure Handling

**Failure Mode**: System executes many operations causing completed operations cache to exceed max size (1000 entries)

### Error Scenario:
1500 operations are executed and complete successfully. The LRU cache is configured with a maximum size of 1000 entries. As operations complete and are recorded, the cache grows. When the 1001st operation completes, the LRU eviction policy removes the oldest (first) entry to maintain the max size. System must gracefully evict old entries, continue accepting new operations, maintain accurate metrics, and not crash.

### Expected Error Handling:
- LRU cache size bounded at 1000 (never exceeds max)
- Old entries automatically evicted (oldest operation removed when new one arrives beyond max)
- New operations continue to be accepted and executed
- Metrics remain accurate despite eviction (total count, success rate)
- System remains operational: no out-of-memory crash
- Health check reflects cache size: health.completedOperations <= 1000

### How to Trigger Error:
1. Create daemon with default LRU cache (1000 max)
2. Schedule 1500 operations with handlers that complete quickly
3. Execute all 1500 operations in a loop
4. After each execution, observe cache size
5. Verify cache size never exceeds 1000
6. Schedule and execute new operations (100 more)
7. Verify they complete successfully despite pressure

### Proof of Correct Handling:
1. Cache size inspection: health.completedOperations <= 1000
2. New operations complete: result.status = 'ok'
3. No out-of-memory error thrown
4. Metrics are accurate: totalOperations=1500, successRate=100%
5. System remains operational: daemon.isRunning=true
6. Health check succeeds: health.isRunning=true

### Test Assertion:
```javascript
expect(health.completedOperations).toBeLessThanOrEqual(1000); // Bounded
expect(daemon.isRunning).toBe(true); // No crash
expect(results.every(r => r.status === 'ok')).toBe(true); // New ops work
expect(metrics.successRate).toBe(100); // Metrics accurate
```

---

## JTBD #6.2: Version Mismatch During Rollback

**Failure Mode**: Receipt version (v1.0.0) does not match current system version (v2.0.0) during rollback attempt

### Error Scenario:
A deployment generated and recorded a receipt for policy enforcement code using generator version v1.0.0. Six months later, the system has been upgraded to version v2.0.0. An incident requires rolling back to the previously recorded policy. When the team attempts to rollback using the v1.0.0 receipt against the v2.0.0 system, the version mismatch is detected. System must prevent the rollback, log the mismatch with context, and suggest checking compatibility.

### Expected Error Handling:
- Version mismatch detected (not silent)
- Rollback prevented: canRollback=false
- Error context logged: {receipt_version: v1.0.0, current_version: v2.0.0, operationId, timestamp}
- Warning message includes both versions for diagnostic
- System state unchanged (no partial or incorrect rollback)
- Suggestion logged: "Check compatibility between v1.0.0 and v2.0.0"

### How to Trigger Error:
1. Create operation receipt with version='v1.0.0'
2. Set current system version to 'v2.0.0'
3. Attempt to verify compatibility: `receipt.version === currentVersion`
4. Observe mismatch (false)
5. Log warning with context
6. Set canRollback=false
7. Prevent rollback execution
8. Execute new operations to verify system stable

### Proof of Correct Handling:
1. Version comparison returns mismatch: versionMismatch=true
2. Warning logged with context: {level: 'warn', contains: 'v1.0.0' and 'v2.0.0'}
3. Rollback not executed: canRollback=false
4. System state unchanged: metrics.totalOperations unchanged
5. New operations execute: result.ok=true
6. Health check succeeds: isRunning=true
7. Compatibility suggestion logged

### Test Assertion:
```javascript
expect(receipt.version).toBe('v1.0.0');
expect(currentVersion).toBe('v2.0.0');
expect(versionMismatch).toBe(true); // Mismatch detected
expect(canRollback).toBe(false);    // Rollback prevented
expect(daemon.isRunning).toBe(true); // System stable
expect(result1.ok).toBe(true);       // New ops work
expect(result2.ok).toBe(true);
```

---

## Summary Table

| JTBD | Failure | Error Type | Detection | Context | Recovery |
|------|---------|-----------|-----------|---------|----------|
| 1.2 | Timeout | Handler error | Via timeout mechanism | operationId, duration | Continue other jobs |
| 2.2 | Receipt verify | Storage/hash error | Verify receipt data | receipt_id, hash mismatch | Keep operation record |
| 3.1 | Crash | Process termination | Handler rejection | nodeId, operationId | Restart, new ops |
| 4.2 | Invalid op | Validation error | At schedule() time | Missing field | Reject, queue unchanged |
| 5.2 | Memory | Cache overflow | LRU eviction | Cache size | Evict oldest entry |
| 6.2 | Version | Mismatch | Version comparison | Both versions | Prevent rollback |

---

## Running Individual Scenarios

```bash
# Run all error path scenarios
pnpm -C packages/daemon test error-path-validation

# Run specific JTBD scenario
pnpm -C packages/daemon test -- --grep "JTBD #1.2"
pnpm -C packages/daemon test -- --grep "JTBD #2.2"
pnpm -C packages/daemon test -- --grep "JTBD #3.1"
pnpm -C packages/daemon test -- --grep "JTBD #4.2"
pnpm -C packages/daemon test -- --grep "JTBD #5.2"
pnpm -C packages/daemon test -- --grep "JTBD #6.2"
```
