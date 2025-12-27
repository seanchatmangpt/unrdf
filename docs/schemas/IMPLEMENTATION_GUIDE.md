# SPARC Pseudocode: Observation Schemas - Implementation Guide

## Overview

This guide provides practical examples for implementing and using OTEL Observation Schemas across the entire system. It covers:

1. **Schema Instantiation** - Creating observations
2. **Validation Patterns** - Validating data
3. **Error Handling** - Recovering from validation failures
4. **Domain-Specific Workflows** - Domain patterns
5. **Capability & Constraint Derivation** - Extracting system properties
6. **Performance Optimization** - Validation at scale

---

## Part 1: Schema Instantiation

### Basic Observation Creation

```javascript
// ============================================================================
// ALGORITHM: CreateRuntimeObservation
// ============================================================================

// PSEUDOCODE:
// ALGORITHM: CreateObservation
// INPUT: agentId (string), domain (string), output (object)
// OUTPUT: observation (Observation)
//
// BEGIN
//     // Phase 1: Capture environment
//     id ← GenerateUUID()
//     timestamp_ns ← GetCurrentTimeNanoseconds()
//
//     // Phase 2: Determine method and guard
//     method ← ResolveMethodForDomain(domain)
//     guard ← { type: "allow" }
//
//     // Phase 3: Compute hash
//     hash ← BLAKE3(JSON.stringify({id, agentId, domain, timestamp_ns, method, output}))
//
//     // Phase 4: Assemble observation
//     observation ← {
//         id, agentId, domain, timestamp_ns, method,
//         input: RedactSecrets(capturedInput),
//         output,
//         hash,
//         guard,
//         receipt: null
//     }
//
//     RETURN observation
// END

// JAVASCRIPT IMPLEMENTATION:
import { v4 as uuidv4 } from 'uuid';
import { createBlake3 } from 'blake3';
import { validateObservation } from './zod-observation-schemas.mjs';

function createRuntimeObservation(agentId, output) {
  const now = BigInt(Date.now());
  const nowNs = (now * 1000000n).toString();

  const observation = {
    id: uuidv4(),
    agentId,
    domain: 'runtime',
    timestamp_ns: nowNs,
    method: 'runtime.check',
    input: {
      // Capture minimal input for reproducibility
      checkTime: new Date().toISOString(),
      agentVersion: '1.0.0'
    },
    output: {
      nodeVersion: process.version.slice(1), // Remove 'v' prefix
      jsEngine: 'v8',
      wasm: typeof WebAssembly !== 'undefined',
      workers: os.cpus().length,
      timersResolution: 1, // Typical: 1ms on Node.js
      icu: false // Detect from Node.js build
    },
    hash: '', // Will compute
    guard: { type: 'allow' },
    receipt: null
  };

  // Compute BLAKE3 hash
  const hashInput = JSON.stringify({
    id: observation.id,
    agentId: observation.agentId,
    domain: observation.domain,
    timestamp_ns: observation.timestamp_ns,
    method: observation.method,
    output: observation.output
  });

  observation.hash = createBlake3()
    .update(hashInput)
    .digest('hex');

  return observation;
}

// ============================================================================
// PATTERN: Domain Factory Functions
// ============================================================================

// Create specialized functions for each domain to ensure consistency

const ObservationFactories = {
  runtime: (agentId, nodeVersion, workers) => ({
    id: uuidv4(),
    agentId,
    domain: 'runtime',
    timestamp_ns: (BigInt(Date.now()) * 1000000n).toString(),
    method: 'runtime.check',
    input: { count: 1 },
    output: {
      nodeVersion,
      jsEngine: 'v8',
      wasm: true,
      workers,
      timersResolution: 100000,
      icu: true
    },
    hash: computeHash({ domain: 'runtime', nodeVersion, workers }),
    guard: { type: 'allow' },
    receipt: null
  }),

  filesystem: (agentId, root, fileCount) => ({
    id: uuidv4(),
    agentId,
    domain: 'fs',
    timestamp_ns: (BigInt(Date.now()) * 1000000n).toString(),
    method: 'fs.check',
    input: { path: root },
    output: {
      root,
      maxPathLength: 255,
      fileCount,
      symlinkBehavior: 'followed',
      writeTest: true
    },
    hash: computeHash({ domain: 'fs', root, fileCount }),
    guard: { type: 'allow' },
    receipt: null
  }),

  performance: (agentId, domain, throughput, p50, p99, variance) => ({
    id: uuidv4(),
    agentId,
    domain: 'perf',
    timestamp_ns: (BigInt(Date.now()) * 1000000n).toString(),
    method: 'perf.measure',
    input: { domain, duration: 1000 },
    output: {
      domain,
      throughput,
      latency_p50: p50,
      latency_p99: p99,
      variance
    },
    hash: computeHash({ domain: 'perf', throughput, p50, p99 }),
    guard: { type: 'allow' },
    receipt: null
  })
};
```

### Creating Denied Observations

```javascript
// ============================================================================
// ALGORITHM: CreateDeniedObservation
// ============================================================================

// PSEUDOCODE:
// ALGORITHM: CreateDeniedObservation
// INPUT: agentId (string), domain (string), reason (string)
// OUTPUT: observation (Observation with guard.type = "deny")
//
// BEGIN
//     observation ← CreateObservation(agentId, domain, {})
//     observation.guard ← {
//         type: "deny",
//         reason: reason
//     }
//     RETURN observation
// END

function createDeniedObservation(agentId, domain, method, reason) {
  const now = BigInt(Date.now());
  const nowNs = (now * 1000000n).toString();

  const observation = {
    id: uuidv4(),
    agentId,
    domain,
    timestamp_ns: nowNs,
    method,
    input: {},
    output: {}, // Sparse for denied observations
    hash: '', // Compute
    guard: {
      type: 'deny',
      reason // REQUIRED for deny guards
    },
    receipt: null
  };

  // Compute hash even for denied observations
  observation.hash = computeHash(observation);

  return observation;
}

// Example: Create denied network observation
const deniedNetObs = createDeniedObservation(
  'agent-2',
  'net',
  'net.resolve',
  'DNS resolution not available in restricted sandbox'
);

// Validation will pass - guard.type = "deny" is valid
const result = validateObservation(deniedNetObs);
console.assert(result.success, 'Denied observations are valid');
console.assert(result.data.guard.type === 'deny', 'Guard is deny');
```

---

## Part 2: Validation Patterns

### Single Observation Validation

```javascript
// ============================================================================
// ALGORITHM: ValidateObservationWithRetry
// ============================================================================

// PSEUDOCODE:
// ALGORITHM: ValidateObservationWithRetry
// INPUT: observation (object), maxRetries (integer)
// OUTPUT: validatedObservation (Observation) or error
//
// BEGIN
//     FOR i = 1 TO maxRetries DO
//         result ← ValidateObservation(observation)
//         IF result.success THEN
//             RETURN result.data
//         END IF
//
//         // Check if error is retryable
//         IF NOT IsRetryableError(result.error) THEN
//             RETURN result.error
//         END IF
//
//         // Log and wait before retry
//         Log(format("Validation attempt %d failed: %s", i, result.error.message))
//         Sleep(100 * i) // Exponential backoff
//     END FOR
//
//     RETURN ValidationError("Max retries exceeded")
// END

function validateObservationWithRetry(observation, maxRetries = 3) {
  let lastError = null;

  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      const result = validateObservation(observation);

      if (result.success) {
        console.log(`✅ Validation succeeded on attempt ${attempt}`);
        return result.data;
      }

      lastError = result.error;

      // Check if error is retryable (would only retry transient errors)
      if (isRetryableError(result.error)) {
        console.warn(
          `⚠️  Attempt ${attempt} failed: ${result.error.message}. Retrying...`
        );
        // In real code, would sleep here
      } else {
        console.error(
          `❌ Non-retryable error: ${result.error.message}`
        );
        throw result.error;
      }
    } catch (err) {
      lastError = err;

      if (attempt === maxRetries) {
        throw new Error(
          `Validation failed after ${maxRetries} attempts: ${lastError.message}`
        );
      }
    }
  }

  throw lastError;
}

function isRetryableError(error) {
  // In practice, only certain errors are retryable
  // Most schema validation errors are permanent
  return error.code === 'TRANSIENT_ERROR';
}

// Usage
const obs = createObservation(...);
try {
  const validated = validateObservationWithRetry(obs, 3);
  console.log('Observation validated:', validated.id);
} catch (err) {
  console.error('Validation failed:', err.message);
}
```

### Batch Validation with Progress

```javascript
// ============================================================================
// ALGORITHM: ValidateObservationBatchWithProgress
// ============================================================================

// PSEUDOCODE:
// ALGORITHM: ValidateObservationBatchWithProgress
// INPUT: observations (array of objects), batchSize (integer)
// OUTPUT: {
//     passed: Observation[],
//     failed: {observation, error}[],
//     summary: {total, passCount, failCount, passRate}
// }
//
// BEGIN
//     passed ← []
//     failed ← []
//
//     FOR i = 0 TO Length(observations) BY batchSize DO
//         batch ← observations.slice(i, i + batchSize)
//         results ← ValidateObservationBatch(batch)
//
//         FOR EACH result IN results DO
//             IF result.success THEN
//                 passed.append(result.data)
//             ELSE
//                 failed.append({
//                     observation: batch[index],
//                     error: result.error
//                 })
//             END IF
//         END FOR
//
//         // Report progress
//         progress ← i / Length(observations) * 100
//         Log(format("Progress: %d%%", progress))
//     END FOR
//
//     RETURN {
//         passed,
//         failed,
//         summary: {
//             total: Length(observations),
//             passCount: Length(passed),
//             failCount: Length(failed),
//             passRate: Length(passed) / Length(observations)
//         }
//     }
// END

function validateObservationBatchWithProgress(
  observations,
  batchSize = 100,
  onProgress = null
) {
  const passed = [];
  const failed = [];

  const totalBatches = Math.ceil(observations.length / batchSize);

  for (let batchIdx = 0; batchIdx < totalBatches; batchIdx++) {
    const start = batchIdx * batchSize;
    const end = Math.min(start + batchSize, observations.length);
    const batch = observations.slice(start, end);

    const { results } = validateObservationBatch(batch);

    batch.forEach((obs, idx) => {
      const result = results[idx];
      if (result.success) {
        passed.push(result.data);
      } else {
        failed.push({
          observation: obs,
          error: result.error
        });
      }
    });

    // Report progress
    const progress = ((batchIdx + 1) / totalBatches) * 100;
    if (onProgress) {
      onProgress({
        batchIdx: batchIdx + 1,
        totalBatches,
        progress,
        passCount: passed.length,
        failCount: failed.length
      });
    }

    console.log(
      `✓ Progress: ${progress.toFixed(1)}% (${batchIdx + 1}/${totalBatches})`
    );
  }

  const total = observations.length;
  const passCount = passed.length;
  const failCount = failed.length;
  const passRate = passCount / total;

  return {
    passed,
    failed,
    summary: {
      total,
      passCount,
      failCount,
      passRate: parseFloat((passRate * 100).toFixed(2))
    }
  };
}

// Usage
const allObservations = [/* ... many observations ... */];

console.log(`Validating ${allObservations.length} observations...`);
const result = validateObservationBatchWithProgress(
  allObservations,
  100,
  (progress) => {
    console.log(`  ${progress.passCount}/${progress.total} passed`);
  }
);

console.log('Validation complete:');
console.log(`  Passed: ${result.summary.passCount}`);
console.log(`  Failed: ${result.summary.failCount}`);
console.log(`  Pass Rate: ${result.summary.passRate}%`);

if (result.failed.length > 0) {
  console.log('\nFirst failures:');
  result.failed.slice(0, 5).forEach(({ error }) => {
    console.log(`  - ${error.message} (path: ${error.path.join('.')})`);
  });
}
```

---

## Part 3: Error Handling

### Validation Error Processing

```javascript
// ============================================================================
// ALGORITHM: ProcessValidationError
// ============================================================================

// PSEUDOCODE:
// ALGORITHM: ProcessValidationError
// INPUT: error (ValidationError), context (object)
// OUTPUT: processedError (object with actionable details)
//
// BEGIN
//     // Phase 1: Extract error details
//     message ← error.message
//     path ← error.path
//     code ← error.code
//
//     // Phase 2: Determine error severity and action
//     severity ← DetermineSeverity(code)
//     action ← DetermineRecoveryAction(code, path)
//
//     // Phase 3: Generate diagnostic info
//     diagnostics ← {
//         timestamp: CurrentTimestamp(),
//         agentId: context.agentId,
//         observation_id: context.observation_id,
//         error_path: path,
//         error_code: code,
//         error_message: message
//     }
//
//     // Phase 4: Log for audit/debugging
//     LogValidationError(diagnostics)
//
//     // Phase 5: Return actionable result
//     RETURN {
//         severity,
//         action,
//         diagnostics,
//         message: GenerateUserMessage(code, path)
//     }
// END

function processValidationError(error, context = {}) {
  const { message, path, code, value } = error;

  // Determine severity
  let severity = 'warning';
  if (code === 'VALIDATION_ERROR' || code === 'ZOD_VALIDATION_ERROR') {
    severity = 'error';
  }

  // Determine recovery action
  let action = 'SKIP'; // Default: skip invalid observation
  if (path.includes('timestamp_ns')) {
    action = 'FIX_TIMESTAMP'; // Auto-fix if possible
  } else if (path.includes('hash')) {
    action = 'RECOMPUTE_HASH'; // Recompute hash
  } else if (path.includes('output')) {
    action = 'INSPECT_OUTPUT'; // Need human review
  }

  // Generate diagnostic info
  const diagnostics = {
    timestamp: new Date().toISOString(),
    agentId: context.agentId || 'unknown',
    observationId: context.observationId || null,
    errorPath: path.join('.'),
    errorCode: code,
    errorMessage: message,
    invalidValue: String(value).slice(0, 100)
  };

  // Log for audit trail
  console.error('[VALIDATION_ERROR]', diagnostics);

  return {
    severity,
    action,
    diagnostics,
    message: generateUserMessage(code, path, value)
  };
}

function generateUserMessage(code, path, value) {
  if (code === 'ZOD_VALIDATION_ERROR') {
    const fieldName = path.join('.');
    return `Invalid value for field '${fieldName}': ${String(value).slice(0, 50)}`;
  }

  if (code === 'RECEIPT_CHAIN_ERROR') {
    return 'Receipt chain is broken or timestamps are not monotonic';
  }

  return 'Validation failed. Please check observation format.';
}

// Usage
const obs = { /* invalid observation */ };
const result = validateObservation(obs);

if (!result.success) {
  const processed = processValidationError(result.error, {
    agentId: 'agent-1',
    observationId: obs.id
  });

  console.log(`Severity: ${processed.severity}`);
  console.log(`Action: ${processed.action}`);
  console.log(`Message: ${processed.message}`);

  // Take action based on recovery
  switch (processed.action) {
    case 'SKIP':
      console.log('Skipping invalid observation');
      break;
    case 'FIX_TIMESTAMP':
      obs.timestamp_ns = getNowNs();
      break;
    case 'RECOMPUTE_HASH':
      obs.hash = computeHash(obs);
      break;
    case 'INSPECT_OUTPUT':
      console.log('Manual review required:', obs);
      break;
  }
}
```

---

## Part 4: Receipt Chain Management

### Creating and Linking Receipts

```javascript
// ============================================================================
// ALGORITHM: CreateReceiptChain
// ============================================================================

// PSEUDOCODE:
// ALGORITHM: CreateReceiptChain
// INPUT: observations (array of Observation), prevHash (string or null)
// OUTPUT: receipts (array of Receipt with linked hashes)
//
// BEGIN
//     receipts ← []
//     currentPrevHash ← prevHash
//
//     FOR EACH obs IN observations DO
//         // Compute receipt hash
//         receipt_hash ← BLAKE3(JSON.stringify(obs without hash field))
//
//         // Create receipt
//         receipt ← {
//             obs_hash: receipt_hash,
//             prev_hash: currentPrevHash,
//             timestamp_ns: obs.timestamp_ns,
//             agentId: obs.agentId
//         }
//
//         receipts.append(receipt)
//         currentPrevHash ← receipt_hash
//     END FOR
//
//     RETURN receipts
// END

function createReceiptChain(observations, previousHash = null) {
  const receipts = [];
  let currentPrevHash = previousHash;

  for (const obs of observations) {
    // Compute hash of observation (used as receipt obs_hash)
    const obsHash = computeObservationHash(obs);

    // Create receipt linking to previous
    const receipt = {
      obs_hash: obsHash,
      prev_hash: currentPrevHash,
      timestamp_ns: obs.timestamp_ns,
      agentId: obs.agentId
    };

    // Validate receipt before adding
    const receiptValidation = validateReceipt(receipt);
    if (!receiptValidation.success) {
      throw new Error(
        `Receipt validation failed: ${receiptValidation.error.message}`
      );
    }

    receipts.push(receipt);
    currentPrevHash = obsHash; // Link next receipt to this one
  }

  return receipts;
}

function computeObservationHash(obs) {
  // Exclude hash and receipt from computation
  const { hash, receipt, ...hashInput } = obs;
  const json = JSON.stringify(hashInput);
  return createBlake3().update(json).digest('hex');
}

// Usage: Create receipt chain for batch of observations
const observations = [
  createObservation('agent-1', 'runtime', { /* ... */ }),
  createObservation('agent-1', 'fs', { /* ... */ }),
  createObservation('agent-1', 'perf', { /* ... */ })
];

try {
  const receipts = createReceiptChain(observations);
  console.log(`✓ Created ${receipts.length} receipts in chain`);

  // Verify chain integrity
  const chainValidation = validateReceiptChain(receipts);
  if (chainValidation.success) {
    console.log('✓ Receipt chain integrity verified');
  } else {
    console.error('✗ Receipt chain invalid:', chainValidation.error.message);
  }

  // Attach receipts to observations
  observations.forEach((obs, idx) => {
    obs.receipt = receipts[idx];
  });
} catch (err) {
  console.error('Failed to create receipt chain:', err.message);
}
```

---

## Part 5: Capability & Constraint Derivation

### Deriving Capabilities from Observations

```javascript
// ============================================================================
// ALGORITHM: DeriveCapabilitiesFromObservations
// ============================================================================

// PSEUDOCODE:
// ALGORITHM: DeriveCapabilitiesFromObservations
// INPUT: observations (array of Observation)
// OUTPUT: capabilities (array of Capability)
//
// BEGIN
//     // Phase 1: Group observations by (domain, method)
//     groups ← GroupBy(observations, [domain, method])
//
//     capabilities ← []
//
//     FOR EACH (domain, method), obsGroup IN groups DO
//         // Phase 2: Filter to successful observations
//         successfulObs ← Filter(obsGroup, obs => obs.guard.type == "allow")
//
//         IF Length(successfulObs) == 0 THEN
//             CONTINUE // No successful observations for this capability
//         END IF
//
//         // Phase 3: Extract metrics
//         maxThroughput ← Max(perfMetrics.throughput for perf obs)
//         minLatency_ns ← Min(perfMetrics.latency for perf obs) * 1000000
//
//         // Phase 4: Compute confidence
//         confidence ← Length(successfulObs) / Length(obsGroup)
//
//         // Phase 5: Create capability
//         capability ← {
//             id: GenerateUUID(),
//             domain,
//             method,
//             maxThroughput,
//             minLatency_ns,
//             available: true,
//             confidence,
//             lastVerified: LatestTimestamp(successfulObs),
//             observationCount: Length(successfulObs)
//         }
//
//         capabilities.append(capability)
//     END FOR
//
//     RETURN capabilities
// END

function deriveCapabilitiesFromObservations(observations) {
  // Phase 1: Group by (domain, method)
  const groups = new Map();

  for (const obs of observations) {
    const key = `${obs.domain}:${obs.method}`;

    if (!groups.has(key)) {
      groups.set(key, []);
    }

    groups.get(key).push(obs);
  }

  // Phase 2: Derive capabilities
  const capabilities = [];

  for (const [key, group] of groups) {
    const [domain, method] = key.split(':');

    // Filter successful observations
    const successful = group.filter(obs => obs.guard.type === 'allow');

    if (successful.length === 0) {
      continue; // No successful observations for this capability
    }

    // Extract performance metrics (if domain is perf)
    let maxThroughput = null;
    let minLatency_ns = null;

    const perfObservations = successful.filter(obs => obs.domain === 'perf');
    if (perfObservations.length > 0) {
      const throughputs = perfObservations.map(obs => obs.output.throughput);
      const latencies = perfObservations.map(obs => obs.output.latency_p50);

      maxThroughput = Math.max(...throughputs);
      minLatency_ns = Math.min(...latencies) * 1000000; // Convert ms to ns
    }

    // Compute confidence
    const confidence = successful.length / group.length;

    // Create capability
    const capability = {
      id: uuidv4(),
      domain,
      method,
      maxThroughput,
      minLatency_ns,
      available: true,
      confidence: parseFloat(confidence.toFixed(2)),
      lastVerified: successful[successful.length - 1].timestamp_ns,
      observationCount: successful.length
    };

    capabilities.push(capability);
  }

  return capabilities;
}

// Usage
const allObservations = [
  // Multiple observations of same capability
  createObservation('agent-1', 'runtime', { /* success */ }),
  createObservation('agent-1', 'runtime', { /* success */ }),
  createObservation('agent-1', 'runtime', { /* success */ }),
  // ...
];

const capabilities = deriveCapabilitiesFromObservations(allObservations);

console.log(`Derived ${capabilities.length} capabilities:`);
capabilities.forEach(cap => {
  console.log(`  - ${cap.domain}.${cap.method}:`);
  console.log(`    Available: ${cap.available}`);
  console.log(`    Confidence: ${cap.confidence * 100}%`);
  console.log(`    Observations: ${cap.observationCount}`);
  if (cap.maxThroughput) {
    console.log(`    Max throughput: ${cap.maxThroughput} ops/s`);
  }
});
```

### Deriving Constraints from Denied Observations

```javascript
// ============================================================================
// ALGORITHM: DeriveConstraintsFromObservations
// ============================================================================

// PSEUDOCODE:
// ALGORITHM: DeriveConstraintsFromObservations
// INPUT: observations (array of Observation)
// OUTPUT: constraints (array of Constraint)
//
// BEGIN
//     constraints ← []
//     deniedObservations ← Filter(observations, obs => obs.guard.type == "deny")
//
//     FOR EACH obs IN deniedObservations DO
//         // Extract constraint from output or reason
//         constraintName ← ExtractConstraintName(obs.method)
//         constraintValue ← ExtractConstraintValue(obs.output)
//
//         // Create constraint entry
//         constraint ← {
//             id: GenerateUUID(),
//             domain: obs.domain,
//             name: constraintName,
//             value: constraintValue,
//             unit: DetermineUnit(obs.domain, constraintName),
//             enforced: true,
//             severity: "error",
//             discoveredAt: obs.timestamp_ns,
//             observationCount: 1
//         }
//
//         // Merge with existing constraint if present
//         existing ← FindConstraint(constraints, obs.domain, constraintName)
//         IF existing THEN
//             existing.observationCount += 1
//         ELSE
//             constraints.append(constraint)
//         END IF
//     END FOR
//
//     RETURN constraints
// END

function deriveConstraintsFromObservations(observations) {
  const constraints = [];
  const deniedObservations = observations.filter(obs => obs.guard.type === 'deny');

  for (const obs of deniedObservations) {
    // Extract constraint info from method and reason
    const { domain, method } = obs;
    const { reason } = obs.guard;

    // Try to parse constraint from reason
    const constraintMatch = reason.match(
      /(?:limit|quota|max|exceeded|denied).*?(\d+)\s*([a-z%]+)?/i
    );

    let constraintName = extractConstraintName(method);
    let constraintValue = null;
    let unit = 'generic';

    if (constraintMatch) {
      constraintValue = parseInt(constraintMatch[1]);
      unit = constraintMatch[2] || determineUnit(domain, constraintName);
    }

    // Find or create constraint
    const key = `${domain}:${constraintName}`;
    let existing = constraints.find(c => `${c.domain}:${c.name}` === key);

    if (existing) {
      existing.observationCount++;
      existing.discoveredAt = obs.timestamp_ns; // Update to most recent
    } else {
      const constraint = {
        id: uuidv4(),
        domain,
        name: constraintName,
        value: constraintValue,
        unit,
        enforced: true,
        severity: 'error',
        discoveredAt: obs.timestamp_ns,
        observationCount: 1
      };

      constraints.push(constraint);
    }
  }

  return constraints;
}

function extractConstraintName(method) {
  const parts = method.split('.');
  return parts[parts.length - 1] || 'unknown';
}

function determineUnit(domain, constraintName) {
  const units = {
    runtime: { workers: 'threads' },
    fs: { quota: 'bytes', pathLength: 'chars' },
    limits: { memory: 'MB', storage: 'bytes' },
    perf: { throughput: 'ops/s', latency: 'ms' },
    net: { payload: 'bytes', connections: 'count' }
  };

  return units[domain]?.[constraintName] || 'generic';
}

// Usage
const constraints = deriveConstraintsFromObservations(allObservations);

console.log(`Discovered ${constraints.length} constraints:`);
constraints.forEach(c => {
  console.log(`  - ${c.domain}.${c.name}:`);
  console.log(`    Value: ${c.value} ${c.unit}`);
  console.log(`    Severity: ${c.severity}`);
  console.log(`    Discovered ${c.observationCount}x`);
});
```

---

## Part 6: Performance & Optimization

### Optimizing Large-Scale Validation

```javascript
// ============================================================================
// ALGORITHM: OptimizedBatchValidation
// ============================================================================

// PSEUDOCODE:
// ALGORITHM: OptimizedBatchValidation
// INPUT: observations (array), workerCount (integer), batchSize (integer)
// OUTPUT: results ({passed, failed, stats})
//
// BEGIN
//     // Phase 1: Partition observations
//     partitions ← PartitionArray(observations, workerCount)
//
//     // Phase 2: Parallel validation (simulated with Promise.all)
//     promises ← []
//     FOR EACH partition IN partitions DO
//         promise ← AsyncValidatePartition(partition)
//         promises.append(promise)
//     END FOR
//
//     // Phase 3: Aggregate results
//     allResults ← AWAIT Promise.all(promises)
//     merged ← MergeResults(allResults)
//
//     // Phase 4: Compute statistics
//     stats ← {
//         totalTime: CurrentTime() - startTime,
//         observationsPerSecond: observations.length / stats.totalTime,
//         errors: [error rates by type]
//     }
//
//     RETURN {
//         passed: merged.passed,
//         failed: merged.failed,
//         stats
//     }
// END

async function optimizedBatchValidation(
  observations,
  workerCount = 4,
  batchSize = 100
) {
  const startTime = Date.now();

  // Phase 1: Partition observations across workers
  const partitionSize = Math.ceil(observations.length / workerCount);
  const partitions = [];

  for (let i = 0; i < workerCount; i++) {
    const start = i * partitionSize;
    const end = Math.min(start + partitionSize, observations.length);
    if (start < observations.length) {
      partitions.push(observations.slice(start, end));
    }
  }

  // Phase 2: Validate partitions in parallel
  const promises = partitions.map(partition =>
    validatePartitionAsync(partition, batchSize)
  );

  const allResults = await Promise.all(promises);

  // Phase 3: Merge results
  const merged = {
    passed: [],
    failed: []
  };

  for (const result of allResults) {
    merged.passed.push(...result.passed);
    merged.failed.push(...result.failed);
  }

  // Phase 4: Compute statistics
  const elapsedMs = Date.now() - startTime;
  const opsPerSecond = (observations.length / elapsedMs) * 1000;

  const stats = {
    totalTimeMs: elapsedMs,
    observationCount: observations.length,
    passCount: merged.passed.length,
    failCount: merged.failed.length,
    passRate: (merged.passed.length / observations.length) * 100,
    throughputOpsPerSecond: opsPerSecond
  };

  return {
    passed: merged.passed,
    failed: merged.failed,
    stats
  };
}

async function validatePartitionAsync(observations, batchSize) {
  const passed = [];
  const failed = [];

  // Process in batches to manage memory
  for (let i = 0; i < observations.length; i += batchSize) {
    const batch = observations.slice(
      i,
      Math.min(i + batchSize, observations.length)
    );

    const { results } = validateObservationBatch(batch);

    results.forEach((result, idx) => {
      if (result.success) {
        passed.push(result.data);
      } else {
        failed.push({
          observation: batch[idx],
          error: result.error
        });
      }
    });

    // Yield to event loop
    await new Promise(resolve => setImmediate(resolve));
  }

  return { passed, failed };
}

// Complexity Analysis:
// Time Complexity:
//   - Sequential: O(n) where n = number of observations
//   - Parallel (w workers): O(n/w) with overhead
//   - Per observation: O(d) where d = output depth (typically O(1))
//   - Total: O(n * d / w)
//
// Space Complexity:
//   - Partition arrays: O(n/w) per worker
//   - Result storage: O(n)
//   - Total: O(n)
//
// Performance Targets:
//   - Single observation: <1ms
//   - 10,000 observations: <10s (4 workers)
//   - Throughput: 1,000-10,000 ops/sec

// Usage
const largeSet = Array(10000).fill(null).map(() =>
  createObservation(...)
);

console.log(`Validating ${largeSet.length} observations with parallel workers...`);

const result = await optimizedBatchValidation(largeSet, 4, 500);

console.log('Results:');
console.log(`  Passed: ${result.stats.passCount}`);
console.log(`  Failed: ${result.stats.failCount}`);
console.log(`  Pass Rate: ${result.stats.passRate.toFixed(1)}%`);
console.log(`  Time: ${result.stats.totalTimeMs}ms`);
console.log(`  Throughput: ${result.stats.throughputOpsPerSecond.toFixed(0)} ops/s`);
```

---

## Summary: Design Patterns

### Key Patterns Used

1. **Factory Pattern** - Domain-specific observation creators
2. **Validation Pipeline** - Multi-phase validation with error recovery
3. **Hash Chain** - Immutable audit log via linked receipts
4. **Batch Processing** - Efficient large-scale validation
5. **Progressive Enhancement** - Retry logic with backoff

### When to Use Each

| Pattern | Use Case | Complexity |
|---------|----------|-----------|
| Validation | Every observation | O(1) per obs |
| Receipt Chain | Audit log requirement | O(n) for chain |
| Capability Derivation | System capability discovery | O(n * m) |
| Constraint Derivation | Limit discovery | O(n * m) |
| Batch Validation | Large datasets | O(n/w) parallel |

### Performance Summary

- **Single Observation**: <1ms validation
- **Batch (1000 obs)**: <1s with 4 workers
- **Memory**: ~1KB per observation
- **Storage**: ~500B per receipt in chain

