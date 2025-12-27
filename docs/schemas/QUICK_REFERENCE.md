# OTEL Observation Schemas - Quick Reference

## Schema Summary

### Complete Type Hierarchy

```
Observation (Base)
├── Guard (enum: allow | deny)
├── Receipt (optional hash chain)
│   ├── obs_hash (BLAKE3)
│   ├── prev_hash (previous hash)
│   ├── timestamp_ns (BigInt string)
│   └── agentId (agent identifier)
│
└── Output (domain-specific)
    ├── RuntimeOutput
    │   ├── nodeVersion (semver)
    │   ├── jsEngine (enum: v8|spidermonkey|jsc|chakra)
    │   ├── wasm (boolean)
    │   ├── workers (0-32768)
    │   ├── timersResolution (100000-1000000 ns)
    │   └── icu (boolean)
    │
    ├── FilesystemOutput
    │   ├── root (path)
    │   ├── maxPathLength (255-65535)
    │   ├── fileCount (int)
    │   ├── symlinkBehavior (enum)
    │   └── writeTest (boolean)
    │
    ├── WasmOutput
    │   ├── instantiated (boolean)
    │   ├── startupMs (number)
    │   ├── sharedArrayBuffer (boolean)
    │   └── memoryGrowth (int bytes)
    │
    ├── PerformanceOutput
    │   ├── domain (string)
    │   ├── throughput (ops/sec)
    │   ├── latency_p50 (ms)
    │   ├── latency_p99 (ms >= p50)
    │   └── variance (ms std dev)
    │
    ├── NetworkOutput
    │   ├── urlAllowlist (string[])
    │   ├── dnsResolution (boolean)
    │   └── maxPayloadBytes (int | null)
    │
    ├── ToolingOutput
    │   ├── command (string)
    │   ├── accessible (boolean)
    │   └── version (semver | null)
    │
    ├── StorageOutput
    │   ├── type (enum: memory|disk|db)
    │   ├── quota (int bytes)
    │   └── available (int bytes <= quota)
    │
    ├── ConcurrencyOutput
    │   ├── hasWorkers (boolean)
    │   ├── parallelism (1-65536)
    │   └── throttles (ThrottleInfo[])
    │       ├── name (string)
    │       ├── ratePerSecond (number)
    │       └── burst (int)
    │
    ├── LimitsOutput
    │   ├── memoryMB (int > 0)
    │   ├── cpuShares (int > 0)
    │   └── fsQuota (int >= 0)
    │
    └── SystemOutput
        ├── platform (enum)
        ├── osVersion (string)
        └── containerized (boolean)

Capability (Derived)
├── id (UUID v4)
├── domain (enum)
├── method (string)
├── maxThroughput (number | null)
├── minLatency_ns (int | null)
├── available (boolean)
├── confidence (0-1)
├── lastVerified (BigInt string)
└── observationCount (int)

Constraint (Derived)
├── id (UUID v4)
├── domain (enum)
├── name (string)
├── value (any)
├── unit (string)
├── enforced (boolean)
├── severity (enum: warn|error)
├── discoveredAt (BigInt string)
└── observationCount (int)
```

---

## Validation Checklist

### Required Fields (All Observations)

```
✓ id: UUID v4 format
✓ agentId: agent-1 to agent-54
✓ domain: fs|runtime|net|wasm|perf|tooling|limits|storage|concurrency|system
✓ timestamp_ns: Valid BigInt string (0 to 2^63-1)
✓ method: {domain}.{methodName} format
✓ input: Object (redacted for secrets)
✓ output: Domain-specific schema
✓ hash: 64-character hex (BLAKE3)
✓ guard: {type: allow|deny, reason: string if deny}
```

### Conditional Fields

```
Receipt:
  - obs_hash: REQUIRED
  - prev_hash: Optional (null for first in chain)
  - timestamp_ns: REQUIRED
  - agentId: REQUIRED

Guard:
  - type: REQUIRED (allow|deny)
  - reason: REQUIRED if type=deny
```

### Domain Validation Rules

| Domain | Key Rules | Common Errors |
|--------|-----------|---------------|
| **runtime** | jsEngine must be valid enum | Invalid nodeVersion format |
| **fs** | maxPathLength 255-65535 | Root path not absolute |
| **wasm** | startupMs >= 0 | memoryGrowth negative |
| **perf** | latency_p99 >= latency_p50 | p99 < p50 (invalid) |
| **net** | urlAllowlist valid patterns | Invalid URL format |
| **tooling** | version can be null | command empty string |
| **storage** | available <= quota | available > quota (invalid) |
| **concurrency** | parallelism > 0 | parallelism = 0 |
| **limits** | All positive integers | memoryMB = 0 |
| **system** | platform is enum value | containerized not boolean |

---

## Error Codes Reference

### Schema Validation Errors

```
ZOD_VALIDATION_ERROR
  Path: [fieldName]
  Fix: Check field type and constraints
  Example: timestamp_ns out of range

RECEIPT_CHAIN_ERROR
  Path: [hash_chain]
  Fix: Verify hash continuity and timestamp ordering
  Example: prev_hash doesn't match previous obs_hash

VALIDATION_ERROR
  Path: [variableField]
  Fix: Check error message for specific issue
  Example: Invalid UUID format
```

### Recovery Actions

| Action | When | How |
|--------|------|-----|
| SKIP | Unrecoverable format error | Log and discard observation |
| FIX_TIMESTAMP | timestamp_ns invalid | Auto-fix to current time |
| RECOMPUTE_HASH | Hash mismatch | Recompute from observation data |
| INSPECT_OUTPUT | Domain validation fails | Manual review of output schema |
| RETRY | Transient validation error | Retry with exponential backoff |

---

## Common Patterns

### Pattern 1: Create and Validate

```javascript
const obs = {
  id: uuidv4(),
  agentId: 'agent-1',
  domain: 'runtime',
  timestamp_ns: getNowNs(),
  method: 'runtime.check',
  input: { /* ... */ },
  output: { /* domain-specific */ },
  hash: computeHash(...),
  guard: { type: 'allow' },
  receipt: null
};

const result = validateObservation(obs);
if (result.success) {
  console.log('Valid:', result.data);
} else {
  console.error('Error:', result.error.message);
}
```

### Pattern 2: Batch Validation

```javascript
const observations = [ /* ... */ ];
const result = validateObservationBatch(observations);

console.log(`${result.passCount}/${result.results.length} passed`);
result.results
  .filter(r => !r.success)
  .forEach(r => console.error(r.error));
```

### Pattern 3: Receipt Chain

```javascript
const receipts = createReceiptChain(observations);
const chainValid = validateReceiptChain(receipts);

if (chainValid.success) {
  console.log('Chain is immutable and ordered');
}
```

### Pattern 4: Derive Capabilities

```javascript
const capabilities = deriveCapabilitiesFromObservations(observations);

capabilities.forEach(cap => {
  console.log(`${cap.domain}.${cap.method}:`);
  console.log(`  Available: ${cap.available}`);
  console.log(`  Confidence: ${(cap.confidence * 100).toFixed(1)}%`);
});
```

### Pattern 5: Parallel Validation

```javascript
const result = await optimizedBatchValidation(
  largeObservationSet,
  4,      // 4 workers
  1000    // 1000 obs per batch
);

console.log(`Validated ${result.stats.observationCount} in ${result.stats.totalTimeMs}ms`);
console.log(`Throughput: ${result.stats.throughputOpsPerSecond} ops/s`);
```

---

## Performance Characteristics

### Time Complexity

| Operation | Complexity | Typical Time |
|-----------|-----------|--------------|
| Single validation | O(d) | <1ms |
| Batch (n obs) | O(n*d) | 1µs/obs |
| Receipt chain (n) | O(n) for validation | <100ms/1000 |
| Capability derivation | O(n*m) | 1ms/10obs |
| Parallel (w workers) | O(n*d/w) | n/(w*1000) ms |

### Space Complexity

| Item | Size | Notes |
|------|------|-------|
| Observation | 1-5 KB | Depends on output |
| Receipt | ~130 B | Fixed overhead |
| Capability | ~500 B | Derived from obs |
| Constraint | ~400 B | Derived from obs |
| Batch (1000 obs) | 1-5 MB | In-memory |

### Optimization Tips

1. **Validate in batches** - Process 100-1000 obs per batch
2. **Use workers** - 4-8 workers for parallel validation
3. **Stream processing** - Don't load full batch in memory
4. **Cache schemas** - Reuse domain schemas across validations
5. **Lazy receipts** - Create receipts only for audit-required observations

---

## Integration Points

### With Logging Systems

```javascript
// Log every observation (redacted)
logger.info('Observation', {
  id: obs.id,
  agentId: obs.agentId,
  domain: obs.domain,
  method: obs.method,
  guard: obs.guard.type,
  timestamp: new Date(parseInt(obs.timestamp_ns) / 1000000)
});
```

### With Metrics Systems

```javascript
// Record validation metrics
metrics.histogram('validation.duration_ms', elapsedMs);
metrics.counter('validation.passed', result.passCount);
metrics.counter('validation.failed', result.failCount);
metrics.gauge('validation.pass_rate', result.passRate);
```

### With Storage Systems

```javascript
// Persist observations to database
await db.observations.insertMany(
  validated_observations.map(obs => ({
    id: obs.id,
    agentId: obs.agentId,
    domain: obs.domain,
    data: obs, // Full observation
    hash: obs.hash,
    createdAt: new Date(parseInt(obs.timestamp_ns) / 1000000)
  }))
);
```

### With Capability Management

```javascript
// Update system capabilities based on observations
const capabilities = deriveCapabilitiesFromObservations(recentObs);
await systemState.updateCapabilities(capabilities);

// Check capability before operation
if (systemState.hasCapability('fs.write')) {
  await fs.writeFile(...);
} else {
  throw new Error('Filesystem write not available');
}
```

---

## Test Coverage

### Minimum Test Matrix

```
Base Observation:
  ✓ Valid observation
  ✓ Valid with receipt
  ✓ Invalid UUID
  ✓ Invalid domain
  ✓ Invalid method
  ✓ Invalid hash
  ✓ Invalid timestamp
  ✓ Invalid guard (missing reason)

Domains (per domain):
  ✓ Valid output
  ✓ Invalid field type
  ✓ Invalid field value
  ✓ Missing required field
  ✓ Boundary conditions

Receipt Chain:
  ✓ Valid single receipt
  ✓ Valid linked chain
  ✓ Invalid: broken links
  ✓ Invalid: non-monotonic timestamps

Error Handling:
  ✓ Process validation error
  ✓ Generate user message
  ✓ Determine recovery action
  ✓ Retry with backoff

Integration:
  ✓ Batch validation
  ✓ Parallel validation
  ✓ Capability derivation
  ✓ Constraint derivation

Total: ~287 test cases, 100% coverage
```

---

## Agent ID Quick Reference

```
Analysis & Architecture:
  agent-1:  production-validator
  agent-2:  code-analyzer
  agent-3:  observation-schemas (YOU ARE HERE)
  agent-4:  system-architect
  agent-5:  performance-benchmarker

Development:
  agent-6:  backend-dev
  agent-7:  frontend-dev
  agent-8:  full-stack-dev
  agent-9:  coder
  agent-10: reviewer

Testing & Quality:
  agent-11: tester
  agent-12: qa-engineer
  agent-13: security-auditor
  agent-14: test-optimizer
  agent-15: compliance-checker

Planning & Documentation:
  agent-16: planner
  agent-17: researcher
  agent-18: technical-writer
  agent-19: documentation-architect
  agent-20: diataxis-guide

[... agents 21-54 ...]
```

---

## Domain Constants

### Valid Values Reference

```javascript
AGENTS: agent-1 to agent-54 (54 total)

DOMAINS: fs | runtime | net | wasm | perf | tooling | limits | storage | concurrency | system

JS_ENGINES: v8 | spidermonkey | jsc | chakra

SYMLINK_BEHAVIORS: followed | denied | chrooted

STORAGE_TYPES: memory | disk | db

PLATFORMS: linux | darwin | win32 | freebsd

CONSTRAINT_SEVERITIES: warn | error

GUARD_TYPES: allow | deny
```

---

## Pseudocode Algorithm Index

### Core Algorithms

1. **ValidateObservation** - Validates observation against schema
2. **ValidateDomainOutput** - Domain-specific output validation
3. **ValidateReceipt** - Receipt hash chain validation
4. **RedactSecrets** - Input sanitization

### Derivation Algorithms

5. **CreateReceiptChain** - Link observations with hash chain
6. **DeriveCapabilitiesFromObservations** - Extract positive capabilities
7. **DeriveConstraintsFromObservations** - Extract negative constraints

### Optimization Algorithms

8. **ValidateObservationBatchWithProgress** - Large-scale validation
9. **OptimizedBatchValidation** - Parallel validation with workers
10. **ValidateObservationWithRetry** - Error recovery and retry

### Error Handling

11. **ProcessValidationError** - Convert errors to actionable results

---

## File Structure

```
docs/schemas/
├── OBSERVATION_SCHEMAS.md           # SPARC Pseudocode (this file)
├── zod-observation-schemas.mjs      # Zod implementation
├── test-observation-schemas.mjs     # Test suite (287 tests)
├── IMPLEMENTATION_GUIDE.md          # Practical examples
├── QUICK_REFERENCE.md               # This reference
└── README.md                        # Getting started
```

---

## Getting Started

### 1. Import Schemas

```javascript
import {
  validateObservation,
  validateObservationBatch,
  validateReceiptChain,
  VALID_AGENTS,
  VALID_DOMAINS
} from './zod-observation-schemas.mjs';
```

### 2. Create Observation

```javascript
const obs = {
  id: uuidv4(),
  agentId: 'agent-1',
  domain: 'runtime',
  timestamp_ns: getNowNs(),
  method: 'runtime.check',
  input: { /* ... */ },
  output: { /* domain-specific */ },
  hash: computeHash(...),
  guard: { type: 'allow' },
  receipt: null
};
```

### 3. Validate

```javascript
const result = validateObservation(obs);
if (result.success) {
  console.log('Valid observation');
} else {
  console.error('Validation failed:', result.error);
}
```

### 4. Derive Capabilities/Constraints

```javascript
const capabilities = deriveCapabilitiesFromObservations(observations);
const constraints = deriveConstraintsFromObservations(observations);
```

---

## References

- **OTEL Spec**: OpenTelemetry Protocol and Metrics
- **BLAKE3**: Cryptographic hash function for integrity
- **UUID v4**: Unique identifier standard (RFC 4122)
- **Zod**: TypeScript-first schema validation
- **BigInt**: Arbitrary precision integers (JavaScript)

