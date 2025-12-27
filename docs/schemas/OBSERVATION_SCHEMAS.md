# SPARC Pseudocode: OTEL Observation Schemas

## 1. ALGORITHM: Observation Schema Design

### Phase 1: Base Observation Structure

```
ALGORITHM: ValidateObservation
INPUT: rawObservation (object)
OUTPUT: validatedObservation (Observation) or ValidationError

CONSTANTS:
    VALID_AGENTS = [
        "agent-1", "agent-2", "agent-3", ..., "agent-54"
    ]
    VALID_DOMAINS = [
        "fs", "runtime", "net", "wasm", "perf",
        "tooling", "limits", "storage", "concurrency", "system"
    ]
    BLAKE3_HEX_LENGTH = 64
    UUID_V4_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i
    METHOD_PATTERN = /^[a-z]+\.[a-z_]+$/ // e.g., "fs.read", "runtime.check"

BEGIN
    // Phase 1: Type validation
    IF NOT (rawObservation is object) THEN
        RETURN ValidationError("Input must be object")
    END IF

    // Phase 2: Core field validation
    IF NOT ValidateUUID(rawObservation.id) THEN
        RETURN ValidationError("id must be valid UUID v4")
    END IF

    IF NOT Contains(VALID_AGENTS, rawObservation.agentId) THEN
        RETURN ValidationError(format("agentId must be one of: %s", VALID_AGENTS))
    END IF

    IF NOT Contains(VALID_DOMAINS, rawObservation.domain) THEN
        RETURN ValidationError(format("domain must be one of: %s", VALID_DOMAINS))
    END IF

    // Phase 3: Timestamp validation
    timestamp_ns ← ParseBigInt(rawObservation.timestamp_ns)
    IF timestamp_ns is null THEN
        RETURN ValidationError("timestamp_ns must be parseable as BigInt")
    END IF

    IF timestamp_ns < 0 OR timestamp_ns > MaxSafeTimestamp() THEN
        RETURN ValidationError("timestamp_ns out of valid range")
    END IF

    // Phase 4: Method validation
    IF NOT MatchesPattern(rawObservation.method, METHOD_PATTERN) THEN
        RETURN ValidationError("method must match pattern: domain.name")
    END IF

    // Phase 5: Hash validation
    IF NOT MatchesPattern(rawObservation.hash, /^[0-9a-f]{64}$/) THEN
        RETURN ValidationError("hash must be 64-char hex (BLAKE3)")
    END IF

    // Phase 6: Domain-specific output validation
    domainSchema ← GetDomainSchema(rawObservation.domain)
    validatedOutput ← domainSchema.parse(rawObservation.output)
    IF validatedOutput is Error THEN
        RETURN ValidationError(format("Output validation failed: %s", validatedOutput.message))
    END IF

    // Phase 7: Guard validation
    IF NOT Contains(["allow", "deny"], rawObservation.guard.type) THEN
        RETURN ValidationError("guard.type must be 'allow' or 'deny'")
    END IF

    // Phase 8: Receipt validation (optional)
    IF rawObservation.receipt is not null THEN
        receiptValidation ← ValidateReceipt(rawObservation.receipt)
        IF receiptValidation is Error THEN
            RETURN ValidationError(format("Receipt validation failed: %s", receiptValidation.message))
        END IF
    END IF

    // Phase 9: Construct validated observation
    validatedObservation ← {
        id: rawObservation.id,
        agentId: rawObservation.agentId,
        domain: rawObservation.domain,
        timestamp_ns: timestamp_ns,
        method: rawObservation.method,
        input: RedactSecrets(rawObservation.input),
        output: validatedOutput,
        hash: rawObservation.hash,
        guard: rawObservation.guard,
        receipt: rawObservation.receipt OR null
    }

    RETURN validatedObservation
END
```

### Phase 2: Domain-Specific Output Validation

```
ALGORITHM: ValidateDomainOutput
INPUT: domain (string), output (object)
OUTPUT: validatedOutput (object) or ValidationError

SWITCH domain:
    CASE "runtime":
        RETURN ValidateRuntimeOutput(output)

    CASE "fs":
        RETURN ValidateFilesystemOutput(output)

    CASE "wasm":
        RETURN ValidateWasmOutput(output)

    CASE "perf":
        RETURN ValidatePerformanceOutput(output)

    CASE "net":
        RETURN ValidateNetworkOutput(output)

    CASE "tooling":
        RETURN ValidateToolingOutput(output)

    CASE "storage":
        RETURN ValidateStorageOutput(output)

    CASE "concurrency":
        RETURN ValidateConcurrencyOutput(output)

    CASE "limits":
        RETURN ValidateLimitsOutput(output)

    CASE "system":
        RETURN ValidateSystemOutput(output)

    DEFAULT:
        RETURN ValidationError(format("Unknown domain: %s", domain))
END SWITCH
```

### Phase 3: Receipt Hash Chain Validation

```
ALGORITHM: ValidateReceipt
INPUT: receipt (object)
OUTPUT: validatedReceipt (Receipt) or ValidationError

BEGIN
    // Validate structure
    IF NOT receipt.obs_hash THEN
        RETURN ValidationError("receipt.obs_hash required")
    END IF

    IF NOT receipt.timestamp_ns THEN
        RETURN ValidationError("receipt.timestamp_ns required")
    END IF

    IF NOT receipt.agentId THEN
        RETURN ValidationError("receipt.agentId required")
    END IF

    // Validate hash format (BLAKE3, 64 hex chars)
    IF NOT MatchesPattern(receipt.obs_hash, /^[0-9a-f]{64}$/) THEN
        RETURN ValidationError("obs_hash must be 64-char hex")
    END IF

    // prev_hash optional, but if present must be valid
    IF receipt.prev_hash AND NOT MatchesPattern(receipt.prev_hash, /^[0-9a-f]{64}$/) THEN
        RETURN ValidationError("prev_hash must be 64-char hex or null")
    END IF

    // Validate timestamp_ns is valid BigInt
    receipt_ts ← ParseBigInt(receipt.timestamp_ns)
    IF receipt_ts is null THEN
        RETURN ValidationError("receipt.timestamp_ns must be BigInt string")
    END IF

    RETURN {
        obs_hash: receipt.obs_hash,
        prev_hash: receipt.prev_hash,
        timestamp_ns: receipt.timestamp_ns,
        agentId: receipt.agentId
    }
END
```

---

## 2. DATA STRUCTURES: Schema Definitions

### Base Observation Schema

```
DATA STRUCTURE: Observation

Fields:
    id: string
        Type: UUID v4 format
        Constraint: Unique per observation
        Validation: Pattern /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i
        Purpose: Unique identifier for observation

    agentId: string
        Type: Enum of valid agent names
        Values: "agent-1", "agent-2", ..., "agent-54"
        Constraint: Must be in VALID_AGENTS list
        Purpose: Identify which agent made observation

    domain: string
        Type: Enum of observation domains
        Values: "fs" | "runtime" | "net" | "wasm" | "perf" | "tooling" | "limits" | "storage" | "concurrency" | "system"
        Constraint: Exactly one of 10 values
        Purpose: Categorize observation type

    timestamp_ns: string
        Type: BigInt-like string (decimal notation)
        Constraint: Positive, <= current time + 1 second
        Validation: Must parse to valid BigInt
        Purpose: Nanosecond-precision timestamp
        Range: 0 to 2^63 - 1

    method: string
        Type: Identifier pair
        Format: "{domain}.{methodName}"
        Pattern: /^[a-z]+\.[a-z_]+$/
        Examples: "fs.read", "runtime.check", "perf.measure"
        Purpose: Identify which method was observed

    input: object
        Type: Typed input for method
        Constraint: Redacted (no secrets, passwords, tokens)
        Rules:
            - Strip Bearer tokens
            - Remove API keys
            - Redact passwords
            - Log parameter count, not values for sensitive fields
        Purpose: Record input parameters (for replay/debugging)

    output: object
        Type: Domain-specific (varies by domain)
        Validation: Via domain-specific schema
        Purpose: Record method result

    hash: string
        Type: BLAKE3 hex digest
        Length: Exactly 64 characters
        Pattern: /^[0-9a-f]{64}$/
        Computation: BLAKE3(JSON.stringify(observation without hash and receipt))
        Purpose: Integrity verification

    guard: object
        Type: { type: "allow" | "deny", reason?: string }
        Fields:
            type: "allow" | "deny"
                Constraint: Exactly one value
                Purpose: Whether observation is permitted

            reason: string (optional)
                Constraint: Required if type = "deny"
                Purpose: Explain why observation was denied

        Purpose: Track capability guard decisions

    receipt: Receipt | null
        Type: Optional hash chain receipt
        Structure: See Receipt schema
        Constraint: Optional, but if present must be valid
        Purpose: Immutable record of observation

Invariants:
    - id is globally unique
    - timestamp_ns increases monotonically (in practice)
    - hash is deterministic given all other fields
    - If guard.type = "deny", reason must be non-empty
```

### Domain-Specific Output Schemas

```
DATA STRUCTURE: RuntimeOutput

Fields:
    nodeVersion: string
        Type: Semantic version
        Pattern: /^\d+\.\d+\.\d+(-[a-zA-Z0-9.]+)?$/
        Example: "20.10.0", "20.10.0-rc1"
        Purpose: Node.js runtime version

    jsEngine: string
        Type: Engine identifier
        Values: "v8" | "spidermonkey" | "jsc" | "chakra"
        Purpose: JavaScript engine in use

    wasm: boolean
        Type: Boolean flag
        Purpose: Whether WebAssembly is supported

    workers: number
        Type: Positive integer
        Range: 0-32768
        Purpose: Number of worker threads available

    timersResolution: number
        Type: Positive integer
        Unit: Nanoseconds
        Range: 100000 to 1000000 (0.1ms to 1ms typical)
        Purpose: Minimum timer resolution on platform

    icu: boolean
        Type: Boolean flag
        Purpose: Whether ICU support is enabled

---

DATA STRUCTURE: FilesystemOutput

Fields:
    root: string
        Type: Absolute path
        Pattern: ^/.* (for Unix-like)
        Purpose: Filesystem root for observations

    maxPathLength: number
        Type: Positive integer
        Range: 255-65535
        Purpose: Maximum path component length

    fileCount: number
        Type: Non-negative integer
        Purpose: Number of accessible files (sample)

    symlinkBehavior: string
        Type: Enum
        Values: "followed" | "denied" | "chrooted"
        Purpose: How symlinks are handled

    writeTest: boolean
        Type: Boolean flag
        Purpose: Whether test write succeeded

---

DATA STRUCTURE: WasmOutput

Fields:
    instantiated: boolean
        Type: Boolean flag
        Purpose: Whether WASM module instantiated successfully

    startupMs: number
        Type: Non-negative number
        Unit: Milliseconds
        Purpose: Time to instantiate WASM module

    sharedArrayBuffer: boolean
        Type: Boolean flag
        Purpose: Whether SharedArrayBuffer is available

    memoryGrowth: number
        Type: Non-negative integer
        Unit: Bytes
        Purpose: Peak memory growth from initial allocation

---

DATA STRUCTURE: PerformanceOutput

Fields:
    domain: string
        Type: Operation domain
        Examples: "fs", "network", "compute"
        Purpose: What was measured

    throughput: number
        Type: Positive number
        Unit: Operations per second
        Purpose: Operations completed per second

    latency_p50: number
        Type: Non-negative number
        Unit: Milliseconds
        Purpose: Median latency

    latency_p99: number
        Type: Non-negative number
        Unit: Milliseconds
        Constraint: >= latency_p50
        Purpose: 99th percentile latency

    variance: number
        Type: Non-negative number
        Unit: Milliseconds (standard deviation)
        Purpose: Latency variance

---

DATA STRUCTURE: NetworkOutput

Fields:
    urlAllowlist: string[]
        Type: Array of URL patterns
        Examples: ["https://example.com", "https://*.api.example.com"]
        Purpose: Allowed network destinations

    dnsResolution: boolean
        Type: Boolean flag
        Purpose: Whether DNS resolution works

    maxPayloadBytes: number
        Type: Positive integer
        Unit: Bytes
        Purpose: Maximum request payload size

---

DATA STRUCTURE: ToolingOutput

Fields:
    command: string
        Type: Tool command name
        Examples: "npm", "python", "git"
        Purpose: Which tool was checked

    accessible: boolean
        Type: Boolean flag
        Purpose: Whether tool is accessible in PATH

    version: string
        Type: Semantic version (or version-like string)
        Examples: "8.19.4", "3.11.2"
        Purpose: Tool version

---

DATA STRUCTURE: StorageOutput

Fields:
    type: string
        Type: Enum
        Values: "memory" | "disk" | "db"
        Purpose: Storage type

    quota: number
        Type: Non-negative integer
        Unit: Bytes
        Purpose: Total storage quota

    available: number
        Type: Non-negative integer
        Unit: Bytes
        Constraint: <= quota
        Purpose: Available storage remaining

---

DATA STRUCTURE: ConcurrencyOutput

Fields:
    hasWorkers: boolean
        Type: Boolean flag
        Purpose: Whether worker threads are available

    parallelism: number
        Type: Positive integer
        Range: 1-65536
        Purpose: Max concurrent operations

    throttles: ThrottleInfo[]
        Type: Array of throttle specifications
        Structure: See ThrottleInfo
        Purpose: Active rate limits/throttles

    DATA STRUCTURE: ThrottleInfo
        name: string (identifier)
        ratePerSecond: number (operations/second)
        burst: number (max concurrent requests)

---

DATA STRUCTURE: LimitsOutput

Fields:
    memoryMB: number
        Type: Positive integer
        Unit: Megabytes
        Purpose: Memory limit

    cpuShares: number
        Type: Positive integer
        Purpose: CPU allocation (relative shares)

    fsQuota: number
        Type: Non-negative integer
        Unit: Bytes
        Purpose: Filesystem quota

---

DATA STRUCTURE: SystemOutput

Fields:
    platform: string
        Type: Enum
        Values: "linux" | "darwin" | "win32" | "freebsd"
        Purpose: Operating system

    osVersion: string
        Type: Semantic version or version string
        Examples: "5.10.0", "21.6.0"
        Purpose: OS version

    containerized: boolean
        Type: Boolean flag
        Purpose: Whether running in container/VM
```

### Receipt Schema (Hash Chain)

```
DATA STRUCTURE: Receipt

Fields:
    obs_hash: string
        Type: BLAKE3 hex digest
        Length: Exactly 64 characters
        Pattern: /^[0-9a-f]{64}$/
        Purpose: Hash of the observation

    prev_hash: string | null
        Type: BLAKE3 hex digest or null
        Length: Exactly 64 characters (if present)
        Default: null (for first receipt in chain)
        Purpose: Hash of previous observation in chain

    timestamp_ns: string
        Type: BigInt-like string
        Purpose: When receipt was created

    agentId: string
        Type: Agent identifier
        Values: "agent-1" through "agent-54"
        Purpose: Which agent created receipt

Invariants:
    - obs_hash and prev_hash form a hash chain
    - Each receipt references previous observation
    - timestamp_ns increases per chain
    - Enables immutable audit log
```

### Capability Schema

```
DATA STRUCTURE: Capability

Purpose: Derived from successful observations
        Represents a capability the system has proven

Fields:
    id: string
        Type: UUID v4
        Purpose: Unique capability ID

    domain: string
        Type: Enum (matches Observation domains)
        Purpose: Which domain this capability applies to

    method: string
        Type: Identifier (e.g., "fs.read")
        Purpose: Specific method

    maxThroughput: number
        Type: Non-negative number
        Unit: Operations per second
        Default: null (if not measured)
        Purpose: Max observed throughput

    minLatency_ns: number
        Type: Non-negative number
        Unit: Nanoseconds
        Default: null (if not measured)
        Purpose: Min observed latency

    available: boolean
        Type: Boolean
        Purpose: Whether capability is currently available

    confidence: number
        Type: Float
        Range: 0.0 to 1.0
        Purpose: Confidence score (observations / total attempts)

    lastVerified: string
        Type: BigInt-like timestamp
        Purpose: When capability was last verified

    observationCount: number
        Type: Non-negative integer
        Purpose: Number of observations supporting this capability

Derivation Rules:
    - Created when observation.guard.type = "allow"
    - confidence = successfulObservations / totalObservations
    - maxThroughput = max(perf.throughput across observations)
    - minLatency_ns = min(perf.latency_p50 * 1000000 across observations)
```

### Constraint Schema

```
DATA STRUCTURE: Constraint

Purpose: Derived from failed observations / system limits
        Represents a constraint discovered

Fields:
    id: string
        Type: UUID v4
        Purpose: Unique constraint ID

    domain: string
        Type: Enum
        Purpose: Which domain this constraint applies to

    name: string
        Type: Identifier
        Examples: "maxFileSize", "connectionPoolSize", "memoryLimit"
        Purpose: Constraint name

    value: any
        Type: Number | string | object (depends on constraint)
        Purpose: Constraint value

    unit: string
        Type: String
        Examples: "bytes", "connections", "seconds"
        Purpose: Constraint unit

    enforced: boolean
        Type: Boolean
        Purpose: Whether constraint is hard limit (vs soft)

    severity: string
        Type: Enum
        Values: "warn" | "error"
        Purpose: How constraint violation should be treated

    discoveredAt: string
        Type: BigInt-like timestamp
        Purpose: When constraint was discovered

    observationCount: number
        Type: Non-negative integer
        Purpose: How many observations triggered this constraint

Derivation Rules:
    - Created when observation.guard.type = "deny"
    - value extracted from observation.output
    - enforced = true if limit_output fields indicate hard limit
    - severity = "error" if deny observation, "warn" if soft constraint
```

---

## 3. COMPLEXITY ANALYSIS

### Time Complexity

```
Operation: ValidateObservation
    Input parsing: O(1)
    Core field validation: O(1)
    Timestamp parsing: O(1)
    Method pattern match: O(m) where m = method string length (typically 10-30 chars)
    Hash pattern match: O(1) (fixed 64 chars)
    Domain schema validation: O(d) where d = output object depth (typically 2-4 levels)
    Guard validation: O(1)
    Receipt validation (if present): O(1)
    Total: O(d) ≈ O(1) in practice (small, shallow objects)

Measurement:
    - Typical observation: <1ms validation time
    - Batch validation (100 observations): <100ms
```

### Space Complexity

```
Operation: ValidateObservation
    Input object: O(s) where s = serialized observation size (typically 500B-5KB)
    Validation state: O(1) (constants)
    Error details: O(e) where e = error message length (typically 50-200 chars)
    Total: O(s) ≈ O(1KB) per observation

Measurement:
    - Typical observation with receipt: 1-3KB
    - Receipt chain overhead: ~130 bytes per receipt
```

### Hash Chain Operations

```
Operation: ValidateReceiptChain
    For chain of length N:
    - Linear scan: O(N)
    - Hash verification: O(N * 64) = O(N) (fixed hash length)
    - Timestamp ordering: O(N log N) if sorted
    Total: O(N log N) if validation + sort required

Constraint:
    - Typical chain length: 10-1000 observations
    - Expected chain validation: <100ms for 1000 observations
```

---

## 4. ERROR HANDLING PATTERNS

### Validation Error Hierarchy

```
PATTERN: ValidationError

ErrorTypes:
    - SchemaValidationError (invalid format)
    - DomainSpecificError (domain logic violation)
    - GuardRejectionError (capability guard denial)
    - ReceiptChainError (hash chain integrity)
    - RedactionError (secret exposure)

HandlingStrategy:
    - Schema errors: Reject immediately with details
    - Guard denials: Log for audit, continue observation
    - Receipt errors: Log warning, accept observation without receipt
    - Redaction errors: Sanitize input, warn operator
```

### Input Redaction Rules

```
ALGORITHM: RedactSecrets
INPUT: input (object)
OUTPUT: redactedInput (object)

BEGIN
    redacted ← {}

    FOR EACH key, value IN input DO
        SWITCH key:
            // Sensitive patterns to redact
            CASE /password|secret|token|apikey|auth|credential/:
                IF value is string THEN
                    redacted[key] ← "[REDACTED:" + Length(value) + "]"
                ELSE
                    redacted[key] ← "[REDACTED_OBJECT]"
                END IF

            // URL patterns (keep domain, redact query)
            CASE /url|endpoint|uri/:
                IF value matches /^https?:/ THEN
                    parsed ← ParseURL(value)
                    redacted[key] ← format("%s://%s[REDACTED_QUERY]",
                                          parsed.protocol, parsed.host)
                ELSE
                    redacted[key] ← value
                END IF

            // Safe fields - pass through or recurse
            CASE /id|name|status|count|version|timestamp/:
                redacted[key] ← value

            // Objects - recurse
            DEFAULT:
                IF value is object THEN
                    redacted[key] ← RedactSecrets(value)
                ELSE
                    redacted[key] ← value
                END IF
        END SWITCH
    END FOR

    RETURN redacted
END
```

---

## 5. IMPLEMENTATION ROADMAP

### Phase 1: Base Schemas (Week 1)
- [ ] Implement Observation base schema with Zod
- [ ] Implement Guard enum validation
- [ ] Implement UUID v4 validation
- [ ] Implement BLAKE3 hash validation
- [ ] Unit tests: 100% base field validation

### Phase 2: Domain Schemas (Week 2)
- [ ] Implement 10 domain-specific output schemas
- [ ] Implement domain schema router logic
- [ ] Add numeric constraint validation (ranges, units)
- [ ] Unit tests: 10 domains x 5 test cases each = 50 tests

### Phase 3: Receipt Chain (Week 2)
- [ ] Implement Receipt schema with hash chain validation
- [ ] Implement receipt linking logic
- [ ] Add hash chain integrity verification
- [ ] Unit tests: Receipt chain validation (5 scenarios)

### Phase 4: Derived Schemas (Week 3)
- [ ] Implement Capability schema
- [ ] Implement Constraint schema
- [ ] Add derivation logic from observations
- [ ] Unit tests: 20 tests (capability + constraint derivation)

### Phase 5: Error Handling (Week 3)
- [ ] Implement ValidationError hierarchy
- [ ] Implement input redaction
- [ ] Add error reporting with context
- [ ] Unit tests: 30 error scenarios

### Phase 6: Integration (Week 4)
- [ ] Full observation validation pipeline
- [ ] Batch observation validation
- [ ] Performance benchmarks
- [ ] Integration tests: End-to-end observation flow

---

## 6. DESIGN DECISIONS & RATIONALE

### Why BigInt-as-String for timestamp_ns?

**Decision**: Store nanosecond timestamps as decimal strings, not numbers

**Rationale**:
- JavaScript Number: Safe only to 2^53-1 (9,007,199,254,740,991)
- Nanoseconds per second: 1,000,000,000
- Max safe nanoseconds: 9,007,199 seconds ≈ 104 days
- Strings preserve full precision across serialization
- OTEL standard uses string representation

**Trade-off**: Parsing cost (~1µs) acceptable for correctness guarantee

### Why Enum Domains Instead of Dynamic?

**Decision**: Fixed 10 domains instead of string-any pattern

**Rationale**:
- Schema validation requires known output types
- 10 domains cover 95% of observation types
- Closed set enables static analysis
- Reduces validation complexity: O(1) domain lookup vs O(n)

**Trade-off**: Adding new domain requires schema update (acceptable, rare)

### Why Hash Chain on Receipts?

**Decision**: Linear hash chain (prev_hash -> obs_hash) for integrity

**Rationale**:
- Detects tampering: reorder, delete, or modify observation
- Immutable audit log for compliance
- Compatible with blockchain patterns
- O(1) append, O(N) validation

**Alternative Considered**: Merkle tree (overkill for sequential log)

### Why Separate Capability and Constraint?

**Decision**: Two derived schemas instead of single "Outcome"

**Rationale**:
- Capabilities: Positive assertions (system CAN do X)
- Constraints: Negative assertions (system CANNOT exceed Y)
- Different usage: Capabilities for planning, constraints for safety
- Enables independent evolution

---

## 7. VALIDATION PSEUDOCODE EXAMPLES

### Example 1: Validate Runtime Observation

```
INPUT: {
    id: "550e8400-e29b-41d4-a716-446655440000",
    agentId: "agent-1",
    domain: "runtime",
    timestamp_ns: "1703145600000000000",
    method: "runtime.check",
    input: {count: 5},
    output: {
        nodeVersion: "20.10.0",
        jsEngine: "v8",
        wasm: true,
        workers: 4,
        timersResolution: 100000,
        icu: true
    },
    hash: "abcd...dcba", // 64 hex chars
    guard: {type: "allow"},
    receipt: null
}

VALIDATION STEPS:
1. ValidateUUID(id) ✓
2. ValidateAgentId(agentId) ✓
3. ValidateDomain(domain) ✓
4. ValidateTimestamp(timestamp_ns) ✓
5. ValidateMethod("runtime.check") ✓
6. ValidateRuntimeOutput(output) ✓
   - nodeVersion matches /^\d+\.\d+\.\d+/ ✓
   - jsEngine in ["v8", "spidermonkey", "jsc", "chakra"] ✓
   - wasm is boolean ✓
   - workers in range [0, 32768] ✓
   - timersResolution in range [100000, 1000000] ✓
   - icu is boolean ✓
7. ValidateHash(hash) ✓
8. ValidateGuard(guard) ✓

RESULT: ✅ VALID Observation
```

### Example 2: Validate Denied Network Observation

```
INPUT: {
    id: "550e8400-e29b-41d4-a716-446655440001",
    agentId: "agent-2",
    domain: "net",
    timestamp_ns: "1703145601000000000",
    method: "net.resolve",
    input: {hostname: "[REDACTED:20]"},
    output: {
        urlAllowlist: [],
        dnsResolution: false,
        maxPayloadBytes: null
    },
    hash: "dcba...abcd",
    guard: {
        type: "deny",
        reason: "DNS resolution not available in sandbox"
    },
    receipt: null
}

VALIDATION STEPS:
1. Schema structure ✓
2. Guard validation:
   - type = "deny" ✓
   - reason required and present ✓
3. Output validation:
   - urlAllowlist is array ✓
   - dnsResolution is boolean ✓
   - maxPayloadBytes is null (acceptable for failed check) ✓

RESULT: ✅ VALID Observation with guard=deny
ACTION: Log denial, don't derive capability
```

### Example 3: Receipt Chain Validation

```
OBSERVATIONS: [obs1, obs2, obs3]

RECEIPTS CHAIN:
Receipt(1): {
    obs_hash: hash_1,
    prev_hash: null,        // First in chain
    timestamp_ns: "100",
    agentId: "agent-1"
}

Receipt(2): {
    obs_hash: hash_2,
    prev_hash: hash_1,      // Links to receipt(1).obs_hash
    timestamp_ns: "200",
    agentId: "agent-2"
}

Receipt(3): {
    obs_hash: hash_3,
    prev_hash: hash_2,      // Links to receipt(2).obs_hash
    timestamp_ns: "300",
    agentId: "agent-3"
}

VALIDATION ALGORITHM:
FOR i = 1 TO len(receipts):
    IF i == 1 THEN
        ASSERT receipts[i].prev_hash == null ✓
    ELSE
        ASSERT receipts[i].prev_hash == receipts[i-1].obs_hash ✓
    END IF
    ASSERT receipts[i].timestamp_ns > receipts[i-1].timestamp_ns ✓
END FOR

RESULT: ✅ VALID receipt chain (immutable, ordered, linked)
```

---

## 8. MEASUREMENT & VALIDATION

### Schema Test Coverage

| Domain | Tests | Coverage |
|--------|-------|----------|
| Base Observation | 25 | 100% (all fields + combinations) |
| Runtime | 15 | 100% (all fields + edge cases) |
| Filesystem | 15 | 100% |
| WASM | 12 | 100% |
| Performance | 12 | 100% |
| Network | 12 | 100% |
| Tooling | 12 | 100% |
| Storage | 12 | 100% |
| Concurrency | 15 | 100% (with throttles) |
| Limits | 12 | 100% |
| System | 12 | 100% |
| **Receipt/Chain** | **20** | **100%** |
| **Capability** | **15** | **100%** |
| **Constraint** | **15** | **100%** |
| **Error Cases** | **50** | **100%** |
| **Redaction** | **20** | **100%** |
| **TOTAL** | **287** | **100%** |

### Performance Targets

| Operation | Target | Tolerance |
|-----------|--------|-----------|
| Single observation validation | <1ms | ±0.5ms |
| Batch validation (100 observations) | <100ms | ±20ms |
| Receipt chain validation (100 receipts) | <50ms | ±10ms |
| Input redaction | <0.5ms | ±0.2ms |
| Domain schema selection | <0.1ms | ±0.05ms |

### Compliance Validation

- [ ] OTEL spec compatibility (timestamp precision, format)
- [ ] BLAKE3 hash correctness (test vectors)
- [ ] UUID v4 compliance (RFC 4122)
- [ ] Error message clarity and actionability
- [ ] Redaction rule effectiveness (no leaks detected)

