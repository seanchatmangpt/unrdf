# OTEL Validation Harness - Implementation Reference

## Quick Start

```bash
# Run validation harness
npm run validate:otel

# Expected output (≥80/100 to pass):
# CHECK 1: AllObservationsHaveGuardStatus     [10/10] PASS ✓
# CHECK 2: NoForbiddenPayloads                [15/15] PASS ✓
# CHECK 3: AllReceiptsVerify                  [20/20] PASS ✓
# CHECK 4: DeterminismStable                  [15/15] PASS ✓
# CHECK 5: PerformanceSLA                     [10/10] PASS ✓
# CHECK 6: CompleteCoverage                   [15/15] PASS ✓
# CHECK 7: ErrorHandling                      [10/10] PASS ✓
# CHECK 8: GuardComprehensiveness              [5/5] PASS ✓
# ═════════════════════════════════════════════════════════
# TOTAL SCORE: 100/100 (100%) ✓ PASS
```

---

## Validation Harness Data Structures

### Input Schema: OTELTrace

```javascript
/*
ALGORITHM: OTELTrace Structure
INPUT: Raw OTEL telemetry from probe execution
OUTPUT: Typed trace object with all relevant spans

PURPOSE:
    Complete structured representation of probe execution.
    Enables comprehensive validation of all correctness aspects.
*/

interface OTELTrace {
  // Trace metadata
  traceId: string                    // Unique identifier (UUID)
  sessionId: string                  // Agent session
  timestamp: ISO8601                 // When trace started

  // Top-level spans (main workflow)
  spans: Span[]                      // All spans in execution order

  // Aggregated metrics
  summary: {
    totalSpans: number               // Count of all spans
    totalDuration: number            // ms from start to end
    errorCount: number               // Unhandled errors
    observationCount: number         // Total observations produced
    receiptCount: number             // Total receipts generated
  }
}

interface Span {
  // Identity
  traceId: string                    // Parent trace
  spanId: string                     // Unique span ID
  parentSpanId?: string              // Parent relationship

  // Metadata
  name: string                       // "scan" | "merge" | "verify" | etc
  startTime: ISO8601                 // When span started
  endTime: ISO8601                   // When span ended
  duration: number                   // milliseconds

  // Status
  status: SpanStatus                 // "OK" | "ERROR" | "TIMEOUT"
  statusMessage?: string             // Error description if status !== OK

  // Domain-specific attributes
  attributes: {
    // Generic
    domain?: string                  // Agent domain (runtime, fs, wasm, etc)
    operationType?: string           // Operation being performed

    // Observations
    observationId?: string           // Observation being processed
    observationCount?: number        // Observations in this span
    guardStatus?: "ALLOW" | "DENY"   // Guard decision

    // Receipts
    receiptId?: string               // Receipt being generated
    receiptHash?: string             // SHA256 hash of receipt
    observationHash?: string         // SHA256 of original observation

    // Merkle
    merkleRoot?: string              // Root hash of Merkle tree
    merklePathValid?: boolean        // Path verifies to root
    merklePathLength?: number        // Depth of verification path

    // Guards
    guardRuleId?: string             // Applied guard rule ID
    guardRuleType?: string           // Type: filesystem|env|network|custom
    denyReason?: string              // Why observation was denied

    // Performance
    runNumber?: number               // Which determinism run (1-10)
    outputHash?: string              // Hash of this run's output

    // Errors
    errorCode?: string               // Error classification
    errorMessage?: string            // Human readable error

    // Custom
    [key: string]: any               // Allow domain-specific attributes
  }

  // Events within span
  events: Event[]                    // Named events during execution

  // Links to other spans
  links?: Link[]                     // Causality references
}

interface Event {
  timestamp: ISO8601                 // When event occurred
  name: string                       // Event name
  attributes?: object                // Event metadata
}

interface Link {
  traceId: string                    // Reference to other span
  spanId: string
  type: string                       // "CHILD_OF" | "FOLLOWS_FROM"
}

type SpanStatus = "OK" | "ERROR" | "TIMEOUT" | "PARTIAL"
type ISO8601 = string                // "2025-12-27T10:00:00.000Z"
```

### Output Schema: ValidationReport

```javascript
/*
ALGORITHM: ValidationReport Structure
OUTPUT: Complete validation results with scoring breakdown
PURPOSE:
    Machine-readable validation results for CI/CD integration.
    Includes all checks, scores, and detailed explanations.
*/

interface ValidationReport {
  // Metadata
  reportId: string                   // Unique report identifier
  timestamp: ISO8601                 // When validation ran
  systemVersion: string              // Version of probe system

  // Overall result
  overallScore: number               // 0-100 final score
  overallStatus: "PASS" | "FAIL" | "PARTIAL"  // Success status
  passed: boolean                    // overallScore >= 80

  // Detailed checks
  checks: ValidationCheck[]          // Array of individual checks

  // Recommendations
  recommendations: {
    priority: "CRITICAL" | "HIGH" | "MEDIUM" | "LOW"
    message: string
    failingChecks: string[]          // Which checks failed
    suggestedActions: string[]       // How to fix
  }

  // Detailed metrics
  metrics: {
    observationCount: number
    receiptCount: number
    receiptVerificationRate: number  // 0.0-1.0
    forbiddenPayloadsFound: number
    deterministicRuns: number
    determinismVariance: number      // 0.0-1.0 (0 = perfect)
    domainsCovered: number           // 0-10
    guardRulesCovered: number        // Count of distinct rules tested
  }

  // Raw data for detailed analysis
  details: {
    checkDetails: CheckDetail[]
    violationExamples: ViolationExample[]
    performanceMetrics: PerformanceMetric[]
  }
}

interface ValidationCheck {
  // Check metadata
  checkId: string                    // Unique check identifier
  checkName: string                  // Human readable name
  maxPoints: number                  // Maximum points possible
  score: number                      // Actual points earned
  percentage: number                 // score / maxPoints * 100

  // Status
  status: "PASS" | "FAIL" | "PARTIAL" | "SKIP"  // Result
  statusMessage?: string             // Explanation if not PASS

  // Details
  details: {
    [key: string]: any               // Check-specific metrics
  }

  // Trace
  executedAt: ISO8601
  duration: number                   // ms to execute check
}

interface CheckDetail {
  checkName: string
  failurePoints: {
    issue: string
    evidence: any                     // Data supporting finding
    severity: "BLOCKER" | "CRITICAL" | "WARN" | "INFO"
  }[]
}

interface ViolationExample {
  type: "forbidden_payload" | "failed_receipt" | "missing_domain" | etc
  description: string
  evidence: object                    // Raw data
}

interface PerformanceMetric {
  operation: string                  // "scan" | "merge" | "verify"
  targetMs: number                   // SLA target
  actualMs: number                   // Actual duration
  status: "OK" | "SLOW" | "TIMEOUT"
}
```

---

## Check Implementation Reference

### Check 1: AllObservationsHaveGuardStatus (10 points)

```javascript
/*
PURPOSE: Verify every observation has guard status assigned
VALUE: Foundation - guard system must track all observations
TARGET: 100% observations have guard status in ["ALLOW", "DENY"]
*/

function checkAllObservationsHaveGuardStatus(observations) {
  const validStatuses = new Set(["ALLOW", "DENY"])
  const missing = observations.filter(o => !validStatuses.has(o.guardStatus))

  const score = Math.round(10 * (observations.length - missing.length) / observations.length)
  const status = missing.length === 0 ? "PASS" : "PARTIAL"

  return {
    checkId: "check_1",
    checkName: "AllObservationsHaveGuardStatus",
    maxPoints: 10,
    score,
    status,
    details: {
      total: observations.length,
      withStatus: observations.length - missing.length,
      missing: missing.length,
      coverage: ((observations.length - missing.length) / observations.length * 100).toFixed(2) + "%"
    }
  }
}

// Expected failures (corrected immediately):
// - New domain agent produces observations without guard status
// - Guard engine not invoked during merge
```

### Check 2: NoForbiddenPayloads (15 points)

```javascript
/*
PURPOSE: Verify no forbidden content leaked into allowed observations
VALUE: Security - critical for preventing data exfiltration
TARGET: 0 forbidden patterns in ALLOW-status observations
CRITICAL: If this fails, there's a guard bypass vulnerability
*/

function checkNoForbiddenPayloads(observations, forbiddenPatterns) {
  let violations = []

  for (const obs of observations) {
    if (obs.guardStatus === "DENY") continue  // Properly denied, OK

    for (const forbidden of forbiddenPatterns) {
      if (obs.domain === forbidden.domain &&
          contains(obs.data, forbidden.pattern)) {
        violations.push({
          observationId: obs.id,
          pattern: forbidden.pattern,
          domain: obs.domain
        })
      }
    }
  }

  const score = violations.length === 0 ? 15 : 0  // All or nothing
  const status = violations.length === 0 ? "PASS" : "FAIL"

  return {
    checkId: "check_2",
    checkName: "NoForbiddenPayloads",
    maxPoints: 15,
    score,
    status,
    details: {
      patternsChecked: forbiddenPatterns.length,
      observationsScanned: observations.length,
      violationsFound: violations.length,
      violations: violations.slice(0, 5)  // First 5 examples
    }
  }
}

// All-or-nothing: Partial credit not allowed (security boundary)
```

### Check 3: AllReceiptsVerify (20 points)

```javascript
/*
PURPOSE: Verify all receipts pass cryptographic validation
VALUE: Integrity - receipts prove observations haven't been tampered
TARGET: 100% of receipts have valid hash chains and Merkle paths
*/

function checkAllReceiptsVerify(receipts, merkleProof, observations) {
  let successCount = 0
  let failures = []

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i]
    const obs = observations.find(o => o.id === receipt.observationId)

    if (!obs) {
      failures.push({
        receiptId: receipt.id,
        issue: "No matching observation found"
      })
      continue
    }

    // Verify hash
    const expectedHash = SHA256(obs)
    if (receipt.observationHash !== expectedHash) {
      failures.push({
        receiptId: receipt.id,
        issue: "Hash mismatch",
        expected: expectedHash,
        actual: receipt.observationHash
      })
      continue
    }

    // Verify Merkle path (if included)
    if (receipt.merklePathValid === false) {
      failures.push({
        receiptId: receipt.id,
        issue: "Merkle path does not verify to root"
      })
      continue
    }

    successCount++
  }

  const rate = successCount / receipts.length
  const score = Math.round(20 * rate)
  const status = rate === 1.0 ? "PASS" : rate >= 0.95 ? "PARTIAL" : "FAIL"

  return {
    checkId: "check_3",
    checkName: "AllReceiptsVerify",
    maxPoints: 20,
    score,
    status,
    details: {
      total: receipts.length,
      verified: successCount,
      failed: failures.length,
      verificationRate: (rate * 100).toFixed(2) + "%",
      failureExamples: failures.slice(0, 3)
    }
  }
}

// Scoring: Full verification rate required for high score
```

### Check 4: DeterminismStable (15 points)

```javascript
/*
PURPOSE: Verify identical inputs produce identical outputs
VALUE: Reproducibility - same scan always produces same artifacts
TARGET: All n=10 deterministic runs produce matching hashes
*/

function checkDeterminismStable(deterministicRuns) {
  if (deterministicRuns.length < 2) {
    return {
      checkId: "check_4",
      checkName: "DeterminismStable",
      maxPoints: 15,
      score: 0,
      status: "SKIP",
      details: {
        reason: "Fewer than 2 determinism runs recorded",
        runsFound: deterministicRuns.length
      }
    }
  }

  const hashes = deterministicRuns.map(r => r.outputHash)
  const firstHash = hashes[0]
  let mismatches = 0

  for (let i = 1; i < hashes.length; i++) {
    if (hashes[i] !== firstHash) {
      mismatches++
    }
  }

  const stability = (hashes.length - mismatches) / hashes.length
  const score = mismatches === 0 ? 15 : Math.round(15 * stability)
  const status = mismatches === 0 ? "PASS" : "PARTIAL"

  return {
    checkId: "check_4",
    checkName: "DeterminismStable",
    maxPoints: 15,
    score,
    status,
    details: {
      runsExecuted: hashes.length,
      hashMatches: hashes.length - mismatches,
      hashMismatches: mismatches,
      stability: (stability * 100).toFixed(2) + "%"
    }
  }
}

// Red flag: Any variance indicates non-determinism (investigate!)
```

### Check 5: PerformanceSLA (10 points)

```javascript
/*
PURPOSE: Verify operations complete within time budgets
VALUE: Usability - probe must complete quickly for CI/CD integration
TARGET: scan <30s, merge <5s, verify <10s
*/

function checkPerformanceSLA(spans) {
  const slaTargets = {
    scan: 30000,    // 30 seconds
    merge: 5000,    // 5 seconds
    verify: 10000   // 10 seconds
  }

  let violations = []
  let pointLoss = 0

  for (const [operation, targetMs] of Object.entries(slaTargets)) {
    const span = spans.find(s => s.name === operation)

    if (!span) continue

    if (span.duration > targetMs) {
      violations.push({
        operation,
        target: targetMs,
        actual: span.duration,
        overage: span.duration - targetMs
      })
      pointLoss += 3  // Lose 3 points per SLA violation
    }
  }

  const score = Math.max(0, 10 - pointLoss)
  const status = violations.length === 0 ? "PASS" : "PARTIAL"

  return {
    checkId: "check_5",
    checkName: "PerformanceSLA",
    maxPoints: 10,
    score,
    status,
    details: {
      targets: slaTargets,
      violations: violations.length,
      violations
    }
  }
}

// Andon: Investigate if performance drops - don't just increase timeout!
```

### Check 6: CompleteCoverage (15 points)

```javascript
/*
PURPOSE: Verify all 10 domains produced observations
VALUE: Completeness - no domain should be completely missing
TARGET: 100% domain coverage (all 10 domains represented)
*/

function checkCompleteCoverage(observations) {
  const requiredDomains = [
    "runtime", "filesystem", "wasm", "environment",
    "network", "custom_1", "custom_2", "custom_3",
    "logging", "metrics"
  ]

  const foundDomains = new Set()
  const domainCounts = {}

  for (const obs of observations) {
    foundDomains.add(obs.domain)
    domainCounts[obs.domain] = (domainCounts[obs.domain] || 0) + 1
  }

  const missingDomains = requiredDomains.filter(d => !foundDomains.has(d))
  const coverage = (requiredDomains.length - missingDomains.length) / requiredDomains.length
  const score = Math.round(15 * coverage)
  const status = missingDomains.length === 0 ? "PASS" : "PARTIAL"

  return {
    checkId: "check_6",
    checkName: "CompleteCoverage",
    maxPoints: 15,
    score,
    status,
    details: {
      required: requiredDomains.length,
      found: foundDomains.size,
      missing: missingDomains,
      domainCounts,
      coverage: (coverage * 100).toFixed(2) + "%"
    }
  }
}

// Missing domains indicate agent shard not running (check CI logs)
```

### Check 7: ErrorHandling (10 points)

```javascript
/*
PURPOSE: Verify no unhandled errors during execution
VALUE: Robustness - system must handle errors gracefully
TARGET: No unexpected errors (expected errors OK)
*/

function checkErrorHandling(spans) {
  const errorSpans = spans.filter(s => s.status === "ERROR")
  const expectedErrors = errorSpans.filter(s => s.attributes.expectedError === true)
  const unexpectedErrors = errorSpans.length - expectedErrors.length

  let pointLoss = 0

  for (const errorSpan of errorSpans) {
    if (!errorSpan.attributes.expectedError) {
      pointLoss += 2  // Lose 2 points per unexpected error
    }
  }

  const score = Math.max(0, 10 - pointLoss)
  const status = unexpectedErrors === 0 ? "PASS" : "PARTIAL"

  return {
    checkId: "check_7",
    checkName: "ErrorHandling",
    maxPoints: 10,
    score,
    status,
    details: {
      totalErrors: errorSpans.length,
      expectedErrors: expectedErrors.length,
      unexpectedErrors,
      errorDetails: errorSpans.map(s => ({
        spanId: s.spanId,
        message: s.statusMessage,
        expected: s.attributes.expectedError
      }))
    }
  }
}

// Investigate each unexpected error immediately
```

### Check 8: GuardComprehensiveness (5 points)

```javascript
/*
PURPOSE: Verify all guard rule types tested
VALUE: Security - coverage ensures all guard types work
TARGET: Tests cover filesystem, environment, network, custom rules
*/

function checkGuardComprehensiveness(spans) {
  const expectedTypes = ["filesystem", "environment", "network", "custom"]
  const guardSpans = spans.filter(s => s.name.includes("guard"))

  const foundTypes = new Set()

  for (const span of guardSpans) {
    if (span.attributes.guardRuleType) {
      foundTypes.add(span.attributes.guardRuleType)
    }
  }

  const coverage = foundTypes.size / expectedTypes.length
  const score = Math.round(5 * coverage)
  const status = coverage === 1.0 ? "PASS" : "PARTIAL"

  return {
    checkId: "check_8",
    checkName: "GuardComprehensiveness",
    maxPoints: 5,
    score,
    status,
    details: {
      expected: expectedTypes,
      found: Array.from(foundTypes),
      coverage: (coverage * 100).toFixed(2) + "%"
    }
  }
}

// Missing rule types: add test cases for untested rules
```

---

## Scoring Summary

```
┌─────────────────────────────────────────────────────────────┐
│ OTEL VALIDATION SCORING BREAKDOWN                            │
├─────────────────────────────────────────────────────────────┤
│ CHECK 1: AllObservationsHaveGuardStatus           10 points  │
│ CHECK 2: NoForbiddenPayloads                      15 points  │
│ CHECK 3: AllReceiptsVerify                        20 points  │
│ CHECK 4: DeterminismStable                        15 points  │
│ CHECK 5: PerformanceSLA                           10 points  │
│ CHECK 6: CompleteCoverage                         15 points  │
│ CHECK 7: ErrorHandling                            10 points  │
│ CHECK 8: GuardComprehensiveness                    5 points  │
├─────────────────────────────────────────────────────────────┤
│ TOTAL POSSIBLE                                   100 points  │
│ PASS THRESHOLD                                     80 points  │
│ FAIL THRESHOLD                                      0 points  │
└─────────────────────────────────────────────────────────────┘

Success Scenarios:
  100/100: Perfect execution - all checks pass
  80-99:   Good execution - minor issues found and addressed
  60-79:   Warn - investigate failures before merging
  0-59:    Fail - do not merge, blocking critical issues
```

---

## Integration with CI/CD

### GitHub Actions Workflow

```yaml
name: OTEL Validation

on:
  pull_request:
    branches: [main, develop]
  push:
    branches: [main, develop]

jobs:
  validate:
    runs-on: ubuntu-latest
    timeout-minutes: 15

    steps:
      - uses: actions/checkout@v3

      - name: Setup Node
        uses: actions/setup-node@v3
        with:
          node-version: '18'

      - name: Install dependencies
        run: npm ci

      - name: Run tests (with OTEL)
        run: npm test -- --collect-coverage

      - name: Run OTEL validation
        run: npm run validate:otel > validation-output.log

      - name: Check OTEL score
        run: |
          SCORE=$(grep "TOTAL SCORE:" validation-output.log | grep -oP '\d+(?=/100)')
          echo "OTEL Validation Score: $SCORE/100"
          if [ "$SCORE" -lt 80 ]; then
            echo "❌ FAILED: Score below 80 threshold"
            cat validation-output.log
            exit 1
          fi
          echo "✅ PASSED: Score is $SCORE/100"

      - name: Upload validation report
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: otel-validation-report
          path: validation-output.log

      - name: Upload coverage
        if: always()
        uses: codecov/codecov-action@v3
        with:
          files: ./coverage/coverage-final.json
```

### Local Validation

```bash
# Run all validation
npm run validate:otel

# Run with detailed output
npm run validate:otel -- --verbose

# Generate HTML report
npm run validate:otel -- --html > report.html

# CI mode (exit code reflects score)
npm run validate:otel -- --ci
# Exit codes: 0 (≥80), 1 (60-79), 2 (<60)
```

---

## Troubleshooting

### Score <80

1. **Check which validation failed**: Review `validation-output.log`
2. **Run failing check in isolation**: `npm run validate:otel -- --check=check_N`
3. **Collect more detailed OTEL data**: `DEBUG=otel:* npm test`
4. **Compare against baseline**: `npm run validate:otel -- --baseline=last-good`

### Determinism Variance (Check 4)

```
Possible causes:
1. Non-deterministic time sources (use frozen time in tests)
2. Hash order dependency (use sorted maps)
3. Random seeding not fixed (use seed parameter)
4. Concurrency race conditions (use sequential execution)

Fix:
1. FreezeEnvironment() sets deterministic time
2. Verify all collections sorted
3. Set RANDOM_SEED environment variable
4. Check span ordering in logs
```

### Forbidden Payloads Found (Check 2)

```
CRITICAL: This is a security issue!

Action items:
1. Identify the forbidden pattern that leaked
2. Check guard rule configuration
3. Verify guard engine invoked before merge
4. Add pattern to testcases
5. Do NOT merge until fixed (use --force-merge at own risk)
```

### Missing Domains (Check 6)

```
Likely causes:
1. Agent shard crashed (check agent logs)
2. Agent not registered in config
3. Agent skipped due to filter
4. Observation count = 0 (agent produced nothing)

Debug:
npm run debug:agent --agent=filesystem
npm run debug:agent --agent=runtime
(etc for each missing domain)
```

---

## References

- OTEL Specification: https://opentelemetry.io/docs/concepts/
- Test Strategy: `/home/user/unrdf/docs/test-validation-strategy.md`
- Fixture Definition: `/home/user/unrdf/docs/test-fixtures.md`
- CLAUDE.md: Quality standards and requirements
