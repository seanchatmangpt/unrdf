# SPARC Pseudocode Phase - Complete Deliverables

**Agent:** Agent-9 (Test/Validation)
**Methodology:** SPARC Pseudocode Phase
**Date:** 2025-12-27
**Status:** Complete

---

## Executive Summary

This document consolidates the complete test and validation strategy for the KGC probe package system, bridging specifications into implementable pseudocode following the SPARC methodology.

**Deliverables:**
1. **Test Strategy** - Architecture, 5 essential tests, coverage matrix
2. **OTEL Validation Harness** - 8 validation checks with scoring (≥80/100 threshold)
3. **Test Fixtures** - Complete fixture definitions for reproducible testing
4. **Implementation Roadmap** - 4-week execution plan with timeline

**Key Metrics:**
- **OTEL Validation Score Target:** ≥80/100
- **Code Coverage Target:** ≥85%
- **Test Pass Rate:** 100%
- **Determinism Variance:** 0%
- **Guard Coverage:** 100%
- **Forbidden Payload Escapes:** 0

---

## Part 1: Test Strategy (Comprehensive)

**Document:** `/home/user/unrdf/docs/test-validation-strategy.md`

### Test Architecture

**Pyramid Structure:**
```
Essential Tests (5) ← HIGH VALUE, LOW MAINTENANCE
├── Determinism (Hash Stability)
├── Guard Enforcement (Deny Receipts)
├── Merge Correctness (Shard Integration)
├── Receipt Verification (Cryptographic)
└── E2E Integration (Full Workflow)

Supporting Tests (15+)
├── Unit Tests (Agent shards, modules)
├── Integration Tests (Component interactions)
├── Property Tests (Invariants)
└── Performance Tests (SLA verification)
```

### Five Essential Tests (Pseudocode)

#### Test 1: Determinism - Hash Stability

```
ALGORITHM: TestDeterminismHashStability
PURPOSE: Verify identical inputs produce identical outputs across 10 runs

INPUT: testFixture (frozen test environment)
OUTPUT: result {status, runs: 10, variance: 0}

PROCESS:
  1. FreezeEnvironment(testFixture)
  2. FOR run 1 to 10:
     - RunProbeScan(testFixture)
     - hash ← SHA256(output)
     - store hash
  3. Verify all 10 hashes identical
  4. RETURN {status: PASS, variance: 0}

SUCCESS CRITERIA:
  ✓ All 10 hashes match exactly
  ✓ Merkle roots identical
  ✓ Variance: 0%
  ✓ No flakiness
```

#### Test 2: Guard Enforcement - Deny Receipts

```
ALGORITHM: TestGuardEnforcementDenyReceipts
PURPOSE: Verify forbidden observations denied with valid receipts

INPUT: guardRules (config), forbiddenObservations (array)
OUTPUT: result {status, guardsCovered: N, receipts: array}

PROCESS:
  1. FOR EACH guard rule (filesystem, env, network, custom):
     - Create forbidden observation
     - ApplyGuards(observation)
     - ASSERT status === "DENY"
     - ASSERT receipt.hash valid
     - ASSERT receipt.denyReason populated
  2. RETURN {status: PASS, guardsCovered: 100%}

SUCCESS CRITERIA:
  ✓ All guards enforced
  ✓ 100% rules covered
  ✓ Valid deny receipts
  ✓ No forbidden payloads leaked
```

#### Test 3: Merge Correctness - Shard Integration

```
ALGORITHM: TestMergeCorrectnessShardsIntegration
PURPOSE: Verify 10 shards merge correctly with cryptographic validation

INPUT: shards (10 pre-computed shard objects)
OUTPUT: result {status, totalShards: 10, merkleVerified: true}

PROCESS:
  1. Load 10 domain shards from fixtures
  2. MergeShardsWithVerification(shards)
  3. Verify:
     - observationCount = SUM(shard.count)
     - MerkleRoot(observations) matches proof.root
     - AllReceiptChains verify
     - Merge deterministic (same input = same output)
  4. Test partial merge (9 shards) - handles gracefully
  5. RETURN {status: PASS, totalShards: 10}

SUCCESS CRITERIA:
  ✓ Observation count correct
  ✓ Merkle root validates
  ✓ All receipts chain
  ✓ Deterministic merge
  ✓ Partial merge handled
```

#### Test 4: Receipt Verification - Cryptographic Validation

```
ALGORITHM: TestReceiptVerificationCryptographic
PURPOSE: Verify receipt integrity through cryptographic validation

INPUT: receipts (array), merkleProof (root + paths), observations (array)
OUTPUT: result {status, verified: 100%, paths: all valid}

PROCESS:
  1. FOR EACH observation:
     - ASSERT receipt.hash === SHA256(observation)
  2. FOR EACH merkle path:
     - Reconstruct root from observation + path
     - ASSERT reconstructed === merkleProof.root
  3. FOR EACH receipt pair:
     - current.previousHash === SHA256(previous)
  4. ASSERT all timestamps ordered chronologically
  5. RETURN {status: PASS, verified: 100%}

SUCCESS CRITERIA:
  ✓ All hashes match SHA256
  ✓ Merkle paths reconstruct root
  ✓ Receipt chains linked
  ✓ Timestamps valid & ordered
  ✓ Guard receipts structured correctly
```

#### Test 5: E2E Integration - Full Workflow

```
ALGORITHM: TestE2EIntegrationFullWorkflow
PURPOSE: Execute complete workflow: scan → merge → verify

INPUT: projectPath (test project), expectedOutcomes (baseline)
OUTPUT: result {status, observationCount, verification: valid}

PROCESS:
  1. EXECUTE: kgc probe scan
     - ASSERT exitCode === 0
     - Load observations, receipts, merkleProof
     - ASSERT artifacts exist
  2. EXECUTE: kgc probe verify
     - ASSERT exitCode === 0
     - ASSERT allHashesValid === true
     - ASSERT allPathsValid === true
  3. VALIDATE:
     - observationCount ≥ baseline
     - All 10 domains present
     - NO forbidden patterns in observations
     - guardEnforced === true
  4. CHECK performance: scan <30s, verify <10s
  5. RETURN {status: PASS, workflow: complete}

SUCCESS CRITERIA:
  ✓ Full scan completes
  ✓ All artifacts generated
  ✓ Verification succeeds
  ✓ No forbidden data present
  ✓ All domains covered
  ✓ SLAs met (<40s total)
```

### Test Categories & Coverage

```
UNIT TESTS (85%+ coverage per module)
├── Runtime agent
├── FileSystem agent
├── WASM agent
├── Environment agent
├── Network agent
├── Custom agents (3)
├── Logging agent
├── Metrics agent
└── Hash/Crypto utilities

INTEGRATION TESTS (80%+ path coverage)
├── Merge logic
├── Receipt validation chains
├── Guard rule application
├── Hash verification

E2E TESTS
├── Complete `kgc probe scan`
├── Complete `kgc probe verify`
├── Artifact file generation
└── End-to-end data flow

DETERMINISM TESTS
├── 10 identical runs
├── Bit-for-bit hash matching
└── Variance measurement

GUARD TESTS (100% rule coverage)
├── FileSystem path guards
├── Environment variable guards
├── Network access guards
└── Custom rule guards
```

### Coverage Targets

```
Code Coverage:         ≥85% overall, ≥90% critical paths
Observation Coverage:  100% (all 10 domains)
Guard Coverage:        100% (all rule types)
Error Path Coverage:   100% (all exception types)
Edge Case Coverage:    100% (boundaries, nulls, empty)
```

---

## Part 2: OTEL Validation Harness

**Document:** `/home/user/unrdf/docs/otel-validation-harness.md`

### Scoring Breakdown

```
POINT ALLOCATION (Total: 100 points)
┌─────────────────────────────────────┐
│ CHECK 1: Guard Status (10 points)   │
│   All observations have ALLOW/DENY  │
├─────────────────────────────────────┤
│ CHECK 2: Forbidden Payloads (15 pts)│
│   Zero escaped forbidden patterns   │
├─────────────────────────────────────┤
│ CHECK 3: Receipt Verification (20)  │
│   All receipts cryptographically OK │
├─────────────────────────────────────┤
│ CHECK 4: Determinism (15 points)    │
│   10 runs produce identical hashes  │
├─────────────────────────────────────┤
│ CHECK 5: Performance SLA (10 points) │
│   scan <30s, merge <5s, verify <10s │
├─────────────────────────────────────┤
│ CHECK 6: Domain Coverage (15 points) │
│   All 10 domains produce obs        │
├─────────────────────────────────────┤
│ CHECK 7: Error Handling (10 points)  │
│   No unexpected unhandled errors    │
├─────────────────────────────────────┤
│ CHECK 8: Guard Comprehensiveness (5)│
│   All guard rule types tested       │
└─────────────────────────────────────┘
      TOTAL: 100 points
      PASS THRESHOLD: ≥80/100
```

### Validation Checks (Implementation Summary)

```
CHECK 1: AllObservationsHaveGuardStatus (10 points)
  Requirement:   Every observation has guardStatus ∈ {ALLOW, DENY}
  Measurement:   Coverage ratio (observations with status / total)
  Scoring:       10 * (covered / total)
  Fail When:     Any observation missing status

CHECK 2: NoForbiddenPayloads (15 points)
  Requirement:   Zero forbidden patterns in ALLOW observations
  Patterns:      AWS creds, SSH keys, /etc/passwd, DB passwords, etc
  Measurement:   Violation count
  Scoring:       All-or-nothing (0 or 15, no partial)
  Fail When:     Even one pattern found in allowed obs
  CRITICAL:      Security boundary - don't compromise

CHECK 3: AllReceiptsVerify (20 points)
  Requirement:   100% of receipts pass cryptographic validation
  Validation:    SHA256 hashes, Merkle paths, receipt chains
  Measurement:   Receipt verification success rate
  Scoring:       20 * (verified / total)
  Fail When:     Any receipt fails to verify

CHECK 4: DeterminismStable (15 points)
  Requirement:   10 identical runs produce identical hashes
  Measurement:   Unique hash count across 10 runs
  Scoring:       15 * (matching_runs / 10)
  Fail When:     Any run produces different hash
  Red Flag:      Indicates non-determinism bug

CHECK 5: PerformanceSLA (10 points)
  Requirement:   Operations complete within time budgets
  Budgets:       scan <30s, merge <5s, verify <10s
  Measurement:   Actual duration vs target
  Scoring:       10 - (3 * violation_count)
  Fail When:     Multiple SLA violations

CHECK 6: CompleteCoverage (15 points)
  Requirement:   All 10 domains produce observations
  Domains:       runtime, filesystem, wasm, environment, network,
                 custom_1, custom_2, custom_3, logging, metrics
  Measurement:   Domain count
  Scoring:       15 * (found_domains / 10)
  Fail When:     Any domain completely missing

CHECK 7: ErrorHandling (10 points)
  Requirement:   No unexpected unhandled errors
  Measurement:   Unexpected error count
  Scoring:       10 - (2 * unexpected_errors)
  Fail When:     Multiple unexpected errors

CHECK 8: GuardComprehensiveness (5 points)
  Requirement:   Test coverage of all guard rule types
  Types:         filesystem, environment, network, custom
  Measurement:   Rule type coverage
  Scoring:       5 * (covered_types / 4)
  Fail When:     Any rule type untested
```

### Success Criteria

```
SCORE ≥80/100:  PASS ✓
  - System meets quality threshold
  - Safe to merge
  - Proceed to production

SCORE 60-79:    WARN ⚠
  - Investigate failures
  - Likely minor issues
  - Fix before merge

SCORE <60:      FAIL ✗
  - Critical issues detected
  - Do not merge
  - Requires investigation
```

### CI/CD Integration

```bash
# GitHub Actions workflow on PR:
1. npm install
2. npm test -- --collect-coverage
3. npm run validate:otel > validation-report.json
4. Check: SCORE ≥ 80
   - If YES: ✓ PASS, approve
   - If NO:  ✗ FAIL, block merge

# Manual validation:
npm run validate:otel
npm run validate:otel -- --verbose
npm run validate:otel -- --html > report.html
```

---

## Part 3: Test Fixtures

**Document:** `/home/user/unrdf/docs/test-fixtures.md`

### Fixture Categories

```
FIXTURE 1: FrozenEnvironment (Determinism)
  File:     tests/fixtures/frozen-environment.json
  Purpose:  Capture identical test state for reproduction
  Contents: Files, env vars, system time, process info
  Usage:    TestDeterminismHashStability (Test 1)

FIXTURE 2: PrecalculatedShards (Merge Testing)
  Files:    tests/fixtures/shards/[domain]_shard_1.json (10 files)
  Purpose:  Known shard outputs for merge verification
  Contents: Observations (per domain), receipts, hashes
  Usage:    TestMergeCorrectnessShardsIntegration (Test 3)

FIXTURE 3: GuardTestCases (Guard Enforcement)
  File:     tests/fixtures/guard-test-cases.json
  Purpose:  Forbidden observation scenarios
  Contents: 9 test cases (7 deny, 2 allow)
  Usage:    TestGuardEnforcementDenyReceipts (Test 2)

FIXTURE 4: ReceiptChainData (Receipt Verification)
  File:     tests/fixtures/receipt-chain.json
  Purpose:  Pre-computed receipt chains for crypto validation
  Contents: Observations, receipts, Merkle proof with paths
  Usage:    TestReceiptVerificationCryptographic (Test 4)

FIXTURE 5: RealProjectSnapshot (E2E Testing)
  Dir:      tests/fixtures/test-projects/real-project-1/
  Purpose:  Realistic project for CLI testing
  Contents: Package.json, source files, .env, secrets.json
  Usage:    TestE2EIntegrationFullWorkflow (Test 5)

FIXTURE 6: E2EBaseline (Validation)
  File:     tests/fixtures/baselines/e2e-baseline.json
  Purpose:  Expected results for E2E validation
  Contents: Expected counts, domains, performance targets
  Usage:    Test 5 assertion validation
```

### Fixture Directory Structure

```
tests/
├── fixtures/
│   ├── frozen-environment.json          ← Test 1 setup
│   ├── guard-test-cases.json            ← Test 2 data
│   ├── receipt-chain.json               ← Test 4 data
│   ├── baselines/
│   │   └── e2e-baseline.json            ← Test 5 validation
│   ├── shards/                          ← Test 3 data (10 files)
│   │   ├── runtime_shard_1.json
│   │   ├── filesystem_shard_1.json
│   │   ├── wasm_shard_1.json
│   │   ├── environment_shard_1.json
│   │   ├── network_shard_1.json
│   │   ├── custom_1_shard_1.json
│   │   ├── custom_2_shard_1.json
│   │   ├── custom_3_shard_1.json
│   │   ├── logging_shard_1.json
│   │   └── metrics_shard_1.json
│   └── test-projects/                   ← Test 5 project
│       └── real-project-1/
│           ├── package.json
│           ├── src/
│           ├── config/
│           └── ...
└── helpers/
    ├── fixture-loader.mjs               ← Load fixtures
    ├── environment-freezer.mjs          ← Setup frozen env
    └── hash-validator.mjs               ← Verify hashes
```

---

## Part 4: Implementation Roadmap

**Document:** `/home/user/unrdf/docs/test-implementation-roadmap.md`

### 4-Week Timeline

```
WEEK 1 (Days 1-5):    FOUNDATION
├── Day 1-2:   Jest setup, test infrastructure
├── Day 3-4:   Fixture system, fixture loaders
├── Day 5:     OTEL infrastructure, tracer
└── Target:    npm test command functional

WEEK 2 (Days 6-15):   5 ESSENTIAL TESTS
├── Day 6-7:   Test 1 - Determinism (10 runs, 0% variance)
├── Day 8-9:   Test 2 - Guard Enforcement (100% rules)
├── Day 10-11: Test 3 - Merge Correctness (10 shards)
├── Day 12-13: Test 4 - Receipt Verification (crypto validation)
├── Day 14-15: Test 5 - E2E Integration (full workflow)
└── Target:    All 5 tests passing, fixtures loaded

WEEK 3 (Days 16-20):  OTEL VALIDATION
├── Day 16-17: Implement 8 validation checks
├── Day 18:    Scoring engine & harness
├── Day 19:    GitHub Actions integration
├── Day 20:    CI/CD gates configured
└── Target:    npm run validate:otel works, score ≥80

WEEK 4 (Days 21-28):  COVERAGE & DOCUMENTATION
├── Day 21-22: Fill code coverage gaps (→85%)
├── Day 23-24: Observation coverage (all 10 domains)
├── Day 25:    Guard coverage (100% rules)
├── Day 26-28: Documentation complete
└── Target:    85%+ coverage, all docs, quality gates pass
```

### Phase Deliverables

```
PHASE 1 DELIVERABLES (Foundation):
  ✓ Jest/Vitest configured
  ✓ Test infrastructure complete
  ✓ Fixture loaders working
  ✓ OTEL instrumentation setup
  ✓ npm test runs successfully

PHASE 2 DELIVERABLES (Essential Tests):
  ✓ Test 1: Determinism stable (0% variance)
  ✓ Test 2: Guards enforced (100% rules)
  ✓ Test 3: Merge correct (10 shards)
  ✓ Test 4: Receipts verify (cryptographic)
  ✓ Test 5: E2E workflow complete
  ✓ All fixtures loaded
  ✓ All tests passing (5/5)

PHASE 3 DELIVERABLES (OTEL Validation):
  ✓ 8 validation checks implemented
  ✓ Scoring engine functional
  ✓ Validation harness complete
  ✓ GitHub Actions workflow
  ✓ CI/CD quality gates
  ✓ npm run validate:otel works

PHASE 4 DELIVERABLES (Coverage & Docs):
  ✓ Code coverage ≥85%
  ✓ Observation coverage: all 10 domains
  ✓ Guard coverage: 100% of rules
  ✓ Documentation complete
  ✓ Troubleshooting guide
  ✓ Fixture maintenance docs
  ✓ Quality gates enforced
```

---

## Summary Table: Key Metrics

| Metric | Target | Validation Method |
|--------|--------|------------------|
| **OTEL Validation Score** | ≥80/100 | `npm run validate:otel` |
| **Test Pass Rate** | 100% | `npm test` output |
| **Code Coverage** | ≥85% | Jest coverage report |
| **Observation Coverage** | 100% | All 10 domains present |
| **Guard Coverage** | 100% | All rule types tested |
| **Determinism Variance** | 0% | 10 runs, identical hashes |
| **Performance (scan)** | <30s | OTEL span duration |
| **Performance (verify)** | <10s | OTEL span duration |
| **Receipt Verification** | 100% | Cryptographic validation |
| **Forbidden Payload Escapes** | 0 | Pattern matching scan |

---

## Integration with CLAUDE.md

**From project instructions:**

```
Adversarial PM - Core Principle:
✓ "Did you RUN it?" - All tests executed, output verified
✓ "Can you PROVE it?" - OTEL spans + test output required
✓ "What BREAKS if wrong?" - Guard bypass, determinism loss, etc
✓ "What's the EVIDENCE?" - Detailed test reports + metrics

Quality Standards:
✓ OTEL Validation ≥80/100 (Trust only OTEL, not claims)
✓ Code Coverage ≥85% (Measured, not estimated)
✓ Test Pass Rate 100% (Zero flakes, zero failures)
✓ Batch operations in one message (Concurrent execution)
✓ Timeout all commands (5s default, justified exceptions)
✓ Measure, don't assume (Run commands, read output)
```

**Alignment:**

```
✓ All tests MUST pass for PR merge
✓ OTEL score MUST be ≥80/100 (not estimated)
✓ Coverage MUST be ≥85% (measured by Jest)
✓ Determinism MUST have 0% variance
✓ Guard rules MUST be 100% tested
✓ No forbidden payloads (security boundary)
```

---

## File References

| Document | Path | Purpose |
|----------|------|---------|
| Test Strategy | `docs/test-validation-strategy.md` | Full architecture, 5 tests, coverage |
| OTEL Harness | `docs/otel-validation-harness.md` | 8 checks, scoring, CI/CD |
| Fixtures | `docs/test-fixtures.md` | Fixture definitions & helpers |
| Roadmap | `docs/test-implementation-roadmap.md` | 4-week timeline & checklist |
| **This File** | `docs/SPARC-PSEUDOCODE-DELIVERABLES.md` | Summary & quick reference |

---

## Quick Start

### For New Developer

1. **Read this file** (5 min) - Overview
2. **Read test-validation-strategy.md** (20 min) - Understand test architecture
3. **Review test-fixtures.md** (15 min) - Understand test data
4. **Implement Phase 1** (Days 1-5) - Setup infrastructure
5. **Implement Phase 2** (Days 6-15) - Build 5 essential tests

### For Running Tests

```bash
# Run all tests
npm test

# Run with coverage
npm test -- --collect-coverage

# Run OTEL validation
npm run validate:otel

# Expected output:
# OTEL Score: X/100
# Coverage: Y%
# All tests: PASS
```

### For Debugging Failures

```bash
# Run single test
npm test -- --testNamePattern="Determinism"

# Get detailed output
npm test -- --verbose

# Generate HTML coverage report
npm test -- --collect-coverage && open coverage/index.html

# Run with debugging
DEBUG=* npm test
```

---

## Next Steps

1. **Review all four documents** in order listed above
2. **Create Phase 1 project** with tasks from Implementation Roadmap
3. **Begin with Foundation** (Jest setup, fixtures, OTEL)
4. **Implement tests sequentially** (one test every 2 days)
5. **Monitor OTEL score** throughout (target ≥80/100)
6. **Verify all gates** before declaring complete

---

## Completion Checklist

Before marking "DONE":

### Evidence Required
- [ ] OTEL score ≥80/100 (show output)
- [ ] Test pass rate 100% (show npm test output)
- [ ] Code coverage ≥85% (show Jest report)
- [ ] All 5 essential tests passing
- [ ] Determinism variance 0% (10 runs identical)
- [ ] Guard coverage 100% (all rules tested)
- [ ] Observation coverage 100% (all 10 domains)
- [ ] All documentation complete
- [ ] CI/CD gates configured and passing

### Process Quality
- [ ] All operations concurrent (single messages)
- [ ] Timeouts on all commands (5s default)
- [ ] MEASURED not assumed (run commands, read output)
- [ ] Fixtures validated before use
- [ ] OTEL spans collected and analyzed
- [ ] No flaky tests (run 10x)
- [ ] Reproducible from scratch

### Questions Before "Done"
- [ ] Did I RUN every command or just write code?
- [ ] Did I read FULL output or stop at first ✓?
- [ ] Can I REPRODUCE from scratch?
- [ ] What BREAKS if I'm wrong?
- [ ] Can I PROVE it with OTEL?

---

## Related Documentation

- **CLAUDE.md** - Project quality standards
- **bb80-20-methodology.md** - Methodology details
- **test-validation-strategy.md** - Complete test strategy
- **otel-validation-harness.md** - Validation reference
- **test-fixtures.md** - Fixture specifications
- **test-implementation-roadmap.md** - Execution timeline

---

**Author:** Agent-9 (Test/Validation Specialist)
**Methodology:** SPARC Pseudocode Phase
**Date:** 2025-12-27
**Status:** Complete & Ready for Implementation
