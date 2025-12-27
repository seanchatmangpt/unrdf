# Test Implementation Roadmap - SPARC Phase

## Overview

This document provides a concise implementation roadmap for the complete test and validation system for the KGC probe package. It synthesizes the test strategy, OTEL validation, and fixtures into an executable plan.

**Total Effort Estimate:** 4 weeks (240 hours)
**Team:** Test Engineer (Agent-9), Backend Dev (Agent-1), Code Reviewer (Agent-3)
**Success Criteria:** OTEL score ≥80/100, 85%+ code coverage, 100% test pass rate

---

## Quick Navigation

| Document | Purpose | Location |
|----------|---------|----------|
| Test Strategy | Architecture, 5 essential tests, coverage targets | `docs/test-validation-strategy.md` |
| OTEL Harness | Validation checks, scoring, CI/CD integration | `docs/otel-validation-harness.md` |
| Fixtures | Test data definitions and helpers | `docs/test-fixtures.md` |
| **This File** | Implementation timeline and checklist | `docs/test-implementation-roadmap.md` |

---

## Phase 1: Foundation (Days 1-5)

### Objective
Establish test infrastructure and core testing utilities.

### Tasks

#### Task 1.1: Test Runner Configuration
```
DELIVERABLE: Working Jest/Vitest setup

Steps:
1. Initialize test runner
   - Install Jest with TypeScript support
   - Configure test environment (Node.js, jsdom)
   - Setup coverage reporting (>80% target)
   - Create test configuration file

2. Setup test helpers
   - Create test utilities for common operations
   - Implement fixture loading helpers
   - Create hash/crypto test utilities
   - Implement mock factories

3. Configure CI pre-commit
   - Add pre-commit hook that runs tests
   - Ensure <30s execution time
   - Add coverage reporting

FILES TO CREATE:
  - jest.config.js
  - tests/setup.js
  - tests/helpers/fixture-loader.mjs
  - tests/helpers/crypto-utils.mjs
  - tests/helpers/mock-factories.mjs

SUCCESS CRITERIA:
  - $ npm test executes successfully
  - Coverage reports generated
  - Can run single test in <5s
```

#### Task 1.2: Fixture Infrastructure
```
DELIVERABLE: Fixture loading and validation system

Steps:
1. Create fixture directories
   - tests/fixtures/frozen-environment.json
   - tests/fixtures/shards/
   - tests/fixtures/guard-test-cases.json
   - tests/fixtures/receipt-chain.json
   - tests/fixtures/test-projects/real-project-1/
   - tests/fixtures/baselines/

2. Implement fixture loaders
   - loadFixture(name) - Load single fixture
   - loadAllShards() - Load all 10 domain shards
   - validateFixture(data) - Validate schema
   - computeFixtureHashes() - Pre-compute hashes

3. Create fixture validators
   - Verify JSON schema compliance
   - Verify hash consistency
   - Verify observation counts
   - Generate validation reports

4. Document fixture usage
   - Create fixture reference
   - Add examples to README
   - Document maintenance process

FILES TO CREATE:
  - tests/helpers/fixture-loader.mjs
  - tests/helpers/fixture-validator.mjs
  - tests/fixtures/frozen-environment.json
  - tests/fixtures/shards/*.json (all 10)
  - tests/fixtures/guard-test-cases.json
  - tests/fixtures/receipt-chain.json
  - tests/fixtures/baselines/e2e-baseline.json

SUCCESS CRITERIA:
  - All fixtures load without errors
  - Fixture validation passes
  - Hashes match pre-computed values
  - Can load fixtures in <100ms
```

#### Task 1.3: OTEL Infrastructure
```
DELIVERABLE: OTEL instrumentation and span collection

Steps:
1. Setup OpenTelemetry
   - Install @opentelemetry packages
   - Configure tracing provider
   - Setup span exporters
   - Configure sampling strategy

2. Create OTEL utilities
   - createSpan(name) - Create new span
   - recordAttribute(key, value) - Add to span
   - recordEvent(name, attributes) - Log event
   - endSpan() - Complete span

3. Instrument probe system
   - Add spans to: scan, merge, verify operations
   - Record attributes: domain, count, hash, status
   - Record events: milestone transitions
   - Record errors: with error code

4. Create span exporter
   - Export spans to JSON file
   - Implement trace aggregation
   - Create trace reader

FILES TO CREATE:
  - config/otel.config.js
  - src/otel/tracer.mjs
  - src/otel/span-helpers.mjs
  - src/otel/span-exporter.mjs
  - tests/helpers/otel-reader.mjs

SUCCESS CRITERIA:
  - Spans generated during probe execution
  - All critical paths instrumented
  - Spans exported to JSON
  - Can read and parse spans
```

---

## Phase 2: Essential Tests (Days 6-15)

### Objective
Implement 5 essential tests covering determinism, guards, merge, receipts, and E2E.

#### Test 1: Determinism (Days 6-7)

```
PSEUDOCODE SPECIFICATION:
  ALGORITHM: TestDeterminismHashStability
  INPUT: testFixture (frozen test environment)
  OUTPUT: result (success or failure)

  Steps:
    1. FreezeEnvironment(testFixture)
    2. For run 1 to 10:
       - RunProbeScan with frozen state
       - Compute SHA256 of output
       - Verify hash matches run 1
    3. Return PASS if all hashes match, else FAIL

IMPLEMENTATION:
  File: tests/determinism.test.js

  describe('Determinism Tests', () => {
    test('hash stability - 10 identical runs', () => {
      // Setup
      const frozenEnv = loadFixture('frozen-environment.json')
      const hashes = []

      // Execute
      for (let run = 1; run <= 10; run++) {
        setupFrozenEnvironment(frozenEnv)
        const output = runProbeScan(frozenEnv.projectPath)
        hashes.push(SHA256(JSON.stringify(output)))
        cleanupFrozenEnvironment(frozenEnv.projectPath)
      }

      // Verify
      const uniqueHashes = new Set(hashes)
      expect(uniqueHashes.size).toBe(1)
      expect(hashes[0]).toBe(hashes[9])
    })
  })

TESTING APPROACH:
  - Use frozen fixtures (no randomness)
  - Fix all timestamps (use frozen system time)
  - Run 10 times sequentially
  - Compute hash of observations + receipts
  - Compare all hashes for equality

EXPECTED RESULT: PASS (0% variance)

COVERAGE:
  - RNG seeding (fixed seed)
  - File I/O determinism
  - Hash computation
  - Observation ordering
  - Receipt generation

SUCCESS CRITERIA:
  - All 10 runs produce identical hashes
  - Test completes in <30s
  - Can reproduce failure analysis
```

#### Test 2: Guard Enforcement (Days 8-9)

```
PSEUDOCODE SPECIFICATION:
  ALGORITHM: TestGuardEnforcementDenyReceipts
  INPUT: guardRules, testScenarios (forbidden observations)
  OUTPUT: result (success or failure)

  Steps:
    1. For each guard rule type:
       - Create forbidden observation
       - Apply guards
       - Verify DENY status
       - Verify deny receipt structure
       - Verify receipt hash
    2. Return PASS if all guards enforced

IMPLEMENTATION:
  File: tests/guard-enforcement.test.js

  describe('Guard Enforcement Tests', () => {
    const testCases = loadFixture('guard-test-cases.json')

    testCases.forEach(testCase => {
      test(`${testCase.name}`, () => {
        const result = applyGuards(testCase.observation, GUARD_RULES)

        if (testCase.shouldDeny) {
          expect(result.status).toBe('DENY')
          expect(result.receipt).toBeDefined()
          expect(result.receipt.guardRuleId).toBe(testCase.guardRuleId)
          expect(result.receipt.denyReason).toBe(testCase.expectedDenyReason)
        } else {
          expect(result.status).toBe('ALLOW')
        }
      })
    })
  })

TESTING APPROACH:
  - Test positive cases (should deny)
  - Test negative cases (should allow)
  - Verify receipt structure
  - Verify deny reason populated
  - Test all guard rule types

EXPECTED RESULT: PASS (100% rules enforced)

COVERAGE:
  - FileSystem path guards
  - Environment variable guards
  - Network access guards
  - Custom rule guards
  - Receipt cryptography

SUCCESS CRITERIA:
  - All guard rules tested
  - All deny receipts valid
  - Test completes in <5s
  - 100% coverage of guard types
```

#### Test 3: Merge Correctness (Days 10-11)

```
PSEUDOCODE SPECIFICATION:
  ALGORITHM: TestMergeCorrectnessShardsIntegration
  INPUT: shards (array of pre-computed shard outputs)
  OUTPUT: result (success or failure)

  Steps:
    1. Load 10 precalculated shards
    2. Merge all shards
    3. Verify:
       - Observation count matches sum
       - Merkle root computes correctly
       - All receipts verify
       - Deterministic merge (same input = same output)

IMPLEMENTATION:
  File: tests/merge-correctness.test.js

  describe('Merge Correctness Tests', () => {
    const shards = loadAllShards()

    test('merge all 10 shards correctly', () => {
      const result = mergeShardsWithVerification(shards)

      // Verify structure
      expect(result.observations).toBeDefined()
      expect(result.receipts).toBeDefined()
      expect(result.merkleProof).toBeDefined()

      // Verify counts
      const expectedCount = Object.values(shards)
        .reduce((sum, s) => sum + s.observationCount, 0)
      expect(result.observations.length).toBe(expectedCount)

      // Verify Merkle tree
      const computed = computeMerkleRoot(result.observations)
      expect(computed).toBe(result.merkleProof.root)

      // Verify receipts
      for (const receipt of result.receipts) {
        const valid = verifyReceiptChain(receipt)
        expect(valid).toBe(true)
      }
    })

    test('merge determinism - identical inputs', () => {
      const result1 = mergeShardsWithVerification(shards)
      const result2 = mergeShardsWithVerification(shards)

      expect(SHA256(result1)).toBe(SHA256(result2))
    })

    test('merge partial - handle missing shards', () => {
      const incomplete = { ...shards }
      delete incomplete['wasm']

      const result = mergeShardsWithVerification(incomplete)

      expect(result.status).toBe('PARTIAL_MERGE')
      expect(result.shardsIncluded).toBe(9)
    })
  })

TESTING APPROACH:
  - Use precalculated fixtures
  - Test full merge (10 domains)
  - Test partial merge (9 domains)
  - Verify Merkle tree mathematics
  - Test merge stability

EXPECTED RESULT: PASS (merge correct)

COVERAGE:
  - Merkle tree computation
  - Receipt chaining
  - Observation aggregation
  - Error handling (partial)

SUCCESS CRITERIA:
  - Merged observation count correct
  - Merkle root validates
  - All receipts chain correctly
  - Merge is deterministic
  - Test completes in <10s
```

#### Test 4: Receipt Verification (Days 12-13)

```
PSEUDOCODE SPECIFICATION:
  ALGORITHM: TestReceiptVerificationCryptographic
  INPUT: receipts, merkleProof, observations
  OUTPUT: result (success or failure)

  Steps:
    1. Verify observation hashes
    2. Verify Merkle proof paths
    3. Verify receipt chaining
    4. Verify timestamp ordering
    5. Verify guard receipt structure

IMPLEMENTATION:
  File: tests/receipt-verification.test.js

  describe('Receipt Verification Tests', () => {
    const chainData = loadFixture('receipt-chain.json')

    test('observation hash validation', () => {
      for (const [i, obs] of chainData.observations.entries()) {
        const receipt = chainData.receipts[i]
        const expected = SHA256(obs)
        expect(receipt.observationHash).toBe(expected)
      }
    })

    test('merkle proof path validation', () => {
      for (const path of chainData.merkleProof.paths) {
        const reconstructed = reconstructMerkleRoot(path)
        expect(reconstructed).toBe(chainData.merkleProof.root)
      }
    })

    test('receipt chaining', () => {
      for (let i = 1; i < chainData.receipts.length; i++) {
        const current = chainData.receipts[i]
        const previous = chainData.receipts[i-1]
        expect(current.previousReceiptHash).toBe(
          SHA256(JSON.stringify(previous))
        )
      }
    })

    test('complete verification workflow', () => {
      const result = verifyAllReceipts(
        chainData.receipts,
        chainData.merkleProof
      )

      expect(result.allHashesValid).toBe(true)
      expect(result.allPathsValid).toBe(true)
      expect(result.rootMatches).toBe(true)
      expect(result.chainLinked).toBe(true)
    })
  })

TESTING APPROACH:
  - Use precalculated receipt chains
  - Verify SHA256 hashes match
  - Verify Merkle proof mathematics
  - Verify timestamp ordering
  - Test all verification paths

EXPECTED RESULT: PASS (cryptographic validity)

COVERAGE:
  - SHA256 computation
  - Merkle tree verification
  - Receipt chaining
  - Timestamp validation
  - Guard receipt structure

SUCCESS CRITERIA:
  - All hashes verify correctly
  - All Merkle paths valid
  - Receipt chains complete
  - Test completes in <5s
  - 100% verification success
```

#### Test 5: E2E Integration (Days 14-15)

```
PSEUDOCODE SPECIFICATION:
  ALGORITHM: TestE2EIntegrationFullWorkflow
  INPUT: projectPath (test project)
  OUTPUT: result (success or failure)

  Steps:
    1. Execute: kgc probe scan
    2. Verify artifacts exist
    3. Execute: kgc probe verify
    4. Validate results match baseline
    5. Check performance SLAs

IMPLEMENTATION:
  File: tests/e2e-integration.test.js

  describe('E2E Integration Tests', () => {
    const testProjectPath = './tests/fixtures/test-projects/real-project-1'
    const baseline = loadFixture('baselines/e2e-baseline.json')

    test('complete workflow scan + verify', () => {
      // Phase 1: Scan
      const scanStart = Date.now()
      const scanResult = execSync(
        `kgc probe scan --project ${testProjectPath}`
      )
      const scanDuration = Date.now() - scanStart

      expect(scanResult.exitCode).toBe(0)
      expect(scanDuration).toBeLessThan(baseline.performanceTargets.scanDuration.max)

      // Load artifacts
      const output = parseOutput(scanResult.stdout)
      const observations = loadJSON(output.observationsFile)
      const receipts = loadJSON(output.receiptsFile)

      // Phase 2: Verify
      const verifyStart = Date.now()
      const verifyResult = execSync(
        `kgc probe verify --observations ${output.observationsFile}`
      )
      const verifyDuration = Date.now() - verifyStart

      expect(verifyResult.exitCode).toBe(0)
      expect(verifyDuration).toBeLessThan(baseline.performanceTargets.verifyDuration.max)

      // Phase 3: Validate
      expect(observations.length).toBeGreaterThanOrEqual(
        baseline.expectedCounts.observations
      )

      // Verify all domains present
      const domains = new Set(observations.map(o => o.domain))
      for (const domain of baseline.expectedDomains) {
        expect(domains).toContain(domain)
      }

      // Verify no forbidden data
      for (const obs of observations) {
        for (const pattern of baseline.forbiddenPatterns) {
          expect(obs.data).not.toMatch(pattern)
        }
      }

      // Verify guard enforcement
      const denyCount = observations.filter(
        o => o.guardStatus === 'DENY'
      ).length
      expect(denyCount).toBe(baseline.expectedCounts.denyReceipts)
    })

    test('performance SLA compliance', () => {
      // Run benchmark
      const results = []
      for (let i = 0; i < 3; i++) {
        const duration = runFullWorkflow()
        results.push(duration)
      }

      const average = results.reduce((a, b) => a + b) / results.length
      expect(average).toBeLessThan(
        baseline.performanceTargets.totalWorkflow.target
      )
    })
  })

TESTING APPROACH:
  - Use realistic test project
  - Execute actual CLI commands
  - Verify artifact generation
  - Validate against baseline
  - Check performance SLAs
  - Run 3 iterations for stability

EXPECTED RESULT: PASS (full workflow valid)

COVERAGE:
  - Complete scan operation
  - Complete verify operation
  - File I/O and artifact generation
  - JSON parsing
  - Cryptographic verification
  - Guard enforcement
  - Performance characteristics

SUCCESS CRITERIA:
  - Scan completes successfully
  - Verify completes successfully
  - All artifacts generated
  - No forbidden data present
  - All domains covered
  - SLAs met
  - Test completes in <90s
```

---

## Phase 3: OTEL Validation (Days 16-20)

### Objective
Implement OTEL validation harness with 8 checks and scoring logic.

#### Task 3.1: OTEL Validation Checks
```
DELIVERABLE: Complete validation harness with all 8 checks

Steps:
1. Implement check functions
   - CHECK 1: AllObservationsHaveGuardStatus (10 pts)
   - CHECK 2: NoForbiddenPayloads (15 pts)
   - CHECK 3: AllReceiptsVerify (20 pts)
   - CHECK 4: DeterminismStable (15 pts)
   - CHECK 5: PerformanceSLA (10 pts)
   - CHECK 6: CompleteCoverage (15 pts)
   - CHECK 7: ErrorHandling (10 pts)
   - CHECK 8: GuardComprehensiveness (5 pts)

2. Implement scoring engine
   - Compute points per check
   - Total score calculation
   - Status determination (PASS/FAIL/PARTIAL)
   - Recommendations generation

3. Create validation harness
   - Aggregate all checks
   - Produce validation report
   - Format results for CI/CD

FILES TO CREATE:
  - src/validation/checks.mjs (8 check functions)
  - src/validation/scoring.mjs (scoring engine)
  - src/validation/harness.mjs (main harness)
  - src/validation/reporter.mjs (result formatting)

SUCCESS CRITERIA:
  - All 8 checks implemented
  - Scoring totals 100 points
  - Reports generated correctly
  - Can run harness in <30s
```

#### Task 3.2: OTEL Integration
```
DELIVERABLE: OTEL data collection and validation

Steps:
1. Instrument probe system
   - Add OTEL spans to all critical paths
   - Record attributes (domain, count, hash, status)
   - Record events (milestones)
   - Record errors with context

2. Implement span collection
   - Export spans to JSON
   - Parse spans for validation
   - Aggregate traces
   - Link causality

3. Integrate with validation
   - Feed spans to validation checks
   - Extract metrics from spans
   - Validate span consistency
   - Report on instrumentation coverage

FILES TO MODIFY:
  - src/probe/scan.mjs (add spans)
  - src/probe/merge.mjs (add spans)
  - src/probe/verify.mjs (add spans)

FILES TO CREATE:
  - src/validation/span-reader.mjs
  - src/validation/span-aggregator.mjs

SUCCESS CRITERIA:
  - All critical paths instrumented
  - Spans exportable to JSON
  - Validation reads spans
  - Coverage ≥95% of execution
```

#### Task 3.3: CI/CD Integration
```
DELIVERABLE: GitHub Actions workflow for OTEL validation

Steps:
1. Create GitHub Actions workflow
   - Run tests with OTEL collection
   - Execute validation harness
   - Check score ≥80
   - Report results
   - Block merge if <80

2. Configure scoring thresholds
   - Pass: ≥80/100
   - Warn: 60-79/100
   - Fail: <60/100

3. Create reporting
   - Detailed check breakdown
   - Recommendations for failures
   - Upload reports as artifacts
   - Comment on PR with results

FILES TO CREATE:
  - .github/workflows/validate.yml

SUCCESS CRITERIA:
  - Workflow runs on PR
  - Reports OTEL score
  - Blocks merge if <80
  - Artifacts uploaded
```

---

## Phase 4: Coverage & Documentation (Days 21-28)

### Objective
Complete coverage targets, documentation, and quality gates.

#### Task 4.1: Code Coverage
```
DELIVERABLE: 85%+ code coverage

Steps:
1. Measure coverage
   - Run: npm test -- --collect-coverage
   - Generate coverage report
   - Identify gaps

2. Fill coverage gaps
   - Add tests for uncovered lines
   - Test error paths
   - Test edge cases
   - Focus on critical paths

3. Setup coverage gates
   - Configure Jest coverage thresholds
   - Block commits if coverage <85%
   - Generate reports
   - Track coverage trends

FILES TO MODIFY:
  - jest.config.js (coverage thresholds)
  - package.json (coverage script)

SUCCESS CRITERIA:
  - Coverage ≥85% overall
  - All critical paths ≥90%
  - Coverage report generated
```

#### Task 4.2: Observation Coverage
```
DELIVERABLE: All 10 domains producing observations

Steps:
1. Verify domain coverage
   - Run full test suite
   - Count observations per domain
   - Verify all 10 domains present
   - Check minimums met

2. Add missing domains
   - If domain missing, investigate agent
   - Fix agent or test configuration
   - Verify observation production

3. Document coverage
   - Create coverage matrix
   - Track per-domain counts
   - Generate reports

SUCCESS CRITERIA:
  - All 10 domains covered
  - Minimum observations met per domain
  - Coverage matrix documented
  - Test validates coverage
```

#### Task 4.3: Guard Coverage
```
DELIVERABLE: 100% guard rule coverage

Steps:
1. Map guard rules
   - List all guard rule types
   - List all guard rule instances
   - Document rule conditions

2. Create test cases
   - One test per rule
   - Positive case (should deny)
   - Negative case (should allow)
   - Verify deny receipt

3. Document coverage
   - Coverage matrix by rule type
   - Coverage matrix by instance
   - Generate reports

SUCCESS CRITERIA:
  - All rule types tested
  - All rule instances covered
  - 100% coverage matrix
```

#### Task 4.4: Documentation
```
DELIVERABLE: Complete test documentation

Files:
  - docs/test-validation-strategy.md (Overview + 5 tests)
  - docs/otel-validation-harness.md (Validation reference)
  - docs/test-fixtures.md (Fixture specifications)
  - docs/test-implementation-roadmap.md (This file)
  - README.md (Update with test instructions)
  - docs/test-troubleshooting.md (Debugging guide)

Content:
  - How to run tests
  - How to run validation
  - How to debug failures
  - How to add new tests
  - How to maintain fixtures
  - Expected results and SLAs

SUCCESS CRITERIA:
  - New developer can add test in 1 hour
  - Complete user can debug failure in <30min
  - All processes documented
```

---

## Implementation Checklist

### Phase 1: Foundation
- [ ] Jest configuration
- [ ] Test helpers and utilities
- [ ] Fixture directory structure
- [ ] Fixture loaders
- [ ] Fixture validators
- [ ] OTEL configuration
- [ ] OTEL instrumentation
- [ ] OTEL span readers
- [ ] `npm test` command works
- [ ] Coverage reporting works

### Phase 2: Essential Tests
- [ ] Test 1 - Determinism (pseudocode → implementation)
- [ ] Test 2 - Guard Enforcement (pseudocode → implementation)
- [ ] Test 3 - Merge Correctness (pseudocode → implementation)
- [ ] Test 4 - Receipt Verification (pseudocode → implementation)
- [ ] Test 5 - E2E Integration (pseudocode → implementation)
- [ ] All fixtures loaded correctly
- [ ] All tests passing
- [ ] Coverage ≥80% for test modules

### Phase 3: OTEL Validation
- [ ] 8 check functions implemented
- [ ] Scoring engine working
- [ ] Validation harness complete
- [ ] Result reporting configured
- [ ] OTEL spans in all critical paths
- [ ] Span collection working
- [ ] Validation reads spans
- [ ] GitHub Actions workflow created
- [ ] CI integration tested

### Phase 4: Polish
- [ ] Code coverage ≥85%
- [ ] Observation coverage complete (all 10 domains)
- [ ] Guard coverage 100%
- [ ] All documentation complete
- [ ] Test troubleshooting guide
- [ ] Fixture maintenance documented
- [ ] New developer onboarding tested
- [ ] All quality gates passing

---

## Success Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| OTEL Score | ≥80/100 | `npm run validate:otel` |
| Test Pass Rate | 100% | `npm test` output |
| Code Coverage | ≥85% | Jest coverage report |
| Observation Coverage | 100% | Domain count check |
| Guard Coverage | 100% | Guard rule matrix |
| Determinism Variance | 0% | 10 runs hash compare |
| Performance (scan) | <30s | OTEL duration |
| Performance (verify) | <10s | OTEL duration |
| Receipt Verification | 100% | Crypto validation |
| Forbidden Escapes | 0 | Pattern matching |

---

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|-----------|
| Non-determinism detected | CRITICAL | Use frozen fixtures, fixed seeds, sequential execution |
| Guard bypass vulnerability | CRITICAL | Exhaustive test cases, pattern matching validation |
| Merkle proof invalid | HIGH | Pre-computed fixtures, mathematical verification |
| Performance SLA miss | MEDIUM | Profile bottlenecks, optimize critical paths |
| OTEL instrumentation cost | MEDIUM | Lazy initialization, sampling strategy |
| Fixture maintenance burden | MEDIUM | Automate regeneration, document process |
| Test flakiness | MEDIUM | Use deterministic fixtures, avoid timing |

---

## Timeline Summary

```
WEEK 1 (Days 1-5):     Foundation setup
WEEK 2 (Days 6-15):    5 Essential tests
WEEK 3 (Days 16-20):   OTEL validation
WEEK 4 (Days 21-28):   Coverage & Documentation

Entry Criteria:
  - UNRDF codebase ready
  - Probe system functional
  - Merge logic implemented
  - Guard rules defined

Exit Criteria:
  - OTEL score ≥80/100
  - Test pass rate 100%
  - Code coverage ≥85%
  - All documentation complete
  - CI/CD gates configured
```

---

## Execution Strategy

### Single-Pass Implementation (Recommended)

1. **Start with Foundation** (Phase 1) - Establish all infrastructure
2. **Implement Tests Sequentially** (Phase 2) - One test per 2 days
3. **Add Validation** (Phase 3) - Integrate OTEL during Phase 2
4. **Polish** (Phase 4) - Fill coverage gaps, complete docs

### Parallel Tracks (If Multiple Engineers)

**Track A (Test Engineer - Agent 9):**
- Phase 1: Fixtures and OTEL
- Phase 2: Tests 1-3
- Phase 3: Validation checks
- Phase 4: Documentation

**Track B (Backend Dev - Agent 1):**
- Phase 1: Test infrastructure
- Phase 2: Tests 4-5 (E2E)
- Phase 3: OTEL integration
- Phase 4: Performance optimization

**Track C (Code Reviewer - Agent 3):**
- Phase 1: Review infrastructure
- Phase 2: Review tests
- Phase 3: Review validation
- Phase 4: Quality gate validation

---

## Related Documents

- **Test Strategy Details**: `/home/user/unrdf/docs/test-validation-strategy.md`
- **OTEL Harness Reference**: `/home/user/unrdf/docs/otel-validation-harness.md`
- **Fixture Specifications**: `/home/user/unrdf/docs/test-fixtures.md`
- **CLAUDE.md**: Project quality standards and requirements
- **BB80/20 Methodology**: `docs/bb80-20-methodology.md`

---

## Getting Started

**Immediate Next Steps:**

```bash
# 1. Clone repository
git clone <repo> /home/user/unrdf
cd /home/user/unrdf

# 2. Review documentation
cat docs/test-validation-strategy.md
cat docs/otel-validation-harness.md
cat docs/test-fixtures.md

# 3. Create Phase 1 tasks in project management
# (Update with actual sprint planning tool)

# 4. Start Phase 1 - Foundation
npm install jest @opentelemetry/sdk-node
mkdir -p tests/fixtures tests/helpers
# ... continue with Task 1.1, 1.2, 1.3
```

---

## Questions & Support

**For clarification on:**
- Test requirements: See `test-validation-strategy.md` (Part 2)
- Validation logic: See `otel-validation-harness.md` (Implementation Reference)
- Test data: See `test-fixtures.md` (Fixture Definitions)
- Implementation order: See above (Phase breakdown)

**For empirical validation:**
- OTEL score: `npm run validate:otel`
- Test status: `npm test`
- Coverage: `npm test -- --collect-coverage`
- All three must pass for PR merge
