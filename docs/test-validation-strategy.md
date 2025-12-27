# KGC Probe Test & Validation Strategy (SPARC Phase)

## Executive Summary

This document defines a complete test and validation architecture for the KGC probe package system, ensuring:
- **Determinism**: Identical inputs produce identical outputs across runs
- **Guard Enforcement**: Forbidden observations are properly denied with valid receipts
- **Merge Correctness**: Shard outputs correctly merge with cryptographic verification
- **E2E Integrity**: Full scan → merge → verify workflow produces valid artifacts
- **OTEL Validation**: Comprehensive observability with scoring ≥80/100 threshold

---

## Part 1: Test Strategy Architecture

### Test Pyramid (by value and maintenance cost)

```
Essential Tests (5)      ← HIGH VALUE, LOW MAINTENANCE
├── Determinism
├── Guard Enforcement
├── Merge Correctness
├── Receipt Verification
└── E2E Integration

Supporting Tests (15+)
├── Unit Tests (agent shards, individual modules)
├── Integration Tests (component interactions)
├── Property Tests (invariants across inputs)
└── Performance Tests (SLA verification)
```

### Test Categories Definition

```
CATEGORY: Unit Tests
PURPOSE: Validate individual agent shards and modules in isolation
SCOPE:
    - Runtime agent (code execution, timeout handling)
    - FileSystem agent (path traversal, permission simulation)
    - Wasm agent (WASM binary loading, execution)
    - Env agent (environment variable extraction)
    - Network agent (connection simulation)
    - Hash/Crypto utilities (SHA256, Merkle tree operations)

METHODS:
    - Arrange: Mock inputs with known outputs
    - Act: Execute single function/agent
    - Assert: Verify output matches expectations

TOOLS: Jest/Vitest with fixtures
COVERAGE TARGET: 85%+ per module


CATEGORY: Integration Tests
PURPOSE: Validate interactions between components
SCOPE:
    - Merge logic (combining multiple shard outputs)
    - Receipt validation chain (guard → observation → receipt)
    - Hash verification (Merkle root computation)
    - Guard rule application (permission checking)

METHODS:
    - Arrange: Create multi-agent scenarios
    - Act: Execute workflows (scan → merge)
    - Assert: Verify end state consistency

TOOLS: Jest with snapshots, computed hashes
COVERAGE TARGET: 80%+ path coverage


CATEGORY: E2E Tests
PURPOSE: Full workflow from probe scan to verification
SCOPE:
    - Complete `kgc probe scan` execution
    - Shard merger with 10 domains
    - `kgc probe verify` validation
    - Artifact file generation

METHODS:
    - Arrange: Real project structure or mock repo
    - Act: Execute full CLI commands
    - Assert: Verify all output files, receipts, hashes

TOOLS: Bash integration, file assertions, hash validation
COVERAGE TARGET: All major code paths


CATEGORY: Determinism Tests
PURPOSE: Verify identical inputs produce identical outputs
SCOPE:
    - Identical agent configurations
    - Same input data across multiple runs
    - Hash stability verification

METHODS:
    - Run 1: Execute scan with frozen state
    - Run 2: Execute identical scan
    - Assert: H(output_1) === H(output_2)

TOOLS: Determinism harness, hash comparison
COVERAGE TARGET: 10+ identical runs, 0 variance


CATEGORY: Guard Tests
PURPOSE: Verify forbidden observations are properly denied
SCOPE:
    - FileSystem path guards (denied paths)
    - Environment variable guards (sensitive vars)
    - Network access guards (blocked domains)
    - Custom rule enforcement

METHODS:
    - Arrange: Scenarios with forbidden observations
    - Act: Attempt to include forbidden data
    - Assert: Verify deny receipt with correct hash

TOOLS: Guard rule engine, receipt validator
COVERAGE TARGET: 100% of guard rule types
```

---

## Part 2: Five Essential Tests (Pseudocode)

### Test 1: Determinism - Hash Stability

```
ALGORITHM: TestDeterminismHashStability
INPUT: testFixture (frozen test environment)
OUTPUT: result (success or failure)

PURPOSE:
    Verify that identical inputs produce identical output hashes
    across multiple runs. This proves the probe system is deterministic.

SETUP:
    frozenEnv ← FreezeEnvironment(testFixture)
    scanConfig ← CreateScanConfig(frozenEnv)

BEGIN
    // Run 1: First scan
    output_1 ← RunProbeScan(scanConfig, frozenEnv)
    hashes_1 ← {
        observations: SHA256(output_1.observations),
        receipts: SHA256(output_1.receipts),
        merkleRoot: output_1.merklePloof.root
    }

    // Cleanup: Reset to frozen state
    RestoreEnvironment(frozenEnv)

    // Run 2: Identical second scan
    output_2 ← RunProbeScan(scanConfig, frozenEnv)
    hashes_2 ← {
        observations: SHA256(output_2.observations),
        receipts: SHA256(output_2.receipts),
        merkleRoot: output_2.merkleProof.root
    }

    // Verification
    deviations ← []
    FOR EACH hash_type IN [observations, receipts, merkleRoot] DO
        IF hashes_1[hash_type] NOT EQUAL hashes_2[hash_type] THEN
            deviations.append({
                type: hash_type,
                expected: hashes_1[hash_type],
                actual: hashes_2[hash_type]
            })
        END IF
    END FOR

    IF LENGTH(deviations) > 0 THEN
        RETURN {status: FAIL, deviations: deviations}
    END IF

    // Repeat for statistical confidence (n=10)
    FOR run FROM 3 TO 10 DO
        RestoreEnvironment(frozenEnv)
        output_n ← RunProbeScan(scanConfig, frozenEnv)
        hashes_n ← ComputeHashes(output_n)

        IF NOT HashesMatch(hashes_n, hashes_1) THEN
            RETURN {status: FAIL, failedRun: run}
        END IF
    END FOR

    RETURN {status: PASS, runs: 10, variance: 0}
END

ASSERTIONS:
    - hashes_1 === hashes_2 (exact match)
    - All 10 runs produce identical hashes
    - Merkle root matches across runs
    - No variance in determinism (0% deviation)

COVERAGE:
    - All domain agents (10 domains)
    - Hash computation (SHA256)
    - Merkle proof generation
    - Run isolation
```

### Test 2: Guard Enforcement - Deny Receipts

```
ALGORITHM: TestGuardEnforcementDenyReceipts
INPUT: guardRules (configuration), testScenarios (forbidden observations)
OUTPUT: result (success or failure)

PURPOSE:
    Verify that forbidden observations are properly rejected with
    valid deny receipts. Guard rules must enforce access control.

CONSTANTS:
    GUARD_RULES ← [
        {domain: "filesystem", rule: "deny /etc/passwd"},
        {domain: "environment", rule: "deny AWS_SECRET_ACCESS_KEY"},
        {domain: "network", rule: "deny *.internal"},
        {domain: "custom", rule: "deny /src/secrets.json"}
    ]

SETUP:
    testCases ← []

    FOR EACH rule IN GUARD_RULES DO
        case ← CreateTestCase(rule)
        testCases.append(case)
    END FOR

BEGIN
    results ← []

    FOR EACH testCase IN testCases DO
        // Arrange: Create observation that would violate guard
        forbiddenObservation ← CreateObservation(testCase.content)

        // Act: Apply guard rules
        guardResult ← ApplyGuards(forbiddenObservation, GUARD_RULES)

        // Assert: Verify deny receipt
        ASSERT guardResult.status === "DENY"
        ASSERT guardResult.receipt IS NOT NULL

        // Verify receipt structure
        denyReceipt ← guardResult.receipt
        ASSERT denyReceipt.observationHash === SHA256(forbiddenObservation)
        ASSERT denyReceipt.guardRuleId === testCase.rule.id
        ASSERT denyReceipt.timestamp IS VALID DATETIME

        // Verify receipt signature (if cryptographic signing used)
        IF SupportsReceiptSignature() THEN
            isValid ← VerifyReceiptSignature(denyReceipt)
            ASSERT isValid === true
        END IF

        results.append({
            rule: testCase.rule.id,
            status: PASS,
            receipt: denyReceipt
        })
    END FOR

    // Verify coverage: all guard rules tested
    IF LENGTH(results) NOT EQUAL LENGTH(GUARD_RULES) THEN
        RETURN {status: FAIL, reason: "Incomplete guard coverage"}
    END IF

    RETURN {status: PASS, guardsCovered: LENGTH(results), receipts: results}
END

ASSERTIONS:
    - Forbidden observations trigger DENY status
    - Each DENY has valid receipt
    - Receipt contains correct observation hash
    - Receipt references applicable guard rule
    - Receipt timestamp is valid
    - 100% coverage of guard rule types

COVERAGE:
    - FileSystem access guards
    - Environment variable guards
    - Network access guards
    - Custom rule guards
    - Receipt cryptography (if enabled)
```

### Test 3: Merge Correctness - Shard Integration

```
ALGORITHM: TestMergeCorrectnessShardsIntegration
INPUT: shards (array of pre-computed shard outputs)
OUTPUT: result (success or failure)

PURPOSE:
    Verify that multiple shard outputs correctly merge with
    cryptographic validation. Tests the core merge algorithm.

SETUP:
    // Pre-computed fixtures from known agent outputs
    fixtures ← LoadFixtures("merge-test-cases")

    shardSet_1 ← {
        runtime: fixtures.runtime_shard_1,
        filesystem: fixtures.fs_shard_1,
        wasm: fixtures.wasm_shard_1,
        environment: fixtures.env_shard_1,
        network: fixtures.network_shard_1,
        custom_1: fixtures.custom1_shard_1,
        custom_2: fixtures.custom2_shard_1,
        custom_3: fixtures.custom3_shard_1,
        logging: fixtures.logging_shard_1,
        metrics: fixtures.metrics_shard_1
    }

BEGIN
    // Test Case 1: Basic merge (10 shards)
    mergedResult_1 ← MergeShardsWithVerification(shardSet_1)

    // Verify structure
    ASSERT mergedResult_1.observations !== NULL
    ASSERT mergedResult_1.receipts !== NULL
    ASSERT mergedResult_1.merkleProof !== NULL

    // Verify observation count (should sum from all shards)
    expectedCount ← SUM(shardSet_1[s].observationCount FOR s IN shardSet_1)
    ASSERT LENGTH(mergedResult_1.observations) === expectedCount

    // Verify Merkle tree validity
    computedRoot ← ComputeMerkleRoot(mergedResult_1.observations)
    ASSERT computedRoot === mergedResult_1.merkleProof.root

    // Verify all receipt chains
    FOR EACH receipt IN mergedResult_1.receipts DO
        isValid ← VerifyReceiptChain(receipt)
        ASSERT isValid === true
    END FOR


    // Test Case 2: Merge with missing shards (handle gracefully)
    shardSet_2 ← COPY(shardSet_1)
    DELETE shardSet_2["wasm"]  // Remove one shard

    mergedResult_2 ← MergeShardsWithVerification(shardSet_2)

    ASSERT mergedResult_2.status === "PARTIAL_MERGE"
    ASSERT mergedResult_2.shardsIncluded === 9
    ASSERT mergedResult_2.shardsMissing === 1


    // Test Case 3: Merge stability (same shards, identical result)
    mergedResult_1b ← MergeShardsWithVerification(shardSet_1)

    ASSERT SHA256(mergedResult_1) === SHA256(mergedResult_1b)
    ASSERT mergedResult_1.merkleProof.root === mergedResult_1b.merkleProof.root


    // Test Case 4: Merkle proof validation
    merkleProof ← mergedResult_1.merkleProof

    FOR EACH observationIndex IN [0, 5, 9, expectedCount-1] DO
        observation ← mergedResult_1.observations[observationIndex]
        path ← merkleProof.paths[observationIndex]

        // Verify path leads to root
        verifyResult ← VerifyMerkleProofPath(observation, path, merkleProof.root)
        ASSERT verifyResult === true
    END FOR


    RETURN {
        status: PASS,
        testCases: 4,
        totalShards: LENGTH(shardSet_1),
        totalObservations: expectedCount,
        merkleVerified: true
    }
END

ASSERTIONS:
    - Merged output contains all observations from all shards
    - Merkle root computed correctly from observations
    - All receipt chains verify successfully
    - Identical merges produce identical hashes
    - Merkle paths validate for all observations
    - Missing shards handled gracefully

COVERAGE:
    - All 10 domain shards
    - Merkle tree construction
    - Receipt chain validation
    - Merge stability
    - Error handling (missing shards)
```

### Test 4: Receipt Verification - Cryptographic Validation

```
ALGORITHM: TestReceiptVerificationCryptographic
INPUT: receipts (array of generated receipts), merkleProof (root + paths)
OUTPUT: result (success or failure)

PURPOSE:
    Verify receipt integrity through cryptographic validation:
    - SHA256 hashes are correct
    - Merkle paths verify
    - Receipt chains link properly
    - Timestamps are valid

SETUP:
    testData ← LoadFixtures("receipt-test-cases")
    observations ← testData.observations
    merkleProof ← testData.merkleProof

BEGIN
    // Test Case 1: Individual receipt hash validation
    FOR EACH i IN [0 TO LENGTH(observations)-1] DO
        observation ← observations[i]
        receipt ← CreateReceiptForObservation(observation)

        // Hash should match observation exactly
        computedHash ← SHA256(observation)
        ASSERT receipt.observationHash === computedHash

        // Observation should be recoverable from hash (immutability check)
        IF SHA256(observation) !== receipt.observationHash THEN
            RETURN {status: FAIL, reason: "Hash mismatch at index " + i}
        END IF
    END FOR


    // Test Case 2: Merkle proof path validation
    FOR EACH i IN [0 TO LENGTH(observations)-1] DO
        observation ← observations[i]
        path ← merkleProof.paths[i]

        // Reconstruct root from observation + path
        reconstructedRoot ← ReconstructMerkleRoot(
            SHA256(observation),
            path
        )

        // Must match actual root
        ASSERT reconstructedRoot === merkleProof.root
    END FOR


    // Test Case 3: Receipt chaining (parent → child links)
    FOR EACH i IN [1 TO LENGTH(observations)-1] DO  // Skip first
        currentReceipt ← CreateReceiptForObservation(observations[i])
        previousReceipt ← CreateReceiptForObservation(observations[i-1])

        // Current receipt should reference previous
        ASSERT currentReceipt.previousReceiptHash === SHA256(previousReceipt)
    END FOR


    // Test Case 4: Timestamp ordering and validity
    FOR EACH receipt IN receipts DO
        ASSERT receipt.timestamp IS VALID DATETIME FORMAT
        ASSERT receipt.timestamp <= CurrentTime()
    END FOR

    // Verify chronological ordering
    FOR EACH i IN [1 TO LENGTH(receipts)-1] DO
        ASSERT receipts[i].timestamp >= receipts[i-1].timestamp
    END FOR


    // Test Case 5: Guard receipt linkage
    guardReceipts ← FILTER(r FOR r IN receipts WHERE r.type === "GUARD_DENY")

    FOR EACH guardReceipt IN guardReceipts DO
        // Guard receipt must reference observation
        ASSERT guardReceipt.observationHash IS NOT NULL

        // Must reference applicable rule
        ASSERT guardReceipt.guardRuleId IS NOT NULL

        // Must have deny justification
        ASSERT guardReceipt.denyReason IS NOT EMPTY STRING
    END FOR


    // Test Case 6: Complete verification workflow
    verificationResult ← VerifyAllReceipts(receipts, merkleProof)

    ASSERT verificationResult.allHashesValid === true
    ASSERT verificationResult.allPathsValid === true
    ASSERT verificationResult.rootMatches === true
    ASSERT verificationResult.chainLinked === true
    ASSERT LENGTH(verificationResult.errors) === 0


    RETURN {
        status: PASS,
        receiptsVerified: LENGTH(receipts),
        hashesChecked: LENGTH(observations),
        pathsValidated: LENGTH(merkleProof.paths),
        guardReceiptsVerified: LENGTH(guardReceipts)
    }
END

ASSERTIONS:
    - All observation hashes match SHA256(observation)
    - All Merkle paths reconstruct to root
    - Receipt chains link chronologically
    - Timestamps are valid and ordered
    - Guard receipts have valid justifications
    - Complete verification passes without errors

COVERAGE:
    - SHA256 hash computation
    - Merkle tree verification
    - Receipt chaining
    - Timestamp validation
    - Guard receipt structure
    - Cryptographic proof validation
```

### Test 5: E2E Integration - Full Workflow

```
ALGORITHM: TestE2EIntegrationFullWorkflow
INPUT: projectPath (test project), expectedOutcomes (baseline)
OUTPUT: result (success or failure)

PURPOSE:
    Execute complete workflow from probe scan through verification.
    Validates all components working together correctly.

SETUP:
    testProject ← CreateTestProject()
    // Includes:
    // - Real JavaScript files (various patterns)
    // - Real config files
    // - Sensitive files (to test guards)
    // - Environment setup
    // - Network simulation

BEGIN
    // Phase 1: Execute scan
    LogInfo("Phase 1: Executing probe scan...")

    scanCmd ← "kgc probe scan --project " + projectPath
    scanResult ← ExecuteCommand(scanCmd, timeout=30s)

    ASSERT scanResult.exitCode === 0
    ASSERT scanResult.stderr IS EMPTY

    scanOutput ← ParseScanOutput(scanResult.stdout)
    ASSERT scanOutput.status === "SUCCESS"
    ASSERT scanOutput.observationCount > 0
    ASSERT scanOutput.shardCount === 10


    // Phase 2: Verify output artifacts exist
    LogInfo("Phase 2: Verifying output artifacts...")

    ASSERT FileExists(scanOutput.observationsFile)
    ASSERT FileExists(scanOutput.receiptsFile)
    ASSERT FileExists(scanOutput.merkleProofFile)

    // Load artifacts
    observations ← LoadJSON(scanOutput.observationsFile)
    receipts ← LoadJSON(scanOutput.receiptsFile)
    merkleProof ← LoadJSON(scanOutput.merkleProofFile)

    ASSERT LENGTH(observations) === scanOutput.observationCount
    ASSERT LENGTH(receipts) >= LENGTH(observations)


    // Phase 3: Execute verification
    LogInfo("Phase 3: Executing probe verify...")

    verifyCmd ← "kgc probe verify --observations " + scanOutput.observationsFile +
                " --receipts " + scanOutput.receiptsFile +
                " --proof " + scanOutput.merkleProofFile

    verifyResult ← ExecuteCommand(verifyCmd, timeout=10s)

    ASSERT verifyResult.exitCode === 0
    verifyOutput ← ParseVerifyOutput(verifyResult.stdout)


    // Phase 4: Validate results match expectations
    LogInfo("Phase 4: Validating against baseline expectations...")

    ASSERT verifyOutput.status === "VERIFIED"
    ASSERT verifyOutput.allHashesValid === true
    ASSERT verifyOutput.allPathsValid === true
    ASSERT verifyOutput.rootMatches === true


    // Phase 5: Verify guard enforcement (no forbidden data)
    LogInfo("Phase 5: Checking guard enforcement...")

    forbiddenPatterns ← [
        "/etc/passwd",
        "AWS_SECRET",
        "DATABASE_URL",
        "/src/secrets.json"
    ]

    FOR EACH observation IN observations DO
        FOR EACH pattern IN forbiddenPatterns DO
            ASSERT NOT Contains(observation.data, pattern)
        END FOR
    END FOR


    // Phase 6: Verify all domains represented
    LogInfo("Phase 6: Checking domain coverage...")

    domainsFound ← SET()
    FOR EACH observation IN observations DO
        domainsFound.add(observation.domain)
    END FOR

    requiredDomains ← ["runtime", "filesystem", "wasm", "environment",
                       "network", "custom_1", "custom_2", "custom_3",
                       "logging", "metrics"]

    FOR EACH domain IN requiredDomains DO
        ASSERT domainsFound.contains(domain)
    END FOR


    // Phase 7: Performance check
    LogInfo("Phase 7: Checking performance SLA...")

    ASSERT scanResult.duration < 30s
    ASSERT verifyResult.duration < 10s

    totalTime ← scanResult.duration + verifyResult.duration
    LogInfo("Total workflow time: " + totalTime + "s")


    // Phase 8: Generate report
    report ← {
        status: "PASS",
        phases: 8,
        observationCount: LENGTH(observations),
        receiptCount: LENGTH(receipts),
        domainsCovered: LENGTH(domainsFound),
        guardEnforced: true,
        hashesVerified: true,
        scanTime: scanResult.duration,
        verifyTime: verifyResult.duration,
        totalTime: totalTime
    }

    RETURN report
END

ASSERTIONS:
    - Scan completes successfully (exitCode 0)
    - All output artifacts exist and are valid JSON
    - Verification completes successfully
    - All hashes and paths verify cryptographically
    - No forbidden observations present
    - All 10 domains produce observations
    - Workflow completes within SLA (30s + 10s)
    - Guard rules are enforced throughout

COVERAGE:
    - Complete CLI execution
    - File I/O and artifact generation
    - JSON parsing and validation
    - Cryptographic verification
    - Guard enforcement
    - Performance characteristics
    - End-to-end data flow
```

---

## Part 3: OTEL Validation Harness (Pseudocode)

### OTEL Validation Architecture

```
ALGORITHM: OTELValidationHarness
INPUT: testResults (array of test results), spans (array of OTEL spans)
OUTPUT: validationScore (0-100)

PURPOSE:
    Comprehensive validation of probe system correctness through
    OTEL observability data. Measures empirical correctness.

DATA STRUCTURE: OTEL Spans

    Span {
        traceId: string           // Unique request ID
        spanId: string            // Unique span ID
        parentSpanId: string      // Parent relationship
        name: string              // "scan", "merge", "verify"
        startTime: datetime
        endTime: datetime
        duration: milliseconds
        status: "OK" | "ERROR"
        attributes: object        // Domain-specific data
        events: array             // Named events within span
        links: array              // Causality references
    }

    Key Attributes:
        - scan.domain: Domain name (runtime, filesystem, etc.)
        - observation.count: Number of observations
        - observation.guardStatus: "ALLOW" | "DENY"
        - receipt.hash: SHA256 hash
        - guard.ruleId: Applied rule identifier
        - error.message: If status === "ERROR"

DATA STRUCTURE: Validation Checks

    Each check produces a score (0-100 points):
    - AllObservationsHaveGuardStatus (10 points)
    - NoForbiddenPayloads (15 points)
    - AllReceiptsVerify (20 points)
    - DeterminismStable (15 points)
    - PerformanceSLA (10 points)
    - CompleteCoverage (15 points)
    - ErrorHandling (10 points)
    - GuardComprehensiveness (5 points)
    - Total: 100 points
```

### OTEL Validation Checks (Pseudocode)

```
ALGORITHM: OTELValidationChecks
INPUT: spans (OTEL spans), observations (probe observations)
OUTPUT: checkResults (array of check results with scores)

BEGIN
    results ← []
    totalScore ← 0


    // CHECK 1: All observations have guard status (10 points)
    check1 ← {
        name: "AllObservationsHaveGuardStatus",
        maxPoints: 10,
        details: []
    }

    observationsWithoutStatus ← []
    FOR EACH observation IN observations DO
        IF observation.guardStatus NOT IN ["ALLOW", "DENY"] THEN
            observationsWithoutStatus.append(observation)
        END IF
    END FOR

    IF LENGTH(observationsWithoutStatus) === 0 THEN
        check1.score ← 10
        check1.result ← "PASS"
    ELSE
        ratio ← (LENGTH(observations) - LENGTH(observationsWithoutStatus)) / LENGTH(observations)
        check1.score ← ROUND(10 * ratio)
        check1.result ← "PARTIAL"
        check1.details ← {
            missing: LENGTH(observationsWithoutStatus),
            coverage: ratio * 100
        }
    END IF

    results.append(check1)
    totalScore ← totalScore + check1.score


    // CHECK 2: No forbidden payloads in observations (15 points)
    check2 ← {
        name: "NoForbiddenPayloads",
        maxPoints: 15,
        details: []
    }

    forbiddenPatterns ← [
        {pattern: "AWS_SECRET", domain: "environment"},
        {pattern: "DATABASE_PASSWORD", domain: "environment"},
        {pattern: "/etc/passwd", domain: "filesystem"},
        {pattern: "/root/.ssh", domain: "filesystem"},
        {pattern: "PRIVATE KEY", domain: "filesystem"},
        {pattern: "mongodb+srv://", domain: "network"}
    ]

    violationFound ← false
    violationDetails ← []

    FOR EACH observation IN observations DO
        IF observation.guardStatus === "DENY" THEN
            CONTINUE  // Properly denied, OK
        END IF

        FOR EACH forbidden IN forbiddenPatterns DO
            IF observation.domain === forbidden.domain AND
               Contains(observation.data, forbidden.pattern) THEN
                violationFound ← true
                violationDetails.append({
                    pattern: forbidden.pattern,
                    observation: observation.id
                })
            END IF
        END FOR
    END FOR

    IF NOT violationFound THEN
        check2.score ← 15
        check2.result ← "PASS"
    ELSE
        check2.score ← 0
        check2.result ← "FAIL"
        check2.details ← violationDetails
    END IF

    results.append(check2)
    totalScore ← totalScore + check2.score


    // CHECK 3: All receipts verify cryptographically (20 points)
    check3 ← {
        name: "AllReceiptsVerify",
        maxPoints: 20,
        details: []
    }

    receiptSpans ← FILTER(s FOR s IN spans WHERE s.name === "receipt_verify")
    successfulVerifications ← 0
    failedVerifications ← []

    FOR EACH receiptSpan IN receiptSpans DO
        IF receiptSpan.status === "OK" THEN
            successfulVerifications ← successfulVerifications + 1
        ELSE
            failedVerifications.append({
                spanId: receiptSpan.spanId,
                error: receiptSpan.attributes.error
            })
        END IF
    END FOR

    verificationRate ← successfulVerifications / LENGTH(receiptSpans)
    check3.score ← ROUND(20 * verificationRate)

    IF verificationRate === 1.0 THEN
        check3.result ← "PASS"
    ELSE IF verificationRate >= 0.95 THEN
        check3.result ← "PARTIAL"
    ELSE
        check3.result ← "FAIL"
    END IF

    check3.details ← {
        verified: successfulVerifications,
        failed: LENGTH(failedVerifications),
        rate: verificationRate * 100
    }

    results.append(check3)
    totalScore ← totalScore + check3.score


    // CHECK 4: Determinism stable (identical inputs = identical outputs) (15 points)
    check4 ← {
        name: "DeterminismStable",
        maxPoints: 15,
        details: []
    }

    deterministicRuns ← FILTER(s FOR s IN spans WHERE s.attributes.runNumber EXISTS)

    IF LENGTH(deterministicRuns) < 2 THEN
        check4.score ← 0
        check4.result ← "SKIP"
        check4.details ← {reason: "Fewer than 2 determinism runs recorded"}
    ELSE
        hashes ← []
        FOR EACH run IN deterministicRuns DO
            hash ← run.attributes.outputHash
            hashes.append(hash)
        END FOR

        // All hashes should be identical
        firstHash ← hashes[0]
        allMatch ← true
        mismatches ← 0

        FOR EACH hash IN hashes[1:] DO
            IF hash NOT EQUAL firstHash THEN
                allMatch ← false
                mismatches ← mismatches + 1
            END IF
        END FOR

        IF allMatch THEN
            check4.score ← 15
            check4.result ← "PASS"
        ELSE
            stability ← (LENGTH(hashes) - mismatches) / LENGTH(hashes)
            check4.score ← ROUND(15 * stability)
            check4.result ← "PARTIAL"
        END IF

        check4.details ← {
            runs: LENGTH(hashes),
            mismatches: mismatches,
            stability: (LENGTH(hashes) - mismatches) / LENGTH(hashes) * 100
        }
    END IF

    results.append(check4)
    totalScore ← totalScore + check4.score


    // CHECK 5: Performance within SLA (10 points)
    check5 ← {
        name: "PerformanceSLA",
        maxPoints: 10,
        details: []
    }

    slaTargets ← {
        scan: 30000,        // 30 seconds in milliseconds
        merge: 5000,        // 5 seconds
        verify: 10000       // 10 seconds
    }

    slaViolations ← []

    FOR EACH spanName IN ["scan", "merge", "verify"] DO
        span ← FindSpan(spans, spanName)
        IF span !== NULL THEN
            IF span.duration > slaTargets[spanName] THEN
                slaViolations.append({
                    operation: spanName,
                    target: slaTargets[spanName],
                    actual: span.duration
                })
            END IF
        END IF
    END FOR

    IF LENGTH(slaViolations) === 0 THEN
        check5.score ← 10
        check5.result ← "PASS"
    ELSE
        check5.score ← MAX(0, 10 - (LENGTH(slaViolations) * 3))
        check5.result ← "PARTIAL"
    END IF

    check5.details ← {
        violations: slaViolations,
        targets: slaTargets
    }

    results.append(check5)
    totalScore ← totalScore + check5.score


    // CHECK 6: Complete coverage (all domains represented) (15 points)
    check6 ← {
        name: "CompleteCoverage",
        maxPoints: 15,
        details: []
    }

    requiredDomains ← ["runtime", "filesystem", "wasm", "environment",
                       "network", "custom_1", "custom_2", "custom_3",
                       "logging", "metrics"]

    domainsFound ← SET()
    FOR EACH observation IN observations DO
        domainsFound.add(observation.domain)
    END FOR

    missingDomains ← []
    FOR EACH domain IN requiredDomains DO
        IF NOT domainsFound.contains(domain) THEN
            missingDomains.append(domain)
        END IF
    END FOR

    coverage ← (LENGTH(requiredDomains) - LENGTH(missingDomains)) / LENGTH(requiredDomains)
    check6.score ← ROUND(15 * coverage)

    IF LENGTH(missingDomains) === 0 THEN
        check6.result ← "PASS"
    ELSE
        check6.result ← "PARTIAL"
    END IF

    check6.details ← {
        found: LENGTH(domainsFound),
        required: LENGTH(requiredDomains),
        missing: missingDomains,
        coverage: coverage * 100
    }

    results.append(check6)
    totalScore ← totalScore + check6.score


    // CHECK 7: Error handling (no unhandled errors) (10 points)
    check7 ← {
        name: "ErrorHandling",
        maxPoints: 10,
        details: []
    }

    errorSpans ← FILTER(s FOR s IN spans WHERE s.status === "ERROR")
    expectedErrors ← FILTER(s FOR s IN errorSpans WHERE s.attributes.expectedError === true)
    unexpectedErrors ← LENGTH(errorSpans) - LENGTH(expectedErrors)

    IF unexpectedErrors === 0 THEN
        check7.score ← 10
        check7.result ← "PASS"
    ELSE
        check7.score ← MAX(0, 10 - (unexpectedErrors * 2))
        check7.result ← "PARTIAL"
    END IF

    check7.details ← {
        errorSpans: LENGTH(errorSpans),
        expected: LENGTH(expectedErrors),
        unexpected: unexpectedErrors
    }

    results.append(check7)
    totalScore ← totalScore + check7.score


    // CHECK 8: Guard rule comprehensiveness (5 points)
    check8 ← {
        name: "GuardComprehensiveness",
        maxPoints: 5,
        details: []
    }

    guardSpans ← FILTER(s FOR s IN spans WHERE "guard" IN s.name)
    guardRuleTypes ← SET()

    FOR EACH span IN guardSpans DO
        ruleType ← span.attributes.guardRuleType
        guardRuleTypes.add(ruleType)
    END FOR

    expectedRuleTypes ← ["filesystem", "environment", "network", "custom"]
    foundRuleTypes ← 0

    FOR EACH ruleType IN expectedRuleTypes DO
        IF guardRuleTypes.contains(ruleType) THEN
            foundRuleTypes ← foundRuleTypes + 1
        END IF
    END FOR

    check8.score ← ROUND(5 * (foundRuleTypes / LENGTH(expectedRuleTypes)))
    check8.result ← IF foundRuleTypes === LENGTH(expectedRuleTypes) THEN "PASS" ELSE "PARTIAL" END
    check8.details ← {
        ruleTypes: ARRAY(guardRuleTypes),
        coverage: foundRuleTypes / LENGTH(expectedRuleTypes) * 100
    }

    results.append(check8)
    totalScore ← totalScore + check8.score


    RETURN {
        checks: results,
        totalScore: totalScore,
        maxScore: 100,
        scorePercentage: totalScore,
        passed: totalScore >= 80,
        timestamp: CurrentTime(),
        recommendation: IF totalScore >= 80 THEN
                           "PASS: System meets quality threshold"
                        ELSE IF totalScore >= 60 THEN
                           "WARN: Investigate failing checks"
                        ELSE
                           "FAIL: Critical issues detected"
                        END
    }
END
```

---

## Part 4: Coverage Matrix

### Code Coverage Targets

```
MODULE COVERAGE TARGETS:

AgentShards (Unit):
    runtime agent               - 85%+ coverage
    filesystem agent            - 85%+ coverage
    wasm agent                  - 85%+ coverage
    environment agent           - 85%+ coverage
    network agent               - 85%+ coverage
    custom agent (3x)           - 85%+ coverage
    logging agent               - 85%+ coverage
    metrics agent               - 85%+ coverage

    ✓ All utility functions    - 90%+ coverage

Core Services (Integration):
    shard merger                - 80%+ coverage
    receipt generator           - 85%+ coverage
    merkle tree builder         - 90%+ coverage (critical path)
    guard rule engine           - 90%+ coverage (critical path)
    hash validator              - 95%+ coverage (security)

    ✓ Error paths              - 100% coverage
    ✓ Edge cases               - 100% coverage

E2E Workflows:
    probe scan CLI              - 75%+ coverage
    probe verify CLI            - 75%+ coverage
    artifact generation         - 80%+ coverage

    ✓ Teardown/cleanup         - 100% coverage


OBSERVATION COVERAGE TARGETS:

Domain Representation:
    ✓ Runtime agent produces   ≥5 observations
    ✓ FileSystem agent        ≥10 observations
    ✓ WASM agent              ≥3 observations
    ✓ Environment agent       ≥15 observations
    ✓ Network agent           ≥5 observations
    ✓ Custom agents (3x)      ≥2 observations each
    ✓ Logging agent           ≥10 observations
    ✓ Metrics agent           ≥10 observations

    Total: ≥60 observations minimum


GUARD COVERAGE TARGETS:

Guard Rule Types:
    ✓ FileSystem guards       - 5+ rules, 100% tested
    ✓ Environment guards      - 8+ rules, 100% tested
    ✓ Network guards          - 4+ rules, 100% tested
    ✓ Custom guards           - 3+ rules, 100% tested

    Test Coverage:
    ✓ Deny receipts generated - 100%
    ✓ Deny reasons populated  - 100%
    ✓ Hash chains verified    - 100%
```

---

## Part 5: Test Fixtures

### Fixture Categories

```
FIXTURE: FrozenEnvironment (Determinism)
    Path: tests/fixtures/frozen-environment.json

    Contents:
    {
        "projectPath": "/tmp/test-project-12345",
        "files": [
            {
                "path": "package.json",
                "content": "{ ... }",
                "hash": "abc123..."
            },
            {
                "path": "src/index.js",
                "content": "console.log(...)",
                "hash": "def456..."
            }
        ],
        "environment": {
            "NODE_ENV": "test",
            "DEBUG": "false",
            "CUSTOM_VAR": "value"
        },
        "systemTime": "2025-12-27T10:00:00Z",
        "metadata": {
            "seed": 42,
            "version": "1.0"
        }
    }

PURPOSE:
    Recreate identical conditions for determinism testing.
    All sources of non-determinism (time, random, env) are frozen.


FIXTURE: PrecalculatedShards (Merge Testing)
    Path: tests/fixtures/shards/*.json

    Example: tests/fixtures/shards/runtime_shard_1.json
    {
        "domain": "runtime",
        "observationCount": 7,
        "observations": [
            {
                "id": "runtime-001",
                "type": "function_call",
                "name": "processData",
                "duration": 45,
                "status": "success",
                "timestamp": "2025-12-27T10:00:00Z"
            },
            ...
        ],
        "receipts": [
            {
                "observationId": "runtime-001",
                "hash": "abc123...",
                "timestamp": "2025-12-27T10:00:00Z"
            }
        ],
        "hash": "complete_shard_hash_abc123...",
        "preComputed": true
    }

PURPOSE:
    Provide known shard outputs for merge testing.
    Enables verification without running actual agents.
    Hash values used for Merkle tree validation.


FIXTURE: ForbiddenObservations (Guard Testing)
    Path: tests/fixtures/guard-test-cases.json

    {
        "testCases": [
            {
                "name": "Sensitive env variable",
                "observation": {
                    "domain": "environment",
                    "variable": "AWS_SECRET_ACCESS_KEY",
                    "value": "AKIAIOSFODNN7EXAMPLE"
                },
                "expectDeny": true,
                "guardRule": "deny_aws_secrets"
            },
            {
                "name": "System password file",
                "observation": {
                    "domain": "filesystem",
                    "path": "/etc/passwd",
                    "content": "root:x:0:0:..."
                },
                "expectDeny": true,
                "guardRule": "deny_system_files"
            },
            {
                "name": "Valid environment var",
                "observation": {
                    "domain": "environment",
                    "variable": "NODE_ENV",
                    "value": "production"
                },
                "expectDeny": false,
                "guardRule": null
            }
        ]
    }

PURPOSE:
    Test guard enforcement with known forbidden patterns.
    Verify correct DENY receipts generated.
    Test both positive and negative cases.


FIXTURE: ReceiptChainData (Receipt Verification)
    Path: tests/fixtures/receipt-chain.json

    {
        "observations": [
            {
                "id": "obs-001",
                "data": "function executed",
                "hash": "hash1abc..."
            },
            {
                "id": "obs-002",
                "data": "file accessed",
                "hash": "hash2def..."
            }
        ],
        "receipts": [
            {
                "observationId": "obs-001",
                "observationHash": "hash1abc...",
                "previousReceiptHash": null,
                "timestamp": "2025-12-27T10:00:00Z",
                "receiptHash": "receipt1xyz..."
            },
            {
                "observationId": "obs-002",
                "observationHash": "hash2def...",
                "previousReceiptHash": "receipt1xyz...",
                "timestamp": "2025-12-27T10:00:01Z",
                "receiptHash": "receipt2uva..."
            }
        ],
        "merkleProof": {
            "root": "merkle_root_hash...",
            "paths": [
                {
                    "observationIndex": 0,
                    "siblings": ["hash2def...", "hash3ghi..."],
                    "directions": ["RIGHT", "LEFT"]
                }
            ]
        }
    }

PURPOSE:
    Pre-computed receipt chains and Merkle proofs.
    Validate cryptographic linking and Merkle verification.
    Test edge cases (root, leaves, middle nodes).


FIXTURE: RealProjectSnapshot (E2E Testing)
    Path: tests/fixtures/test-projects/real-project-1/

    Structure:
    test-projects/real-project-1/
    ├── package.json          (Real package metadata)
    ├── .env.test             (Mock environment)
    ├── src/
    │   ├── index.js
    │   ├── utils.js
    │   └── secrets.json      (To be caught by guards)
    ├── config/
    │   ├── database.js       (Sensitive config)
    │   └── api.js
    ├── node_modules/         (Symlink or stub)
    └── .gitignore

PURPOSE:
    Realistic project structure for E2E testing.
    Includes files that should trigger guards.
    Realistic for CLI execution testing.
    Used to generate baseline observations.


FIXTURE: ExpectedBaselineResults (Validation)
    Path: tests/fixtures/baselines/e2e-baseline.json

    {
        "expectedObservationCount": 65,
        "expectedReceiptCount": 68,
        "expectedDomainCoverage": [
            "runtime", "filesystem", "wasm", "environment",
            "network", "custom_1", "custom_2", "custom_3",
            "logging", "metrics"
        ],
        "expectedDenyCount": 8,
        "expectedMerkleRoot": "baseline_root_hash...",
        "performanceTargets": {
            "scanDuration": "< 30000ms",
            "verifyDuration": "< 10000ms",
            "totalWorkflow": "< 40000ms"
        }
    }

PURPOSE:
    Baseline for E2E test validation.
    Verify observed counts match expectations.
    Performance regression detection.
```

---

## Part 6: Implementation Roadmap

### Phase 1: Foundation (Week 1)

```
TASK 1.1: Test infrastructure
    - Set up Jest/Vitest test runner
    - Configure test environment isolation
    - Create fixture loading utilities
    - Implement hash/crypto test helpers
    TARGET: Can run `npm test`

TASK 1.2: Frozen environment fixture
    - Capture test project snapshot
    - Create determinism fixture
    - Implement environment freezer/restorer
    TARGET: Frozen environment reproducible

TASK 1.3: Precalculated shards
    - Define shard JSON schema
    - Generate fixtures for all 10 domains
    - Compute and verify hashes
    TARGET: All fixtures validated
```

### Phase 2: Essential Tests (Week 2)

```
TASK 2.1: Test 1 - Determinism
    - Implement FreezeEnvironment
    - Implement RunProbeScan wrapper
    - Implement HashComparison
    - Create test with n=10 runs
    TARGET: Test 1 passing, 0% variance

TASK 2.2: Test 2 - Guard Enforcement
    - Implement ApplyGuards logic
    - Create forbidden observation cases
    - Implement ReceiptValidator
    TARGET: Test 2 passing, 100% guards covered

TASK 2.3: Test 3 - Merge Correctness
    - Implement MergeShardsWithVerification
    - Implement MerkleRootComputation
    - Test with 10 domains + partial cases
    TARGET: Test 3 passing, all paths verified

TASK 2.4: Test 4 - Receipt Verification
    - Implement ReceiptChainValidator
    - Implement MerkleProofValidator
    - Create test with all verification types
    TARGET: Test 4 passing, 100% coverage

TASK 2.5: Test 5 - E2E Integration
    - Implement CLI command wrappers
    - Implement artifact validators
    - Create real project fixture
    - Implement baseline comparison
    TARGET: Test 5 passing, full workflow validated
```

### Phase 3: OTEL & Coverage (Week 3)

```
TASK 3.1: OTEL instrumentation
    - Add span generation to probe scan
    - Add span generation to merge
    - Add span generation to verify
    - Implement observation guard status tracking
    TARGET: All critical paths instrumented

TASK 3.2: OTEL validation harness
    - Implement all 8 validation checks
    - Implement scoring function
    - Create validation report generator
    TARGET: Can run validation, produces 0-100 score

TASK 3.3: Coverage measurement
    - Add Jest coverage reporting
    - Add observation coverage analysis
    - Add guard coverage matrix
    TARGET: Coverage reports show targets met

TASK 3.4: CI/CD integration
    - Add test runs to GitHub Actions
    - Add coverage reports
    - Add OTEL validation to CI
    - Configure SLA checks
    TARGET: Tests run on every commit
```

### Phase 4: Refinement (Week 4)

```
TASK 4.1: Performance optimization
    - Profile test execution
    - Optimize slow tests
    - Reduce CI run time
    TARGET: All tests complete in <5 minutes

TASK 4.2: Documentation
    - Document test strategy
    - Create fixture guide
    - Create debugging guide
    TARGET: New developer can add tests in 1 hour

TASK 4.3: Baseline validation
    - Run all tests 100+ times
    - Verify determinism holds
    - Create baseline reports
    TARGET: Confidence in test stability

TASK 4.4: Quality gates
    - Enforce OTEL ≥80/100
    - Enforce coverage ≥80%
    - Enforce all tests passing
    - Configure blocking PR checks
    TARGET: Quality guaranteed by CI
```

---

## Key Success Metrics

```
Metric                          Target      Measurement
─────────────────────────────── ─────────── ──────────────────────
OTEL Validation Score           ≥80/100     `npm run validate:otel`
Test Pass Rate                  100%        Test output summary
Code Coverage                   ≥85%        Jest coverage report
Observation Coverage            100%        Domain count check
Guard Coverage                  100%        Rule matrix validation
Determinism Variance            0%          10 runs hash compare
Performance (scan)              <30s        OTEL duration span
Performance (verify)            <10s        OTEL duration span
Receipt Verification Rate       100%        Cryptographic validation
Forbidden Payload Detection     0 escapes   Pattern matching scan
```

---

## Related Documentation

- **Implementation Files**: `/home/user/unrdf/src/test/`
- **OTEL Configuration**: `/home/user/unrdf/config/otel.json`
- **Fixture Directory**: `/home/user/unrdf/tests/fixtures/`
- **Coverage Reports**: `/home/user/unrdf/coverage/`
- **CLAUDE.md**: Project instructions and quality standards
- **BB80/20 Methodology**: `docs/bb80-20-methodology.md`
