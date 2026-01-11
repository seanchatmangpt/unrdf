# @unrdf/daemon - E2E JTBD Integration Coverage Matrix

**Date**: 2026-01-10
**Purpose**: Map which E2E test scenarios exercise which daemon integrations
**Methodology**: Analysis of 12 JTBD scenarios against 6 integration points

---

## Integration Coverage Matrix

```
Integration               JTBD 1.1 | JTBD 1.2 | JTBD 2.1 | JTBD 2.2 | JTBD 3.1 | JTBD 3.2 | JTBD 4.1 | JTBD 4.2 | JTBD 5.1 | JTBD 5.2 | JTBD 6.1 | JTBD 6.2
@unrdf/hooks            |    ✓    |    ✓    |    -    |    -    |    ✓    |    ✓    |    ✓    |    ✓    |    ✓    |    ✓    |    ✓    |    ✓
@unrdf/streaming        |    -    |    -    |    -    |    -    |    -    |    ✓    |    -    |    -    |    -    |    -    |    -    |    -
@unrdf/consensus        |    -    |    ✓    |    -    |    -    |    -    |    ✓    |    -    |    -    |    ✓    |    ✓    |    -    |    -
@unrdf/kgc-4d           |    ✓    |    ✓    |    ✓    |    ✓    |    ✓    |    -    |    ✓    |    ✓    |    -    |    ✓    |    ✓    |    ✓
@unrdf/receipts         |    ✓    |    ✓    |    ✓    |    ✓    |    ✓    |    -    |    ✓    |    ✓    |    ✓    |    ✓    |    ✓    |    ✓
@unrdf/observability    |    -    |    -    |    -    |    ✓    |    ✓    |    ✓    |    -    |    ✓    |    -    |    -    |    ✓    |    -
```

### Legend

| Symbol | Meaning |
|--------|---------|
| ✓ | Scenario exercises this integration |
| - | Integration not used in scenario |

---

## Scenario Breakdown: Which Integrations Are Exercised

### JTBD 1: "I need to deploy a new control in hours, not quarters"

**Scenario 1.1: Basic approval rule generation**
- **Description**: Submit approval rule → generate enforcer → test → deploy
- **Key Flow**: Policy spec → Hook-scheduled enforcement → Receipt proof
- **Integrations Used**:
  - **@unrdf/hooks** ✓ - Register approval rule as schedulable hook
  - **@unrdf/kgc-4d** ✓ - Event-source the policy change
  - **@unrdf/receipts** ✓ - Generate receipt proving generation < 2 min

**Scenario 1.2: Three interdependent rules (atomic deployment)**
- **Description**: Submit 3 rules (approval + notification + audit) → all deploy together
- **Key Flow**: Multi-rule specification → atomicity guarantee → cluster-wide deployment
- **Integrations Used**:
  - **@unrdf/hooks** ✓ - Register all 3 hooks atomically
  - **@unrdf/kgc-4d** ✓ - Event-source all 3 changes with causal relationship
  - **@unrdf/consensus** ✓ - Coordinate atomic deployment across cluster
  - **@unrdf/receipts** ✓ - Batch receipt for all 3 rules together

---

### JTBD 2: "I need to know exactly what changed and why, and prove it to my auditor"

**Scenario 2.1: Auditor receipt replay verification**
- **Description**: Auditor takes receipt → regenerates code → verifies hash match
- **Key Flow**: Receipt retrieval → generator replay → deterministic verification
- **Integrations Used**:
  - **@unrdf/kgc-4d** ✓ - Retrieve original policy specification (O)
  - **@unrdf/receipts** ✓ - Extract receipt, verify cryptographic hash (A)

**Scenario 2.2: Receipt chain over time (coherence verification)**
- **Description**: Verify all policy changes over a week form unbroken receipt chain
- **Key Flow**: Timeline of receipts → chain continuity → audit trail completeness
- **Integrations Used**:
  - **@unrdf/kgc-4d** ✓ - Event timeline with causality (O_old → ΔO chain)
  - **@unrdf/receipts** ✓ - Receipt chain linkage (R_n references R_{n-1})
  - **@unrdf/observability** ✓ - Monitor receipt generation SLAs, track completeness metrics

---

### JTBD 3: "I need my operations team to never be surprised by a control failure"

**Scenario 3.1: Control failure diagnosis**
- **Description**: Control fails in production → ops retrieves receipt → regenerates → verifies if enforcer is correct
- **Key Flow**: Failure observation → receipt-based diagnosis → root cause isolation
- **Integrations Used**:
  - **@unrdf/kgc-4d** ✓ - Retrieve spec (O) at time of failure
  - **@unrdf/receipts** ✓ - Verify generated code (A) matches receipt (proves generation correctness)
  - **@unrdf/observability** ✓ - Health metrics proving which component failed

**Scenario 3.2: Graceful degradation under failure (e.g., notification service down)**
- **Description**: Notification service crashes → approval process continues → system degrades gracefully
- **Key Flow**: Fault detection → hook suspension → streaming reactive recovery → cluster resilience
- **Integrations Used**:
  - **@unrdf/hooks** ✓ - Suspend notification hook, keep approval hook running
  - **@unrdf/streaming** ✓ - Subscribe to notification service health feed, reactively retry
  - **@unrdf/consensus** ✓ - Leader detects node failure, rebalances work
  - **@unrdf/observability** ✓ - Alert operator of degraded state (success rate 80-95%)

---

### JTBD 4: "I need to know that my compliance team wrote the rules, not my engineers"

**Scenario 4.1: Policy change with compliance approval**
- **Description**: Compliance writes spec → engineers review syntax (not logic) → system generates code
- **Key Flow**: Compliance spec ownership → enforcement chain → immutable receipt
- **Integrations Used**:
  - **@unrdf/hooks** ✓ - Enforce generated hooks (not engineer-written)
  - **@unrdf/kgc-4d** ✓ - Event-source policy ownership (who approved? when?)
  - **@unrdf/receipts** ✓ - Receipt proves compliance team approved O, not engineers modified A

**Scenario 4.2: Policy constraint violation rejection (e.g., SOD violation)**
- **Description**: Engineer submits spec that violates segregation-of-duties → system rejects before generation
- **Key Flow**: Constraint validation → Andon event → alert compliance team
- **Integrations Used**:
  - **@unrdf/hooks** ✓ - Validation hook prevents non-compliant enforcement
  - **@unrdf/kgc-4d** ✓ - Schema validation against compliance constraints
  - **@unrdf/receipts** ✓ - Receipt documents rejection (immutable proof)
  - **@unrdf/observability** ✓ - Alert operator of constraint violation

---

### JTBD 5: "I need to minimize the blast radius if something goes wrong"

**Scenario 5.1: Single rule change, other 14 rules untouched**
- **Description**: Change approval threshold → only 1 hook regenerated → verify 14 others byte-identical
- **Key Flow**: Minimal diff → selective re-hook → hash comparison → risk proportional to change
- **Integrations Used**:
  - **@unrdf/hooks** ✓ - Selectively regenerate only changed hook
  - **@unrdf/consensus** ✓ - Coordinate minimal deployment (1 node updated, others untouched)
  - **@unrdf/receipts** ✓ - Hash unchanged hooks to prove no mutation

**Scenario 5.2: Post-deployment rollback (deterministic regeneration)**
- **Description**: Bug discovered → retrieve previous receipt → regenerate old version → deterministically deploy
- **Key Flow**: Version history → deterministic μ(O_old) → identical A_old → safe rollback
- **Integrations Used**:
  - **@unrdf/kgc-4d** ✓ - Retrieve O_old and μ version from historical receipt
  - **@unrdf/hooks** ✓ - Re-deploy previous hook version
  - **@unrdf/consensus** ✓ - Coordinate rollback across cluster (revert leadership state if needed)
  - **@unrdf/receipts** ✓ - Verify regenerated A_old matches historical receipt hash

---

### JTBD 6: "I need to know this system will still be operationally correct a year from now"

**Scenario 6.1: Long-term drift detection (6-month regeneration)**
- **Description**: After 6 months of changes, regenerate entire system from merged spec → compare with production
- **Key Flow**: Specification as single source of truth → temporal consistency → drift detection
- **Integrations Used**:
  - **@unrdf/kgc-4d** ✓ - Merge all O changes from 6 months into single spec snapshot
  - **@unrdf/receipts** ✓ - Compare regenerated hash (μ(merged_O)) with production hash
  - **@unrdf/observability** ✓ - Measure drift metrics (0 = perfect alignment)

**Scenario 6.2: Generator upgrade idempotence (upgrade μ version, behavior unchanged)**
- **Description**: Update μ to new version → regenerate all O with new μ → verify A identical (internal improvements only)
- **Key Flow**: Generational improvement → behavior invariance → zero customer impact
- **Integrations Used**:
  - **@unrdf/hooks** ✓ - Re-execute all hooks with new generator
  - **@unrdf/kgc-4d** ✓ - Compare audit trails (μ(O) with μ_old vs μ_new)
  - **@unrdf/receipts** ✓ - Hash-verify A_old == A_new (behavior unchanged)

---

## Coverage Summary

### Integration Coverage by Frequency

| Integration | Used In | Coverage % | Status |
|-------------|---------|-----------|--------|
| **@unrdf/hooks** | 10/12 scenarios | 83% | **WELL-COVERED** |
| **@unrdf/receipts** | 10/12 scenarios | 83% | **WELL-COVERED** |
| **@unrdf/kgc-4d** | 10/12 scenarios | 83% | **WELL-COVERED** |
| **@unrdf/observability** | 5/12 scenarios | 42% | MODERATE |
| **@unrdf/consensus** | 5/12 scenarios | 42% | MODERATE |
| **@unrdf/streaming** | 1/12 scenarios | 8% | **LIGHT** |

### Scenario Coverage by Completeness

| JTBD | Scenario | Integrations Tested | Coverage |
|------|----------|-------------------|----------|
| 1 | 1.1 | 3/6 (hooks, kgc-4d, receipts) | CORE |
| 1 | 1.2 | 4/6 (hooks, kgc-4d, consensus, receipts) | EXTENDED |
| 2 | 2.1 | 2/6 (kgc-4d, receipts) | MINIMAL |
| 2 | 2.2 | 3/6 (kgc-4d, receipts, observability) | CORE |
| 3 | 3.1 | 3/6 (kgc-4d, receipts, observability) | CORE |
| 3 | 3.2 | 4/6 (hooks, streaming, consensus, observability) | EXTENDED |
| 4 | 4.1 | 3/6 (hooks, kgc-4d, receipts) | CORE |
| 4 | 4.2 | 4/6 (hooks, kgc-4d, receipts, observability) | EXTENDED |
| 5 | 5.1 | 3/6 (hooks, consensus, receipts) | CORE |
| 5 | 5.2 | 4/6 (kgc-4d, hooks, consensus, receipts) | EXTENDED |
| 6 | 6.1 | 3/6 (kgc-4d, receipts, observability) | CORE |
| 6 | 6.2 | 3/6 (hooks, kgc-4d, receipts) | CORE |

---

## Gap Analysis

### Under-Tested Integrations (High Priority)

**1. @unrdf/streaming (8% coverage - CRITICAL GAP)**
- **Current Use**: Only scenario 3.2 (graceful degradation)
- **Why This Matters**: Streaming enables reactive, event-driven daemon operations. Low coverage means:
  - Change feed subscriptions untested
  - Pattern-matching triggers untested
  - Backpressure handling untested
  - Reactive recovery mechanisms untested
- **Impact**: Production failures in reactive workflows; streaming-related bugs slip through
- **Severity**: HIGH - Streaming is core to "push-pull" architectures

**2. @unrdf/observability (42% coverage - MODERATE GAP)**
- **Current Use**: Scenarios 2.2, 3.1, 3.2, 4.2, 6.1 (health, alerts, metrics)
- **Why This Matters**: Observability is often treated as "nice to have" but is critical for:
  - Verifying SLAs (time-to-policy < 2 min)
  - Detecting subtle performance regressions
  - Supporting auditor queries ("how long did deployment take?")
- **Missing Coverage**:
  - Time-to-policy metrics (JTBD 1) - not explicitly tested
  - Performance regression detection (JTBD 5) - not explicitly tested
  - Correctness metrics (JTBD 6) - only scenario 6.1
- **Severity**: MODERATE-HIGH - Observability gaps prevent SLA verification

**3. @unrdf/consensus (42% coverage - MODERATE GAP)**
- **Current Use**: Scenarios 1.2, 3.2, 5.1, 5.2 (cluster coordination)
- **Why This Matters**: Consensus is critical for:
  - Atomic deployments (multiple rules)
  - Distributed failure recovery
  - Minimal blast radius (selective deployment)
  - Deterministic rollbacks
- **Missing Coverage**:
  - Basic cluster formation (no test)
  - Leader election under partitions (no test)
  - Byzantine fault tolerance (no test)
  - Cluster rebalancing during node failure (only scenario 3.2)
- **Severity**: MODERATE - Consensus gaps risk data loss in multi-node scenarios

### Light-Use Integrations (Lower Priority)

**@unrdf/streaming** - Only used for reactive degradation:
- Recommend: Add scenario for "deployment triggered by change feed event"
- Recommend: Add scenario for "policy change subscription" (compliance watching for unauthorized changes)

---

## Gap Closure Recommendations (80/20 Analysis)

### Top 3 Gaps That Matter Most (20% effort, 80% risk reduction)

**PRIORITY 1: Add Streaming to Core Scenario (JTBD 3.2 upgrade)**
- **Current**: 3.2 tests graceful degradation (notification service down)
- **Gap**: Doesn't test reactive trigger execution
- **Recommendation**: Extend scenario 3.2 to explicitly verify:
  - Pattern-matching triggers fire on change events
  - Backpressure handling prevents queue overflow
  - Reactive recovery completes < SLA
- **Effort**: 2-3 hours (add streaming subscription to failure scenario)
- **Risk Reduction**: HIGH - Catches streaming bugs before production
- **Test Coverage Impact**: +3-5% overall, streaming moves from 8% → 25%

**PRIORITY 2: Add Observability to JTBD 1 (Time-to-Policy Metrics)**
- **Current**: JTBD 1 scenarios (1.1, 1.2) don't explicitly test latency metrics
- **Gap**: SLA claims ("< 2 minutes") aren't verified by observability
- **Recommendation**:
  - Scenario 1.1: Verify `enforcement_latency < 2 min` recorded and exported
  - Scenario 1.2: Verify batch receipt includes latency breakdown (validation + generation + test + deploy)
- **Effort**: 1-2 hours (instrument existing tests, add metric assertions)
- **Risk Reduction**: HIGH - Prevents silent SLA violations
- **Test Coverage Impact**: +2-3%, observability moves from 42% → 50%

**PRIORITY 3: Add Consensus to JTBD 5 (Cluster Rebalancing)**
- **Current**: 5.1 tests minimal change (1 of 15 hooks), 5.2 tests rollback
- **Gap**: Doesn't test rebalancing when a node fails mid-deployment
- **Recommendation**: Add scenario 5.3 (optional):
  - Deploy 5 hook updates across 3-node cluster
  - During deployment, node 2 crashes
  - Verify: Remaining 2 nodes complete deployment, failed updates retried
  - Verify: Post-recovery rebalancing distributes load evenly
- **Effort**: 3-4 hours (new scenario, cluster failure injection)
- **Risk Reduction**: HIGH - Prevents data loss in multi-node clusters
- **Test Coverage Impact**: +2-3%, consensus moves from 42% → 50%

---

## Implementation Roadmap

### Phase 1: High-Impact Gaps (1 sprint)
1. **Streaming Gap** (JTBD 3.2 upgrade) - 2-3 hours
2. **Observability Gap** (JTBD 1.1/1.2 upgrade) - 1-2 hours

**Expected Outcome**: Streaming moves to 25%, Observability to 50%, overall matrix completeness 85%

### Phase 2: Moderate Gaps (2-3 sprints)
3. **Consensus Gap** (JTBD 5 cluster rebalancing) - 3-4 hours
4. **Observability Deep Dive** (JTBD 6 correctness metrics) - 2-3 hours

**Expected Outcome**: All integrations > 60% coverage, overall matrix completeness 95%

### Phase 3: Polish (ongoing)
- Capture edge cases (Byzantine failures, partition tolerance)
- Performance regression detection
- Multi-rack deployments

---

## Doctrinal Notes

### Why These Gaps Matter

The UNRDF daemon follows a **correct-by-construction** doctrine:
```
A = μ(O)  ⟹  Production system = μ(Specification)
```

Coverage gaps translate directly to doctrine violations:

| Gap | Doctrine Risk |
|-----|--------|
| Streaming untested | Reactive workflows untrusted (can't prove A = μ(O) for streaming case) |
| Observability untested | SLAs unverified (can't prove time-to-policy < 2 min) |
| Consensus untested | Atomicity unproven (can't guarantee all-or-nothing deployment) |

**Recommendation**: Treat these gaps as blocking issues for production deployment.

---

## Audit Verification Checklist

When auditor reviews daemon testing:

- [ ] Scenario 1.1: Does receipt show `enforcement_latency < 2 min`?
- [ ] Scenario 2.1: Can auditor independently regenerate A from O and verify hash?
- [ ] Scenario 3.1: Does receipt prove generated code is correct (distinguish O bugs from μ bugs)?
- [ ] Scenario 3.2: Can system degrade gracefully when component fails?
- [ ] Scenario 4.1: Does receipt prove compliance team approved spec?
- [ ] Scenario 4.2: Is constraint violation caught before generation?
- [ ] Scenario 5.1: Are unchanged hooks proven byte-identical?
- [ ] Scenario 5.2: Is rollback deterministic (same O → same A)?
- [ ] Scenario 6.1: Is there zero drift between spec and production?
- [ ] Scenario 6.2: Do generator upgrades preserve behavior?

---

**Document**: Integration Coverage Matrix
**Version**: 1.0
**Last Updated**: 2026-01-10
**Maintenance**: Review after each test scenario addition
