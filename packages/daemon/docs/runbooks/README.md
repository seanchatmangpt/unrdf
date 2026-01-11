# Operational Runbooks for @unrdf/daemon

Concise, field-tested runbooks for on-call operators and compliance teams based on the FinOps Fabric E2E JTBD test scenarios.

## Overview

Each runbook guides operators through a specific **Job-To-Be-Done** (JTBD) in production:
- Deploy new controls rapidly
- Prove changes to auditors
- Diagnose failures without blame
- Enforce compliance ownership
- Minimize change blast radius
- Ensure long-term system correctness

**Format**: Each runbook is <1 page, actionable in <30 minutes.

## When to Use These Runbooks

| Scenario | Use This Runbook | Typical Frequency |
|----------|------------------|-------------------|
| Deploying a new financial policy | [JTBD #1: Deploy New Control](#jtbd-1-deploy-new-control-in-hours-not-quarters) | Per-change (weekly) |
| Auditor asks "What changed?" | [JTBD #2: Audit Verification](#jtbd-2-prove-what-changed-to-auditor) | On-demand (quarterly) |
| Control failure alert fires | [JTBD #3: Failure Diagnosis](#jtbd-3-diagnose-control-failure-without-operational-blame) | Incident response (rare) |
| Verifying compliance approval | [JTBD #4: Compliance Ownership](#jtbd-4-ensure-compliance-team-owns-rules-not-engineers) | Per-change (weekly) |
| Minimizing deployment risk | [JTBD #5: Minimal Blast Radius](#jtbd-5-minimize-blast-radius-of-changes) | Per-change (weekly) |
| Quarterly compliance review | [JTBD #6: Long-Term Correctness](#jtbd-6-ensure-system-remains-correct-over-time) | Quarterly |

## Runbook Quick Reference

### JTBD #1: Deploy New Control in Hours, Not Quarters
**File**: [`jtbd-1-deploy-new-control.md`](./jtbd-1-deploy-new-control.md)

**When**: Compliance team approved a new policy, need to enforce it in production

**Goal**: Policy definition → operational enforcement in <2 hours

**Key Steps**:
1. Submit policy change (ΔO)
2. Validate ontology (O ⊨ Σ)
3. Generate enforcer (μ(O) → A)
4. Run golden tests
5. Deploy to test, then production

**Success**: Control operational in hours, not quarters. Receipt proves correctness.

---

### JTBD #2: Prove What Changed to Auditor
**File**: [`jtbd-2-audit-verification.md`](./jtbd-2-audit-verification.md)

**When**: Auditor asks for proof that deployed controls match approved policies

**Goal**: Independent verification without human testimony

**Key Steps**:
1. Retrieve deployment receipt
2. Fetch ontology snapshots (O_old, O_new)
3. Replay generation (auditor regenerates A)
4. Verify integrity (hash match)
5. Build audit trail across multiple changes

**Success**: Auditor verifies correctness independently. Zero interviews required.

---

### JTBD #3: Diagnose Control Failure Without Operational Blame
**File**: [`jtbd-3-failure-diagnosis.md`](./jtbd-3-failure-diagnosis.md)

**When**: Production alert fires: control allowed/blocked transaction incorrectly

**Goal**: Prove failure is in generator (μ), configuration (O), or runtime (not operations)

**Key Steps**:
1. Capture failure details (transaction that failed)
2. Retrieve enforcer receipt
3. Replay generation in clean environment
4. Run golden tests against replayed enforcer
5. Escalate to correct team (generator, config, or infrastructure)

**Success**: Root cause found in <30 min. Operations team cleared of blame.

---

### JTBD #4: Ensure Compliance Team Owns Rules, Not Engineers
**File**: [`jtbd-4-compliance-ownership.md`](./jtbd-4-compliance-ownership.md)

**When**: Verifying that deployed controls reflect compliance decisions, not engineer preferences

**Goal**: Prove compliance team designed controls (with audit trail)

**Key Steps**:
1. Identify policy change PR
2. Verify compliance team approval
3. Review PR discussion for compliance-driven requirements
4. Verify no direct code commits (all generated from O)
5. Verify receipt binds to approved O

**Success**: Compliance owns controls. Engineers cannot bypass approval. Auditor confirms.

---

### JTBD #5: Minimize Blast Radius of Changes
**File**: [`jtbd-5-minimal-blast-radius.md`](./jtbd-5-minimal-blast-radius.md)

**When**: Deploying policy change, need to minimize risk of unintended side effects

**Goal**: Prove only affected components changed (no unnecessary recompilation)

**Key Steps**:
1. Compute ontology diff (what changed in O)
2. Generate new enforcer
3. Compute enforcer diff (what changed in A)
4. Verify minimality (only affected processes recompiled)
5. Verify rollback capability (deterministic restore)

**Success**: Blast radius ≤10% for single-rule change. Fast rollback available.

---

### JTBD #6: Ensure System Remains Correct Over Time
**File**: [`jtbd-6-long-term-correctness.md`](./jtbd-6-long-term-correctness.md)

**When**: Quarterly compliance review or generator upgrade

**Goal**: Prove accumulated changes remain coherent, no drift or technical debt

**Key Steps**:
1. Collect all historical receipts
2. Compute merged ontology state (O_tn)
3. Validate merged ontology
4. Regenerate from scratch (verify idempotence)
5. Compare fresh vs production (detect drift)
6. Test generator upgrade (Kaizen)

**Success**: No drift. No technical debt. Generator upgrades are safe. Compliance verified.

---

## Runbook Conventions

### Structure
Every runbook follows this format:
- **Objective**: One sentence goal
- **Prerequisites**: Required access, tools, data
- **Steps**: Numbered, actionable, <10 steps
- **Success Criteria**: Observable outcomes (checkboxes)
- **Troubleshooting**: If X then Y (symptom → action)
- **References**: Test scenarios, related runbooks

### Execution Time
- Most runbooks: <30 minutes
- Incident response (JTBD #3): <30 minutes (urgent)
- Quarterly verification (JTBD #6): 1-2 hours (thorough)

### Roles
- **Operator**: Executes runbooks (daily/weekly tasks)
- **Compliance Team**: Approves policies, reviews audit reports
- **Auditor**: Independent verifier (uses receipts)
- **Engineer**: Escalation target for μ bugs or infrastructure issues

## Integration with Test Suite

These runbooks operationalize the E2E JTBD tests:

| Runbook | Test Scenario | Test File Reference |
|---------|---------------|---------------------|
| JTBD #1 | 1.1, 1.2 | `finops-fabric-e2e-jtbd-tests.md` lines 16-81 |
| JTBD #2 | 2.1, 2.2 | `finops-fabric-e2e-jtbd-tests.md` lines 83-142 |
| JTBD #3 | 3.1, 3.2 | `finops-fabric-e2e-jtbd-tests.md` lines 145-204 |
| JTBD #4 | 4.1, 4.2 | `finops-fabric-e2e-jtbd-tests.md` lines 207-265 |
| JTBD #5 | 5.1, 5.2 | `finops-fabric-e2e-jtbd-tests.md` lines 268-326 |
| JTBD #6 | 6.1, 6.2 | `finops-fabric-e2e-jtbd-tests.md` lines 329-386 |

**Test-Driven Runbooks**: Each runbook step corresponds to a test scenario step. If tests pass, runbooks work in production.

## Doctrine & TPS Alignment

Every runbook enforces at least one doctrine invariant and TPS gate:

| Runbook | Doctrine Invariant | TPS Gate |
|---------|-------------------|----------|
| JTBD #1 | Correct-by-Construction | Determinism, Ontology |
| JTBD #2 | Provenance | Replay |
| JTBD #3 | Fault Containment | Operational |
| JTBD #4 | Typing | Ontology, Jidoka |
| JTBD #5 | Minimality | Waste Elimination |
| JTBD #6 | Idempotence | Kaizen |

**Quality Assurance**: If doctrine/TPS violations occur, runbook troubleshooting sections guide escalation.

## Common Patterns Across Runbooks

### Receipt Verification
Most runbooks verify receipts:
```bash
# Extract receipt hash
receipt_hash=$(jq -r '.receipt_hash' receipt.json)

# Verify receipt signature
μ verify-receipt --receipt=receipt.json --public-key=auditor.pub

# Compare hashes
[[ "$production_hash" == "$receipt_hash" ]] || echo "MISMATCH"
```

### Golden Test Execution
Multiple runbooks run golden tests:
```bash
# Run golden test suite
μ test --enforcer=A_new --golden-suite

# Run specific scenario
μ test --enforcer=A_new --scenario=transaction_105k_L2_actor
```

### Determinism Verification
Critical for rollback and audit:
```bash
# Generate twice, verify identical
μ generate --input=O --output=A1
μ generate --input=O --output=A2
diff <(sha256sum A1) <(sha256sum A2)  # Should be identical
```

## Troubleshooting Decision Tree

```
Control Failure Alert
    ├─> Receipt available?
    │   ├─> YES: Use JTBD #3 (Failure Diagnosis)
    │   └─> NO: Check if deployment followed JTBD #1 (Deploy Control)
    │
    ├─> Auditor asks for proof?
    │   └─> Use JTBD #2 (Audit Verification)
    │
    ├─> Compliance dispute?
    │   └─> Use JTBD #4 (Compliance Ownership)
    │
    ├─> Deployment risk concern?
    │   └─> Use JTBD #5 (Minimal Blast Radius)
    │
    └─> Quarterly review?
        └─> Use JTBD #6 (Long-Term Correctness)
```

## Getting Started

### First-Time Operators
1. Read: [`jtbd-1-deploy-new-control.md`](./jtbd-1-deploy-new-control.md) (most common workflow)
2. Practice: Run in test environment with sample policy change
3. Verify: Check that receipt is generated correctly

### Compliance Teams
1. Read: [`jtbd-4-compliance-ownership.md`](./jtbd-4-compliance-ownership.md) (your approval workflow)
2. Review: PR approval process ensures compliance gate
3. Audit: Use [`jtbd-2-audit-verification.md`](./jtbd-2-audit-verification.md) for quarterly reviews

### On-Call Engineers
1. Keep handy: [`jtbd-3-failure-diagnosis.md`](./jtbd-3-failure-diagnosis.md) (incident response)
2. Know escalation: μ bugs vs configuration vs infrastructure
3. Practice: Simulate failure scenario in test environment

## Continuous Improvement

These runbooks evolve based on operational learnings:
- **After incident**: Update troubleshooting section with new patterns
- **After audit**: Add clarifications from auditor questions
- **After μ upgrade**: Update commands for new ggen versions

**Contribution**: Operators should propose runbook improvements via PR (treat runbooks like code).

## References

- **Source Tests**: `/home/user/unrdf/docs/finops-fabric-e2e-jtbd-tests.md`
- **Doctrine**: `/home/user/unrdf/docs/finops-fabric-coherence-proof.md`
- **Daemon Docs**: `/home/user/unrdf/packages/daemon/docs/README.md`
- **TPS Production System**: Press release promises + production gates

---

**Last Updated**: 2026-01-10
**Version**: 1.0.0
**Maintained By**: Operations Team + Compliance Team
