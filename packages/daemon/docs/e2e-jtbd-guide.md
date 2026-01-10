# Understanding E2E JTBD Tests: A Guide for Operators and Auditors

**Version**: 1.0  
**Audience**: Operations teams, compliance officers, auditors, non-engineering stakeholders  
**Purpose**: Explain what E2E JTBD tests are, how they work, and how to use them for verification and compliance  

---

## 1. Introduction: What Are E2E JTBD Tests?

### What is a JTBD?

**JTBD** stands for "Job To Be Done" — a customer-centric way of thinking about what a system must accomplish. Instead of asking "what features does the daemon have?", we ask "what job does a customer hire the daemon to do?"

For the UNRDF daemon, customers are typically **operations teams** and **compliance officers** at regulated organizations who need to deploy financial controls quickly, audit them reliably, and ensure they remain correct over time.

### Why Does the Daemon Need JTBD Tests?

Traditional software tests verify that code works ("does function X return Y?"). JTBD tests verify that **customer outcomes are achieved** ("can I deploy a policy in hours instead of quarters?").

The daemon manages background operations, scheduled tasks, and event-driven workflows. These operations often involve:
- **Financial controls** (approval rules, spending limits)
- **Audit requirements** (immutable logs, cryptographic receipts)
- **Compliance constraints** (segregation of duties, escalation paths)

If a test passes but the customer's job isn't done, the test is useless. E2E JTBD tests ensure that **customer jobs are actually completed**, not just that the code executes without errors.

### How Do They Relate to Daemon Architecture?

The daemon's architecture follows a **correct-by-construction** model:
- **O (Ontology)**: The specification of what controls should exist (written by compliance teams)
- **μ (Generator)**: The function that converts O into executable code
- **A (Artifacts)**: The generated code that runs in production

The equation **A = μ(O)** means: "the production system is exactly what was generated from the specification, nothing more, nothing less."

E2E JTBD tests verify this equation holds across all customer scenarios, producing **receipts** (cryptographic proofs) that auditors can independently verify.

---

## 2. The 6 Jobs To Be Done

### JTBD #1: "I need to deploy a new control in hours, not quarters"

**Customer Problem**: Traditional infrastructure changes require code reviews, manual deployments, and coordination across multiple teams. A simple policy change (e.g., "approvals over $100k require VP sign-off") can take weeks or months.

**What Success Looks Like**: A compliance officer writes a policy specification in the morning, and by afternoon it's enforced in production — without writing code, without manual deployment scripts, without engineering bottlenecks.

**How the Test Proves It**:
- **Scenario 1.1**: Submit a new approval rule → system generates enforcer code → code is tested → deployed to production
  - **Success metric**: Generation time < 2 minutes, production enforcement < 30 minutes
- **Scenario 1.2**: Submit three interdependent rules (approval + notification + audit) → system generates all three atomically
  - **Success metric**: All three deploy together or none deploy (no partial state)

**Why This Matters**: Regulated industries need to respond to compliance changes quickly (new regulations, new risk profiles). Slow time-to-policy means compliance risk.

---

### JTBD #2: "I need to know exactly what changed and why, and I need to prove it to my auditor"

**Customer Problem**: Audits are expensive because they require manual interviews, archaeology through git logs, and reconstructing what happened months ago. Auditors ask "who approved this control change?" and the answer is buried in email threads.

**What Success Looks Like**: An auditor receives a **receipt** (a cryptographic record) that contains:
- The exact policy specification (O)
- The exact generated code (A)
- The exact version of the generator (μ)
- A hash proving A was generated from O (not hand-written)

The auditor can **independently reproduce** A from O and verify the hash matches. No human testimony required.

**How the Test Proves It**:
- **Scenario 2.1**: Auditor takes a receipt → runs the generator with the same inputs → verifies output hash matches
  - **Success metric**: Hashes match, proving integrity without trusting operations team
- **Scenario 2.2**: Auditor reviews a chain of receipts over time → verifies every change was documented
  - **Success metric**: No "unexplained" transitions; every change has a receipt

**Why This Matters**: Audit costs scale with manual effort. Receipts reduce audit time from weeks to hours, and reduce audit findings (because everything is documented by construction).

---

### JTBD #3: "I need my operations team to never be surprised by a control failure"

**Customer Problem**: When a financial control fails in production (e.g., an approval that should have been blocked wasn't), operations teams spend days debugging: "Was it a code bug? A configuration error? An operator mistake?"

**What Success Looks Like**: When a control fails, the system **automatically isolates the root cause**:
- If the generated code is correct (verified by receipt), the failure is upstream (in the specification or generator)
- If the generated code is incorrect, the failure is a generator bug

Operations teams don't debug control logic — they escalate to the compliance team (for O fixes) or engineering team (for μ fixes).

**How the Test Proves It**:
- **Scenario 3.1**: Simulate a control failure → operations retrieves receipt → replays generation → verifies enforcer code is correct
  - **Success metric**: Operations concludes "enforcer is correct; failure is not in generated code"
- **Scenario 3.2**: Simulate a process crash (e.g., notification service down) → verify approval process continues unaffected
  - **Success metric**: Approval decisions are not blocked by notification failures (graceful degradation)

**Why This Matters**: Operational surprises are expensive (downtime, compliance violations, reputation damage). Fault isolation and graceful degradation reduce mean-time-to-recovery.

---

### JTBD #4: "I need to know that my compliance team wrote the rules, not my engineers"

**Customer Problem**: In many organizations, engineers write code that "implements" compliance rules, but the compliance team doesn't directly control what's deployed. This creates risk: engineers might misinterpret rules, optimize for performance over correctness, or make unauthorized changes.

**What Success Looks Like**: The compliance team writes the specification (O), and the system **guarantees** that what's in production is exactly what the compliance team approved — no engineer can modify the rules without changing O.

**How the Test Proves It**:
- **Scenario 4.1**: Compliance team submits a policy change → engineers review syntax (not logic) → system generates code from approved policy
  - **Success metric**: Receipt proves generated system matches compliance-approved O
- **Scenario 4.2**: Engineer mistakenly submits a policy that violates a constraint (e.g., segregation of duties) → system rejects it before generation
  - **Success metric**: System blocks compliance violations automatically; compliance team is alerted

**Why This Matters**: Regulatory compliance requires that **compliance teams own controls**, not engineering teams. Receipts prove ownership.

---

### JTBD #5: "I need to minimize the blast radius if something goes wrong"

**Customer Problem**: Traditional deployments often rebuild everything, even if only one small thing changed. This increases risk: more code changes = more potential for bugs.

**What Success Looks Like**: When a single approval rule changes, only that one process is updated. The other 14 processes remain byte-identical to their previous versions. The deployment is **minimal**: restart 1 process, leave 14 processes untouched.

**How the Test Proves It**:
- **Scenario 5.1**: Change one rule → verify only one process is regenerated → verify other 14 processes are unchanged (hash comparison)
  - **Success metric**: Blast radius is bounded; risk is proportional to change size
- **Scenario 5.2**: Discover a bug post-deployment → retrieve receipt from previous version → regenerate old version → deploy old version
  - **Success metric**: Rollback is deterministic (same O → same A), no manual revert scripts

**Why This Matters**: Large blast radius = high risk. Minimal changes reduce the probability of unintended consequences.

---

### JTBD #6: "I need to know this system will still be operationally correct a year from now"

**Customer Problem**: Systems accumulate technical debt over time. Six months of incremental changes can result in a system that nobody fully understands. Subtle interactions between changes cause unexpected behavior.

**What Success Looks Like**: At any time, the compliance team can ask: "If I regenerate the system from scratch using the current specification, will it match production?" The answer should be **yes** (or the delta should be explainable and documented).

**How the Test Proves It**:
- **Scenario 6.1**: After 6 months of changes, regenerate the system from the merged specification → compare with production → verify they match
  - **Success metric**: No drift between specification and production; all changes are documented
- **Scenario 6.2**: Upgrade the generator (μ) to a newer version → regenerate the system → verify behavior is identical (even if internal implementation improved)
  - **Success metric**: Generator improvements don't change policy behavior; improvements are trackable

**Why This Matters**: Technical debt causes compliance drift. Self-validating systems prevent drift by ensuring O is always the single source of truth.

---

## 3. Test Scenarios: How to Read and Interpret

### Anatomy of a Test Scenario

Each JTBD has 2 test scenarios (12 total). Each scenario follows the **Given-When-Then** pattern:

**Given** (Preconditions):
- Current system state (e.g., "15 rules exist, 1 is changing")
- Input data (e.g., "new approval rule with $100k threshold")
- Environment constraints (e.g., "auditor has clean runtime")

**When** (Actions):
- The system performs a sequence of operations
- Example: "μ validates O → generates code → runs tests → produces receipt"

**Then** (Expected Outcomes):
- Verification criteria (checkboxes)
- Example: "[ ] Generation time < 2 minutes"
- Example: "[ ] Receipt hash matches regenerated hash"
- Doctrine invariants verified (e.g., "Correct-by-Construction: VERIFIED")
- TPS gates passed (e.g., "Determinism Gate: PASS")

**Proof of Job Done** (Success Evidence):
- How we know the customer's job was actually completed
- Example: "Auditor produces their own receipt from replay; two receipts are identical"

**Failure Modes Detected** (Error Cases):
- What errors the test catches
- Example: "O violates policy invariants → Andon event, no deployment"

### What "Success Proof" Means

A **success proof** is evidence that can be independently verified by a third party (typically an auditor). It's not just "the test passed" — it's **cryptographic evidence** that the job was done correctly.

Examples:
- **Hash match**: Regenerating A from O produces identical bytecode (hash comparison)
- **Receipt chain**: Every change is documented in a receipt, forming an audit trail
- **Golden test results**: Enforcer code was tested against known scenarios and passed

### How to Interpret Test Output

When a test runs, it produces a **receipt** (see next section). The receipt contains:

**Process Section** (what happened):
```json
"process": {
  "step_1_validation": "PASS",
  "step_2_generation": "PASS (time: 87s)",
  "step_3_golden_tests": "PASS (14/14 tests)",
  "step_4_receipt_production": "PASS"
}
```

**Outputs Section** (results):
```json
"outputs": {
  "A": "hash(...)",
  "enforcement_latency": "< 2 minutes",
  "deployment_latency": "< 30 minutes"
}
```

**Verification Section** (proof):
```json
"doctrine_invariants_checked": [
  "Correct-by-Construction: VERIFIED",
  "Idempotence: VERIFIED",
  "Provenance: VERIFIED"
],
"tps_gates_passed": [
  "Ontology Gate: PASS",
  "Determinism Gate: PASS",
  "Operational Gate: PASS"
]
```

**Interpretation**:
- All steps show "PASS" → test passed
- Any step shows "FAIL" → investigate that specific step
- Latency metrics are compared against SLAs (e.g., "< 2 minutes")
- Doctrine invariants and TPS gates provide **structural guarantees** (not just test pass/fail)

---

## 4. Receipts: The Audit Trail

### What is a Receipt?

A **receipt** is a cryptographic record of an operation. Think of it like a blockchain transaction: it contains:
- **Inputs**: What went into the operation (e.g., policy specification O, generator version μ)
- **Process**: What steps were executed (validation, generation, testing)
- **Outputs**: What was produced (generated code A, hashes, metrics)
- **Proof**: Cryptographic hashes that prove integrity

Receipts are **immutable** and **self-verifying**: anyone with the receipt can independently verify that the operation was performed correctly.

### Receipt Fields and What They Mean

**Metadata**:
- `test_id`: Unique identifier (e.g., "JTBD-1.1-policy-to-enforcer")
- `jtbd`: Which customer job this relates to
- `timestamp`: When the operation occurred

**Inputs** (what you started with):
- `O_old`: Hash of the previous policy specification
- `ΔO`: Hash of the policy change
- `ggen_version`: Exact version of the generator used (e.g., "1.0.1")

**Process** (what happened):
- `step_1_validation`: Did the policy pass validation? ("PASS" or "FAIL")
- `step_2_generation`: Was code generated? (includes timing: "PASS (time: 87s)")
- `step_3_golden_tests`: Did the generated code pass tests? ("PASS (14/14 tests)")
- `step_4_receipt_production`: Was the receipt created? ("PASS")

**Outputs** (what was produced):
- `A`: Hash of the generated code (e.g., "hash(0x1a2b3c...)")
- `enforcement_latency`: How long until the control was enforced ("< 2 minutes")
- `deployment_latency`: How long until production deployment ("< 30 minutes")

**Verification** (proof of correctness):
- `doctrine_invariants_checked`: Which mathematical invariants were verified
  - Example: "Correct-by-Construction: VERIFIED" means code is generated, not hand-written
  - Example: "Idempotence: VERIFIED" means regenerating produces identical results
  - Example: "Provenance: VERIFIED" means hash(A) = hash(μ(O))
- `tps_gates_passed`: Which quality gates passed
  - Example: "Ontology Gate: PASS" means policy passed type checking
  - Example: "Determinism Gate: PASS" means generation is repeatable
  - Example: "Operational Gate: PASS" means generated code boots successfully

**Proof of Job**:
- `job_done`: Human-readable summary ("Policy became operational enforcer in 2 minutes")
- `auditor_verifiable`: Can an auditor independently verify this? (true/false)
- `receipt_hash`: Hash of the entire receipt (for integrity)

### How to Verify a Receipt Independently

**As an auditor**, you can verify a receipt without asking the operations team:

1. **Read the receipt** to get inputs: O, μ version
2. **Download the generator** (μ) at the exact version specified
3. **Run the generator**: `μ(O)` to produce A'
4. **Compare hashes**: Does `hash(A')` equal `hash(A)` from the receipt?
5. **If hashes match**: The receipt is valid; the production system matches the specification
6. **If hashes differ**: The receipt is invalid or production doesn't match specification

**No trust required**: You're not trusting the operations team; you're verifying cryptographic hashes.

---

## 5. Operational Usage: When and How to Run Tests

### When to Run E2E JTBD Tests

**Per-Change Tests** (every policy change):
- JTBD #1, Scenario 1.1: Basic policy → enforcer generation
- JTBD #4, Scenario 4.1: Compliance team ownership
- JTBD #5, Scenario 5.1: Minimal diff verification

**Purpose**: Ensure every change is correct before it reaches production.  
**Timing**: Run during CI/CD pipeline, before merge.  
**Expected Duration**: < 5 minutes.

**Weekly Tests** (full coherence check):
- JTBD #2, Scenario 2.2: Audit trail across the week's changes
- JTBD #3, Scenario 3.2: Graceful degradation under load
- JTBD #5, Scenario 5.2: Rollback capability

**Purpose**: Ensure accumulated changes over the week are still coherent.  
**Timing**: Sunday night, before the next week's work.  
**Expected Duration**: < 30 minutes.

**Quarterly Tests** (long-term stability):
- JTBD #6, Scenario 6.1: Continuous compliance verification
- JTBD #6, Scenario 6.2: Generational improvements (generator upgrades)

**Purpose**: Ensure the system hasn't drifted over months of changes.  
**Timing**: End of quarter, before audit period.  
**Expected Duration**: < 1 hour.

**Audit Tests** (on-demand):
- JTBD #2, Scenario 2.1: Auditor replay from receipt
- JTBD #3, Scenario 3.1: Control failure diagnosis

**Purpose**: Respond to auditor requests or investigate control failures.  
**Timing**: When auditor asks or when control failure is detected.  
**Expected Duration**: < 10 minutes.

### How to Interpret Test Failures

**Type A: Doctrine Violation** (Critical)
```
Symptom: Receipt shows "Doctrine invariant breached"
Example: hash(A) ≠ hash(μ(O)) (provenance broken)
```
**What it means**: The generated code doesn't match what the generator should have produced. This is a **generator defect**, not an operational error.

**Response**:
1. Immediately halt all deployments using this generator version
2. Retrieve all receipts from affected deployments
3. Escalate to engineering team (this is a μ bug, not an O issue)
4. Roll back to previous generator version
5. Re-run generation with previous version to verify correctness

**Do NOT**: Attempt to manually fix the generated code. This will break provenance.

---

**Type B: Promise Violation** (High Priority)
```
Symptom: Receipt shows "Customer job not done"
Example: Time-to-policy > 4 hours (JTBD #1 broken)
```
**What it means**: The system works, but performance is below SLA. The customer's job isn't being completed within the promised timeframe.

**Response**:
1. Check receipt for timing breakdown (which step is slow?)
2. If `step_2_generation` is slow: investigate generator performance
3. If `step_3_golden_tests` is slow: reduce test suite or parallelize
4. If deployment is slow: investigate deployment pipeline
5. Apply **Kaizen** (continuous improvement) to reduce time

**Do NOT**: Ignore this. Slow time-to-policy defeats the value proposition.

---

**Type C: TPS Gate Failure** (Block Deployment)
```
Symptom: Receipt shows "Gate not passed"
Example: Determinism gate fails (same O → different A)
```
**What it means**: A quality gate failed. The system detected a defect before it reached production.

**Response**:
1. **Ontology Gate failure**: O doesn't pass type checking
   - Fix: Compliance team fixes O specification, resubmit
2. **Determinism Gate failure**: Generation is non-deterministic
   - Fix: Engineering team fixes μ (generator bug)
3. **Invariant Gate failure**: O violates a policy constraint
   - Fix: Compliance team reviews constraint, either fix O or request override
4. **Operational Gate failure**: Generated code fails to boot
   - Fix: Engineering team fixes μ (code generation bug)

**Do NOT**: Override the gate without understanding root cause. Gates exist to prevent production defects.

---

**Type D: Operational Degradation** (Investigate)
```
Symptom: Receipt shows "System behavior degraded"
Example: Graceful degradation test fails (failure cascades)
```
**What it means**: The system didn't fail completely, but fault isolation failed. A single process failure caused other processes to fail.

**Response**:
1. Review supervision structure (are processes properly isolated?)
2. Check logs for cascading failures
3. Escalate to architecture team (supervision design issue)
4. Apply fault injection tests to verify fixes

**Do NOT**: Deploy to production until graceful degradation is restored.

---

### How to Troubleshoot Test Failures

**Step 1: Read the receipt**
- Which step failed? (validation, generation, testing, deployment?)
- What was the error message?
- What were the inputs (O, μ version)?

**Step 2: Reproduce locally**
- Retrieve the exact O and μ version from the receipt
- Run `μ(O)` locally
- Does it reproduce the same error?

**Step 3: Isolate the failure**
- If local reproduction succeeds, the issue is environmental (test environment vs production)
- If local reproduction fails, the issue is in O or μ

**Step 4: Determine ownership**
- O error (invalid specification) → Compliance team
- μ error (generator bug) → Engineering team
- Environment error (deployment issue) → Operations team

**Step 5: Apply fix and re-run**
- Fix the root cause
- Re-run the test
- Verify receipt shows all steps passing

### When to Involve Engineers vs Read Receipts

**Read receipts only** (no engineer needed):
- Verifying that a change was deployed correctly
- Answering auditor questions ("what changed when?")
- Comparing production state to specification
- Checking if a policy is enforced

**Involve engineers**:
- Generator defects (doctrine violations, TPS gate failures)
- Non-determinism (same O produces different A)
- Performance issues (time-to-policy > SLA)
- Graceful degradation failures

**Involve compliance team**:
- Policy specification errors (O doesn't pass ontology gate)
- Policy constraint violations (SOD, approval hierarchy, etc.)
- Policy changes that require override

---

## 6. Auditor Usage: Independent Verification

### How to Verify Daemon Operations Using Receipts

As an auditor, you can verify the system's correctness **without access to production** and **without trusting the operations team**.

**Step 1: Request receipts**
- Ask the operations team for receipts for the audit period
- Example: "Provide all receipts from Q4 2025"

**Step 2: Verify receipt chain**
- Each receipt should reference the previous receipt (O_old)
- Verify the chain is unbroken: R1 → R2 → R3 → ... → RN
- Any missing receipts indicate undocumented changes

**Step 3: Spot-check random receipts**
- Select 5-10 receipts at random
- For each receipt:
  1. Extract O and μ version
  2. Download μ at that version
  3. Run μ(O) to generate A'
  4. Compare hash(A') to hash(A) in receipt
  5. If hashes match: receipt is valid
  6. If hashes differ: flag for investigation

**Step 4: Verify TPS gates**
- Check that all receipts show:
  - Ontology Gate: PASS
  - Determinism Gate: PASS
  - Invariant Gate: PASS
  - Replay Gate: PASS
  - Operational Gate: PASS

**Step 5: Conclude**
- If all receipts are valid and all gates passed: **System is operationally correct**
- If any receipt is invalid: **System integrity is compromised; investigate**

### No Daemon Access Needed

The beauty of receipts is that **you don't need access to the running daemon**. You only need:
1. The receipts (provided by operations)
2. The generator (μ) binary (open source or provided)
3. A clean environment to run μ

You can verify correctness offline, in your own controlled environment, without trusting production infrastructure.

This is **independent verification**: the system proves its own correctness through cryptographic receipts, not through human testimony.

---

## Appendix: Quick Reference

### Success Criteria Summary

| JTBD | Success Metric | Receipt Field |
|------|---------------|---------------|
| #1: Time-to-policy | Generation < 2 min, deployment < 30 min | `outputs.enforcement_latency` |
| #2: Auditability | Hash match on replay | `proof_of_job.auditor_verifiable: true` |
| #3: Defect prevention | Control failures isolated to O or μ | `doctrine_invariants_checked` |
| #4: Compliance ownership | O is source of truth | `inputs.O_old`, `inputs.ΔO` |
| #5: Minimal blast | Only changed processes recompiled | `outputs.changed_artifacts` count |
| #6: Long-term correctness | Regeneration matches production | Hash comparison (receipt vs fresh) |

### Failure Response Matrix

| Failure Type | Severity | Response Owner | Action |
|--------------|----------|----------------|--------|
| Doctrine violation | CRITICAL | Engineering | Rollback, fix μ |
| Promise violation | HIGH | Operations | Optimize pipeline, Kaizen |
| TPS gate failure | BLOCK | Varies by gate | Fix O or μ, no deploy |
| Operational degradation | INVESTIGATE | Architecture | Fix supervision |

### Test Cadence

| Frequency | Tests | Purpose | Duration |
|-----------|-------|---------|----------|
| Per-change | 1.1, 4.1, 5.1 | Prevent defects | < 5 min |
| Weekly | 2.2, 3.2, 5.2 | Coherence check | < 30 min |
| Quarterly | 6.1, 6.2 | Long-term stability | < 1 hour |
| On-demand | 2.1, 3.1 | Audit/investigation | < 10 min |

---

**Document Version**: 1.0  
**Last Updated**: 2026-01-10  
**Maintained By**: UNRDF Documentation Team  
**Questions?**: See `docs/README.md` for support contacts
