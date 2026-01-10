# JTBD #6 Runbook: Ensure System Remains Correct Over Time

## Objective
Verify that accumulated policy changes remain coherent and that the system does not degrade due to entropy, technical debt, or forgotten constraints (quarterly verification + generator upgrades).

## Prerequisites
- Production system running for ≥3 months
- All historical receipts (R_t0, R_t1, ..., R_tn)
- Current O_tn (merged state of all changes)
- ggen version history
- Clean test environment

## Steps

1. **Collect Historical Receipts**
   - Retrieve: All receipts from audit log (R_t0→t1, R_t1→t2, ..., R_tn-1→tn)
   - Count: Number of policy changes since initial deployment
   - Example: "12 policy changes over 6 months"

2. **Compute Merged Ontology State**
   - Extract: Current production ontology O_tn
   - Verify: O_tn includes all accumulated changes (not just latest ΔO)
   - Document: Full policy state as of time tn

3. **Validate Merged Ontology**
   - Run: `μ validate --ontology=O_tn --constraints=Σ`
   - Verify: O_tn ⊨ Σ (full ontology type check)
   - Expected: All accumulated changes still satisfy compliance constraints

4. **Regenerate from Scratch**
   - Run: `μ generate --input=O_tn --output=A_fresh`
   - Output: A_fresh (as if deploying from scratch with current O_tn)
   - Purpose: Verify idempotence (μ∘μ = μ)

5. **Compare Fresh vs Production**
   - Compare: `hash(A_fresh) vs hash(A_production)`
   - **If hashes match**: System is operationally consistent (no drift)
   - **If hashes differ**: Receipt explains exact differences (investigate drift)

6. **Document Drift (if any)**
   - Run: `μ diff --old=A_production --new=A_fresh --verbose`
   - List: Processes with hash differences
   - Investigate: Why drift occurred? Examples: manual edits, environment changes

7. **Verify No Technical Debt**
   - Check: Zero manual edits to A (all changes generated from O)
   - Check: All past changes documented in receipts (no "unexplained" code)
   - Expected: Every line of A is traceable to O via receipts

8. **Test Generator Upgrade (Kaizen)**
   - Identify: New ggen version (e.g., v1.1 with cleaner rules)
   - Run: `μ_v1.1 generate --input=O_tn --output=A_v11`
   - Run golden tests: `μ_v1.1 test --enforcer=A_v11 --golden-suite`
   - Verify: Policy behavior unchanged (only internal improvements)

9. **Generate Kaizen Receipt**
   - Document: `{old_ggen: v1.0, new_ggen: v1.1, O_unchanged: true, behavior_equivalent: true}`
   - Proof: Golden tests prove policy equivalence
   - Deploy: A_v11 with confidence (zero customer impact)

## Success Criteria
- [ ] Merged ontology O_tn passes validation (O_tn ⊨ Σ)
- [ ] hash(A_fresh) == hash(A_production) OR drift is explained in receipt
- [ ] Zero technical debt (all changes documented in receipts)
- [ ] Generator upgrade preserves policy behavior (golden tests pass)
- [ ] Doctrine invariant verified: Idempotence (μ∘μ = μ)
- [ ] System quality increases over time (via generator improvements), not degrades
- [ ] Compliance team confirms: "System is what we expect, no hidden changes"

## Troubleshooting

### If O_tn validation fails
- **Symptom**: `μ validate` reports constraint violations in merged state
- **Action**: Accumulated changes violate compliance constraints
- **Fix**: Identify which ΔO introduced violation. May require retroactive policy fix. This indicates gate failure (constraint should have been caught at time of change)

### If drift detected (A_fresh ≠ A_production)
- **Symptom**: Fresh generation differs from production
- **Action**: Investigate cause: manual edits? environment changes? ggen version mismatch?
- **Fix**: If manual edits found → VIOLATION (revert edits, regenerate from O). If environment → document in receipt. If ggen mismatch → redeploy with correct version

### If technical debt found (unexplained code)
- **Symptom**: Code in A_production not traceable to O via receipts
- **Action**: AUDIT DEFECT. This code is undocumented
- **Fix**: Archaeology required: git blame, deployment logs. Document findings. Add missing receipts. If cannot explain → may need to regenerate entire A from O

### If generator upgrade changes policy behavior
- **Symptom**: Golden tests FAIL with new ggen version
- **Action**: ggen v1.1 is NOT behavior-equivalent to v1.0
- **Fix**: DO NOT deploy. This is NOT Kaizen (continuous improvement). This is a breaking change. Requires compliance team review (treat as new ΔO)

### If compliance team finds unexpected behavior
- **Symptom**: "We didn't approve this control behavior"
- **Action**: Possible drift or misunderstood requirement
- **Fix**: Compare current O_tn to compliance team's expectations. If mismatch → ΔO was misinterpreted. Requires new policy change to align with intent

### If system performance degrades over time
- **Symptom**: Latency increases, memory usage grows despite no policy changes
- **Action**: May indicate accumulating inefficiency in generated code
- **Fix**: Profile A_production. If inefficiency found → upgrade ggen with performance improvements (Kaizen). If external cause (data growth) → scale infrastructure

## References
- Test scenario: JTBD 6.1 (Continuous Compliance Verification)
- Test scenario: JTBD 6.2 (Generational Improvements / Kaizen)
- Doctrine invariant: Idempotence, Minimality
- TPS gates: Determinism, Kaizen
- Related runbooks: JTBD #2 (Audit Verification), JTBD #5 (Minimal Blast Radius)
- Full test specification: `/home/user/unrdf/docs/finops-fabric-e2e-jtbd-tests.md` lines 329-386
