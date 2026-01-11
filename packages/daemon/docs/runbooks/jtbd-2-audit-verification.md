# JTBD #2 Runbook: Prove What Changed to Auditor

## Objective
Produce independently verifiable audit artifacts that prove a policy change was deployed correctly, without requiring human testimony.

## Prerequisites
- Production deployment receipt (from JTBD #1 step 9)
- Clean Erlang runtime environment (for auditor replay)
- ggen binary (version specified in receipt)
- Access to ontology repository (O_old, O_new)
- Auditor credentials (read-only)

## Steps

1. **Retrieve Deployment Receipt**
   - Locate: Production receipt from audit log
   - Extract: `{inputs: O_new_hash, rules: ggen_version, outputs: A_hash, timestamp}`
   - Verify: Receipt signature is valid

2. **Fetch Ontology Snapshots**
   - Download: O_old and O_new from ontology repository
   - Verify: `hash(O_new) == O_new_hash` from receipt
   - Document: What changed between O_old and O_new (ΔO)

3. **Replay Generation (Auditor Side)**
   - Run: `μ_auditor generate --input=O_new --ggen-version={from_receipt}`
   - Important: Use EXACT ggen version from receipt
   - Output: A_auditor (auditor's regenerated enforcer)

4. **Verify Integrity**
   - Compare: `hash(A_auditor) vs hash(A_from_receipt)`
   - Expected: Hashes match (bitwise identical)
   - Result: If match → "Production was generated correctly from O_new"

5. **Produce Auditor Receipt**
   - Generate: Auditor's own receipt from replay
   - Include: `{O_new_hash, ggen_version, A_auditor_hash, auditor_id, timestamp}`
   - Sign: With auditor's private key

6. **Compare Receipts**
   - Verify: Production receipt hash == Auditor receipt hash
   - Check: Both receipts agree on: O_hash, A_hash, ggen_version
   - Document: "Independent verification confirms correctness"

7. **Build Audit Trail (Multi-Change)**
   - Collect: All receipts R_t0→t1, R_t1→t2, ..., R_tn-1→tn
   - Compute: diff(O_t0, O_tn) to show accumulated changes
   - Verify: Each transition is valid (O_ti ⊨ Σ at each step)

8. **Answer Audit Questions**
   - Query: "What controls changed between t0 and t2?"
   - Answer: Read receipts, extract ΔO from each step
   - Proof: Chain of receipts proves all changes are documented

## Success Criteria
- [ ] Auditor can replay generation using only receipt + public inputs
- [ ] hash(A_auditor) == hash(A_production) (integrity verified)
- [ ] Zero human testimony required for verification
- [ ] Receipts form complete chain (no unexplained transitions)
- [ ] Doctrine invariant verified: Provenance (hash(A) = hash(μ(O)))
- [ ] Audit cost: <1 hour per policy change (read receipts, not interview personnel)

## Troubleshooting

### If auditor replay fails (A_auditor ≠ A_production)
- **Symptom**: Hashes do not match after regeneration
- **Action**: Check ggen version mismatch: auditor may be using wrong μ version
- **Fix**: Ensure auditor uses EXACT ggen_version from receipt. If versions match but hashes differ → CRITICAL BUG (non-deterministic generation)

### If receipt is incomplete
- **Symptom**: Receipt missing inputs (O_hash), rules (ggen_version), or outputs (A_hash)
- **Action**: This violates TPS Replay gate
- **Fix**: Regenerate receipt from production logs. If logs unavailable → CANNOT VERIFY (audit failure)

### If ontology snapshots unavailable
- **Symptom**: Cannot fetch O_t0 or O_tn from repository
- **Action**: Check ontology repository retention policy
- **Fix**: Restore from backup or immutable ledger. If unavailable → audit trail is broken (compliance violation)

### If audit trail has gaps
- **Symptom**: Missing receipts between t0 and tn (e.g., R_t1→t2 missing)
- **Action**: This indicates undocumented changes
- **Fix**: Search deployment logs for missing receipts. If not found → AUDIT DEFECT (unexplained transition)

### If receipt signature invalid
- **Symptom**: Receipt fails cryptographic signature verification
- **Action**: Possible tampering or corrupted storage
- **Fix**: DO NOT trust receipt. Investigate source of corruption. Re-audit from original deployment logs

## References
- Test scenario: JTBD 2.1 (Auditor Replay from Receipt)
- Test scenario: JTBD 2.2 (Change Delta Audit Trail)
- Doctrine invariant: Provenance, Idempotence
- TPS gates: Replay
- Related runbooks: JTBD #1 (Deploy Control), JTBD #3 (Failure Diagnosis)
- Full test specification: `/home/user/unrdf/docs/finops-fabric-e2e-jtbd-tests.md` lines 83-142
