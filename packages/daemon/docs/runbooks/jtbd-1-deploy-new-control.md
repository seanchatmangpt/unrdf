# JTBD #1 Runbook: Deploy a New Control in Hours, Not Quarters

## Objective
Deploy a financial constraint as operational enforcement (e.g., "approver must be L3+ for >$100k spends") from policy definition to production in hours without manual code writing.

## Prerequisites
- Ontology repository access (O with read/write)
- μ (ggen) binary available
- Test environment credentials
- Production deployment window scheduled (30-min epoch)
- Compliance team approval for policy change

## Steps

1. **Submit Policy Change (ΔO)**
   - Create policy definition in ontology format
   - Example: `ApprovalRule { amount_threshold: 100000, required_authority_level: "L3+" }`
   - Submit PR with compliance team as reviewer

2. **Validate Ontology**
   - Run: `μ validate --ontology=O_new --schema=Σ`
   - Verify: "O ⊨ Σ" check passes (type check + required predicates)
   - If validation fails, fix ΔO and retry

3. **Generate Enforcer**
   - Run: `μ generate --input=O_new --output=A_new`
   - Monitor: Generation should complete <2 minutes
   - Verify: Receipt produced with hash(O_new) → hash(A_new)

4. **Run Golden Tests**
   - Execute: `μ test --enforcer=A_new --golden-suite`
   - Verify: All test scenarios pass (e.g., $105k + L2 → REJECT, $105k + L3+ → APPROVE)
   - Review: Test results in receipt

5. **Deploy to Test Environment**
   - Run: `μ deploy --target=test --enforcer=A_new`
   - Wait: Enforcer boots (should be available <3 minutes)
   - Verify: Health check returns "healthy"

6. **Verify Determinism**
   - Run: `μ generate --input=O_new --output=A_verify`
   - Compare: `hash(A_new) == hash(A_verify)` (byte-identical)
   - This proves deterministic generation

7. **Schedule Production Deployment**
   - Submit: Deployment request for next epoch window
   - Attach: Receipt from step 3
   - Confirm: Deployment window <30 minutes from now

8. **Monitor Production Rollout**
   - Watch: Supervisor starts new enforcer process
   - Verify: No errors in deployment logs
   - Confirm: First transaction enforced correctly

9. **Generate Audit Receipt**
   - Run: `μ receipt --deployment=production --timestamp=$(date -Iseconds)`
   - Store: Receipt in audit log (for JTBD #2 compliance)
   - Verify: Receipt includes {O_hash, A_hash, ggen_version, deployment_timestamp}

## Success Criteria
- [ ] Policy definition → enforcer generation: <2 minutes
- [ ] Enforcer available in test: <3 minutes from generation
- [ ] Enforcer available in production: <30 minutes (next epoch)
- [ ] Receipt proves: hash(A) = hash(μ(O)) deterministically
- [ ] Golden tests: 100% pass rate
- [ ] No human code edits required
- [ ] Doctrine invariant verified: Correct-by-Construction

## Troubleshooting

### If ontology validation fails (O ⊭ Σ)
- **Symptom**: `μ validate` returns errors
- **Action**: Review missing predicates or type mismatches in ΔO
- **Fix**: Update ΔO with required fields, resubmit for compliance approval

### If generation time >2 minutes
- **Symptom**: Generation hangs or exceeds timeout
- **Action**: Check μ process logs for blocking gates
- **Fix**: May indicate complex rule interactions; investigate golden tests for edge cases

### If golden tests fail
- **Symptom**: Test assertions fail (e.g., L2 approved when should reject)
- **Action**: This is a CRITICAL defect in μ generator logic
- **Fix**: DO NOT deploy. Rollback to O_old. File bug against μ. This violates Doctrine (Correct-by-Construction)

### If deployment fails in production
- **Symptom**: Supervisor cannot boot enforcer, health check fails
- **Action**: Verify receipt matches deployed artifact: `hash(A_production) == hash(A_from_receipt)`
- **Fix**: If hashes differ, redeploy from receipt. If hashes match, check runtime environment (BEAM version, dependencies)

### If non-deterministic generation (A_new ≠ A_verify)
- **Symptom**: Same O produces different A on re-generation
- **Action**: CRITICAL BUG. This violates TPS Determinism gate
- **Fix**: DO NOT deploy. File P0 incident against μ. This breaks audit replayability

## References
- Test scenario: JTBD 1.1 (Simple Policy → Compiled Enforcer)
- Test scenario: JTBD 1.2 (Cascading Control → Multi-Process Coordination)
- Doctrine invariant: Correct-by-Construction
- TPS gates: Determinism, Ontology
- Related runbooks: JTBD #2 (Audit Trail), JTBD #4 (Compliance Ownership)
- Full test specification: `/home/user/unrdf/docs/finops-fabric-e2e-jtbd-tests.md` lines 16-81
