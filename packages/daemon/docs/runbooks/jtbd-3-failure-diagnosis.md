# JTBD #3 Runbook: Diagnose Control Failure Without Operational Blame

## Objective
When a control fails in production (e.g., approver rule incorrectly allowed/blocked transaction), prove whether failure is in generated code (μ bug) or operational environment (runtime issue).

## Prerequisites
- Production receipt for current enforcer
- Access to production logs (transaction that triggered failure)
- Clean test environment (for replay)
- Escalation path to μ engineering team

## Steps

1. **Capture Failure Details**
   - Record: Transaction details (amount, actor, timestamp)
   - Record: Expected outcome vs actual outcome
   - Example: "$105k transaction + L2 actor → APPROVED (should be REJECTED)"

2. **Retrieve Current Enforcer Receipt**
   - Locate: Receipt R for approval enforcer currently in production
   - Extract: `{O_hash, A_hash, ggen_version, deployment_timestamp}`
   - Verify: Receipt is for current production version (not stale)

3. **Replay Generation in Clean Environment**
   - Run: `μ generate --input=O_production --ggen-version={from_receipt}`
   - Output: A_replay
   - Compare: `hash(A_replay) == hash(A_production)`
   - Expected: Hashes match (proves production enforcer is correct artifact from O)

4. **Run Golden Tests Against Replayed Enforcer**
   - Execute: `μ test --enforcer=A_replay --scenario={failing_transaction}`
   - Input: Exact transaction that failed in production
   - Expected: If enforcer is correct, test should FAIL same way production did

5. **Analyze Golden Test Results**
   - **If test PASSES (enforcer blocks L2 correctly)**: Failure is NOT in enforcer code
     - Conclusion: Runtime environment issue or O changed since deployment
     - Next: Check if O was updated after deployment (O_hash mismatch)
   - **If test FAILS (enforcer allows L2 incorrectly)**: Failure IS in enforcer code
     - Conclusion: μ generated incorrect enforcer from O (CRITICAL BUG)
     - Next: Escalate to μ engineering

6. **Check for Configuration Drift**
   - Verify: Current production O matches O_hash from receipt
   - Run: `hash(O_current) == O_hash_from_receipt`
   - If mismatch: O was changed without redeployment (configuration drift)

7. **Escalate to Correct Team**
   - **If μ bug**: File P0 incident against generator team
     - Attach: Receipt, failing transaction, golden test results
     - DO NOT blame operations
   - **If configuration drift**: File incident against deployment team
     - Question: Why was O updated without triggering redeployment?
   - **If runtime issue**: File incident against infrastructure team
     - Examples: BEAM version mismatch, dependency corruption

8. **Document Root Cause**
   - Update incident: "Failure is in [generation|configuration|runtime], NOT operations"
   - Proof: Golden test results + receipt replay
   - Outcome: Operations team is cleared (no blame)

## Success Criteria
- [ ] Root cause identified in <30 minutes
- [ ] Operations team does NOT spend time debugging control logic
- [ ] Blame assigned to: μ (generator), O (configuration), or runtime (infrastructure)
- [ ] Doctrine invariant verified: Fault Containment (defect is upstream, not operational)
- [ ] Next receipt incorporates fix (updated O or μ version)
- [ ] Zero manual code review of enforcer logic

## Troubleshooting

### If replay does not match production (hash mismatch)
- **Symptom**: A_replay ≠ A_production (different hashes)
- **Action**: This indicates production artifact does NOT match receipt
- **Fix**: CRITICAL SECURITY ISSUE. Investigate how production was modified outside deployment process. Redeploy from receipt immediately

### If golden tests are inconclusive
- **Symptom**: Cannot reproduce failure in clean environment
- **Action**: May indicate transient runtime issue (e.g., race condition, network timeout)
- **Fix**: Check production logs for runtime errors. Add monitoring for transient failures

### If O changed since deployment (configuration drift)
- **Symptom**: O_current_hash ≠ O_hash_from_receipt
- **Action**: Someone updated O without triggering μ regeneration
- **Fix**: Enforce policy: O changes MUST trigger automated regeneration + deployment. This is a process violation

### If escalation is unclear (multiple possible causes)
- **Symptom**: Golden test passes, O matches, but production still fails
- **Action**: Check runtime environment: BEAM version, dependency versions, system resources
- **Fix**: May require A/B test: deploy enforcer to canary environment, compare behavior

### If μ engineering disputes bug report
- **Symptom**: μ team claims enforcer is "correct" despite failure
- **Action**: Provide EXACT inputs (O, ggen_version), receipt, and golden test showing failure
- **Fix**: If μ team cannot reproduce, possible environment-specific bug. Capture full runtime state (logs, env vars, BEAM dump)

## References
- Test scenario: JTBD 3.1 (Control Failure Diagnosis)
- Test scenario: JTBD 3.2 (Graceful Degradation Under Supervisor Failure)
- Doctrine invariant: Fault Containment
- TPS gates: Operational
- Related runbooks: JTBD #2 (Audit Verification), JTBD #5 (Rollback)
- Full test specification: `/home/user/unrdf/docs/finops-fabric-e2e-jtbd-tests.md` lines 145-204
