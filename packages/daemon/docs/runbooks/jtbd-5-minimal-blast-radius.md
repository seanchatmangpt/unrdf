# JTBD #5 Runbook: Minimize Blast Radius of Changes

## Objective
Verify that policy changes produce minimal code changes (only affected components recompiled), reducing risk of unintended side effects and enabling fast rollback.

## Prerequisites
- O_old (pre-change ontology)
- O_new (post-change ontology with ΔO applied)
- A_old (current production enforcer)
- Clean environment for generation
- Deployment plan access

## Steps

1. **Compute Ontology Diff**
   - Run: `diff O_old O_new` or `git diff O_old O_new`
   - Document: Which rules changed (e.g., "ApprovalRule threshold updated")
   - Count: Number of rules changed vs total rules

2. **Generate New Enforcer**
   - Run: `μ generate --input=O_new --output=A_new`
   - Wait: Generation completes (<2 minutes)
   - Output: A_new (new enforcer artifact)

3. **Compute Enforcer Diff**
   - Run: `μ diff --old=A_old --new=A_new --verbose`
   - Output: List of changed processes/modules
   - Example: "Only approval_supervisor recompiled, 14 other processes unchanged"

4. **Verify Minimality**
   - Check: Number of changed processes ≤ number of changed rules in ΔO
   - Expected: If 1 rule changed → only 1 process changed
   - If more processes changed: Investigate why (may indicate coupling)

5. **Verify Unchanged Artifacts**
   - Run: `μ verify-hashes --old=A_old --new=A_new`
   - Output: List of processes with identical hashes
   - Expected: Unchanged processes are byte-identical (no unnecessary recompilation)

6. **Plan Minimal Deployment**
   - Generate: Deployment plan showing which processes to restart
   - Example: "Restart 1 process (approval_supervisor), no restart for 14 others"
   - Document: Blast radius = number of restarted processes

7. **Execute Minimal Deployment**
   - Deploy: Only changed processes to production
   - Verify: Unchanged processes continue running (no disruption)
   - Monitor: Only affected workflows experience brief restart

8. **Verify Rollback Capability**
   - Test: Can rollback to O_old deterministically?
   - Run: `μ generate --input=O_old --output=A_rollback`
   - Verify: `hash(A_rollback) == hash(A_old)` (exact restoration)

9. **Document Blast Radius**
   - Generate: Change report showing minimality
   - Include: "Changed 1 of 15 rules → recompiled 1 of 15 processes (6.7% blast radius)"
   - Proof: Receipts show unchanged processes have identical hashes

## Success Criteria
- [ ] Only affected processes recompiled (no unnecessary changes)
- [ ] Unchanged processes byte-identical to A_old (zero recompilation waste)
- [ ] Blast radius ≤ 10% for typical single-rule change
- [ ] Deployment plan shows minimal restarts (only changed processes)
- [ ] Rollback is deterministic (O_old → A_old exactly)
- [ ] Doctrine invariant verified: Minimality (argmin drift(A))
- [ ] TPS gate verified: Waste Elimination

## Troubleshooting

### If all processes recompiled (blast radius = 100%)
- **Symptom**: μ diff shows all processes changed, even for 1-rule change
- **Action**: This violates Minimality principle
- **Fix**: Investigate μ generator: why are unchanged rules recompiled? May indicate missing incremental build logic or global coupling

### If unchanged processes have different hashes
- **Symptom**: Processes not affected by ΔO have different hashes in A_new
- **Action**: Non-deterministic generation or unnecessary recompilation
- **Fix**: Check for: timestamps in generated code, random IDs, dependency changes. Should be zero hash changes for unchanged logic

### If rollback does not restore exact state
- **Symptom**: A_rollback ≠ A_old (different hashes)
- **Action**: Idempotence violation (μ∘μ ≠ μ)
- **Fix**: CRITICAL BUG. Rollback is not deterministic. Investigate: ggen version mismatch? O_old corrupted? If cannot restore exactly → CANNOT ROLLBACK SAFELY

### If blast radius > expected (e.g., 50% for 1-rule change)
- **Symptom**: More processes changed than rules changed
- **Action**: Indicates tight coupling between rules
- **Fix**: Review O for unnecessary dependencies. Consider refactoring rules to reduce coupling. Document coupling in explanation for future changes

### If deployment plan requires full restart
- **Symptom**: Deployment tooling cannot do partial restart (all-or-nothing)
- **Action**: Deployment infrastructure limitation
- **Fix**: Upgrade deployment tooling to support partial process restarts. Until fixed, blast radius benefits are limited to code changes (not runtime restarts)

### If rollback takes >5 minutes
- **Symptom**: Rollback requires long generation or deployment time
- **Action**: Rollback is too slow for incident response
- **Fix**: Pre-generate rollback artifacts during deployment. Store A_old as "known-good snapshot" for instant restore

## References
- Test scenario: JTBD 5.1 (Minimal Diff Verification)
- Test scenario: JTBD 5.2 (Rollback Capability)
- Doctrine invariant: Minimality, Idempotence
- TPS gates: Waste Elimination
- Related runbooks: JTBD #1 (Deploy Control), JTBD #3 (Failure Diagnosis)
- Full test specification: `/home/user/unrdf/docs/finops-fabric-e2e-jtbd-tests.md` lines 268-326
