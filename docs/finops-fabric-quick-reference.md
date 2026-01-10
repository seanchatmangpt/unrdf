# FinOps Fabric: Quick Reference Cheat Sheet

## The Equation
```
A = μ(O)

A = Erlang platform (executable artifacts)
μ = ggen (generator + validators + compilers)
O = FIBO ontology + policies + constraints
```

---

## Doctrine (7 Non-Negotiable Invariants)

| # | Invariant | Means | Consequence |
|---|-----------|-------|-------------|
| 1 | **Correct-by-Construction** | Code generated, never hand-written | No code review needed; defects are in O, not A |
| 2 | **Idempotence** | μ∘μ = μ; same O → same A always | Deterministic; reproducible forever |
| 3 | **Typing** | All artifacts typed; all messages enforced | Type violations impossible |
| 4 | **Provenance** | hash(A) = hash(μ(O)); full traceability | Auditor can verify without humans |
| 5 | **No Partials** | O valid → A valid, or neither | No half-deployed state; Andon catches issues |
| 6 | **Fault Containment** | Process boundaries isolate failures | System degrades, never crashes |
| 7 | **Minimality** | argmin drift(A); only necessary changes | Risk proportional to change size |

**Quick check**: If any invariant is violated, the system is broken.

---

## Market Promise (4 Customer Jobs)

| Job | What It Means | How We Deliver |
|-----|---------------|----------------|
| **Time-to-Policy** | Deploy new rule in hours, not quarters | No code review; μ generates + tests in <5m; next epoch deploys |
| **Auditability** | Know exactly what changed and why | Receipt proves hash(A) = hash(μ(O)); auditor can replay independently |
| **Defect Prevention** | Controls are correct by construction | Type system + generation make defects impossible; catch at O time, not runtime |
| **Compliance Ownership** | Compliance writes rules, not engineers | O is spec; engineers run μ(O); code is generated, not hand-written |

**Quick check**: If any promise is broken, a doctrine invariant or TPS gate has failed.

---

## TPS Production System (7 Hard Gates)

| Gate | Checks | Blocks | Triggers |
|------|--------|--------|----------|
| **Ontology** | O ⊨ Σ (types, required predicates) | Generation if invalid O | Every ΔO |
| **Determinism** | Same O → identical A (byte-stable) | Deploy if non-deterministic | Post-generation (compare hashes) |
| **Invariant** | Policy constraints (SOD, retention, etc.) | Generation if violation | Every ΔO |
| **Replay** | Receipt can reproduce A in clean env | Deploy if unreplayable | Pre-deployment (auditor test) |
| **Operational** | Processes boot + health checks pass | Deploy if startup fails | Pre-deployment |
| **Jidoka** | Line stops on any gate failure | Entire release if gate fails | On gate failure (Andon) |
| **Waste Elimination** | No unintended recompilation; min blast | Over-generation if detected | Per artifact comparison |

**Quick check**: If any gate fails, generation or deployment stops; receipt only.

---

## JTBD Test Checklist

### Before Every Change (ΔO)
- [ ] JTBD #1: Time-to-policy < 4 hours? (policy → production)
- [ ] JTBD #4: Compliance owned the O change? (PR approved by compliance)
- [ ] JTBD #5: Minimal diff? (only changed artifacts recompiled)

### Per Week (Coherence)
- [ ] JTBD #2: Audit trail is clean? (all changes have receipts)
- [ ] JTBD #3: Graceful degradation works? (1 process failure ≠ system failure)
- [ ] JTBD #5: Rollback is fast? (< 30 min from receipt)

### Per Quarter (Long-Term)
- [ ] JTBD #6: Compliance verification passes? (O_accumulated regenerates to A_production)
- [ ] JTBD #6: Generator improvements applied? (Kaizen: update μ version safely)

### On Audit (Proof)
- [ ] JTBD #2: Auditor can replay? (receipt + ggen binary = verify A integrity)
- [ ] JTBD #3: Control failure diagnosis clear? (blame is on O or μ, never ops)

---

## Receipt Structure (Every Change)

```json
{
  "timestamp": "2025-01-10T14:30:00Z",
  "inputs": {
    "O_hash": "0x...",
    "ΔO_hash": "0x...",
    "ggen_version": "1.0.1"
  },
  "generation": {
    "duration_ms": 3000,
    "status": "SUCCESS"
  },
  "outputs": {
    "A_hash": "0x...",
    "artifacts": ["process_a", "process_b"],
    "golden_tests_passed": 14
  },
  "doctrine_invariants": {
    "correct_by_construction": "✓",
    "idempotence": "✓",
    "typing": "✓",
    "provenance": "✓",
    "no_partials": "✓",
    "fault_containment": "✓",
    "minimality": "✓"
  },
  "tps_gates": {
    "ontology": "PASS",
    "determinism": "PASS",
    "invariant": "PASS",
    "replay": "PASS",
    "operational": "PASS",
    "waste_elimination": "PASS"
  },
  "deployment": {
    "approved_for_epoch": true,
    "epoch_window": "next_daily"
  }
}
```

**Quick check**: All doctrine ✓ and all gates PASS → ready to deploy.

---

## Failure Diagnosis Tree

```
System behavior unexpected?
├─ Receipt shows violation
│  ├─ Doctrine invariant broken
│  │  └─ → Generator (μ) defect; investigate ggen source
│  └─ TPS gate failed
│     ├─ Ontology gate → O is invalid; compliance team fixes
│     ├─ Determinism gate → μ is non-deterministic; engineer investigates
│     ├─ Invariant gate → O violates policy; compliance team overrides
│     ├─ Replay gate → Receipt integrity compromised; audit issue
│     └─ Operational gate → Generated A fails to boot; generate defect
└─ Receipt is normal
   ├─ O itself is wrong
   │  └─ → Compliance team fixes O; resubmit ΔO
   ├─ Test environment doesn't match production
   │  └─ → Environmental issue; not a system defect
   └─ This is not a defect; behavior is as designed per receipt
```

---

## Doctrine → TPS → JTBD Mapping

| Doctrine | TPS Gate | JTBD | Customer Benefit |
|----------|----------|------|------------------|
| Correct-by-Construction | Ontology | #1, #4 | Fast deployment, compliance owns rules |
| Idempotence | Determinism | #6 | Durable over time, Kaizen-safe |
| Typing | Ontology | #3, #4 | Defect prevention |
| Provenance | Replay | #2 | Auditability |
| No Partials | Jidoka | #3 | Reliable ops |
| Fault Containment | Operational | #3 | Graceful degradation |
| Minimality | Waste Elimination | #5 | Minimal blast radius |

---

## Cadence

### Daily
- [ ] Next epoch ready? (ΔO reviewed, all gates pass)

### Per Change
- [ ] Receipt produced? (all doctrine checks ✓)
- [ ] All TPS gates passed?
- [ ] Approved for deployment?

### Weekly
- [ ] Audit trail complete? (all changes have receipts)
- [ ] Control failures detected? (if yes, diagnose)

### Monthly
- [ ] Kaizen applied? (generator or O improved)
- [ ] Compliance verification run? (O regenerates to A_production)

### Quarterly
- [ ] Long-term durability verified? (JTBD #6 tests pass)
- [ ] Audit conducted? (independent verification of receipts)

---

## Vocabulary

| Term | Means |
|------|-------|
| **O** | Ontology (FIBO + local policies + constraints) |
| **ΔO** | Change to ontology (what compliance team submits) |
| **μ** | Generator (ggen binary; transforms O → A) |
| **A** | Artifacts (Erlang platform; processes, modules, etc.) |
| **Receipt** | Proof of generation (inputs, rules, outputs, hashes) |
| **Epoch** | Deployment window (e.g., daily, weekly) |
| **Andon** | Alert signal; TPS line stops on gate failure |
| **Kaizen** | Continuous improvement (usually in μ rules or O simplification) |
| **Genchi Genbutsu** | Go to the source (inspect receipts, replay exact μ(O)) |
| **Hash** | Cryptographic fingerprint (proves artifact integrity) |
| **Replay** | Regenerate exact A from receipt + O; verify hash match |

---

## Gold Standard

**The system is operationally correct when**:

1. ✓ All 7 doctrine invariants verified by receipts
2. ✓ All 7 TPS gates passed in production logs
3. ✓ All 6 JTBDs measurably delivered
4. ✓ Auditor independent verification successful
5. ✓ No manual exceptions to process
6. ✓ Compliance team owns O; engineers run μ

**If all 6 are true, go to production. Otherwise, find what's broken and fix it.**

---

## One-Liner

**FinOps Fabric** = ontology-compiled Erlang platform where **A = μ(O)**, all invariants are verifiable via receipts, all TPS gates are automated, and compliance team owns the control logic, not engineers.

```
Faster: no code review (code is generated)
Safer: correct-by-construction (O errors caught pre-generation)
Auditable: receipt proves integrity (auditor can verify independently)
Operationally sound: TPS gates prevent all defects from shipping
```

---

## Next Actions

1. **To deploy a change**: Write ΔO, compliance approves, submit to epoch queue
2. **To audit**: Request receipt + ggen binary version, run replay verification
3. **To troubleshoot**: Read receipt, follow diagnosis tree, find which artifact is wrong
4. **To improve**: Kaizen: simplify O or improve μ rules (never hand-edit A)
5. **To verify system health**: Run JTBD #6 monthly; regenerate O_accumulated and compare hash

---

**Remember**: A = μ(O). If A is wrong, either O is wrong or μ is wrong. Never both, and never neither. Find which, fix it, regenerate.
