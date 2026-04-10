# Verification Standard — UNRDF

No claim is complete without proof. Verify by running, not by reading.

## Evidence Checklist

Before declaring any work complete:

- [ ] **Did I RUN the code?** (not just read it)
- [ ] **Can I PROVE it works?** (show output, not "should work")
- [ ] **What BREAKS if I'm wrong?** (specific failure modes)
- [ ] **Did I read FULL output?** (not stop at first pass)
- [ ] **Are claims VERIFIABLE?** (use authoritative sources: package.json, actual CLI output)

## Trust Levels

| Source                | Trust | Required Proof                          |
| --------------------- | ----- | --------------------------------------- |
| "Tests pass" claim    | 0%    | Full `pnpm test` output with pass count |
| "100% coverage" claim | 0%    | `pnpm test:coverage` with metrics       |
| "Build works" claim   | 0%    | `pnpm build` successful completion      |
| Actual test output    | 90%   | Ran + read full output                  |
| OTEL spans            | 95%   | External truth source                   |

## Process Mining Constitution (Van der Aalst Verification)

For process-aware systems (CodeManufactory, OSA, BusinessOS workflows):

**Doctrine:** If the code says it worked but the event log cannot prove a lawful process happened, then it did not work.

### Event-Log Falsification

OTel traces are candidate event evidence, not proof. Must pass:

- [ ] **Log-to-model replay** — Observed events replay against intended process model
- [ ] **Process discovery audit** — Discovered process vs declared process alignment
- [ ] **Object-centric conformance** — OCEL validation across artifact/receipt/proof/release objects
- [ ] **Temporal lawfulness** — Stage ordering, no impossible overlaps, proof gate sequencing
- [ ] **Lifecycle completeness** — Every object has lawful lifecycle history
- [ ] **Variant analysis** — No hidden loops, rework storms, or undocumented branches
- [ ] **Fitness/precision** — Model is tight enough to reject fraud but general enough for lawful variance

### Required Tools

- **OTel** — Runtime event emissions
- **OCEL** — Object-centric event log structure
- **pm4py** — Process discovery, conformance, replay, diagnostics

### Fail Conditions

System fails if event log reveals:

- Skipped stages (release before validate)
- Impossible ordering (validate before breed)
- Duplicate terminal states (same artifact both failed and released)
- Orphan objects (receipt for non-existent artifact)
- Causal inconsistencies (proof gate pass without causal predecessor)
- Unexplained variants (deterministic system showing variant explosion)

### Negative Testing

Generate impossible event logs and verify detection:

- Release before validate
- Validate before breed
- Concurrent terminal states for one artifact
- Receipt for non-existent artifact
- Benchmark for never-compiled object
- Proof gate pass with missing predecessor
- Orphan MCP tool invocation

## Verify Before Commit

```bash
timeout 5s pnpm test:fast    # Pre-push tests
timeout 30s pnpm lint        # Linting check
timeout 60s pnpm build       # Build verification
```
