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

| Source | Trust | Required Proof |
|--------|-------|---------------|
| "Tests pass" claim | 0% | Full `pnpm test` output with pass count |
| "100% coverage" claim | 0% | `pnpm test:coverage` with metrics |
| "Build works" claim | 0% | `pnpm build` successful completion |
| Actual test output | 90% | Ran + read full output |
| OTEL spans | 95% | External truth source |

## Verify Before Commit

```bash
timeout 5s pnpm test:fast    # Pre-push tests
timeout 30s pnpm lint        # Linting check
timeout 60s pnpm build       # Build verification
```
