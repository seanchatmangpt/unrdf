# UNRDF WIP Quarantine Archive

This directory contains stale benchmarks, proof-of-concept experiments, and test artifacts that have been quarantined from active production packages.

## Contents

- **benchmarks/** — Performance testing suites for atomvm, daemon, yawl packages
- **poc/** — Proof-of-concept implementations (POC files, not part of production)
- **stale-tests/** — Old test artifacts and run logs
- **abandoned/** — Placeholder files

## Important

**Playgrounds and experiments are NOT archived** — they remain in production:
- `packages/atomvm/playground/` — Active experimental UI/hooks framework
- `packages/kgc-4d/playground/` — Active experimental Next.js multiverse API
- `packages/atomvm/experiments/` — Active experimental work

## Recovery

All archived content is 100% recoverable via git:

```bash
# Restore a benchmark suite
git checkout archival-wip-quarantine -- unrdf-archive/benchmarks/atomvm-benchmarks/

# Restore a specific POC
git show archival-wip-quarantine:unrdf-archive/poc/governance-poc.test.mjs

# Restore all archived content
git checkout archival-wip-quarantine -- unrdf-archive/
```
