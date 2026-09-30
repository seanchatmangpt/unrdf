# CI architecture

The pull-request court is **one workflow, `ci.yml`**, built to establish the required claims in the least
wall-clock time. Everything else is either path-gated (it can only have something to say when specific files
change) or scheduled (slow, report-style, or only meaningful over time).

## Why it looks like this (measured, not assumed)

Baseline for one PR commit (head `8f178285`, OBSERVED from Actions job timestamps):

| | |
|---|---|
| workflow runs fanned out per push | 20 |
| jobs executed / skipped | 49 / 13 |
| runner time | 3852 s (64.2 min) |
| queue wait (job created → started) | median 256 s, p90 ≈ 723 s, max 796 s |
| time spent in per-job setup/teardown (checkout, Node, install, post steps) | 1108 s = 28.8 % of runner time |
| first queued → last finished | 22.6 min |
| longest jobs | Security Invariants 709 s, verify-packages 380 s, CodeQL 296 s |

Two facts drove the design:

1. **Queue wait dominated.** ~25 workflows × several jobs each exceeded the repository's concurrent-runner
   limit, so most jobs waited minutes for a runner before doing seconds of work. Fewer jobs per PR is the
   biggest lever, more than any step-level speed-up.
2. **The same claims were verified many times.** Every PR ran the whole test suite in `ci.yml` (twice, on two
   Node versions), `checks`, `quality`, `quality-gates`, `thesis-validation` and `package-matrix`; lint and
   install were repeated in about six workflows; the audit ratchet ran twice; the 11-minute
   `full-check`/dashboard security reports ran on PRs although nothing gates on them.

## Topology

```
pull_request ──► ci.yml
                   ├─ static      lint ∥ prettier, n3/TS policy, script tests, audit ratchet, security gate, workflow validation
                   ├─ secrets     TruffleHog + Gitleaks (PR diff)
                   ├─ dependency-review   (needs DEPENDENCY_GRAPH_ENABLED=true)
                   ├─ packages    node 22 × 2 weight-balanced shards
                   │               lint/build/test only for changed packages + dependents; import-smoke for every package
                   └─ gate        "CI Gate": fails if any lane failed; writes a timing receipt to the job summary
               path-gated:  codeql.yml, cli-sync-tests.yml, capability-docs.yml, atomvm-*.yml, r83/r88, interchangeable-parts,
                            otel-weaver-validate, unrdf-sync-integration, perf.yml (regression gate), thesis-validation.yml

push to main ──► ci.yml with the full matrix: node 20/22 × 3 shards, everything (no affected filtering)
nightly      ──► scheduled.yml (coverage + quality thresholds, advisory analysis), security.yml (deep scans)
```

### Change impact (`scripts/ci/affected.mjs`)

A changed file belongs to the package with the longest matching directory. Every package that (transitively)
depends on a changed package is verified too. Toolchain-level files (lockfile, root `package.json`, shared
configs, `.github/actions/**`, `ci.yml`, `scripts/ci/**`, the matrix engine) select **every** package, and an
unreadable change list also selects everything. A wrong guess can cost time, never coverage. Import-smoke runs
for all packages regardless, so a broken entry point can never hide behind "not affected". main always runs
the full set.

### Sharding

`scripts/ci/package-weights.json` holds measured seconds per package; packages are assigned to shards by
longest-processing-time-first. Assignment ignores which packages are selected, so a package always lands on
the same shard. Stale weights degrade balance only. Shard count (2 on PRs) is chosen so runner start-up plus
repeated setup stays below the wall-clock saved.

### Caching

* pnpm store: `actions/setup-node` `cache: pnpm`, keyed on `pnpm-lock.yaml` (observed: setup-node including cache restore 12–20 s, install 9–15 s).
* Playwright browsers: `actions/cache`, keyed on the lockfile, and only on shards that select `@unrdf/atomvm`
  (the only package with browser tests).
* No cache is required for correctness; a miss only costs download time.
* Not cached on purpose: build outputs. Builds are ~21 % of matrix time (234 of 1105 s) and are produced in the shard that uses
  them, so an artifact round-trip would cost more than it saves.

## What moved, and why no verification was lost

| Old check | Now | Note |
|---|---|---|
| CI `Lint & Format` | `static` | lint ∥ prettier in one job |
| CI `Test Suite (20)/(22)`, `checks`, `quality`, `Quality Gate 20.x/22.x`, `thesis validate` (lint+test), `package-matrix` | `packages` | one execution of each package's tests; Node 20/22 on main |
| CI `Security Audit` + security.yml `Dependency Audit` | `static` (PR), `security.yml audit` (nightly) | were the identical ratchet twice |
| `Security Invariants` | `static` (gate, 3 s) / `security.yml invariants` (+ report-only `full-check`, dashboard) | the gate step never read the slow reports |
| `Secrets Detection` | `secrets` (PR), `security.yml secrets` (full history) | |
| `SAST Analysis` | `codeql.yml` (path-gated on JS changes, main, weekly) | |
| `License Compliance`, `Supply Chain`, `Security Best Practices` | `security.yml advisory` | none of these could fail a build |
| `Code Quality` ×6 | `scheduled.yml advisory` | all report-only (`\|\| echo`) |
| CLI sync ×6 jobs + CI duplicate | `cli-sync-tests.yml` (1 job) | same tests, same scripts |
| `Legacy package admission` | `static` step | receipt still uploaded |
| `TypeScript Gate`, n3 policy, file-size advisory | `static` steps | |
| `Quality Gate & OTEL Validation` | `Capability Docs Structure` | the lint/`test:fast` parts were duplicates; the OTEL steps were `continue-on-error` warnings and the same `run-all` still gates in thesis-validation |
| coverage threshold in `ci.yml` | removed | it read `coverage/coverage-summary.json`, which no job produces, and always printed "skipping" |
| coverage ≥ 80 % / score ≥ 70 gates in `quality.yml` | `scheduled.yml quality` | reported nightly; enforced only when repo variable `ENFORCE_QUALITY_GATES=true` (the old job aborted before reaching them, so their pass state is unknown) |

### Decisions left to maintainers

* Branch protection should require **`CI Gate`** (it aggregates `static`, `secrets`, `dependency-review`,
  `packages`). Path-gated workflows cannot be required checks, and the old per-job names no longer exist.
* Pre-merge assurance that moved post-merge: Node 20 test runs (PRs use Node 22; main runs Node 20 and 22), unchanged-package lint/build/test, coverage/quality thresholds.
  The scheduled and main runs still execute them.
* `DEPENDENCY_GRAPH_ENABLED` and `ENFORCE_QUALITY_GATES` repository variables.

## Reading CI timing

Each `ci.yml` run writes a **timing receipt** (queue and run seconds per job) to the `CI Gate` job summary, and
each shard writes its package-matrix summary (selection, shard, states). `receipt.json` is uploaded per shard.
Refresh `package-weights.json` from those receipts when shard balance drifts.
