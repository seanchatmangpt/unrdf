# CI baselines (ratchets)

These files let a security gate fail **only on things that got worse**, instead of staying red on every PR because of
debt that already exists. The debt is not hidden: it is listed here, in review, and CI prints how much of it remains.

| File                      | Used by                                                      | Key                                            | Gate                                           |
| ------------------------- | ------------------------------------------------------------ | ---------------------------------------------- | ---------------------------------------------- |
| `audit.json`              | `Static checks` (ci.yml, PRs), `Dependency Audit` (security.yml, nightly) | `advisory id \| package \| installed versions` | fails on any moderate+ advisory not listed     |
| `injection-critical.json` | `Static checks` (ci.yml, PRs), `Security Invariants` (security.yml)                 | `file \| rule` -> count                        | fails on a new `file\|rule`, or a higher count |

Secrets scanning is **not** ratcheted: it stays at zero tolerance.

## Working with them

```bash
node scripts/ci/audit-ratchet.mjs                    # compare the current tree to audit.json
node scripts/ci/audit-ratchet.mjs --update           # after FIXING advisories: shrink the baseline
node --test scripts/ci/ratchet.test.mjs              # self-test of the ratchet logic
```

For the injection baseline, generate the report from a clean checkout (a working tree with extra git worktrees or
`node_modules` copies is scanned too and inflates the numbers), then:

```bash
node scripts/ci/findings-ratchet.mjs --report injection-report.json \
  --baseline .github/baselines/injection-critical.json          # compare
node scripts/ci/findings-ratchet.mjs --report injection-report.json \
  --baseline .github/baselines/injection-critical.json --update # after FIXING findings
```

Only ever `--update` to make a baseline **smaller**. Growing it to make CI green defeats the point; a reviewer should
reject a baseline diff that adds entries unless the finding is genuinely accepted.

Exit codes: `0` nothing new, `1` new/increased findings, `2` the audit or report could not be read (never a pass).

## Dependency Review

`ci.yml`'s `Dependency Review` job needs the repository's _Dependency graph_ setting. Once it is enabled, set the
repository variable `DEPENDENCY_GRAPH_ENABLED=true` and the job starts enforcing (until then it is skipped).
