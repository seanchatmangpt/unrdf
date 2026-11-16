# unrdf: commit or clean uncommitted changes

- Standing: OPEN
- Created: 2026-09-19 (v26.9.19 gh survey wave)
- Source: working tree dirty at survey time
- Evidence: `git status --porcelain` → 5 path(s) (tracked-modified: 4, untracked: 1); sample:  M docs/TROUBLESHOOTING.md; M docs/archive/legacy-pre-2030/nextra-repo; M open-ontologies;?? benchmark-run.mjs;

## Work to complete
- Review the 4 tracked-modified path(s); commit them as atomic pieces on a purpose branch, or revert what is transient.
- For the 1 untracked path(s): add intentional files to git and commit; gitignore or delete build artifacts/temp files.
- Note: this survey's ticket files under docs/jira/v26.9.19/ are intentionally uncommitted; include or exclude them deliberately in the commit plan.

## Acceptance
- `git status --porcelain` is clean (except items deliberately deferred and recorded here).

## History
- 2026-09-19 | OPEN | survey found dirty tree | 5 paths (T4/U1) | commit/clean pending
