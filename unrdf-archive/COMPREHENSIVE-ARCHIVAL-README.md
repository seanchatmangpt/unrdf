# Comprehensive WIP Archival

**Scope:** All tracked WIP, proofs-of-concept, and test artifacts removed from active packages.

## What's Archived

### Test Results & Reports (atomvm)
- 6 Playwright test result directories with failure screenshots and logs
- Playwright HTML report with test execution data

### Proof-of-Concept Implementations
- atomvm/proofs/ — BEAM pattern matching and serialization POCs
- hooks/proofs/ — Policy-controlled workflow standalone implementations

### Generated Code
- chatman-equation/generated/ — Auto-generated API reference and tutorials

## What's NOT Archived (Already .gitignored)

These filesystem artifacts are already excluded from git and don't need archival:
- coverage/ (21 dirs, 4.1GB) — Test coverage reports, auto-generated
- dist/ (18 dirs, 3GB+) — Build artifacts, auto-generated
- .next/ — Next.js build cache

## Impact

- 25 git-tracked files archived
- Zero production code affected
- All content preserved in git history
- Cleaner active codebase

## Recovery

```bash
git checkout archival-comprehensive -- unrdf-archive/
```

