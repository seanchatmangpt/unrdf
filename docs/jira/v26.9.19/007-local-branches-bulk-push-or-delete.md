# unrdf: triage 23 local-only branches

- Standing: OPEN
- Created: 2026-09-19 (v26.9.19 gh survey wave)
- Source: local branches with commits not on `origin/main` and no upstream
- Evidence: `git rev-list --count origin/main..<branch>` > 0 for each: 005-unify-packages:497 006-maturity-matrix:518 archival-non-production-packages:1 archival-wip-quarantine:1 claude/audit-browser-package-015GinWqfYj11WLkphurmcRi:329 claude/c4-monorepo-diagram-01LdBb5HcvX9RoQgVfyN7A2S:260 claude/cli-production-readiness-01UuWkrkmBsjphRXdgTgbm3p:357 claude/database-canvas-whiteboard-01E6Ze9cxsBL8q6SqUY13NcT:402 claude/finish-packages-lbLaq:488 claude/fix-nextra-styling-01872ukYRtzQeeiDFSyPzM3X:423 claude/implement-2028-features-013nhcD2ZB4631WU7k2z9Wbf:112 claude/knowledge-hooks-benchmarks-014TtWzu7p8wpAJ7ivpQoGXB:245 claude/remove-playground-01Ka4RfdH1LamevddvQ1ggrr:345 claude/rename-kgen-templates-012kKvTvdDffvMYqRyqcr6nV:445 claude/review-package-docs-01Td6aLfAEjy69Wg93tEdi6D:444 claude/review-react-structure-015jWwcnfiG3vX7fRWCjfCYC:329 claude/rewrite-docs-monorepo-011wca5kv4dmUw4KRSdFnSx4:271 claude/run-tests-verify-readme-01Q2AUKJgS6zT7FfW2ufH6tn:151 claude/setup-mdx-nextra-01CfrPWT4YzhTucMUz3cyPVJ:426 claude/setup-nuxt-ui-docs-01UM7zgaDRnszoPKM7793bHD:442 gh-pages:1 merge-2028-features-for-v4:122 wip/save-all-20260615:12

## Work to complete
- Triage each branch: push (`git push -u origin <branch>`) or delete after confirming the commits are obsolete/recoverable. Batch the work; record decisions in History.

## Acceptance
- Every listed branch is pushed or deleted; no local-only branch with unique commits remains.

## History
- 2026-09-19 | OPEN | survey found 23 local-only branches | full list above | triage pending
