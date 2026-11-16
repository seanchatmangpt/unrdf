# unrdf: triage 15 PR-less unmerged remote branches

- Standing: OPEN
- Created: 2026-09-19 (v26.9.19 gh survey wave)
- Source: origin branches not merged into `main` with no open PR
- Evidence: `git branch -r --no-merged origin/main`: agent/dense-tranche-three agent/finish-unrdf-wip agent/merge-crown-tip-20260812 chore/ggen-parent-snapshot-2026-03-31 claude/add-claude-documentation-S3gJi claude/consolidate-tests-Fssji claude/finish-all-packages-fvblw claude/finish-daemon-haz0I claude/launch-agents-v6-release-6W6vW claude/launch-task-agents-dMjer claude/setup-rdf-prefixes-N1O85 claude/yawl-gap-analysis-w8HBu fanout/r83-tcps-ready-set gh-pages ws5/ggen-sync-only-20260828

## Work to complete
- Triage each branch: land (open a PR) or delete (`git push origin --delete <branch>`). Work in batches; record decisions in History.

## Acceptance
- `git branch -r --no-merged origin/main` is empty after `git fetch --prune`.

## History
- 2026-09-19 | OPEN | survey found 15 PR-less branches | full list above | triage pending
