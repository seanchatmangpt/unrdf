# unrdf migration plan v26.11.16

Standing: UNVERIFIED

## Identity
- Path: /Users/sac/unrdf
- Remote: https://github.com/seanchatmangpt/unrdf.git
- Branch: v26926/unrdf-113-court2@0550d963 — NO upstream tracking (census up=-)
- Last commit: 2026-09-28 ("Merge pull request #115 ... claude/keen-thompson-ja7xo2")
- Commits: 1475; size: 1274 MB
- Languages/build: JS/TS pnpm monorepo (package.json, pnpm-workspace.yaml), pyproject.toml,
  makefile, Dockerfile, k8s, grafana; GEMINI.md, no CLAUDE.md
- Nested repos (catalogued here):
  - unrdf/open-ontologies — VENDOR, main@80ecc49, github fabio-rovai/open-ontologies, in sync,
    244 commits, 364 MB, dirty 46 + 30 untracked (.claude-plugin, .mcp, CHANGELOG, README,
    SKILL.md modified — Sean's local edits on a third-party clone), last 2026-05-02

## Observed state
- Dirty: 63 tracked, untracked: 14. Grouped: docs/ (59 modified), packages/ (4), README.md,
  pnpm-workspace.yaml, pnpm-lock.yaml, benchmark-run.mjs, examples-survey.md, open-ontologies
  gitlink.
- HEAD branch has no upstream; ahead/behind unknown. Stash: 1247. Local branches: 33,
  29 unpushed (005-*, 006-*, archival-*, many `claude/*` session branches).
- Build dirs: no root node_modules found. Submodule: `open-ontologies` gitlink without
  .gitmodules mapping (`git submodule status` errors).

## What it is
UNRDF: a JavaScript RDF knowledge-graph platform (RDF/SPARQL/SHACL, hooks, daemon, CLI) with
extensive docs, benchmarks and k8s/grafana tooling. "Integrated Vision 2030" status banner.

## Classification
ACTIVE (census) -> WARM: last commit 2026-09-28, large dirty docs set, HEAD off-main. Either
label leads to the same action.

## Migration risk
- Current branch `v26926/unrdf-113-court2` has no upstream — may be entirely unpushed.
- 29 unpushed branches, 1247 stashes, 63 dirty files (mostly docs).
- open-ontologies: 76 local changes on a third-party clone (.claude-plugin, .mcp, SKILL.md)
  that have no home upstream — would be lost.
Rating: HIGH.

## Recommended action
COMMIT-PUSH

1. `git add docs packages README.md pnpm-workspace.yaml pnpm-lock.yaml benchmark-run.mjs
   examples-survey.md && git commit -m "wip: pre-migration snapshot 2026-11-16"`.
2. `git push -u origin v26926/unrdf-113-court2`.
3. `git push --all origin` (DECISION: prune stale `claude/*` session branches first).
4. `tar czf /Volumes/archive/unrdf-dotgit.tgz -C /Users/sac/unrdf .git` (1247 stashes).
5. open-ontologies: `git -C /Users/sac/unrdf/open-ontologies diff > /Volumes/archive/
   open-ontologies-local.patch` and tar untracked files; DECISION: fork fabio-rovai/
   open-ontologies to seanchatmangpt and push a branch, or drop the local edits.
6. Fix the gitlink: add `.gitmodules` for open-ontologies or `git rm --cached open-ontologies`.
7. New machine: clone fresh; `rm -rf node_modules` locally before any fallback copy.

## First commands on the new machine
`pnpm install --frozen-lockfile && pnpm -r test` (Node >= 18), `make check` if the makefile
defines it. Stated, not run.

## See Also
- ../v26.9.19/
- /Users/sac/docs/jira/v26.11.16/README.md
