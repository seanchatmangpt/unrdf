# Cleanup and Merge Plan: unrdf

## Current State (as observed 2026-09-17)

| Path                     | Type                                                                                                                    | Git status summary                                                                                                                                                                                                                                  | Last commit (local HEAD)                                                                                                                                               | Size |
| ------------------------ | ----------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---- |
| `/Users/sac/unrdf`       | Canonical repo (has GitHub remote, most commits, most branches)                                                         | Dirty: 12 modified, 2 modified-submodule-ish (`open-ontologies`, `packages/wasm4pm`, `docs/archive/legacy-pre-2030/nextra-repo`), 4 deleted, 13 untracked. On branch `wip/save-all-20260615`, not `main`.                                           | `11affe02` 2026-06-15 "chore(wip): save in-progress work" (current HEAD, branch `wip/save-all-20260615`). `main` branch tip is `05e46431` (separate, not checked out). | 1.2G |
| `/Users/sac/unrdf-clean` | Duplicate/independent repo (own `.git`, remote = `file:///Users/sac/unrdf`, i.e. a local clone of unrdf, not of GitHub) | Dirty: 157 deleted (all under `archive/2026-01-11/...`), 1 modified (`pnpm-lock.yaml`), 2 untracked (`Justfile` already exists in canonical at a newer date; `.specify/integrations/` already exists in canonical). On branch `main` at `57875caf`. | `57875caf` 2026-04-03 "archive(comprehensive-wip): quarantine tracked WIP and test artifacts"                                                                          | 370M |

Notes on the table above, with the evidence behind each claim:

- `git worktree list` run inside `/Users/sac/unrdf` returns **only** `/Users/sac/unrdf` itself (`11affe02 [wip/save-all-20260615]`). There are no registered git worktrees of this repo anywhere (no `.claude/worktrees` entries either — that directory exists but is empty), so `/Users/sac/unrdf-clean` is not a worktree; it is a fully independent `.git` clone.
- `/Users/sac/unrdf-clean`'s remote is `origin -> file:///Users/sac/unrdf` (confirmed via `git -C /Users/sac/unrdf-clean remote -v`), i.e. it was cloned from the local `unrdf` checkout at some point in the past, not from GitHub directly.
- `/Users/sac/unrdf`'s own remote is the real GitHub repo: `origin -> https://github.com/seanchatmangpt/unrdf.git`.
- `/Users/sac/unrdf`'s local `main` branch (`05e46431`) is reported by `git branch -vv` as `[origin/main: ahead 9, behind 121]` — i.e. `/Users/sac/unrdf`'s `main` has 9 local commits GitHub doesn't have, and is 121 commits behind GitHub's current `main` tip (`62aaa803`, confirmed via `git log origin/main --oneline`).
- `/Users/sac/unrdf-clean`'s `main` (`57875caf`) is confirmed, via `git -C /Users/sac/unrdf branch --contains 57875caf`, to already be reachable from **five** branches inside the canonical repo: `archival-comprehensive`, `feat/otel-observability-stack`, `main`, `wip/save-all-20260615` (current HEAD), and `worktree-ostar-innovations-4-6`. In fact `57875caf` is the exact tip commit of `archival-comprehensive` in the canonical repo.
- `git -C /Users/sac/unrdf-clean log main ^57875caf` (i.e. "commits reachable from unrdf-clean's main but not from its own tip") returns nothing — trivially true since HEAD==main, but combined with the branch-containment check above this confirms `unrdf-clean` has **zero commits that do not already exist in the canonical `unrdf` repo**.
- `unrdf-clean`'s 157 uncommitted deletions are all under `archive/2026-01-11/...`. Checked whether that path still exists in canonical `unrdf`'s `main` tree: `git -C /Users/sac/unrdf show main:archive/2026-01-11/agents/README.md` exits 128 (path does not exist). So these are not new/unique deletions — canonical `unrdf` already removed this directory tree in commits made after `57875caf`. The uncommitted deletions in `unrdf-clean` are the working tree catching up to a state canonical has already reached; they carry no unique intent.
- `unrdf-clean`'s remaining uncommitted changes: `pnpm-lock.yaml` (M, routine lockfile drift, not unique content) and two untracked entries, `Justfile` and `.specify/integrations/`. Both paths already exist in canonical `unrdf` (`Justfile` present, newer mtime Jun 15; `.specify/integrations/portfolio.json` present, newer mtime May 4) with different content — these are stale untracked copies from `unrdf-clean`'s older checkout point, not net-new work.
- User confirmed "gemma" was already deleted manually. `find -iname "*gemma*"` over both `/Users/sac/unrdf` and `/Users/sac/unrdf-clean` (maxdepth 3) returns no matches in either — consistent, nothing further to do there.
- No other `unrdf*` git repos or worktrees exist under `/Users/sac`. `ls -d /Users/sac/unrdf*` shows only `unrdf`, `unrdf-clean`, and three unrelated stray text files (`unrdf-convo.txt`, `unrdf-src.txt`, `unrdf.txt`, not investigated further as they are plain files, not repos). The broader `~/*-worktree*`/`~/*-wt` sweep found worktree directory families for _other_ projects (`ex4pm-worktrees`, `ggen-marketplace-worktrees`, `unibit-overnight-worktree`, `wasm4pm-worktrees`, `worktrees`, `xaas-worktrees`, `ash_a2a-wt`, `ash-surface-wt`, `praxis-hierarchical-projection-wt`, `zoela-wt`) — none of these are unrdf-related; they are out of scope for this doc, which is scoped to the unrdf family per the investigation request.

## What "merged" should look like

**`/Users/sac/unrdf` is canonical, going forward.** Evidence:

- It is the only path with a real GitHub remote (`origin -> https://github.com/seanchatmangpt/unrdf.git`); `unrdf-clean`'s remote points at `unrdf` itself, not GitHub, so `unrdf-clean` is structurally downstream of `unrdf`, never the reverse.
- It has the far larger and more recent commit graph: 25+ local branches including active `claude/*` work branches, `main` at `05e46431` (2026-06-15 area) vs. `unrdf-clean`'s single `main` branch frozen at `57875caf` (2026-04-03).
- Every commit reachable in `unrdf-clean` (`57875caf` and its ancestors) is already reachable inside `unrdf` (confirmed via `branch --contains`, above) — so `unrdf-clean` contributes **zero unique history**.
- `unrdf-clean`'s only uncommitted changes are (a) deletions that redundantly re-derive a state canonical already committed, and (b) untracked files that already exist in canonical under different content. None of it is unique, reviewable work.

**`/Users/sac/unrdf-clean` — what must happen before deletion:**

This is an independent clone with no unique commits and no unique uncommitted content (per the evidence above). Per the harness's own instruction, this still needs a human glance rather than a blind `rm -rf`, because:

- Its working tree has 157 pending deletions, 1 modification, and 2 untracked files. Automation cannot prove with certainty that none of the untracked/modified content (`pnpm-lock.yaml`, `Justfile`, `.specify/integrations/portfolio.json`) was intentionally hand-edited here as a draft for something not yet ported to canonical — the content differs from canonical's current versions and no commit exists to diff against cleanly.
- MANUAL REVIEW REQUIRED: a one-time `diff` of these 3 files against their canonical counterparts (commands below) to confirm nothing in `unrdf-clean`'s working copy is a wanted edit that never made it into `unrdf`. Given the evidence (older mtimes than canonical, and canonical demonstrably evolved further), the expected outcome is "nothing unique, safe to delete," but that diff should be eyeballed once before the `rm -rf`, not assumed.
- Once that review confirms no wanted content, deletion is a plain directory removal — not a git operation, and not a merge, since there is nothing to merge.

## Commands to run (in order), once approved

```bash
# 1. Confirm no other process/editor has unrdf-clean open (safety, no-op if clean)
lsof +D /Users/sac/unrdf-clean 2>/dev/null | head -20

# 2. MANUAL REVIEW REQUIRED — diff the 3 non-redundant paths against canonical
#    before deleting. Read the output; do not skip this step.
diff /Users/sac/unrdf-clean/Justfile /Users/sac/unrdf/Justfile
diff /Users/sac/unrdf-clean/.specify/integrations/portfolio.json /Users/sac/unrdf/.specify/integrations/portfolio.json
diff /Users/sac/unrdf-clean/pnpm-lock.yaml /Users/sac/unrdf/pnpm-lock.yaml | head -50

# 3. Only after step 2 confirms nothing worth keeping, remove the duplicate clone.
#    This is a plain directory deletion, not a git operation — unrdf-clean is an
#    independent clone, not a worktree, so `git worktree remove` does not apply.
rm -rf /Users/sac/unrdf-clean

# 4. Separately, and NOT gated on step 3: canonical unrdf's own working tree
#    (branch wip/save-all-20260615) is dirty with real, non-redundant changes
#    (modified atomvm/kgc-4d/graph-analytics source files, deleted/regenerated
#    AtomVM wasm build artifacts, new benchmark scripts). This is unrelated to
#    the unrdf-clean duplicate and is MANUAL REVIEW REQUIRED — decide per-file
#    whether to commit, stash, or discard. No blanket command is offered here
#    because some of these are build artifacts (safe to discard) and some are
#    real source edits (not safe to discard blindly). Start with:
git -C /Users/sac/unrdf status --short
git -C /Users/sac/unrdf diff --stat
```

No commands here touch `/Users/sac/unrdf` itself beyond read-only `status`/`diff` — the canonical repo's own dirty working tree is a separate, unrelated cleanup decision from the `unrdf-clean` duplicate-removal decision, and is left to manual review as instructed.

## Open questions

1. **`unrdf-clean`'s 3 non-redundant files** (`Justfile`, `.specify/integrations/portfolio.json`, `pnpm-lock.yaml`) — git alone cannot tell whether these represent an intentional draft edit made directly in `unrdf-clean` that the user meant to port over, or just leftover state from whatever workflow created this clone. The diffs in step 2 above should resolve this, but only a human (or the user) can say whether any difference found is "wanted, port it" vs. "stale, discard it."
2. **Canonical `unrdf`'s own dirty working tree** (branch `wip/save-all-20260615`, 12 modified + 4 deleted + 13 untracked files spanning `packages/atomvm`, `packages/kgc-4d`, `packages/graph-analytics`, `pnpm-lock.yaml`/`pnpm-workspace.yaml`, and dirty submodule-like entries `open-ontologies` and `packages/wasm4pm`) is out of scope for "is `unrdf-clean` safe to delete," but it is real, uncommitted, non-trivial work sitting on a non-`main` branch. Git cannot tell from the diff alone whether this is active WIP the user is mid-edit on or an abandoned save-point from 2026-06-15 that should be committed/discarded. Ask the user directly before touching it.
3. **`unrdf`'s `main` branch is 121 commits behind GitHub's `origin/main` and 9 commits ahead of it** (diverged, not just behind) — outside this doc's scope (which is the `unrdf`/`unrdf-clean` duplication question), but worth flagging: reconciling local `main` with `origin/main` is a separate, larger merge decision the user may want a dedicated pass for, not folded into this cleanup.
4. **The three stray files** `/Users/sac/unrdf-convo.txt`, `/Users/sac/unrdf-src.txt`, `/Users/sac/unrdf.txt` sitting alongside the two repos were noticed but not investigated (not git repos, not requested in scope) — flagging in case they are also cleanup candidates in a future pass.
5. **The broader "~/ worktrees temps scratch etc" sweep** the user's original request named is wider than the `unrdf`/`unrdf-clean` pair this doc covers (other worktree families exist for `ex4pm`, `ggen-marketplace`, `wasm4pm`, `xaas`, `ash_a2a`, `ash-surface`, `praxis-hierarchical-projection`, `zoela`, and a generic `~/worktrees`). None of those were investigated here — this doc is scoped strictly to the unrdf family per the computed task. A follow-up pass, one project family at a time, would be needed to cover the rest.
