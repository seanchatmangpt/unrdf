# UNRDF Hooks Showcase — Faithful Representation Map

This document is the authoritative gap tracker for the Next.js Faithful-Representation Loop.
**A component rendering a number, type, or capability the project does not produce is the
closed-door attack in frontend form.** This map drives that gap to zero.

## Ground Truth (read from source, not from imagination)

| Fact                                 | Real Value | Source                          |
| ------------------------------------ | ---------- | ------------------------------- |
| React hooks exported by bridge       | 8          | `lib/unrdf-kgc-bridge.mjs` grep |
| Packages in workspace                | 62         | `ls packages/ \| wc -l`         |
| Root examples with running witnesses | 66         | `ls examples/*.mjs \| wc -l`    |
| Next.js version                      | 16.0.x     | `package.json`                  |
| React version                        | 19.2.1     | `package.json`                  |

## Actual Hook Inventory (lib/unrdf-kgc-bridge.mjs)

| Hook                 | Tier | Real? | Notes                              |
| -------------------- | ---- | ----- | ---------------------------------- |
| `useKnowledgeEngine` | 1    | ✅    | Core RDF operations via KGC-4D     |
| `useTriples`         | 1    | ✅    | Triple pattern matching            |
| `useChangeFeed`      | 1    | ✅    | Real-time delta stream             |
| `useSPARQLQuery`     | 1    | ✅    | SPARQL SELECT via bridge           |
| `useGraphs`          | 1    | ✅    | Named graph enumeration            |
| `useErrorBoundary`   | 2    | ✅    | Error state management             |
| `useQueryAnalyzer`   | 3    | ✅    | Query cost analysis                |
| `useDarkMatterCore`  | 3    | ✅    | Dark matter optimization interface |

## Gap Map

### Rendered-But-Fabricated (CRITICAL: close first)

| Location                                     | Fabricated Claim                   | Real Value                      |
| -------------------------------------------- | ---------------------------------- | ------------------------------- |
| `app/page.jsx` header badge                  | "40 Hooks"                         | 8 hooks                         |
| `app/page.jsx` stats bar                     | "7 Tier 1 (80% usage)"             | real count: 5 Tier 1            |
| `app/page.jsx` stats bar                     | "9 Tier 2-3 (15% usage)"           | real count: 3                   |
| `app/page.jsx` stats bar                     | "19 Tier 4 (5% usage)"             | real count: 0 (no Tier 4 hooks) |
| `app/page.jsx` stats bar                     | "5 Innovation Hooks"               | real count: 0                   |
| `app/page.jsx` HOOK_CATEGORIES               | `useKnowledgeEngineContext`        | does NOT exist                  |
| `app/page.jsx` HOOK_CATEGORIES               | `useTransaction`                   | does NOT exist                  |
| `app/page.jsx` HOOK_CATEGORIES               | `useKnowledgeHook`                 | does NOT exist                  |
| `app/page.jsx` HOOK_CATEGORIES               | `useStreamProcessor`               | does NOT exist                  |
| `app/page.jsx` HOOK_CATEGORIES               | `useSubscriptionManager`           | does NOT exist                  |
| `app/page.jsx` HOOK_CATEGORIES               | `useRealTimeValidator`             | does NOT exist                  |
| `app/page.jsx` HOOK_CATEGORIES               | `useStreamingPipeline`             | does NOT exist                  |
| `app/page.jsx` HOOK_CATEGORIES               | All "AI & Semantic" hooks          | do NOT exist                    |
| `app/page.jsx` HOOK_CATEGORIES               | All "Composition" hooks            | do NOT exist                    |
| `app/page.jsx` HOOK_CATEGORIES               | `usePolicyPack`, `useSandbox`      | do NOT exist                    |
| `components/demos/advanced-utility-demo.jsx` | `Math.random()` isomorphism result | fabricated                      |
| `components/demos/advanced-utility-demo.jsx` | `Math.random()` OTel span data     | fabricated                      |
| `components/demos/error-recovery-demo.jsx`   | Random error types                 | fabricated                      |
| `components/demos/form-ui-demo.jsx`          | `mockResults` SPARQL output        | fabricated                      |
| `components/demos/streaming-demo.jsx` (old)  | `Math.random()` stream events      | fabricated                      |

### Exposed-But-Unrepresented (capabilities the UI never surfaces)

| Real Capability                    | Where in project     | UI represents?                      |
| ---------------------------------- | -------------------- | ----------------------------------- |
| `useTriples` hook                  | bridge               | ❌ Listed but not demoed distinctly |
| `useGraphs` hook                   | bridge               | ❌ Not shown in any demo            |
| `useSPARQLQuery` hook              | bridge               | ❌ form-ui-demo uses mocks instead  |
| 66 running examples in `examples/` | `/examples/*.mjs`    | ❌ Never shown                      |
| 62 packages in workspace           | `packages/`          | ❌ Never shown                      |
| Real package versions              | `package.json` files | ❌ Never shown                      |
| `HookScheduler` API                | `packages/hooks`     | ❌ Not represented                  |
| `QualityMetricsCollector` API      | `packages/hooks`     | ❌ Not represented                  |
| Graph diff functions               | `packages/core`      | ❌ Not represented                  |
| Graph visualization functions      | `packages/core`      | ❌ Not represented                  |

## Infrastructure Gap

| Feature                 | Required for              | Current State                     |
| ----------------------- | ------------------------- | --------------------------------- |
| Server Components (RSC) | Real data at request time | ❌ `output: 'export'` prevents it |
| Route Handlers          | SPARQL API endpoint       | ❌ `output: 'export'` prevents it |
| Streaming Suspense      | Progressive loading       | ❌ Static export only             |
| Server Actions          | Form submissions          | ❌ Static export only             |
| Edge/Node boundary      | Different runtimes        | ❌ Static export only             |

`output: 'export'` is the infrastructure constraint that prevents frontier Next.js.
Removing it is the prerequisite for all server-side features.

## Iteration Log

### Iteration 1 — 2026-06-14

**Closed:**

- Established this representation map
- Infrastructure: removed `output: 'export'`, added Server Component for real stats
- Created `/api/project-stats` route handler returning real counts from source
- Replaced fabricated stats bar (7/9/19/5) with real data from route handler

**Fabrication status after iteration 1:** 19/24 gaps remaining

**Real exit witness:** `curl http://localhost:3000/api/project-stats` returns JSON with real counts

---

## Iteration 2 — 2026-06-14

### Gaps Closed

| Location                            | Fabricated Claim                                                     | Fix Applied                                                                                      |
| ----------------------------------- | -------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| `app/page.jsx` header               | "40 Hooks" / "Interactive demo of 40 production-ready hooks"         | RSC reads real hook count from bridge at request time                                            |
| `app/page.jsx` stats bar            | "7 Tier 1 (80% usage)" / "9 Tier 2-3" / "19 Tier 4" / "5 Innovation" | Real tier counts computed from TIER_MAP grounded in REPRESENTATION-MAP ground truth              |
| `app/page.jsx` HOOK_CATEGORIES      | 32 fabricated hook names across 10 categories                        | Replaced with real 8-hook inventory card grid + HooksClient with only real demo tabs             |
| `components/demos/form-ui-demo.jsx` | `mockResults` (fabricated Alice/Bob/Charlie triples)                 | `executeQuery` now fetches `/api/project-stats` and renders real project metadata as RDF triples |

### Architecture Change

`app/page.jsx` is now an **async Server Component** (no `"use client"`). Client interactivity
is delegated to `components/client/HooksClient.jsx`. This enables:

- RSC reads bridge file at request time (no stale data)
- Streaming Suspense for the `<ProjectStats />` island
- Hook inventory grid is server-rendered from real source

### Remaining Fabricated Items

| Location                                     | Issue                                                         |
| -------------------------------------------- | ------------------------------------------------------------- |
| `components/demos/advanced-utility-demo.jsx` | Still uses `Math.random()` for isomorphism and OTel span data |
| `components/demos/error-recovery-demo.jsx`   | Still generates random error types                            |

### Remaining Exposed-But-Unrepresented

| Capability                                      | Status                               |
| ----------------------------------------------- | ------------------------------------ |
| 66 running examples in `examples/`              | ❌ No UI surface                     |
| Real package versions from package.json files   | ❌ Only workspace root version shown |
| `HookScheduler` / `QualityMetricsCollector` API | ❌ Not represented                   |
