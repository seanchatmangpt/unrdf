# UNRDF v5 Monorepo - Visual Diagrams

## Package Architecture

```
                        ┌─────────────────┐
                        │  @unrdf/core    │
                        │  RDF Operations │
                        │  SPARQL Queries │
                        │  Canonicalize   │
                        │  150KB          │
                        └────────┬────────┘
                                 │
            ┌────────────────────┼────────────────────┐
            │                    │                    │
            │                    │                    │
      ┌─────▼─────┐      ┌──────▼──────┐      ┌─────▼──────┐
      │@unrdf/    │      │@unrdf/      │      │@unrdf/     │
      │hooks      │      │federation   │      │streaming   │
      │Policy API │      │Peer Queries │      │Change Feed │
      │50KB       │      │80KB         │      │60KB        │
      └────┬──────┘      └──────┬──────┘      └─────┬──────┘
           │                    │                    │
           └────────┬───────────┴────────┬──────────┘
                    │                    │
            ┌───────▼────┐        ┌──────▼──────┐
            │@unrdf/cli  │        │@unrdf/      │
            │CLI Tools   │        │browser      │
            │70KB        │        │Browser SDK  │
            │            │        │40KB         │
            └────────────┘        └─────────────┘


Optional Extensions (Build on Core):

            ┌─────────────────────────────────────────┐
            │       @unrdf/knowledge-engine           │
            │    Rule Engine, Inference, AI           │
            │    250KB (separate version: 2.x)        │
            └─────────────────────────────────────────┘

            ┌─────────────────────────────────────────┐
            │         @unrdf/dark-matter              │
            │   Query Optimization, Performance       │
            │    100KB (separate version: 1.x)        │
            └─────────────────────────────────────────┘

            ┌─────────────────────────────────────────┐
            │        @unrdf/composables               │
            │   Vue 3 Composables, Reactive State     │
            │    30KB (web only, separate version)    │
            └─────────────────────────────────────────┘
```

## User Installation Paths

### Path 1: Minimal (Edge/IoT)
```
User Code
    ↓
@unrdf/core (150KB)
    ↓
Done! No dependencies, just RDF ops
```

### Path 2: Substrate (Most Users)
```
User Code (Agents, ETL, Data Pipelines)
    ↓
@unrdf/core (150KB)
  ├── @unrdf/hooks (50KB)
  ├── @unrdf/federation (80KB)
  └── @unrdf/streaming (60KB)
    ↓
Total: 340KB - Complete substrate
```

### Path 3: Web App
```
Vue 3 App
    ↓
@unrdf/composables (30KB)
  ├── @unrdf/browser (40KB)
  │   ├── @unrdf/core (150KB)
  │   └── @unrdf/streaming (60KB)
  └── @unrdf/streaming (60KB)
    ↓
Total: 340KB - Vue app with RDF state
```

### Path 4: CLI/Data Engineer
```
ETL Script / Data Pipeline
    ↓
@unrdf/cli (70KB)
  ├── @unrdf/core (150KB)
  ├── @unrdf/hooks (50KB)
  ├── @unrdf/federation (80KB)
  └── @unrdf/streaming (60KB)
    ↓
Total: 410KB - Full data engineering toolset
```

### Path 5: Full Stack (Everything)
```
User Application
    ↓
All 10 packages installed
@unrdf/core (150KB) +
@unrdf/hooks (50KB) +
@unrdf/federation (80KB) +
@unrdf/streaming (60KB) +
@unrdf/browser (40KB) +
@unrdf/cli (70KB) +
@unrdf/knowledge-engine (250KB) +
@unrdf/dark-matter (100KB) +
@unrdf/composables (30KB) +
@unrdf/project-engine (200KB)
    ↓
Total: 1.03MB - Complete ecosystem
```

## VOC to Package Mapping

```
┌────────────────────────────────────────────────────────────────┐
│               7 SYNTHETIC VOICES OF CUSTOMER (VOCs)            │
└────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────┬──────────────────────────────┐
│   4 AI AGENT VOCs                │   3 HUMAN VOCs               │
├──────────────────────────────────┼──────────────────────────────┤
│                                  │                              │
│ 1. Knowledge Agent               │ 5. Data Engineer             │
│    Multi-agent coordination      │    ETL pipeline builder      │
│    → core,hooks,federation       │    → cli (auto-installs)     │
│                                  │                              │
│ 2. Sync Agent                    │ 6. App Developer             │
│    Real-time replication         │    Web/mobile app builder    │
│    → core,streaming,federation   │    → composables             │
│                                  │                              │
│ 3. ML Agent                      │ 7. DevOps Operator           │
│    Pattern learning              │    Production operations     │
│    → core,hooks,streaming        │    → cli + observability     │
│                                  │                              │
│ 4. Audit Agent                   │                              │
│    Compliance monitoring         │                              │
│    → core,hooks,streaming        │                              │
│                                  │                              │
└──────────────────────────────────┴──────────────────────────────┘

All 7 VOCs satisfied by substrate alone!
No knowledge-engine, dark-matter, or composables needed.
(These are optional enhancements for specific scenarios)
```

## Evolution from v4 to v5

### v4.x: Monolithic
```
┌─────────────────────────────────────────┐
│           UNRDF v4.2.3                  │
│  "Everything in one package"            │
│                                         │
│  ├─ RDF operations       (NEEDED)       │
│  ├─ Knowledge Hooks      (NEEDED)       │
│  ├─ Federation           (NEEDED)       │
│  ├─ Streaming            (NEEDED)       │
│  ├─ Browser SDK          (OPTIONAL)     │
│  ├─ CLI                  (OPTIONAL)     │
│  ├─ Knowledge Engine     (RARELY USED)  │
│  ├─ Dark Matter          (RARELY USED)  │
│  ├─ Composables          (WEB ONLY)     │
│  ├─ Project Engine       (DEV ONLY)     │
│  └─ Examples & Docs                     │
│                                         │
│  Size: 2.5 MB                           │
│  Useful: ~30%                           │
│  Bloat: ~70%                            │
│                                         │
└─────────────────────────────────────────┘
```

### v5.0+: Monorepo
```
┌─────────────────────────────────────────────────────────────────┐
│                    UNRDF v5.0 MONOREPO                          │
│        "One repo, multiple focused packages"                    │
│                                                                 │
│  ESSENTIAL SUBSTRATE:                                          │
│  ├─ @unrdf/core              (150KB) ⭐ Always needed         │
│  ├─ @unrdf/hooks             (50KB)  ⭐ Always needed         │
│  ├─ @unrdf/federation        (80KB)  ⭐ Always needed         │
│  ├─ @unrdf/streaming         (60KB)  ⭐ Always needed         │
│  └─ @unrdf/browser           (40KB)  ⭐ Web apps needed       │
│                                                                 │
│  TOOLING:                                                       │
│  └─ @unrdf/cli               (70KB)  ⭐ Operators need        │
│                                                                 │
│  OPTIONAL EXTENSIONS:                                          │
│  ├─ @unrdf/knowledge-engine  (250KB)  💡 Optional: Rules      │
│  ├─ @unrdf/dark-matter       (100KB)  💡 Optional: Perf       │
│  └─ @unrdf/composables       (30KB)   💡 Optional: Vue 3      │
│                                                                 │
│  DEVELOPMENT ONLY:                                             │
│  └─ @unrdf/project-engine    (200KB)  🔧 Contributing only    │
│                                                                 │
│  Minimal Install: 150 KB  (@unrdf/core)                       │
│  Typical Install: 340 KB  (substrate)                         │
│  Full Stack:      1.0 MB  (all packages)                      │
│                                                                 │
│  Users get exactly what they need, nothing more.              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Size Reduction Impact

### Before (v4.x)
```
npm install unrdf

2.5 MB total
  ├─ Core RDF: 500 KB (needed by all)
  ├─ Hooks: 200 KB (needed by most)
  ├─ Federation: 300 KB (needed by many)
  ├─ Streaming: 250 KB (needed by many)
  ├─ Knowledge Engine: 600 KB (used by few)
  ├─ Dark Matter: 200 KB (used by few)
  ├─ Composables: 100 KB (web only)
  └─ Other: 350 KB (examples, docs, etc)

User has 70% bloat - things they'll never use
```

### After (v5.0)
```
Minimal User:
  npm install @unrdf/core
  150 KB ✅ No bloat

Typical User:
  npm install @unrdf/core @unrdf/hooks @unrdf/federation @unrdf/streaming
  340 KB ✅ Everything needed, nothing extra

Power User:
  npm install @unrdf/core @unrdf/knowledge-engine @unrdf/dark-matter
  500 KB ✅ Core + optional features, user's choice
```

**Result: 68-69% size reduction for typical users**

## Versioning Timeline

```
       v4.x (Stable)  v5.0-alpha  v5.0-rc   v5.0 (Stable)  v5.1+
         ↓               ↓          ↓          ↓              ↓
┌───────────────────────────────────────────────────────────────┐
│                                                               │
│  @unrdf/core:          ─────────────────────────→ 5.0.0+    │
│     (Stable, minimal changes)                                │
│                                                               │
│  @unrdf/hooks:         ─────────────────────────→ 5.0.0+    │
│     (Stable core, can extend via plugins)                    │
│                                                               │
│  @unrdf/knowledge-engine: ──→ 2.0.0-alpha → 2.0.0+         │
│     (Faster innovation, independent releases)               │
│                                                               │
│  @unrdf/dark-matter:   ──→ 1.0.0-alpha → 1.0.0+            │
│     (Optional, updates independently)                        │
│                                                               │
│  @unrdf/composables:   ──→ 1.0.0-alpha → 1.0.0+            │
│     (Updates with Vue.js changes)                            │
│                                                               │
└───────────────────────────────────────────────────────────────┘

Users can upgrade core at their own pace
while getting features from extensions immediately
```

---

These diagrams show:
1. **Architecture**: How packages relate
2. **Installation Paths**: What users install for their use case
3. **VOC Mapping**: Which VOCs use which packages
4. **Size Evolution**: Problem we solved
5. **Version Timeline**: Independent update cycles
