# UNRDF Monorepo Dependency Analysis Report

**Generated:** 2025-12-20
**Packages Analyzed:** 19
**Total Internal Dependencies:** 42 (dependencies + devDependencies)

---

## 🚨 CRITICAL FINDING: Core ↔ Oxigraph Circular Dependency

### The Issue

**TRUE CIRCULAR DEPENDENCY DETECTED:**

```
@unrdf/core → @unrdf/oxigraph (runtime dependency)
@unrdf/oxigraph → @unrdf/core (devDependency for tests)
```

**Evidence:**

1. **`packages/core/package.json`** line 50:
   ```json
   "dependencies": {
     "@unrdf/oxigraph": "workspace:*"
   }
   ```

2. **`packages/oxigraph/package.json`** line 47:
   ```json
   "devDependencies": {
     "@unrdf/core": "workspace:*"
   }
   ```

### Impact Analysis

**This circular dependency cascades to 13 packages:**

| Package | Depth | Path to Circular Dependency |
|---------|-------|----------------------------|
| `@unrdf/cli` | 5 | cli → core → oxigraph → core |
| `@unrdf/knowledge-engine` | 5 | knowledge-engine → core → oxigraph → core |
| `@unrdf/composables` | 5 | composables → core → oxigraph → core |
| `@unrdf/streaming` | 4 | streaming → core → oxigraph → core |
| `@unrdf/federation` | 4 | federation → core → oxigraph → core |
| `@unrdf/hooks` | 3 | hooks → core → oxigraph → core |
| `@unrdf/kgc-4d` | 3 | kgc-4d → core → oxigraph → core |
| `@unrdf/engine-gateway` | 3 | engine-gateway → core → oxigraph → core |
| `@unrdf/kgn` | 3 | kgn → core → oxigraph → core |
| `@unrdf/dark-matter` | 3 | dark-matter → core → oxigraph → core |
| `@unrdf/project-engine` | 3 | project-engine → core → oxigraph → core |
| `@unrdf/core` | 2 | core → oxigraph → core |
| `@unrdf/oxigraph` | 2 | oxigraph → core → oxigraph |

**Severity:** 🔴 **HIGH** - This is a development-time circular dependency (not runtime), but impacts:
- Build order complexity
- Test isolation
- Package versioning
- Unification process

---

## 📊 Dependency Graph Visualization

### Full Dependency Tree

```
┌─────────────────────────────────────────────────────────────────────┐
│                          DEPENDENCY LAYERS                          │
└─────────────────────────────────────────────────────────────────────┘

LAYER 0 (Leaf Packages - No Internal Dependencies)
├── @unrdf/validation
├── @unrdf/test-utils
├── @unrdf/domain
├── @unrdf/atomvm
├── @unrdf/nextra-docs
└── docs

LAYER 1 (Foundation - Direct Leaf Dependencies)
└── @unrdf/oxigraph
    └── (external: oxigraph, zod)

LAYER 2 (Core Infrastructure)
├── @unrdf/core ────────────► @unrdf/oxigraph
│   └── (external: @rdfjs/*, n3, jsonld, zod)
│
├── @unrdf/dark-matter ─────► @unrdf/core
├── @unrdf/project-engine ──► @unrdf/core
├── @unrdf/kgn ─────────────► @unrdf/core, @unrdf/test-utils
├── @unrdf/kgc-4d ──────────► @unrdf/core, @unrdf/oxigraph
├── @unrdf/engine-gateway ──► @unrdf/core, @unrdf/oxigraph
└── @unrdf/hooks ───────────► @unrdf/core, @unrdf/oxigraph

LAYER 3 (Mid-Level Services)
├── @unrdf/federation ──────► @unrdf/core, @unrdf/hooks
└── @unrdf/streaming ───────► @unrdf/core, @unrdf/hooks, @unrdf/oxigraph

LAYER 4 (High-Level Applications)
├── @unrdf/knowledge-engine ► @unrdf/core, @unrdf/streaming
└── @unrdf/composables ─────► @unrdf/core, @unrdf/streaming

LAYER 5 (CLI - Integration Hub)
└── @unrdf/cli ─────────────► @unrdf/core, @unrdf/federation,
                               @unrdf/hooks, @unrdf/oxigraph,
                               @unrdf/streaming
```

---

## 🔍 Deep Dependency Chains

**Top 10 Longest Dependency Chains:**

| Rank | Chain | Length |
|------|-------|--------|
| 1 | `cli → federation → hooks → core → oxigraph` | 5 |
| 2 | `composables → streaming → hooks → core → oxigraph` | 5 |
| 3 | `knowledge-engine → streaming → hooks → core → oxigraph` | 5 |
| 4 | `streaming → hooks → core → oxigraph` | 4 |
| 5 | `federation → hooks → core → oxigraph` | 4 |
| 6 | `hooks → core → oxigraph` | 3 |
| 7 | `kgc-4d → core → oxigraph` | 3 |
| 8 | `engine-gateway → core → oxigraph` | 3 |
| 9 | `kgn → core → oxigraph` | 3 |
| 10 | `project-engine → core → oxigraph` | 3 |

**All chains terminate at `@unrdf/oxigraph`** - the foundational RDF store layer.

---

## 📦 Package Categorization

### Leaf Packages (0 internal dependencies)

**Count:** 6 packages (31.6%)

- `@unrdf/validation` - Validation utilities
- `@unrdf/test-utils` - Test helpers
- `@unrdf/domain` - Domain models
- `@unrdf/atomvm` - AtomVM experiments
- `@unrdf/nextra-docs` - Documentation site
- `docs` - General documentation

**Role:** Independent utilities, no impact on core dependency graph.

---

### Mid-Level Packages (1-3 internal dependencies)

**Count:** 12 packages (63.2%)

| Package | Internal Deps | Direct Dependencies |
|---------|---------------|---------------------|
| `@unrdf/core` | 1 | `@unrdf/oxigraph` |
| `@unrdf/oxigraph` | 1 | `@unrdf/core` (devDep) ⚠️ |
| `@unrdf/dark-matter` | 1 | `@unrdf/core` |
| `@unrdf/project-engine` | 1 | `@unrdf/core` |
| `@unrdf/kgn` | 2 | `@unrdf/core`, `@unrdf/test-utils` |
| `@unrdf/kgc-4d` | 2 | `@unrdf/core`, `@unrdf/oxigraph` |
| `@unrdf/engine-gateway` | 2 | `@unrdf/core`, `@unrdf/oxigraph` |
| `@unrdf/hooks` | 2 | `@unrdf/core`, `@unrdf/oxigraph` |
| `@unrdf/federation` | 2 | `@unrdf/core`, `@unrdf/hooks` |
| `@unrdf/knowledge-engine` | 2 | `@unrdf/core`, `@unrdf/streaming` |
| `@unrdf/composables` | 2 | `@unrdf/core`, `@unrdf/streaming` |
| `@unrdf/streaming` | 3 | `@unrdf/core`, `@unrdf/hooks`, `@unrdf/oxigraph` |

**Role:** Core infrastructure and specialized services.

---

### Hub Packages (4+ internal dependencies)

**Count:** 1 package (5.3%)

| Package | Internal Deps | Direct Dependencies |
|---------|---------------|---------------------|
| `@unrdf/cli` | 5 | `@unrdf/core`, `@unrdf/federation`, `@unrdf/hooks`, `@unrdf/oxigraph`, `@unrdf/streaming` |

**Role:** Integration point - CLI depends on multiple subsystems.

**Analysis:** This is the ONLY hub package. It's an appropriate integration layer.

---

## 🎯 Dependency Depth Analysis

**Maximum Depth:** 5 levels (cli, knowledge-engine, composables)

| Depth | Packages |
|-------|----------|
| **Depth 0** | validation, test-utils, nextra-docs, domain, docs, atomvm (6) |
| **Depth 1** | *(none)* |
| **Depth 2** | oxigraph, core (2) |
| **Depth 3** | project-engine, kgn, kgc-4d, hooks, engine-gateway, dark-matter (6) |
| **Depth 4** | streaming, federation (2) |
| **Depth 5** | knowledge-engine, composables, cli (3) |

**Distribution:** Depth is well-distributed, but circular dependency at depth 2 affects all higher levels.

---

## 📈 Dependency Statistics

### Overall Metrics

| Metric | Value |
|--------|-------|
| Total packages | 19 |
| Total internal dependencies | 42 (deps + devDeps) |
| Average dependencies per package | 2.21 |
| Packages with 0 internal deps | 6 (31.6%) |
| Packages with 1-3 internal deps | 12 (63.2%) |
| Packages with 4+ internal deps | 1 (5.3%) |
| Maximum dependency chain length | 5 |
| Circular dependency clusters | 1 (core ↔ oxigraph) |

### Dependency Direction (Most Depended Upon)

| Package | Depended Upon By | Count |
|---------|------------------|-------|
| `@unrdf/core` | cli, streaming, composables, knowledge-engine, federation, hooks, kgc-4d, engine-gateway, kgn, dark-matter, project-engine, *oxigraph (devDep)* | **12** |
| `@unrdf/oxigraph` | core, streaming, cli, hooks, kgc-4d, engine-gateway | **6** |
| `@unrdf/hooks` | streaming, federation, cli | **3** |
| `@unrdf/streaming` | composables, knowledge-engine, cli | **3** |
| `@unrdf/test-utils` | kgn | **1** |
| `@unrdf/federation` | cli | **1** |

**Key Insight:** `@unrdf/core` is the central dependency - 12/19 packages depend on it.

---

## ⚠️ CIRCULAR DEPENDENCY DETECTION TOOL RECOMMENDATION

### Do We Need a Tool?

**YES - STRONGLY RECOMMENDED for unification process.**

### Rationale

1. **Current Circular Dependency:**
   - Core ↔ Oxigraph circular dep exists NOW
   - Affects 13/19 packages (68%)
   - Will complicate build/test order during unification

2. **Future Risk During Unification:**
   - Merging packages can accidentally introduce new cycles
   - Manual tracking across 19 packages is error-prone
   - Need automated gates in CI/CD

3. **Impact on Unification:**
   - Build order becomes non-deterministic
   - Cannot guarantee clean package boundary separation
   - Version bumping becomes complex (which package updates first?)

### Recommended Tools

#### Option 1: `madge` (Recommended)
```bash
npm install -g madge

# Detect circular dependencies
madge --circular --extensions mjs packages/*/src

# Generate visual dependency graph
madge --image deps.svg packages/*/src

# Fail CI on circular deps
madge --circular --extensions mjs packages/*/src || exit 1
```

**Pros:**
- Zero config for basic usage
- Visual graph generation
- CI integration
- Fast (~1-2s for this codebase)

**Cons:**
- Runtime imports only (won't catch devDependencies circle)

#### Option 2: `dependency-cruiser`
```bash
npm install --save-dev dependency-cruiser

# Run with config
depcruise --config .dependency-cruiser.js packages/
```

**Pros:**
- Catches both runtime AND devDependency cycles
- Highly configurable rules
- Can enforce architectural boundaries
- Generates detailed reports

**Cons:**
- Requires configuration file
- Slower than madge (~5-10s)

#### Option 3: Custom pnpm Workspace Check
```bash
# Add to package.json scripts
{
  "scripts": {
    "check:circular": "node scripts/check-circular-deps.mjs"
  }
}
```

**Implementation:** Parse `package.json` files, build graph, detect cycles with DFS.

**Pros:**
- Catches package.json devDependency cycles (current issue!)
- No external dependencies
- Fast (~500ms)
- Can customize to workspace structure

**Cons:**
- Requires maintenance
- Doesn't analyze runtime imports

### RECOMMENDED APPROACH

**Use BOTH:**
1. **Custom pnpm script** - Detect package.json circular deps (catches current core ↔ oxigraph issue)
2. **madge** - Detect runtime import cycles (future-proofs against code-level cycles)

**Add to CI:**
```yaml
# .github/workflows/ci.yml
- name: Check circular dependencies
  run: |
    pnpm run check:circular
    npx madge --circular --extensions mjs packages/*/src
```

---

## 🔧 RESOLUTION STRATEGY: Core ↔ Oxigraph Circular Dependency

### Root Cause

**Why does oxigraph devDep on core?**
- Likely for testing: oxigraph tests need core's RDF utilities
- This creates dev-time circular dependency

### Resolution Options

#### Option 1: Extract Test Fixtures (Recommended)
```
BEFORE:
  @unrdf/oxigraph (devDep) → @unrdf/core → @unrdf/oxigraph

AFTER:
  @unrdf/oxigraph (devDep) → @unrdf/test-utils (shared fixtures)
  @unrdf/core → @unrdf/oxigraph
```

**Action:**
- Move shared test utilities from `@unrdf/core` to `@unrdf/test-utils`
- Update `oxigraph/test/*` to use test-utils instead of core

#### Option 2: Inline Test Data
```
AFTER:
  @unrdf/core → @unrdf/oxigraph
  @unrdf/oxigraph (no devDep on core)
```

**Action:**
- Copy minimal test fixtures directly into oxigraph tests
- Remove devDependency on core

#### Option 3: Reverse Dependency (NOT Recommended)
```
AFTER:
  @unrdf/oxigraph → @unrdf/core (runtime)
```

**Why NOT:** Core SHOULD depend on oxigraph (layered architecture), not vice versa.

### Recommended Action Plan

1. **Audit:** Identify WHY oxigraph tests need core (grep test files)
2. **Extract:** Move shared test fixtures to `@unrdf/test-utils`
3. **Update:** Change oxigraph devDep from `@unrdf/core` to `@unrdf/test-utils`
4. **Verify:** Run `pnpm run check:circular` to confirm resolution
5. **Document:** Update architecture docs with dependency rules

---

## 📋 UNIFICATION IMPACT ASSESSMENT

### Build Order Complexity

**Current state:** Build order is ambiguous due to core ↔ oxigraph cycle.

**Impact on unification:**
- Cannot use topological sort for build order
- May hit "waiting for dependency" deadlocks
- Package version updates require manual coordination

### Recommended Build Order (After Cycle Resolution)

```
1. LAYER 0: validation, test-utils, domain, atomvm, nextra-docs, docs
2. LAYER 1: oxigraph
3. LAYER 2: core
4. LAYER 3: dark-matter, project-engine, kgn, kgc-4d, engine-gateway, hooks
5. LAYER 4: streaming, federation
6. LAYER 5: knowledge-engine, composables
7. LAYER 6: cli
```

### Version Bump Strategy

**Current issue:** If core updates, oxigraph devDep must update. But oxigraph is a dep of core!

**Solution (after cycle resolution):**
1. Bump leaf packages first (layer 0)
2. Bump up the dependency tree (layers 1-6)
3. Use `pnpm --filter` to control order

---

## ✅ ACTION ITEMS

### Immediate (Pre-Unification)

- [ ] **Install circular dependency detection tool**
  - Choice: Custom script + madge
  - Add to CI pipeline
  - Set up pre-commit hook

- [ ] **Resolve core ↔ oxigraph circular dependency**
  - Audit oxigraph test dependencies on core
  - Extract shared test utilities to `@unrdf/test-utils`
  - Update oxigraph devDependencies
  - Verify with `check:circular` script

- [ ] **Document dependency rules**
  - Add architecture decision record (ADR)
  - Define allowed dependency directions
  - Set up linting rules to enforce

### During Unification

- [ ] **Automated checks on every PR**
  - Run circular dependency check
  - Block merge if cycles detected
  - Generate dependency graph on changes

- [ ] **Monitor dependency depth**
  - Alert if new package creates depth > 6
  - Review hub packages (4+ deps) carefully

- [ ] **Version coordination**
  - Use workspace protocol (`workspace:*`)
  - Automate cross-package version bumps
  - Test build order in CI

### Post-Unification

- [ ] **Regular audits**
  - Monthly dependency graph review
  - Identify new circular dependencies early
  - Refactor packages approaching hub status (4+ deps)

---

## 📊 APPENDIX: Full Dependency Matrix

| Package | Dependencies (runtime) | DevDependencies (test) |
|---------|------------------------|------------------------|
| `@unrdf/cli` | core, federation, hooks, oxigraph, streaming | - |
| `@unrdf/streaming` | core, hooks, oxigraph | - |
| `@unrdf/composables` | core, streaming | - |
| `@unrdf/knowledge-engine` | core, streaming | - |
| `@unrdf/federation` | core, hooks | - |
| `@unrdf/hooks` | core, oxigraph | - |
| `@unrdf/kgc-4d` | core, oxigraph | - |
| `@unrdf/engine-gateway` | core, oxigraph | - |
| `@unrdf/kgn` | core, test-utils | - |
| `@unrdf/core` | **oxigraph** ⚠️ | - |
| `@unrdf/oxigraph` | - | **core** ⚠️ |
| `@unrdf/project-engine` | core | - |
| `@unrdf/dark-matter` | core | - |
| `@unrdf/validation` | - | - |
| `@unrdf/test-utils` | - | - |
| `@unrdf/domain` | - | - |
| `@unrdf/atomvm` | - | - |
| `@unrdf/nextra-docs` | - | - |
| `docs` | - | - |

⚠️ **Circular dependency:** core ↔ oxigraph

---

## 🎯 CONCLUSION

### Summary

The UNRDF monorepo has:
- ✅ **Good overall structure** - Well-layered architecture with clear separation
- ✅ **Single hub package** - CLI appropriately aggregates subsystems
- ✅ **Reasonable depth** - Max depth of 5 is acceptable
- ⚠️ **One critical issue** - Core ↔ Oxigraph circular dependency

### Critical Path for Unification

1. **Resolve circular dependency** (BLOCKER)
2. **Install detection tooling** (PREVENT regression)
3. **Define build order** (ENABLE automated builds)
4. **Automate version coordination** (REDUCE manual errors)

### Risk Level

**Overall Risk: 🟡 MEDIUM**
- Current circular dep is dev-time only (not runtime)
- Affects 68% of packages (13/19) indirectly
- Resolvable with test fixture extraction
- Must resolve BEFORE unification to avoid build order deadlocks

---

**Report Generated by:** UNRDF Dependency Analysis
**Analysis Method:** Static package.json parsing + DFS cycle detection
**Code Review Status:** Ready for architectural review
