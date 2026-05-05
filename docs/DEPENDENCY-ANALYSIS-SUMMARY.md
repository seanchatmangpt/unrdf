# UNRDF Dependency Analysis - Executive Summary

**Date:** 2025-12-20
**Packages Analyzed:** 19
**Status:** 🟡 **MEDIUM RISK** - 1 circular dependency detected

---

## 🎯 Key Findings

### ✅ Strengths
- **Well-layered architecture** - Clear separation of concerns across 6 layers
- **Low coupling** - Only 1 hub package (`@unrdf/cli` with 5 deps)
- **Reasonable depth** - Maximum dependency chain is 5 levels
- **Good distribution** - 63% mid-level, 32% leaf, 5% hub packages

### ⚠️ Critical Issue
**ONE CIRCULAR DEPENDENCY DETECTED:**

```
@unrdf/core → (runtime) @unrdf/oxigraph
@unrdf/oxigraph ⇢ (dev) @unrdf/core
```

**Impact:** Affects 13/19 packages (68%) indirectly through dependency chains.

---

## 📊 Package Categorization

| Category | Count | Percentage | Packages |
|----------|-------|------------|----------|
| **Leaf** (0 deps) | 6 | latest% | validation, test-utils, domain, atomvm, nextra-docs, docs |
| **Mid-Level** (1-3 deps) | 12 | latest% | core, oxigraph, hooks, streaming, federation, etc. |
| **Hub** (4+ deps) | 1 | latest% | cli |

---

## 🔗 Dependency Chains (Top 5)

1. `cli → federation → hooks → core → oxigraph` (5 levels)
2. `composables → streaming → hooks → core → oxigraph` (5 levels)
3. `knowledge-engine → streaming → hooks → core → oxigraph` (5 levels)
4. `streaming → hooks → core → oxigraph` (4 levels)
5. `federation → hooks → core → oxigraph` (4 levels)

**Pattern:** All chains terminate at `@unrdf/oxigraph` (the RDF store layer).

---

## 🚨 Circular Dependency Details

### Root Cause
`@unrdf/oxigraph` uses `@unrdf/core` as a **devDependency** (likely for test utilities), creating a development-time circular dependency.

### Why This Matters for Unification
1. **Build order ambiguity** - Cannot use topological sort
2. **Version coordination complexity** - Which package updates first?
3. **Test isolation issues** - Circular imports in test environment
4. **CI/CD complications** - May hit build deadlocks

### Resolution Options

**RECOMMENDED: Extract Test Fixtures**
```bash
# Move shared test utilities from @unrdf/core to @unrdf/test-utils
# Update oxigraph devDependencies:
#   FROM: "@unrdf/core": "workspace:*"
#   TO:   "@unrdf/test-utils": "workspace:*"
```

**Alternative: Inline Test Data**
```bash
# Copy minimal test fixtures directly into oxigraph/test/*
# Remove devDependency on @unrdf/core
```

---

## 🛠️ Tools Installed

### 1. Circular Dependency Checker
```bash
pnpm run check:circular
```
**Output:**
```
❌ Found 2 circular dependency cycle(s):
1. @unrdf/core → (runtime) @unrdf/oxigraph ⇢ (dev) @unrdf/core
```

### 2. Dependency Graph Generator
```bash
pnpm run deps:graph        # Mermaid + ASCII + stats
pnpm run deps:stats        # Statistics only
```

### 3. Files Created
- `/Users/sac/unrdf/scripts/check-circular-deps.mjs` - Detection script
- `/Users/sac/unrdf/scripts/generate-dep-graph.mjs` - Visualization script
- `/Users/sac/unrdf/docs/DEPENDENCY-ANALYSIS-REPORT.md` - Full report (26KB)
- `/Users/sac/unrdf/docs/dependency-graph.md` - Mermaid diagram

---

## 📈 Statistics

| Metric | Value |
|--------|-------|
| Total packages | 19 |
| Total internal dependencies | 42 |
| Average deps per package | latest |
| Maximum dependency depth | 5 |
| Circular dependency clusters | 1 |

### Most Depended Upon
1. `@unrdf/core` - 12 packages depend on it (63%)
2. `@unrdf/oxigraph` - 6 packages depend on it (32%)
3. `@unrdf/hooks` - 3 packages depend on it (16%)
4. `@unrdf/streaming` - 3 packages depend on it (16%)

---

## ✅ Action Items

### Pre-Unification (CRITICAL)
- [ ] **Resolve circular dependency** (BLOCKER)
  - Audit `oxigraph/test/**/*.test.mjs` for core dependencies
  - Extract to `@unrdf/test-utils` or inline fixtures
  - Verify with `pnpm run check:circular`

- [ ] **Add CI check**
  ```yaml
  # .github/workflows/ci.yml
  - name: Check circular dependencies
    run: pnpm run check:circular
  ```

### During Unification
- [ ] Run `pnpm run check:circular` on every PR
- [ ] Monitor dependency depth (alert if > 6)
- [ ] Review any new hub packages (4+ deps)

### Post-Unification
- [ ] Monthly dependency graph review
- [ ] Refactor packages approaching hub status
- [ ] Document architectural boundaries

---

## 🎓 Recommendations

### REQUIRED Before Unification
1. ✅ **Circular dependency detection tool** - INSTALLED (`pnpm run check:circular`)
2. 🔴 **Resolve core ↔ oxigraph cycle** - NOT RESOLVED (see resolution options above)

### RECOMMENDED Best Practices
1. Enforce layered architecture (no upward dependencies)
2. Keep hub packages to minimum (currently 1, which is good)
3. Limit dependency chains to ≤ 5 levels (currently at limit)
4. Use `workspace:*` protocol for all internal deps (already in use ✅)

---

## 🔍 Visual Dependency Graph

See [`docs/dependency-graph.md`](/Users/sac/unrdf/docs/dependency-graph.md) for interactive Mermaid diagram.

**Color Legend:**
- 🟢 **Green (Leaf)** - No internal dependencies
- 🟠 **Orange (Mid-Level)** - 1-3 internal dependencies
- 🔴 **Red (Hub)** - 4+ internal dependencies

**Line Style:**
- **Solid arrow (→)** - Runtime dependency
- **Dashed arrow (⇢)** - DevDependency

---

## 📝 Quick Reference

```bash
# Check for circular dependencies
pnpm run check:circular

# Generate dependency graph
pnpm run deps:graph

# View statistics
pnpm run deps:stats

# Full analysis report
cat docs/DEPENDENCY-ANALYSIS-REPORT.md
```

---

## 🚦 Risk Assessment

| Risk | Level | Mitigation |
|------|-------|------------|
| Circular dependency impacts build | 🟡 Medium | Resolve before unification |
| Deep dependency chains | 🟢 Low | Already at acceptable level (5) |
| Hub package proliferation | 🟢 Low | Only 1 hub package (latest%) |
| Dependency graph complexity | 🟢 Low | Well-structured layers |

**Overall Risk:** 🟡 **MEDIUM** - Single circular dependency is resolvable but blocks unification.

---

## 📚 Related Documentation

- **Full Report:** [`DEPENDENCY-ANALYSIS-REPORT.md`](/Users/sac/unrdf/docs/DEPENDENCY-ANALYSIS-REPORT.md)
- **Mermaid Graph:** [`dependency-graph.md`](/Users/sac/unrdf/docs/dependency-graph.md)
- **Detection Script:** [`scripts/check-circular-deps.mjs`](/Users/sac/unrdf/scripts/check-circular-deps.mjs)

---

**Next Steps:** Resolve circular dependency → Add CI checks → Proceed with unification

**Priority:** 🔴 HIGH - Circular dependency is a BLOCKER for unification.
