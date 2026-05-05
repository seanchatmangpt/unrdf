# UNRDF Monorepo Dependency Analysis

> Comprehensive analysis of the UNRDF workspace dependency structure,
> identifying risks, optimizations, and providing actionable guidance.

**Analysis Date:** 2025-12-26
**Analyzer Version:** 1.0.0
**Total Packages:** 71
**Total Dependencies:** 113 edges

---

## Executive Summary

The UNRDF monorepo demonstrates **healthy architecture** with no circular dependencies and a well-organized layer structure. The dependency graph forms a valid Directed Acyclic Graph (DAG), enabling predictable builds and clear ownership boundaries.

### Key Metrics

| Metric | Value | Status |
|--------|-------|--------|
| Total Packages | 71 | - |
| Dependency Edges | 113 | - |
| Circular Dependencies | 0 | PASS |
| Max Dependency Depth | 7 | OK |
| Overall Health Score | 90/100 | GOOD |
| Architecture Score | 86/100 | GOOD |

### Quick Assessment

- **No circular dependencies detected** - The dependency graph is acyclic
- **Package health is good** - Average health score of 90/100
- **Architecture is well-organized** - Clear layer boundaries with minimal violations
- **5 critical packages identified** - Require stability focus

---

## Package Architecture

### Layer Organization

The monorepo is organized into four architectural layers:

```
+------------------------------------------------------------------+
|                    APPLICATION LAYER (4 packages)                 |
|  CLI examples, playground applications, integration tests         |
+------------------------------------------------------------------+
                              |
                              v
+------------------------------------------------------------------+
|                      DOMAIN LAYER (24 packages)                   |
|  Feature packages: yawl-*, streaming, federation, consensus       |
+------------------------------------------------------------------+
                              |
                              v
+------------------------------------------------------------------+
|                       CORE LAYER (21 packages)                    |
|  Core functionality: core, hooks, kgc-4d, knowledge-engine        |
+------------------------------------------------------------------+
                              |
                              v
+------------------------------------------------------------------+
|                   FOUNDATION LAYER (22 packages)                  |
|  Leaf packages: oxigraph, graph-analytics, atomvm, observability  |
+------------------------------------------------------------------+
```

### Critical Packages (High Fan-In)

These packages are depended upon by many others. Changes require extra care:

| Package | Direct Dependents | Transitive Dependents | Risk Level |
|---------|-------------------|----------------------|------------|
| @unrdf/core | 36 | 49 | CRITICAL |
| @unrdf/oxigraph | 20 | 54 | CRITICAL |
| @unrdf/hooks | 10 | 34 | HIGH |
| @unrdf/kgc-4d | 10 | 14 | HIGH |
| @unrdf/yawl | 10 | 10 | HIGH |
| @unrdf/streaming | 7 | 17 | MEDIUM |
| @unrdf/knowledge-engine | 6 | 9 | MEDIUM |

**Recommendation:** These packages should have:
- Higher test coverage (95%+)
- Stricter code review
- Semantic versioning with API stability guarantees
- Careful deprecation processes

---

## Dependency Depth Analysis

The maximum dependency depth is 7, occurring in CLI example packages. Most packages fall within acceptable depth ranges.

### Depth Distribution

```
Depth 0 (Leaf):  22 packages  ########################################
Depth 1:         14 packages  ##########################
Depth 2:          8 packages  ###############
Depth 3:          7 packages  #############
Depth 4:          9 packages  #################
Depth 5:          5 packages  #########
Depth 6:          4 packages  ########
Depth 7:          2 packages  ####
```

### Deepest Dependency Chains

1. **cli-example-format-conversion** (depth 7)
   ```
   cli-example-format-conversion -> cli -> knowledge-engine -> streaming -> hooks -> core -> oxigraph
   ```

2. **cli-example-graph-commands** (depth 7)
   ```
   cli-example-graph-commands -> cli -> knowledge-engine -> streaming -> hooks -> core -> oxigraph
   ```

**Recommendation:** Consider flattening deep chains by:
- Extracting common functionality to shared packages
- Using dependency injection for optional features
- Evaluating if all transitive dependencies are truly needed

---

## Stability Analysis (Martin Metrics)

Using Robert Martin's stability metrics to assess package changeability:

- **Instability (I):** 0 = stable (hard to change), 1 = unstable (easy to change)
- **Abstractness (A):** Ratio of abstract elements to total elements
- **Distance from Main Sequence (D):** Measures architectural balance

### Stability Zones

| Zone | Packages | Description |
|------|----------|-------------|
| Balanced | 69 | Healthy balance of stability and flexibility |
| Zone of Pain | 0 | Concrete + Stable = Hard to modify |
| Zone of Uselessness | 0 | Abstract + Unstable = Unused abstractions |

### Packages Requiring Attention

| Package | Instability | Distance | Issue |
|---------|-------------|----------|-------|
| @unrdf/hooks | 0.57 | 0.43 | Moderate distance from main sequence |
| @unrdf/kgc-4d | 0.57 | 0.43 | Moderate distance from main sequence |

---

## Build Order (Topological Sort)

For correct dependency resolution, packages should be built in this order:

### Foundation Layer (Build First)
1. @unrdf/oxigraph
2. @unrdf/atomvm
3. @unrdf/graph-analytics
4. @unrdf/observability
5. @unrdf/diataxis-kit
6. @unrdf/yawl-ai (partial)

### Core Layer
7. @unrdf/core
8. @unrdf/test-utils
9. @unrdf/caching
10. @unrdf/semantic-search
11. @unrdf/dark-matter
12. @unrdf/hooks
13. @unrdf/kgc-4d

### Domain Layer
14. @unrdf/streaming
15. @unrdf/federation
16. @unrdf/yawl
17. @unrdf/knowledge-engine
18. @unrdf/composables
19. @unrdf/consensus
20. @unrdf/blockchain
... (remaining packages)

### Application Layer (Build Last)
- @unrdf/cli
- Integration tests
- Examples

---

## Optimization Recommendations

### Priority: HIGH

#### 1. Stabilize @unrdf/knowledge-engine

**Issue:** This package has 6 dependents but high instability (0.83).
Core packages should be stable.

**Actions:**
- Reduce dependencies on volatile packages
- Move implementation details to separate packages
- Increase test coverage to enable confident refactoring
- Consider semantic versioning with strict API compatibility

**Impact:** 8/10 | **Effort:** 6/10

### Priority: MEDIUM

#### 2. Extract Interfaces from High-Coupling Packages

Several packages have moderate coupling that could benefit from interface extraction:
- @unrdf/cli (7 dependencies)
- @unrdf/integration-tests (7 dependencies)

**Actions:**
- Define clear interfaces for public APIs
- Consider a `@unrdf/types` or `@unrdf/interfaces` package
- Use dependency inversion where appropriate

**Impact:** 6/10 | **Effort:** 5/10

#### 3. Consider Consolidating YAWL Ecosystem

The following packages share many dependencies and might represent a single domain:
- @unrdf/yawl
- @unrdf/yawl-api
- @unrdf/yawl-queue
- @unrdf/yawl-langchain
- @unrdf/yawl-observability

**Actions:**
- Create a meta-package that depends on all of them
- Consider merging if functionality overlaps
- Ensure clear boundaries between packages

**Impact:** 5/10 | **Effort:** 7/10

### Priority: LOW

#### 4. Remove Unused Dependencies

Run the analyzer with `--usage` flag to identify declared but unused workspace dependencies.

```bash
node src/dependency-analyzer/cli.mjs analyze --usage
```

---

## Change Impact Guide

Use this guide to understand the blast radius of changes to critical packages:

### @unrdf/core

**Impact if changed:** 49 packages (69% of monorepo)

```
Direct: 36 packages
  hooks, streaming, federation, composables, knowledge-engine,
  dark-matter, cli, project-engine, serverless, collab,
  ml-inference, integration-tests, examples, ...

Transitive: 13 additional packages
  yawl, consensus, validation, cli-examples, ...
```

**Guidelines for changes:**
1. All changes require thorough review
2. Breaking changes need major version bump
3. Deprecation period of at least 2 minor versions
4. Update all examples and documentation

### @unrdf/oxigraph

**Impact if changed:** 54 packages (76% of monorepo)

This is the most critical package in the monorepo. Handle with extreme care.

### Quick Impact Check

```bash
# Check impact of changing a specific package
node src/dependency-analyzer/cli.mjs impact @unrdf/core
```

---

## Health Monitoring

### Running the Analyzer

```bash
# Full analysis
node src/dependency-analyzer/cli.mjs analyze

# Quick health check (CI/CD)
node src/dependency-analyzer/cli.mjs health

# Generate GOS receipt
node src/dependency-analyzer/cli.mjs receipt --output receipt.json

# Export JSON for dashboards
node src/dependency-analyzer/cli.mjs analyze --format json --output analysis.json
```

### CI/CD Integration

Add to your CI pipeline:

```yaml
dependency-check:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - run: node src/dependency-analyzer/cli.mjs health
    - run: |
        node src/dependency-analyzer/cli.mjs receipt --output receipt.json
        if [ "$(cat receipt.json | jq -r '.type')" = "DENIAL" ]; then
          echo "Dependency analysis failed"
          exit 1
        fi
```

---

## Appendix: Full Package List

### Foundation Layer (22 packages)
- @unrdf/oxigraph
- @unrdf/atomvm
- @unrdf/graph-analytics
- @unrdf/observability
- @unrdf/diataxis-kit
- @unrdf/domain
- @unrdf/engine-gateway
- @unrdf/nextra
- @unrdf/yawl-ai (partial deps only)
- Various example packages (no workspace deps)

### Core Layer (21 packages)
- @unrdf/core
- @unrdf/hooks
- @unrdf/kgc-4d
- @unrdf/test-utils
- @unrdf/caching
- @unrdf/semantic-search
- @unrdf/dark-matter
- @unrdf/serverless
- @unrdf/collab
- Various internal packages

### Domain Layer (24 packages)
- @unrdf/streaming
- @unrdf/federation
- @unrdf/yawl
- @unrdf/knowledge-engine
- @unrdf/composables
- @unrdf/consensus
- @unrdf/blockchain
- @unrdf/ml-inference
- @unrdf/project-engine
- @unrdf/validation
- YAWL extension packages

### Application Layer (4 packages)
- @unrdf/cli
- Integration tests
- CLI examples
- Playground applications

---

## Version History

| Date | Version | Changes |
|------|---------|---------|
| 2025-12-26 | 1.0.0 | Initial analysis |

---

*Generated by UNRDF Dependency Analyzer v1.0.0*
