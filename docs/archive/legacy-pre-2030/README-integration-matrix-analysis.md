# Integration Matrix Analysis Scripts

Automated analysis tools for exploring cross-package integration complexity in the UNRDF monorepo.

## Scripts

### 1. `analyze-integration-matrix.mjs`
**Purpose**: Analyzes package dependency graph and layer interactions

**Output**:
- Dependency graph statistics (nodes, edges, depth)
- Hub analysis (most connected packages)
- Layer interaction heat map
- Circular dependency detection
- API surface complexity
- Integration matrix explosion metrics

**Usage**:
```bash
node scripts/analyze-integration-matrix.mjs
```

**Runtime**: ~3 seconds

---

### 2. `analyze-import-patterns.mjs`
**Purpose**: Analyzes actual import statements to understand function-level integration

**Output**:
- Cross-package import statistics
- Most common integration patterns
- Most frequently imported functions
- Integration diversity metrics

**Usage**:
```bash
node scripts/analyze-import-patterns.mjs
```

**Runtime**: ~5 seconds (analyzes 906 source files)

---

### 3. `analyze-layer-violations.mjs`
**Purpose**: Detects architectural layer violations and version compatibility issues

**Output**:
- Layer violation statistics (upward, cross-domain)
- Critical violations (severity-ranked)
- Version compatibility matrix
- Version distribution analysis

**Usage**:
```bash
node scripts/analyze-layer-violations.mjs
```

**Runtime**: ~2 seconds

---

### 4. `visualize-integration-matrix.mjs`
**Purpose**: ASCII visualization of integration matrix complexity

**Output**:
- Hub-and-spoke topology diagram
- Layer dependency flow diagram
- Integration pattern heat map
- Critical API dependencies chart
- Circular dependency chain diagram
- Complexity metrics summary
- Key findings and recommendations

**Usage**:
```bash
node scripts/visualize-integration-matrix.mjs
```

**Runtime**: Instant (displays pre-generated visualization)

---

## Running All Analyses

```bash
# Run all analyses in sequence
node scripts/analyze-integration-matrix.mjs
node scripts/analyze-import-patterns.mjs
node scripts/analyze-layer-violations.mjs
node scripts/visualize-integration-matrix.mjs

# Or with timeout protection
timeout 30s node scripts/analyze-integration-matrix.mjs && \
timeout 30s node scripts/analyze-import-patterns.mjs && \
timeout 30s node scripts/analyze-layer-violations.mjs && \
node scripts/visualize-integration-matrix.mjs
```

---

## Analysis Results

Full analysis report: `/home/user/unrdf/docs/research/cross-package-integration-matrix.md`

### Key Findings Summary

**Package Count**: 58 packages
**Dependency Edges**: 133 (3.95% of 3,364 potential combinations)
**Max Dependency Depth**: 7 levels
**Circular Dependencies**: 1 cycle (kgc-multiverse ↔ receipts)
**Layer Violations**: 14 (1 critical)

**Hub Packages**:
- `oxigraph`: 30 dependents (51.7% of packages)
- `core`: 28 dependents (48.3% of packages)
- `kgc-4d`: 20 dependents (34.5% of packages)
- `yawl`: 13 dependents (22.4% of packages)

**Critical APIs** (most imported functions):
1. `now()` - 64 imports
2. `createStore()` - 51 imports
3. `dataFactory` - 51 imports
4. `toISO()` - 34 imports
5. `VectorClock` - 16 imports

**Architecture Grade**: B+ (Good)

---

## Methodology

### Data Sources
1. **Package.json files** (58 files) - dependency declarations
2. **Source files** (906 .mjs files) - actual import statements
3. **Layer definitions** (from CLAUDE.md) - architectural layer mapping

### Analysis Techniques
- **Graph theory**: Degree analysis, depth calculation, cycle detection
- **Static analysis**: Import regex extraction, function name tracking
- **Layer validation**: Hierarchy comparison, violation severity ranking
- **Version analysis**: Version distribution, compatibility matrix calculation

### Confidence Level
**HIGH** - Based on empirical code analysis with:
- Automated parsing of 58 package.json files
- Regex extraction of 140 import statements from 906 source files
- Zero manual assumptions (all data-driven)

---

## Maintenance

### When to Re-run
- After adding/removing packages
- After major dependency changes
- During architecture reviews
- Before major version releases
- Quarterly (recommended)

### Updating Layer Definitions
Edit layer mappings in each script (search for `const LAYERS = {`):
- `analyze-integration-matrix.mjs` (lines 15-28)
- `analyze-layer-violations.mjs` (lines 15-28)

---

## Future Enhancements

Potential additions:
- [ ] GraphML/DOT export for visualization tools
- [ ] Breaking change impact analysis
- [ ] Dependency upgrade simulation
- [ ] Performance regression correlation
- [ ] Historical trend tracking (git history integration)
- [ ] Automated violation PR comments

---

**Created**: 2026-01-11
**Author**: Research Agent (Claude Code)
**Last Updated**: 2026-01-11
