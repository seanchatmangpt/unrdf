# Dimension Certificate Specification

## Multi-Package Measurement Framework for UNRDF Monorepo

**Version**: 1.0.0
**Status**: Active
**Last Updated**: 2024-12-26

---

## 1. Introduction

This specification defines the measurement framework that instruments the 43-package UNRDF monorepo. The framework extends the single-package S_t model to a multi-package context, enabling comprehensive observability across all layers of the system.

### 1.1 Purpose

The Dimension Certificate provides a cryptographically verifiable proof of system state at any point in time. It answers the fundamental question: **"What is the entropy of this codebase, and how much change capacity remains?"**

### 1.2 Scope

This specification covers:
- Mathematical definitions for multi-package entropy measurement
- Computation algorithms for D_t, TC, TE, and C_t
- Certificate schema and validation methodology
- Rollup rules for aggregating measurements across packages

---

## 2. Mathematical Foundation

### 2.1 Extended System State Model

The system state S_t at epoch t extends the single-package model to multi-package context:

```
S_t = <O_core, O_packages, Pi_gen, Lambda_order, Q_inv, H_guard, R_receipts, U_traces>
```

Where:
- **O_core**: Core package state partitions
- **O_packages**: Map of package_id to partition state O_pkg
- **Pi_gen**: Generative policy rules (cross-package)
- **Lambda_order**: Partial ordering constraints between packages
- **Q_inv**: Invariant predicates that must hold globally
- **H_guard**: Guard conditions at package boundaries
- **R_receipts**: Cryptographic receipts of state transitions
- **U_traces**: OTEL traces linking operations across packages

### 2.2 Package Partition Model

Each package p_i is a partition with four canonical sub-partitions:

```
O_pkg = {O_src, O_tests, O_docs, O_config}
```

Where:
- **O_src**: Source code files (*.mjs, *.ts)
- **O_tests**: Test files (*.test.mjs, *.spec.mjs)
- **O_docs**: Documentation (*.md, *.mdx)
- **O_config**: Configuration (package.json, *.config.mjs)

### 2.3 Entropy Dimensions

#### 2.3.1 Structural Entropy (D_t)

The total system entropy D_t is the sum of per-package entropies weighted by coupling factor:

```
D_t = sum(i=1 to N) w_i * D_t(p_i) + D_cross
```

Where:
- **N**: Total number of packages (43 for UNRDF)
- **w_i**: Weight factor for package p_i based on criticality
- **D_t(p_i)**: Entropy of package p_i
- **D_cross**: Cross-package entropy from coupling

Per-package entropy is computed as:

```
D_t(p_i) = sum(j in {src,tests,docs,config}) H(O_j) * alpha_j
```

Where:
- **H(O_j)**: Shannon entropy of partition j
- **alpha_j**: Partition weight (alpha_src=0.4, alpha_tests=0.3, alpha_docs=0.2, alpha_config=0.1)

#### 2.3.2 Temporal Coupling (TC)

Temporal coupling TC measures how changes in one package correlate with changes in others:

```
TC(p_i, p_j) = |commits_containing(p_i) intersection commits_containing(p_j)| /
               |commits_containing(p_i) union commits_containing(p_j)|
```

The system-wide TC matrix is a 43x43 symmetric matrix where:
- **TC(p_i, p_i) = 1.0** (self-coupling)
- **TC(p_i, p_j) in [0, 1]** (cross-coupling)

High TC indicates packages that frequently change together and should be treated as a unit.

#### 2.3.3 Transitive Entropy (TE)

Transitive entropy TE measures policy propagation through the dependency graph:

```
TE(p_i) = H(p_i) + beta * sum(p_j in deps(p_i)) TE(p_j)
```

Where:
- **beta**: Decay factor (0.7 recommended)
- **deps(p_i)**: Direct dependencies of package p_i

For policy-influencing packages (core, hooks, admission), TE captures how changes ripple through the system.

#### 2.3.4 Change Capacity (C_t)

Change capacity C_t is the remaining entropy budget before system complexity becomes unmanageable:

```
C_t(p_i) = C_max(p_i) - D_t(p_i)
```

Where:
- **C_max(p_i)**: Maximum allowed entropy for package p_i (tier-dependent)
- **D_t(p_i)**: Current entropy

System-wide capacity:

```
C_t = sum(i=1 to N) C_t(p_i) / N
```

---

## 3. Package Classification

### 3.1 Tier Definitions

Packages are classified into tiers based on their role and entropy budget:

| Tier | Description | C_max | Examples |
|------|-------------|-------|----------|
| **T1 (Core)** | Foundational packages | 24 bits | core, oxigraph, validation |
| **T2 (Infrastructure)** | Platform services | 20 bits | hooks, streaming, observability |
| **T3 (Application)** | Business logic | 18 bits | kgc-4d, yawl, federation |
| **T4 (Integration)** | External adapters | 16 bits | yawl-kafka, yawl-langchain |
| **T5 (Documentation)** | Docs and examples | 12 bits | docs, nextra, diataxis-kit |

### 3.2 Package Registry

The 43 packages are assigned as follows:

**Tier 1 (Core)** - 5 packages:
- `core`, `oxigraph`, `validation`, `domain`, `test-utils`

**Tier 2 (Infrastructure)** - 8 packages:
- `hooks`, `streaming`, `observability`, `caching`, `consensus`, `engine-gateway`, `project-engine`, `serverless`

**Tier 3 (Application)** - 12 packages:
- `kgc-4d`, `kgn`, `yawl`, `federation`, `graph-analytics`, `knowledge-engine`, `semantic-search`, `collab`, `composables`, `blockchain`, `ml-inference`, `ml-versioning`

**Tier 4 (Integration)** - 12 packages:
- `yawl-ai`, `yawl-api`, `yawl-durable`, `yawl-kafka`, `yawl-langchain`, `yawl-observability`, `yawl-queue`, `yawl-realtime`, `yawl-viz`, `rdf-graphql`, `react`, `cli`

**Tier 5 (Documentation)** - 6 packages:
- `docs`, `nextra`, `diataxis-kit`, `dark-matter`, `atomvm`, `integration-tests`

---

## 4. Computation Algorithms

### 4.1 Per-Package Feature Extraction

For each package p_i, extract the following features:

```javascript
function extractFeatures(package_path) {
  return {
    // File counts
    src_file_count: countFiles(package_path, 'src/**/*.mjs'),
    test_file_count: countFiles(package_path, '**/*.test.mjs'),
    doc_file_count: countFiles(package_path, '**/*.md'),

    // Code metrics
    total_lines: sumLines(package_path, 'src/**/*.mjs'),
    avg_file_length: avgLines(package_path, 'src/**/*.mjs'),
    max_file_length: maxLines(package_path, 'src/**/*.mjs'),

    // Complexity metrics
    function_count: countExports(package_path),
    cyclomatic_complexity: avgCyclomaticComplexity(package_path),

    // Dependency metrics
    internal_deps: countInternalDeps(package_path),
    external_deps: countExternalDeps(package_path),

    // Test coverage
    test_coverage_percent: getCoverage(package_path),
    test_pass_rate: getPassRate(package_path)
  };
}
```

### 4.2 Entropy Computation

Per-package entropy uses the Shannon formula:

```javascript
function computeEntropy(features) {
  const { src_file_count, total_lines, function_count, internal_deps } = features;

  // Normalize to probability distribution
  const p_files = src_file_count / MAX_FILES;
  const p_lines = total_lines / MAX_LINES;
  const p_funcs = function_count / MAX_FUNCS;
  const p_deps = internal_deps / MAX_DEPS;

  // Shannon entropy
  const H_files = p_files > 0 ? -p_files * Math.log2(p_files) : 0;
  const H_lines = p_lines > 0 ? -p_lines * Math.log2(p_lines) : 0;
  const H_funcs = p_funcs > 0 ? -p_funcs * Math.log2(p_funcs) : 0;
  const H_deps = p_deps > 0 ? -p_deps * Math.log2(p_deps) : 0;

  // Weighted sum
  return 0.3 * H_files + 0.3 * H_lines + 0.25 * H_funcs + 0.15 * H_deps;
}
```

### 4.3 Rollup Aggregation

System-wide metrics are computed by aggregating package metrics:

```javascript
function rollupToSystem(packageMeasurements) {
  const N = packageMeasurements.length;

  // Total entropy
  const D_t = packageMeasurements.reduce((sum, pm) => {
    const weight = TIER_WEIGHTS[pm.tier];
    return sum + weight * pm.D_t;
  }, 0);

  // Average capacity
  const C_t = packageMeasurements.reduce((sum, pm) => sum + pm.C_t, 0) / N;

  // TC matrix (43x43)
  const TC = computeTCMatrix(packageMeasurements);

  // TE via topological sort
  const TE = computeTETransitive(packageMeasurements);

  return { D_t, C_t, TC, TE, timestamp: Date.now() };
}
```

---

## 5. Certificate Schema

### 5.1 JSON Schema Definition

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "https://unrdf.dev/schemas/dimension-certificate.json",
  "title": "UNRDF Dimension Certificate",
  "type": "object",
  "required": ["version", "timestamp", "epoch", "checksum", "measurements", "signature"],
  "properties": {
    "version": {
      "type": "string",
      "pattern": "^\\d+\\.\\d+\\.\\d+$"
    },
    "timestamp": {
      "type": "string",
      "format": "date-time"
    },
    "epoch": {
      "type": "integer",
      "minimum": 0
    },
    "checksum": {
      "type": "string",
      "pattern": "^sha256:[a-f0-9]{64}$"
    },
    "measurements": {
      "type": "object",
      "properties": {
        "system": { "$ref": "#/definitions/systemMeasurement" },
        "packages": {
          "type": "array",
          "items": { "$ref": "#/definitions/packageMeasurement" }
        }
      }
    },
    "signature": {
      "type": "string"
    }
  },
  "definitions": {
    "systemMeasurement": {
      "type": "object",
      "properties": {
        "D_t": { "type": "number", "minimum": 0 },
        "C_t": { "type": "number" },
        "TC_density": { "type": "number", "minimum": 0, "maximum": 1 },
        "TE_max": { "type": "number", "minimum": 0 },
        "package_count": { "type": "integer" }
      }
    },
    "packageMeasurement": {
      "type": "object",
      "required": ["name", "tier", "D_t", "C_t", "features"],
      "properties": {
        "name": { "type": "string" },
        "tier": { "type": "integer", "minimum": 1, "maximum": 5 },
        "D_t": { "type": "number", "minimum": 0 },
        "C_t": { "type": "number" },
        "TC": { "type": "array", "items": { "type": "number" } },
        "TE": { "type": "number", "minimum": 0 },
        "features": { "$ref": "#/definitions/packageFeatures" }
      }
    },
    "packageFeatures": {
      "type": "object",
      "properties": {
        "src_file_count": { "type": "integer" },
        "test_file_count": { "type": "integer" },
        "total_lines": { "type": "integer" },
        "function_count": { "type": "integer" },
        "internal_deps": { "type": "integer" },
        "external_deps": { "type": "integer" },
        "test_coverage_percent": { "type": "number" },
        "cyclomatic_complexity": { "type": "number" }
      }
    }
  }
}
```

### 5.2 Example Certificate

A certificate for a system with D_t = 18.79 bits would look like:

```json
{
  "version": "1.0.0",
  "timestamp": "2024-12-26T12:00:00.000Z",
  "epoch": 1735214400000,
  "checksum": "sha256:a1b2c3d4e5f6...",
  "measurements": {
    "system": {
      "D_t": 18.79,
      "C_t": 5.21,
      "TC_density": 0.23,
      "TE_max": 42.3,
      "package_count": 43
    },
    "packages": [
      {
        "name": "core",
        "tier": 1,
        "D_t": 4.82,
        "C_t": 19.18,
        "TC": [1.0, 0.87, 0.45, ...],
        "TE": 42.3,
        "features": {
          "src_file_count": 21,
          "test_file_count": 18,
          "total_lines": 4500,
          "function_count": 156,
          "internal_deps": 0,
          "external_deps": 12,
          "test_coverage_percent": 94.2,
          "cyclomatic_complexity": 8.3
        }
      }
      // ... 42 more packages
    ]
  },
  "signature": "ed25519:..."
}
```

### 5.3 Interpretation Guide

For a system with D_t = 18.79 bits across 43 packages:

1. **Per-package average**: 18.79 / 43 = 0.437 bits per package
2. **Interpretation**: Low average entropy indicates well-factored codebase
3. **Distribution matters**: Check for outliers (packages with D_t > 3.0 bits)

Entropy thresholds:
- **D_t < 10 bits**: Low complexity, high change capacity
- **D_t = 10-20 bits**: Moderate complexity, balanced state
- **D_t = 20-30 bits**: High complexity, limited change capacity
- **D_t > 30 bits**: Critical complexity, requires refactoring

---

## 6. Verification Methodology

### 6.1 Reproducibility Checks

To verify a certificate, re-compute measurements and compare checksums:

```javascript
async function verifyCertificate(certificate) {
  // Step 1: Re-extract features
  const packages = await Promise.all(
    PACKAGE_NAMES.map(name => extractFeatures(`packages/${name}`))
  );

  // Step 2: Re-compute measurements
  const measurements = packages.map(computeMeasurement);

  // Step 3: Compute checksum
  const payload = JSON.stringify({ measurements });
  const checksum = `sha256:${crypto.createHash('sha256').update(payload).digest('hex')}`;

  // Step 4: Compare
  if (checksum !== certificate.checksum) {
    return { valid: false, reason: 'Checksum mismatch' };
  }

  // Step 5: Verify signature
  const signatureValid = verifySignature(certificate);

  return { valid: signatureValid, checksum };
}
```

### 6.2 Invariant Assertions

The following invariants must hold for any valid certificate:

1. **Completeness**: All 43 packages must be measured
2. **Tier Consistency**: Each package's tier matches the registry
3. **Entropy Bounds**: D_t(p_i) in [0, C_max(tier)]
4. **Capacity Non-negative**: C_t(p_i) >= 0
5. **TC Symmetry**: TC(p_i, p_j) == TC(p_j, p_i)
6. **TC Diagonal**: TC(p_i, p_i) == 1.0

### 6.3 Validation Pipeline

```
Input: Git repository state
  |
  v
[Feature Extraction] --> For each package
  |
  v
[Entropy Computation] --> D_t per package
  |
  v
[TC Matrix Construction] --> 43x43 coupling matrix
  |
  v
[TE Propagation] --> Transitive closure
  |
  v
[Rollup Aggregation] --> System-wide metrics
  |
  v
[Certificate Generation] --> JSON + checksum + signature
  |
  v
[Invariant Verification] --> All 6 assertions pass
  |
  v
Output: Dimension Certificate (valid/invalid)
```

---

## 7. Integration with Admission Control

### 7.1 Pre-Commit Hook

Before any commit is admitted, check change capacity:

```javascript
function preCommitAdmission(changedFiles, certificate) {
  const affectedPackages = detectAffectedPackages(changedFiles);

  for (const pkg of affectedPackages) {
    const measurement = certificate.measurements.packages.find(p => p.name === pkg);

    // Estimate entropy change
    const deltaD = estimateEntropyDelta(changedFiles.filter(f => f.startsWith(`packages/${pkg}`)));

    if (deltaD > measurement.C_t) {
      return {
        admitted: false,
        reason: `Package ${pkg} exceeds capacity: deltaD=${deltaD}, C_t=${measurement.C_t}`
      };
    }
  }

  return { admitted: true };
}
```

### 7.2 CI/CD Gate

The measurement pipeline integrates with CI/CD:

1. **On PR open**: Generate provisional certificate
2. **On PR merge**: Update epoch, issue new certificate
3. **Nightly build**: Full measurement with historical comparison
4. **Release**: Publish certificate as release artifact

---

## 8. Dashboard Integration

The certificate feeds real-time dashboards (see MEASUREMENT-DASHBOARD-DESIGN.md):

1. **D_t Tracker**: System and per-package entropy over time
2. **TC Heatmap**: 43x43 coupling visualization
3. **TE Graph**: Dependency-weighted entropy flow
4. **C_t Gauge**: Remaining capacity with alerts

---

## 9. Appendix

### 9.1 Constants

```javascript
const MAX_FILES = 100;      // Normalization constant for file count
const MAX_LINES = 10000;    // Normalization constant for line count
const MAX_FUNCS = 500;      // Normalization constant for function count
const MAX_DEPS = 50;        // Normalization constant for dependency count

const TIER_WEIGHTS = {
  1: 2.0,   // Core packages weighted 2x
  2: 1.5,   // Infrastructure weighted 1.5x
  3: 1.0,   // Application weighted 1x
  4: 0.8,   // Integration weighted 0.8x
  5: 0.5    // Documentation weighted 0.5x
};

const ENTROPY_THRESHOLDS = {
  LOW: 10,
  MODERATE: 20,
  HIGH: 30,
  CRITICAL: 40
};
```

### 9.2 Practical Examples

**Example 1: Interpreting D_t = 18.79 bits for 43 packages**

When the system reports D_t = 18.79 bits, this indicates moderate complexity:
- Average per-package entropy: 18.79 / 43 = 0.437 bits
- This is well below the critical threshold of 40 bits
- The system has approximately 21.2 bits of remaining capacity

**Example 2: High Coupling Detection**

When TC(core, oxigraph) = 0.87, this indicates:
- 87% of commits touching one package also touch the other
- These packages should be treated as a unit for capacity planning
- Changes to either package likely affect both

**Example 3: Policy Impact Analysis**

When TE(core) = 38.7 bits while D_t(core) = 4.82 bits:
- The difference (33.88 bits) represents dependent package entropy
- Changes to core ripple through 33.88 bits of downstream entropy
- This justifies core's Tier 1 classification and strict review requirements

### 9.3 References

1. Shannon, C. E. (1948). "A Mathematical Theory of Communication"
2. Brooks, F. P. (1975). "The Mythical Man-Month"
3. Lehman, M. M. (1980). "Programs, Life Cycles, and Laws of Software Evolution"
4. UNRDF Project Guidelines - CLAUDE.md

---

## 10. Changelog

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2024-12-26 | Initial specification |

---

*This specification is maintained by the UNRDF Architecture Team.*
