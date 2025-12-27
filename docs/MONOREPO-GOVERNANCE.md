# Monorepo Governance - Governed Ontology Substrate Extension

This document defines the governance framework for the UNRDF monorepo, extending the Governed Ontology Substrate (GOS) to provide unified governance across all 43+ packages.

## Overview

The UNRDF monorepo is modeled as a **unified universe O** where each package is an **admissible partition**. The Monorepo Governance system ensures that all changes across packages maintain global invariants and respect cross-package constraints.

```
O = O_core + O_hooks + O_yawl + O_ml + O_infrastructure +
    O_observability + O_documentation + O_testing + O_utility
```

Every change to any package must be **admitted** through the governance engine, which enforces:
- **Invariants (Q)**: Properties that must always hold true
- **Guards (H)**: Operations that are forbidden under any circumstances

## Partition Categories

Packages are organized into logical partitions based on their function:

| Category | Identifier | Packages | Description |
|----------|------------|----------|-------------|
| Core | `O_core` | core, oxigraph, validation, domain | Foundational RDF/SPARQL functionality |
| Hooks | `O_hooks` | hooks | Knowledge hooks and event sourcing |
| YAWL | `O_yawl` | yawl, yawl-* (10 packages) | Workflow orchestration |
| ML | `O_ml` | ml-inference, ml-versioning | Machine learning layers |
| Infrastructure | `O_infrastructure` | serverless, streaming, caching, etc. | Deployment infrastructure |
| Observability | `O_observability` | observability, kgc-4d, kgn | Monitoring and tracing |
| Documentation | `O_documentation` | docs, nextra, diataxis-kit | Documentation system |
| Testing | `O_testing` | integration-tests, test-utils | Test infrastructure |
| Utility | `O_utility` | cli, composables, react, etc. | Utility packages |

## Protected Packages

Certain packages are designated as **protected** and receive additional scrutiny:

- `@unrdf/core` - The foundational RDF store and operations
- `@unrdf/oxigraph` - The Oxigraph binding layer
- `@unrdf/hooks` - Event sourcing and hook execution
- `@unrdf/validation` - SHACL validation engine

Protected packages:
1. Must maintain version alignment with core
2. Cannot be deprecated
3. Breaking changes require major version bump
4. Public API changes require approval

## Cross-Package Invariants (Q_monorepo)

The following invariants must hold true across the entire monorepo:

### Q_version_consistency

**Version alignment across dependent packages.**

Rules:
- If core is v5.x.y, all dependent packages must be >= v4.9.0
- Protected packages must share the same major.minor version as core
- Workspace dependencies should have matching major versions
- Maximum minor version drift is configurable (default: 1)

Example violation:
```
@unrdf/core: 5.0.1
@unrdf/hooks: 4.0.0  // VIOLATION: Major version mismatch for protected package
```

### Q_api_stability

**Breaking changes require corresponding downstream updates.**

Rules:
- Breaking changes to a package require updates in all direct dependents
- Export removals require migration in all consumers
- Signature changes require version bumps in downstream packages

Example:
```javascript
// If @unrdf/hooks changes 'defineHook' signature:
// @unrdf/yawl (depends on hooks) MUST also be updated in the same delta
```

### Q_dependency_acyclic

**No circular dependencies in the package graph.**

The dependency graph must be a DAG (Directed Acyclic Graph). Cycles are detected using Kahn's algorithm during admission.

Violation example:
```
@unrdf/core -> @unrdf/hooks -> @unrdf/core  // CYCLE DETECTED
```

### Q_license_compliance

**All packages must have compatible licenses.**

Compatible licenses (with MIT base):
- MIT, ISC, BSD-2-Clause, BSD-3-Clause
- Apache-2.0, 0BSD, Unlicense, CC0-1.0

Incompatible licenses (blocked):
- GPL-3.0, AGPL-3.0, proprietary

### Q_documentation_coverage

**Public APIs must have documentation coverage >= 80%.**

- Protected packages: Required (strict enforcement)
- Other packages: Recommended (warning only)
- Documentation packages: Exempt

### Q_test_coverage

**Packages must have test coverage >= 70%.**

- Core packages: Required (strict enforcement)
- Other packages: Recommended (warning only)
- Documentation packages: Exempt

## Cross-Package Guards (H_monorepo)

Guards prevent dangerous operations that should never be allowed:

### H_core_signature_break

**Cannot change core public API without major version bump.**

Protected symbols in core packages:
- `createStore`, `dataFactory`, `quad`, `namedNode`, `blankNode`
- `literal`, `variable`, `defaultGraph`, `SPARQLEngine`, `RDFStore`

Breaking changes to these symbols require:
1. Major version bump in the same delta
2. Updates to all downstream packages

### H_internal_api_external_export

**Cannot expose @private symbols in public API.**

Internal patterns (blocked from export):
- `_` prefix (underscore-prefixed symbols)
- `@private` JSDoc annotation
- `@internal` JSDoc annotation
- `.internal.` in path
- `/internal/` in path

### H_circular_dependency

**Cannot add dependency that creates a cycle.**

Before any dependency addition, the system checks if the addition would create a cycle in the dependency graph.

### H_license_incompatible

**Cannot depend on packages with incompatible licenses.**

Both direct dependencies and proposed dependency additions are checked.

### H_protected_package_delete

**Cannot deprecate or delete protected packages.**

Protected packages are foundational to the ecosystem and cannot be removed.

### H_cross_category_break

**Cannot violate cross-category dependency rules.**

Category dependency rules:
| Category | Can Depend On |
|----------|---------------|
| Core | Nothing (foundational) |
| Hooks | Core |
| YAWL | Core, Hooks |
| ML | Core, Hooks, YAWL |
| Infrastructure | Core, Hooks |
| Observability | Core |
| Documentation | Nothing (isolated) |
| Testing | Anything (*) |
| Utility | Core, Hooks |

## Admission Flow

### Single Package Change

```
1. Developer proposes delta with changes
2. Engine validates delta structure
3. Run local package checks:
   - Protected package approval check
   - Version format validation
4. Run cross-package guards (H_monorepo):
   - H_core_signature_break
   - H_internal_api_external_export
   - H_circular_dependency
   - H_license_incompatible
   - H_protected_package_delete
   - H_cross_category_break
5. Run cross-package invariants (Q_monorepo):
   - Q_version_consistency
   - Q_api_stability
   - Q_dependency_acyclic
   - Q_license_compliance
   - Q_documentation_coverage
   - Q_test_coverage
6. Decision: ALLOW or DENY
7. Generate cryptographic receipt
```

### Multi-Package Change (Atomic Admission)

For changes affecting multiple packages:

```
1. Package all changes into single PackageDelta
2. Run admission flow (all checks at once)
3. ATOMIC DECISION:
   - If ALL packages pass: ALLOW entire delta
   - If ANY package fails: DENY entire delta
4. No partial admissions - maintain consistency
```

### Example: Coordinated Version Bump

```javascript
const delta = new PackageDelta({
  agent: 'release-manager',
  justification: 'Coordinated 5.1.0 release',
  changes: [
    { packageName: '@unrdf/core', changeType: 'version_bump',
      details: { newVersion: '5.1.0', bumpType: 'minor' } },
    { packageName: '@unrdf/hooks', changeType: 'version_bump',
      details: { newVersion: '5.1.0', bumpType: 'minor' } },
    { packageName: '@unrdf/yawl', changeType: 'version_bump',
      details: { newVersion: '5.1.0', bumpType: 'minor' } }
  ]
});

const result = await engine.admit(delta);
// Either all 3 packages are bumped, or none
```

## Breaking Change Governance

Breaking changes require special handling:

### Major Version Bump Flow

1. Propose breaking change with major version bump
2. Include updates to ALL affected downstream packages
3. Provide justification and migration guide
4. Get approval for protected packages

```javascript
const delta = new PackageDelta({
  agent: 'core-team',
  justification: 'BREAKING: New store architecture',
  changes: [
    { packageName: '@unrdf/core', changeType: 'version_bump',
      details: { newVersion: '6.0.0', bumpType: 'major', approvedBy: 'lead' } },
    { packageName: '@unrdf/core', changeType: 'api_breaking',
      details: { symbol: 'createStore', migration: 'docs/migration-6.0.md' } },
    // All dependents must be updated:
    { packageName: '@unrdf/hooks', changeType: 'version_bump',
      details: { newVersion: '6.0.0', bumpType: 'major' } },
    { packageName: '@unrdf/yawl', changeType: 'version_bump',
      details: { newVersion: '6.0.0', bumpType: 'major' } },
    // ... all other dependents
  ]
});
```

### API Stability Contract

The following symbols are part of the public API contract:

**@unrdf/core:**
- `createStore()` - Create RDF store
- `dataFactory` - RDF term factory
- `quad()`, `namedNode()`, `blankNode()`, `literal()` - Term constructors
- `SPARQLEngine` - Query engine
- `RDFStore` - Store class

**@unrdf/oxigraph:**
- `createStore()` - Create Oxigraph store
- `dataFactory` - Oxigraph data factory
- `OxigraphStore` - Store class

**@unrdf/hooks:**
- `defineHook()` - Define knowledge hook
- `createHookExecutor()` - Create executor
- `HookExecutor` - Executor class

## Receipts and Audit Trail

Every admission generates a cryptographic receipt:

```javascript
{
  receiptId: "uuid",
  admissionId: "uuid",
  timestamp: "ISO8601",
  decision: "ALLOW" | "DENY",
  inputHashes: {
    deltaHash: "sha256",
    universeHashBefore: "sha256",
    partitionHashes: [{ name: "@unrdf/core", hash: "sha256" }]
  },
  outputHashes: {
    universeHashAfter: "sha256",
    receiptHash: "blake3"
  },
  checks: {
    guardsRun: 6,
    guardsPassed: 6,
    invariantsRun: 6,
    invariantsPassed: 6
  },
  epoch: "tau_2025_12_26_1234_567"
}
```

Receipts provide:
- **Immutability**: Object.freeze() prevents modification
- **Determinism**: Same inputs produce same hash
- **Verifiability**: Can verify receipt hash at any time
- **Audit trail**: Complete history of all admission decisions

## Configuration

### Universe Configuration

```javascript
const universe = new MonorepoUniverse({
  rootPath: '/path/to/unrdf',
  strictMode: true,
  enabledInvariants: [
    'Q_version_consistency',
    'Q_api_stability',
    'Q_dependency_acyclic',
    'Q_license_compliance'
  ],
  enabledGuards: [
    'H_core_signature_break',
    'H_circular_dependency',
    'H_license_incompatible'
  ],
  thresholds: {
    minTestCoverage: 70,
    minDocCoverage: 80,
    maxVersionDrift: 1
  }
});
```

### Engine Configuration

```javascript
const engine = new MonorepoAdmissionEngine(universe, {
  strictMode: true,
  atomicAdmission: true,
  generateReceipts: true,
  enableLocalChecks: true,
  auditLog: true,
  maxBatchSize: 50
});
```

## Implementation Files

The implementation is located in `/home/user/unrdf/src/monorepo-admission/`:

| File | Purpose | Lines |
|------|---------|-------|
| `package-partition.mjs` | Package as partition model | ~400 |
| `cross-package-invariants.mjs` | Q_monorepo invariants | ~400 |
| `cross-package-guards.mjs` | H_monorepo guards | ~350 |
| `monorepo-universe.mjs` | Universe orchestrator | ~350 |
| `monorepo-admission-engine.mjs` | Atomic admission engine | ~400 |
| `index.mjs` | Public API exports | ~100 |
| `monorepo-admission.test.mjs` | Comprehensive tests | ~500 |

Total: ~2500 lines of implementation

## Summary

The Monorepo Governance system provides:

1. **Unified Universe Model**: All 43+ packages as partitions in universe O
2. **Six Invariants (Q)**: Version consistency, API stability, acyclic deps, license compliance, doc coverage, test coverage
3. **Six Guards (H)**: Core signature, internal API, circular deps, license, protected packages, cross-category
4. **Atomic Admission**: All-or-none decisions for multi-package changes
5. **Cryptographic Receipts**: Immutable, verifiable audit trail
6. **Flexible Configuration**: Enable/disable checks as needed

This governance framework ensures the monorepo maintains consistency, prevents breaking changes from slipping through, and provides a complete audit trail for all changes.
