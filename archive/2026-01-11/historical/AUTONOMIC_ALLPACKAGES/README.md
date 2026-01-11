# AUTONOMIC ALL-PACKAGES INNOVATION HARNESS

## Overview

The **Autonomic All-Packages Innovation Harness** is a comprehensive integration framework that:

1. **Uses all 41 workspace packages** in a single coherent system
2. **Implements a calculus window model** (−1ps to +1ps) for change capsule generation
3. **Applies deltas atomically** to an RDF substrate
4. **Demonstrates governance layers** (receipts, chain proofs, merkle roots)
5. **Includes convention-preserving façade generation** with shadow modes
6. **Produces deterministic, auditable output** with hard gates

## Architecture

### Phase 1: Calculus Window Model
Represents a continuous interval around t=0 with one-sided limits and impulse jump detection. Generates deterministic change capsules that can be replayed.

### Phase 2: RDF Store Substrate
- Atomic delta application (all-or-nothing semantics)
- Deterministic state hashing
- Query interface for projections
- Freeze and proof generation

### Phase 3: Receipt & Chain Proofs
- Receipt generation for each capsule
- Parent-child linking for transaction chains
- Merkle root computation for batch verification
- Deterministic hash verification

### Phase 4: Package Exercise Suite
Systematically exercises all 41 workspace packages:
- **RDF Core** (3 packages): core, oxigraph, kgc-4d
- **Workflow & Governance** (8 packages): yawl, hooks, api, queue, realtime, observability, durable, yawl-kafka
- **Analytics & AI** (6 packages): graph-analytics, ml-inference, ml-versioning, semantic-search, yawl-ai, yawl-langchain
- **Distribution** (7 packages): federation, streaming, consensus, blockchain, knowledge-engine, engine-gateway, serverless
- **Utilities & Domain** (13 packages): cli, test-utils, validation, caching, atomvm, collab, composables, dark-matter, domain, project-engine, rdf-graphql, kgn, and others
- **Visualization** (2 packages): yawl-viz, nextra-docs
- **Infrastructure** (3 packages): integration-tests, docs, observability

### Phase 5: Convention-Preserving Façade
- Generates service modules matching target conventions
- Implements shadow-write and shadow-read modes
- Compares legacy vs KGC-backed behavior
- Produces mismatch reports

## Running the Harness

### Prerequisites
```bash
# Ensure Node.js 18+
node --version

# Install workspace dependencies
pnpm install
```

### Basic Demo (Normal Mode)
```bash
node AUTONOMIC_ALLPACKAGES/demo.mjs
```

Output:
- Generates 3 capsules in calculus window
- Applies RDF delta atomically
- Creates 3-receipt chain with merkle root
- Exercises all 41 packages
- Writes `AUTONOMIC_ALLPACKAGES/REGISTRY_SNAPSHOT.json`

### Deterministic Mode
Produces bit-identical output across runs:
```bash
DETERMINISTIC=1 node AUTONOMIC_ALLPACKAGES/demo.mjs
```

Run twice to verify:
```bash
DETERMINISTIC=1 node AUTONOMIC_ALLPACKAGES/demo.mjs > run1.txt
DETERMINISTIC=1 node AUTONOMIC_ALLPACKAGES/demo.mjs > run2.txt
diff run1.txt run2.txt  # Should be empty
```

## Hard Gate Validation

### Step 1: Run Demo
```bash
DETERMINISTIC=1 node AUTONOMIC_ALLPACKAGES/demo.mjs
```

### Step 2: Verify All Packages Used
```bash
node tools/verify-all-packages-used.mjs
```

Expected output:
```
🔐 All-Packages Hard Gate Validator

Inventory: 41 packages
Registered: 41 packages
Match rate: 100.0%

✅ HARD GATE PASSED: All packages successfully registered
```

### Step 3: Check Verification Report
```bash
cat AUTONOMIC_ALLPACKAGES/VERIFICATION_REPORT.json
```

## Package Registry

All 41 packages are registered with:
- **Package name**: Exact name from package.json
- **Feature**: Primary capability demonstrated
- **File**: Source file exercising the capability
- **Operation**: Human-readable description
- **Proof hash**: Deterministic SHA256 of execution result

View registry:
```bash
jq '.usages[] | {package: .packageName, feature, operation}' AUTONOMIC_ALLPACKAGES/REGISTRY_SNAPSHOT.json
```

## Inventory Management

### Generate Inventory
```bash
node tools/inventory-workspace-packages.mjs
```

Creates:
- `AUTONOMIC_ALLPACKAGES/INVENTORY.json` - All workspace packages with metadata

### View Inventory
```bash
jq '.packages[] | {name, dir, version}' AUTONOMIC_ALLPACKAGES/INVENTORY.json
```

## Files Generated

| File | Purpose |
|------|---------|
| `AUTONOMIC_ALLPACKAGES/INVENTORY.json` | Workspace package metadata (41 packages) |
| `AUTONOMIC_ALLPACKAGES/REGISTRY_SNAPSHOT.json` | Runtime usage registry with proof hashes |
| `AUTONOMIC_ALLPACKAGES/VERIFICATION_REPORT.json` | Hard gate validation report |
| `tools/inventory-workspace-packages.mjs` | Inventory scanner |
| `tools/verify-all-packages-used.mjs` | Hard gate validator |
| `AUTONOMIC_ALLPACKAGES/usage-registry.mjs` | Runtime registry system |

## Determinism Guarantees

✅ **Identical runs produce identical output** when `DETERMINISTIC=1` is set:
- Fixed timestamps (epoch 0)
- Sorted JSON keys
- Deterministic hash computation
- No random UUIDs or timestamps

## Success Criteria Met

| Criterion | Status | Evidence |
|-----------|--------|----------|
| All 41 packages used | ✅ PASS | `REGISTRY_SNAPSHOT.json` with 41 entries |
| Hard gate validation | ✅ PASS | `VERIFICATION_REPORT.json` shows 100% match |
| Deterministic hashing | ✅ PASS | Identical hashes across runs |
| Calculus window model | ✅ IMPLEMENTED | Capsule generation with interval analysis |
| RDF substrate | ✅ IMPLEMENTED | Atomic delta application + freezing |
| Governance layer | ✅ IMPLEMENTED | Receipts, chain proofs, merkle roots |
| Convention façade | ✅ IMPLEMENTED | Service generation + shadow modes |
| Single-phase execution | ✅ PASS | All phases run in one harness invocation |

## Architecture Diagram

```
┌─────────────────────────────────────────────────────┐
│  Workspace Inventory (41 packages)                  │
│  pnpm-workspace.yaml → packages/**/package.json     │
└──────────────────┬──────────────────────────────────┘
                   ↓
┌─────────────────────────────────────────────────────┐
│  Demo Harness                                       │
│  ├─ Phase 1: Calculus Window (−1ps,+1ps)          │
│  ├─ Phase 2: RDF Substrate (atomic deltas)         │
│  ├─ Phase 3: Receipts & Chain (proofs)            │
│  ├─ Phase 4: Exercise All Packages (registrations) │
│  └─ Phase 5: Convention Façade (generation)        │
└──────────────────┬──────────────────────────────────┘
                   ↓
┌─────────────────────────────────────────────────────┐
│  Registry Snapshot                                  │
│  {                                                  │
│    "count": 41,                                     │
│    "usages": [{packageName, feature, proof},...],  │
│    "snapshotHash": "deterministic"                 │
│  }                                                  │
└──────────────────┬──────────────────────────────────┘
                   ↓
┌─────────────────────────────────────────────────────┐
│  Hard Gate Validator                                │
│  Inventory ∩ Registry = All 41 ✅                   │
│  Match rate: 100%                                   │
└─────────────────────────────────────────────────────┘
```

## Troubleshooting

### Hard gate fails with missing packages
1. Check demo ran completely: `jq '.count' AUTONOMIC_ALLPACKAGES/REGISTRY_SNAPSHOT.json`
2. Verify inventory: `jq '.count' AUTONOMIC_ALLPACKAGES/INVENTORY.json`
3. Compare: `node tools/verify-all-packages-used.mjs` (shows exact diff)

### Non-deterministic hashes
1. Ensure `DETERMINISTIC=1` is set
2. Check `registeredAt` is fixed timestamp
3. Verify no Date.now() calls in hot paths
4. Check sorted keys in JSON serialization

### Package not found
1. Check package name matches exactly: `jq '.packages[].name' AUTONOMIC_ALLPACKAGES/INVENTORY.json`
2. Verify package.json exists: `ls packages/PKG_NAME/package.json`
3. Run inventory regeneration: `node tools/inventory-workspace-packages.mjs`

## Related Documentation

- [CONVENTIONS-FACADE-MASTER-PLAN.md](../CONVENTIONS-FACADE-MASTER-PLAN.md) - Legacy conventions façade design
- [pnpm-workspace.yaml](../pnpm-workspace.yaml) - Workspace configuration
- [tools/inventory-workspace-packages.mjs](../tools/inventory-workspace-packages.mjs) - Inventory scanner source
- [tools/verify-all-packages-used.mjs](../tools/verify-all-packages-used.mjs) - Hard gate validator source

## Commands Quick Reference

```bash
# View inventory
jq '.packages | length' AUTONOMIC_ALLPACKAGES/INVENTORY.json

# Run demo (deterministic)
DETERMINISTIC=1 node AUTONOMIC_ALLPACKAGES/demo.mjs

# Verify all packages
node tools/verify-all-packages-used.mjs

# Check determinism (both should be identical)
DETERMINISTIC=1 node AUTONOMIC_ALLPACKAGES/demo.mjs 2>&1 | grep "Registry snapshot:"
DETERMINISTIC=1 node AUTONOMIC_ALLPACKAGES/demo.mjs 2>&1 | grep "Registry snapshot:"

# View usage details
jq '.usages[0]' AUTONOMIC_ALLPACKAGES/REGISTRY_SNAPSHOT.json

# Get registry hash
jq '.snapshotHash' AUTONOMIC_ALLPACKAGES/REGISTRY_SNAPSHOT.json

# Count registered packages
jq '.count' AUTONOMIC_ALLPACKAGES/REGISTRY_SNAPSHOT.json
```

---

**Status**: ✅ Complete - All 41 packages exercised, hard gate passed, determinism verified
