# @unrdf/fusion

**Unified Integration Layer** - 7-Day Innovation Consolidation

Canonical API surface for UNRDF's recent innovations:
- **KGC-4D**: Time-travel store, Git snapshots, HDIT coordinates
- **Blockchain**: Cryptographic receipts, Merkle proofs, anchoring
- **Hooks**: Policy execution, validation, transformation
- **Caching**: Multi-layer cache, resource management
- **Oxigraph**: High-performance RDF store

## Quick Start

```javascript
import { createEngine, prove } from '@unrdf/fusion';

// Create unified engine
const engine = await createEngine({
  enableCaching: true,
  enableBlockchain: true,
  enableGit: false,
});

// Access subsystems
const { store, kgcStore, receipts, policies, resources } = engine;

// Get combined stats
console.log(engine.getStats());

// Cleanup
await engine.close();
```

## Deterministic Proof

Run the E2E proof scenario:

```bash
DETERMINISTIC=1 node tools/prove.mjs
```

This executes a complete workflow:
1. Create KGC store + Git backbone
2. Apply policy hooks
3. Allocate resources via cache
4. Execute test case
5. Generate blockchain receipts
6. Produce Merkle proof

Output:
- Final proof hash
- Merkle root
- Receipt count
- Verification status
- Full ledger JSON

## Exported APIs

### Core Store
- `createStore()` - Create Oxigraph RDF store
- `dataFactory` - RDF term creation (namedNode, literal, etc.)

### KGC-4D Engine
- `KGCStore` - Time-travel enabled store
- `GitBackbone` - Git-backed snapshots
- `freezeUniverse()` - Snapshot creation
- `VectorClock` - Distributed time
- HDIT functions: `coordsForEvent`, `findKNearest`, `projectPCA`

### Blockchain
- `ReceiptAnchorer` - Ethereum anchoring
- `MerkleProofGenerator` - Merkle tree generation
- `WorkflowVerifier` - Smart contract verification

### Policy Hooks
- `defineHook()` - Hook definition
- `executeHook()` - Hook execution
- `createHookRegistry()` - Hook management
- `builtinHooks` - Standard validators

### Caching
- `createCachingSystem()` - Multi-layer cache
- `MultiLayerCache` - L1/L2/L3 caching
- `DependencyTracker` - Invalidation tracking

## Integration Map

```
@unrdf/fusion
├── Store (@unrdf/oxigraph)
│   └── createStore(), dataFactory
├── Engine (@unrdf/kgc-4d)
│   ├── KGCStore (time-travel)
│   ├── GitBackbone (snapshots)
│   └── HDIT (coordinates, similarity)
├── Receipts (@unrdf/blockchain)
│   ├── ReceiptAnchorer
│   └── MerkleProofGenerator
├── Policies (@unrdf/hooks)
│   ├── defineHook()
│   └── executeHook()
└── Resources (@unrdf/caching)
    ├── MultiLayerCache
    └── DependencyTracker
```

## Proof Artifacts

Ledger location: `ARTIFACTS/last7days-ledger.json`

Schema:
```json
{
  "proofHash": "sha256 hex",
  "scenario": {
    "workflowCreated": true,
    "policyApplied": true,
    "resourceAllocated": true,
    "caseExecuted": true,
    "receiptsEmitted": 5,
    "merkleRoot": "0x...",
    "verificationPassed": true
  },
  "versions": { ... },
  "hashes": { ... }
}
```

## For Agents 2-10

This package provides the canonical imports. Do NOT re-implement:

```javascript
// ✅ CORRECT - Import from fusion
import { createEngine, KGCStore, ReceiptAnchorer } from '@unrdf/fusion';

// ❌ WRONG - Re-implementing
const myStore = new CustomStore(); // Don't do this
```

All APIs are delegated to existing packages. Zero reimplementation.
