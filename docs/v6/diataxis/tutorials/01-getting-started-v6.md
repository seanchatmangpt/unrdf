# Getting Started with UNRDF v6

**Time to Complete**: ~15 minutes
**Prerequisites**: Node.js ≥18, basic knowledge of RDF
**What You'll Learn**: Install v6, create your first receipt, run your first delta command

---

## What is UNRDF v6?

UNRDF v6 introduces a revolutionary **receipt-driven** architecture where every operation produces cryptographic proof of execution. This enables:

- **Deterministic execution**: Same input → same output, always
- **Auditable operations**: Full provenance chain for compliance
- **Compositional deltas**: Changes compose like mathematical functions
- **L1-L5 maturity ladder**: Graduated path from "it works" to "production-hardened"

---

## Installation

### Step 1: Install v6-core

```bash
# Using pnpm (recommended)
pnpm add @unrdf/v6-core @unrdf/oxigraph @unrdf/kgc-4d

# Or using npm
npm install @unrdf/v6-core @unrdf/oxigraph @unrdf/kgc-4d
```

**Why these packages?**
- `@unrdf/v6-core`: ΔGate control plane and CLI
- `@unrdf/oxigraph`: High-performance SPARQL engine (replaces N3)
- `@unrdf/kgc-4d`: Receipt engine (KGC = Knowledge Git Commits)

---

### Step 2: Verify Installation

```bash
# Check v6-core CLI is available
npx kgc --version
# Expected output: @unrdf/v6-core v6.0.0-alpha.1

# Check available commands
npx kgc --help
# You should see 10 canonical nouns: universe, eventlog, receipt, policy, workflow, resource, grammar, thesis, package, delta
```

---

## Your First Receipt

Receipts are the heart of v6. Every operation you perform generates a cryptographic receipt proving:
- **What** happened (operation type)
- **When** it happened (timestamp)
- **Who** initiated it (agent)
- **How** to reproduce it (inputs + deterministic hash)

### Create a Receipt for a Simple Operation

Create a file `first-receipt.mjs`:

```javascript
import { withReceipt } from '@unrdf/v6-core/receipts';
import { sha256 } from 'hash-wasm';

// Wrap any function to make it receipt-generating
const addWithReceipt = withReceipt(async (a, b) => {
  return a + b;
});

// Execute and get receipt
const result = await addWithReceipt(5, 3);

console.log('Result:', result.value);        // 8
console.log('Receipt:', result.receipt);     // { hash: "sha256:...", timestamp: ..., inputs: [5, 3], output: 8 }
console.log('Hash:', result.receipt.hash);   // Deterministic hash of this operation

// Verify the receipt
import { verifyReceipt } from '@unrdf/v6-core/receipts';
const isValid = await verifyReceipt(result.receipt);
console.log('Receipt valid?', isValid);      // true
```

Run it:

```bash
node first-receipt.mjs
```

**Expected Output:**
```
Result: 8
Receipt: {
  hash: "sha256:3f79bb7b435b05321651daefd374cdc681dc06faa65e374e38337b88ca046dea",
  timestamp: 1704067200000,
  operation: "addWithReceipt",
  inputs: [5, 3],
  output: 8,
  version: "6.0.0-alpha.1"
}
Hash: sha256:3f79bb7b435b05321651daefd374cdc681dc06faa65e374e38337b88ca046dea
Receipt valid? true
```

✅ **You just created your first cryptographically verifiable receipt!**

---

## Your First Delta Command

Deltas (Δ) are **admissible state transitions** - changes that can be applied to a knowledge graph with proof of correctness.

### Step 1: Create a Delta Definition

Create `my-first-delta.json`:

```json
{
  "id": "delta-001",
  "type": "add-triple",
  "description": "Add a person to the knowledge graph",
  "delta": {
    "additions": [
      {
        "subject": "http://example.org/alice",
        "predicate": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        "object": "http://xmlns.com/foaf/0.1/Person"
      },
      {
        "subject": "http://example.org/alice",
        "predicate": "http://xmlns.com/foaf/0.1/name",
        "object": "\"Alice Smith\""
      }
    ],
    "deletions": []
  },
  "preconditions": {
    "requiredPredicates": ["http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]
  }
}
```

### Step 2: Verify Delta is Well-Formed

```bash
npx kgc delta verify --file my-first-delta.json
```

**Expected Output:**
```
✅ Delta delta-001 is well-formed
✅ Preconditions satisfied
✅ Additions: 2 triples
✅ Deletions: 0 triples
Receipt: sha256:a1b2c3d4...
```

### Step 3: Apply Delta to a Universe

```bash
# Create a new universe (KGC-4D spacetime snapshot)
npx kgc universe create --name "Tutorial Universe"
# Output: Universe created with ID: universe-abc123

# Apply the delta
npx kgc delta apply --file my-first-delta.json --universe universe-abc123
```

**Expected Output:**
```
✅ Delta delta-001 applied successfully
Universe state hash (before): sha256:empty
Universe state hash (after):  sha256:f4e5d6c7...
Receipt chain: [sha256:a1b2c3d4..., sha256:f4e5d6c7...]
```

---

## Understanding the Receipt

Every command you ran generated a receipt. Let's inspect one:

```bash
npx kgc receipt verify --hash sha256:a1b2c3d4...
```

**Output:**
```json
{
  "hash": "sha256:a1b2c3d4...",
  "timestamp": 1704067200000,
  "operation": "delta.apply",
  "inputs": {
    "deltaId": "delta-001",
    "universeId": "universe-abc123"
  },
  "outputs": {
    "newStateHash": "sha256:f4e5d6c7...",
    "triplesAdded": 2,
    "triplesDeleted": 0
  },
  "proof": {
    "merkleRoot": "sha256:...",
    "witness": [...],
    "chainOfCustody": ["sha256:a1b2c3d4..."]
  },
  "signature": "ed25519:..."
}
```

**Key Fields:**
- `hash`: Unique identifier (deterministic)
- `proof.merkleRoot`: Anchors this receipt to a blockchain-style merkle tree
- `proof.chainOfCustody`: Links to parent receipts (creates a chain)

---

## Freeze and Restore a Universe

V6 integrates with **KGC-4D**, which snapshots your knowledge graph to Git:

```bash
# Freeze universe to Git
npx kgc universe freeze --id universe-abc123
# Output: Snapshot saved to .kgc/snapshots/universe-abc123-<timestamp>.git

# Verify the snapshot
npx kgc universe verify --id universe-abc123
# Output: ✅ Universe verified against Git snapshot

# Restore from snapshot (e.g., after corruption)
npx kgc universe restore --snapshot universe-abc123-<timestamp>
# Output: ✅ Universe restored from snapshot
```

---

## What's Different from v5?

| Feature | v5 | v6 |
|---------|----|----|
| **Store** | `new Store()` from N3 | `createStore()` from Oxigraph |
| **Receipts** | Optional, manual | Automatic, mandatory |
| **Validation** | Runtime errors | Zod schemas (compile-time + runtime) |
| **CLI** | Package-specific | Unified `kgc <noun> <verb>` |
| **Determinism** | Best-effort | Guaranteed (L3+) |
| **Maturity** | None | L1-L5 ladder enforced |

---

## Next Steps

Now that you've completed the basics, try:

1. **[How-To: Migrate a v5 Package to v6](../how-to/01-migrate-v5-to-v6.md)**
   Step-by-step guide to migrating existing code

2. **[How-To: Compose Cross-Package Deltas](../how-to/02-compose-deltas.md)**
   Build complex changes from simple deltas

3. **[Explanation: Why ΔGate Architecture?](../explanation/01-deltagate-architecture.md)**
   Deep dive into the theoretical foundations

4. **[Reference: CLI Command Matrix](../reference/01-cli-command-matrix.md)**
   Complete reference for all 10 nouns × 25 verbs

---

## Troubleshooting

### "Cannot find module '@unrdf/v6-core'"

**Solution**: Ensure you're using Node.js ≥18 and have installed v6-core:
```bash
node --version  # Should be ≥18.0.0
pnpm install @unrdf/v6-core
```

### "Receipt verification failed"

**Cause**: Receipt hash mismatch (non-deterministic input)

**Solution**: Check for `Date.now()` or `Math.random()` in your code. All operations must be deterministic at L3+.

### "Delta preconditions not satisfied"

**Cause**: Universe state doesn't match expected preconditions

**Solution**: Verify universe state before applying delta:
```bash
npx kgc universe verify --id <universe-id>
```

---

## Summary

You've learned:

✅ How to install UNRDF v6
✅ How to create and verify receipts
✅ How to use the unified `kgc` CLI
✅ How to create, verify, and apply deltas
✅ How to freeze/restore universes with KGC-4D

**Next**: [How-To: Migrate Your First Package →](../how-to/01-migrate-v5-to-v6.md)
