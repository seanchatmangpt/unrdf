# How-To: Compose Cross-Package Deltas

**Time**: ~30 minutes
**Difficulty**: Intermediate
**Prerequisites**: Understanding of [ΔGate architecture](../explanation/01-deltagate-architecture.md)

---

## What Are Deltas?

Deltas (Δ) are **admissible state transitions** - minimal, composable changes to knowledge graphs. Think of them as Git commits for RDF.

**Key Properties:**
- **Atomic**: All-or-nothing application
- **Composable**: Δ₁ ∘ Δ₂ = Δ₃ (function composition)
- **Reversible**: Every Δ has an inverse Δ⁻¹
- **Verifiable**: Cryptographic proof of correctness

---

## Delta Structure

```json
{
  "id": "delta-uuid",
  "type": "composite",
  "description": "Human-readable description",
  "delta": {
    "additions": [/* RDF quads to add */],
    "deletions": [/* RDF quads to remove */]
  },
  "preconditions": {
    "requiredPredicates": ["rdf:type"],
    "forbiddenPredicates": [],
    "stateHash": "sha256:expected-hash"
  },
  "metadata": {
    "package": "@unrdf/example",
    "author": "alice@example.org",
    "timestamp": 1704067200000
  }
}
```

---

## Example: Compose Two Simple Deltas

### Step 1: Create Delta 1 (Add Person)

`delta-add-person.json`:
```json
{
  "id": "delta-001",
  "type": "add-entity",
  "description": "Add Alice as a Person",
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

### Step 2: Create Delta 2 (Add Affiliation)

`delta-add-affiliation.json`:
```json
{
  "id": "delta-002",
  "type": "add-relationship",
  "description": "Add Alice's affiliation to ACME Corp",
  "delta": {
    "additions": [
      {
        "subject": "http://example.org/alice",
        "predicate": "http://xmlns.com/foaf/0.1/member",
        "object": "http://example.org/acme-corp"
      },
      {
        "subject": "http://example.org/acme-corp",
        "predicate": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        "object": "http://xmlns.com/foaf/0.1/Organization"
      }
    ],
    "deletions": []
  },
  "preconditions": {
    "requiredSubjects": ["http://example.org/alice"]
  }
}
```

### Step 3: Compose Deltas

```bash
# Compose two deltas into one
npx kgc delta compose \
  --input delta-add-person.json \
  --input delta-add-affiliation.json \
  --output delta-composite.json
```

**Output** (`delta-composite.json`):
```json
{
  "id": "delta-composite-001-002",
  "type": "composite",
  "description": "Add Alice as a Person with ACME Corp affiliation",
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
      },
      {
        "subject": "http://example.org/alice",
        "predicate": "http://xmlns.com/foaf/0.1/member",
        "object": "http://example.org/acme-corp"
      },
      {
        "subject": "http://example.org/acme-corp",
        "predicate": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        "object": "http://xmlns.com/foaf/0.1/Organization"
      }
    ],
    "deletions": []
  },
  "preconditions": {
    "requiredPredicates": ["http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]
  },
  "composition": {
    "sources": ["delta-001", "delta-002"],
    "operator": "sequential",
    "receipt": "sha256:composition-proof..."
  }
}
```

---

## Programmatic Composition

For complex use cases, compose deltas programmatically:

```javascript
import { composeDelta, withReceipt } from '@unrdf/v6-core/delta';
import { z } from 'zod';

const composePerson = withReceipt(async function composePerson(name, organization) {
  const addPersonDelta = {
    id: `delta-person-${name}`,
    type: 'add-entity',
    delta: {
      additions: [
        {
          subject: `http://example.org/${name}`,
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          object: 'http://xmlns.com/foaf/0.1/Person'
        },
        {
          subject: `http://example.org/${name}`,
          predicate: 'http://xmlns.com/foaf/0.1/name',
          object: `"${name}"`
        }
      ],
      deletions: []
    }
  };

  const addAffiliationDelta = {
    id: `delta-affiliation-${name}-${organization}`,
    type: 'add-relationship',
    delta: {
      additions: [
        {
          subject: `http://example.org/${name}`,
          predicate: 'http://xmlns.com/foaf/0.1/member',
          object: `http://example.org/${organization}`
        }
      ],
      deletions: []
    },
    preconditions: {
      requiredSubjects: [`http://example.org/${name}`]
    }
  };

  const compositeDelta = await composeDelta([addPersonDelta, addAffiliationDelta]);
  return compositeDelta;
});

// Usage
const { value: delta, receipt } = await composePerson('alice', 'acme-corp');
console.log('Composite delta:', delta.id);
console.log('Receipt:', receipt.hash);
```

---

## Cross-Package Delta Composition

Deltas from different packages can compose seamlessly:

### Example: YAWL Workflow + Hooks Policy

```javascript
import { defineWorkflow } from '@unrdf/yawl';
import { defineHook } from '@unrdf/hooks';
import { composeDelta } from '@unrdf/v6-core/delta';

// Define workflow delta
const workflowDelta = {
  id: 'delta-workflow-001',
  type: 'start-workflow',
  delta: {
    additions: [
      {
        subject: 'http://example.org/workflow-001',
        predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        object: 'http://unrdf.org/yawl#Workflow'
      },
      {
        subject: 'http://example.org/workflow-001',
        predicate: 'http://unrdf.org/yawl#status',
        object: '"running"'
      }
    ],
    deletions: []
  },
  metadata: {
    package: '@unrdf/yawl'
  }
};

// Define hook delta
const hookDelta = {
  id: 'delta-hook-001',
  type: 'activate-hook',
  delta: {
    additions: [
      {
        subject: 'http://example.org/hook-001',
        predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        object: 'http://unrdf.org/hooks#PolicyHook'
      },
      {
        subject: 'http://example.org/hook-001',
        predicate: 'http://unrdf.org/hooks#attachedTo',
        object: 'http://example.org/workflow-001'
      }
    ],
    deletions: []
  },
  metadata: {
    package: '@unrdf/hooks'
  }
};

// Compose across packages
const compositeDelta = await composeDelta([workflowDelta, hookDelta]);

// Apply to universe
import { applyDelta } from '@unrdf/v6-core/delta';
const { receipt } = await applyDelta(compositeDelta, universeId);

console.log('Cross-package composition receipt:', receipt.hash);
console.log('Packages involved:', receipt.metadata.packages); // ['@unrdf/yawl', '@unrdf/hooks']
```

---

## Delta Verification

Always verify deltas before applying:

```bash
npx kgc delta verify --file delta-composite.json
```

**Checks:**
- ✅ Well-formed JSON
- ✅ Valid RDF quads
- ✅ Preconditions satisfiable
- ✅ No conflicts (additions ∩ deletions = ∅)
- ✅ Receipt signature valid

---

## Delta Inversion (Undo)

Every delta can be inverted:

```bash
npx kgc delta invert --file delta-composite.json --output delta-undo.json
```

**Result**: Additions ↔ Deletions swapped

Apply the inverse to undo:
```bash
npx kgc delta apply --file delta-undo.json --universe universe-abc123
```

---

## Advanced: Conditional Deltas

Deltas can have complex preconditions:

```json
{
  "id": "delta-conditional",
  "type": "update-status",
  "delta": {
    "additions": [
      {
        "subject": "http://example.org/workflow-001",
        "predicate": "http://unrdf.org/yawl#status",
        "object": "\"completed\""
      }
    ],
    "deletions": [
      {
        "subject": "http://example.org/workflow-001",
        "predicate": "http://unrdf.org/yawl#status",
        "object": "\"running\""
      }
    ]
  },
  "preconditions": {
    "sparqlCheck": "ASK WHERE { <http://example.org/workflow-001> <http://unrdf.org/yawl#status> \"running\" }",
    "stateHash": "sha256:expected-before-hash"
  }
}
```

The delta only applies if:
1. SPARQL query returns `true`
2. Universe state hash matches `stateHash`

---

## Delta Receipt Chain

Each delta application generates a receipt that chains to the previous:

```javascript
// Apply delta 1
const { receipt: receipt1 } = await applyDelta(delta1, universeId);

// Apply delta 2 (chains to receipt1)
const { receipt: receipt2 } = await applyDelta(delta2, universeId);

console.log('Receipt chain:', receipt2.metadata.parentReceipts);
// ['sha256:receipt1-hash', 'sha256:receipt2-hash']

// Verify entire chain
import { verifyReceiptChain } from '@unrdf/v6-core/receipts';
const isValid = await verifyReceiptChain([receipt1, receipt2]);
console.log('Chain valid?', isValid); // true
```

---

## Performance Considerations

- **Small deltas compose faster**: Prefer 10 small deltas over 1 large
- **Parallel composition**: Independent deltas can compose in parallel
- **Receipt caching**: Verify receipts once, cache results

**Benchmark**:
```bash
npx kgc delta benchmark --file delta-composite.json
```

**Output:**
```
Composition time: 3.2ms
Application time: 12.5ms
Verification time: 5.1ms
Receipt size: 2.4 KB
```

---

## Troubleshooting

### "Preconditions not satisfied"

**Cause**: Universe state doesn't match expected preconditions

**Solution**: Check current state:
```bash
npx kgc universe query --id universe-abc123 \
  --sparql "SELECT * WHERE { ?s ?p ?o } LIMIT 10"
```

### "Conflict detected (addition matches deletion)"

**Cause**: Delta tries to add and delete the same quad

**Solution**: Review delta definition - additions and deletions must be disjoint

### "Receipt verification failed"

**Cause**: Receipt hash mismatch (non-deterministic composition)

**Solution**: Ensure deterministic ordering when composing:
```javascript
const deltas = [delta1, delta2].sort((a, b) => a.id.localeCompare(b.id));
const composite = await composeDelta(deltas);
```

---

## Summary

✅ Deltas are atomic, composable state transitions
✅ Compose deltas with `kgc delta compose` or programmatically
✅ Cross-package deltas compose seamlessly
✅ Every delta has an inverse for undo
✅ Receipt chains preserve full provenance
✅ Verify deltas before applying

**Next**: [How-To: Verify Receipt Chain Integrity](./03-verify-receipt-chain.md)
