# Tutorial: Create and Freeze Your First Universe

**Objective:** Learn how to create a KGC store, add RDF data, and freeze it into a Git-backed snapshot.

**Time:** 10 minutes  
**Level:** Beginner

---

## Prerequisites

**Capabilities Needed:**
- Capability: "RDF quad storage"
- Capability: "Event append with ACID semantics"
- Capability: "Universe freeze to Git"
- Capability: "BLAKE3 hashing"

**Prerequisites:**
- [Tutorial 01: Nanosecond Timestamps](./01-nanosecond-timestamps.md)
- Basic RDF/triple knowledge (helpful but not required)

---

## What You'll Learn

1. Initialize a KGCStore and GitBackbone
2. Add RDF quads to the Universe graph
3. Append events atomically with receipts
4. Freeze the universe to create a snapshot
5. Verify the cryptographic receipt

---

## Step 1: Initialize Store and Git Backend

```javascript
import { KGCStore, GitBackbone } from '@unrdf/kgc-4d';

// Create store (holds RDF data)
const store = new KGCStore();

// Create Git backbone (stores snapshots)
const git = new GitBackbone('./my-kgc-repo');

console.log('Store initialized');
```

**What's happening:** KGCStore extends the standard UNRDF store with 4D capabilities. GitBackbone uses isomorphic-git for snapshots.

---

## Step 2: Create RDF Data

```javascript
import { dataFactory } from '@unrdf/oxigraph';

// Create RDF quad: Alice is a Person
const alice = dataFactory.namedNode('http://example.org/Alice');
const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const person = dataFactory.namedNode('http://example.org/Person');

const aliceQuad = dataFactory.quad(alice, rdfType, person);

console.log('Created quad:', aliceQuad);
```

**RDF Reminder:** A quad is (subject, predicate, object, graph). Here: (Alice, rdf:type, Person, Universe).

---

## Step 3: Append Event to EventLog

```javascript
import { EVENT_TYPES } from '@unrdf/kgc-4d';

// Append event with state delta
const receipt = await store.appendEvent(
  {
    type: EVENT_TYPES.CREATE,
    payload: { description: 'Added Alice to universe' },
  },
  [{ type: 'add', ...aliceQuad }]
);

console.log('Event appended at:', receipt.receipt.timestamp_iso);
console.log('Event count:', receipt.receipt.event_count);
console.log('Receipt ID:', receipt.receipt.id);
```

**ACID Guarantee:** The event and state change are atomic. If anything fails, both rollback.

---

## Step 4: Freeze the Universe

```javascript
import { freezeUniverse } from '@unrdf/kgc-4d';

const frozen = await freezeUniverse(store, git);

console.log('Frozen at:', frozen.timestamp_iso);
console.log('Universe hash:', frozen.universe_hash.substring(0, 16) + '...');
console.log('Git commit:', frozen.git_ref.substring(0, 8) + '...');
console.log('Quad count:', frozen.nquad_count);
```

**What happened:**
1. Store dumped all Universe quads to N-Quads format
2. BLAKE3 hash computed over sorted quads (deterministic)
3. Snapshot committed to Git
4. Receipt recorded in EventLog with hash + Git ref

---

## Step 5: Complete Working Example

```javascript
import {
  KGCStore,
  GitBackbone,
  freezeUniverse,
  verifyReceipt,
  EVENT_TYPES,
} from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

async function main() {
  // Initialize
  const store = new KGCStore();
  const git = new GitBackbone('./tutorial-02-repo');

  // Create RDF data
  const alice = dataFactory.namedNode('http://example.org/Alice');
  const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
  const person = dataFactory.namedNode('http://example.org/Person');
  const aliceQuad = dataFactory.quad(alice, rdfType, person);

  // Append event
  const receipt1 = await store.appendEvent(
    {
      type: EVENT_TYPES.CREATE,
      payload: { description: 'Added Alice' },
    },
    [{ type: 'add', ...aliceQuad }]
  );

  console.log('Event appended:', receipt1.receipt.id);

  // Freeze universe
  const frozen = await freezeUniverse(store, git);
  console.log('Frozen at:', frozen.timestamp_iso);
  console.log('Hash:', frozen.universe_hash.substring(0, 16) + '...');
  console.log('Git ref:', frozen.git_ref);

  // Verify receipt
  const verification = await verifyReceipt(frozen, git, store);
  console.log('Verification:', verification.valid ? 'PASSED' : 'FAILED');

  return { store, git, frozen };
}

main();
```

---

## Verification

Run the complete example:

```bash
node tutorial-02-freeze.mjs
```

Expected output:
```
Event appended: 550e8400-e29b-41d4-a716-446655440000
Frozen at: 2025-12-27T10:30:00.123Z
Hash: blake3_abc123de...
Git ref: a1b2c3d4e5f6g7h8
Verification: PASSED
```

Check Git repository:
```bash
cd tutorial-02-repo
git log --oneline
# Shows: a1b2c3d Freeze snapshot at 2025-12-27T10:30:00.123Z
```

---

## Evidence

**Source Code:**
- KGCStore: `/home/user/unrdf/packages/kgc-4d/src/store.mjs`
- GitBackbone: `/home/user/unrdf/packages/kgc-4d/src/git.mjs`
- freezeUniverse: `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs:35`
- verifyReceipt: `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs` (verify function)

**Tests:**
- Freeze tests: `/home/user/unrdf/packages/kgc-4d/test/freeze.test.mjs`
- Store tests: `/home/user/unrdf/packages/kgc-4d/test/store.test.mjs`

**Examples:**
- Basic usage: `/home/user/unrdf/packages/kgc-4d/examples/basic-usage.mjs:13-48`

---

## Key Takeaways

1. **Three-graph model:** Universe (hot state), EventLog (history), System (metadata)
2. **Atomic events:** appendEvent() ensures ACID semantics
3. **Git backing:** Snapshots are standard Git commits (version control for knowledge graphs!)
4. **Cryptographic receipts:** BLAKE3 hash proves exact state at freeze time
5. **Verifiable:** Anyone can fetch Git commit and verify hash

---

## Troubleshooting

**Q: "Git not found" error?**  
A: KGC-4D uses isomorphic-git (pure JS), no Git CLI needed. Check GitBackbone initialization.

**Q: Hash different on re-run?**  
A: Hashes should be deterministic. Check that quads are sorted (freeze.mjs does this automatically).

**Q: What if universe is empty?**  
A: Empty universe freeze is valid! Creates genesis snapshot with hash of empty string.

---

## Next Steps

**Continue Learning:**
- [Tutorial 03: Time Travel](./03-time-travel.md) - Reconstruct this frozen state
- [Tutorial 04: Query Event Logs](./04-query-event-logs.md) - Query the immutable history
- [How-To 01: Freeze and Verify](../how-to/01-freeze-and-verify.md) - Production patterns

**Deep Dive:**
- [Explanation 03: Zero-Information Invariant](../explanation/03-zero-info-invariant.md) - Why this architecture?
- [Explanation 05: Why BLAKE3](../explanation/05-why-blake3.md) - Hash algorithm choice

**Reference:**
- [KGCStore API](../reference/kgc-store-api.md)
- [Receipt Schema](../reference/receipt-schema.md)

---

**Navigate:** [← Previous](./01-nanosecond-timestamps.md) | [Tutorials](./README.md) | [Next →](./03-time-travel.md)
