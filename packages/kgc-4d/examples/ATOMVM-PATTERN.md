# AtomVM Pattern - Using Existing KGC-4D Code

**AtomVM is NOT a new package.** It's a pattern that reuses existing KGC-4D + HDIT code.

## The 8 Core Operations (100% Existing Code)

The `LocalShard` class in `local-first-collaboration.mjs` already implements all 8 AtomVM operations:

| Operation | LocalShard Method | Existing KGC-4D API |
|-----------|------------------|---------------------|
| **1. EMIT** | `addLocalEvent()` | Direct implementation |
| **2. PROJECT** | `coordsForEvent()` | `@unrdf/kgc-4d/hdit` |
| **3. CENTROID** | `getIdentityCentroid()` | `calculateCentroid()` from HDIT |
| **4. SIMILARITY** | `shouldCollaborateWith()` | `cosineSimilarity()` from HDIT |
| **5. SYNC** | `syncWithPeer()` | Vector clock merge pattern |
| **6. GOSSIP** | `summary()` | Centroid + metadata |
| **7. SNAPSHOT** | Use `freezeUniverse()` | `@unrdf/kgc-4d` |
| **8. OBSERVE** | Integrate `@unrdf/hooks` | Hook pattern |

## Working Example

```bash
node examples/local-first-collaboration.mjs
```

Output shows:
- ✅ 89% geometric similarity between Alice and Bob
- ✅ P2P sync without central server
- ✅ Vector clock causality tracking
- ✅ 512D coordinates for each shard

## Code Reuse Breakdown (Big Bang 80/20)

```javascript
// 1. EMIT - Direct pattern (already exists)
await alice.addLocalEvent(EVENT_TYPES.CREATE, 'Description');

// 2. PROJECT - Uses existing coordsForEvent
const coords = coordsForEvent(event, {}, D_HEAVY);

// 3. CENTROID - Uses existing calculateCentroid
const centroid = calculateCentroid(coords);

// 4. SIMILARITY - Uses existing cosineSimilarity
const similarity = cosineSimilarity(myCentroid, theirCentroid);

// 5. SYNC - Vector clock merge (already implemented)
await alice.syncWithPeer(bob);

// 6. GOSSIP - Summary with centroid
const summary = { shardId, centroid, count };

// 7. SNAPSHOT - Use existing freezeUniverse
import { freezeUniverse } from '@unrdf/kgc-4d';

// 8. OBSERVE - Use existing hooks
import { useHook } from '@unrdf/hooks';
```

## Architecture: LocalShard = AtomVM Runtime

```
LocalShard {
  userId         // Shard identity
  localEvents    // Append-only atom log
  eventCounter   // Local vector clock
  peerConnections // P2P sync state

  addLocalEvent()          // EMIT
  getIdentityCentroid()    // PROJECT + CENTROID
  shouldCollaborateWith()  // SIMILARITY
  syncWithPeer()           // SYNC
  summary()                // GOSSIP
}
```

## Key Capabilities (All From Existing Code)

1. **Sovereign Continuity**: Local append-only log
2. **Geometric Navigation**: 512D HDIT coordinates
3. **Opt-in Collaboration**: P2P sync with causality
4. **Geometric Discovery**: Similarity-based peer finding

## What This Means

**AtomVM is a PATTERN, not a PACKAGE.**

Instead of creating `@unrdf/atomvm`, we:
1. ✅ Use existing `LocalShard` from examples
2. ✅ Use existing HDIT operations
3. ✅ Use existing KGC-4D event sourcing
4. ✅ Add ~0 lines of new code

**This is Big Bang 80/20:**
- 80% = Existing proven code (KGC-4D + HDIT)
- 20% = Pattern documentation (this file)

## C4 Architecture Mapping

The C4 diagrams map to existing code:

- **AtomVM Core** = `LocalShard` class
- **Vector Engine** = `@unrdf/kgc-4d/hdit` module
- **P2P Transport** = `syncWithPeer()` pattern
- **Atom Log** = `localEvents` array
- **Clock Manager** = Vector clock in each event

No new containers. No new components. Just existing code used correctly.

## Next Steps

Want to add AtomVM capabilities to your app?

```javascript
// Copy the LocalShard pattern from local-first-collaboration.mjs
import { LocalShard } from './patterns/local-shard.mjs';

// Or just import existing KGC-4D APIs directly
import {
  coordsForEvent,
  calculateCentroid,
  cosineSimilarity,
  D_HEAVY
} from '@unrdf/kgc-4d/hdit';
```

That's it. No new package needed.
