# How-To: Work with HDIT Coordinates

**Problem:** You need to find similar events, cluster events, or visualize event relationships.

**Solution:** Use HDIT (Hyperdimensional Information Theory) coordinates for event similarity.

**Time:** 25 minutes

---

## Prerequisites

- Understanding of vector embeddings (helpful)
- KGC-4D events

---

## Pattern: Event Similarity Search

### 1. Generate Coordinates

```javascript
import { coordsForEvent, createUniverseContext } from '@unrdf/kgc-4d';

const context = createUniverseContext(256); // 256 dimensions

const event1 = { type: 'CREATE', payload: { subject: 'Alice' } };
const coords1 = coordsForEvent(event1, context);

console.log('Coordinates:', coords1); // Float32Array[256]
```

### 2. Find Similar Events

```javascript
import { findKNearest, cosineSimilarity } from '@unrdf/kgc-4d';

const allCoords = events.map(e => coordsForEvent(e, context));
const queryCoords = coords1;

const nearest = findKNearest(queryCoords, allCoords, 5); // Top 5 similar
console.log('Nearest events:', nearest);
```

### 3. Cluster Events

```javascript
import { clusterProjection, projectPCA } from '@unrdf/kgc-4d';

const projected = projectPCA(allCoords, 2); // 2D projection
const clusters = clusterProjection(projected, 5); // 5 clusters
console.log('Clusters:', clusters);
```

---

## Evidence

**Source:** `/home/user/unrdf/packages/kgc-4d/src/hdit/index.mjs`  
**Tests:** `/home/user/unrdf/packages/kgc-4d/test/hdit/`

---

## Related

- [Reference: HDIT System](../reference/hdit-system.md)
- [Explanation: Performance Tradeoffs](../../explanation/performance-tradeoffs.md)
