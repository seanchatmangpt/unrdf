# Reference: HDIT System

**Source:** `/home/user/unrdf/packages/kgc-4d/src/hdit/index.mjs`

---

## Hyperdimensional Information Theory

Functions for event similarity, clustering, and visualization using vector coordinates.

---

## Core Functions

### coordsForEvent()

Generate hyperdimensional coordinates for an event.

**Signature:**
```javascript
coordsForEvent(event: object, context: UniverseContext): Float32Array
```

---

### cosineSimilarity()

Calculate cosine similarity between two coordinate vectors.

**Signature:**
```javascript
cosineSimilarity(a: Float32Array, b: Float32Array): number
```

**Returns:** Similarity score [0, 1] (1 = identical)

---

### findKNearest()

Find K nearest neighbors.

**Signature:**
```javascript
findKNearest(query: Float32Array, candidates: Float32Array[], k: number): Array<{ index: number, distance: number }>
```

---

### projectPCA()

Project to lower dimensions using PCA.

**Signature:**
```javascript
projectPCA(coords: Float32Array[], targetDim: number): Float32Array[]
```

---

## Related

- [How-To 06: Work with HDIT](../how-to/06-work-with-hdit.md)
