# How-To 04: Integrate with Existing Graphs

**Objective:** Integrate UNRDF with existing RDF triple stores and knowledge graphs without data migration.

**Audience:** Intermediate - architects integrating UNRDF into existing systems

**Prerequisites:**
- Understanding of RDF triple stores (N3, Apache Jena, etc.)
- Familiarity with SPARQL endpoints
- **Capability Atoms:** `@unrdf/federation` (federated queries), `@unrdf/core` (adapters)

**Estimated Time:** 25 minutes

---

## Problem

You need to:
- Connect UNRDF to existing triple stores
- Query multiple graphs without migration
- Add UNRDF features (hooks, receipts) to existing data
- Maintain existing workflows
- Gradual migration strategy

---

## Solution

Use UNRDF's federation and adapter patterns to integrate with existing systems.

---

## Step-by-Step Guide

### 1. Connect to Existing Store

**[Placeholder - Content to be filled]**

```javascript
import { createFederatedSystem } from '@unrdf/federation';
import { sparqlEndpointAdapter } from '@unrdf/core';

// Connect to existing triple store
const existingStore = sparqlEndpointAdapter({
  endpoint: 'http://localhost:3030/dataset/query',
  updateEndpoint: 'http://localhost:3030/dataset/update'
});

const federated = createFederatedSystem({
  stores: [existingStore]
});
```

**Evidence:** Adapters at `/home/user/unrdf/packages/federation/src/adapters.mjs`

---

### 2. Query Across Stores

**[Placeholder - Content to be filled]**

```javascript
// Query both UNRDF and existing store
const results = await federated.query(`
  SELECT ?person ?skill WHERE {
    # From existing store
    SERVICE <existing-store> {
      ?person :hasSkill ?skill .
    }
    # From UNRDF store
    SERVICE <unrdf-store> {
      ?person :verified true .
    }
  }
`);
```

**Evidence:** Federated queries at `/home/user/unrdf/packages/federation/src/federated-query.mjs`

---

### 3. Add UNRDF Features

**[Placeholder - Content to be filled]**

```javascript
// Add hooks to existing data without migration
import { defineHook } from '@unrdf/hooks';

const validationHook = defineHook({
  meta: { name: 'validate-existing-data' },
  channel: {
    graphs: ['existing-store']  // Target existing store
  },
  when: {
    kind: 'sparql-ask',
    ref: { /* ... */ }
  },
  run: async (context) => {
    // Validate data in existing store
  }
});
```

**Evidence:** Cross-store hooks at `/home/user/unrdf/packages/hooks/src/cross-store-hooks.mjs`

---

### 4. Gradual Migration Strategy

**[Placeholder - Content to be filled]**

```javascript
// Phase 1: Read-only federation
// Phase 2: Add UNRDF features
// Phase 3: Migrate critical data
// Phase 4: Full migration

const migrationPlan = {
  phase1: { /* read-only */ },
  phase2: { /* add hooks */ },
  phase3: { /* migrate data */ },
  phase4: { /* switch primary */ }
};
```

**Evidence:** Migration guide at `/home/user/unrdf/packages/federation/src/migration-strategy.mjs`

---

### 5. Maintain Compatibility

**[Placeholder - Content to be filled]**

```javascript
// Keep existing SPARQL endpoint functional
import { createSparqlProxy } from '@unrdf/federation';

const proxy = createSparqlProxy({
  backend: existingStore,
  enhanceWith: unrdfStore,
  transparentMode: true
});

// Existing clients continue to work
```

**Evidence:** Proxy at `/home/user/unrdf/packages/federation/src/sparql-proxy.mjs`

---

## Complete Example

**[Placeholder - Link to complete example]**

**Evidence:** Full example at `/home/user/unrdf/examples/integration-existing.mjs`

---

## Common Issues

**[Placeholder - Troubleshooting]**

- Issue: "Endpoint authentication failed"
- Issue: "Federated query timeout"
- Issue: "Data synchronization conflict"

---

## Related Guides

- **[How-To 03: Measure Query Performance](./03-measure-query-performance.md)** - Optimize federated queries
- **[Explanation 03: Cross-Runtime Bridging](../explanation/cross-runtime-bridging.md)** - Runtime strategies
- **[Reference: Package Exports](../reference/package-exports.md)** - Federation APIs

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
