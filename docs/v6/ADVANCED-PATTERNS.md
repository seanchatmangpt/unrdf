# UNRDF v6 Advanced Usage Patterns

**Target Audience**: Developers familiar with UNRDF basics
**Prerequisites**: Complete [Quick Start Guide](/home/user/unrdf/docs/v6/QUICK-START.md)
**Time**: 30-45 minutes

This guide demonstrates advanced UNRDF v6 patterns including delta proposals, streaming, federation, and production-ready architectures.

---

## Table of Contents

1. [Delta-Based Versioning](#1-delta-based-versioning)
2. [Streaming Large Graphs](#2-streaming-large-graphs)
3. [Federated Queries](#3-federated-queries)
4. [Receipt Chains and Verification](#4-receipt-chains-and-verification)
5. [Production Patterns](#5-production-patterns)
6. [Performance Optimization](#6-performance-optimization)

---

## 1. Delta-Based Versioning

### Overview

UNRDF v6 introduces delta proposals for explicit, auditable version transitions.

### Basic Delta Creation

```javascript
import { createStore } from '@unrdf/oxigraph';
import { createDeltaProposal, applyDelta } from '@unrdf/v6-core/delta';
import { dataFactory } from '@unrdf/core/rdf';

const store = await createStore();
const { namedNode, literal, quad } = dataFactory;

// Initial state (v1.0)
await store.add(
  quad(
    namedNode('http://example.org/Alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice')
  )
);

// Create delta proposal for v1.0 → v1.1
const proposal = createDeltaProposal('v1.0', 'v1.1', [
  // Update name
  {
    type: 'remove',
    quad: {
      subject: 'http://example.org/Alice',
      predicate: 'http://xmlns.com/foaf/0.1/name',
      object: 'Alice'
    }
  },
  {
    type: 'add',
    quad: {
      subject: 'http://example.org/Alice',
      predicate: 'http://xmlns.com/foaf/0.1/name',
      object: 'Alice Smith'
    }
  },
  // Add email
  {
    type: 'add',
    quad: {
      subject: 'http://example.org/Alice',
      predicate: 'http://xmlns.com/foaf/0.1/mbox',
      object: 'alice@example.org'
    }
  }
]);

// Apply delta with receipt
const receipt = await applyDelta(store, proposal);

console.log('Delta applied:', receipt.id);
console.log('Version:', proposal.from, '→', proposal.to);
```

### Delta with Conflict Detection

```javascript
import { createDeltaProposal, applyDelta, detectConflicts } from '@unrdf/v6-core/delta';

// Concurrent deltas
const deltaA = createDeltaProposal('v1.0', 'v1.1-branch-a', [
  { type: 'add', quad: { /* ... */ } }
]);

const deltaB = createDeltaProposal('v1.0', 'v1.1-branch-b', [
  { type: 'add', quad: { /* ... */ } }
]);

// Detect conflicts
const conflicts = detectConflicts([deltaA, deltaB]);

if (conflicts.length > 0) {
  console.log('Conflicts detected:', conflicts);
  // Resolve manually or use merge strategy
} else {
  await applyDelta(store, deltaA);
  await applyDelta(store, deltaB);
}
```

### Delta Storage and Retrieval

```javascript
import { adapters } from '@unrdf/v6-core/delta';

// Use memory adapter (or filesystem, network)
const adapter = new adapters.MemoryAdapter();

// Store delta
const deltaId = await adapter.store(proposal);
console.log('Delta stored:', deltaId);

// Retrieve delta
const retrieved = await adapter.retrieve(deltaId);
console.log('Retrieved delta:', retrieved.from, '→', retrieved.to);

// List all deltas
const allDeltas = await adapter.list();
console.log('Total deltas:', allDeltas.length);
```

**Use Cases**:
- Version control for knowledge graphs
- Rollback capabilities
- Audit trails
- Collaborative editing

---

## 2. Streaming Large Graphs

### AsyncIterator-Based Streaming

UNRDF v6 uses modern AsyncIterator API for streaming:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { createReadStream } from '@unrdf/streaming';

const store = await createStore();

// Stream triples from file
const stream = createReadStream('/path/to/large-graph.ttl', {
  format: 'turtle',
  bufferSize: 1000 // Process in batches
});

let count = 0;

for await (const quad of stream) {
  await store.add(quad);
  count++;

  if (count % 10000 === 0) {
    console.log(`Processed ${count} triples...`);
  }
}

console.log(`✅ Loaded ${count} triples`);

// Access receipt
const receipt = await stream.receipt();
console.log('Stream receipt:', receipt.merkleRoot);
```

### Streaming with Transformation

```javascript
import { createReadStream, transform } from '@unrdf/streaming';
import { dataFactory } from '@unrdf/core/rdf';

const { namedNode } = dataFactory;

// Stream with transformation pipeline
const stream = createReadStream('/path/to/data.ttl')
  .pipe(
    transform(quad => {
      // Filter: only keep foaf:Person triples
      if (quad.predicate.value.includes('type') &&
          quad.object.value.includes('Person')) {
        return quad;
      }
      return null; // Skip
    })
  )
  .pipe(
    transform(quad => {
      // Transform: rename namespace
      return {
        ...quad,
        subject: namedNode(quad.subject.value.replace('old.org', 'new.org'))
      };
    })
  );

for await (const quad of stream) {
  console.log('Transformed quad:', quad);
}
```

### Backpressure Handling

```javascript
import { createReadStream, createWriteStream } from '@unrdf/streaming';

const readStream = createReadStream('/path/to/input.ttl');
const writeStream = createWriteStream('/path/to/output.nt', {
  format: 'ntriples'
});

// Automatic backpressure handling
await readStream.pipe(writeStream);

console.log('✅ Stream completed with backpressure control');
```

**Use Cases**:
- Loading massive datasets (>1M triples)
- ETL pipelines
- Real-time data processing
- Memory-constrained environments

---

## 3. Federated Queries

### Query Multiple Stores

```javascript
import { createStore } from '@unrdf/oxigraph';
import { Federation, sparql } from '@unrdf/federation';

// Create multiple stores
const store1 = await createStore();
const store2 = await createStore();

// Populate stores
// ... (add data to store1 and store2)

// Create federation
const federation = new Federation([store1, store2]);

// Query across all stores
const results = await federation.query(
  sparql`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?person ?name
    WHERE {
      ?person foaf:name ?name .
    }
  `
    .timeout(5000) // 5-second timeout
    .receipt(true) // Generate receipt
);

for (const binding of results) {
  console.log('Person:', binding.get('name')?.value);
}

// Access receipt
const receipt = results.receipt();
console.log('Query receipt:', receipt.id);
```

### Remote SPARQL Endpoints

```javascript
import { Federation, RemoteEndpoint } from '@unrdf/federation';

// Mix local and remote stores
const localStore = await createStore();
const remoteEndpoint = new RemoteEndpoint('https://dbpedia.org/sparql');

const federation = new Federation([localStore, remoteEndpoint]);

// Query both local and remote data
const results = await federation.query(
  sparql`
    SELECT ?label
    WHERE {
      ?resource rdfs:label ?label .
      FILTER (lang(?label) = "en")
    }
  `.timeout(10000)
);
```

### Federated Joins

```javascript
import { Federation, sparql } from '@unrdf/federation';

const userStore = await createStore(); // User data
const productStore = await createStore(); // Product data

const federation = new Federation([userStore, productStore], {
  optimizeJoins: true // Enable join optimization
});

// Cross-store join
const results = await federation.query(
  sparql`
    PREFIX ex: <http://example.org/>

    SELECT ?userName ?productName
    WHERE {
      # From userStore
      ?user ex:name ?userName ;
            ex:purchased ?product .

      # From productStore
      ?product ex:productName ?productName .
    }
  `.timeout(5000)
);
```

**Use Cases**:
- Distributed knowledge graphs
- Multi-tenant applications
- Hybrid local/remote queries
- Microservices integration

---

## 4. Receipt Chains and Verification

### Building Receipt Chains

```javascript
import { createReceipt, verifyReceipt, MerkleTree } from '@unrdf/v6-core/receipts';

const receipts = [];

// Operation 1
const receipt1 = createReceipt('operation-1', { data: 'value1' });
receipts.push(receipt1);

// Operation 2 (links to receipt1)
const receipt2 = createReceipt('operation-2', {
  data: 'value2',
  previousReceipt: receipt1.id
});
receipts.push(receipt2);

// Operation 3 (links to receipt2)
const receipt3 = createReceipt('operation-3', {
  data: 'value3',
  previousReceipt: receipt2.id
});
receipts.push(receipt3);

console.log('Receipt chain:', receipts.map(r => r.id));
```

### Verifying Receipt Chains

```javascript
import { verifyReceipt, verifyChain } from '@unrdf/v6-core/receipts';

// Verify individual receipt
const isValid = verifyReceipt(receipt1);
console.log('Receipt valid:', isValid);

// Verify entire chain
const chainValid = verifyChain(receipts);
console.log('Chain valid:', chainValid);

// Detect tampering
receipts[1].metadata.data = 'modified'; // Tamper with data
const tampered = verifyChain(receipts);
console.log('Tampered chain valid:', tampered); // false
```

### Merkle Proof Verification

```javascript
import { MerkleTree } from '@unrdf/v6-core/receipts';

// Create Merkle tree from operations
const operations = ['op1', 'op2', 'op3', 'op4'];
const tree = new MerkleTree(operations);

console.log('Merkle root:', tree.root);

// Generate proof for specific operation
const proof = tree.getProof(2); // Proof for 'op3'

// Verify proof
const verified = MerkleTree.verify('op3', proof, tree.root);
console.log('Proof verified:', verified);

// Store only root + proofs (not full tree)
const compactReceipt = {
  merkleRoot: tree.root,
  operation: 'op3',
  proof
};
```

**Use Cases**:
- Audit trails
- Compliance reporting
- Tamper detection
- Deterministic replay

---

## 5. Production Patterns

### Error Handling

```javascript
import { z } from 'zod';
import { createStore } from '@unrdf/oxigraph';
import { ZodError } from 'zod';

const TripleSchema = z.object({
  subject: z.string().url(),
  predicate: z.string().url(),
  object: z.string().min(1)
});

async function addTripleSafely(data) {
  try {
    // Validate input
    const validated = TripleSchema.parse(data);

    // Add to store with timeout
    const timeout = setTimeout(() => {
      throw new Error('Operation timeout (5s)');
    }, 5000);

    const store = await createStore();
    await store.add(/* ... */);

    clearTimeout(timeout);

    return { success: true };
  } catch (error) {
    if (error instanceof ZodError) {
      console.error('Validation error:', error.errors);
      return { success: false, error: 'Invalid input' };
    } else if (error.message.includes('timeout')) {
      console.error('Timeout error:', error);
      return { success: false, error: 'Operation timed out' };
    } else {
      console.error('Unknown error:', error);
      return { success: false, error: 'Internal error' };
    }
  }
}
```

### Timeout Guards (Andon Principle)

```javascript
import { withTimeout } from '@unrdf/v6-core';

// Wrap operations with timeout
const result = await withTimeout(
  async () => {
    const results = await executeSparql(store, query);
    return results;
  },
  5000 // 5-second timeout
);

// If timeout exceeded, throws TimeoutError
```

### Configuration Management

```javascript
import { z } from 'zod';

const ConfigSchema = z.object({
  store: z.object({
    backend: z.enum(['memory', 'sqlite']),
    path: z.string().optional()
  }),
  query: z.object({
    defaultTimeout: z.number().int().positive().default(5000),
    maxResults: z.number().int().positive().default(1000)
  }),
  receipts: z.object({
    enabled: z.boolean().default(true),
    storage: z.enum(['memory', 'filesystem', 'network'])
  })
});

// Load and validate config
const config = ConfigSchema.parse({
  store: { backend: 'memory' },
  query: { defaultTimeout: 10000 },
  receipts: { enabled: true, storage: 'filesystem' }
});

// Use throughout application
const store = await createStore({
  backend: config.store.backend,
  path: config.store.path
});
```

### Logging and Monitoring

```javascript
import { createReceipt } from '@unrdf/v6-core/receipts';

class MonitoredStore {
  constructor(store) {
    this.store = store;
    this.metrics = {
      operations: 0,
      errors: 0,
      avgLatency: 0
    };
  }

  async add(quad) {
    const start = Date.now();
    const receipt = createReceipt('store-add', { quad });

    try {
      await this.store.add(quad);
      this.metrics.operations++;
      const latency = Date.now() - start;
      this.metrics.avgLatency =
        (this.metrics.avgLatency * (this.metrics.operations - 1) + latency) /
        this.metrics.operations;

      console.log(`✅ [${receipt.id}] Added quad (${latency}ms)`);
      return receipt;
    } catch (error) {
      this.metrics.errors++;
      console.error(`❌ [${receipt.id}] Error:`, error);
      throw error;
    }
  }

  getMetrics() {
    return this.metrics;
  }
}

// Usage
const store = new MonitoredStore(await createStore());
await store.add(/* ... */);
console.log('Metrics:', store.getMetrics());
```

**Use Cases**:
- Production deployments
- Error tracking
- Performance monitoring
- Compliance logging

---

## 6. Performance Optimization

### Batch Operations

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = await createStore();

// ❌ Slow: Individual inserts
for (const quad of quads) {
  await store.add(quad);
}

// ✅ Fast: Batch insert
await store.addAll(quads); // 10-100x faster
```

### Query Optimization

```javascript
import { executeSparql } from '@unrdf/core/sparql';

// ❌ Slow: Inefficient query
const slowQuery = `
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
    FILTER regex(str(?s), "Alice")
  }
`;

// ✅ Fast: Use specific predicates
const fastQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name
  WHERE {
    ?person foaf:name "Alice" .
  }
`;
```

### Memory Management

```javascript
import { createStore } from '@unrdf/oxigraph';

// Use persistent backend for large datasets
const store = await createStore({
  backend: 'sqlite',
  path: '/path/to/data.db',
  options: {
    cacheSize: 10000 // Tune cache size
  }
});

// Stream instead of loading all at once
const stream = createReadStream('/path/to/huge-graph.ttl');
for await (const quad of stream) {
  await store.add(quad);
  // Memory stays constant
}
```

### Connection Pooling

```javascript
import { createStore } from '@unrdf/oxigraph';

class StorePool {
  constructor(size = 5) {
    this.size = size;
    this.pool = [];
    this.initialize();
  }

  async initialize() {
    for (let i = 0; i < this.size; i++) {
      this.pool.push(await createStore());
    }
  }

  acquire() {
    if (this.pool.length === 0) {
      throw new Error('Pool exhausted');
    }
    return this.pool.pop();
  }

  release(store) {
    this.pool.push(store);
  }
}

// Usage
const pool = new StorePool(10);
const store = pool.acquire();
try {
  // Use store
  await store.add(/* ... */);
} finally {
  pool.release(store);
}
```

**Performance Targets**:
- SPARQL queries: <1ms for simple, <10ms for complex
- Triple insertion: <0.3μs per triple (batched)
- Memory usage: <100MB for 1M triples (persistent backend)

---

## Complete Advanced Example

```javascript
/**
 * UNRDF v6 Advanced Example
 * Demonstrates: Deltas, streaming, federation, receipts, and production patterns
 */

import { z } from 'zod';
import { createStore } from '@unrdf/oxigraph';
import { executeSparql } from '@unrdf/core/sparql';
import { createDeltaProposal, applyDelta } from '@unrdf/v6-core/delta';
import { createReceipt, verifyChain } from '@unrdf/v6-core/receipts';
import { Federation, sparql } from '@unrdf/federation';
import { createReadStream } from '@unrdf/streaming';

// --- Configuration ---

const ConfigSchema = z.object({
  timeout: z.number().default(5000),
  batchSize: z.number().default(1000)
});

const config = ConfigSchema.parse({
  timeout: 10000,
  batchSize: 5000
});

// --- Initialize Stores ---

const store1 = await createStore({ backend: 'memory' });
const store2 = await createStore({ backend: 'memory' });

console.log('🚀 UNRDF v6 Advanced Example\n');

// --- Stream Large Dataset ---

const stream = createReadStream('/path/to/data.ttl', {
  bufferSize: config.batchSize
});

let count = 0;
for await (const quad of stream) {
  await store1.add(quad);
  count++;
}

console.log(`✅ Loaded ${count} triples via streaming\n`);

// --- Delta Versioning ---

const delta = createDeltaProposal('v1.0', 'v1.1', [
  { type: 'add', quad: { /* ... */ } }
]);

const deltaReceipt = await applyDelta(store1, delta);
console.log(`✅ Applied delta: ${delta.from} → ${delta.to}\n`);

// --- Federated Query ---

const federation = new Federation([store1, store2]);

const results = await federation.query(
  sparql`SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10`
    .timeout(config.timeout)
    .receipt(true)
);

console.log(`🔍 Federated query returned ${results.length} results\n`);

// --- Verify Receipt Chain ---

const receipts = [deltaReceipt, results.receipt()];
const chainValid = verifyChain(receipts);

console.log(`📝 Receipt chain valid: ${chainValid}\n`);
console.log('✨ Advanced example complete!\n');
```

---

## Next Steps

- **[API Reference](/home/user/unrdf/docs/v6/API-REFERENCE.md)** - Complete API documentation
- **[Migration Guide](/home/user/unrdf/docs/v6/MIGRATION_PLAN.md)** - Upgrade from v5
- **[Core Concepts](/home/user/unrdf/docs/v6/CORE-CONCEPTS.md)** - Deep dive into v6 architecture
- **[Examples](/home/user/unrdf/examples/)** - More code examples

---

**Questions?** Open an issue on [GitHub](https://github.com/unrdf/unrdf/issues) 🚀
