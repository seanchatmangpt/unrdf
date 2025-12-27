# KGC-Swarm User Guide

**Version**: 1.0.0
**Audience**: Developers, DevOps Engineers, Knowledge Engineers
**Prerequisites**: Node.js ≥ 18, pnpm, basic understanding of RDF

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Core Concepts](#core-concepts)
3. [API Reference](#api-reference)
4. [Usage Examples](#usage-examples)
5. [Integration with @unrdf/kgn Templates](#integration-with-unrdfkgn-templates)
6. [Configuration Options](#configuration-options)
7. [Troubleshooting](#troubleshooting)

---

## Quick Start

### Installation

```bash
# Install KGC-Swarm packages
pnpm add @unrdf/kgc-substrate @unrdf/kgc-runtime @unrdf/receipts

# Verify installation
pnpm test @unrdf/kgc-substrate
```

**Expected Output**:
```
✓ ReceiptChain: append block (5ms)
✓ ReceiptChain: verify chain integrity (3ms)
✓ TamperDetector: detect modifications (8ms)
...
Tests: 15 passed, 15 total
```

### 5-Minute Tutorial

**Step 1**: Create a knowledge store

```javascript
import { createStore } from '@unrdf/oxigraph';
import { KnowledgeStore } from '@unrdf/kgc-substrate';

const store = createStore();
const kgStore = new KnowledgeStore({ store });
```

**Step 2**: Initialize receipt chain

```javascript
import { ReceiptChain } from '@unrdf/kgc-substrate';

const chain = new ReceiptChain({
  genesis_hash: '0'.repeat(64),
  enforce_monotonic_time: true
});
```

**Step 3**: Add data and generate receipt

```javascript
import { dataFactory } from '@unrdf/oxigraph';

// Add some RDF triples
await kgStore.add(
  dataFactory.quad(
    dataFactory.namedNode('http://example.org/Alice'),
    dataFactory.namedNode('http://xmlns.com/foaf/0.1/knows'),
    dataFactory.namedNode('http://example.org/Bob')
  )
);

// Generate receipt
const receipt = await chain.append({
  agent_id: 'user-agent-1',
  toolchain_version: '1.0.0',
  artifacts: [
    {
      type: 'rdf',
      path: 'knowledge-graph.ttl',
      hash: await kgStore.getHash(),
      size_bytes: await kgStore.size()
    }
  ]
});

console.log('Receipt:', receipt);
```

**Output**:
```javascript
{
  block: {
    before_hash: '0000...0000',
    after_hash: 'a7f3...c8d1',
    timestamp_ns: 1735329600000000000n,
    agent_id: 'user-agent-1',
    toolchain_version: '1.0.0',
    artifacts: [ ... ]
  },
  index: 0,
  merkle_root: 'f4e2...9b3a'
}
```

**Step 4**: Verify integrity

```javascript
import { TamperDetector } from '@unrdf/kgc-substrate';

const detector = new TamperDetector(chain);
const isValid = detector.verify();

console.log('Chain valid:', isValid); // true
```

---

## Core Concepts

### Observable Space (O)

The **observable space** represents all possible data sources that the system can observe:

- **File System**: `O_file(path)` - observe files at given path
- **RDF Graphs**: `O_rdf(graph)` - observe RDF triple stores
- **SPARQL Endpoints**: `O_sparql(endpoint)` - observe remote SPARQL services
- **Receipt Chains**: `O_receipt(chain)` - observe existing receipt chains
- **Agent State**: `O_agent(state)` - observe agent internal state

**Example**:
```javascript
// Observe RDF file
const obs = {
  type: 'file',
  path: '/home/user/data.ttl',
  format: 'turtle'
};
```

### Compression Operator (μ)

The **compression operator** μ transforms observables into compressed artifacts:

```
μ : O → A
```

**Properties**:
- **Idempotent**: μ(μ(O)) = μ(O)
- **Deterministic**: Same input produces same output
- **Lossless**: Can reconstruct original from compressed form (with hash verification)

**Example**:
```javascript
import { compress } from '@unrdf/kgc-runtime';

const observable = await fs.readFile('large-dataset.ttl', 'utf8');
const compressed = compress(observable);

console.log('Original size:', observable.length);
console.log('Compressed size:', compressed.length);
console.log('Ratio:', compressed.length / observable.length);
```

### Guards (H)

**Guards** are predicates that enforce poka-yoke (mistake-proofing) boundaries:

```javascript
// Example: No N3 imports guard
const H_no_n3 = (code) => {
  return !code.includes("from 'n3'");
};

// Example: Pure function guard
const H_pure = (func) => {
  const source = func.toString();
  return !source.includes('fetch') &&
         !source.includes('Date.now') &&
         !source.includes('Math.random');
};
```

### Receipts (ρ)

**Receipts** are cryptographic proof chains that document all operations:

**Structure**:
```javascript
{
  before_hash: '0000...', // Previous block hash
  after_hash: 'a7f3...',  // Current block hash
  timestamp_ns: 1735329600000000000n,
  agent_id: 'agent-42',
  toolchain_version: '1.0.0',
  artifacts: [
    { type: 'code', path: 'src/app.mjs', hash: '...', size_bytes: 1024 }
  ]
}
```

**Key Properties**:
- **Tamper Evident**: Any modification invalidates chain (P_detect ≥ 1 - 2^-128)
- **Monotonic Time**: Timestamps must increase (enforces causality)
- **Chain Integrity**: Each block links to previous via hash

---

## API Reference

### ReceiptChain

**Constructor**:
```javascript
new ReceiptChain(config)
```

**Parameters**:
- `config.genesis_hash` (string, optional): Custom genesis hash (default: all zeros)
- `config.enforce_monotonic_time` (boolean, default: true): Enforce monotonic timestamps

**Methods**:

#### `append(blockData)`
Append a new block to the chain.

```javascript
await chain.append({
  agent_id: 'string',           // Required: Agent identifier
  toolchain_version: 'string',  // Required: Toolchain version
  artifacts: [                  // Required: List of artifacts
    {
      type: 'string',           // e.g., 'code', 'rdf', 'config'
      path: 'string',           // File path or identifier
      hash: 'string',           // SHA-256 hash (64 hex chars)
      size_bytes: number        // Size in bytes
    }
  ],
  timestamp_ns: bigint          // Optional: Custom timestamp (default: now)
});
```

**Returns**: `Promise<{ block, index, merkle_root }>`

**Throws**:
- `Error` if agent_id or toolchain_version missing
- `Error` if artifacts invalid
- `Error` if timestamp not monotonic (when enforced)

#### `getHeadHash()`
Get the hash of the most recent block.

```javascript
const headHash = chain.getHeadHash();
// Returns: '0000...0000' (genesis) or SHA-256 hash
```

#### `getLength()`
Get the number of blocks in the chain.

```javascript
const length = chain.getLength();
// Returns: number
```

#### `getBlock(index)`
Get a specific block by index.

```javascript
const block = chain.getBlock(0); // Get first block
// Returns: block object or null
```

#### `getAllBlocks()`
Get all blocks (defensive copy).

```javascript
const blocks = chain.getAllBlocks();
// Returns: Array of blocks
```

#### `toJSON()`
Serialize chain to JSON.

```javascript
const json = chain.toJSON();
// Returns: { genesis_hash, length, head_hash, blocks }
```

#### `static fromJSON(json)`
Deserialize chain from JSON.

```javascript
const chain = ReceiptChain.fromJSON(json);
```

#### `toBase64()`
Encode chain to base64 (for embedding).

```javascript
const encoded = chain.toBase64();
```

#### `static fromBase64(base64)`
Decode chain from base64.

```javascript
const chain = ReceiptChain.fromBase64(encoded);
```

---

### TamperDetector

**Constructor**:
```javascript
new TamperDetector(chain)
```

**Parameters**:
- `chain` (ReceiptChain): The chain to verify

**Methods**:

#### `verify()`
Verify entire chain integrity.

```javascript
const isValid = detector.verify();
// Returns: boolean
```

#### `verifyBlock(index)`
Verify a specific block.

```javascript
const isValid = detector.verifyBlock(5);
// Returns: boolean
```

---

### KnowledgeStore

**Constructor**:
```javascript
new KnowledgeStore({ store })
```

**Parameters**:
- `store` (Oxigraph Store): RDF store instance

**Methods**:

#### `add(quad)`
Add a quad to the store.

```javascript
await kgStore.add(quad);
```

#### `getHash()`
Compute SHA-256 hash of store contents.

```javascript
const hash = await kgStore.getHash();
// Returns: '64-character hex string'
```

#### `size()`
Get size of store in bytes.

```javascript
const bytes = await kgStore.size();
```

---

## Usage Examples

### Example 1: Multi-Agent Workflow

```javascript
import { ReceiptChain } from '@unrdf/kgc-substrate';

const chain = new ReceiptChain();

// Agent 1: Data collection
await chain.append({
  agent_id: 'collector-agent',
  toolchain_version: '1.0.0',
  artifacts: [
    { type: 'rdf', path: 'raw-data.ttl', hash: '...', size_bytes: 50000 }
  ]
});

// Agent 2: Data validation
await chain.append({
  agent_id: 'validator-agent',
  toolchain_version: '1.0.0',
  artifacts: [
    { type: 'report', path: 'validation.json', hash: '...', size_bytes: 2048 }
  ]
});

// Agent 3: Data transformation
await chain.append({
  agent_id: 'transformer-agent',
  toolchain_version: '1.0.0',
  artifacts: [
    { type: 'rdf', path: 'enriched-data.ttl', hash: '...', size_bytes: 75000 }
  ]
});

// Verify entire workflow
const detector = new TamperDetector(chain);
console.log('Workflow valid:', detector.verify());
```

### Example 2: Compression with Guards

```javascript
import { compress } from '@unrdf/kgc-runtime';
import { z } from 'zod';

// Define guard schema
const CodeSchema = z.object({
  content: z.string(),
  language: z.enum(['javascript', 'typescript']),
  purity: z.boolean()
});

// Guard: No side effects
const H_pure = (code) => {
  const forbidden = ['fetch', 'XMLHttpRequest', 'Date.now', 'Math.random'];
  return !forbidden.some(pattern => code.includes(pattern));
};

// Observable: Source code
const observable = {
  content: `
    export function add(a, b) {
      return a + b;
    }
  `,
  language: 'javascript',
  purity: true
};

// Validate against schema
CodeSchema.parse(observable);

// Check guard
if (!H_pure(observable.content)) {
  throw new Error('Guard violation: Code contains side effects');
}

// Compress
const compressed = compress(observable.content);
console.log('Compression ratio:', compressed.length / observable.content.length);
```

### Example 3: Receipt Chain Serialization

```javascript
import { ReceiptChain } from '@unrdf/kgc-substrate';
import fs from 'fs/promises';

// Create and populate chain
const chain = new ReceiptChain();
await chain.append({ agent_id: 'agent-1', toolchain_version: '1.0.0', artifacts: [] });
await chain.append({ agent_id: 'agent-2', toolchain_version: '1.0.0', artifacts: [] });

// Serialize to file
const json = chain.toJSON();
await fs.writeFile('receipt-chain.json', JSON.stringify(json, null, 2));

// Later: Load from file
const loadedJson = JSON.parse(await fs.readFile('receipt-chain.json', 'utf8'));
const loadedChain = ReceiptChain.fromJSON(loadedJson);

console.log('Loaded chain length:', loadedChain.getLength());
console.log('Head hash:', loadedChain.getHeadHash());
```

### Example 4: Idempotent Compression

```javascript
import { compress } from '@unrdf/kgc-runtime';

const data = 'Hello, World!'.repeat(1000);

// First compression
const compressed1 = compress(data);

// Second compression (should be identical - idempotence)
const compressed2 = compress(compressed1);

console.log('Original size:', data.length);
console.log('First compression:', compressed1.length);
console.log('Second compression:', compressed2.length);
console.log('Idempotent:', compressed1 === compressed2); // true
```

---

## Integration with @unrdf/kgn Templates

KGC-Swarm integrates seamlessly with Knowledge Graph Notation (KGN) templates for declarative knowledge graph construction.

### Basic Integration

```javascript
import { KnowledgeStore } from '@unrdf/kgc-substrate';
import { ReceiptChain } from '@unrdf/kgc-substrate';
import { renderKGN } from '@unrdf/kgn';
import { createStore, dataFactory } from '@unrdf/oxigraph';

// Step 1: Define KGN template
const template = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:Alice a foaf:Person ;
  foaf:name "Alice" ;
  foaf:knows ex:Bob .

ex:Bob a foaf:Person ;
  foaf:name "Bob" .
`;

// Step 2: Render KGN to quads
const quads = renderKGN(template);

// Step 3: Create knowledge store
const store = createStore();
const kgStore = new KnowledgeStore({ store });

// Step 4: Add quads to store
for (const quad of quads) {
  await kgStore.add(quad);
}

// Step 5: Generate receipt
const chain = new ReceiptChain();
const receipt = await chain.append({
  agent_id: 'kgn-renderer',
  toolchain_version: '1.0.0',
  artifacts: [
    {
      type: 'kgn',
      path: 'template.kgn',
      hash: await kgStore.getHash(),
      size_bytes: template.length
    }
  ]
});

console.log('Receipt:', receipt);
```

### Advanced: Multi-Template Composition

```javascript
// Template 1: Schema
const schemaTemplate = `
@prefix schema: <http://schema.org/> .
@prefix ex: <http://example.org/> .

ex:PersonClass a schema:Class ;
  schema:name "Person" ;
  schema:description "A human being" .
`;

// Template 2: Data
const dataTemplate = `
@prefix ex: <http://example.org/> .
@prefix schema: <http://schema.org/> .

ex:Alice a ex:PersonClass ;
  schema:name "Alice Smith" ;
  schema:age 30 .
`;

// Compose templates
const allQuads = [
  ...renderKGN(schemaTemplate),
  ...renderKGN(dataTemplate)
];

// Store with receipt chain
const chain = new ReceiptChain();

// Receipt for schema
await chain.append({
  agent_id: 'schema-loader',
  toolchain_version: '1.0.0',
  artifacts: [{ type: 'schema', path: 'schema.kgn', hash: '...', size_bytes: schemaTemplate.length }]
});

// Receipt for data
await chain.append({
  agent_id: 'data-loader',
  toolchain_version: '1.0.0',
  artifacts: [{ type: 'data', path: 'data.kgn', hash: '...', size_bytes: dataTemplate.length }]
});

console.log('Total blocks:', chain.getLength());
```

---

## Configuration Options

### Receipt Chain Configuration

```javascript
const chain = new ReceiptChain({
  // Genesis hash (default: all zeros)
  genesis_hash: '0'.repeat(64),

  // Enforce monotonic timestamps (default: true)
  // Set to false when loading historical chains
  enforce_monotonic_time: true
});
```

### Compression Configuration (σ, κ, B)

**σ (Size Function)**: Controls size measurement strategy

```javascript
// Default: byte count
const σ_bytes = (data) => Buffer.byteLength(data, 'utf8');

// Alternative: token count
const σ_tokens = (data) => data.split(/\s+/).length;

// Alternative: entropy-based
const σ_entropy = (data) => {
  const freq = {};
  for (const char of data) {
    freq[char] = (freq[char] || 0) + 1;
  }
  let entropy = 0;
  for (const count of Object.values(freq)) {
    const p = count / data.length;
    entropy -= p * Math.log2(p);
  }
  return entropy;
};
```

**κ (Complexity Measure)**: Controls compression aggressiveness

```javascript
// Default: Kolmogorov complexity approximation
const κ_kolmogorov = (data) => {
  // Approximate via LZ77 compression ratio
  const compressed = compress(data);
  return compressed.length;
};

// Alternative: Cyclomatic complexity (for code)
const κ_cyclomatic = (code) => {
  const ifCount = (code.match(/\bif\b/g) || []).length;
  const whileCount = (code.match(/\bwhile\b/g) || []).length;
  const forCount = (code.match(/\bfor\b/g) || []).length;
  return 1 + ifCount + whileCount + forCount;
};
```

**B (Boundary Specification)**: Controls guard enforcement

```javascript
const boundary = {
  pre: [
    // Pre-conditions (guards on input)
    (data) => data !== null && data !== undefined,
    (data) => typeof data === 'string',
    (data) => data.length < 1000000 // 1MB limit
  ],
  post: [
    // Post-conditions (guards on output)
    (result) => result !== null,
    (result) => result.hash.length === 64, // Valid SHA-256
    (result) => result.compressed.length <= result.original.length
  ]
};

// Enforce boundary
function compressWithBoundary(data) {
  // Check pre-conditions
  for (const guard of boundary.pre) {
    if (!guard(data)) {
      throw new Error('Pre-condition violation');
    }
  }

  // Perform compression
  const result = compress(data);

  // Check post-conditions
  for (const guard of boundary.post) {
    if (!guard(result)) {
      throw new Error('Post-condition violation');
    }
  }

  return result;
}
```

---

## Troubleshooting

### Problem: "Timestamp not monotonic" error

**Symptom**:
```
Error: ReceiptChain.append: Timestamp not monotonic (1735329600000000000 <= 1735329601000000000)
```

**Solution**:
```javascript
// Option 1: Ensure system clock is monotonic
// Use process.hrtime.bigint() for high-precision timestamps

const timestamp_ns = process.hrtime.bigint();
await chain.append({
  agent_id: 'agent',
  toolchain_version: '1.0.0',
  artifacts: [],
  timestamp_ns
});

// Option 2: Disable monotonic enforcement (for testing only)
const chain = new ReceiptChain({ enforce_monotonic_time: false });
```

### Problem: Hash mismatch after deserialization

**Symptom**:
```
Expected: a7f3...c8d1
Got:      b2e4...d9f2
```

**Solution**:
```javascript
// Ensure BigInt timestamps are properly serialized
const json = chain.toJSON();

// Convert BigInts to strings for JSON
const serialized = JSON.stringify(json, (key, value) =>
  typeof value === 'bigint' ? value.toString() : value
);

// When deserializing, convert back to BigInt
const deserialized = JSON.parse(serialized, (key, value) =>
  key === 'timestamp_ns' ? BigInt(value) : value
);

const chain2 = ReceiptChain.fromJSON(deserialized);
```

### Problem: Guard violations not caught at runtime

**Symptom**: Code with forbidden patterns passes through

**Solution**:
```javascript
// Use Zod for runtime validation
import { z } from 'zod';

const GuardedCodeSchema = z.object({
  content: z.string().refine(
    (code) => !code.includes("from 'n3'"),
    { message: "Guard violation: N3 import detected" }
  ),
  pure: z.boolean().refine(
    (isPure) => isPure === true,
    { message: "Guard violation: Code not pure" }
  )
});

// Validate before processing
try {
  GuardedCodeSchema.parse({ content: code, pure: true });
} catch (err) {
  console.error('Validation failed:', err.message);
}
```

### Problem: Receipt chain grows too large

**Symptom**: JSON serialization exceeds memory limits

**Solution**:
```javascript
// Option 1: Use streaming serialization
import { createWriteStream } from 'fs';
import { pipeline } from 'stream/promises';

const stream = createWriteStream('chain.jsonl');
for (const block of chain.getAllBlocks()) {
  stream.write(JSON.stringify(block) + '\n');
}
await stream.end();

// Option 2: Periodic checkpointing
const CHECKPOINT_INTERVAL = 1000; // blocks
if (chain.getLength() % CHECKPOINT_INTERVAL === 0) {
  const checkpoint = chain.toJSON();
  await fs.writeFile(`checkpoint-${chain.getLength()}.json`, JSON.stringify(checkpoint));

  // Start new chain from checkpoint
  const newChain = new ReceiptChain({ genesis_hash: chain.getHeadHash() });
}
```

### Problem: Compression ratio poor

**Symptom**: Compressed data nearly same size as original

**Solution**:
```javascript
// Check if data is compressible
const entropy = calculateEntropy(data);
if (entropy > 0.9) {
  console.log('Data has high entropy - already compressed or random');
  // Skip compression for high-entropy data
  return data;
}

// Use alternative compression for structured data
import { gzipSync } from 'zlib';

const compressed = gzipSync(data);
console.log('Compression ratio:', compressed.length / data.length);
```

---

## Best Practices

1. **Always verify chains after loading**:
   ```javascript
   const chain = ReceiptChain.fromJSON(json);
   const detector = new TamperDetector(chain);
   if (!detector.verify()) {
     throw new Error('Chain integrity compromised');
   }
   ```

2. **Use monotonic timestamps in production**:
   ```javascript
   const chain = new ReceiptChain({ enforce_monotonic_time: true });
   ```

3. **Hash artifacts before adding to chain**:
   ```javascript
   import { sha256 } from 'hash-wasm';

   const hash = await sha256(artifactContent);
   await chain.append({ ..., artifacts: [{ ..., hash }] });
   ```

4. **Implement guard composition**:
   ```javascript
   const guardAll = (...guards) => (data) => guards.every(g => g(data));

   const H_safe_code = guardAll(H_no_n3, H_pure, H_type_safe);
   ```

5. **Checkpoint large chains periodically**:
   ```javascript
   if (chain.getLength() > 10000) {
     await saveCheckpoint(chain);
   }
   ```

---

## Further Reading

- **Formal Specification**: See `formal-specification.md` for mathematical foundations
- **Implementation Notes**: See `implementation-notes.md` for architecture decisions
- **Package Documentation**: See `/home/user/unrdf/packages/kgc-substrate/README.md`
- **Test Examples**: See `/home/user/unrdf/packages/kgc-substrate/test/*.test.mjs`

---

**Document Version**: 1.0.0
**Last Updated**: 2025-12-27
**Maintainer**: UNRDF Team
**License**: See repository LICENSE
