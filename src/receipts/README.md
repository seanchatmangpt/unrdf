# UNRDF Receipt & Provenance System

Cryptographic audit trails for admissibility decisions, validation runs, and projection operations.

## Overview

The receipt system provides **immutable, deterministic, cryptographic proofs** for all UNRDF operations requiring audit trails:

- ✅ **Deterministic**: Same inputs → Same receipt hash
- ✅ **Immutable**: Object.freeze() prevents tampering
- ✅ **Chainable**: beforeHash → afterHash linkage
- ✅ **Batchable**: Merkle tree for efficient proof-of-inclusion
- ✅ **Reproducible**: Toolchain versions recorded

## Modules

### 1. `receipt.mjs` - Receipt Class

Core immutable receipt with cryptographic binding.

```javascript
import { Receipt } from './receipt.mjs';

const receipt = await Receipt.create({
  inputHashes: {
    ontologyReleases: ['hash1', 'hash2'],
    deltaCapsule: 'hash3'
  },
  decision: 'allow',
  outputHash: 'hash4',
  toolchainVersion: {
    node: 'v18.19.0',
    packages: { '@unrdf/core': '^5.0.1' }
  }
});

console.log(receipt.receiptHash);  // Deterministic BLAKE3 hash
console.log(receipt.epoch);         // τ_2025_12_26_1430_123
```

**Features**:
- Auto-generates epoch τ (deterministic timestamp)
- Computes BLAKE3 hash of canonical JSON
- Immutable (Object.freeze)
- Serializes to JSON-LD and Turtle (TTL)

### 2. `receipt-chain.mjs` - Receipt Chain

Maintain cryptographic chain across epochs.

```javascript
import { ReceiptChain } from './receipt-chain.mjs';

const chain = new ReceiptChain();

await chain.append(receipt1);  // beforeHash = null
await chain.append(receipt2);  // beforeHash = receipt1.receiptHash
await chain.append(receipt3);  // beforeHash = receipt2.receiptHash

const { valid, errors } = await chain.verify();
console.log('Chain valid:', valid);
```

**Invariants**:
- `chain[i].beforeHash === chain[i-1].receiptHash`
- Epochs monotonically increasing
- All receipt hashes valid

### 3. `merkle-root.mjs` - Merkle Tree Batching

Batch N receipts into single Merkle root.

```javascript
import { computeMerkleRoot, generateMerkleProof, verifyMerkleProof } from './merkle-root.mjs';

// Batch receipts
const hashes = receipts.map(r => r.receiptHash);
const merkleRoot = await computeMerkleRoot(hashes);

// Generate proof for receipt at index 5
const { root, proof } = await generateMerkleProof(hashes, 5);

// Verify proof
const isValid = await verifyMerkleProof(hashes[5], root, proof);
```

**Use cases**:
- Efficient storage (1 root vs N receipts)
- Proof of inclusion (log N verification)
- Tamper detection

### 4. `receipt-generator.mjs` - Receipt Generator

Stateful generator maintaining receipt chain.

```javascript
import { ReceiptGenerator } from './receipt-generator.mjs';

const generator = new ReceiptGenerator();

// Emit admissibility receipt
const receipt1 = await generator.emitAdmissibilityReceipt({
  ontologyReleases: ['ont1', 'ont2'],
  deltaCapsule: 'delta1',
  decision: 'allow',
  universeState: { classes: ['Person', 'Org'] }
});

// Emit validation receipt (auto-linked)
const receipt2 = await generator.emitValidationReceipt({
  ontologyReleases: ['ont1', 'ont2'],
  validationReport: 'report1',
  decision: 'allow',
  validationState: { violations: 0 }
});

console.log(receipt2.beforeHash === receipt1.receiptHash);  // true
```

**Features**:
- Auto-links receipts (beforeHash)
- Auto-collects toolchain versions
- Auto-computes output hashes
- Maintains receipt chain

## Data Model

### Receipt Structure

```typescript
{
  inputHashes: {
    ontologyReleases: string[],  // Ontology release hashes
    deltaCapsule: string         // Δ capsule hash
  },
  decision: 'allow' | 'deny',    // Admissibility decision
  epoch: string,                  // τ_YYYY_MM_DD_HHMM_nnn
  outputHash: string,             // Resulting universe state hash
  toolchainVersion: {
    node: string,                 // Node.js version
    packages: {[key]: string}     // Package versions
  },
  generatedAtTime: string,        // ISO8601 timestamp
  beforeHash: string | null,      // Previous receipt hash (chaining)
  merkleRoot: string | null,      // Merkle root (batching)
  receiptHash: string             // BLAKE3(canonical(receipt))
}
```

### Epoch Format

`τ_YYYY_MM_DD_HHMM_nnn`

Example: `τ_2025_12_26_1430_123`

- `YYYY`: Year (2025)
- `MM`: Month (12)
- `DD`: Day (26)
- `HHMM`: Hour/minute (1430 = 2:30 PM)
- `nnn`: Milliseconds (123)

## Determinism Guarantee

**Same inputs → Same receipt hash**

```javascript
const options = {
  inputHashes: { ontologyReleases: ['h1'], deltaCapsule: 'h2' },
  decision: 'allow',
  outputHash: 'h3',
  toolchainVersion: { node: 'v18.19.0', packages: {} },
  timestamp: new Date('2025-01-01T00:00:00Z')
};

const r1 = await Receipt.create(options);
const r2 = await Receipt.create(options);

console.log(r1.receiptHash === r2.receiptHash);  // true
```

**Mechanism**:
1. Canonical JSON serialization (sorted keys)
2. BLAKE3 cryptographic hash
3. Deterministic epoch generation

## Serialization

### JSON-LD

```json
{
  "@context": {
    "unrdf": "https://unrdf.org/vocab#",
    "xsd": "http://www.w3.org/2001/XMLSchema#",
    "prov": "http://www.w3.org/ns/prov#"
  },
  "@type": "unrdf:Receipt",
  "@id": "urn:receipt:a3f5d8c9...",
  "unrdf:decision": "allow",
  "unrdf:epoch": "τ_2025_12_26_1430_123",
  "unrdf:outputHash": "d6e8f1a2...",
  "prov:generatedAtTime": {
    "@type": "xsd:dateTime",
    "@value": "2025-12-26T14:30:00.123Z"
  }
}
```

### Turtle (TTL)

```turtle
@prefix unrdf: <https://unrdf.org/vocab#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix prov: <http://www.w3.org/ns/prov#> .

<urn:receipt:a3f5d8c9...> a unrdf:Receipt ;
  unrdf:decision "allow" ;
  unrdf:epoch "τ_2025_12_26_1430_123" ;
  unrdf:outputHash "d6e8f1a2..." ;
  prov:generatedAtTime "2025-12-26T14:30:00.123Z"^^xsd:dateTime ;
  unrdf:receiptHash "a3f5d8c9..." .
```

## Testing

```bash
# Run tests
pnpm test

# Run examples
node examples.mjs
```

## Use Cases

### 1. Admissibility Audit Trail

Track all allow/deny decisions for Δ capsules.

```javascript
const receipt = await generator.emitAdmissibilityReceipt({
  ontologyReleases: ['ont_v1.0'],
  deltaCapsule: 'delta_add_person',
  decision: 'allow',
  universeState: { classes: ['Person'] }
});
```

### 2. Validation Proof

Prove which ontologies validated successfully.

```javascript
const receipt = await generator.emitValidationReceipt({
  ontologyReleases: ['ont_v1.0'],
  validationReport: 'shacl_report',
  decision: 'allow',
  validationState: { violations: 0 }
});
```

### 3. Projection Proof

Prove query results for given ontology state.

```javascript
const receipt = await generator.emitProjectionReceipt({
  ontologyReleases: ['ont_v1.0'],
  projectionInput: 'sparql_query',
  decision: 'allow',
  projectionOutput: { bindings: [...] }
});
```

### 4. Merkle Batching

Batch 1000s of receipts into single root for efficient storage.

```javascript
const receipts = [...1000 receipts...];
const merkleRoot = await batchReceipts(receipts);

// Later: verify receipt was in batch
const { root, proof } = await generateMerkleProof(hashes, index);
const isValid = await verifyMerkleProof(hash, root, proof);
```

## Security Properties

### Immutability

```javascript
const receipt = await Receipt.create(options);
receipt.decision = 'deny';  // TypeError: Cannot assign to read only property
```

### Tamper Detection

```javascript
// Tamper with receipt
const tampered = Object.assign(Object.create(Object.getPrototypeOf(receipt)), receipt);
tampered.decision = 'deny';

const isValid = await tampered.verify();  // false
```

### Chain Integrity

```javascript
const chain = new ReceiptChain();
await chain.append(receipt1);
await chain.append(tamperedReceipt);  // Error: Receipt chain broken
```

## Performance

- **Receipt creation**: ~5ms (BLAKE3 hashing)
- **Chain verification**: ~N × 5ms (N receipts)
- **Merkle root**: ~N × 5ms (N receipts)
- **Merkle proof generation**: ~log(N) × 5ms
- **Merkle proof verification**: ~log(N) × 5ms

## Dependencies

- `hash-wasm`: BLAKE3 cryptographic hashing
- `zod`: Schema validation

## License

MIT
