# UNRDF Universal Receipt Standard

## Overview

The UNRDF Universal Receipt Standard defines a canonical format for recording the outcome of all operations across the 42+ packages in the UNRDF ecosystem. Receipts provide cryptographic proof of operations, enabling:

- **Audit trails**: Complete history of all admission decisions, test runs, builds, and deployments
- **Chain verification**: Cryptographic linkage ensures temporal ordering and tamper detection
- **Cross-package consistency**: Unified format works identically across all packages
- **Query capabilities**: Fast retrieval by time range, decision, type, or custom filters

## Core Concepts

### Receipt Types

The standard supports the following receipt types:

| Type | Description | Primary Use Case |
|------|-------------|------------------|
| `admission` | GOS gate admission decisions | Delta capsule validation |
| `test` | Test execution results | CI/CD pipelines |
| `build` | Build/compilation outcomes | Bundle generation |
| `deployment` | Deployment to environments | Production releases |
| `projection` | Documentation generation | Diataxis publishing |
| `query` | SPARQL query execution | Knowledge graph queries |
| `workflow` | Workflow/task execution | YAWL orchestration |
| `validation` | Schema/data validation | Data quality checks |
| `migration` | Data migration operations | Schema evolution |
| `sync` | Synchronization operations | Federation sync |
| `audit` | Audit log entries | Compliance logging |
| `custom` | Extension receipts | Domain-specific |

### Decision Outcomes

Every receipt contains a decision field with one of these outcomes:

- `ALLOW`: Operation succeeded/permitted
- `DENY`: Operation failed/blocked
- `WARN`: Operation allowed with warnings
- `SKIP`: Operation skipped
- `PENDING`: Operation in progress

### Chain Linkage

Receipts form a cryptographic chain via the `beforeHash` field:

```
Receipt 1 (beforeHash: null)     <- Genesis receipt
    |
    v receiptHash
Receipt 2 (beforeHash: hash1)
    |
    v receiptHash
Receipt 3 (beforeHash: hash2)
```

This ensures:
- Temporal ordering
- Tamper detection (broken link = compromised chain)
- Append-only semantics

## Receipt Schema

### Universal Receipt Structure

```javascript
{
  // Identity
  id: "urn:receipt:@unrdf/core:test:tau_2025_01_15_1430_123:abc123",
  type: "test",
  version: "1.0.0",

  // Package context
  package: "@unrdf/core",
  namespace: "unit-tests",

  // Temporal
  epoch: "tau_2025_01_15_1430_123",
  timestamp: "2025-01-15T14:30:00.123Z",

  // Decision
  decision: "ALLOW",
  reason: "All 42 tests passed",

  // Provenance
  provenance: {
    agent: "vitest",
    source: "@unrdf/core",
    version: "5.0.1",
    sessionId: "session-abc",
    requestId: "req-123"
  },

  // Toolchain
  toolchain: {
    node: "v18.19.0",
    platform: "linux",
    arch: "x64",
    packages: {
      "vitest": "^4.0.15",
      "zod": "^4.1.13"
    }
  },

  // Input/Output
  input: {
    hashes: { testFile: "hash123" },
    sources: ["src/core.test.mjs"],
    metadata: {}
  },
  output: {
    hash: "result-hash-456",
    artifacts: ["coverage/lcov.info"],
    metadata: {}
  },

  // Chain linkage
  beforeHash: "previous-receipt-hash",
  merkleRoot: null,

  // Type-specific extension
  extension: {
    type: "test",
    data: {
      suite: "core",
      file: "src/core.test.mjs",
      total: 42,
      passed: 42,
      failed: 0,
      skipped: 0,
      coverage: {
        lines: 98.5,
        branches: 92.3
      }
    }
  },

  // Computed hash
  receiptHash: "blake3-hash-of-receipt"
}
```

### Epoch Format

The epoch field uses the `tau` format for deterministic temporal identification:

```
tau_YYYY_MM_DD_HHmm_SSS
     |    |  |   |    |
     |    |  |   |    +-- Milliseconds (000-999)
     |    |  |   +------- Hours + Minutes (0000-2359)
     |    |  +----------- Day (01-31)
     |    +-------------- Month (01-12)
     +------------------- Year (YYYY)
```

Example: `tau_2025_01_15_1430_123` = January 15, 2025 at 14:30:00.123 UTC

## Usage Examples

### Creating Receipts

#### Test Receipt

```javascript
import { createTestReceipt } from './receipts/receipt-standard.mjs';

const receipt = await createTestReceipt({
  pkg: '@unrdf/core',
  suite: 'unit-tests',
  file: 'src/core.test.mjs',
  total: 42,
  passed: 42,
  failed: 0,
  duration: 1234,
  coverage: { lines: 98.5, branches: 92.3 }
});

console.log(receipt.decision); // 'ALLOW'
console.log(receipt.reason);   // 'All 42 tests passed'
```

#### Admission Receipt

```javascript
import { createAdmissionReceipt } from './receipts/receipt-standard.mjs';

const receipt = await createAdmissionReceipt({
  pkg: '@unrdf/core',
  capsuleId: 'delta-123',
  decision: 'DENY',
  reason: 'Invariant Q_version_consistency violated',
  partition: 'ontology',
  guards: [{ name: 'H_delete_published', passed: true }],
  invariants: [{ name: 'Q_version_consistency', passed: false }],
  quadCount: 150,
  beforeHash: 'universe-hash-before',
  afterHash: 'universe-hash-before' // Unchanged on DENY
});
```

#### Build Receipt

```javascript
import { createBuildReceipt } from './receipts/receipt-standard.mjs';

const receipt = await createBuildReceipt({
  pkg: '@unrdf/cli',
  target: 'dist',
  mode: 'production',
  success: true,
  bundleSize: 245760,
  chunks: [
    { name: 'main', size: 200000 },
    { name: 'vendor', size: 45760 }
  ],
  warnings: ['Circular dependency detected'],
  duration: 5432
});

console.log(receipt.decision); // 'WARN' (success with warnings)
```

### Using the Receipt Builder

For fine-grained control, use the fluent `ReceiptBuilder`:

```javascript
import { ReceiptBuilder } from './receipts/receipt-standard.mjs';

const receipt = await new ReceiptBuilder('workflow', '@unrdf/yawl')
  .namespace('data-pipeline')
  .decision('ALLOW', 'Workflow completed: 5/5 tasks succeeded')
  .provenance({ agent: 'yawl-engine', sessionId: 'session-xyz' })
  .toolchain({ packages: { '@unrdf/yawl': '1.0.0' } })
  .input({ hashes: { workflow: 'wf-hash' } })
  .output({ hash: 'output-hash', artifacts: ['result.json'] })
  .metrics({
    duration: 12500,
    startTime: '2025-01-15T14:00:00Z',
    endTime: '2025-01-15T14:00:12.500Z'
  })
  .extension({
    type: 'workflow',
    data: {
      workflowId: 'wf-123',
      workflowName: 'DataPipeline',
      taskCount: 5,
      completedTasks: 5,
      failedTasks: 0
    }
  })
  .build();
```

### Using the Ledger

The ledger provides append-only storage with chain verification:

```javascript
import { createMemoryLedger, createFileLedger } from './receipts/receipt-ledger.mjs';

// Memory ledger (for testing)
const memLedger = createMemoryLedger();

// File ledger (for production)
const fileLedger = createFileLedger('/var/lib/unrdf/receipts');

// Append receipt (validates chain linkage)
const result = await ledger.append('@unrdf/core', receipt);
if (!result.success) {
  console.error('Failed to append:', result.error);
}

// Verify chain integrity
const verification = await ledger.verifyChain('@unrdf/core');
console.log('Chain valid:', verification.valid);
console.log('Receipts checked:', verification.checked);

// Get chain statistics
const stats = ledger.getChainStats('@unrdf/core');
console.log('Total receipts:', stats.count);
console.log('Decisions:', stats.decisions);
// { ALLOW: 45, DENY: 3, WARN: 2 }
```

### Querying Receipts

The indexer provides fast queries across all receipts:

```javascript
import { createIndexer } from './receipts/receipt-indexer.mjs';

const indexer = await createIndexer(ledger);

// Query with fluent API
const deniedTests = indexer.query()
  .package('@unrdf/core')
  .type('test')
  .decision('DENY')
  .inTimeRange('tau_2025_01_01_0000_000', 'tau_2025_01_31_2359_999')
  .limit(50)
  .execute();

console.log('Denied tests:', deniedTests.total);
for (const receipt of deniedTests.receipts) {
  console.log(`  ${receipt.epoch}: ${receipt.reason}`);
}

// Search by invariant
const invariantViolations = indexer.getByInvariant('Q_version_consistency');

// Get all test failures
const testFailures = indexer.getTestFailures();

// Custom field queries
const failedWithHighCount = indexer.query()
  .type('test')
  .where('extension.data.failed', 5)
  .execute();
```

## Serialization Formats

### JSON-LD

Export receipt to semantic web format:

```javascript
import { receiptToJSONLD } from './receipts/receipt-standard.mjs';

const jsonld = receiptToJSONLD(receipt);
// Returns JSON-LD with @context, @type, etc.
```

### Turtle (TTL)

Export to RDF Turtle format:

```javascript
import { receiptToTurtle } from './receipts/receipt-standard.mjs';

const turtle = receiptToTurtle(receipt);
// Returns prefixed Turtle statements
```

### Binary

Efficient binary encoding:

```javascript
import { receiptToBinary, receiptFromBinary } from './receipts/receipt-standard.mjs';

const binary = receiptToBinary(receipt);  // Uint8Array
const restored = receiptFromBinary(binary);
```

## Adding Receipt Emission to a Package

### Step 1: Import the Factory

```javascript
import {
  createTestReceipt,
  createBuildReceipt,
  // ... other factories
} from '@unrdf/receipts';
```

### Step 2: Emit After Operations

```javascript
async function runTests() {
  const startTime = new Date();
  const results = await vitest.run();

  const receipt = await createTestReceipt({
    pkg: '@my-package/name',
    total: results.total,
    passed: results.passed,
    failed: results.failed,
    duration: Date.now() - startTime.getTime(),
    provenance: { agent: 'vitest' }
  });

  await ledger.append('@my-package/name', receipt);

  return results;
}
```

### Step 3: Chain Linkage

For proper chain linkage, pass the previous receipt hash:

```javascript
const lastReceipt = ledger.getLatest('@my-package/name');
const receipt = await createTestReceipt({
  // ...options
  beforeReceiptHash: lastReceipt?.receiptHash || null
});
```

## Chain Verification

Verify chain integrity at any time:

```javascript
const result = await ledger.verifyChain('@unrdf/core');

if (!result.valid) {
  console.error('Chain integrity compromised!');
  for (const error of result.errors) {
    console.error(`  ${error}`);
  }
}
```

## Merkle Batching

Batch multiple receipts under a single Merkle root:

```javascript
const merkleRoot = await ledger.computeChainMerkle('@unrdf/core');
console.log('Chain Merkle root:', merkleRoot);
```

## Best Practices

1. **Always emit receipts** - Every significant operation should produce a receipt
2. **Maintain chain linkage** - Pass `beforeReceiptHash` to link receipts
3. **Use appropriate types** - Choose the correct receipt type for the operation
4. **Include metrics** - Timing information aids debugging and optimization
5. **Verify regularly** - Run chain verification in CI/CD pipelines
6. **Index for queries** - Build indexes for production query workloads

## Receipt Namespace URIs

| Prefix | URI |
|--------|-----|
| `unrdf` | `https://unrdf.org/receipts#` |
| `prov` | `http://www.w3.org/ns/prov#` |
| `dct` | `http://purl.org/dc/terms/` |
| `xsd` | `http://www.w3.org/2001/XMLSchema#` |

## Security Considerations

- Receipt hashes use BLAKE3 for cryptographic strength
- Chain linkage prevents reordering attacks
- Merkle roots enable efficient inclusion proofs
- Append-only semantics prevent modification

## Performance Notes

- In-memory index: O(1) lookup by hash, O(n) for filtering
- File storage: Append-only NDJSON for fast writes
- Merkle computation: O(n log n) for n receipts
