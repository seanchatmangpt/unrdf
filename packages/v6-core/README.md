# @unrdf/v6-core

**UNRDF v6 Alpha** - Receipt-driven RDF with Merkle proofs, delta proposals, and versioned grammar.

## Status

**Version**: 6.0.0-rc.3
**Status**: Release Candidate (Production-ready pending final validation)
**Purpose**: Exploration and experimentation with v6 concepts

## Overview

The v6-core package is the foundation of UNRDF v6, introducing receipt-driven operations with cryptographic guarantees, delta-based versioning, and a versioned grammar system.

### Core Concepts

1. **Receipts**: Every operation generates a receipt with Merkle proof
2. **Delta Proposals**: Version transitions as explicit, verifiable proposals
3. **CLI Spine**: Unified command interface across all v6 operations
4. **Versioned Grammar**: Explicit grammar definitions with version tracking
5. **Documentation Capsule**: Self-documenting system

## Quick Start

### Installation

```bash
# From workspace root
pnpm install

# Navigate to v6-core
cd packages/v6-core
```

### Basic Usage

```javascript
import {
  createReceipt,
  createDeltaProposal,
  buildCLISpine,
  getV6Status,
} from '@unrdf/v6-core';

// Check v6 status
const status = getV6Status();
console.log(status);
// {
//   version: '6.0.0-alpha.1',
//   features: { receipts: true, delta: true, ... },
//   status: 'alpha',
//   timestamp: '2025-12-27T...'
// }

// Create a receipt for an operation
const receipt = createReceipt('add-triple', {
  subject: 'http://example.org/s',
  predicate: 'http://example.org/p',
  object: 'http://example.org/o',
});
console.log(receipt);
// {
//   id: 'receipt-...',
//   operation: 'add-triple',
//   timestamp: '...',
//   merkleRoot: '...',
//   proof: ['...'],
//   metadata: { subject: '...', ... }
// }

// Create a delta proposal
const proposal = createDeltaProposal('v1', 'v2', [
  { type: 'add', quad: { /* ... */ } },
  { type: 'remove', quad: { /* ... */ } },
]);
console.log(proposal);
// {
//   id: 'delta-...',
//   from: 'v1',
//   to: 'v2',
//   operations: [...],
//   timestamp: '...'
// }

// Build CLI spine
const spine = buildCLISpine();
console.log(spine.commands);
// {
//   'receipt:create': '...',
//   'delta:propose': '...',
//   ...
// }
```

## Architecture

### Capsule Structure

```
v6-core/
├── receipts/      - Receipt-driven operations with Merkle proofs
├── delta/         - Delta proposal and versioning system
├── cli/           - CLI spine integration
├── grammar/       - Versioned grammar definitions
└── docs/          - Documentation capsule
```

### Module Exports

```javascript
// Main entry point
import * from '@unrdf/v6-core';

// Capsule-specific imports
import * from '@unrdf/v6-core/receipts';
import * from '@unrdf/v6-core/delta';
import * from '@unrdf/v6-core/cli';
import * from '@unrdf/v6-core/grammar';
import * from '@unrdf/v6-core/docs';
```

## Testing

```bash
# Run all tests
pnpm test

# Run smoke tests only
pnpm test:smoke

# Run specific capsule tests
pnpm test:receipts
pnpm test:delta
pnpm test:grammar

# Validate entire package
pnpm validate
```

### Test Coverage

The smoke test suite validates:
- ✅ All modules import successfully
- ✅ Version and status reporting
- ✅ Receipt creation and verification
- ✅ Merkle tree operations
- ✅ Delta proposal creation and application
- ✅ CLI command execution
- ✅ Grammar validation
- ✅ Documentation retrieval

**Current**: 30+ smoke tests covering all capsules

## Features

### Receipts Capsule

```javascript
import { createReceipt, verifyReceipt, MerkleTree } from '@unrdf/v6-core/receipts';

// Create receipt
const receipt = createReceipt('operation', { data: 'value' });

// Verify receipt
const isValid = verifyReceipt(receipt);

// Merkle tree operations
const tree = new MerkleTree(['leaf1', 'leaf2', 'leaf3']);
const proof = tree.getProof(0);
const verified = MerkleTree.verify('leaf1', proof, tree.root);
```

### Delta Capsule

```javascript
import { createDeltaProposal, applyDelta, adapters } from '@unrdf/v6-core/delta';

// Create proposal
const proposal = createDeltaProposal('v1', 'v2', [
  { type: 'add', quad: {} },
]);

// Apply to store
await applyDelta(store, proposal);

// Use adapters
const adapter = new adapters.MemoryAdapter();
const id = await adapter.store(proposal);
const retrieved = await adapter.retrieve(id);
```

### CLI Capsule

```javascript
import { buildCLISpine, executeCommand, V6_COMMANDS } from '@unrdf/v6-core/cli';

// Build CLI
const spine = buildCLISpine();

// Execute command
const result = await executeCommand('receipt:create', { operation: 'test' });

// List commands
console.log(V6_COMMANDS);
```

### Grammar Capsule

```javascript
import { getGrammarDefinition, validateAgainstGrammar } from '@unrdf/v6-core/grammar';

// Get definition
const def = getGrammarDefinition('receipt');

// Validate data
const isValid = validateAgainstGrammar('receipt', data);
```

### Docs Capsule

```javascript
import { getDocumentation, listTopics } from '@unrdf/v6-core/docs';

// Get docs
const overview = getDocumentation('overview');

// List topics
const topics = listTopics();
```

## Development

### Scripts

- `pnpm test` - Run all tests
- `pnpm test:smoke` - Run smoke tests
- `pnpm lint` - Run linter
- `pnpm validate` - Full validation

### Contributing

This is an alpha package for exploration. Contributions should:

1. Add tests for new features
2. Follow existing patterns
3. Update documentation
4. Run validation before submitting

## Roadmap

### Alpha Goals (Current)

- [x] Receipts capsule with Merkle proofs
- [x] Delta proposal system
- [x] CLI spine integration
- [x] Versioned grammar
- [x] Documentation capsule
- [x] Smoke test suite

### Beta Goals

- [ ] Full Merkle tree implementation
- [ ] Delta conflict resolution
- [ ] OTEL validation integration
- [ ] Performance benchmarks
- [ ] Production adapters (filesystem, network)

### v6 Release Goals

- [ ] Complete documentation
- [ ] Production-ready implementations
- [ ] Migration guides from v5
- [ ] Performance optimization
- [ ] Security audit

## Links

- **Documentation**: [/home/user/unrdf/docs/v6/](../../docs/v6/)
- **Examples**: See [test/integration/](test/integration/)
- **Architecture**: See [docs/v6/architecture.md](../../docs/v6/architecture.md)
- **Changelog**: See [CHANGELOG.md](CHANGELOG.md)

## License

MIT - See LICENSE file for details.

---

**Note**: This is alpha software. APIs may change without notice. Do not use in production.
