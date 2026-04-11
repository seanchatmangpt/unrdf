# @unrdf/pictl-semantics Integration Guide

**PICTL Semantics Integration with @unrdf Federation**

Ontology-driven process mining with cryptographic quorum consensus. Multiple PICTL instances vote on result trustworthiness via Byzantine fault-tolerant consensus.

## Quick Start

### Installation

```bash
pnpm install @unrdf/pictl-semantics
```

### Basic Usage

```javascript
import { initializePictlSemantics, shutdownPictlSemantics } from '@unrdf/pictl-semantics';

// Initialize PICTL context for this node
const pictl = await initializePictlSemantics({
  nodeId: 'pictl-node-1',
  quorumThreshold: 2,
});

// Load ontology
await pictl.loadOntology();

// Propose a result
const result = {
  fitness: 0.92,
  precision: 0.88,
  model: { type: 'petri-net' },
};
const proposed = pictl.proposeResult(result, 'pictl-node-1');

// Vote on the result
const vote = pictl.votePictlResult(proposed.resultId, 'approve', 'SHACL validation passed');

// Check quorum status
const status = pictl.getQuorumStatus();
console.log(`Approved: ${status.results.approved}, Pending: ${status.results.pending}`);

// Shutdown
await shutdownPictlSemantics(pictl);
```

---

## Architecture

### Three-Layer Federation

```
Application Layer (PICTL Results)
        ↓
Federation Quorum (M-of-N Consensus, BLAKE3 Receipts)
        ↓
RDF Ontology (SPARQL, SHACL, In-Memory Graph)
```

### Components

| Module                     | Responsibility                                |
| -------------------------- | --------------------------------------------- |
| `src/index.mjs`            | Main API, context initialization              |
| `src/ontology-loader.mjs`  | PICTL classes/properties, SPARQL queries      |
| `src/quorum.mjs`           | Proposal, voting, receipt chaining, consensus |
| `src/result-validator.mjs` | SHACL shape validation, quality checks        |

---

## Federation Quorum Voting

### Consensus Threshold

Default: **2/3 majority** (customizable via `quorumThreshold`)

- With 3 nodes: need 2 approvals to accept
- With 5 nodes: need 4 approvals to accept
- With N nodes: need ceil(2N/3) approvals

### Vote States

| State      | Meaning             | Transition                                        |
| ---------- | ------------------- | ------------------------------------------------- |
| `pending`  | Awaiting votes      | → `approved` or `rejected` when threshold reached |
| `approved` | ≥2/3 nodes approved | Final, receipt created                            |
| `rejected` | <2/3 nodes approved | Final, receipt created                            |

### Propose → Vote → Consensus → Receipt

```javascript
// 1. One node proposes result
const proposed = pictl.proposeResult(result, 'pictl-node-1');
// Status: pending, votes: []

// 2. Multiple nodes vote
const vote1 = pictl.votePictlResult(proposed.resultId, 'approve', 'Valid');
// Status: pending, votes: [vote1]

const vote2 = pictl.votePictlResult(proposed.resultId, 'approve', 'Valid');
// Status: approved (2/3 threshold reached)
// Receipt created with BLAKE3 hash chain

// 3. Check consensus
const status = pictl.getQuorumStatus();
console.log(status.approvedResults);
```

---

## BLAKE3 Receipt Chain

### Receipt Structure

```javascript
{
  hash: "abc123...",               // BLAKE3 of this receipt
  previousHash: "prev123...",       // BLAKE3 of previous receipt (forms chain)
  timestamp: 1710123456789,
  nodeId: "pictl-node-1",
  resultId: "result-abc123",
  voteCount: 3,                     // Total votes cast
  approvalCount: 2                  // Votes in favor
}
```

### Chain Integrity Verification

```javascript
// Validate receipt chain
const validation = pictl.getQuorumStatus();
console.log(validation.receipts.chainValid); // true = chain unbroken

// Manual chain validation
import { validateVoteTally } from '@unrdf/pictl-semantics/quorum';
const check = validateVoteTally(pictl.quorumState);
if (!check.valid) {
  console.error(`Chain break at index ${check.breakIndex}`);
}
```

---

## PICTL Ontology

### Core Classes

```
Log
├─ hasEvent → Event
├─ hasTrace → Trace
└─ hasCaseId → Case

Event
├─ hasActivity → Activity
├─ hasTimestamp → timestamp
├─ hasResource → Resource
└─ hasCaseId → Case

ProcessModel
├─ hasPlace → Place
├─ hasTransition → Transition
└─ hasArc → Arc

Quality Metrics
├─ Fitness (fitnessScore: 0.0–1.0)
├─ Precision (precisionScore: 0.0–1.0)
├─ Generalization (generalizationScore: 0.0–1.0)
└─ Simplicity (simplicityScore: 0.0–1.0)
```

### SPARQL Query Examples

#### Query for all events in a log

```javascript
const results = await pictl.query(`
  SELECT ?event ?activity ?timestamp WHERE {
    ?log a <urn:pictl:Log> .
    ?log <urn:pictl:hasEvent> ?event .
    ?event <urn:pictl:hasActivity> ?activity .
    ?event <urn:pictl:hasTimestamp> ?timestamp
  }
`);
```

#### ASK: Does this log have any events?

```javascript
const result = await pictl.query(`
  ASK {
    ?log a <urn:pictl:Log> .
    ?log <urn:pictl:hasEvent> ?event
  }
`);

if (result.boolean) {
  console.log('Log has events');
}
```

#### Query for fitness metrics

```javascript
const results = await pictl.query(`
  SELECT ?result ?fitness WHERE {
    ?result a <urn:pictl:ConformanceResult> .
    ?result <urn:pictl:fitnessScore> ?fitness .
    FILTER (?fitness > 0.8)
  }
`);
```

---

## Result Validation (SHACL Shapes)

### Pre-Voting Validation

Before nodes vote, they validate results against SHACL shapes:

```javascript
import {
  validatePictlResult,
  validateAgainstShapes,
} from '@unrdf/pictl-semantics/result-validator';

// Schema validation (comprehensive)
const schemaCheck = validatePictlResult({
  fitness: 0.92,
  precision: 0.88,
  model: { type: 'petri-net', places: 5, transitions: 4 },
});

if (!schemaCheck.valid) {
  console.error('Schema errors:', schemaCheck.errors);
  // Result fails before voting stage
}

// SHACL shape validation (metric-specific)
const fitnessCheck = validateAgainstShapes({ fitness: 0.92 }, 'urn:pictl:FitnessShape');

if (!fitnessCheck.valid) {
  console.error('Fitness violations:', fitnessCheck.violations);
  // Vote 'reject'
}
```

### Pre-Defined Shapes

| Shape                 | Metric         | Range      | Constraint  |
| --------------------- | -------------- | ---------- | ----------- |
| `FitnessShape`        | fitness        | [0.0, 1.0] | Double      |
| `PrecisionShape`      | precision      | [0.0, 1.0] | Double      |
| `GeneralizationShape` | generalization | [0.0, 1.0] | Double      |
| `SimplicityShape`     | simplicity     | [0.0, 1.0] | Double      |
| `AlignmentCostShape`  | alignmentCost  | [0, ∞)     | Integer ≥ 0 |
| `LogSizeShape`        | logSize        | [1, ∞)     | Integer ≥ 1 |

### Quality Warnings

```javascript
const validation = validatePictlResult({
  fitness: 1.0, // Warning: perfect fit may indicate overfitting
  precision: 0.0, // Warning: zero precision suggests poor model
  fitness: 0.95,
  precision: 0.6, // Warning: large fitness-precision gap
});

console.log(validation.warnings);
// [
//   'Fitness score of 1.0 may indicate overfitting',
//   'Large gap between fitness and precision suggests model quality issue'
// ]
```

---

## Multi-Node Voting Pattern

### Setup: 5-Node Federation

```javascript
// Node 1
const pictl1 = await initializePictlSemantics({
  nodeId: 'pictl-node-1',
  quorumThreshold: 2,
});

// Node 2
const pictl2 = await initializePictlSemantics({
  nodeId: 'pictl-node-2',
  quorumThreshold: 2,
});

// ... nodes 3, 4, 5 similarly
```

### Process: Propose → Validate → Vote

```javascript
// Node 1: Propose result
const result = {
  fitness: 0.92,
  precision: 0.88,
  model: { type: 'petri-net', places: 5, transitions: 4 },
};
const proposed = pictl1.proposeResult(result, 'pictl-node-1');

// Broadcast proposed.resultId to all nodes (in real system)

// Each node: Validate and vote
for (const pictl of [pictl1, pictl2, pictl3, pictl4, pictl5]) {
  // Validate result locally
  const validation = validatePictlResult(result);

  if (validation.valid) {
    // Validate against local SHACL shapes
    const fitnessCheck = pictl.validateAgainstShapes(result, 'urn:pictl:FitnessShape');

    if (fitnessCheck.valid) {
      // Vote approve
      pictl.votePictlResult(proposed.resultId, 'approve', 'All checks passed');
    } else {
      // Vote reject
      pictl.votePictlResult(proposed.resultId, 'reject', fitnessCheck.violations[0]);
    }
  } else {
    // Schema invalid
    pictl.votePictlResult(proposed.resultId, 'reject', 'Schema validation failed');
  }
}

// Check consensus (any node can check)
const status = pictl1.getQuorumStatus();
console.log(`Result ${proposed.resultId}: ${status.approvedResults[0]?.result.fitness}`);
```

---

## Receipt Chain Validation

### Verifying Cryptographic Proof

```javascript
import { validateVoteTally } from '@unrdf/pictl-semantics/quorum';

const validation = validateVoteTally(pictl.quorumState);

if (validation.valid) {
  console.log(`Receipt chain verified: ${validation.receiptCount} entries`);
  console.log(`Latest hash: ${validation.chainHash}`);
} else {
  console.error(`Chain broken at entry ${validation.breakIndex}`);
  console.error(`Expected hash: ${validation.error}`);
  // Escalate to supervisor or restart node
}
```

### Persistent Storage

For production, persist receipts to database:

```javascript
// Get latest receipts
const status = pictl.getQuorumStatus();
const latestHash = status.receipts.latestHash;

// Store in database (pseudo-code)
await db.receipts.insert({
  chainHash: latestHash,
  timestamp: Date.now(),
  nodeId: pictl.nodeId,
});

// Later: restore from database
const stored = await db.receipts.findLatest();
if (stored.chainHash !== runtime.receipts[receipts.length - 1].hash) {
  console.error('Receipt chain mismatch after restart');
}
```

---

## Integration with @unrdf/federation

### Federated Query + Quorum Voting

```javascript
import { createCoordinator } from '@unrdf/federation';
import { initializePictlSemantics } from '@unrdf/pictl-semantics';

// 1. Setup federation
const coordinator = await createCoordinator({
  nodeId: 'fed-node-1',
  peers: ['fed-node-2', 'fed-node-3'],
});

// 2. Setup PICTL quorum
const pictl = await initializePictlSemantics({
  nodeId: 'pictl-node-1',
});

// 3. Execute federated SPARQL query (from @unrdf/federation)
const results = await coordinator.executeDistributedQuery(
  'SELECT * WHERE { ?s <urn:pictl:fitnessScore> ?fitness }'
);

// 4. Validate and vote on results
for (const result of results) {
  const validation = pictl.validateResult(result);
  if (validation.valid) {
    pictl.votePictlResult(resultId, 'approve', 'Fed query result valid');
  }
}
```

---

## Testing

### Run All Tests

```bash
pnpm --filter @unrdf/pictl-semantics test
pnpm --filter @unrdf/pictl-semantics test:fast
pnpm --filter @unrdf/pictl-semantics test:watch
```

### Test Coverage

- **quorum.test.mjs**: Proposal, voting, consensus, receipt chaining
- **ontology.test.mjs**: Triple storage, SPARQL queries, N-Triples export
- **validation.test.mjs**: Schema validation, SHACL shapes, quality warnings

### Mock Multi-Node Voting (Test Example)

```javascript
describe('5-Node Consensus', () => {
  it('should approve with 4/5 votes', () => {
    const quorumState = {
      nodeId: 'test-node',
      quorumThreshold: 2,
      results: new Map(),
      votes: new Map(),
      receipts: [],
    };

    const result = { fitness: 0.92 };
    const proposed = proposeResult(result, 'proposer', quorumState);

    // Votes from 5 nodes: 4 approve, 1 rejects
    quorumState.nodeId = 'node-1';
    votePictlResult(proposed.resultId, 'approve', '', quorumState);

    quorumState.nodeId = 'node-2';
    votePictlResult(proposed.resultId, 'approve', '', quorumState);

    quorumState.nodeId = 'node-3';
    votePictlResult(proposed.resultId, 'approve', '', quorumState);

    quorumState.nodeId = 'node-4';
    votePictlResult(proposed.resultId, 'approve', '', quorumState);

    quorumState.nodeId = 'node-5';
    const finalVote = votePictlResult(proposed.resultId, 'reject', '', quorumState);

    // 4/5 approvals = 80% > 66.7% threshold
    expect(finalVote.consensusStatus).toBe('approved');
  });
});
```

---

## OpenTelemetry Tracing

All operations emit OTEL spans for observability:

```javascript
// Traces are automatically emitted:
// - quorum.propose_result
// - quorum.vote_result
// - quorum.validate_tally
// - quorum.get_status
// - pictl.ontology.load
// - pictl.ontology.query
// - pictl.validate_result
// - pictl.validate_shapes
// - pictl.validate_batch

// View in Jaeger:
// Service: @unrdf/pictl-semantics
// Operations: quorum.*, pictl.ontology.*, pictl.validate_*
```

---

## Error Handling

### Common Errors

| Error                       | Cause                       | Resolution                            |
| --------------------------- | --------------------------- | ------------------------------------- |
| `Result not found: <id>`    | Vote on non-existent result | Ensure result was proposed first      |
| `Shape not found: <uri>`    | Invalid SHACL shape URI     | Use URI from `PICTL_SHAPES`           |
| `Chain integrity violation` | Receipt chain tampered      | Restart node, restore from backup     |
| `Schema validation failed`  | Invalid result structure    | Check `validation.errors` for details |

### Recovery Pattern

```javascript
try {
  pictl.votePictlResult(resultId, 'approve', reason);
} catch (error) {
  if (error.message.includes('Result not found')) {
    // Result wasn't proposed yet
    console.warn('Result not yet proposed, waiting...');
    await delay(1000);
    retry();
  } else if (error.message.includes('chain integrity')) {
    // Critical: chain corrupted
    await shutdownPictlSemantics(pictl);
    process.exit(1);
  } else {
    throw error;
  }
}
```

---

## Performance Characteristics

### Benchmarks

| Operation          | Time  | Notes                          |
| ------------------ | ----- | ------------------------------ |
| Propose result     | <1ms  | Generates hash, stores in Map  |
| Vote on result     | <2ms  | Validation + receipt creation  |
| Ontology load      | ~50ms | In-memory graph initialization |
| SPARQL query       | <10ms | Simple pattern matching        |
| Receipt validation | <5ms  | Chain walk, hash verification  |

### Scalability

- **Results**: Unbounded (limited by memory)
- **Receipts**: ~200 bytes each, can store 50K before 10MB
- **Quorum nodes**: Supports 3–100+ nodes (M-of-N consensus)
- **Concurrent votes**: Safe up to node count

---

## License

MIT — See LICENSE file
