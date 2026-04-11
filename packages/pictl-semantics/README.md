# @unrdf/pictl-semantics

**PICTL Semantics Integration with @unrdf Federation**

Ontology-driven process mining with cryptographic quorum consensus. Integrates PICTL process mining ontologies into the @unrdf knowledge graph through Byzantine fault-tolerant federation voting.

```
Multiple PICTL instances propose process mining results
        ↓
Each node validates locally against SHACL shapes
        ↓
M-of-N quorum consensus (default 2/3 majority)
        ↓
Approved results recorded with BLAKE3 receipt chain
        ↓
Cryptographic proof of result trustworthiness
```

## Features

✅ **PICTL Ontology** — 10+ core process mining classes and properties (Log, Event, Activity, ProcessModel, Fitness, Precision, Generalization, Simplicity, ConformanceResult)

✅ **Federation Quorum Voting** — M-of-N Byzantine consensus (≥2/3 by default). Tracks votes, detects consensus when threshold reached.

✅ **BLAKE3 Receipt Chaining** — Cryptographic hash chain linking all votes. Verify chain integrity to detect tampering.

✅ **SHACL Shape Validation** — Pre-defined shapes for quality metrics (fitness [0,1], precision [0,1], etc.). Nodes vote only on schema-valid results.

✅ **SPARQL Query** — Query the PICTL ontology with SELECT and ASK queries. Add custom facts, run conformance checks.

✅ **OpenTelemetry Tracing** — Spans for proposal, voting, consensus, validation. View in Jaeger at `localhost:16686`.

## Installation

```bash
pnpm install @unrdf/pictl-semantics
```

## Quick Start

```javascript
import { initializePictlSemantics, shutdownPictlSemantics } from '@unrdf/pictl-semantics';

// Initialize PICTL context for this node
const pictl = await initializePictlSemantics({
  nodeId: 'pictl-node-1',
  quorumThreshold: 2, // Require 2/3 approval
});

// Load ontology
await pictl.loadOntology();

// Propose a result from process mining
const result = {
  fitness: 0.92,
  precision: 0.88,
  model: { type: 'petri-net', places: 5, transitions: 4 },
};
const proposed = pictl.proposeResult(result, 'pictl-node-1');

// Vote on the result
const vote = pictl.votePictlResult(proposed.resultId, 'approve', 'SHACL validation passed');

// Check quorum status
const status = pictl.getQuorumStatus();
console.log(`Approved: ${status.results.approved}`);
console.log(`Receipts: ${status.receipts.count}`);
```

## Architecture

### Three Layers

| Layer           | Technology    | Purpose                                             |
| --------------- | ------------- | --------------------------------------------------- |
| **Application** | PICTL Results | Process mining outputs (fitness, precision, models) |
| **Federation**  | Quorum Voting | M-of-N consensus, BLAKE3 receipts                   |
| **RDF**         | SPARQL/SHACL  | Ontology storage, shape validation                  |

### Module Breakdown

- **`src/index.mjs`** — Main API: `initializePictlSemantics()`, context methods
- **`src/ontology-loader.mjs`** — PICTL classes/properties, SPARQL queries
- **`src/quorum.mjs`** — Proposal, voting, consensus, receipt chaining
- **`src/result-validator.mjs`** — SHACL shape validation, quality checks

## Quorum Consensus

### Threshold Logic

Default: **≥2/3 approval** to accept result

```
3 nodes: need 2 approvals
5 nodes: need 4 approvals
7 nodes: need 5 approvals
```

### Vote Flow

```javascript
// 1. Propose
const proposed = pictl.proposeResult(result, 'proposer');
// Status: pending

// 2. Vote (can be from different node)
pictl.votePictlResult(proposed.resultId, 'approve', reason);
pictl.votePictlResult(proposed.resultId, 'approve', reason);
// If 2 approvals ≥ 2/3 threshold → Status: approved

// 3. Receipt created
const status = pictl.getQuorumStatus();
console.log(status.receipts.latestHash); // BLAKE3 hash
```

## Receipt Chain

Each consensus creates a BLAKE3 receipt chained to the previous:

```javascript
Receipt 1: hash = SHA256(...)
Receipt 2: hash = SHA256(...), previousHash = Receipt1.hash
Receipt 3: hash = SHA256(...), previousHash = Receipt2.hash

// Verify chain
const validation = pictl.getQuorumStatus();
console.log(validation.receipts.chainValid); // true = unbroken
```

## PICTL Ontology

### Core Concepts

```
Log (event log)
  ├─ hasEvent → Event
  ├─ hasTrace → Trace
  └─ logSize → integer

Event
  ├─ hasActivity → Activity name
  ├─ hasTimestamp → datetime
  ├─ hasResource → Resource
  └─ hasCaseId → Case ID

ProcessModel
  ├─ hasPlace → Place (Petri net)
  ├─ hasTransition → Transition
  └─ hasArc → Arc

Quality Metrics
  ├─ Fitness: [0.0, 1.0]
  ├─ Precision: [0.0, 1.0]
  ├─ Generalization: [0.0, 1.0]
  └─ Simplicity: [0.0, 1.0]
```

### SPARQL Queries

```javascript
// Query events in a log
const results = await pictl.query(`
  SELECT ?event ?activity WHERE {
    ?log a <urn:pictl:Log> .
    ?log <urn:pictl:hasEvent> ?event .
    ?event <urn:pictl:hasActivity> ?activity
  }
`);

// Check for fitness metrics
const metrics = await pictl.query(`
  SELECT ?score WHERE {
    ?result a <urn:pictl:ConformanceResult> .
    ?result <urn:pictl:fitnessScore> ?score .
    FILTER (?score > 0.8)
  }
`);
```

## Result Validation

### Schema Validation

```javascript
import { validatePictlResult } from '@unrdf/pictl-semantics/result-validator';

const validation = validatePictlResult({
  fitness: 0.92,
  precision: 0.88,
  model: { type: 'petri-net' },
});

if (!validation.valid) {
  console.error('Errors:', validation.errors);
  // Result rejected before voting
}
```

### SHACL Shape Validation

```javascript
import { validateAgainstShapes, PICTL_SHAPES } from '@unrdf/pictl-semantics/result-validator';

const fitnessValid = validateAgainstShapes({ fitness: 0.92 }, PICTL_SHAPES.fitness.shapeUri);

if (fitnessValid.valid) {
  pictl.votePictlResult(resultId, 'approve', 'Fitness valid');
} else {
  console.error('Violations:', fitnessValid.violations);
  pictl.votePictlResult(resultId, 'reject', fitnessValid.violations[0]);
}
```

### Quality Warnings

```javascript
const validation = validatePictlResult({ fitness: 1.0 });
console.log(validation.warnings);
// ['Fitness score of 1.0 may indicate overfitting']
```

## Multi-Node Pattern

```javascript
// Node 1 proposes
const pictl1 = await initializePictlSemantics({ nodeId: 'node-1' });
const result = { fitness: 0.92, precision: 0.88 };
const proposed = pictl1.proposeResult(result, 'node-1');

// Node 2 votes (receives resultId via message)
const pictl2 = await initializePictlSemantics({ nodeId: 'node-2' });
const validation2 = pictl2.validateResult(result);
pictl2.votePictlResult(proposed.resultId, validation2.valid ? 'approve' : 'reject', '...');

// Node 3 votes
const pictl3 = await initializePictlSemantics({ nodeId: 'node-3' });
const validation3 = pictl3.validateResult(result);
pictl3.votePictlResult(proposed.resultId, validation3.valid ? 'approve' : 'reject', '...');

// Check consensus (any node)
const status = pictl1.getQuorumStatus();
console.log(status.approvedResults); // Results with ≥2/3 approval
```

## Testing

```bash
# All tests
pnpm test

# Fast tests only
pnpm test:fast

# Watch mode
pnpm test:watch

# Coverage
pnpm test -- --coverage
```

Tests include:

- **quorum.test.mjs** — Proposal, voting, consensus, receipt chaining
- **ontology.test.mjs** — Triple storage, SPARQL queries, N-Triples export
- **validation.test.mjs** — Schema validation, SHACL shapes, quality warnings

## OpenTelemetry Integration

Traces automatically emitted for all operations:

```
Service: @unrdf/pictl-semantics
Spans:
  - quorum.propose_result
  - quorum.vote_result
  - quorum.validate_tally
  - quorum.get_status
  - pictl.ontology.load
  - pictl.ontology.query
  - pictl.validate_result
  - pictl.validate_shapes
  - pictl.validate_batch
```

View traces: `http://localhost:16686` (Jaeger)

## API Reference

### Initialization

```javascript
export async function initializePictlSemantics(config: {
  nodeId: string,
  quorumThreshold?: number,
  ontologyPath?: string,
  shapesPath?: string
}): Promise<PictlContext>
```

### Core Operations

```javascript
// Proposal
proposeResult(result: Object, nodeId: string): ProposedResult

// Voting
votePictlResult(resultId: string, vote: 'approve'|'reject', reason?: string): VoteRecord

// Queries
query(sparql: string): Promise<SparqlResult>

// Status
getQuorumStatus(): QuorumStatus
```

### Validation

```javascript
validatePictlResult(result: Object): ValidationResult
validateAgainstShapes(data: Object, shapeUri: string): ShapeValidationResult
validateBatchResults(results: Object[]): BatchValidationResult
```

## Limitations

- **SPARQL**: Simplified pattern matching (not full SPARQL 1.1)
- **Ontology**: In-memory (no persistent RDF store; use Oxigraph if needed)
- **Quorum**: Max ~100 nodes (no consensus optimization beyond simple tally)
- **Shapes**: Pre-defined SHACL shapes only (no custom shape loading)

## See Also

- **@unrdf/federation** — Distributed SPARQL queries, RAFT consensus
- **@unrdf/core** — RDF data model, Quads
- **@unrdf/hooks** — SPARQL CONSTRUCT, N3 forward-chaining
- **INTEGRATION.md** — Comprehensive usage guide

## License

MIT
