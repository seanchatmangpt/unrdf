# @unrdf/engine-gateway

**μ(O) Engine Gateway** - Enforcement layer for Oxigraph-first, N3-minimal RDF processing.

## Overview

The Engine Gateway implements the **μ(O) Minimal-N3 Architecture**, a canonical principle for RDF engine selection:

> **Oxigraph is the authoritative engine. N3 exists only at 5 justified boundaries.**

This package provides:
- ✅ **Operation Detection** - Identify which engine should handle each operation
- ✅ **Routing** - Route operations to correct engine (Oxigraph or N3)
- ✅ **Validation** - Enforce μ(O) rules at runtime
- ✅ **Metadata** - Explain routing decisions and justifications

## Installation

```bash
npm install @unrdf/engine-gateway
```

## Quick Start

```javascript
import { EngineGateway } from '@unrdf/engine-gateway';
import { createStore } from '@unrdf/oxigraph';

// Create gateway
const store = createStore();
const gateway = new EngineGateway({ store, enforce: true });

// Route SPARQL query to Oxigraph (authoritative)
const results = gateway.route('query', `
  SELECT ?s ?p ?o WHERE { ?s ?p ?o }
`);

// Route streaming parse to N3 (justified) → re-enters Oxigraph
const parsed = await gateway.route('stream-parse', fs.createReadStream('large.ttl'));
```

## The 5 Justified N3 Operations

N3 is ONLY used for these 5 operations:

### 1. **Stream Parse** (Backpressure handling)
```javascript
const store = await gateway.route('stream-parse', readableStream);
// N3 provides backpressure-aware streaming parser
// Re-enters Oxigraph immediately
```

### 2. **Stream Serialize** (Output streaming)
```javascript
await gateway.route('stream-serialize', store, 'turtle', writableStream);
// N3 provides streaming writer
// Enables backpressure control for large outputs
```

### 3. **N3 Reasoning** (Forward-chaining logic)
```javascript
const inferred = await gateway.route('n3-reason', store, rules);
// N3 provides forward-chaining not available in SPARQL
// Re-enters Oxigraph with inferred triples
```

### 4. **Permissive Parse** (Malformed RDF recovery)
```javascript
const recovered = gateway.route('permissive-parse', dirtyRdf);
// N3 recovers from syntax errors
// Re-enters Oxigraph with best-effort parse
```

### 5. **RDF Transform** (Structural rewrites)
```javascript
const transformed = gateway.route('rdf-transform', store, transformFunction);
// N3 for complex graph transformations
// Re-enters Oxigraph with transformed quads
```

## All Other Operations → Oxigraph

Everything else uses Oxigraph:

```javascript
// SPARQL queries (Oxigraph-only)
const results = gateway.route('query', sparqlQuery);

// SPARQL updates (Oxigraph-only)
gateway.route('update', sparqlUpdate);

// Storage (Oxigraph-only)
gateway.route('add', quad);
gateway.route('delete', quad);
gateway.route('clear');

// Basic parsing (Oxigraph-only)
const store = gateway.route('parse', rdf, { format: 'turtle' });

// Basic serialization (Oxigraph-only)
const ttl = gateway.route('serialize', store, { format: 'turtle' });

// Pattern matching (Oxigraph-only)
const quads = gateway.route('match', subject, predicate, object, graph);
```

## Routing Metadata

Get information about operation routing:

```javascript
const metadata = gateway.getRoutingMetadata('query');
console.log(metadata);
// {
//   operation: 'query',
//   engine: 'oxigraph',
//   valid: true,
//   message: 'Operation "query" correctly uses Oxigraph as authoritative engine'
// }

const n3Metadata = gateway.getRoutingMetadata('stream-parse');
console.log(n3Metadata);
// {
//   operation: 'stream-parse',
//   engine: 'n3',
//   valid: true,
//   justification: 'Streaming required for backpressure or memory budget',
//   reenterOxigraph: true,
//   message: 'Operation "stream-parse" is a justified N3 use case'
// }
```

## Validation & Enforcement

The gateway enforces μ(O) rules by default:

```javascript
// Enforcement enabled (throws on violations)
const gateway = new EngineGateway({ store, enforce: true });

try {
  gateway.route('query-with-n3'); // Throws error
} catch (e) {
  console.error(e.message);
  // "μ(O) Violation: Operation "query-with-n3" is not a justified N3 use case..."
}

// Enforcement disabled (log-only mode)
const permissiveGateway = new EngineGateway({ store, enforce: false });
const result = permissiveGateway.route('query-with-n3'); // Logs warning but allows
```

## Operation Detection

Detect which engine should handle an operation:

```javascript
import { detectOperationType, isN3Operation, isOxigraphOperation } from '@unrdf/engine-gateway';

detectOperationType('query');           // 'oxigraph'
detectOperationType('stream-parse');    // 'n3'

isOxigraphOperation('query');           // true
isN3Operation('stream-parse');          // true

isOxigraphOperation('stream-parse');    // false
isN3Operation('query');                 // false
```

## Validators

Validate operation compliance with μ(O) rules:

```javascript
import { validateN3Usage, validateOxigraphUsage } from '@unrdf/engine-gateway';

// Validate N3 operation
const n3Validation = validateN3Usage('stream-parse');
if (n3Validation.valid) {
  console.log(n3Validation.justification);
}

// Validate Oxigraph operation
const oxValidation = validateOxigraphUsage('query');
if (oxValidation.valid) {
  console.log('Query correctly uses Oxigraph');
}

// Throws on violation
try {
  validateN3Usage('add'); // Storage ops can't use N3
} catch (e) {
  console.error(e.message);
  // "μ(O) Violation: N3 cannot be used for storage operation..."
}
```

## Architecture Diagram

```
┌─────────────────────────────────────────────────────┐
│           Application Code                          │
│  (queries, parsing, storage, serialization, etc.)   │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
        ┌──────────────────────┐
        │  EngineGateway       │
        │  - Operation detect  │
        │  - Routing logic     │
        │  - Validation        │
        └──────────────────────┘
              ▲         ▲
       ┌──────┘         └──────┐
       │                       │
       ▼                       ▼
  ┌──────────────┐      ┌──────────────────┐
  │ Oxigraph     │      │ N3 (5 cases)     │
  │ (Primary)    │      │ (Boundaries)     │
  │              │      │                  │
  │ • Query      │      │ • Stream Parse   │
  │ • Update     │      │ • Stream Serialize
  │ • Storage    │      │ • N3 Reasoning   │
  │ • Parse      │      │ • Permissive     │
  │ • Serialize  │      │ • Transform      │
  │ • Match      │      │                  │
  └──────────────┘      │ ↓ Re-enter Oxigraph
                        └──────────────────┘
```

## Configuration

```javascript
const gateway = new EngineGateway({
  // Required
  store: oxigraphStoreInstance,

  // Optional
  enforce: true,      // Strict enforcement (default: true)
  verbose: false      // Log routing decisions (default: false)
});

// Enable verbose logging
const debugGateway = new EngineGateway({ store, verbose: true });
debugGateway.route('query', sparql);
// [μ(O)] Routing "query" to oxigraph
```

## Integration with UnrdfStore

```javascript
import { UnrdfStore } from '@unrdf/core';
import { EngineGateway } from '@unrdf/engine-gateway';

const store = new UnrdfStore();
const gateway = new EngineGateway({ store, enforce: true });

// All store operations routed through gateway
const results = await gateway.route('query', `
  SELECT * WHERE { ?s ?p ?o }
`);
```

## Error Handling

```javascript
import { EngineGateway } from '@unrdf/engine-gateway';

const gateway = new EngineGateway({ store, enforce: true });

try {
  // This would violate μ(O) - N3 can't be used for queries
  gateway.route('query-with-n3', sparql);
} catch (error) {
  if (error.message.includes('μ(O) Violation')) {
    console.error('Architectural violation detected:', error.message);
  }
}
```

## Machine-Readable Ontology

The routing rules are defined in RDF/Turtle for machine-readability:

```turtle
# From: docs/ontology/minimal-n3-routing.ttl
@prefix kgc: <http://kgc.unrdf.dev/ns#> .

kgc:QueryOperation a kgc:Operation ;
  kgc:routesTo kgc:Oxigraph ;
  kgc:justification "Oxigraph provides full SPARQL 1.1 support" .

kgc:StreamParseOperation a kgc:Operation ;
  kgc:routesTo kgc:N3 ;
  kgc:justification "N3 provides backpressure-aware streaming parser" ;
  kgc:reenterOxigraph true .
```

Query the routing rules:

```javascript
import { createStore } from '@unrdf/oxigraph';

const ontology = createStore();
ontology.load(readFileSync('minimal-n3-routing.ttl'), { format: 'turtle' });

const routingRules = ontology.query(`
  PREFIX kgc: <http://kgc.unrdf.dev/ns#>

  SELECT ?operation ?engine ?justification
  WHERE {
    ?operation a kgc:Operation ;
      kgc:routesTo ?engine ;
      kgc:justification ?justification .
  }
`);
```

## Testing

```bash
npm test
```

Tests verify:
- ✅ All operations route to correct engine
- ✅ N3 operations re-enter Oxigraph
- ✅ μ(O) violations detected and thrown
- ✅ Metadata correctly explains routing decisions
- ✅ Validation functions work properly

## API Reference

### EngineGateway

#### `constructor(config)`
Create a new gateway instance.

#### `route(operation, ...args)`
Route operation to appropriate engine and execute it.

#### `getRoutingMetadata(operation)`
Get metadata about how operation is routed.

#### `canRoute(operation)`
Check if gateway can route this operation.

### Operation Detection

#### `detectOperationType(operation)`
Determine if operation uses N3 or Oxigraph.

#### `isN3Operation(operation)`
Check if operation is N3-justified.

#### `isOxigraphOperation(operation)`
Check if operation uses Oxigraph.

#### `getOperationMetadata(operation)`
Get detailed metadata about operation.

### Validation

#### `validateN3Usage(operation)`
Validate N3 operation complies with μ(O).

#### `validateOxigraphUsage(operation)`
Validate Oxigraph usage is appropriate.

#### `validateReenterOxigraph(operation)`
Verify N3 operation will re-enter Oxigraph.

## References

- **μ(O) Architecture**: `/docs/ontology/minimal-n3-routing.ttl`
- **N3 Audit Report**: `/docs/audit/n3-usage-violations.md`
- **Minimal N3 Integration**: `/packages/core/src/rdf/minimal-n3-integration.mjs`
- **Oxigraph**: https://www.npmjs.com/package/oxigraph
- **N3.js**: https://www.npmjs.com/package/n3

## License

MIT
