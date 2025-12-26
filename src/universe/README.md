# RDF Universe - Core Partition Management Layer

## Overview

The RDF Universe module provides a foundational layer for managing RDF data across multiple logical partitions with strict governance, ontology allow-listing, and content verification through deterministic hashing.

## Architecture

### Components

1. **rdf-utils.mjs** - RDF utility functions
   - Turtle/TTL loading and parsing
   - IRI validation (RFC 3987 compliant)
   - Deterministic SHA256 content hashing
   - Quad canonicalization and serialization
   - Namespace extraction

2. **ontology-release.mjs** - Ontology version management
   - `OntologyRelease` - Represents a specific ontology version with content hash
   - `AllowedOntology` - Manages multiple releases of an ontology

3. **registry.mjs** - Ontology allow-listing
   - `OntologyRegistry` - Central registry for allowed ontologies
   - Lookup by namespace IRI or content hash
   - Standard registry with 7 W3C ontologies

4. **partition.mjs** - Partition classes
   - `Partition` (abstract base class)
   - `IndustrialSubstrate` - Foundational W3C ontologies (read-only)
   - `CorporateCanon` - Enterprise canonical models (read-only)
   - `BusinessUnitOverlay` - Business unit extensions (read-write)
   - `RegionalOverlay` - Regional data and localization (read-write)
   - `ExecutionLedger` - Execution events and audit trails (read-write)
   - `SystemPolicyPartition` - Policies and governance (read-only)

5. **universe.mjs** - Universe orchestration
   - `Universe` - Manages all partitions in strict order
   - Unified query and merge capabilities
   - Partition validation and governance

## Partition Architecture

### Partition Order (Priority)

1. **IndustrialSubstrate** (Highest Priority)
   - Contains: PROV, ODRL, SKOS, OWL-Time, DCAT, ORG, OA
   - Status: Read-only
   - Protected: All 7 namespaces

2. **CorporateCanon**
   - Contains: Enterprise-wide canonical models
   - Status: Read-only (by default)

3. **BusinessUnitOverlay**
   - Contains: Business unit-specific extensions
   - Status: Read-write

4. **RegionalOverlay**
   - Contains: Region-specific data
   - Status: Read-write

5. **ExecutionLedger**
   - Contains: Events, transactions, audit trails
   - Status: Read-write

6. **SystemPolicyPartition** (Lowest Priority)
   - Contains: System policies, access controls
   - Status: Read-only (by default)

## Standard Ontologies (Industrial Substrate)

The Industrial Substrate partition includes exactly 7 W3C ontologies:

| Ontology | Namespace IRI | Description |
|----------|---------------|-------------|
| PROV-O | `http://www.w3.org/ns/prov#` | W3C Provenance Ontology |
| ODRL | `http://www.w3.org/ns/odrl/2/` | Open Digital Rights Language |
| SKOS | `http://www.w3.org/2004/02/skos/core#` | Simple Knowledge Organization System |
| OWL-Time | `http://www.w3.org/2006/time#` | Time Ontology in OWL |
| DCAT | `http://www.w3.org/ns/dcat#` | Data Catalog Vocabulary |
| ORG | `http://www.w3.org/ns/org#` | Organization Ontology |
| OA | `http://www.w3.org/ns/oa#` | Web Annotation Ontology |

### Content Hashes

The standard registry uses placeholder hashes (all zeros with incremental last digit). In production, these should be replaced with actual SHA256 hashes of the ontology content:

```javascript
const actualHashes = {
  'http://www.w3.org/ns/prov#': 'abc123...', // Compute from actual PROV-O TTL
  'http://www.w3.org/ns/odrl/2/': 'def456...', // Compute from actual ODRL TTL
  // ... etc for all 7 ontologies
};

const universe = Universe.createStandard(actualHashes);
```

## Usage Examples

### Basic Setup

```javascript
import { Universe } from './universe/index.mjs';

// Create universe with standard configuration
const universe = Universe.createStandard();

// Access partitions
const industrial = universe.getPartition('IndustrialSubstrate');
const businessUnit = universe.getPartition('BusinessUnitOverlay');

console.log('Partition count:', universe.getAllPartitions().length); // 6
```

### Loading Data

```javascript
// Load business unit data
const businessUnit = universe.getPartition('BusinessUnitOverlay');

businessUnit.loadTurtle(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:department1 a ex:Department ;
    ex:name "Engineering" .
`);

console.log('BusinessUnitOverlay size:', businessUnit.size);
```

### Querying

```javascript
// Query across all partitions
const results = universe.query(`
  PREFIX prov: <http://www.w3.org/ns/prov#>
  SELECT ?activity WHERE {
    ?activity a prov:Activity .
  }
`);

console.log('Found activities:', results.bindings.length);
```

### Content Verification

```javascript
// Compute content hash for a partition
const hash = await businessUnit.getContentHash();
console.log('BusinessUnitOverlay hash:', hash);

// Verify against registry
const isAllowed = universe.registry.isHashAllowed(hash);
console.log('Hash is allowed:', isAllowed);
```

### Validation

```javascript
// Validate Industrial Substrate
const validation = await universe.validateIndustrialSubstrate();

if (validation.valid) {
  console.log('✅ IndustrialSubstrate is valid');
  console.log('Namespaces:', validation.namespaces);
  console.log('Content hash:', validation.contentHash);
} else {
  console.error('❌ Validation failed:', validation.error);
}
```

## Design Principles

### 1. Deterministic Hashing
All content hashing is deterministic:
- Quads are canonicalized (sorted) before hashing
- Uses SHA256 for cryptographic strength
- Reproducible across different systems

### 2. Immutability
- Partition configurations are frozen (Object.freeze)
- Read-only partitions enforce immutability
- Protected namespaces prevent modification

### 3. Type Safety
- Zod validation for runtime type checking
- JSDoc type hints for IDE support
- No TypeScript in source (per project standards)

### 4. Pure Functions
- RDF utilities are pure functions
- No side effects in core operations
- Testable and composable

### 5. No N3 Imports
- All RDF operations use `@unrdf/oxigraph`
- Compliant with project migration strategy
- Uses `createStore()` and `dataFactory` from oxigraph

## Implementation Details

### File Statistics

```
43 lines   - index.mjs (exports)
242 lines  - ontology-release.mjs
334 lines  - rdf-utils.mjs
334 lines  - registry.mjs
365 lines  - partition.mjs
388 lines  - universe.mjs
353 lines  - universe.test.mjs
────────────────────────────
2,059 lines total
```

### Dependencies

- `@unrdf/oxigraph` - RDF store and SPARQL engine
- `hash-wasm` - SHA256 hashing (deterministic)
- `zod` - Runtime type validation

### Test Coverage

The test suite (`universe.test.mjs`) validates:

1. ✅ RDF Utils
   - IRI validation (RFC 3987)
   - Turtle loading and parsing
   - Deterministic hashing
   - Namespace extraction

2. ✅ Ontology Classes
   - OntologyRelease creation and validation
   - AllowedOntology management
   - JSON serialization/deserialization

3. ✅ OntologyRegistry
   - Registration and lookups
   - Allow-listing verification
   - Standard registry with 7 ontologies

4. ✅ Partitions
   - Abstract base class enforcement
   - All 6 partition subclasses
   - Read-only/read-write enforcement

5. ✅ Universe
   - Partition management
   - Merge operations
   - Validation
   - Cross-partition queries

6. ✅ Integration
   - End-to-end workflows
   - Content loading and querying
   - Hash computation and verification

## Production Checklist

Before deploying to production:

- [ ] Compute actual SHA256 hashes for all 7 W3C ontologies
- [ ] Load ontology content into IndustrialSubstrate
- [ ] Validate IndustrialSubstrate with `validateIndustrialSubstrate()`
- [ ] Configure protected namespaces in SystemPolicyPartition
- [ ] Set up OTEL spans for partition operations
- [ ] Implement audit logging for ExecutionLedger
- [ ] Test merge performance with realistic data volumes
- [ ] Configure access controls for read-write partitions

## Future Enhancements

1. **Streaming Support**
   - Stream large partitions without loading all into memory
   - Incremental merge operations

2. **Time Travel**
   - Immutable append-only ExecutionLedger
   - Point-in-time partition snapshots

3. **Federation**
   - Distributed partitions across nodes
   - Remote partition queries

4. **Encryption**
   - Partition-level encryption at rest
   - Encrypted content hashes

## License

MIT License (per project)

## Author

UNRDF Project - Backend API Developer Agent
