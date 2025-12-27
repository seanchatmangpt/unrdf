# Agent 3: Lens Compiler and API Projection

## Mission
Implement a Lens primitive that deterministically maps existing API payloads ↔ RDF graph operations with stable identifiers and lossless round-trip guarantees.

## Architecture

### Core Principles
1. **Determinism**: Same (domain, entity, attr) → same IRI always
2. **Stability**: IRI generation never changes for given inputs
3. **Lossless**: DTO → RDF → DTO preserves all information
4. **No API Churn**: Existing APIs unmodified, lens maps to RDF

### Components

#### 1. `stable-ids.mjs` - Stable Identifier Generation
**Functions**:
- `stableIRI(domain, entity, attr)` → IRI string
  - Uses SHA-256 hash of `domain:entity:attr`
  - Format: `http://kgc.internal/{domain}/{entity}/{attr}#{hash}`
  - Deterministic: 1000 calls with same inputs → identical IRI

- `stableSkolem(template, values)` → blank node string
  - Template: "customer-{id}-{attr}"
  - Values: { id: "123", attr: "address" }
  - Uses SHA-256 hash of `template + JSON.stringify(values)`
  - Format: `_:skolem-{hash}`

**Guarantees**:
- Collision resistance: SHA-256 provides 2^256 space
- Determinism: Pure function, no random state
- Portability: Hash-based, works across systems

#### 2. `lens.mjs` - Lens Definition and Compilation
**Functions**:
- `defineLens(name, rules)` → Lens object
  - Rules: `{ dto_field, rdf_predicate, type, validator?, transform? }`
  - Returns lens metadata with name and rules

- `compileLens(lens)` → LensProgram (JSON)
  - Converts lens definition to executable JSON program
  - No closures, fully serializable
  - Format:
    ```json
    {
      "name": "CustomerLens",
      "version": "1.0.0",
      "toGraph": [...mapping rules...],
      "fromGraph": [...reverse mapping rules...],
      "stableIds": { "domain": "...", "entity": "..." }
    }
    ```

- `executeLensToGraph(dtoPayload, lensProgram)` → { quads, subjects }
  - Transform DTO → RDF quads using @unrdf/oxigraph
  - Generate stable IRIs for subjects
  - Return array of quads + subject IRIs

- `executeLensFromGraph(subjects, store, lensProgram)` → dtoPayload
  - Query store for subject IRIs
  - Project RDF quads → DTO shape
  - Reconstruct original DTO structure

**LensProgram Structure**:
```json
{
  "name": "CustomerLens",
  "version": "1.0.0",
  "stableIds": {
    "domain": "kgc-facade",
    "entity": "customer"
  },
  "toGraph": [
    {
      "dto_field": "id",
      "rdf_predicate": "http://schema.org/identifier",
      "type": "string",
      "transform": "identity"
    },
    {
      "dto_field": "name",
      "rdf_predicate": "http://schema.org/name",
      "type": "string",
      "transform": "identity"
    }
  ],
  "fromGraph": [
    {
      "rdf_predicate": "http://schema.org/identifier",
      "dto_field": "id",
      "type": "string",
      "transform": "identity"
    },
    {
      "rdf_predicate": "http://schema.org/name",
      "dto_field": "name",
      "type": "string",
      "transform": "identity"
    }
  ]
}
```

#### 3. `demo-customer-lens.mjs` - Reference Implementation
**Customer DTO**:
```json
{
  "id": "customer-123",
  "name": "Alice Johnson",
  "email": "alice@example.com",
  "registeredAt": "2025-01-15T10:30:00Z"
}
```

**RDF Mapping**:
- Subject: `stableIRI("kgc-facade", "customer", "customer-123")`
- `schema:identifier` → id
- `schema:name` → name
- `schema:email` → email
- `schema:dateCreated` → registeredAt

**Round-trip Test**:
1. DTO → executeLensToGraph → quads
2. Store quads in @unrdf/oxigraph
3. executeLensFromGraph → reconstructed DTO
4. Deep equality check: original === reconstructed

#### 4. `test.mjs` - Comprehensive Test Suite
**Test Coverage**:
1. Stable IRI determinism (1000 iterations)
2. Skolem determinism (100 iterations)
3. Lens round-trip (DTO → RDF → DTO)
4. Lens program serialization (JSON portability)
5. Multiple DTOs with same ID space
6. Hash collision resistance (statistical test)

**Success Criteria**:
- All tests pass (100% pass rate)
- Round-trip preserves byte-identical DTO
- IRI generation < 1ms per operation
- No dependencies on external state

## Dependencies
- `node:crypto` - SHA-256 hashing
- `@unrdf/oxigraph` - RDF store and data factory
- No external lens libraries (pure implementation)

## Performance Targets
- IRI generation: < 1ms per call
- Lens compilation: < 10ms for 50 rules
- DTO → RDF: < 5ms for 20 fields
- RDF → DTO: < 5ms for 20 fields
- Round-trip: < 15ms total

## Determinism Guarantees
1. **Hash Stability**: SHA-256 spec never changes
2. **Input Normalization**: JSON.stringify for complex values
3. **No Random State**: Pure functions only
4. **Version Locking**: Lens programs include version field

## API Stability
- Existing service APIs unchanged
- Lens applied at facade boundary
- Consumers unaware of RDF transformation
- Migration path: service → lens → graph → lens → service

## Files Manifest
1. `PLAN.md` - This document
2. `stable-ids.mjs` - IRI/skolem generation (pure functions)
3. `lens.mjs` - Lens definition/compilation/execution (4 functions)
4. `demo-customer-lens.mjs` - Reference Customer lens
5. `index.mjs` - Public API exports
6. `test.mjs` - Comprehensive test suite (6 test cases)

## Success Metrics
- ✅ Tests pass: 6/6 (100%)
- ✅ Round-trip lossless: byte-identical DTO
- ✅ Determinism: 1000 iterations → identical IRI
- ✅ Portability: LensProgram JSON-serializable
- ✅ Performance: < 15ms round-trip
- ✅ Zero API changes to existing services

## Next Steps (Agent 4+)
1. Integrate lens with KGC-4D facade
2. Create lens library for common patterns
3. Add OTEL spans for lens execution
4. Build lens registry for versioned programs
