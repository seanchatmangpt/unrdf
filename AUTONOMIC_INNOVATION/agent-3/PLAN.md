# Lens Compiler - Agent 3 Plan

## 🎯 Objective

Implement a **bidirectional, deterministic, serializable lens compiler** that maps API payloads (JSON) to RDF graph operations and back, with stable IRI generation and skolem ID support.

## 🧠 Core Insight

**Lens = Declarative Bidirectional Transform**
- NOT a runtime function (closure)
- YES a compiled data structure (JSON-serializable)
- Determinism: Same input → Same output (IRIs, quads, ordering)
- Reversibility: payload → quads → payload (lossless)

## 📋 Files to Create

### Core Implementation (src/)

```
src/
├── lens.mjs              # Lens schema, validation, definition
├── compiler.mjs          # Compile lens → deterministic program
├── execute.mjs           # Execute compiled lens (to/from graph)
├── skolem.mjs            # Stable IRI + skolem ID generation
└── index.mjs             # Public API exports
```

### Tests (test/)

```
test/
├── lens.test.mjs         # Lens definition + compilation
├── execute.test.mjs      # Execution + determinism verification
├── skolem.test.mjs       # IRI/skolem stability tests
└── integration.test.mjs  # End-to-end bidirectional flow
```

### Examples (examples/)

```
examples/
├── customer-lens.mjs     # "Customer" domain lens demo
├── order-lens.mjs        # Nested entity example
└── run-demo.mjs          # Executable demonstration
```

---

## 📊 Data Structures

### 1. Lens Schema (Zod + JSDoc)

```javascript
/**
 * @typedef {Object} LensProfile
 * @property {string} namespace - Base IRI namespace (e.g., "https://example.org/")
 * @property {Object} prefixes - Prefix mappings { schema: "http://schema.org/" }
 * @property {Object} conventions - Naming conventions { idField: "id", case: "camelCase" }
 */

/**
 * @typedef {Object} PredicateMapping
 * @property {string} iri - Full IRI or prefixed (schema:name)
 * @property {string} [datatype] - XSD datatype (xsd:string, xsd:integer)
 * @property {Function} [toRDF] - Transform value for RDF (payload → quad)
 * @property {Function} [fromRDF] - Transform value from RDF (quad → payload)
 * @property {boolean} [required] - Field required in payload
 */

/**
 * @typedef {Object} SubjectRule
 * @property {string} pattern - IRI pattern with placeholders: "{namespace}{type}/{id}"
 * @property {string[]} keys - Fields used for IRI generation ["id"] or ["type", "localId"]
 * @property {string} [skolemPattern] - Optional skolem pattern for blank nodes
 */

/**
 * @typedef {Object} EntityMapping
 * @property {SubjectRule} subject - How to generate subject IRI
 * @property {Object<string, PredicateMapping>} predicates - Property → predicate mappings
 * @property {string} [type] - RDF type IRI (e.g., "schema:Customer")
 */

/**
 * @typedef {Object} Lens
 * @property {string} id - Stable lens identifier
 * @property {string} version - Semantic version
 * @property {LensProfile} profile - Namespace + conventions
 * @property {Object<string, EntityMapping>} mappings - Entity type → mapping rules
 * @property {Object} [metadata] - Optional metadata (author, description)
 */

/**
 * @typedef {Object} CompiledLens
 * @property {string} lensId - Original lens ID
 * @property {string} version - Lens version
 * @property {Object} profile - Resolved profile (namespaces, conventions)
 * @property {Object} compiledMappings - Pre-computed mapping rules
 * @property {string} [canonicalHash] - SHA-256 hash of canonical form
 */
```

### 2. Compiled Program Structure

**CRITICAL**: Compiled lens is a data structure, not a closure.

```javascript
{
  lensId: "customer-v1",
  version: "1.0.0",
  profile: {
    namespace: "https://example.org/",
    prefixes: { schema: "http://schema.org/", xsd: "http://www.w3.org/2001/XMLSchema#" },
    conventions: { idField: "id", case: "camelCase" }
  },
  compiledMappings: {
    Customer: {
      subject: {
        pattern: "{namespace}{type}/{id}",
        keys: ["id"],
        resolvedPattern: "https://example.org/Customer/{id}"  // Pre-resolved
      },
      type: "http://schema.org/Customer",
      predicates: [
        // Sorted canonically by property name
        {
          property: "email",
          iri: "http://schema.org/email",
          datatype: "http://www.w3.org/2001/XMLSchema#string",
          required: true,
          transformId: null  // No custom transform
        },
        {
          property: "name",
          iri: "http://schema.org/name",
          datatype: "http://www.w3.org/2001/XMLSchema#string",
          required: true,
          transformId: null
        }
      ]
    }
  },
  canonicalHash: "sha256:abc123..."  // For determinism verification
}
```

**Why Data Structure?**
- Serializable to JSON → store/transmit
- Debuggable → inspect compiled rules
- Cacheable → reuse across processes
- Deterministic → same lens → same compiled output

---

## 🔧 Key Algorithms

### 1. Lens Definition (`lens.mjs`)

```javascript
/**
 * Define a new lens with validation
 * @param {string} id - Lens identifier
 * @param {LensProfile} profile - Namespace + conventions
 * @param {Object<string, EntityMapping>} mappings - Entity mappings
 * @returns {Lens} Validated lens
 */
export function defineLens(id, profile, mappings) {
  // 1. Validate with Zod schema
  // 2. Normalize prefixes (expand all short forms)
  // 3. Validate IRI patterns (check placeholder syntax)
  // 4. Return frozen object (immutable)
}
```

**Validation Rules**:
- IRI patterns must be valid URI templates
- Placeholder keys must exist in entity schema
- Prefixes must be valid URIs
- No circular references in nested mappings

### 2. Lens Compilation (`compiler.mjs`)

```javascript
/**
 * Compile lens into deterministic executable program
 * @param {Lens} lens - Lens definition
 * @returns {CompiledLens} Serializable compiled program
 */
export function compileLens(lens) {
  // Algorithm:
  // 1. Resolve all IRI patterns (expand prefixes)
  // 2. Sort predicates canonically (alphabetical by property name)
  // 3. Pre-compute resolved IRIs where possible
  // 4. Extract transform functions → reference by ID (not inline)
  // 5. Compute canonical hash (SHA-256 of sorted JSON)
  // 6. Return data structure (NO closures)
}
```

**Determinism Guarantees**:
- Same lens → same compiled output (bit-identical)
- Predicate ordering: alphabetical by property name
- IRI resolution: prefix expansion deterministic
- Hash: SHA-256 of canonical JSON (sorted keys)

**Algorithm Steps**:
```javascript
1. Normalize lens:
   - Expand all prefixed IRIs to full URIs
   - Sort mapping keys alphabetically
   - Sort predicate keys alphabetically

2. Resolve patterns:
   - Replace {namespace} with profile.namespace
   - Keep {id}, {type}, etc. as placeholders
   - Store resolved pattern in compiledMappings

3. Extract transforms:
   - Cannot serialize functions → use transform registry
   - toRDF/fromRDF → transformId: "sha256:hash-of-function-body"
   - Built-in transforms: identity, parseDate, stringifyDate

4. Compute hash:
   - JSON.stringify(compiledLens, sortedKeys)
   - SHA-256 hash
   - Store as canonicalHash

5. Return frozen object
```

### 3. Execute Lens To Graph (`execute.mjs`)

```javascript
/**
 * Transform API payload to RDF quads using compiled lens
 * @param {Object} payload - API payload (e.g., { id: "123", name: "Alice" })
 * @param {CompiledLens} compiledLens - Compiled lens program
 * @param {Store} store - Oxigraph store to add quads
 * @returns {Quad[]} Generated quads
 */
export function executeLensToGraph(payload, compiledLens, store) {
  // Algorithm:
  // 1. Determine entity type from payload (root key or convention)
  // 2. Get compiled mapping for entity type
  // 3. Generate subject IRI using subject rule + payload values
  // 4. For each predicate mapping:
  //    a. Extract value from payload
  //    b. Apply toRDF transform if exists
  //    c. Create quad: (subject, predicate, object)
  //    d. Add to store
  // 5. Add rdf:type triple if specified
  // 6. Return all generated quads (sorted for determinism)
}
```

**Determinism Rules**:
- Quads generated in canonical order (sorted by predicate IRI)
- IRI generation uses stable pattern + hash (no UUIDs)
- Transform functions are pure (same input → same output)
- No timestamp/random data unless explicitly requested

**Example Execution**:
```javascript
Input payload:
{
  id: "cust-123",
  name: "Alice Smith",
  email: "alice@example.com"
}

Compiled lens: customer-v1

Generated quads (N-Triples):
<https://example.org/Customer/cust-123> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Customer> .
<https://example.org/Customer/cust-123> <http://schema.org/email> "alice@example.com"^^<http://www.w3.org/2001/XMLSchema#string> .
<https://example.org/Customer/cust-123> <http://schema.org/name> "Alice Smith"^^<http://www.w3.org/2001/XMLSchema#string> .
```

### 4. Execute Lens From Graph (`execute.mjs`)

```javascript
/**
 * Extract API payload from RDF quads using compiled lens
 * @param {string} subjectIRI - Subject IRI to extract
 * @param {CompiledLens} compiledLens - Compiled lens program
 * @param {Store} store - Oxigraph store with quads
 * @returns {Object} Reconstructed payload
 */
export function executeLensFromGraph(subjectIRI, compiledLens, store) {
  // Algorithm:
  // 1. Query all quads with subject = subjectIRI
  // 2. Determine entity type from rdf:type triple
  // 3. Get compiled mapping for entity type
  // 4. For each predicate in mapping:
  //    a. Find quad with matching predicate IRI
  //    b. Extract object value
  //    c. Apply fromRDF transform if exists
  //    d. Set payload property
  // 5. Extract ID from IRI using subject pattern (reverse operation)
  // 6. Return payload object
}
```

**Reversibility Test**:
```javascript
// Must be true (lossless round-trip):
payload1 === executeLensFromGraph(
  executeLensToGraph(payload1, lens, store),
  lens,
  store
)
```

### 5. Stable IRI Generation (`skolem.mjs`)

```javascript
/**
 * Create stable IRI for entity using lens rules
 * @param {Object} entity - Entity data
 * @param {string} lensId - Lens identifier
 * @param {SubjectRule} rule - Subject generation rule
 * @returns {string} Stable IRI
 */
export function createStableIRI(entity, lensId, rule) {
  // Algorithm:
  // 1. Extract key values from entity using rule.keys
  // 2. Sort keys canonically
  // 3. If all keys present:
  //    → Use pattern substitution: pattern.replace(/{id}/, entity.id)
  // 4. If keys missing:
  //    → Use content hash: sha256(JSON.stringify(entity, sortedKeys))
  // 5. Ensure valid IRI encoding (percent-encode special chars)
  // 6. Return IRI string
}
```

**Determinism Rules**:
- Same entity data → same IRI (always)
- No timestamps, UUIDs, random data
- Hash-based fallback when explicit ID missing
- Canonical JSON serialization (sorted keys)

**Example**:
```javascript
Entity: { id: "123", name: "Alice" }
Rule: { pattern: "{namespace}Customer/{id}", keys: ["id"] }
Lens: { profile: { namespace: "https://example.org/" } }

Result: "https://example.org/Customer/123"

Entity: { name: "Bob" }  // No ID
Rule: { pattern: "{namespace}Customer/{hash}", keys: ["id"] }

Result: "https://example.org/Customer/sha256:abc123..."
```

### 6. Skolem ID Generation (`skolem.mjs`)

```javascript
/**
 * Create deterministic skolem ID (blank node alternative)
 * @param {string} skolemPattern - Pattern template
 * @param {Object} values - Values for pattern
 * @returns {string} Skolem IRI
 */
export function createSkolemID(skolemPattern, values) {
  // Algorithm:
  // 1. Sort values canonically by key
  // 2. Compute content hash: sha256(JSON.stringify(values, sortedKeys))
  // 3. Substitute hash into pattern
  // 4. Prefix with skolem namespace: urn:skolem:
  // 5. Return IRI
}
```

**Use Case**: Address without explicit ID
```javascript
Payload:
{
  customer: { id: "123" },
  address: { street: "Main St", city: "NYC" }  // No ID
}

Skolem pattern: "urn:skolem:address:{hash}"
Values: { street: "Main St", city: "NYC" }
Hash: sha256("{"city":"NYC","street":"Main St"}")

Result: "urn:skolem:address:sha256:def456..."
```

---

## 🧪 Test Cases (Minimum 5 per file)

### test/lens.test.mjs

```javascript
describe('Lens Definition', () => {
  test('1. Define valid lens with all fields', () => {
    // ❓ Does defineLens accept valid schema?
    // ❓ Are all fields preserved?
    // Evidence: lens object matches input
  });

  test('2. Reject invalid IRI pattern', () => {
    // ❓ Does validation catch malformed patterns?
    // Evidence: throws ZodError
  });

  test('3. Normalize prefixed IRIs', () => {
    // ❓ Are prefixes expanded correctly?
    // Evidence: schema:name → http://schema.org/name
  });

  test('4. Validate required fields', () => {
    // ❓ Are missing namespace/mappings caught?
    // Evidence: throws validation error
  });

  test('5. Freeze lens object (immutable)', () => {
    // ❓ Can lens be modified after creation?
    // Evidence: Object.isFrozen(lens) === true
  });
});
```

### test/execute.test.mjs

```javascript
describe('Lens Execution', () => {
  test('1. Transform Customer payload → quads', () => {
    // ❓ Are all properties mapped to quads?
    // ❓ Are quads sorted canonically?
    // Evidence: 4 quads (type + 3 properties), sorted by predicate
  });

  test('2. Reverse: quads → payload (lossless)', () => {
    // ❓ Is round-trip lossless?
    // Evidence: payload1 === fromGraph(toGraph(payload1))
  });

  test('3. Determinism: same payload → same quads', () => {
    // ❓ Execute twice → identical quads?
    // Evidence: deepEqual(quads1, quads2)
  });

  test('4. Handle missing optional fields', () => {
    // ❓ Does execution skip undefined properties?
    // Evidence: only 2 quads if 1 property undefined
  });

  test('5. Error on missing required field', () => {
    // ❓ Does execution fail on missing required?
    // Evidence: throws error with field name
  });

  test('6. Handle nested entities (Address)', () => {
    // ❓ Are nested objects mapped correctly?
    // Evidence: Customer + Address quads generated
  });

  test('7. Stable IRI generation', () => {
    // ❓ Same ID → same IRI?
    // Evidence: IRI === "https://example.org/Customer/123"
  });
});
```

### test/skolem.test.mjs

```javascript
describe('Stable IRI & Skolem', () => {
  test('1. Pattern substitution: {namespace}{type}/{id}', () => {
    // ❓ Are placeholders replaced correctly?
    // Evidence: IRI matches expected pattern
  });

  test('2. Hash fallback when ID missing', () => {
    // ❓ Is content hash used when no ID?
    // Evidence: IRI contains sha256:...
  });

  test('3. Deterministic hash (same data → same hash)', () => {
    // ❓ Same entity → same hash?
    // Evidence: hash1 === hash2
  });

  test('4. Skolem ID from content', () => {
    // ❓ Is skolem IRI deterministic?
    // Evidence: urn:skolem:address:sha256:abc123...
  });

  test('5. IRI encoding (special characters)', () => {
    // ❓ Are spaces/special chars encoded?
    // Evidence: "Name With Spaces" → "Name%20With%20Spaces"
  });
});
```

### test/integration.test.mjs

```javascript
describe('End-to-End Bidirectional', () => {
  test('1. Customer API → Graph → API', () => {
    // ❓ Complete round-trip lossless?
    // Evidence: finalPayload === initialPayload
  });

  test('2. Multiple entities in store', () => {
    // ❓ Can extract correct entity from multi-entity store?
    // Evidence: 2 customers → query 1 → correct payload
  });

  test('3. Lens compilation determinism', () => {
    // ❓ Compile same lens twice → identical result?
    // Evidence: compiledLens1.canonicalHash === compiledLens2.canonicalHash
  });

  test('4. Cross-references between entities', () => {
    // ❓ Customer → Order → Address (linked)?
    // Evidence: quads contain object references
  });

  test('5. Large payload (100 fields)', () => {
    // ❓ Performance acceptable (<100ms)?
    // Evidence: time execution with console.time()
  });
});
```

---

## 🎯 Implementation Strategy (Big Bang 80/20)

### Phase 1: Core Primitives (20% → 80% value)

**Files**: `lens.mjs`, `skolem.mjs`

**Deliverables**:
- Zod schemas for Lens/Profile/Mapping
- `defineLens()` with validation
- `createStableIRI()` with pattern substitution + hash fallback
- `createSkolemID()` with deterministic hashing

**Tests**: 15 tests (lens.test.mjs, skolem.test.mjs)

**Success Criteria**:
- ✅ All validation rules enforce
- ✅ IRI generation deterministic (run 100x → same output)
- ✅ Hash collision probability < 2^-128

### Phase 2: Compiler (Critical Path)

**Files**: `compiler.mjs`

**Deliverables**:
- `compileLens()` with deterministic output
- Canonical JSON serialization
- SHA-256 hash computation
- Prefix expansion + pattern resolution

**Tests**: 8 tests (compiler.test.mjs - new file)

**Success Criteria**:
- ✅ Compiled lens is pure data (JSON-serializable)
- ✅ Same lens → same canonicalHash (100/100 attempts)
- ✅ No closures in output (verify with typeof checks)

### Phase 3: Execution Engine

**Files**: `execute.mjs`

**Deliverables**:
- `executeLensToGraph()` with deterministic quad generation
- `executeLensFromGraph()` with lossless extraction
- Predicate sorting (canonical order)
- Transform application (toRDF/fromRDF)

**Tests**: 12 tests (execute.test.mjs)

**Success Criteria**:
- ✅ Round-trip test: payload → quads → payload (lossless)
- ✅ Determinism: 100 executions → identical quads (sorted)
- ✅ Performance: <10ms for 50-field payload

### Phase 4: Examples & Integration

**Files**: `customer-lens.mjs`, `order-lens.mjs`, `run-demo.mjs`

**Deliverables**:
- Working Customer domain lens
- Nested entity example (Customer → Address)
- Executable demo script

**Tests**: 5 integration tests (integration.test.mjs)

**Success Criteria**:
- ✅ Demo runs without errors
- ✅ Output matches expected (show diffs)
- ✅ All integration tests pass

---

## 📦 Dependencies

```json
{
  "dependencies": {
    "@unrdf/core": "workspace:*",
    "@unrdf/oxigraph": "workspace:*",
    "zod": "^3.22.4"
  },
  "optionalDependencies": {
    "@unrdf/kgc-4d": "workspace:*"
  }
}
```

**Why Optional**:
- KGC-4D provides advanced ID generation
- Not required for basic lens functionality
- Use if available, fallback to built-in hash

---

## 🔄 Public API (src/index.mjs)

```javascript
// Lens definition
export { defineLens } from './lens.mjs';

// Compilation
export { compileLens } from './compiler.mjs';

// Execution
export {
  executeLensToGraph,
  executeLensFromGraph
} from './execute.mjs';

// IRI/Skolem utilities
export {
  createStableIRI,
  createSkolemID
} from './skolem.mjs';

// Types (JSDoc)
/**
 * @typedef {import('./lens.mjs').Lens} Lens
 * @typedef {import('./lens.mjs').LensProfile} LensProfile
 * @typedef {import('./compiler.mjs').CompiledLens} CompiledLens
 */
```

---

## 🚨 Critical Constraints

### 1. Determinism (Non-Negotiable)

**Verification Command**:
```bash
timeout 5s node test/determinism-verify.mjs
```

**Test**:
```javascript
const lens1 = compileLens(customerLens);
const lens2 = compileLens(customerLens);

assert(lens1.canonicalHash === lens2.canonicalHash);

const quads1 = executeLensToGraph(payload, lens1, store1);
const quads2 = executeLensToGraph(payload, lens2, store2);

assert.deepEqual(quads1, quads2);
```

**Evidence Required**:
- ❓ Run 100 times → same hash?
- ❓ Show hash values (visual inspection)
- ❓ Diff quads (0 differences)

### 2. Serialization (No Closures)

**Verification Command**:
```bash
timeout 5s node test/serialization-verify.mjs
```

**Test**:
```javascript
const compiled = compileLens(lens);
const json = JSON.stringify(compiled);
const restored = JSON.parse(json);

// All properties must survive serialization
assert.deepEqual(compiled, restored);

// No functions in output
const hasFunctions = JSON.stringify(compiled).includes('function');
assert(!hasFunctions);
```

**Evidence Required**:
- ❓ JSON.stringify succeeds (no errors)?
- ❓ Round-trip preserves all data?
- ❓ No 'function' strings in JSON?

### 3. Reversibility (Lossless)

**Verification Command**:
```bash
timeout 5s node test/reversibility-verify.mjs
```

**Test**:
```javascript
const store = createStore();
const quads = executeLensToGraph(originalPayload, compiledLens, store);
const reconstructed = executeLensFromGraph(quads[0].subject.value, compiledLens, store);

assert.deepEqual(originalPayload, reconstructed);
```

**Evidence Required**:
- ❓ Deep equality check passes?
- ❓ Show before/after payloads (visual)
- ❓ Property count matches?

---

## 🎓 Counter-Practice Lessons Applied

### ✅ DO (Evidence-Based)

1. **Pure Functions**: `createStableIRI()`, `compileLens()` have NO side effects
2. **Deterministic Output**: Same input → Same output (verified with 100 iterations)
3. **Data Structures**: Compiled lens is JSON, not closures
4. **Zod Validation**: All inputs validated before processing
5. **Canonical Ordering**: Predicates sorted alphabetically

### 🚫 DON'T (Will Fail)

1. ❌ Runtime closures in compiled lens (breaks serialization)
2. ❌ UUIDs for IRI generation (breaks determinism)
3. ❌ Unsorted quads (breaks determinism verification)
4. ❌ Inline transforms (use transform registry)
5. ❌ Trust claims without running tests

---

## 📊 Success Metrics

### Test Coverage

```bash
timeout 10s npm test -- --coverage
```

**Required**:
- ✅ Line coverage ≥ 90%
- ✅ Branch coverage ≥ 85%
- ✅ Function coverage 100%
- ✅ All tests pass (50/50)

### Determinism Verification

```bash
timeout 15s node test/determinism-100x.mjs
```

**Required**:
- ✅ 100 compilations → 1 unique hash
- ✅ 100 executions → identical quads (sorted)
- ✅ Hash collision count: 0

### Performance Benchmarks

```bash
timeout 10s node test/benchmark.mjs
```

**Required**:
- ✅ Compilation: <5ms per lens
- ✅ Execution (50 fields): <10ms
- ✅ IRI generation: <1ms
- ✅ Round-trip (payload → graph → payload): <20ms

---

## 🏁 Definition of Done

### Code Quality

- [ ] All files <500 lines (check with `wc -l src/*.mjs`)
- [ ] JSDoc 100% coverage (check with `npm run lint`)
- [ ] No `from 'n3'` imports (grep verification)
- [ ] All functions pure (no OTEL in implementation)

### Testing

- [ ] 50+ tests written and passing (show `npm test` output)
- [ ] Coverage ≥90% (show report)
- [ ] Determinism verified 100/100 (show log)
- [ ] Performance benchmarks met (show timing)

### Documentation

- [ ] README.md with API examples
- [ ] JSDoc on all exports
- [ ] Examples run without errors (`node examples/run-demo.mjs`)

### Integration

- [ ] Exports from src/index.mjs (verify with `node -e "import('./src/index.mjs')"`)
- [ ] Dependencies correct in package.json
- [ ] OTEL validation ≥80/100 (if OTEL added)

---

## 🤔 Adversarial PM Questions

**Before claiming "Lens Compiler complete"**:

### Determinism

- ❓ Did I RUN 100 iterations or just code it?
- ❓ Can I PROVE hash is identical (show hash values)?
- ❓ What BREAKS if non-deterministic? (downstream consumers)
- ❓ Evidence: `node test/determinism-100x.mjs` output

### Serialization

- ❓ Did I verify JSON round-trip or assume it works?
- ❓ Can I SHOW the JSON output (cat compiled-lens.json)?
- ❓ What BREAKS if functions leak in? (transmission fails)
- ❓ Evidence: `typeof compiled.mappings[key]` never "function"

### Reversibility

- ❓ Did I test ACTUAL round-trip or just one direction?
- ❓ Can I PROVE lossless (show diff of payloads)?
- ❓ What BREAKS if lossy? (data corruption)
- ❓ Evidence: `diff <(echo $original) <(echo $reconstructed)` → 0 lines

### Performance

- ❓ Did I MEASURE execution time or guess?
- ❓ Can I SHOW benchmark output (ms per operation)?
- ❓ What BREAKS if slow? (API timeout)
- ❓ Evidence: `time node test/benchmark.mjs` → <10s total

---

## 🔗 Related Work

**Depends On**:
- Agent 1: Graph Store (uses createStore, quads)
- Agent 2: Autonomic Controller (orchestrates lens execution)

**Enables**:
- Agent 4: Neural Network Mapping (uses lens for I/O)
- Agent 5: DSL Parser (uses lens for AST → Graph)
- Agent 6: Optimization Engine (uses lens for rule extraction)

**Integration Point**:
```javascript
// Agent 2 calls:
const compiledLens = compileLens(customerLens);
const quads = executeLensToGraph(apiPayload, compiledLens, autonomicStore);
```

---

## 📝 Example Usage (Preview)

```javascript
import { defineLens, compileLens, executeLensToGraph } from '@unrdf/lens';
import { createStore } from '@unrdf/oxigraph';

// 1. Define lens
const customerLens = defineLens('customer-v1', {
  namespace: 'https://example.org/',
  prefixes: { schema: 'http://schema.org/' },
  conventions: { idField: 'id' }
}, {
  Customer: {
    subject: {
      pattern: '{namespace}Customer/{id}',
      keys: ['id']
    },
    type: 'schema:Customer',
    predicates: {
      name: { iri: 'schema:name', required: true },
      email: { iri: 'schema:email', required: true }
    }
  }
});

// 2. Compile lens
const compiled = compileLens(customerLens);
console.log('Hash:', compiled.canonicalHash); // sha256:abc123...

// 3. Execute transformation
const payload = { id: 'cust-123', name: 'Alice', email: 'alice@example.com' };
const store = createStore();
const quads = executeLensToGraph(payload, compiled, store);

console.log('Generated quads:', quads.length); // 4 (type + 3 properties)
```

---

## 🎯 Final Truth

**The Adversarial PM Question**: *If I claim "Lens Compiler is deterministic, serializable, and reversible", can I prove it RIGHT NOW with command output?*

**Evidence Required**:
1. `node test/determinism-100x.mjs` → "100/100 hashes identical"
2. `node test/serialization-verify.mjs` → "JSON round-trip: PASS"
3. `node test/reversibility-verify.mjs` → "Lossless: PASS (0 diffs)"
4. `npm test` → "50/50 tests passing"

If ANY command fails → NOT done. No excuses, no "should work", only EVIDENCE.

---

**Plan Status**: READY FOR IMPLEMENTATION

**Next Steps**:
1. Review this plan with adversarial lens (question everything)
2. Implement Phase 1 (lens.mjs, skolem.mjs)
3. Run tests + show output (not "tests pass" but ACTUAL output)
4. Iterate through phases 2-4
5. Final verification with all adversarial tests

**Remember**: Code is cheap. Proof is expensive. Measure, don't assume.
