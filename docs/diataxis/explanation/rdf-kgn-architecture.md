# RDF-KGN Architecture

Deep dive into the architectural design of RDF-KGN integration.

## Overview

RDF-KGN (RDF Knowledge Graph from Nunjucks) is a template-driven system for generating, querying, and validating RDF knowledge graphs. It combines deterministic template rendering with semantic web technologies to create maintainable, type-safe RDF workflows.

## System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    RDF-KGN Architecture                      │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌───────────────┐      ┌────────────────┐                  │
│  │  Template     │      │   RDF Parser   │                  │
│  │   Engine      │─────▶│   (JSDoc →     │                  │
│  │  (Nunjucks)   │      │    RDF)        │                  │
│  └───────────────┘      └────────────────┘                  │
│         │                       │                            │
│         │                       │                            │
│         ▼                       ▼                            │
│  ┌───────────────┐      ┌────────────────┐                  │
│  │  RDF Filters  │      │  RDF Builder   │                  │
│  │  (expand,     │      │  (Turtle,      │                  │
│  │   contract)   │      │   JSON-LD)     │                  │
│  └───────────────┘      └────────────────┘                  │
│         │                       │                            │
│         └───────────┬───────────┘                            │
│                     ▼                                        │
│            ┌─────────────────┐                               │
│            │   RDF Store     │                               │
│            │  (Oxigraph)     │                               │
│            └─────────────────┘                               │
│                     │                                        │
│        ┌────────────┼────────────┐                           │
│        ▼            ▼            ▼                           │
│  ┌─────────┐ ┌──────────┐ ┌──────────┐                      │
│  │ SPARQL  │ │  SHACL   │ │   OWL    │                      │
│  │  Query  │ │Validation│ │Reasoning │                      │
│  └─────────┘ └──────────┘ └──────────┘                      │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. Template Engine (Nunjucks)

The template engine is the foundation of RDF-KGN, providing:

**Deterministic Rendering**
- Static build times ensure identical output across runs
- No random functions or timestamps
- Content-addressable through hashing

**Template Composition**
- Inheritance via `{% extends %}`
- Inclusion via `{% include %}`
- Macros for reusable patterns

**Control Flow**
- Loops: `{% for item in items %}`
- Conditionals: `{% if condition %}`
- Filters: `{{ value | filter }}`

**Why Nunjucks?**
- Server-side JavaScript (no browser required)
- Powerful template inheritance
- Synchronous and asynchronous rendering
- Extensible filter system

### 2. RDF Parser

Extracts semantic information from source code:

```
JavaScript/TypeScript Source
          │
          ▼
    JSDoc Parser
    (Babel AST)
          │
          ▼
   Structured Data
   (functions, classes,
    parameters, returns)
          │
          ▼
     RDF Builder
          │
          ▼
  RDF Triples (Turtle)
```

**Parsing Pipeline:**

1. **Comment Extraction**: Find all JSDoc blocks
2. **AST Traversal**: Walk the syntax tree
3. **Symbol Resolution**: Match comments to code elements
4. **Metadata Extraction**: Extract types, parameters, descriptions
5. **RDF Generation**: Convert to semantic triples

**Why This Approach?**
- Language-agnostic (works with any language with structured comments)
- Maintains single source of truth (code → documentation)
- Enables semantic queries across codebase
- Supports cross-referencing and validation

### 3. RDF Filters

Template filters ensure syntactically correct RDF:

**URI Handling**
- `expand`: CURIE → full URI
- `contract`: full URI → CURIE
- `rdfResource`: wrap in angle brackets

**Literal Handling**
- `rdfLiteral`: create RDF literal with escaping
- `rdfDatatype`: add XSD datatype
- Language tags for i18n

**SPARQL Integration**
- `sparqlVar`: ensure valid variable names
- Query generation from templates

**Design Pattern: Filter Chain**
```nunjucks
{{ value | clean | rdfLiteral('en') }}
<!-- Chain filters for complex transformations -->
```

### 4. RDF Store (Oxigraph)

Oxigraph provides high-performance RDF storage:

**Performance Benefits:**
- Written in Rust (compiled, not interpreted)
- 10-100x faster than JavaScript-based stores
- Efficient SPARQL query execution
- Low memory footprint

**Storage Model:**
```
Quad Store
  ├── Subject Index
  ├── Predicate Index
  ├── Object Index
  └── Graph Index
```

**Why Oxigraph over N3?**
- Performance: Native code vs JavaScript
- Standards compliance: Full SPARQL 1.1
- Maturity: Battle-tested in production
- Maintenance: Active development

### 5. SHACL Validator

Template-based validation shape generation:

**Validation Architecture:**
```
SHACL Templates
      │
      ▼
Shape Generator
      │
      ▼
SHACL Shapes (Turtle)
      │
      ▼
Validator Engine
      │
      ├──▶ RDF Data
      │
      ▼
Validation Report
```

**Shape Templates:**
- Basic node shapes
- Property constraints
- SPARQL-based rules
- Custom validators

**Why Template-Based?**
- DRY: Define once, generate variations
- Type-safe: Validated at generation time
- Maintainable: Update template, regenerate all
- Versioned: Track changes via git

## Data Flow

### Generate Flow

```
1. Source Code (JSDoc)
        │
        ▼
2. Parse JSDoc → Structured Data
        │
        ▼
3. Template Rendering → Turtle/JSON-LD
        │
        ▼
4. Store in Oxigraph
        │
        ▼
5. Query with SPARQL
```

### Validate Flow

```
1. RDF Data (Turtle/JSON-LD)
        │
        ▼
2. Load into Store
        │
        ▼
3. Generate SHACL Shapes (Templates)
        │
        ▼
4. Run Validation
        │
        ▼
5. Report Violations
```

### Query Flow

```
1. Build SPARQL Query (Template/Builder)
        │
        ▼
2. Validate Query Syntax
        │
        ▼
3. Execute on Store
        │
        ▼
4. Return Results (Bindings/Quads/Boolean)
```

## Design Decisions

### Decision 1: Nunjucks over Other Template Engines

**Alternatives Considered:**
- Handlebars: Limited control flow
- Mustache: No logic in templates
- EJS: Too JavaScript-specific
- Liquid: Ruby ecosystem

**Why Nunjucks:**
- ✅ Full-featured template language
- ✅ Inheritance and composition
- ✅ Synchronous rendering
- ✅ Extensible filter system
- ✅ Server-side JavaScript

### Decision 2: Oxigraph over N3

**N3 Limitations:**
- JavaScript implementation (slower)
- Basic SPARQL support
- Higher memory usage
- Limited to Node.js

**Oxigraph Benefits:**
- ✅ Native code (Rust → WASM)
- ✅ Full SPARQL 1.1
- ✅ Works in Node.js and browsers
- ✅ Production-grade performance

### Decision 3: Template-Based Generation over Code Generation

**Code Generation Approach:**
```javascript
// Generate JavaScript code
const code = generateRDFBuilder(schema);
eval(code); // ❌ Security risk, hard to debug
```

**Template-Based Approach:**
```nunjucks
{# Generate RDF directly #}
ex:{{ entity.id }}
  a {{ entity.type }} ;
  rdfs:label {{ entity.name | rdfLiteral }} .
```

**Why Templates:**
- ✅ Declarative (what, not how)
- ✅ Visual inspection
- ✅ No eval() security risks
- ✅ Easy to debug
- ✅ Supports multiple output formats

### Decision 4: Deterministic Mode by Default

**Motivation:**
- Reproducible builds
- Content-addressable output
- Git-friendly (no spurious diffs)
- Testing reliability

**Implementation:**
```javascript
{
  deterministicMode: true,
  staticBuildTime: '2024-01-01T00:00:00.000Z',
  disableRandom: true,
  hashSeed: 'constant'
}
```

**Trade-off:**
- ❌ Can't use `now()` in templates
- ✅ Guaranteed identical output

## Integration Points

### With @unrdf/core

RDF-KGN builds on @unrdf/core:

```javascript
import { createStore } from '@unrdf/oxigraph';
import { executeSelectSync } from '@unrdf/core/sparql/executor-sync.mjs';
import { buildRDFGraph } from '@unrdf/kgn/src/doc-generator/rdf-builder.mjs';

// Parse → Build → Store → Query
const parsed = parseFile(sourceFile);
const rdf = buildRDFGraph(parsed);
const store = createStore();
// Load RDF into store
const results = executeSelectSync(store, sparqlQuery);
```

### With @unrdf/hooks

Policy-driven RDF generation:

```javascript
import { PolicyEngine } from '@unrdf/hooks';

const policy = new PolicyEngine();

// Hook: validate RDF after generation
policy.on('rdf:generated', async (rdf) => {
  const valid = await validateSHACL(rdf, shapes);
  if (!valid) {
    throw new Error('RDF validation failed');
  }
});

// Generate with policy enforcement
const rdf = await generateRDF(data);
```

### With @unrdf/streaming

Real-time RDF generation:

```javascript
import { ChangeStream } from '@unrdf/streaming';

const stream = new ChangeStream();

stream.on('entity:created', async (entity) => {
  const rdf = await renderTemplate('entity.njk', entity);
  await store.add(parseRDF(rdf));
});
```

## Performance Characteristics

### Template Rendering

```
Operation                  Time      Notes
─────────────────────────────────────────────
Template compilation       ~5ms      Cached
Simple render (10 vars)    ~0.1ms    Interpolation only
Complex render (loops)     ~1-5ms    Depends on data size
```

### RDF Parsing

```
Operation                  Time      Throughput
──────────────────────────────────────────────
Parse 1 file (100 LOC)     ~10ms     10 files/s
Parse 10 files (1000 LOC)  ~100ms    100 files/s
```

### SPARQL Queries

```
Operation                  Time      Notes
─────────────────────────────────────────────
Simple SELECT (10 triples) ~1ms      Indexed lookup
Complex JOIN (1000 triples) ~10-50ms Depends on selectivity
CONSTRUCT (100 results)    ~5-10ms   Triple construction overhead
```

### Memory Usage

```
Component                  Memory    Notes
─────────────────────────────────────────────
Template engine            ~5 MB     Base overhead
RDF store (1M triples)     ~100 MB   Compressed indexes
SHACL validator            ~2 MB     Shape cache
```

## Scalability

### Horizontal Scaling

RDF-KGN supports distributed processing:

```javascript
// Worker 1: Parse files 1-100
const batch1 = parseFiles(files.slice(0, 100));

// Worker 2: Parse files 101-200
const batch2 = parseFiles(files.slice(100, 200));

// Merge results
const allRDF = mergeBatches([batch1, batch2]);
```

### Vertical Scaling

Optimize for large datasets:

**Streaming Processing:**
```javascript
const stream = createReadStream('large-dataset.json');
stream
  .pipe(jsonParser())
  .pipe(rdfGenerator())
  .pipe(createWriteStream('output.ttl'));
```

**Batch Processing:**
```javascript
for (const batch of chunks(data, 1000)) {
  const rdf = await generateBatch(batch);
  await appendToStore(rdf);
}
```

## Security Considerations

### Template Injection

**Risk:** User input in templates can execute arbitrary code

**Mitigation:**
```javascript
// ❌ Dangerous: User input directly in template
const template = `Hello {{ ${userInput} }}`;

// ✅ Safe: User input as context variable
const template = 'Hello {{ name }}';
const result = render(template, { name: sanitize(userInput) });
```

### SPARQL Injection

**Risk:** User input in SPARQL queries can access/modify unauthorized data

**Mitigation:**
```javascript
// ❌ Dangerous: String concatenation
const query = `SELECT * WHERE { ?s ?p "${userInput}" }`;

// ✅ Safe: Use query builder with validation
const query = new SPARQLBuilder()
  .select('s', 'p', 'o')
  .where('?s', '?p', escapeString(userInput))
  .build();
```

### Validation Bypass

**Risk:** Malformed RDF could bypass SHACL validation

**Mitigation:**
- Always validate RDF syntax before SHACL validation
- Use strict parsing mode
- Validate shapes themselves

## Future Directions

### Planned Enhancements

1. **Incremental RDF Generation**
   - Only regenerate changed entities
   - Delta-based updates

2. **Advanced Caching**
   - Memoize template renders
   - Cache parsed ASTs
   - Persistent query results

3. **Browser Support**
   - Client-side RDF generation
   - WASM-based Oxigraph
   - Offline-first workflows

4. **Visual Editor**
   - Template IDE
   - Live preview
   - Validation feedback

5. **Performance Monitoring**
   - Built-in profiling
   - Query optimization suggestions
   - Memory leak detection

### Research Areas

- **Provenance Tracking**: Record template → RDF lineage
- **Differential Privacy**: Privacy-preserving RDF generation
- **Federated Queries**: Distributed SPARQL across stores
- **Machine Learning**: Learn templates from examples

## Conclusion

RDF-KGN's architecture balances:

- **Developer Experience**: Template-based, declarative
- **Performance**: Native code (Oxigraph), efficient parsing
- **Correctness**: Type-safe filters, SHACL validation
- **Maintainability**: Deterministic, version-controlled
- **Scalability**: Streaming, batching, caching

This design enables building complex RDF workflows while maintaining simplicity and reliability.

## See Also

- [Template-Driven RDF Generation](./template-driven-rdf-generation.md)
- [Performance Optimization Strategies](./performance-optimization-strategies.md)
- [RDF-KGN API Reference](../reference/rdf-kgn-api.md)
