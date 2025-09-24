# unrdf

**Opinionated composable framework for RDF knowledge operations with Knowledge Hooks**

unrdf is the opinionated RDF framework for JavaScript that transforms knowledge graphs into intelligent, reactive systems. Built on N3.js, Comunica, SHACL, and Zod, unrdf provides **Knowledge Hooks** — deterministic, auditable triggers that detect meaningful changes in RDF graphs and respond with precise actions.

## 🎯 **Knowledge Hooks: The Game Changer**

**Knowledge Hooks** are pure data + pure functions that evaluate knowledge change and emit signed receipts. They turn SPARQL results and SHACL conformance into deterministic, auditable actions — without glue code or bespoke pipelines.

### **What Knowledge Hooks Do**

- **ASK predicates** (true/false intent detection)
- **SHACL predicates** (shape conformance/violations)  
- **DELTA predicates** (stable row digests, added/removed quads)
- **THRESHOLD predicates** (counts, metrics, cohort checks)
- **WINDOW predicates** (tumbling/hopping time windows)

Every evaluation emits a **Receipt** (why it fired, what changed, hashes/provenance, durations), enabling compliance-grade audit trails.

### **Why Knowledge Hooks Are Different**

- **One Store Rule.** A single **N3.Store** context; no multiple models, no confusion.
- **Functional core.** Hooks are pure; evaluation is referentially transparent.
- **Provenance by default.** URDNA2015 canonical hashes for data, queries, shapes, rules, and Zod schemas.
- **JSDoc, not TypeScript.** Runtime truth, minimal surface, explicit contracts via Zod.
- **80/20 dark matter.** Covers the high-value cases: compliance gates, config drift, KPI thresholds, doc→knowledge sync, CI guardrails.

## 🚀 **Quick Start: Knowledge Hooks**

```javascript
import { initStore, defineHook, evaluateHook } from 'unrdf';

// Initialize the knowledge base
const runApp = initStore();

runApp(async () => {
  // Define a service health monitoring hook
  const healthHook = defineHook({
    id: 'ex:ServiceHealth',
    select: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }',
    predicates: [
      { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } },
      { kind: 'DELTA', spec: { change: 'increase', key: ['service'] } }
    ],
    combine: 'AND'
  });

  // Evaluate the hook
  const receipt = await evaluateHook(healthHook, { persist: true });
  
  if (receipt.fired) {
    console.log('🔥 Service health alert!');
    console.log('Evidence:', receipt.predicates);
    console.log('Provenance:', receipt.provenance);
  } else {
    console.log('✅ All services healthy');
  }
});
```

### **CLI Usage**

```bash
# Install globally
pnpm install -g unrdf

# Evaluate a knowledge hook
unrdf hook eval --hook hooks/service-health.json --graph ./data/

# Plan hook evaluation (show predicate tree)
unrdf hook plan --hook ex:ServiceHealth

# View hook receipts
unrdf hook receipts --hook ex:ServiceHealth --tail
```

## 📋 **Knowledge Hooks Examples**

### **Service Health Monitoring**
```javascript
const serviceHealthHook = defineHook({
  id: 'ex:ServiceHealth',
  select: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
  ],
  combine: 'AND'
});
```

### **Compliance Validation**
```javascript
const complianceHook = defineHook({
  id: 'ex:ComplianceCheck',
  select: 'SELECT ?resource WHERE { ?resource ex:sensitive true }',
  predicates: [
    { kind: 'SHACL', spec: { shape: 'ex:SensitiveDataShape', strict: true } }
  ],
  combine: 'AND'
});
```

### **Configuration Drift Detection**
```javascript
const configDriftHook = defineHook({
  id: 'ex:ConfigDrift',
  select: 'SELECT ?config ?value WHERE { ?config ex:currentValue ?value }',
  predicates: [
    { kind: 'DELTA', spec: { change: 'any', key: ['config'] } }
  ],
  combine: 'AND'
});
```

## 🏗️ **Core Architecture**

### **Philosophy**

**No TypeScript. Ever.** TypeScript is an illusion of safety that collapses at runtime. unrdf guarantees correctness at the only level that matters: execution.

**JSDoc is the source of truth.** Documentation, type hints, and developer experience are delivered directly via JSDoc, keeping the codebase minimal and expressive.

**Zod is the contract.** Runtime validation ensures that what you think your data is, and what it actually is, are always in sync.

**Context is everything.** unrdf enforces the "One Store Rule" through a global context system that ensures all composables share the same RDF engine and store instance.

**Composables everywhere.** Every aspect of RDF — graphs, queries, validation, reasoning, serialization — is accessible through consistent composable functions.

### **Context-Based Architecture**

unrdf uses [unctx](https://github.com/unjs/unctx) for global store management, ensuring there's only one store by default:

```javascript
import { initStore, useStore, useGraph, useValidator, useZod } from 'unrdf';

// Initialize the store context at the root of your application
const runApp = initStore([], { baseIRI: 'http://example.org/' });

runApp(() => {
  // All composables now share the same store automatically
  const store = useStore();
  const graph = useGraph();
  const validator = useValidator();
  const zod = useZod();
  
  // Create RDF data
  const subject = store.namedNode('http://example.org/person1');
  const predicate = store.namedNode('http://example.org/name');
  const object = store.literal('John Doe');
  const quad = store.quad(subject, predicate, object);
  
  // Add to the shared store
  store.add(quad);
  
  // Query the data
  const results = await graph.select(`
    PREFIX ex: <http://example.org/>
    SELECT ?s ?p ?o WHERE { ?s ?p ?o }
  `);
  
  console.log('Query results:', results);
  
  // Validate with SHACL
  const report = await validator.validate(shapesStore);
  console.log('Validation report:', report);
  
  // Type-safe validation with Zod
  const PersonSchema = z.object({
    id: z.string().url(),
    name: z.string(),
    age: z.number().int().min(0)
  });
  
  const validation = await zod.validateResults(results, PersonSchema);
  console.log('Zod validation:', validation);
});
```

## 🔧 **Core Composables**

### **Knowledge Hooks**

#### defineHook
Define a Knowledge Hook with predicates and combination logic.

```javascript
const hook = defineHook({
  id: 'ex:MyHook',
  select: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'o', op: '>', value: 100 } },
    { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a ex:Important }' } }
  ],
  combine: 'AND'
});
```

#### evaluateHook
Evaluate a hook and return a receipt with provenance.

```javascript
const receipt = await evaluateHook(hook, { persist: true });
console.log('Fired:', receipt.fired);
console.log('Evidence:', receipt.predicates);
console.log('Provenance:', receipt.provenance);
```

### **Context Management**

#### initStore
Initialize the global store context for your application.

```javascript
import { initStore } from 'unrdf';

// Initialize with empty store
const runApp = initStore();

// Initialize with existing data
const runApp = initStore(quads, { baseIRI: 'http://example.org/' });

// Run your application code
runApp(() => {
  // All composables share the same context here
  const store = useStore();
  const graph = useGraph();
  // ... rest of your code
});
```

#### useStore
Access the shared store instance from context.

```javascript
const store = useStore();

// Add quads
store.add(quad);

// Get statistics
const stats = store.stats();

// Serialize to Turtle
const turtle = await store.serialize();
```

### **RDF Operations**

#### useTerms
RDF term creation and manipulation. Enforces the "One Terms Rule" - N3 DataFactory is the only term creation method.

```javascript
const terms = useTerms();
const subject = terms.iri("http://example.org/person");
const name = terms.lit("John Doe");
const age = terms.lit(30, "http://www.w3.org/2001/XMLSchema#integer");
const bnode = terms.bnode("person1");
const statement = terms.quad(subject, terms.iri("http://example.org/name"), name);
```

#### useGraph
High-level RDF operations including SPARQL queries and set operations.

```javascript
const graph = useGraph();

// SPARQL SELECT queries
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`);

// SPARQL ASK queries
const exists = await graph.ask(`
  PREFIX ex: <http://example.org/>
  ASK WHERE { ?s a ex:Person }
`);

// Get store statistics
const stats = graph.getStats();
```

### **Validation & Canonicalization**

#### useValidator
SHACL validation for RDF graphs.

```javascript
const validator = useValidator();

// Validate against SHACL shapes
const report = await validator.validate(shapesStore);

// Validate and throw on failure
await validator.validateOrThrow(shapesTurtle);

// Summarize validation results
const summary = validator.summarize(report);

// Filter by severity
const violations = validator.filterBySeverity(report, "http://www.w3.org/ns/shacl#Violation");
```

#### useCanon
Canonicalization and isomorphism checking using URDNA2015.

```javascript
const canon = useCanon();

// Canonicalize a store
const canonical = await canon.canonicalize(store);

// Check if two stores are isomorphic
const isIsomorphic = await canon.isIsomorphic(store1, store2);

// Generate canonical hash
const hash = await canon.hash(store);
```

#### useZod
Runtime validation for RDF-derived data.

```javascript
const zod = useZod();

const PersonSchema = z.object({
  name: z.string(),
  age: z.number().int().min(0)
});

const validation = await zod.validateResults(sparqlResults, PersonSchema);
console.log(validation.validated); // [{ name: "John Doe", age: 30 }]
```

### **Advanced Composables**

#### useTypes
Comprehensive RDF term type checking and validation using `@rdfjs/types`.

```javascript
const types = useTypes();

// Type checking
const isNamedNode = types.isNamedNode(term);
const termType = types.getTermType(term);

// Type-safe term creation
const factory = types.createFactory();
const node = factory.namedNode('http://example.org/test');

// Store analysis
const stats = types.getTermStats(store);
```

#### useJSONLD
Full JSON-LD processing capabilities using the `jsonld` library.

```javascript
const jsonld = useJSONLD();

// Expand JSON-LD
const expanded = await jsonld.expand(jsonldData);

// Compact JSON-LD
const compacted = await jsonld.compact(expanded, context);

// Convert to RDF
const rdfStore = await jsonld.toRDF(jsonldData);

// Convert from RDF
const jsonldData = await jsonld.fromRDF(rdfStore);
```

#### useRDFExt
Advanced RDF dataset and graph operations using `rdf-ext`.

```javascript
const rdfExt = useRDFExt();

// Create datasets
const dataset = rdfExt.createDataset();
const graph = rdfExt.createGraph();

// Dataset operations
const union = rdfExt.union(dataset1, dataset2);
const intersection = rdfExt.intersection(dataset1, dataset2);
const difference = rdfExt.difference(dataset1, dataset2);

// Convert between stores and datasets
const dataset = rdfExt.storeToDataset(store);
const store = rdfExt.datasetToStore(dataset);
```

## 🎯 **Opinionated Design**

unrdf enforces a single, opinionated path through the RDF universe:

- **One Store**: N3.Store is the only memory model, managed through context
- **One Engine**: Single RdfEngine instance shared across all composables
- **One Terms**: N3 DataFactory is the only term creation method
- **One Query Engine**: Comunica is the only SPARQL engine
- **One Validator**: SHACL is the only validation method
- **One Canonicalization**: URDNA2015 is the only canonicalization method
- **One Validation**: Zod is the only runtime validation
- **One Context**: Global context system ensures consistency
- **One Hooks**: Knowledge Hooks are the only trigger system

This eliminates choice paralysis and ensures consistency across all RDF operations.

## 🚀 **Why unrdf?**

The RDF ecosystem has matured into a diverse set of libraries, but this diversity has created fragmentation. A typical project may mix N3 for parsing, Comunica for SPARQL, rdf-ext for datasets, rdf-validate-shacl for constraints, and eyereasoner for inference. Each library is useful in isolation, but together they form a patchwork of styles, APIs, and stores.

unrdf addresses this by enforcing a single opinionated path with a context-based architecture. The framework selects a canonical implementation for each layer, wraps them in a composable API pattern, and ensures they all work together through a shared context system. **Knowledge Hooks** add intelligent, reactive capabilities that transform static knowledge graphs into dynamic, responsive systems.

The result is not a new ontology language or reasoner but a reduction of cognitive overhead for practitioners, with the added power of deterministic, auditable knowledge triggers.

## 📦 **Installation**

```bash
pnpm add unrdf
```

## 📄 **License**

MIT

## 🤝 **Contributing**

This project follows the opinionated design philosophy. Contributions should align with the single-path approach, maintain the composable API pattern, respect the context-based architecture, and enhance the Knowledge Hooks system.