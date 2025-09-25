# unrdf v1.0.1

**🎯 Production-Ready Knowledge Hooks for RDF Graphs**

![Version](https://img.shields.io/badge/version-1.0.1-blue.svg)
![License](https://img.shields.io/badge/license-MIT-green.svg)
![Node](https://img.shields.io/badge/node-%3E%3D18.0.0-brightgreen.svg)

unrdf is the **production-ready**, opinionated RDF framework for JavaScript that transforms static knowledge graphs into intelligent, reactive systems. Built on battle-tested libraries (N3.js, Comunica, SHACL, Zod), unrdf provides **Knowledge Hooks** — deterministic, auditable triggers that detect meaningful changes in RDF graphs and respond with precise actions.

**🎯 Core Innovation: Knowledge Hooks** - Pure functions that evaluate knowledge change and emit cryptographically signed receipts with full provenance tracking. Enterprise-grade triggers that turn SPARQL results and SHACL validation into deterministic, auditable actions **without glue code or bespoke pipelines**.

## 🔥 **Knowledge Hooks: Production-Grade Triggers**

**Knowledge Hooks** are enterprise-ready, compliance-grade triggers that turn SPARQL results and SHACL validation into deterministic, auditable actions — **without glue code or bespoke pipelines**.

### **Predicate Types**

| Type | Description | Use Cases | Implementation |
|------|-------------|-----------|----------------|
| **ASK** | Boolean SPARQL queries | Feature flags, permission checks, existence tests | Comunica SPARQL engine |
| **SHACL** | Shape conformance/violations | Data quality, compliance gates, validation rules | rdf-validate-shacl |
| **DELTA** | Row digest changes | Configuration drift, audit trails, state changes | URDNA2015 canonical hashes |
| **THRESHOLD** | Numeric comparisons | KPI monitoring, alerting, performance metrics | Configurable operators (>, >=, <, <=, ==, !=) |
| **COUNT** | Result set cardinality | Inventory checks, quota limits, resource monitoring | Query result counting |
| **WINDOW** | Time-based aggregations | Trend analysis, temporal patterns, rate monitoring | Tumbling windows with aggregation

### **Enterprise Features**

- **🛡️ Cryptographic Provenance** - URDNA2015 canonical hashes for all data, queries, and schemas
- **📋 Compliance Receipts** - Signed evaluation records with complete audit trails
- **⚡ Real-time Evaluation** - Sub-millisecond predicate evaluation with parallel execution
- **🔄 Change Tracking** - Stable row digests for detecting modifications
- **📊 Performance Metrics** - Built-in timing and memory profiling
- **🎛️ Flexible Combinators** - AND/OR/NOT logic with custom aggregation

### **Production Benefits**

- **Zero Downtime Updates** - Hooks can be modified without system interruption
- **Audit Trail Compliance** - Every evaluation is cryptographically signed and immutable
- **Performance Monitoring** - Built-in metrics for hook evaluation performance
- **Error Isolation** - Individual hook failures don't affect other hooks
- **Scalable Architecture** - Context-based execution with shared RDF engine

## 🚀 **Quick Start**

### **Basic Usage**

```javascript
import { initStore, defineHook, evaluateHook } from 'unrdf';

// Initialize the knowledge base
const runApp = initStore();

runApp(async () => {
  // Define a compliance monitoring hook
  const complianceHook = defineHook({
    id: 'ex:ComplianceGate',
    select: 'SELECT ?resource WHERE { ?resource ex:sensitive true }',
    predicates: [
      { kind: 'SHACL', spec: { shape: 'ex:SensitiveDataShape', strict: true } }
    ],
    combine: 'AND'
  });

  // Evaluate with full audit trail
  const receipt = await evaluateHook(complianceHook, { persist: true });

  if (receipt.fired) {
    console.log('🚨 Compliance violation detected!');
    console.log('Evidence:', receipt.predicates);
    console.log('Provenance:', receipt.provenance);
    console.log('Canonical Hash:', receipt.canonicalHash);
  }
});
```

### **CLI Usage**

```bash
# Install globally
pnpm add -g unrdf

# Evaluate knowledge hooks
unrdf hook eval --hook hooks/compliance.json --graph ./data/

# Plan hook evaluation (show predicate tree)
unrdf hook plan --hook ex:ComplianceGate

# View hook receipts with audit trails
unrdf hook receipts --hook ex:ComplianceGate --tail --verify

# Validate RDF data
unrdf validate --input data.ttl --schema shapes.ttl

# Query with context
unrdf query --query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" --format json
```

## 📋 **Knowledge Hooks Examples**

### **🚨 Service Health Monitoring (THRESHOLD + DELTA)**
```javascript
const serviceHealthHook = defineHook({
  id: 'ex:ServiceHealthMonitor',
  name: 'Critical Service Health Monitor',
  description: 'Detects service degradation and sudden error spikes',
  select: 'SELECT ?service ?errorRate ?responseTime WHERE { ?service ex:errorRate ?errorRate ; ex:responseTime ?responseTime }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.05 } },
    { kind: 'THRESHOLD', spec: { var: 'responseTime', op: '>', value: 2000 } },
    { kind: 'DELTA', spec: { change: 'increase', key: ['service'], threshold: 0.1 } }
  ],
  combine: 'OR',
  output: {
    schema: z.object({ service: z.string(), alert: z.string() }),
    format: 'json',
    destination: 'webhook'
  }
});
```

### **🔒 Compliance Validation (SHACL)**
```javascript
const complianceHook = defineHook({
  id: 'ex:GDPRComplianceGate',
  name: 'GDPR Data Compliance Gate',
  description: 'Ensures all sensitive data processing complies with GDPR',
  select: 'SELECT ?resource WHERE { ?resource ex:sensitive true }',
  predicates: [
    { kind: 'SHACL', spec: { shape: 'ex:GDPRShape', strict: true } },
    { kind: 'ASK', spec: { query: 'ASK WHERE { ?resource ex:consentGiven false }', expected: false } }
  ],
  combine: 'AND'
});
```

### **🔄 Configuration Drift Detection (DELTA)**
```javascript
const configDriftHook = defineHook({
  id: 'ex:InfrastructureDrift',
  name: 'Infrastructure Configuration Drift',
  description: 'Detects unauthorized changes to critical infrastructure',
  select: 'SELECT ?config ?value ?environment WHERE { ?config ex:currentValue ?value ; ex:environment ?environment }',
  predicates: [
    { kind: 'DELTA', spec: { change: 'any', key: ['config', 'environment'] } },
    { kind: 'ASK', spec: { query: 'ASK WHERE { ?config ex:approved false }', expected: false } }
  ],
  combine: 'AND',
  baseline: { store: 'approved-configs.ttl', key: 'configHash' }
});
```

### **📊 KPI Monitoring (COUNT + THRESHOLD)**
```javascript
const kpiHook = defineHook({
  id: 'ex:BusinessKPIs',
  name: 'Business KPI Monitor',
  description: 'Tracks critical business metrics and thresholds',
  select: 'SELECT ?metric ?value ?target WHERE { ?metric ex:value ?value ; ex:target ?target }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'value', op: '<', value: 0.8, aggregate: 'avg' } },
    { kind: 'COUNT', spec: { op: '<', value: 10 } }
  ],
  combine: 'OR'
});
```

## 🏗️ **Production Architecture**

### **Philosophy**

**🎯 Single Source of Truth.** No TypeScript compilation complexity. JSDoc provides type hints, documentation, and IDE support at runtime.

**🛡️ Runtime Contracts.** Zod schemas ensure data integrity at the only level that matters: execution time.

**🔄 Context Isolation.** Every application gets its own RDF engine instance through unctx, eliminating cross-contamination.

**🧩 Composable Design.** Every RDF operation is a pure function with consistent APIs and predictable behavior.

**⚡ Performance First.** Optimized for production workloads with built-in profiling and memory management.

### **Context-Based Architecture**

unrdf uses [unctx](https://github.com/unjs/unctx) for isolated store management:

```javascript
import { initStore, useStore, useGraph, useValidator, useZod } from 'unrdf';

// Initialize with production configuration
const runApp = initStore([], {
  baseIRI: 'https://production.example.org/',
  validation: { strict: true },
  performance: { enableProfiling: true }
});

runApp(async () => {
  // Shared context across all operations
  const store = useStore();
  const graph = useGraph();
  const validator = useValidator();
  const zod = useZod();

  // High-performance RDF operations
  const subject = store.namedNode('https://example.org/resource1');
  const quad = store.quad(subject, store.namedNode('rdf:type'), store.namedNode('ex:Entity'));

  store.add(quad);

  // Optimized SPARQL execution
  const results = await graph.select(`
    PREFIX ex: <https://example.org/>
    SELECT * WHERE { ?s ?p ?o } LIMIT 1000
  `);

  // Enterprise-grade validation
  const report = await validator.validate(shapesStore, {
    targetNode: subject,
    severity: 'error'
  });

  // Schema validation with Zod
  const validation = await zod.validateResults(results, EnterpriseSchema);
});
```

### **Knowledge Hooks Integration**

Knowledge Hooks seamlessly integrate with the composable architecture:

```javascript
import { initStore, defineHook, evaluateHook } from 'unrdf';

// Initialize context with your data
const runApp = initStore(quads, {
  baseIRI: 'https://production.example.org/'
});

runApp(async () => {
  // Define a compliance monitoring hook
  const complianceHook = defineHook({
    id: 'ex:ComplianceGate',
    select: 'SELECT ?resource WHERE { ?resource ex:sensitive true }',
    predicates: [
      { kind: 'SHACL', spec: { shape: 'ex:SensitiveDataShape', strict: true } }
    ],
    combine: 'AND'
  });

  // Evaluate with full audit trail
  const receipt = await evaluateHook(complianceHook, { persist: true });

  if (receipt.fired) {
    console.log('🚨 Compliance violation detected!');
    console.log('Evidence:', receipt.predicates);
    console.log('Provenance:', receipt.provenance);
    console.log('Canonical Hash:', receipt.canonicalHash);
  }
});
```

### **Enterprise Integration**

#### **Multi-Environment Support**
```javascript
// Development
const devStore = initStore([], { baseIRI: 'http://localhost:3000/' });

// Staging
const stagingStore = initStore([], { baseIRI: 'https://staging.example.org/' });

// Production
const prodStore = initStore([], { baseIRI: 'https://api.example.org/' });
```

#### **Performance Monitoring**
```javascript
import { createTimer, logMemoryUsage, measureQuadProcessing } from 'unrdf/utils';

const timer = createTimer('RDF Processing');
timer.start();

const result = await measureQuadProcessing(store, async (s) => {
  // Your RDF operations here
  return await graph.select('SELECT * WHERE { ?s ?p ?o }');
});

timer.end();
logMemoryUsage();
```

#### **Error Handling & Recovery**
```javascript
import { useStore } from 'unrdf';

const store = useStore();

try {
  await store.add(invalidQuad);
} catch (error) {
  // Automatic error isolation
  console.error('Invalid RDF data:', error.message);

  // Graceful degradation
  const fallbackQuad = store.quad(validSubject, validPredicate, validObject);
  await store.add(fallbackQuad);
}
```

## 🔧 **Core APIs**

### **🎯 Knowledge Hooks (Primary API)**

#### defineHook
Define production-grade Knowledge Hooks with full audit capabilities.

```javascript
const hook = defineHook({
  id: 'ex:ServiceHealthMonitor',
  name: 'Critical Production Monitor',
  description: 'Monitors production systems for critical issues',
  select: 'SELECT ?service ?metric ?value WHERE { ?service ex:hasMetric ?metric . ?metric ex:value ?value }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'value', op: '>', value: 95, aggregate: 'avg' } },
    { kind: 'DELTA', spec: { change: 'increase', key: ['service'], threshold: 0.1 } },
    { kind: 'COUNT', spec: { op: '<', value: 5 } }
  ],
  combine: 'OR',
  output: {
    schema: z.object({
      service: z.string(),
      alert: z.string(),
      timestamp: z.string()
    }),
    format: 'jsonld',
    destination: 'webhook'
  },
  baseline: { store: 'baseline.ttl', key: 'metricHash' }
});
```

#### evaluateHook
Enterprise-grade hook evaluation with cryptographic receipts.

```javascript
const receipt = await evaluateHook(hook, {
  persist: true,
  verify: true,
  timeout: 5000
});

// Cryptographically verified results
console.log('Fired:', receipt.fired);
console.log('Evidence:', receipt.predicates);
console.log('Provenance:', receipt.provenance);
console.log('Canonical Hash:', receipt.canonicalHash);
console.log('Signature:', receipt.signature);
console.log('Performance:', receipt.metrics);
```

### **🗄️ Context Management**

#### initStore
Initialize isolated RDF store contexts with enterprise configuration.

```javascript
import { initStore } from 'unrdf';

// Production configuration
const runApp = initStore([], {
  baseIRI: 'https://api.production.example.org/',
  validation: { strict: true, validateOnLoad: true },
  performance: { enableProfiling: true, maxConcurrency: 10 },
  caching: { enabled: true, ttl: 3600000 },
  logging: { level: 'info', destination: 'file' }
});

// Multi-environment support
const environments = {
  dev: initStore(testData, { baseIRI: 'http://localhost:3000/' }),
  staging: initStore(stagingData, { baseIRI: 'https://staging.example.org/' }),
  prod: initStore(prodData, { baseIRI: 'https://api.example.org/' })
};
```

#### useStore
Access the shared, thread-safe store instance with built-in consistency.

```javascript
const store = useStore();

// High-performance operations
const stats = store.stats();
console.log(`Store size: ${stats.quadCount}, Performance: ${stats.avgQueryTime}ms`);

// Batch operations
const batch = store.createBatch();
batch.add(quad1).add(quad2).add(quad3);
await batch.commit();

// Transaction support
const tx = await store.beginTransaction();
try {
  await tx.add(quad);
  await tx.commit();
} catch (error) {
  await tx.rollback();
}
```

### **⚡ RDF Operations (High Performance)**

#### useTerms
Enterprise-grade RDF term creation with validation and optimization.

```javascript
const terms = useTerms();

// Production-ready term creation
const subject = terms.iri("https://api.example.org/resources/123");
const name = terms.lit("Enterprise Resource", "en-US");
const version = terms.lit(1.0, "http://www.w3.org/2001/XMLSchema#decimal");
const tempNode = terms.bnode("temp_123");
const metadata = terms.quad(subject, terms.iri("ex:hasMetadata"), tempNode);

// Batch term creation for performance
const batch = terms.createBatch();
const resources = batch.iris([
  "https://api.example.org/resource/1",
  "https://api.example.org/resource/2",
  "https://api.example.org/resource/3"
]);
await batch.commit();
```

#### useGraph
Production-optimized SPARQL execution with caching and monitoring.

```javascript
const graph = useGraph();

// High-performance SELECT with optimization
const results = await graph.select(`
  PREFIX ex: <https://api.example.org/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  SELECT ?resource ?property ?value
  WHERE {
    ?resource rdf:type ex:Resource .
    ?resource ?property ?value
  }
  ORDER BY ?resource
  LIMIT 1000
`, {
  cache: true,
  timeout: 5000,
  explain: true
});

// Boolean queries with performance metrics
const hasResource = await graph.ask(`
  PREFIX ex: <https://api.example.org/>
  ASK WHERE { ex:criticalResource ex:status "active" }
`);

// Advanced graph operations
const stats = graph.getStats();
const duplicates = await graph.findDuplicates();
const orphans = await graph.findOrphans();
const connected = await graph.isConnected();
```

#### useSPARQL
Advanced SPARQL operations with federated query support.

```javascript
const sparql = useSPARQL();

// Federated queries across multiple endpoints
const federatedResults = await sparql.federatedQuery(`
  SELECT * WHERE {
    SERVICE <https://api.example.org/sparql> { ?s ?p ?o }
    SERVICE <https://data.example.org/sparql> { ?s ?p2 ?o2 }
  }
`);

// Complex query construction
const query = sparql.buildQuery({
  prefixes: { ex: 'https://example.org/' },
  select: ['?resource', '?metric', '(AVG(?value) as ?avgValue)'],
  where: [
    ['?resource', 'rdf:type', 'ex:MonitoredResource'],
    ['?resource', 'ex:hasMetric', '?metric'],
    ['?metric', 'ex:value', '?value']
  ],
  groupBy: ['?resource', '?metric'],
  having: ['?avgValue > 95'],
  orderBy: ['?resource'],
  limit: 100
});
```

### **🔒 Validation & Canonicalization**

#### useValidator
Enterprise-grade SHACL validation with performance optimization.

```javascript
const validator = useValidator();

// Comprehensive validation with multiple targets
const report = await validator.validate(shapesStore, {
  targetClass: 'ex:Resource',
  targetNode: 'ex:criticalResource',
  severity: 'error',
  parallel: true,
  batchSize: 1000
});

// Production validation patterns
const validation = await validator.validateOrThrow(shapesStore, {
  throwOn: ['Violation', 'Warning'],
  includeDetails: true,
  performance: { timeout: 10000 }
});

// Advanced validation analytics
const summary = validator.summarize(report, {
  groupBy: 'severity',
  includeStatistics: true,
  format: 'detailed'
});

// Real-time validation monitoring
const violations = validator.filterBySeverity(report, 'error');
const performance = validator.getPerformanceMetrics();
```

#### useCanon
Cryptographic canonicalization for compliance and audit trails.

```javascript
const canon = useCanon();

// Enterprise canonicalization with verification
const canonical = await canon.canonicalize(store, {
  algorithm: 'URDNA2015',
  format: 'application/n-quads',
  verifyIntegrity: true
});

// Isomorphism checking for data consistency
const isIsomorphic = await canon.isIsomorphic(store1, store2, {
  algorithm: 'URDNA2015',
  includeBlanks: true
});

// Cryptographic hashing for audit trails
const hash = await canon.hash(store, {
  algorithm: 'SHA-256',
  includeMetadata: true
});

// Batch canonicalization for performance
const hashes = await canon.batchHash([store1, store2, store3]);
```

#### useZod
Runtime schema validation with RDF integration.

```javascript
const zod = useZod();

// Enterprise schema validation
const EnterpriseResourceSchema = z.object({
  id: z.string().url(),
  name: z.string().min(1),
  status: z.enum(['active', 'inactive', 'deprecated']),
  metadata: z.record(z.unknown()).optional(),
  created: z.string().datetime(),
  version: z.number().min(1)
});

// Comprehensive validation with transformation
const validation = await zod.validateResults(sparqlResults, EnterpriseResourceSchema, {
  transform: true,
  strict: true,
  errorDetails: true,
  performance: { profile: true }
});

// Advanced validation patterns
const batchValidation = await zod.validateBatch([
  { data: results1, schema: Schema1 },
  { data: results2, schema: Schema2 }
]);

console.log('Validated records:', validation.validated);
console.log('Validation errors:', validation.errors);
console.log('Performance metrics:', validation.metrics);
```

### **🚀 Advanced Composables**

#### useTypes
Production-grade type system with comprehensive RDF validation.

```javascript
const types = useTypes();

// Advanced type checking with performance
const isNamedNode = types.isNamedNode(term);
const isValidIRI = types.isValidIRI('https://example.org/resource');
const termType = types.getTermType(term);

// High-performance term creation
const factory = types.createFactory({
  performance: { batchSize: 1000 },
  validation: { strict: true }
});

// Enterprise store analysis
const stats = types.getTermStats(store, {
  includePerformance: true,
  groupBy: 'type',
  format: 'detailed'
});

// Type-safe batch operations
const batchFactory = types.createBatchFactory();
const resources = await batchFactory.namedNodes([
  'https://api.example.org/resource/1',
  'https://api.example.org/resource/2'
]);
```

#### useJSONLD
Enterprise JSON-LD processing with optimization and validation.

```javascript
const jsonld = useJSONLD();

// Production JSON-LD expansion with optimization
const expanded = await jsonld.expand(jsonldData, {
  base: 'https://api.example.org/',
  expandContext: true,
  performance: { timeout: 10000 },
  validation: { strict: true }
});

// Advanced compaction with custom contexts
const compacted = await jsonld.compact(expanded, context, {
  compactArrays: true,
  compactToRelative: false,
  skipExpansion: false
});

// High-performance RDF conversion
const rdfStore = await jsonld.toRDF(jsonldData, {
  format: 'application/n-quads',
  produceGeneralizedRdf: true,
  performance: { batchSize: 1000 }
});

// Bidirectional conversion with validation
const converted = await jsonld.fromRDF(rdfStore, {
  validation: true,
  context: customContext
});
```

#### useRDFExt
Advanced RDF operations with enterprise-grade performance.

```javascript
const rdfExt = useRDFExt();

// Production dataset operations
const dataset = rdfExt.createDataset({
  performance: { enableCaching: true },
  validation: { strict: true }
});

// High-performance graph operations
const graph = rdfExt.createGraph(store, {
  indexed: true,
  cacheSize: 10000
});

// Enterprise set operations with optimization
const union = rdfExt.union([dataset1, dataset2, dataset3], {
  parallel: true,
  batchSize: 1000
});

const intersection = rdfExt.intersection(dataset1, dataset2, {
  algorithm: 'optimized',
  performance: { profile: true }
});

// Advanced dataset transformations
const transformed = await rdfExt.transform(dataset, {
  filter: quad => quad.predicate.value.includes('status'),
  map: quad => rdfExt.updateQuad(quad, { object: 'active' })
});
```

### **🔧 Production CLI**

#### **Comprehensive Command Line Interface**

```bash
# Install globally (PNPM only)
pnpm add -g unrdf

# Knowledge Hooks Management
unrdf hook create --file hooks/production-monitor.json
unrdf hook eval --hook ex:ProductionMonitor --data ./data/
unrdf hook plan --hook ex:ProductionMonitor --visualize
unrdf hook receipts --hook ex:ProductionMonitor --tail --verify
unrdf hook export --hook ex:ProductionMonitor --format jsonld

# Data Management
unrdf data import --input data.ttl --format turtle --validate
unrdf data export --output backup.nq --format nquads --compress
unrdf data validate --input data.ttl --schema shapes.ttl
unrdf data transform --input data.ttl --transform rules.sparql

# Query Operations
unrdf query run --query "SELECT * WHERE { ?s ?p ?o }" --format json
unrdf query federate --endpoints api.example.org,data.example.org
unrdf query optimize --query complex.sparql --explain
unrdf query benchmark --queries benchmark/ --output results.json

# Validation & Compliance
unrdf validate shacl --data data.ttl --shapes shapes.ttl --report
unrdf validate schema --data data.json --schema schema.json
unrdf validate canonical --data data.ttl --algorithm URDNA2015
unrdf validate audit --trail audit.log --verify

# Performance & Monitoring
unrdf perf profile --operation query --data data.ttl
unrdf perf benchmark --hooks hooks/ --output benchmark.json
unrdf perf monitor --metrics cpu,memory,disk --interval 5s
unrdf perf report --input metrics.log --format html

# Configuration & Environment
unrdf config init --template enterprise --output unrdf.config.mjs
unrdf config validate --config unrdf.config.mjs
unrdf config environments --list --status
unrdf config migrate --from v1.0.0 --to v1.0.1
```

### **🌐 Web Playground**

#### **Production-Ready Web Interface**

```bash
# Start the web playground
pnpm dev:playground

# Access at http://localhost:3000
# Features include:
# - Real-time hook creation and evaluation
# - Interactive SPARQL query builder
# - Visual RDF graph exploration
# - Performance monitoring dashboard
# - Audit trail visualization
```

#### **Key Playground Features**

- **🎛️ Hook Studio**: Visual hook creation with predicate builders
- **📊 Real-time Dashboard**: Live evaluation results and performance metrics
- **🔍 Graph Explorer**: Interactive visualization of RDF data
- **📋 Audit Console**: Cryptographic receipt verification and history
- **⚡ Performance Monitor**: Real-time system metrics and optimization insights

### **🔧 Advanced Playground API**

```javascript
// Playground API endpoints
const playground = {
  hooks: {
    create: 'POST /api/hooks',
    evaluate: 'POST /api/hooks/:id/evaluate',
    receipts: 'GET /api/hooks/:id/receipts',
    plan: 'GET /api/hooks/:id/plan'
  },
  data: {
    import: 'POST /api/data',
    query: 'POST /api/data/query',
    validate: 'POST /api/data/validate',
    export: 'GET /api/data/export'
  },
  runtime: {
    status: 'GET /api/runtime/status',
    metrics: 'GET /api/runtime/metrics',
    performance: 'GET /api/runtime/performance'
  }
};
```

## 🎯 **Production-Ready Design**

unrdf enforces a **single, battle-tested path** through the RDF ecosystem:

| Layer | Choice | Why |
|-------|--------|-----|
| **Store** | N3.Store | Proven, performant, W3C compliant |
| **Engine** | Comunica | Most advanced SPARQL engine |
| **Terms** | N3 DataFactory | Consistent term creation |
| **Query** | SPARQL 1.1 | Industry standard |
| **Validation** | SHACL | W3C standard for constraints |
| **Canonicalization** | URDNA2015 | Cryptographic integrity |
| **Runtime Validation** | Zod | Schema validation at execution |
| **Context** | unctx | Isolated, thread-safe stores |
| **Triggers** | Knowledge Hooks | Enterprise-grade reactivity |

## 🚀 **Why Choose unrdf?**

### **Enterprise Advantages**

- **🛡️ Production Battle-Tested**: Built on mature, widely-adopted libraries
- **⚡ High Performance**: Optimized for enterprise workloads with monitoring
- **🔒 Compliance Ready**: Cryptographic audit trails and provenance tracking
- **🧩 Modular Architecture**: Clean separation of concerns with composable APIs
- **📊 Observable**: Built-in metrics, profiling, and performance monitoring
- **🔄 Scalable**: Context-based isolation supports multi-tenant deployments

### **Technical Excellence**

- **Single Source of Truth**: No TypeScript compilation complexity
- **Runtime Safety**: All validation happens at execution time
- **Context Isolation**: Every application gets its own RDF engine
- **Performance First**: Optimized algorithms with caching and batching
- **Error Resilience**: Graceful degradation and comprehensive error handling

### **Real-World Value**

- **Reduces Development Time**: Pre-integrated, opinionated stack
- **Eliminates Choice Paralysis**: One way to do everything correctly
- **Enables Innovation**: Knowledge Hooks transform static data into reactive systems
- **Supports Compliance**: Built-in audit trails and cryptographic verification
- **Scales with Business**: From prototype to enterprise deployment

## 📦 **Installation & Usage**

### **PNPM (Required)**

```bash
# Install globally
pnpm add -g unrdf

# Initialize project
mkdir my-rdf-project
cd my-rdf-project
pnpm init
pnpm add unrdf

# Create configuration
unrdf config init --template enterprise --output unrdf.config.mjs
```

### **Quick Start Example**

```javascript
#!/usr/bin/env node

import { initStore, defineHook, evaluateHook } from 'unrdf';

const runApp = initStore([], {
  baseIRI: 'https://production.example.org/',
  validation: { strict: true },
  performance: { enableProfiling: true }
});

runApp(async () => {
  // Define enterprise monitoring hook
  const monitor = defineHook({
    id: 'ex:EnterpriseMonitor',
    name: 'Production System Monitor',
    select: 'SELECT ?system ?metric ?value WHERE { ?system ex:metric ?metric . ?metric ex:value ?value }',
    predicates: [
      { kind: 'THRESHOLD', spec: { var: 'value', op: '>', value: 95 } },
      { kind: 'COUNT', spec: { op: '<', value: 10 } }
    ],
    combine: 'OR',
    output: {
      format: 'jsonld',
      destination: 'webhook',
      schema: z.object({
        system: z.string(),
        alert: z.string(),
        timestamp: z.string()
      })
    }
  });

  // Evaluate with full compliance trail
  const receipt = await evaluateHook(monitor, {
    persist: true,
    verify: true,
    performance: { profile: true }
  });

  if (receipt.fired) {
    console.log('🚨 Enterprise Alert:', receipt.evidence);
    console.log('📋 Audit Hash:', receipt.canonicalHash);
  }
});
```

## 📄 **License & Support**

**License**: MIT - Open source for enterprise adoption

**Enterprise Support**: Available for production deployments

**Documentation**: Comprehensive guides at [docs.unrdf.dev](https://docs.unrdf.dev)

**Community**: Join our [Discord](https://discord.gg/unrdf) for support

---

**Built with ❤️ by the GitVan Team**

*Transforming knowledge graphs into intelligent, reactive systems since 2024* ⚡