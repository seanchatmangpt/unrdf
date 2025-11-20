# 🔧 UNRDF API Reference Summary

## Overview

This document summarizes the complete UNRDF API surface area, covering all modules, classes, functions, and configuration options documented in the mdBook.

---

## 📦 Core Modules

### 1. Knowledge Engine (`unrdf`)

**Primary Export:**
```typescript
import { createKnowledgeEngine } from 'unrdf';
```

**Engine Creation:**
```typescript
const engine = await createKnowledgeEngine(config?: KnowledgeEngineConfig);
```

**Configuration Options:**

#### Store Configuration
```typescript
interface StoreConfig {
  type: 'memory' | 'persistent' | 'distributed';
  backend: 'n3' | 'oxigraph' | 'postgres';
  connectionString?: string;
  poolSize?: number;
  idleTimeout?: number;
}
```

#### Transaction Settings
```typescript
interface TransactionConfig {
  isolation: 'read-uncommitted' | 'read-committed' | 'repeatable-read' | 'serializable';
  timeout: number;  // milliseconds
  retries: number;
  autoCommit: boolean;
}
```

#### Query Optimization
```typescript
interface QueryConfig {
  enableCache: boolean;
  cacheSize: string;      // e.g., '1GB'
  cacheTTL: number;       // seconds
  optimizer: 'aggressive' | 'conservative' | 'disabled';
  parallelism: number;    // concurrent queries
}
```

#### Knowledge Hooks
```typescript
interface HooksConfig {
  enabled: boolean;
  sandbox: 'isolated-vm' | 'vm2' | 'worker-threads';
  timeout: number;        // milliseconds
  memoryLimit: string;    // e.g., '128MB'
}
```

#### Policy Packs
```typescript
interface PoliciesConfig {
  enabled: boolean;
  strictMode: boolean;    // fail on violations
  packs: string[];        // ['compliance-v1', 'security-v2']
}
```

#### Browser Features
```typescript
interface BrowserConfig {
  enableIndexedDB: boolean;
  dbName: string;
  syncInterval: number;   // milliseconds
}
```

#### Streaming
```typescript
interface StreamingConfig {
  enabled: boolean;
  batchSize: number;
  flushInterval: number;  // milliseconds
  enableWindowing: boolean;
}
```

#### Federation
```typescript
interface FederationConfig {
  enabled: boolean;
  topology: 'mesh' | 'hierarchical' | 'ring' | 'star';
  consensus: 'byzantine-ftb' | 'raft' | 'gossip';
  nodes: FederationNode[];
}
```

#### Observability
```typescript
interface ObservabilityConfig {
  enabled: boolean;
  serviceName: string;
  exporters: ('console' | 'jaeger' | 'prometheus')[];
  sampleRate: number;     // 0.0 - 1.0
  includeQueries: boolean;
  includeResults: boolean;
}
```

---

## 🔨 Knowledge Engine Methods

### Data Insertion

**Insert quads:**
```typescript
await engine.insert(
  quads: Quad[],
  options?: {
    graph?: NamedNode;
    validate?: boolean;    // run Policy Packs
    notify?: boolean;      // trigger Knowledge Hooks
    transaction?: Transaction;
  }
): Promise<void>
```

### Data Deletion

**Delete specific quads:**
```typescript
await engine.delete(quads: Quad[]): Promise<void>
```

**Delete by pattern:**
```typescript
await engine.deleteMatching(pattern: {
  subject?: Term;
  predicate?: Term;
  object?: Term;
  graph?: Term;
}): Promise<void>
```

**Delete entire graph:**
```typescript
await engine.deleteGraph(graph: NamedNode): Promise<void>
```

**Delete all quads:**
```typescript
await engine.deleteAll(): Promise<void>
```

### Data Updates

**Atomic update:**
```typescript
await engine.update({
  delete: Quad[];
  insert: Quad[];
}): Promise<void>
```

### Querying

**Basic SPARQL:**
```typescript
const results = await engine.query(
  sparql: string,
  options?: {
    defaultGraph?: NamedNode;
    graphs?: NamedNode[];
    unionDefaultGraph?: boolean;
  }
): Promise<Bindings[]>
```

**Type-safe query:**
```typescript
import { z } from 'zod';

const schema = z.object({
  name: z.string(),
  age: z.number(),
  email: z.string().email().optional()
});

const results = await engine.queryTyped<typeof schema>({
  query: string;
  schema: z.ZodSchema;
}): Promise<z.infer<typeof schema>[]>
```

**Parameterized query:**
```typescript
const results = await engine.query({
  query: string;
  bindings: Record<string, number | string | boolean>;
}): Promise<Bindings[]>
```

**Semantic search:**
```typescript
const results = await engine.semanticSearch({
  query: string;
  limit?: number;
  minSimilarity?: number;  // 0.0 - 1.0
  fields?: string[];       // predicate IRIs
}): Promise<SearchResult[]>
```

### Transactions

**Begin transaction:**
```typescript
const tx = await engine.beginTransaction(
  options?: {
    isolation?: 'serializable' | 'repeatable-read' | 'read-committed';
    readOnly?: boolean;
  }
): Promise<Transaction>
```

**Commit transaction:**
```typescript
await tx.commit(): Promise<void>
```

**Rollback transaction:**
```typescript
await tx.rollback(): Promise<void>
```

**Automatic transaction:**
```typescript
await engine.transaction(
  async (tx: Transaction) => {
    // operations here
  },
  options?: {
    readOnly?: boolean;
    isolation?: IsolationLevel;
  }
): Promise<T>
```

### Performance

**Query with metrics:**
```typescript
const { results, metrics } = await engine.queryWithMetrics(sparql: string);

// metrics: {
//   duration: number;        // milliseconds
//   quadsScanned: number;
//   cacheHit: boolean;
//   planningTime: number;
//   executionTime: number;
// }
```

**Explain plan:**
```typescript
const plan = await engine.explainQuery(sparql: string): Promise<QueryPlan>
```

**Batch operations:**
```typescript
await engine.batch([
  { type: 'insert', quads: Quad[] },
  { type: 'delete', quads: Quad[] },
  { type: 'update', delete: Quad[], insert: Quad[] }
]): Promise<void>
```

### Serialization

**Serialize to format:**
```typescript
const data = await engine.serialize(
  format: 'turtle' | 'jsonld' | 'ntriples' | 'nquads' | 'rdfxml'
): Promise<string>
```

**Deserialize from format:**
```typescript
await engine.deserialize(
  data: string,
  format: 'turtle' | 'jsonld' | 'ntriples' | 'nquads' | 'rdfxml'
): Promise<void>
```

### Reasoning

**Enable OWL reasoning:**
```typescript
const engine = await createKnowledgeEngine({
  reasoning: {
    enabled: true;
    profile: 'RDFS' | 'OWL-DL' | 'OWL-FULL';
    materialize: boolean;  // pre-compute inferences
  }
});
```

---

## 🪝 Knowledge Hooks API

### Define Hook

**Import:**
```typescript
import { defineKnowledgeHook } from 'unrdf/hooks';
```

**Pre-transaction hook:**
```typescript
defineKnowledgeHook({
  id: string;
  type: 'pre-transaction';
  predicate: (delta: Delta) => boolean;  // trigger condition
  effect: (delta: Delta, context: HookContext) => void | Promise<void>;
  priority?: number;  // execution order
})
```

**Post-transaction hook:**
```typescript
defineKnowledgeHook({
  id: string;
  type: 'post-transaction';
  predicate: (delta: Delta) => boolean;
  effect: (delta: Delta, context: HookContext) => void | Promise<void>;
})
```

### Hook Context

**Available in effects:**
```typescript
interface HookContext {
  engine: KnowledgeEngine;
  transaction: Transaction;
  timestamp: Date;
  userId?: string;
  metadata?: Record<string, any>;

  // Helper methods
  insert(quads: Quad[]): Promise<void>;
  delete(quads: Quad[]): Promise<void>;
  query(sparql: string): Promise<Bindings[]>;
  emit(event: string, data: any): void;
}
```

### Delta Object

**Change information:**
```typescript
interface Delta {
  added: Quad[];      // inserted quads
  removed: Quad[];    // deleted quads
  graph?: NamedNode;  // affected graph
  timestamp: Date;
}
```

### Built-in Predicates

**Predicate factories:**
```typescript
import {
  matchPattern,      // match quad patterns
  matchSHACL,        // validate with SHACL shapes
  askQuery,          // ASK query predicate
  thresholdCount,    // trigger after N quads
  customPredicate    // custom logic
} from 'unrdf/hooks/predicates';

// Example: Match pattern
matchPattern({
  subject?: Term;
  predicate?: Term;
  object?: Term;
  graph?: Term;
})

// Example: SHACL validation
matchSHACL(shapesGraph: Quad[])

// Example: ASK query
askQuery(sparql: string)

// Example: Threshold
thresholdCount(count: number)
```

---

## 🛡️ Policy Packs API

### Create Policy Pack

**Import:**
```typescript
import { createPolicyPack } from 'unrdf/policy';
```

**Define policy pack:**
```typescript
const policyPack = createPolicyPack({
  id: string;
  version: string;
  name: string;
  description?: string;
  shapes: Quad[];     // SHACL shapes
  rules: PolicyRule[];
  metadata?: Record<string, any>;
})
```

### Policy Rule

**Rule structure:**
```typescript
interface PolicyRule {
  id: string;
  severity: 'error' | 'warning' | 'info';
  message: string;
  condition: (delta: Delta) => boolean;
  enforcement?: 'block' | 'warn' | 'log';
}
```

### Apply Policy Pack

**To engine:**
```typescript
await engine.setPolicyPack(policyPack: PolicyPack): Promise<void>
```

**Validate quads:**
```typescript
const report = await engine.validate(quads: Quad[]): Promise<ValidationReport>

interface ValidationReport {
  conforms: boolean;
  violations: Violation[];
}

interface Violation {
  rule: string;
  severity: 'error' | 'warning' | 'info';
  message: string;
  focusNode?: Term;
  path?: Term;
}
```

---

## 🌐 Browser Integration API

### React Hooks

**Import:**
```typescript
import { useKnowledgeHook } from 'unrdf/react';
```

**Use hook:**
```typescript
const { data, loading, error, refetch } = useKnowledgeHook<T>({
  hookId: string;
  filter?: Record<string, any>;
  fallback?: T;
  onUpdate?: (data: T) => void;
})
```

**Example:**
```typescript
'use client';

const { data, loading } = useKnowledgeHook({
  hookId: 'product-price-updates',
  filter: { productId },
  fallback: initialPrice
});
```

### IndexedDB

**Enable browser storage:**
```typescript
const engine = await createKnowledgeEngine({
  browser: {
    enableIndexedDB: true,
    dbName: 'my-knowledge-graph',
    syncInterval: 5000  // sync every 5 seconds
  }
});
```

**Manual sync:**
```typescript
await engine.sync(): Promise<void>
```

---

## 📡 Streaming API

### Subscribe to Changes

**Change feed:**
```typescript
const subscription = engine.subscribe({
  graph?: NamedNode;
  pattern?: QuadPattern;
  callback: (delta: Delta) => void;
})

// Unsubscribe
subscription.unsubscribe()
```

**Stream processor:**
```typescript
import { createStreamProcessor } from 'unrdf/streaming';

const processor = createStreamProcessor({
  batchSize: 100,
  flushInterval: 1000,
  windowSize?: number;
  windowType?: 'tumbling' | 'sliding' | 'session';

  onBatch: (quads: Quad[]) => void | Promise<void>;
})

processor.push(quad: Quad);
await processor.flush();
processor.close();
```

---

## 🌍 Federation API

### Initialize Federation

**Create federated engine:**
```typescript
const engine = await createKnowledgeEngine({
  federation: {
    enabled: true,
    topology: 'mesh',
    consensus: 'byzantine-ftb',
    nodes: [
      { id: 'node1', url: 'https://node1.example.com' },
      { id: 'node2', url: 'https://node2.example.com' }
    ]
  }
});
```

### Federated Operations

**Federated query:**
```typescript
const results = await engine.federatedQuery({
  query: string;
  nodes?: string[];  // specific nodes (optional)
  strategy: 'all' | 'any' | 'quorum';
}): Promise<Bindings[]>
```

**Sync nodes:**
```typescript
await engine.syncNodes(nodeIds?: string[]): Promise<void>
```

---

## 📊 Observability API

### OpenTelemetry Integration

**Configure telemetry:**
```typescript
const engine = await createKnowledgeEngine({
  observability: {
    serviceName: 'knowledge-api',
    exporters: ['jaeger', 'prometheus'],
    sampleRate: 0.1,  // 10% sampling
    attributes: {
      'deployment.environment': 'production',
      'service.version': '4.0.0'
    }
  }
});
```

### Custom Instrumentation

**Create span:**
```typescript
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('my-service');
const span = tracer.startSpan('my-operation');

try {
  // operation
  span.setStatus({ code: SpanStatusCode.OK });
} catch (error) {
  span.recordException(error);
  span.setStatus({ code: SpanStatusCode.ERROR });
} finally {
  span.end();
}
```

**Record metrics:**
```typescript
import { metrics } from '@opentelemetry/api';

const meter = metrics.getMeter('my-service');
const counter = meter.createCounter('query_count');
const histogram = meter.createHistogram('query_duration');

counter.add(1, { operation: 'query' });
histogram.record(42, { operation: 'query' });
```

---

## 🧠 AI & Semantic API

### Vector Embeddings

**Generate embeddings:**
```typescript
import { generateEmbedding } from 'unrdf/ai';

const embedding = await generateEmbedding(
  text: string,
  model?: 'openai' | 'cohere' | 'huggingface'
): Promise<number[]>
```

**Store embeddings:**
```typescript
await engine.storeEmbedding({
  subject: NamedNode;
  embedding: number[];
  metadata?: Record<string, any>;
}): Promise<void>
```

### Semantic Search

**Vector similarity search:**
```typescript
const results = await engine.semanticSearch({
  query: string | number[];  // text or embedding
  limit: number;
  minSimilarity: number;     // 0.0 - 1.0
  fields: string[];          // predicates to search
  filters?: Record<string, any>;
}): Promise<SearchResult[]>

interface SearchResult {
  subject: NamedNode;
  similarity: number;
  metadata: Record<string, any>;
}
```

---

## 📚 Type Definitions

### Core RDF Types

**From @rdfjs/types:**
```typescript
import type {
  Quad,
  NamedNode,
  BlankNode,
  Literal,
  Variable,
  Term,
  DefaultGraph
} from '@rdfjs/types';
```

**Factory functions:**
```typescript
import { namedNode, literal, blankNode, quad } from '@rdfjs/data-model';

const subject = namedNode('http://example.org/alice');
const predicate = namedNode('http://xmlns.com/foaf/0.1/name');
const object = literal('Alice');
const graph = namedNode('http://example.org/graph1');

const q = quad(subject, predicate, object, graph);
```

### UNRDF Types

**Exported types:**
```typescript
export type {
  KnowledgeEngine,
  KnowledgeEngineConfig,
  Transaction,
  QueryBindings,
  Delta,
  HookContext,
  PolicyPack,
  ValidationReport,
  StreamProcessor,
  FederationNode,
  SearchResult
} from 'unrdf';
```

---

## 🎯 Complete API Surface Area

**Total Exports:**
- **Core Module**: 1 primary export (`createKnowledgeEngine`)
- **Knowledge Engine**: 25+ methods
- **Knowledge Hooks**: 8+ methods and 5 predicate factories
- **Policy Packs**: 4 methods
- **Browser Integration**: 3 React hooks, 2 IndexedDB methods
- **Streaming**: 4 methods, 1 processor class
- **Federation**: 3 methods
- **Observability**: OpenTelemetry integration (automatic)
- **AI & Semantic**: 3 methods

**Configuration Options:**
- **8 major config sections** (Store, Transactions, Query, Hooks, Policies, Browser, Streaming, Federation, Observability)
- **50+ individual settings**

**Type Definitions:**
- **20+ exported interfaces**
- **15+ exported types**
- Full TypeScript + JSDoc support

---

## 📖 Documentation Coverage

**API Documentation Status:**
- ✅ All public methods documented
- ✅ All configuration options documented
- ✅ 100+ code examples
- ✅ Type signatures for all exports
- ✅ Common patterns and best practices
- ✅ Error handling strategies
- ✅ Performance considerations

**Reference Locations in mdBook:**
- **Full 360°/Knowledge Engine** (547 lines) - Complete Knowledge Engine API
- **Full 360°/Knowledge Hooks** - Complete Hooks API
- **Full 360°/Policy Packs** - Complete Policy API
- **Full 360°/Browser Integration** - React Hooks and IndexedDB
- **Full 360°/Streaming** - Streaming and real-time API
- **Full 360°/Federation** - Distributed features API
- **Full 360°/Observability** - OpenTelemetry integration
- **Full 360°/AI & Semantic** - Vector embeddings and search
- **Appendices/API Reference** - Alphabetical reference

---

**Built with SPARC methodology • Full TypeScript/JSDoc support • Production-ready**

Generated: 2025-11-20
