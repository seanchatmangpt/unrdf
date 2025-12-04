# UNRDF Packages

UNRDF is organized as a monorepo with specialized packages. This guide explains what each package does and when to use it.

## Package Overview

```
@unrdf/
  ├─ core ...................... Main RDF library (START HERE)
  ├─ oxigraph .................. Rust-based triple store
  ├─ hooks ..................... Knowledge Hooks framework
  ├─ streaming ................. Large graph streaming
  ├─ federation ................ Federated queries
  ├─ knowledge-engine .......... Inference & reasoning
  ├─ browser ................... Browser runtime
  ├─ cli ....................... Command-line interface
  ├─ composables ............... React/Vue hooks
  ├─ project-engine ............ Workspace management
  ├─ dark-matter ............... Performance optimization
  └─ engine-gateway ............ API gateway
```

---

## Essential Packages

### ⭐ `@unrdf/core` - Main Library

**Purpose:** Core RDF operations, SPARQL queries, SHACL validation

**Install:**
```bash
npm install @unrdf/core
```

**What you get:**
- RDF triple storage (in-memory by default)
- SPARQL 1.1 query execution
- SHACL shape validation
- Multiple RDF format support
- Transaction support
- Type safety with Zod

**Exports:**
```javascript
import {
  // Main API
  createKnowledgeSubstrateCore,

  // RDF operations
  parseRdf,
  query,
  updateRdf,
  validateShacl,

  // Data model
  namedNode,
  literal,
  blankNode,
  variable,

  // Format handlers
  parseTurtle,
  toTurtle,
  parseNTriples,
  toNTriples,
  parseJsonLD,
  toJsonLD,

  // Utilities
  validateShape,
  createStore,
} from '@unrdf/core';
```

**Example:**
```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();
const store = core.parseRdf(`@prefix ex: <http://example.org/> .
  ex:Alice ex:knows ex:Bob .`);
const results = await core.query(store, 'SELECT * WHERE { ?s ?p ?o }');
```

**When to use:** Always. This is the foundation.

**Depends on:** `@unrdf/oxigraph`

---

### `@unrdf/oxigraph` - Rust Triple Store Backend

**Purpose:** Persistent, high-performance RDF storage

**Install:**
```bash
npm install @unrdf/oxigraph
```

**What you get:**
- Compiled Rust performance
- Persistent storage (SQLite)
- ~10x-100x faster than in-memory for large graphs
- Full SPARQL support
- Binary serialization
- Index optimization

**When to use:**
- Production environments
- Graphs > 10M triples
- Need persistent storage
- Performance critical operations

**Setup:**
```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore({
  backend: 'oxigraph',
  oxigraphOptions: {
    path: './data.db'  // SQLite file location
  }
});

const store = await core.loadStore('file://data.db');
```

**Performance:**
- Add 1M triples: ~5 seconds
- Query 1B triples: ~100ms
- Memory: ~1KB per triple

---

### `@unrdf/hooks` - Knowledge Hooks

**Purpose:** Define autonomous behaviors that react to data changes

**Install:**
```bash
npm install @unrdf/hooks
```

**What you get:**
- Event-driven architecture
- SPARQL pattern triggers
- Pre/post hooks
- Effect sandbox for safe execution
- Hook composition
- Lifecycle management

**Exports:**
```javascript
import {
  defineHook,
  registerHook,
  unregisterHook,
  triggerHook,
  createHookContext,
  defineHookEffect,
} from '@unrdf/hooks';
```

**Example:**
```javascript
import { defineHook, registerHook } from '@unrdf/hooks';

// Define a hook
const emailNotifier = defineHook({
  meta: { name: 'email-notifier', version: '1.0' },
  trigger: 'INSERT',
  pattern: '?user ex:email ?email .',

  run(event) {
    // Automatically called when pattern matches
    sendWelcomeEmail(event.quad.object.value);
  }
});

// Register it
registerHook(emailNotifier);

// Now when data matching the pattern is added, the hook fires automatically
```

**Use cases:**
- Audit logging
- Notifications
- Derived data generation
- Cache invalidation
- Policy enforcement
- Workflow triggers

---

## Extended Feature Packages

### `@unrdf/streaming` - Large Graph Streaming

**Purpose:** Process RDF graphs that don't fit in memory

**Install:**
```bash
npm install @unrdf/streaming
```

**What you get:**
- Pipe-based streaming
- Backpressure handling
- Transform streams
- Memory-efficient processing
- Batch operations

**Exports:**
```javascript
import {
  createReadStream,
  createWriteStream,
  createTransformStream,
  createFilterStream,
  createMapStream,
  pipe,
} from '@unrdf/streaming';
```

**Example:**
```javascript
import { createReadStream } from '@unrdf/streaming';

// Stream 1TB file, process only 1GB at a time
const readStream = createReadStream(hugeFile);
const processStream = createTransformStream(quad => {
  // Process each quad
  return processQuad(quad);
});
const writeStream = createWriteStream(outputFile);

readStream.pipe(processStream).pipe(writeStream);
```

**When to use:**
- Graphs > available RAM
- ETL pipelines
- Real-time data feeds
- Batch processing
- Log files
- Distributed data

---

### `@unrdf/federation` - Federated Queries

**Purpose:** Query multiple RDF sources as one

**Install:**
```bash
npm install @unrdf/federation
```

**What you get:**
- Multi-store querying
- Transparent source handling
- Distributed joins
- Union semantics
- Remote SPARQL endpoints

**Exports:**
```javascript
import {
  createFederatedStore,
  federatedQuery,
  addSourceShaping,
} from '@unrdf/federation';
```

**Example:**
```javascript
import { createFederatedStore } from '@unrdf/federation';

// Combine multiple sources
const fedStore = createFederatedStore([
  store1,           // Local store
  store2,           // Another local store
  remoteEndpoint    // Remote SPARQL endpoint
]);

// Query as if it's a single store
const results = await core.query(fedStore, `
  SELECT ?name WHERE {
    ?person foaf:name ?name ;
            foaf:knows ?friend .
    ?friend foaf:name ?friendName .
  }
`);
```

**Use cases:**
- Data integration
- Multi-tenant systems
- Distributed knowledge graphs
- Public data mashups
- Enterprise integration

---

### `@unrdf/knowledge-engine` - Inference & Reasoning

**Purpose:** Derive new facts from existing data using rules

**Install:**
```bash
npm install @unrdf/knowledge-engine
```

**What you get:**
- Forward chaining rules
- Backward chaining query
- OWL reasoning (partial)
- Custom rule definitions
- Performance optimized

**Exports:**
```javascript
import {
  createRuleSet,
  addRule,
  inferenceQuery,
  applyRules,
  createReasoner,
} from '@unrdf/knowledge-engine';
```

**Example:**
```javascript
import { applyRules } from '@unrdf/knowledge-engine';

// Define rules
const rules = `
  @prefix ex: <http://example.org/> .

  [rule1: (?a ex:parentOf ?b) (?b ex:parentOf ?c)
    -> (?a ex:grandparentOf ?c)]

  [rule2: (?a ex:sibling ?b) (?b ex:sibling ?c)
    -> (?a ex:sibling ?c)]
`;

// Apply rules (derive new facts)
const enrichedStore = await applyRules(store, rules);

// Query enriched store
const grandparents = await core.query(enrichedStore, `
  SELECT ?gp WHERE { ?gp ex:grandparentOf ?person }
`);
```

**Supports:**
- SPARQL rules
- N3 rules
- Custom JavaScript rules
- Transitive closure
- Inverse relations

---

### `@unrdf/browser` - Browser Runtime

**Purpose:** Run UNRDF in web browsers

**Install:**
```bash
npm install @unrdf/browser
```

**What you get:**
- Full UNRDF API in browser
- IndexedDB backend option
- Web Worker support
- Service Worker compatible
- Reduced bundle size
- IE 11+ support (with polyfills)

**Exports:**
```javascript
import {
  createKnowledgeSubstrateCore,
  // All core exports available
} from '@unrdf/browser';
```

**Example:**
```html
<script type="module">
  import { createKnowledgeSubstrateCore } from 'https://cdn.jsdelivr.net/npm/@unrdf/browser';

  const core = await createKnowledgeSubstrateCore({
    backend: 'indexeddb'  // Persistent in browser
  });

  const store = core.parseRdf(turtleData);
  const results = await core.query(store, sparqlQuery);
</script>
```

**Backends:**
- `memory` - Fastest, lost on page reload
- `indexeddb` - Persistent across sessions
- `localstorage` - Small graphs only (<5MB)

**Performance:**
- Memory: Same as Node
- IndexedDB: 10-100ms overhead per operation
- Query: ~1μs per triple (same as Node)

---

### `@unrdf/cli` - Command-Line Interface

**Purpose:** Query and manipulate RDF files from the command line

**Install:**
```bash
npm install -g @unrdf/cli
# or
npx @unrdf/cli
```

**Commands:**
```bash
# Query
unrdf query data.ttl --sparql "SELECT ?s WHERE { ?s ?p ?o }"

# Validate
unrdf validate data.ttl --shapes shapes.ttl

# Convert formats
unrdf convert data.ttl --from turtle --to ntriples

# Load
unrdf load data.ttl --backend oxigraph --db ./data.db

# Merge
unrdf merge file1.ttl file2.ttl --output merged.ttl

# Info
unrdf info data.ttl
```

**Example:**
```bash
# Count all triples
unrdf info data.ttl
# Output: Triples: 1,234,567, Subjects: 45,678, ...

# Find all people
unrdf query data.ttl --sparql "
  SELECT ?name WHERE {
    ?person rdf:type foaf:Person ;
            foaf:name ?name .
  }
"

# Validate against SHACL
unrdf validate data.ttl --shapes rules.ttl --report report.json
```

---

### `@unrdf/project-engine` - Project Management

**Purpose:** Manage RDF workspaces and projects

**Install:**
```bash
npm install @unrdf/project-engine
```

**What you get:**
- Project structure management
- Version control integration
- Backup & restore
- Multi-graph management
- Project metadata

**Example:**
```javascript
import { createProject } from '@unrdf/project-engine';

const project = await createProject({
  name: 'my-knowledge-base',
  version: '1.0.0'
});

// Add graphs
project.addGraph('employees', employeeStore);
project.addGraph('departments', deptStore);

// Save to disk
await project.save('./kb-backup');

// Load later
const loaded = await loadProject('./kb-backup');
```

---

## Utility Packages

### `@unrdf/composables` - React/Vue Hooks

**Purpose:** Composable hooks for using RDF in React/Vue

**Install:**
```bash
npm install @unrdf/composables
```

**Exports:**
```javascript
// React
import {
  useStore,
  useQuery,
  useValidate,
  useHook,
} from '@unrdf/composables/react';

// Vue
import {
  useStore,
  useQuery,
  useValidate,
  useHook,
} from '@unrdf/composables/vue';
```

**Example (React):**
```jsx
import { useQuery } from '@unrdf/composables/react';

function PersonList() {
  const { data, loading } = useQuery(`
    SELECT ?name WHERE {
      ?person foaf:name ?name .
    }
  `);

  return loading ? <div>Loading...</div> : (
    <ul>
      {data.map(row => <li key={row.name}>{row.name}</li>)}
    </ul>
  );
}
```

---

### `@unrdf/dark-matter` - Performance Optimization

**Purpose:** Advanced performance tuning and optimization

**Install:**
```bash
npm install @unrdf/dark-matter
```

**Features:**
- Query plan optimization
- Index management
- Memory profiling
- Caching strategies
- Benchmark utilities

---

### `@unrdf/engine-gateway` - API Gateway

**Purpose:** REST/GraphQL gateway for RDF engines

**Install:**
```bash
npm install @unrdf/engine-gateway
```

**Provides:**
- REST API endpoints
- GraphQL interface
- Authentication
- Rate limiting
- Request validation

---

## Choosing Packages

### Minimal Setup
```bash
npm install @unrdf/core
```

### Standard Setup (Recommended)
```bash
npm install @unrdf/core @unrdf/oxigraph @unrdf/hooks
```

### Full Stack
```bash
npm install \
  @unrdf/core \
  @unrdf/oxigraph \
  @unrdf/hooks \
  @unrdf/streaming \
  @unrdf/federation \
  @unrdf/knowledge-engine \
  @unrdf/browser \
  @unrdf/cli
```

### Browser Only
```bash
npm install @unrdf/browser
```

### Node CLI
```bash
npm install -g @unrdf/cli
```

---

## Dependencies Between Packages

```
@unrdf/core
  └─ @unrdf/oxigraph

@unrdf/hooks
  └─ @unrdf/core

@unrdf/streaming
  └─ @unrdf/core

@unrdf/federation
  └─ @unrdf/core
  └─ @unrdf/streaming

@unrdf/knowledge-engine
  └─ @unrdf/core
  └─ @unrdf/streaming

@unrdf/browser
  └─ @unrdf/core

@unrdf/cli
  └─ @unrdf/core
  └─ @unrdf/oxigraph

@unrdf/composables
  └─ @unrdf/core

@unrdf/project-engine
  └─ @unrdf/core

@unrdf/engine-gateway
  └─ @unrdf/core
  └─ (optional) @unrdf/federation
```

---

## Version Compatibility

All packages are released together with compatible versions:

```json
{
  "@unrdf/core": "5.0.0",
  "@unrdf/oxigraph": "5.0.0",
  "@unrdf/hooks": "5.0.0",
  "@unrdf/streaming": "5.0.0",
  "@unrdf/federation": "5.0.0"
}
```

Always keep versions synchronized.

---

## Migration Guides

### Upgrade from v4 to v5

```javascript
// v4 (old)
import { RDFStore } from '@unrdf/core';
const store = new RDFStore();

// v5 (new)
import { createKnowledgeSubstrateCore } from '@unrdf/core';
const core = await createKnowledgeSubstrateCore();
```

### Add Streaming to Existing Project

```javascript
// Existing
const results = await core.query(store, sparql);

// With streaming
import { createReadStream } from '@unrdf/streaming';
const stream = createReadStream(store);
// Process stream...
```

### Add Federation to Existing Project

```javascript
// Existing
const results = await core.query(store, sparql);

// With federation
import { createFederatedStore } from '@unrdf/federation';
const fedStore = createFederatedStore([store, remoteStore]);
const results = await core.query(fedStore, sparql);
```

---

**Next:** Read [GETTING-STARTED/INSTALLATION.md](GETTING-STARTED/INSTALLATION.md)
