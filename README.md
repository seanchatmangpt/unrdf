# UNRDF - RDF Knowledge Graph Platform

**Research-grade RDF knowledge graphs with batteries included.**

> **Status: v6.0.0-rc.2 (Release Candidate)** - Production-grade architecture, 99%+ test pass rate, 67 packages. See [Release Notes](RELEASE_NOTES.md) for details.

UNRDF is a streamlined, open-source platform for building intelligent knowledge graph applications. It combines semantic web standards (RDF, SPARQL, SHACL) with modern JavaScript/TypeScript tooling and nanosecond-precision temporal event sourcing.

**Perfect for:** Knowledge management systems, semantic search, event-sourced applications, temporal data tracking, and audit-trail systems.

**Latest Release**: [v6.0.0-rc.2](RELEASE_NOTES.md) - Critical integration fixes, 67 packages, Oxigraph 15K+ ops/sec

[![Version](https://img.shields.io/badge/version-6.0.0--rc.2-blue)](RELEASE_NOTES.md)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Node.js >= 18](https://img.shields.io/badge/node-%3E%3D18-brightgreen)](https://nodejs.org)
[![Test Pass Rate](https://img.shields.io/badge/tests-99%25%20passing-brightgreen)](PACKAGE_OPERATIONAL_STATUS.md)
[![Packages](https://img.shields.io/badge/packages-67-blue)](packages/)

> **🎯 Consolidation Notice (Dec 2024):** UNRDF has been streamlined to 3 production-ready packages with 100% test pass rate. See [CONSOLIDATION.md](permutation-tests/EXECUTIVE-SUMMARY.md) for details.

---

## 🎯 New to UNRDF? Start Here!

**[Getting Started Guide →](docs/GETTING_STARTED.md)** - Get productive in 15 minutes with hands-on examples!

This progressive guide takes you from zero to building your first knowledge graph application:

- ✅ Install and run your first example (5 min)
- ✅ Understand core concepts (5 min)
- ✅ Build a complete application (5 min)
- ✅ 5 working examples included

---

## 🚀 Quick Start

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

// Initialize the knowledge substrate with all features
const core = await createKnowledgeSubstrateCore();

// Parse RDF data (Turtle format)
const store = core.parseRdf(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:Alice foaf:name "Alice Smith" ;
           foaf:knows ex:Bob .

  ex:Bob foaf:name "Bob Johnson" .
`);

// Query with SPARQL
const results = await core.query(
  store,
  `
  SELECT ?name WHERE {
    ?person <http://xmlns.com/foaf/0.1/name> ?name .
  }
`
);

// Access results
for (const binding of results) {
  console.log(binding.get('name')?.value);
}
// Output:
// Alice Smith
// Bob Johnson
```

**That's it.** `createKnowledgeSubstrateCore()` gives you everything:

- **RDF Storage & Querying** - Triple store with SPARQL support
- **SHACL Validation** - Automated data validation
- **Transactions** - ACID operations on graphs
- **Knowledge Hooks** - Define autonomous behaviors that react to data changes
- **Streaming** - Process large graphs efficiently
- **Federation** - Query across distributed sources
- **Browser Support** - Run RDF operations in the browser
- **CLI Tools** - Command-line access to all features
- **Type Safety** - Zod validation throughout

---

## 📚 Documentation

### For Users

| Resource                                      | Purpose                              |
| --------------------------------------------- | ------------------------------------ |
| **[START-HERE.md](docs/START-HERE.md)**       | New? Read this first for orientation |
| **[ARCHITECTURE.md](docs/ARCHITECTURE.md)**   | How the system is organized          |
| **[PACKAGES.md](docs/PACKAGES.md)**           | Detailed package documentation       |
| **[GETTING-STARTED/](docs/GETTING-STARTED/)** | Installation & tutorials             |
| **[API-REFERENCE.md](docs/API-REFERENCE.md)** | Complete API documentation           |
| **[EXAMPLES.md](docs/EXAMPLES.md)**           | Code examples & sample projects      |

### For Contributors (Monorepo)

| Resource                                                            | Purpose                                   |
| ------------------------------------------------------------------- | ----------------------------------------- |
| **[MONOREPO-QUICK-REFERENCE.md](docs/MONOREPO-QUICK-REFERENCE.md)** | Quick overview of all 20 packages         |
| **[LOCAL-DEVELOPMENT.md](docs/LOCAL-DEVELOPMENT.md)**               | Setup dev environment, run tests & builds |
| **[WORKSPACE-STRUCTURE.md](docs/WORKSPACE-STRUCTURE.md)**           | File layout and naming conventions        |
| **[PACKAGE-DEVELOPMENT.md](docs/PACKAGE-DEVELOPMENT.md)**           | Create and modify packages                |
| **[TESTING-STRATEGY.md](docs/TESTING-STRATEGY.md)**                 | Cross-package testing guide               |

### Quick Links

- **[Installation](#installation)** - Get up and running
- **[Core Concepts](#core-concepts)** - Understand RDF basics
- **[Use Cases](#use-cases)** - See what you can build
- **[Packages](#packages)** - All components explained
- **[Contributing](#contributing)** - Help us improve

### Verification Requirements

All claims in this manifest are verifiable via:

1. **OTEL Validation**: `node validation/run-all.mjs comprehensive`
2. **SHACL Conformance**: `pnpm test -- --grep "shacl"`
3. **Performance Benchmarks**: `pnpm test:dark-matter`
4. **Type Safety**: Zod runtime validation on all inputs

---

## Installation

### Node.js (NPM/PNPM)

```bash
# Using npm
npm install @unrdf/core

# Using pnpm (recommended)
pnpm add @unrdf/core
```

### Browser

```html
<script type="module">
  import { createKnowledgeSubstrateCore } from 'https://cdn.jsdelivr.net/npm/@unrdf/browser';
  const core = await createKnowledgeSubstrateCore();
</script>
```

### Requirements

- **Node.js:** 18.0.0 or higher
- **Package Manager:** npm 8+, pnpm 7+, or yarn 3.2+
- **Module Type:** ES Modules (ESM)

For detailed installation instructions, see [GETTING-STARTED/INSTALLATION.md](docs/GETTING-STARTED/INSTALLATION.md).

---

## Core Concepts

### What is RDF?

RDF (Resource Description Framework) is a W3C standard for representing knowledge as a graph of statements called "triples":

```
Subject → Predicate → Object
Alice   → knows     → Bob
```

### Triple Store

UNRDF stores triples in memory or in persistent stores (Oxigraph, SQLite). Query and modify them safely with transactions.

```javascript
// Add triples
store.addQuad(subj, pred, obj);

// Query
const quads = store.match(subj, pred, obj);

// Validate against SHACL shapes
const isValid = await validateShacl(store, shapes);
```

### SPARQL Queries

SPARQL is the query language for RDF (like SQL for databases):

```sparql
SELECT ?name ?email
WHERE {
  ?person a foaf:Person .
  ?person foaf:name ?name .
  ?person foaf:mbox ?email .
  FILTER (regex(?email, "gmail.com"))
}
```

### Knowledge Hooks

Define autonomous behaviors that react to data changes:

```javascript
const myHook = defineHook({
  meta: { name: 'auto-notify-friends' },
  trigger: 'INSERT',
  pattern: '?person foaf:status ?status .',

  run(event) {
    // When someone's status changes, notify their friends
    const friends = queryFriends(event.quad.subject);
    notifyUsers(friends, `Friend updated: ${event.quad.object.value}`);
  },
});

registerHook(myHook);
```

---

## Core Features

### 1. **RDF Graph Operations**

- Parse multiple RDF formats (Turtle, N-Triples, JSON-LD, RDFa)
- Store triples in memory or persistent backends
- Query with SPARQL 1.1
- Export to any RDF format

### 2. **SHACL Validation**

- Define shapes for your data
- Validate graphs against constraints
- Generate validation reports
- Automated property validation

### 3. **Transactions**

- Atomic multi-statement operations
- Rollback on failure
- ACID guarantees
- Isolation levels

### 4. **Knowledge Hooks**

- React to data changes automatically
- Define custom business logic
- Compose behaviors declaratively
- Streaming event processing

### 5. **Streaming**

- Process large graphs without memory bloat
- Pipe-based streaming interface
- Backpressure handling
- Composable transformations

### 6. **Federation**

- Query multiple stores simultaneously
- Transparent query execution
- Cross-graph joins
- Distributed reasoning

### 7. **Browser Support**

- Run RDF operations in browser
- Indexeddb backend option
- Web Worker integration
- Service Worker ready

### 8. **CLI Tools**

- Load and query RDF files
- Validate against SHACL
- Convert between formats
- Execute SPARQL queries

### 9. **Security & Validation**

- ✅ Input sanitization via Zod schemas
- ✅ Handler sandboxing (isolated execution)
- ✅ RBAC authentication (token-based)
- ✅ XSS prevention (output escaping)
- ✅ Memory limits (10K triple max per operation)
- ✅ Protection against prototype pollution
- ✅ RDF injection prevention (URI validation)
- ✅ **NEW: API Key Authentication** (BLAKE3 hashing, constant-time verification)
- ✅ **NEW: Comprehensive Security Audit** across 13 daemon integration modules
- ✅ **NEW: Secret Detection** and injection attack prevention

**Security Audit**: All microframeworks have been security-hardened against OWASP Top 10 vulnerabilities (Dec 2025). See [SECURITY-REPORT-ADVERSARIAL-FRAMEWORKS.md](SECURITY-REPORT-ADVERSARIAL-FRAMEWORKS.md) for details.

**v6.0.0 Security Enhancements** (Jan 2026): Enterprise-grade authentication system with BLAKE3 cryptographic hashing, comprehensive security validation across all daemon operations, and zero CRITICAL/HIGH security vulnerabilities. See [packages/daemon/AUTHENTICATION.md](packages/daemon/AUTHENTICATION.md) and [packages/daemon/SECURITY_INTEGRATION_SUMMARY.md](packages/daemon/SECURITY_INTEGRATION_SUMMARY.md) for details.

---

## Use Cases

### 📊 Knowledge Management

Store and query organizational knowledge - people, projects, skills, expertise:

```javascript
// Query experts by skill
const experts = await core.query(
  store,
  `
  SELECT ?expert ?skill WHERE {
    ?expert a ex:Employee ;
            ex:hasSkill ?skill ;
            ex:experienceLevel "expert" .
  }
`
);
```

### 🔍 Semantic Search

Build intelligent search powered by semantic relationships:

```javascript
// Find similar documents by topic
const similar = await core.query(
  store,
  `
  SELECT ?doc1 ?doc2 WHERE {
    ?doc1 dct:subject ?topic .
    ?doc2 dct:subject ?topic .
    FILTER (?doc1 != ?doc2)
  }
`
);
```

### 🤖 Reasoning & Inference

Derive new facts from existing data:

```javascript
// SPARQL with inference rules
const results = await core.query(
  store,
  `
  SELECT ?ancestor WHERE {
    ?person ex:parentOf+ ?ancestor .  # transitive closure
  }
`
);
```

### 📋 Policy Management

Define and enforce policies through knowledge graphs:

```javascript
// Query policy compliance
const violations = await core.query(
  store,
  `
  SELECT ?resource ?violation WHERE {
    ?resource a sec:ProtectedResource ;
              sec:policy ?policy .
    ?policy sec:requires ?requirement .
    FILTER NOT EXISTS { ?resource sec:has ?requirement }
  }
`
);
```

### 🌐 Federated Data Integration

Combine data from multiple sources:

```javascript
// Query across databases
const results = await core.federatedQuery([store1, store2, remoteGraphEndpoint], sparqlQuery);
```

---

## Production Packages

UNRDF is organized as a **67-package monorepo** with clear separation of concerns:

### Essential Packages (Start Here)

- **`@unrdf/core`** - RDF storage, SPARQL queries, SHACL validation ⭐
- **`@unrdf/oxigraph`** - Rust-based persistent triple store backend
- **`@unrdf/hooks`** - Knowledge Hooks autonomous behaviors framework
- **`@unrdf/daemon`** - Background operation orchestrator with enterprise-grade security ⭐ NEW

### Extended Features

- **`@unrdf/streaming`** - Large graph streaming & real-time sync
- **`@unrdf/federation`** - Distributed query execution across stores
- **`@unrdf/knowledge-engine`** - Inference and semantic reasoning (EYE)
- **`@unrdf/browser`** - Browser runtime with IndexedDB support
- **`@unrdf/cli`** - Command-line interface & tools
- **`@unrdf/react`** - React hooks & component integration
- **`@unrdf/engine-gateway`** - API gateway & µ(O) enforcement layer

### Optional/Alpha

- **`@unrdf/composables`** - Vue 3 composable integration
- **`@unrdf/dark-matter`** - Query optimization & performance analysis
- **`@unrdf/project-engine`** - Workspace management (dev tools)

### Internal Packages

- **`@unrdf/test-utils`** - Shared testing infrastructure
- **`@unrdf/validation`** - OTEL validation & compliance checking
- **`@unrdf/domain`** - Type definitions & schemas (Zod)

**Full Stack Integration:** Test 11 validates all 3 packages work together ✅ (563ms)

### 📊 Package Health Status (v6.0.0-rc.2)

| Category | Count | Status |
| -------- | ----- | ------ |
| **Operational** | 8+ | 99%+ test pass rate |
| **Partially Operational** | 3+ | Minor issues, core works |
| **Research Prototypes** | 40+ | Experimental features |
| **Infrastructure/Docs** | 10+ | Tooling and documentation |

**Recent Fixes** (rc.2):
- ✅ `@unrdf/test-utils` - Corrected exports (PolicyPackManager, createLockchainWriter)
- ✅ `@unrdf/core` - Added missing export specifier for lockchain-writer
- ✅ `@unrdf/knowledge-engine` - Resolved circular dependency

**Evidence:** See [PACKAGE_OPERATIONAL_STATUS.md](PACKAGE_OPERATIONAL_STATUS.md) for detailed status.

---

## Architecture

UNRDF follows a layered architecture:

```
┌─────────────────────────────────────────┐
│  Application Layer                      │
│  (Your RDF-powered applications)        │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│  Knowledge Substrate                    │
│  (Transactions, Hooks, Validation)      │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│  RDF Core                               │
│  (SPARQL, SHACL, Storage)               │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│  Backends                               │
│  (Memory, Oxigraph, Remote)             │
└─────────────────────────────────────────┘
```

See [ARCHITECTURE.md](docs/ARCHITECTURE.md) for detailed system design.

---

## Examples

### Example: Build a Simple Blog Knowledge Base

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();

// Define blog data
const blogData = `
  @prefix blog: <http://example.org/blog/> .
  @prefix dct: <http://purl.org/dc/terms/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  blog:post1 a blog:BlogPost ;
            dct:title "Hello UNRDF" ;
            dct:creator blog:alice ;
            dct:date "2024-01-01" .

  blog:alice a foaf:Person ;
            foaf:name "Alice" ;
            foaf:mbox "alice@example.org" .
`;

const store = core.parseRdf(blogData);

// Query: Find all blog posts with their creators
const results = await core.query(
  store,
  `
  PREFIX blog: <http://example.org/blog/>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?title ?author WHERE {
    ?post a blog:BlogPost ;
          dct:title ?title ;
          dct:creator ?creator .
    ?creator foaf:name ?author .
  }
`
);

// Display results
for (const row of results) {
  console.log(`${row.get('title').value} by ${row.get('author').value}`);
}
// Output: Hello UNRDF by Alice
```

More examples:

- [GETTING-STARTED/QUICK-START.md](docs/GETTING-STARTED/QUICK-START.md) - Minimal example
- [EXAMPLES.md](docs/EXAMPLES.md) - Full example collection
- [examples/](examples/) - Runnable examples in the repository

---

## TypeScript Support

Full TypeScript support with exported type definitions:

```typescript
import {
  createKnowledgeSubstrateCore,
  type KnowledgeSubstrate,
  type SPARQLResult,
} from '@unrdf/core';

const core: KnowledgeSubstrate = await createKnowledgeSubstrateCore();
const results: SPARQLResult[] = await core.query(store, sparql);
```

---

## Configuration

Customize the Knowledge Substrate:

```javascript
const core = await createKnowledgeSubstrateCore({
  // Default: all enabled (the 20% that delivers 80% of value)
  enableTransactionManager: true,
  enableKnowledgeHookManager: true,
  enableValidation: true,
  enableObservability: true,

  // Optional: enable only if needed
  enableFederation: false,
  enableStreaming: false,
  enableBrowserSupport: false,
});
```

---

## Performance

UNRDF is optimized for performance:

- **In-memory operations:** ~1μs per triple
- **SPARQL queries:** Optimized execution plans
- **Streaming:** Constant memory usage with large graphs
- **Observability:** Minimal overhead with optional telemetry (<5% overhead)
- **Validation:** Zod schema validation adds ~0.1ms per operation

See [docs/PERFORMANCE.md](docs/PERFORMANCE.md) for benchmarks and optimization tips.

---

## Security

UNRDF follows security best practices:

- **Zero CRITICAL/HIGH CVEs** (as of Dec 2025)
- **Input validation** via Zod schemas on all public APIs
- **Sandboxed execution** for untrusted RDF handlers
- **OWASP Top 10 compliance** in all microframeworks
- **No hardcoded secrets** - environment-based configuration
- **Regular security audits** with adversarial testing

**Security Policy**: Report vulnerabilities to security@unrdf.dev

**Recent Fixes** (v5.0.0-beta.1 → v5.0.0-beta.2):

- ✅ Fixed 7 vulnerabilities (CVSS 4.0-9.8) in microframeworks
- ✅ Added input sanitization to prevent XSS attacks
- ✅ Implemented handler sandboxing (no process access)
- ✅ Added RBAC authentication for all routes
- ✅ Protected against prototype pollution and RDF injection

---

## Browser Support

UNRDF works in modern browsers:

```html
<script type="module">
  import { createKnowledgeSubstrateCore } from 'https://cdn.jsdelivr.net/npm/@unrdf/browser@latest';

  const core = await createKnowledgeSubstrateCore({
    backend: 'indexeddb', // Persistent storage in browser
  });

  const store = core.parseRdf(turtleData);
  const results = await core.query(store, sparqlQuery);
</script>
```

Supported browsers:

- Chrome/Edge 90+
- Firefox 88+
- Safari 14+
- Mobile browsers (iOS Safari 14+, Chrome Mobile)

---

## CLI Usage

Query RDF files from the command line:

```bash
# Query a Turtle file
npx @unrdf/cli query data.ttl --sparql "SELECT * WHERE { ?s ?p ?o }"

# Validate against SHACL shapes
npx @unrdf/cli validate data.ttl --shapes shapes.ttl

# Convert formats
npx @unrdf/cli convert data.ttl --to json-ld

# Load and save
npx @unrdf/cli load data.ttl --backend oxigraph
```

See [docs/CLI.md](docs/CLI.md) for complete CLI documentation.

---

## Testing

UNRDF has comprehensive test coverage:

```bash
# Run all tests
npm test

# Run specific package tests
npm run test:core
npm run test:hooks
npm run test:streaming

# Watch mode
npm run test:watch

# Coverage report
npm run test -- --coverage
```

---

## Contributing

We welcome contributions! Here's how to get started:

1. **Fork the repository** on GitHub
2. **Clone and install:**
   ```bash
   git clone https://github.com/yourusername/unrdf.git
   cd unrdf
   pnpm install
   ```
3. **Create a feature branch:**
   ```bash
   git checkout -b feat/your-feature
   ```
4. **Make your changes** and run tests:
   ```bash
   pnpm test
   pnpm lint
   ```
5. **Commit with clear messages:**
   ```bash
   git commit -m "feat: add your feature"
   ```
6. **Push and create a Pull Request**

See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.

---

## Roadmap

Planned features and improvements:

- [ ] GraphQL support (query RDF with GraphQL)
- [ ] Enhanced inference engine (more OWL support)
- [ ] Distributed triple store (IPFS backend)
- [ ] GraphQL Federation
- [ ] Real-time subscriptions
- [ ] Machine learning integrations
- [ ] Visual graph editor
- [ ] Mobile SDKs

---

## Troubleshooting

### Common Issues

**Q: "Module not found" error**

```javascript
// ❌ Wrong
import { createKnowledgeSubstrateCore } from 'unrdf';

// ✅ Correct
import { createKnowledgeSubstrateCore } from '@unrdf/core';
```

**Q: SPARQL query returns no results**

- Use [GETTING-STARTED/SPARQL.md](docs/GETTING-STARTED/SPARQL.md) to debug queries
- Check your prefixes match the data
- Validate your RDF is correctly parsed

**Q: Browser compatibility issues**

- Ensure you're using `@unrdf/browser` not `@unrdf/core`
- Check browser support (Chrome 90+, Firefox 88+, Safari 14+)

For more help, see [docs/TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md) or open an [issue](https://github.com/unrdf/unrdf/issues).

---

## Resources

- **[Official Documentation](https://unrdf.dev)** - Complete guides
- **[API Reference](https://unrdf.dev/api)** - Full API documentation
- **[GitHub Issues](https://github.com/unrdf/unrdf/issues)** - Bug reports & feature requests
- **[GitHub Discussions](https://github.com/unrdf/unrdf/discussions)** - Community Q&A
- **[RDF Specification](https://www.w3.org/RDF/)** - W3C RDF standard
- **[SPARQL Specification](https://www.w3.org/TR/sparql11-query/)** - SPARQL query language

---

## Performance Comparison

How UNRDF compares to alternatives:

| Feature              | UNRDF           | GraphDB | Virtuoso  | RDFLib   |
| -------------------- | --------------- | ------- | --------- | -------- |
| **Language**         | JavaScript/Node | Java    | C++       | Python   |
| **SPARQL Support**   | ✅ 1.1          | ✅ 1.1  | ✅ 1.1    | ✅       |
| **Transactions**     | ✅              | ✅      | ✅        | ❌       |
| **Streaming**        | ✅              | ✅      | ✅        | ❌       |
| **Browser Support**  | ✅              | ❌      | ❌        | ❌       |
| **Federation**       | ✅              | ✅      | ✅        | ❌       |
| **In-memory Speed**  | Very Fast       | Fast    | Very Fast | Moderate |
| **Setup Complexity** | Simple          | Complex | Complex   | Simple   |

---

## Limitations

**Current Research Prototype Status:**

- **Test Coverage:** KGC-4D: 90.4% pass rate (9 test failures); YAWL: No tests
- **Performance Claims:** Measured benchmarks show sub-millisecond SPARQL queries and 2,492 receipts/sec (see [BENCHMARK-RESULTS.md](BENCHMARK-RESULTS.md))
- **Production Readiness:** Architecturally complete, operationally unvalidated
- **Known Issues:** Event counting and time-travel reconstruction edge cases (see [TEST-RESULTS.md](TEST-RESULTS.md))

**What We Cannot Claim:**

1. Production-grade reliability without comprehensive test coverage
2. Performance guarantees beyond measured benchmarks
3. Scalability to 1B+ triples (not tested)
4. Comparison benchmarks vs Temporal.io, Camunda, Airflow (not performed)

See [PERFORMANCE-VALIDATION.md](PERFORMANCE-VALIDATION.md) for detailed claims vs reality analysis.

---

## License

MIT © 2024-2025 UNRDF Contributors

See [LICENSE](LICENSE) for details.

---

## Sponsors

UNRDF is maintained with support from:

- [Your Organization](https://example.org)
- [The Community](https://github.com/unrdf/unrdf/blob/main/CONTRIBUTORS.md)

Interested in sponsoring? [Contact us](mailto:sponsors@unrdf.dev)

---

## Changelog

See [CHANGELOG.md](CHANGELOG.md) for complete version history.

**Latest Release**: [v6.0.0-rc.2 Release Notes](RELEASE_NOTES.md)

**Migration**: [v5 to v6 Migration Guide](MIGRATION_GUIDE_v6.md)

---

**Ready to get started?** → [START-HERE.md](docs/START-HERE.md)
