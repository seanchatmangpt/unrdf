# UNRDF - RDF Knowledge Graph Platform

**Research-grade RDF knowledge graphs with batteries included.**

> **Status:** Research Prototype - Architecturally complete, not production-validated.

UNRDF is a JavaScript platform for building intelligent knowledge graph applications using semantic web standards (RDF, SPARQL, SHACL) with modern tooling.

[![npm version](https://img.shields.io/badge/npm-v-26.4.7-blue)](https://www.npmjs.com/package/@unrdf/core)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Node.js >= 18](https://img.shields.io/badge/node-%3E%3D18-brightgreen)](https://nodejs.org)
[![tests](https://img.shields.io/badge/tests-100%25%20passing-brightgreen)](permutation-tests/)

---

## 🚀 Quick Start

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

// Initialize with all features
const core = await createKnowledgeSubstrateCore();

// Parse RDF data
const store = core.parseRdf(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:Alice foaf:name "Alice Smith" ; foaf:knows ex:Bob .
  ex:Bob foaf:name "Bob Johnson" .
`);

// Query with SPARQL
const results = await core.query(
  store,
  `
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`
);

for (const binding of results) {
  console.log(binding.get('name')?.value);
}
// Output: Alice Smith, Bob Johnson
```

**That's it.** You get:

- ✅ RDF storage & SPARQL queries
- ✅ SHACL validation
- ✅ ACID transactions
- ✅ Knowledge Hooks (reactive behaviors)
- ✅ Federation (distributed queries)
- ✅ Streaming (large graphs)
- ✅ CLI tools
- ✅ TypeScript types

---

## 📦 Installation

```bash
# npm
npm install @unrdf/core

# pnpm (recommended)
pnpm add @unrdf/core
```

**Requirements:** Node.js 18+, ESM modules

---

## 📚 Core Packages

| Package                 | Purpose                                                      |
| ----------------------- | ------------------------------------------------------------ |
| **`@unrdf/core`**       | RDF storage, SPARQL, SHACL validation ⭐                     |
| **`@unrdf/oxigraph`**   | Rust-based persistent backend                                |
| **`@unrdf/hooks`**      | Autonomous behaviors (react to data changes)                 |
| **`@unrdf/daemon`**     | Background orchestrator, Groq LLM integration, MCP server ⭐ |
| **`@unrdf/streaming`**  | Large graph streaming                                        |
| **`@unrdf/federation`** | Distributed queries                                          |
| **`@unrdf/cli`**        | Command-line tools                                           |
| **`@unrdf/browser`**    | Browser runtime (IndexedDB)                                  |

**59+ packages total** - See [docs/PACKAGES.md](docs/PACKAGES.md) for complete list.

---

## 🎯 Key Features

1. **RDF Graph Operations** - Parse Turtle/N-Triples/JSON-LD, SPARQL 1.1, export to any format
2. **SHACL Validation** - Define shapes, validate constraints, generate reports
3. **Transactions** - Atomic operations, ACID guarantees, rollback on failure
4. **Knowledge Hooks** - Define autonomous behaviors that react to data changes
5. **Streaming** - Process large graphs without memory bloat
6. **Federation** - Query multiple distributed stores simultaneously
7. **Browser Support** - Full RDF capabilities in modern browsers
8. **CLI Tools** - Query, validate, convert RDF files from command line
9. **Groq LLM Integration** - Build autonomous agents with AI-powered RDF reasoning
10. **OpenTelemetry** - Full observability with trace validation

---

## 🤖 Autonomous Agents with LLM Integration

Build agents that reason about RDF graphs using Groq:

```javascript
import { getGroqProvider } from '@unrdf/daemon';
import { generateText } from 'ai';

const provider = getGroqProvider();
const model = provider.getDefaultModel();

const result = await generateText({
  model,
  prompt: 'Analyze this RDF graph and suggest enrichments',
  tools: {
    queryGraph: {
      description: 'Query the knowledge graph',
      execute: async ({ query }) => await store.query(query),
    },
    enrichGraph: {
      description: 'Add triples to improve the graph',
      execute: async ({ triple }) => await store.add(triple),
    },
  },
  toolChoice: 'auto',
});
```

Perfect for knowledge graph curation, data quality improvement, and automated enrichment.

**See:** [packages/daemon/GROQ-INTEGRATION.md](packages/daemon/GROQ-INTEGRATION.md)

---

## 📖 Documentation

| Resource                                          | Purpose                                 |
| ------------------------------------------------- | --------------------------------------- |
| **[GETTING_STARTED.md](docs/GETTING_STARTED.md)** | Installation & tutorials (15 min setup) |
| **[ARCHITECTURE.md](docs/ARCHITECTURE.md)**       | System design & architecture            |
| **[PACKAGES.md](docs/PACKAGES.md)**               | Complete package documentation          |
| **[API_REFERENCE.md](docs/API_REFERENCE.md)**     | Full API documentation                  |
| **[CLI.md](docs/CLI.md)**                         | Command-line tools reference            |
| **[MCP_INTEGRATION.md](docs/MCP_INTEGRATION.md)** | Model Context Protocol guide            |

---

## 🧪 Testing

```bash
# Run all tests
pnpm test

# Fast pre-push suite
pnpm test:fast

# Coverage report
pnpm test --coverage
```

**100% test pass rate** on core packages (466+ tests).

---

## 🔒 Security

- ✅ Zero CRITICAL/HIGH CVEs (as of v26.4.7)
- ✅ Input validation via Zod schemas
- ✅ Sandboxed handler execution
- ✅ API key authentication (BLAKE3 hashing)
- ✅ OWASP Top 10 compliance
- ✅ Comprehensive security audit

**Report vulnerabilities:** security@unrdf.dev

---

## ⚡ Performance

- In-memory operations: ~1μs per triple
- Optimized SPARQL execution plans
- Streaming: constant memory usage
- Observability: <5% overhead

**See:** [docs/PERFORMANCE.md](docs/PERFORMANCE.md) for benchmarks.

---

## 🌐 Browser Support

Chrome/Edge 90+, Firefox 88+, Safari 14+

```html
<script type="module">
  import { createKnowledgeSubstrateCore } from 'https://cdn.jsdelivr.net/npm/@unrdf/browser';

  const core = await createKnowledgeSubstrateCore({ backend: 'indexeddb' });
</script>
```

---

## 📊 Use Cases

**Knowledge Management** - Query experts by skill, organizational knowledge graphs

**Semantic Search** - Find similar documents by topic, semantic relationships

**Reasoning & Inference** - SPARQL with property paths, transitive closure

**Policy Management** - Define compliance policies, validate against SHACL shapes

**Federated Data** - Query across multiple databases simultaneously

**Autonomous Agents** - LLM-powered graph reasoning and enrichment

---

## 🏗️ Architecture

```
Application Layer (Your apps)
    ↓
Knowledge Substrate (Hooks, Transactions, Validation)
    ↓
RDF Core (SPARQL, SHACL, Storage)
    ↓
Backends (Memory, Oxigraph, Remote)
```

**See:** [ARCHITECTURE.md](docs/ARCHITECTURE.md) for details.

---

## 🤝 Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

```bash
git clone https://github.com/unrdf/unrdf.git
cd unrdf
pnpm install
git checkout -b feat/your-feature
pnpm test && pnpm lint
git commit -m "feat: add your feature"
git push
```

---

## 📜 Resources

- **[Official Docs](https://unrdf.dev)** - Complete guides
- **[API Reference](https://unrdf.dev/api)** - Full API documentation
- **[GitHub Issues](https://github.com/unrdf/unrdf/issues)** - Bug reports
- **[GitHub Discussions](https://github.com/unrdf/unrdf/discussions)** - Community Q&A
- **[RDF Spec](https://www.w3.org/RDF/)** - W3C RDF standard
- **[SPARQL Spec](https://www.w3.org/TR/sparql11-query/)** - SPARQL 1.1

---

## 📝 License

MIT © 2024-2025 UNRDF Contributors

---

**Ready to start?** → [GETTING_STARTED.md](docs/GETTING_STARTED.md)
