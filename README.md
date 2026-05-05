# UNRDF - RDF Knowledge Graph Platform

**Research-grade RDF knowledge graphs with batteries included.**

> **Status:** Integrated Vision 2030 - Autonomic ready, L4/L5 hardened.

UNRDF is a JavaScript platform for building intelligent knowledge graph applications using semantic web standards (RDF, SPARQL, SHACL) with modern tooling.

[![npm version](https://img.shields.io/badge/npm-v-26.5.4-blue)](https://www.npmjs.com/package/@unrdf/core)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Node.js >= 18](https://img.shields.io/badge/node-%3E%3D18-brightgreen)](https://nodejs.org)
[![tests](https://img.shields.io/badge/tests-100%25%20passing-brightgreen)](packages/daemon/test/)

---

## 🚀 Quick Start

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

// Initialize with Vision 2030 features
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

- ✅ **PoWL v2 Process Mining** (wasm4pm)
- ✅ **Open Ontologies** (Semantic sidecar)
- ✅ RDF storage & SPARQL queries
- ✅ SHACL validation
- ✅ Knowledge Hooks (reactive semantic behaviors)
- ✅ Federation (distributed queries)
- ✅ Streaming (large graphs)
- ✅ CLI tools & MCP Server

---

## 📦 Installation

```bash
# npm
npm install @unrdf/core

# pnpm (recommended)
pnpm add @unrdf/core
```

**Requirements:** Node.js 18+, Rust (for Open Ontologies binary), ESM modules

---

## 📚 Core Packages

| Package                 | Purpose                                                      |
| ----------------------- | ------------------------------------------------------------ |
| **`@unrdf/core`**       | RDF storage, SPARQL, SHACL validation ⭐                     |
| **`@unrdf/wasm4pm`**    | PoWL v2 Process Mining (pictl WASM) ⭐                       |
| **`@unrdf/daemon`**     | Autonomic orchestrator, Open Ontologies sidecar manager ⭐    |
| **`@unrdf/hooks`**      | Autonomous behaviors (semantic-inference hooks)              |
| **`@unrdf/oxigraph`**   | Rust-based persistent backend                                |
| **`@unrdf/streaming`**  | Large graph streaming                                        |
| **`@unrdf/federation`** | Distributed queries                                          |
| **`@unrdf/cli`**        | Command-line tools                                           |

**59+ packages total** - See [docs/PACKAGES.md](docs/PACKAGES.md) for complete list.

---

## 🎯 Key Features

1. **Autonomic Process Mining** - Discover and validate processes in real-time using PoWL v2.
2. **Open Ontologies Sidecar** - High-performance semantic reasoning via a supervised Rust binary.
3. **Knowledge Hooks** - Define autonomous behaviors that react to semantic inference.
4. **RDF Graph Operations** - Parse Turtle/N-Triples/JSON-LD, SPARQL 1.1, export to any format.
5. **SHACL Validation** - Define shapes, validate constraints, generate reports.
6. **Streaming & Federation** - Process large, distributed graphs without memory bloat.
7. **Native MCP Support** - Expose your knowledge graph directly to LLM agents.
8. **Browser Support** - Full RDF capabilities in modern browsers.
9. **Groq LLM Integration** - AI-powered RDF reasoning.
10. **OpenTelemetry** - Full observability with trace validation.

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
| **[VISION-2030-BEST-PRACTICES.md](VISION-2030-BEST-PRACTICES.md)** | Foundational principles & best practices ⭐ |
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

- ✅ Zero CRITICAL/HIGH CVEs (as of v26.5.4)
- ✅ Input validation via Zod schemas
- ✅ Sandboxed handler execution
- ✅ API key authentication (BLAKE3 hashing)
- ✅ OWASP Top 10 compliance
- ✅ Comprehensive security audit

**Report vulnerabilities:** security@unrdf.dev

### Admission Engine

Control what data enters the knowledge graph with policy-based admission:

```javascript
import { createAdmissionEngine, wouldAdmit } from './src/admission/admission-engine.mjs';

// Create engine with config
const engine = createAdmissionEngine({ maxTriples: 10000 });

// Check if a delta capsule would be admitted
const admitted = await wouldAdmit(capsule, { maxTriples: 10000 });
```

#### `AdmissionConfigSchema`

Zod schema validating `{ maxTriples?, allowedPrefixes?, denyPatterns? }`.

#### `DecisionResultSchema`

Zod schema for admission decisions: `{ admitted: boolean, reason: string, violations: string[] }`.

#### `AdmissionEngine`

Class with `.evaluate(capsule)` returning a `DecisionResult`.

#### `createAdmissionEngine(config?)`

Factory that validates config via `AdmissionConfigSchema` and returns an `AdmissionEngine`.

#### `wouldAdmit(capsule, config?)`

Stateless helper — creates an ephemeral engine and returns `true` if the capsule passes admission.

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

- **[Official Docs](docs/GETTING_STARTED.md)** - Complete guides
- **[API Reference](docs/API_REFERENCE.md)** - Full API documentation
- **[GitHub](https://github.com/seanchatmangpt)** - Source & issues
- **[RDF Spec](https://www.w3.org/RDF/)** - W3C RDF standard
- **[SPARQL Spec](https://www.w3.org/TR/sparql11-query/)** - SPARQL 1.1

---

## 📝 License

MIT © 2024-2025 UNRDF Contributors

---

**Ready to start?** → [GETTING_STARTED.md](docs/GETTING_STARTED.md)
