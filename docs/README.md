# UNRDF Documentation

**Production-ready RDF Knowledge Graph Library**

Welcome to the UNRDF documentation! We use the [Diátaxis framework](https://diataxis.fr/) to organize our documentation into four distinct categories based on your needs.

---

## 📚 Documentation Structure

<table>
<tr>
<td width="50%">

### 🎓 [Tutorials](./tutorials/README.md)
**Learning-oriented** • **Hands-on lessons**

Step-by-step guides to learn UNRDF by building practical applications.

**Best for:**
- Newcomers to UNRDF
- Learning new features
- Getting started quickly

**Popular tutorials:**
- [Quick Start (15 min)](./tutorials/01-quick-start.md)
- [First Knowledge Hook (30 min)](./tutorials/02-first-knowledge-hook.md)
- [Browser Integration (45 min)](./tutorials/03-browser-integration.md)

</td>
<td width="50%">

### 🔧 [How-to Guides](./how-to/README.md)
**Task-oriented** • **Problem solutions**

Recipes for accomplishing specific tasks and solving real-world problems.

**Best for:**
- Experienced users
- Specific task solutions
- Quick reference

**Popular guides:**
- [Optimize SPARQL Queries](./how-to/optimize-sparql-queries.md)
- [Deploy with Docker](./how-to/deploy-with-docker.md)
- [Create Validation Hooks](./how-to/create-validation-hooks.md)

</td>
</tr>
<tr>
<td width="50%">

### 📖 [Reference](./reference/README.md)
**Information-oriented** • **Technical specs**

Complete API documentation, configuration options, and technical specifications.

**Best for:**
- API lookups
- Configuration details
- Technical specifications

**Key sections:**
- [Core API](./reference/README.md#core-api)
- [CLI Reference](./reference/README.md#cli-reference)
- [Type Definitions](./reference/README.md#type-definitions)
- [Error Catalog](./reference/errors/error-catalog.md)

</td>
<td width="50%">

### 💡 [Explanation](./explanation/README.md)
**Understanding-oriented** • **Deep dives**

Conceptual guides explaining the "why" behind UNRDF's design and architecture.

**Best for:**
- Understanding concepts
- Architecture insights
- Design decisions

**Popular topics:**
- [Knowledge Hooks Philosophy](./explanation/knowledge-hooks-philosophy.md)
- [Architecture Overview](./explanation/architecture-overview.md)
- [80/20 Principle](./explanation/80-20-principle.md)

</td>
</tr>
</table>

---

## 🚀 Quick Start

### Installation
```bash
npm install unrdf
# or
pnpm add unrdf
```

### Your First Knowledge Graph
```javascript
import { createKnowledgeEngine } from 'unrdf';
import { namedNode, literal } from '@rdfjs/data-model';

// Create engine
const engine = await createKnowledgeEngine();

// Insert triples
await engine.insert([
  {
    subject: namedNode('http://example.org/alice'),
    predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
    object: literal('Alice'),
    graph: namedNode('http://example.org/graph1')
  }
]);

// Query with SPARQL
const results = await engine.query(`
  SELECT ?name WHERE {
    ?person <http://xmlns.com/foaf/0.1/name> ?name .
  }
`);

console.log(results); // [{ name: 'Alice' }]
```

👉 **Next:** [Complete the Quick Start Tutorial](./tutorials/01-quick-start.md)

---

## 🎯 Find What You Need

### I want to...

**Learn UNRDF from scratch**
→ Start with [Tutorials](./tutorials/README.md)

**Solve a specific problem**
→ Check [How-to Guides](./how-to/README.md)

**Look up API details**
→ Browse [Reference Documentation](./reference/README.md)

**Understand concepts deeply**
→ Read [Explanation Guides](./explanation/README.md)

**Fix an issue**
→ See [Troubleshooting Guide](./TROUBLESHOOTING.md)

**Get answers quickly**
→ Check [FAQ](./FAQ.md)

---

## 🌟 Key Features

### Knowledge Hooks
Reactive hooks that execute effects when RDF data changes. Perfect for validation, transformation, and audit trails.

**Learn more:** [Knowledge Hooks Tutorial](./tutorials/02-first-knowledge-hook.md) • [Philosophy](./explanation/knowledge-hooks-philosophy.md)

### Browser Support
Run UNRDF in the browser with IndexedDB storage and React hooks integration.

**Learn more:** [Browser Integration Tutorial](./tutorials/03-browser-integration.md) • [Architecture](./explanation/browser-integration-design.md)

### Policy Packs
Composable validation policies with SHACL constraints and custom rules for data governance.

**Learn more:** [Policy Packs Tutorial](./tutorials/04-policy-packs.md) • [Design](./explanation/policy-pack-design.md)

### Real-time Streaming
Process change feeds, implement windowing, and build reactive data pipelines.

**Learn more:** [Streaming Tutorial](./tutorials/05-real-time-streaming.md) • [Architecture](./explanation/change-feed-architecture.md)

### Distributed Federation
Scale across multiple nodes with consensus protocols and federated queries.

**Learn more:** [Federation Tutorial](./tutorials/06-distributed-federation.md) • [Architecture](./explanation/federation-architecture.md)

### AI Integration
NLP query builders, semantic analyzers, and embedding managers for semantic search.

**Learn more:** [AI Integration Tutorial](./tutorials/07-ai-semantic-integration.md)

### Production-Ready
OpenTelemetry observability, Docker/Kubernetes deployment, and comprehensive testing.

**Learn more:** [Production Deployment Tutorial](./tutorials/08-production-deployment.md) • [Best Practices](./explanation/monitoring-alerting.md)

---

## 📦 What's in the Box

- **Core Engine:** ACID transactions, SPARQL 1.1, RDF 1.1 support
- **Knowledge Hooks:** Pre/post transaction hooks with isolated-VM sandboxing
- **Policy Packs:** SHACL validation and custom rules
- **Browser Support:** IndexedDB storage, React hooks, offline-first
- **Streaming:** Change feeds, windowing, real-time validation
- **Federation:** Multi-node consensus, distributed queries
- **AI/Semantic:** NLP, embeddings, semantic search
- **Observability:** OpenTelemetry spans, metrics, distributed tracing
- **Deployment:** Docker, Kubernetes, Terraform, Testcontainers
- **CLI:** Query execution, data loading, validation

---

## 🎓 Learning Paths

### Beginner (2-3 hours)
1. [Quick Start Tutorial](./tutorials/01-quick-start.md) (15 min)
2. [First Knowledge Hook](./tutorials/02-first-knowledge-hook.md) (30 min)
3. [Browser Integration](./tutorials/03-browser-integration.md) (45 min)

### Intermediate (4-5 hours)
1. All beginner tutorials
2. [Policy Packs](./tutorials/04-policy-packs.md) (40 min)
3. [Real-time Streaming](./tutorials/05-real-time-streaming.md) (50 min)
4. [How-to Guides](./how-to/README.md) for your use case

### Advanced (6-8 hours)
1. All intermediate tutorials
2. [Distributed Federation](./tutorials/06-distributed-federation.md) (60 min)
3. [AI Integration](./tutorials/07-ai-semantic-integration.md) (55 min)
4. [Production Deployment](./tutorials/08-production-deployment.md) (90 min)
5. Deep dive into [Explanation Guides](./explanation/README.md)

---

## 🤝 Getting Help

### Community Support
- **[GitHub Discussions](https://github.com/unrdf/unrdf/discussions)** - Ask questions, share ideas
- **[Stack Overflow](https://stackoverflow.com/questions/tagged/unrdf)** - Search existing Q&A
- **[GitHub Issues](https://github.com/unrdf/unrdf/issues)** - Report bugs, request features

### Documentation Resources
- **[FAQ](./FAQ.md)** - Frequently asked questions
- **[Troubleshooting](./TROUBLESHOOTING.md)** - Common problems and solutions
- **[Migration Guide](./migration-v3-to-v4.md)** - Upgrading from v3.x
- **[Changelog](../CHANGELOG.md)** - Version history

---

## 📊 Project Status

- **Version:** 4.0.0 (Latest)
- **Status:** Production Ready ✅
- **Test Coverage:** 100% (349/349 tests passing)
- **OTEL Validation:** 94/100 (Production Ready)
- **License:** MIT
- **Node:** ≥18.0.0
- **Browser:** Modern browsers with IndexedDB support

---

## 🗺️ Documentation Map

```
docs/
├── README.md                    ← You are here
│
├── tutorials/                   ← 🎓 Learning-oriented
│   ├── 01-quick-start.md
│   ├── 02-first-knowledge-hook.md
│   ├── 03-browser-integration.md
│   ├── 04-policy-packs.md
│   ├── 05-real-time-streaming.md
│   ├── 06-distributed-federation.md
│   ├── 07-ai-semantic-integration.md
│   └── 08-production-deployment.md
│
├── how-to/                      ← 🔧 Task-oriented
│   ├── Core Operations/
│   ├── Knowledge Hooks/
│   ├── Browser & Client-Side/
│   ├── Policy & Validation/
│   ├── Streaming & Real-time/
│   ├── Distributed Systems/
│   ├── Observability & Monitoring/
│   └── Deployment & Production/
│
├── reference/                   ← 📖 Information-oriented
│   ├── api/                    (API documentation)
│   ├── config/                 (Configuration)
│   ├── cli/                    (CLI reference)
│   ├── types/                  (Type definitions)
│   ├── errors/                 (Error catalog)
│   └── benchmarks/             (Performance data)
│
├── explanation/                 ← 💡 Understanding-oriented
│   ├── Core Concepts/
│   ├── Architecture/
│   ├── Design Decisions/
│   ├── Best Practices/
│   └── Advanced Topics/
│
├── FAQ.md                       ← Quick answers
├── TROUBLESHOOTING.md          ← Problem solving
├── ROADMAP.md                  ← Future plans
└── migration-v3-to-v4.md       ← Upgrade guide
```

---

## 🚢 Ready to Start?

Choose your path:

**New to UNRDF?** → [Quick Start Tutorial](./tutorials/01-quick-start.md)

**Need something specific?** → [How-to Guides](./how-to/README.md)

**Looking for API details?** → [Reference](./reference/README.md)

**Want to understand deeply?** → [Explanations](./explanation/README.md)

---

<p align="center">
  <strong>Built with SPARC methodology • Powered by 80/20 principle • Production-ready</strong>
</p>

<p align="center">
  <a href="https://github.com/unrdf/unrdf">GitHub</a> •
  <a href="https://www.npmjs.com/package/unrdf">npm</a> •
  <a href="https://github.com/unrdf/unrdf/discussions">Discussions</a> •
  <a href="../CONTRIBUTING.md">Contributing</a>
</p>
