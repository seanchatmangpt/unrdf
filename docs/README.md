# unrdf Documentation

**unrdf** is the world's first **autonomic RDF framework** that transforms static knowledge graphs into intelligent, reactive, self-governing systems. It provides a "one true path" approach to autonomic RDF development, eliminating the "dark matter" of boilerplate glue code that typically plagues RDF workflows.

## Philosophy

unrdf is not a neutral toolkit—it's a **canon** for **autonomic knowledge graphs**. When you import unrdf, you're accepting its way. No escape hatches, no alternative backends, no configuration flexibility. This opinionated approach eliminates the 80/20 "dark matter" problem in RDF development.

**🌊 Blue Ocean Innovation: Autonomic Knowledge Hooks** are the crown jewel of unrdf - the world's first enterprise-grade triggers that enable reactive, self-governing knowledge systems with multi-agent coordination, policy pack governance, and cryptographic audit trails.

### Core Principles

- **🌊 Blue Ocean Innovation**: The world's first autonomic RDF framework
- **🤖 Autonomic Knowledge Hooks**: Self-governing triggers with multi-agent coordination
- **📦 Policy Pack Governance**: Versioned, portable governance units
- **🛡️ Cryptographic Integrity**: URDNA2015 canonical hashes with Git-anchored lockchain
- **⚡ Secure Execution**: VM2/worker thread sandboxing for safe hook execution
- **🔍 Delta-Aware Optimization**: Query optimization with caching and indexing
- **One Store**: N3.Store is the only memory model
- **One Terms**: N3 DataFactory is the only term creation method
- **One Query Engine**: Comunica is the only SPARQL engine
- **One Validator**: SHACL is the only validation method
- **One Canonicalization**: URDNA2015 is the only canonicalization method
- **One Validation**: Zod is the only runtime validation

## Quick Start

### Autonomic Knowledge Hooks (Primary API)

```javascript
import { initStore, defineHook, evaluateHook, PolicyPackManager } from 'unrdf';

// Initialize autonomic store with your RDF data
const runApp = initStore(turtleData, {
  enableLockchain: true,
  enableResolution: true,
  enablePolicyPacks: true
});

runApp(async () => {
  // Create policy pack manager
  const policyManager = new PolicyPackManager();
  await policyManager.loadPolicyPack('health-monitoring-v1');
  
  // Define an autonomic service health monitoring hook
  const healthHook = defineHook({
    meta: {
      name: 'autonomic-health-monitor',
      description: 'Self-governing service health monitoring with multi-agent coordination'
    },
    when: {
      kind: 'sparql-ask',
      ref: { 
        uri: 'file://health-check.rq',
        sha256: 'def456...',
        mediaType: 'application/sparql-query'
      }
    },
    run: async (event) => {
      // Autonomic decision-making logic
      return { 
        healthy: true, 
        actions: ['log-metrics', 'notify-ops'],
        agentConsensus: 0.98
      };
    }
  });

  // Evaluate with full autonomic audit trail
  const receipt = await evaluateHook(healthHook, { 
    persist: true,
    enableLockchain: true,
    enableMultiAgent: true
  });

  if (receipt.fired) {
    console.log('🤖 Autonomic health check completed');
    console.log('🔗 Lockchain Hash:', receipt.lockchainHash);
    console.log('📋 Policy Pack:', receipt.policyPack);
    console.log('🤝 Agent Consensus:', receipt.consensus);
    console.log('🛡️ Cryptographic Proof:', receipt.canonicalHash);
  }
});
```

### Autonomic Composables (Secondary API)

```javascript
import { useStore, useTerms, useGraph, useValidator, useZod, useLockchain, useResolution, usePolicyPacks } from 'unrdf';

// Create an autonomic store with multi-agent capabilities
const store = useStore();
const lockchain = useLockchain();
const resolution = useResolution();
const policyPacks = usePolicyPacks();

// Add some data
const terms = useTerms();
const quad = terms.quad(
  terms.iri("http://example.org/Person"),
  terms.iri("http://xmlns.com/foaf/0.1/name"),
  terms.lit("John Doe")
);
store.add(quad);

// Create an autonomic graph interface
const graph = useGraph(store.store);

// Query with SPARQL and optimization
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`, { enableCache: true, deltaAware: true });

// Multi-agent coordination
const proposal = await resolution.submitProposal('agent-1', {
  additions: [quad],
  removals: [],
  metadata: { confidence: 0.95 }
});

// Lockchain audit trail
const receipt = await lockchain.writeReceipt({
  operation: 'data-addition',
  hash: 'abc123...',
  signature: 'def456...'
});

// Validate with Zod and policy packs
const zod = useZod();
const PersonSchema = z.object({
  name: z.string()
});

const validation = await zod.validateResults(results, PersonSchema, {
  policyPack: 'compliance-v1'
});
console.log(validation.validated); // [{ name: "John Doe" }]
```

## Documentation Structure

### 🤖 Autonomic Knowledge Hooks (Primary)
- **[Autonomic Knowledge Hooks Guide](./guides/knowledge-hooks.md)** - Complete guide to the primary API
- **[Getting Started Guide](./guides/getting-started.md)** - Complete introduction to unrdf
- **[Advanced Patterns](./guides/advanced-patterns.md)** - Best practices and advanced usage
- **[Policy Pack Governance](./guides/policy-packs.md)** - Versioned governance units
- **[Multi-Agent Coordination](./guides/multi-agent.md)** - Distributed decision-making
- **[Lockchain Audit Trails](./guides/lockchain.md)** - Cryptographic provenance

### API Reference
- **[Autonomic Knowledge Hooks API](./api/knowledge-hooks.md)** - Primary API reference
- **[Composables API](./api/composables.md)** - Secondary composables reference
- **[Policy Pack API](./api/policy-packs.md)** - Governance units API
- **[Multi-Agent API](./api/multi-agent.md)** - Coordination and resolution API
- **[Lockchain API](./api/lockchain.md)** - Audit trail and provenance API
- **[Utilities API](./api/utilities.md)** - Helper functions for common operations

### Examples and Tutorials
- **[Autonomic Knowledge Hooks Examples](./examples/knowledge-hooks/)** - Primary API examples
- **[Policy Pack Examples](./examples/policy-packs/)** - Governance unit examples
- **[Multi-Agent Examples](./examples/multi-agent/)** - Coordination examples
- **[Lockchain Examples](./examples/lockchain/)** - Audit trail examples
- **[Basic Usage Examples](./examples/basic-usage.mjs)** - Fundamental operations
- **[Validation and Reasoning](./examples/validation-reasoning.mjs)** - Advanced features

### CLI Documentation
- **[CLI Reference](./cli/README.md)** - Command-line interface usage
- **[Autonomic Knowledge Hooks CLI](./cli/knowledge-hooks.md)** - Hook management commands
- **[Policy Pack CLI](./cli/policy-packs.md)** - Governance unit commands
- **[Multi-Agent CLI](./cli/multi-agent.md)** - Coordination commands
- **[Lockchain CLI](./cli/lockchain.md)** - Audit trail commands

## Core APIs

### 🤖 Autonomic Knowledge Hooks (Primary)
- **`defineHook`** - Define autonomic knowledge hooks
- **`evaluateHook`** - Evaluate hooks with cryptographic receipts
- **`initStore`** - Context management for hooks
- **`useKnowledgeHooks`** - Composable hook interface
- **`PolicyPackManager`** - Versioned governance units
- **`ResolutionLayer`** - Multi-agent coordination
- **`LockchainWriter`** - Cryptographic audit trails

### Foundation Composables (Secondary)
- **`useStore`** - N3.Store management and operations
- **`useTerms`** - RDF term creation and manipulation
- **`usePrefixes`** - Prefix management and CURIE operations
- **`useLockchain`** - Cryptographic audit trail operations
- **`useResolution`** - Multi-agent coordination operations
- **`usePolicyPacks`** - Governance unit operations

### Data Operations (Secondary)
- **`useLists`** - RDF list operations
- **`useGraph`** - High-level RDF operations and SPARQL queries
- **`useTurtle`** - Turtle parsing and serialization
- **`useNQuads`** - N-Quads parsing and serialization
- **`useJsonLd`** - JSON-LD operations
- **`useTurtleFS`** - File system operations for Turtle files

### Advanced Features (Secondary)
- **`usePointer`** - Clownface-based graph traversal
- **`useValidator`** - SHACL validation
- **`useReasoner`** - EYE-based reasoning
- **`useCanon`** - Canonicalization and isomorphism checking
- **`useZod`** - Runtime validation for RDF-derived data
- **`useTypes`** - RDF term type checking
- **`useRDFExt`** - Advanced RDF dataset operations

### Utilities (Secondary)
- **`useCache`** - Caching operations
- **`useDelta`** - Delta operations for RDF stores
- **`useMetrics`** - Metrics and analytics

## Key Features

- **🎯 Knowledge Hooks**: Enterprise-grade reactive triggers with cryptographic provenance
- **Composable Architecture**: Focused, single-responsibility functions
- **Cryptographic Provenance**: URDNA2015 canonical hashes for all operations
- **Type Safety**: JSDoc + Zod for runtime validation
- **Performance**: Sub-millisecond hook evaluation with optimized SPARQL
- **Developer Experience**: Minimal boilerplate, maximum productivity
- **Testing**: Comprehensive test suite with edge case coverage
- **CLI Tools**: Command-line interface for hook management
- **Error Handling**: Comprehensive error handling with descriptive messages
- **Documentation**: Extensive documentation with examples

## Installation

```bash
# Using pnpm (recommended)
pnpm add unrdf

# Using npm
npm install unrdf

# Using yarn
yarn add unrdf
```

## Why unrdf?

The RDF ecosystem has matured into a diverse set of libraries, but this diversity has created fragmentation. A typical project may mix N3 for parsing, Comunica for SPARQL, rdf-ext for datasets, rdf-validate-shacl for constraints, and eyereasoner for inference. Each library is useful in isolation, but together they form a patchwork of styles, APIs, and stores.

unrdf addresses this by enforcing a single opinionated path. The framework selects a canonical implementation for each layer and wraps them in a composable API pattern. The result is not a new ontology language or reasoner but a reduction of cognitive overhead for practitioners.

## Examples

### Basic Usage
```javascript
import { useStore, useTerms, useGraph } from 'unrdf';

const store = useStore();
const terms = useTerms();

// Add data
const person = terms.iri("http://example.org/person1");
const name = terms.lit("John Doe");
store.add(terms.quad(person, terms.iri("name"), name));

// Query data
const graph = useGraph(store.store);
const results = await graph.select(`
  SELECT ?name WHERE {
    ?person <http://example.org/name> ?name .
  }
`);
```

### Validation
```javascript
import { useValidator, useZod } from 'unrdf';
import { z } from 'zod';

// SHACL validation
const validator = useValidator();
const report = await validator.validate(dataStore, shapesStore);

// Zod validation
const zod = useZod();
const schema = z.object({ name: z.string() });
const validation = await zod.validateResults(results, schema);
```

### Reasoning
```javascript
import { useReasoner } from 'unrdf';

const reasoner = useReasoner();
const inferred = await reasoner.reason(dataStore, rulesStore);
```

## CLI Usage

```bash
# Convert between formats
npx unrdf convert input.ttl output.nq --from turtle --to nquads

# Validate data
npx unrdf validate data.ttl --shapes shapes.ttl

# Query data
npx unrdf query data.ttl "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"

# Apply reasoning
npx unrdf reason data.ttl --rules rules.n3 --output inferred.ttl
```

## License

MIT License - see [LICENSE](../LICENSE) for details.

## Contributing

This project follows the opinionated design philosophy. Contributions should align with the single-path approach and maintain the composable API pattern.

## Support

- **[GitHub Issues](https://github.com/gitvan/unrdf/issues)** - Report bugs or request features
- **[Discussions](https://github.com/gitvan/unrdf/discussions)** - Ask questions or share ideas
- **[Documentation](https://github.com/gitvan/unrdf#readme)** - Complete API reference