# Getting Started with unrdf

This guide will help you get up and running with unrdf quickly. unrdf provides **Knowledge Hooks** as the primary API for reactive knowledge graphs, along with powerful composable functions for traditional RDF operations.

## Installation

Install unrdf using your preferred package manager:

```bash
# npm
npm install unrdf

# pnpm (recommended)
pnpm add unrdf

# yarn
yarn add unrdf
```

## 🎯 Knowledge Hooks (Primary API)

Knowledge Hooks are the crown jewel of unrdf - they transform static knowledge graphs into intelligent, reactive systems with built-in cryptographic provenance.

### Your First Knowledge Hook

Let's start with a simple service health monitoring hook:

```javascript
import { initStore, defineHook, evaluateHook } from 'unrdf';

// Initialize context with your RDF data
const runApp = initStore();

// Define a service health monitoring hook
const healthHook = defineHook({
  id: 'ex:ServiceHealthMonitor',
  name: 'Service Health Monitor',
  description: 'Monitors service error rates',
  select: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
  ],
  combine: 'OR'
});

// Run your application
runApp(async () => {
  // Load some sample data
  const sampleData = `
    @prefix ex: <http://example.org/> .
    ex:service1 ex:errorRate 0.01 .
    ex:service2 ex:errorRate 0.05 .
    ex:service3 ex:errorRate 0.08 .
  `;

  // Evaluate the hook with cryptographic receipt
  const receipt = await evaluateHook(healthHook, { data: sampleData });

  if (receipt.fired) {
    console.log('🚨 Service health issues detected!');
    console.log('Services with high error rates:', receipt.fired ? 'Found' : 'None');
    console.log('Evaluation took:', receipt.durations.totalMs, 'ms');
    console.log('Cryptographic proof:', receipt.provenance.receiptHash);
  }
});
```

### CLI Usage

```bash
# Create the hook file
cat > service-monitor.json << 'EOF'
{
  "id": "ex:ServiceHealthMonitor",
  "name": "Service Health Monitor",
  "description": "Monitors service error rates",
  "select": "SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }",
  "predicates": [
    {
      "kind": "THRESHOLD",
      "spec": {
        "var": "errorRate",
        "op": ">",
        "value": 0.02
      }
    }
  ],
  "combine": "OR"
}
EOF

# Load sample data
cat > services.ttl << 'EOF'
@prefix ex: <http://example.org/> .
ex:service1 ex:errorRate 0.01 .
ex:service2 ex:errorRate 0.05 .
ex:service3 ex:errorRate 0.08 .
EOF

# Evaluate the hook
unrdf hook eval --hook service-monitor.json --data services.ttl
```

## Traditional Composables (Secondary API)

The composable functions provide granular control over RDF operations when you need more control than Knowledge Hooks provide.

### 1. Create a Store

```javascript
import { useStoreContext } from 'unrdf';

// Create an empty store
const store = useStoreContext();

// Or create a store with initial data
const storeWithData = useStore([
  // ... initial quads
]);
```

### 2. Parse Turtle Data

```javascript
import { useTurtle } from 'unrdf';

const turtle = useTurtle();
const store = await turtle.parse(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:person a foaf:Person ;
    foaf:name "John Doe" ;
    foaf:age 30 .
`);
```

### 3. Query with SPARQL

```javascript
import { useGraph } from 'unrdf';

const graph = useGraph(store);

// Execute a SPARQL query
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`);

console.log(results); // [{ name: "John Doe" }]
```

## Why Choose Knowledge Hooks?

Knowledge Hooks provide significant advantages over traditional RDF workflows:

### 🚀 **Reactive by Design**
- **Declarative**: Define what to monitor, not how to monitor it
- **Automatic**: No need for polling or custom event systems
- **Proactive**: Issues are detected immediately when they occur

### 🛡️ **Cryptographic Provenance**
- **Signed Receipts**: Every evaluation is cryptographically signed
- **Audit Trail**: Complete history of all evaluations
- **Tamper-Proof**: Provenance cannot be modified after creation

### ⚡ **Enterprise Performance**
- **Sub-millisecond**: Typical evaluations complete in under 20ms
- **Optimized**: Uses efficient SPARQL engines and caching
- **Scalable**: Handles large datasets and complex queries

### 🔧 **Production Ready**
- **Error Isolation**: Individual hook failures don't affect others
- **Monitoring**: Built-in performance and health monitoring
- **Integration**: Easy integration with existing systems via webhooks

## Next Steps

### 🎯 Explore Knowledge Hooks
- **[Knowledge Hooks Guide](guides/knowledge-hooks.md)**: Complete guide to the primary API
- **[Knowledge Hooks API Reference](api/knowledge-hooks.md)**: Detailed API documentation
- **[Knowledge Hooks Examples](examples/knowledge-hooks/)**: Real-world examples

### 🧩 Learn Composables
- **[Composables API Reference](api/composables.md)**: Secondary API documentation
- **[Traditional Examples](examples/)**: Classic RDF operations examples

### 🖥️ Use the Playground
- **[Playground Guide](playground/)**: Interactive development environment
- Access the web interface for visual hook development
- Test hooks with real-time feedback

## Support

- **📚 Documentation**: Complete guides and API references
- **💡 Examples**: Extensive collection of real-world examples
- **🖥️ Playground**: Interactive web-based development environment
- **🐛 Issues**: Report bugs and request features on GitHub
- **💬 Discussions**: Ask questions and share ideas

Welcome to the future of reactive knowledge graphs with unrdf! 🎉

