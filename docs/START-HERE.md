# Start Here: The Simplest Path

**Most users need only 11 lines of code.**

## The Pit of Success

```javascript
import { initStore, useGraph, useTurtle } from 'unrdf';

await initStore();
const turtle = useTurtle();
const graph = useGraph();

const store = turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:Alice ex:knows ex:Bob .
`);

const results = graph.select(`SELECT ?person WHERE { ?person ?p ?o }`);
console.log(results);
```

**That's a complete RDF application.**

---

## What Each Line Does

| Line | Purpose |
|------|---------|
| `import { initStore, useGraph, useTurtle }` | Import 3 functions (not 300) |
| `await initStore()` | Initialize context (call once at startup) |
| `const turtle = useTurtle()` | Get Turtle parser/serializer |
| `const graph = useGraph()` | Get graph operations |
| `turtle.parse(...)` | Parse RDF Turtle string into store |
| `graph.select(...)` | Execute SPARQL query |

---

## When to Add Complexity

**Default answer: Don't.**

| Question | If YES | If NO |
|----------|--------|-------|
| Do you need to validate data? | Add SHACL | Skip it |
| Do you need autonomous behaviors on every change? | Add Knowledge Hooks | Skip it |
| Do you need an audit trail? | Add Lockchain | Skip it |
| Do you need to optimize performance? | Add Dark Matter | Skip it |
| Are you building a React app? | Add React Hooks | Skip it |

**If all answers are NO, you're done.** Use the 11-line example.

---

## The 10 Essential Functions

You probably only need these:

| Function | When to Use |
|----------|-------------|
| `initStore()` | Once at app startup |
| `useGraph()` | For queries and modifications |
| `useTurtle()` | To parse/serialize Turtle |
| `useTerms()` | To create RDF terms |
| `useValidator()` | Only if you need SHACL validation |

**That's 5 functions for 80% of use cases.**

---

## What You Probably Don't Need

Unless you have a specific requirement:

- **Knowledge Hooks** - Only for autonomous, event-driven behaviors
- **TransactionManager** - Only for hook-driven transactions
- **LockchainWriter** - Only for cryptographic audit trails
- **PolicyPackManager** - Only for governance workflows
- **Dark Matter** - Only for performance optimization at scale
- **React Hooks** - Only for React applications
- **Federation** - Only for distributed systems
- **Streaming** - Only for real-time data pipelines

---

## Next Steps

1. **Copy the 11-line example** and run it
2. **Modify** it for your data
3. **Only add features** when you hit a specific need
4. **Consult [WHICH-FEATURES.md](WHICH-FEATURES.md)** if unsure

---

## Stop Here

If you're not sure whether you need more features, you don't.

The 11-line example is production-ready. Ship it.
