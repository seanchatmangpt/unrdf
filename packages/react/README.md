# UNRDF React

React hooks and integration layer for the UNRDF knowledge engine. Build reactive RDF applications with React.

## Overview

`unrdf-react` provides a comprehensive set of React hooks for integrating UNRDF knowledge engine capabilities into React applications:

- **97 React hooks** across 25 categories
- **18 test files** with comprehensive coverage
- **Full TypeScript support** with JSDoc
- **Context-based state management**
- **Knowledge Hooks integration**
- **Federation support**
- **Real-time streaming**
- **Error boundaries and recovery**
- **Query caching and optimization**

## Installation

```bash
npm install unrdf-react
# or
pnpm add unrdf-react
```

## Quick Start

```javascript
import { KnowledgeEngineProvider, useStore } from 'unrdf-react';

function App() {
  return (
    <KnowledgeEngineProvider>
      <GraphEditor />
    </KnowledgeEngineProvider>
  );
}

function GraphEditor() {
  const { store, addQuads } = useStore();

  return (
    <div>
      <p>Store has {store.size} quads</p>
      <button onClick={() => addQuads([/* ... */])}>Add Quad</button>
    </div>
  );
}
```

## Core Hooks

### `useStore()`
Manage RDF store state with reactive updates.

### `useGraphs()`
Access and manage named graphs.

### `useKnowledgeHook(hookDef, deps)`
Register and execute knowledge hooks.

### `useTerms()`
Work with RDF terms and namespaces.

## Hook Categories (97 Total)

- **Core**: Store, graphs, terms, triples (core/)
- **Context**: KnowledgeEngineContext, providers (context/)
- **Knowledge Hooks**: Hook manager, execution, registry (knowledge-hooks/)
- **Dark Matter**: Optimization, query analysis (dark-matter/)
- **Federation**: Consensus, replication, distributed queries (federation/)
- **Streaming**: Change feeds, subscriptions, real-time validation (streaming/)
- **Error Recovery**: Error boundaries, recovery strategies (error-recovery/)
- **Query**: SPARQL, reasoning, validation (query/)
- **Caching**: Query cache, memoization (cache/)
- **Batch**: Batch operations, optimization (batch/)
- **AI Semantic**: Embeddings, anomaly detection (ai-semantic/)
- **Effects**: Lifecycle hooks (effects/)
- **Form UI**: Form validation, graph visualizer (form-ui/)
- **Policy Security**: Policy pack, sandbox, security (policy-security/)
- **Storage**: Audit trail, IndexedDB store (storage/)
- **HTF**: Hypothesis Testing Framework (htf/)
- **Composition**: Stack, offline store (composition/)
- **Advanced Utility**: Graph diff, merge, isomorphism (advanced-utility/)
- **Utils**: Debug, namespaces, validation (utils/)

## Documentation

For detailed documentation, see [React Integration Guide](../docs/how-to/use-hooks-in-react.md)

## Features

✅ Reactive Store Management
✅ Knowledge Hooks Integration
✅ Context API Integration
✅ Full TypeScript Support
✅ Query Caching
✅ Federation Support
✅ Error Handling
✅ Real-time Streaming
✅ Comprehensive Test Suite

## Examples

See [How-To: Use Knowledge Hooks in React](../docs/how-to/use-hooks-in-react.md) for complete examples including:
- Form validation with hooks
- Auto-save with knowledge hooks
- Permission checking
- Knowledge graph search
- Real-world patterns

## Testing

```bash
pnpm test
pnpm test:watch
```

## License

MIT

## Related Projects

- [UNRDF](https://github.com/seanchatmangpt/unrdf) - Core RDF library
- [UNRDF React Docs](../docs/how-to/use-hooks-in-react.md) - React integration guide
