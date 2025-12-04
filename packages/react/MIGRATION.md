# UNRDF React - Migration Guide

## Overview

`unrdf-react` is now a standalone npm package extracted from the UNRDF core repository. This migration guide explains how to use it in your projects.

## What's Included

- **97 React hooks** organized in 20 categories
- **18 comprehensive test files** 
- **Full TypeScript support** with JSDoc
- **Context-based state management**
- **Knowledge Hooks integration**

## Hook Categories

### Core (7 hooks)
- `useStore()` - RDF store state management
- `useGraphs()` - Named graph management
- `useTerms()` - RDF terms and namespaces
- `useTriples()` - Triple management
- `useKnowledgeEngine()` - Main engine hook
- `useModulePreloader()` - Module preloading

### Context (4 hooks)
- `KnowledgeEngineProvider` - Context provider
- `useKnowledgeEngineContext()` - Access context
- `useConfigContext()` - Configuration access

### Knowledge Hooks (4 hooks)
- `useKnowledgeHook()` - Hook definition & execution
- `useHookManager()` - Hook management
- `useHookExecution()` - Hook execution control
- `useHookRegistry()` - Hook registry access

### Federation (5 hooks)
- `useFederatedSystem()` - Federated system
- `useConsensusManager()` - Consensus coordination
- `useDataReplication()` - Data replication
- `useDistributedQuery()` - Distributed queries
- `useFederationHealth()` - Health monitoring

### Streaming (6 hooks)
- `useChangeFeed()` - Real-time change feed
- `useSubscriptionManager()` - Subscription management
- `useStreamProcessor()` - Stream processing
- `useRealTimeValidator()` - Real-time validation
- `useStreamingPipeline()` - Complete pipeline

### Error Recovery (3 hooks)
- `useErrorBoundary()` - Error boundary control
- `useRecovery()` - Error recovery
- `useErrorReporting()` - Error reporting

### Query (5 hooks)
- `useSPARQLQuery()` - SPARQL queries
- `useQueryAsync()` - Async queries
- `useDeltaQuery()` - Delta queries
- `useReasoning()` - Reasoning
- `useShapeValidation()` - SHACL validation

### Caching (3 hooks)
- `useQueryCache()` - Query caching
- `useMemoizedQuery()` - Memoized queries
- `useCacheStats()` - Cache statistics

### Dark Matter (4 hooks)
- `useDarkMatterCore()` - Dark Matter core
- `useCriticalPath()` - Critical path analysis
- `useOptimizer()` - Query optimization
- `useQueryAnalyzer()` - Query analysis

### Storage (4 hooks)
- `useTransaction()` - Transaction management
- `useQuadStore()` - Quad storage
- `useIndexedDBStore()` - IndexedDB storage
- `useAuditTrail()` - Audit trail tracking

### And 10 more categories...

## Installation

```bash
npm install unrdf-react
# or
pnpm add unrdf-react
```

## Basic Usage

### Setup

```javascript
import { KnowledgeEngineProvider } from 'unrdf-react';

function App() {
  return (
    <KnowledgeEngineProvider>
      <YourComponent />
    </KnowledgeEngineProvider>
  );
}
```

### Use Hooks

```javascript
import { useStore, useGraphs } from 'unrdf-react';

function MyComponent() {
  const { store, addQuads } = useStore();
  const { graphs } = useGraphs();

  return (
    <div>
      <p>Quads: {store.size}</p>
      <p>Graphs: {graphs.length}</p>
    </div>
  );
}
```

## Integration with UNRDF

`unrdf-react` is a peer dependency of the main UNRDF package. When using React:

```javascript
import { defineHook } from 'unrdf';
import { useKnowledgeHook } from 'unrdf-react';

function App() {
  const hook = defineHook({ /* ... */ });
  const { execute } = useKnowledgeHook(hook);
  
  return (/* ... */);
}
```

## Examples

See the main UNRDF documentation for examples:
- [React Integration Guide](../docs/how-to/use-hooks-in-react.md)
- [Knowledge Hooks in React](../docs/how-to/use-hooks-in-react.md)

## Package Structure

```
unrdf-react/
├── src/react-hooks/
│   ├── core/              # Core hooks
│   ├── context/           # Context providers
│   ├── knowledge-hooks/   # Hook integration
│   ├── federation/        # Multi-store support
│   ├── streaming/         # Real-time updates
│   ├── error-recovery/    # Error handling
│   ├── dark-matter/       # Optimization
│   ├── query/             # Query utilities
│   ├── caching/           # Query caching
│   ├── storage/           # Storage integration
│   ├── batch/             # Batch operations
│   ├── ai-semantic/       # AI features
│   ├── form-ui/           # Form integration
│   ├── policy-security/   # Security
│   ├── composition/       # Hook composition
│   ├── effects/           # Side effects
│   ├── advanced-utility/  # Advanced features
│   ├── htf/               # HTF support
│   ├── utils/             # Utilities
│   └── index.mjs          # Main export
├── test/                  # Test suite
├── package.json
└── README.md
```

## Testing

```bash
npm test              # Run tests
npm run test:watch    # Watch mode
npm run test:ui       # UI mode
```

## Breaking Changes

None - this is the initial release of `unrdf-react` as a standalone package.

## Migration Path (from UNRDF Core)

If you were using React hooks from UNRDF core:

Before:
```javascript
import { useStore } from 'unrdf';
```

After:
```javascript
import { useStore } from 'unrdf-react';
```

Both UNRDF core and `unrdf-react` continue to work, but it's recommended to use the dedicated React package.

## Support

See the main UNRDF repository for issues and documentation.
