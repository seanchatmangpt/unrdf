# @unrdf/composables

**Vue 3 Composables for Reactive RDF State** *(Optional Extension)*

Build reactive RDF web applications with Vue 3. Composables for RDF graphs, changes, and queries.

## Installation

```bash
pnpm add @unrdf/composables
```

## Quick Start

```javascript
import { useGraph, useDelta } from '@unrdf/composables'

export default {
  setup() {
    // Reactive graph state
    const { store, loading, error } = useGraph('http://api/graph')

    // Track changes for undo/redo
    const { deltas, push, undo, redo } = useDelta(store)

    return { store, loading, error, push, undo, redo }
  }
}
```

## Features

- ✅ `useGraph` - Reactive RDF store
- ✅ `useDelta` - Change tracking and undo/redo
- ✅ `useQuery` - Reactive SPARQL queries
- ✅ `useTerms` - RDF term helpers
- ✅ `useValidator` - Validation composables
- ✅ Real-time updates via streaming

## Use Cases

- **RDF web apps**: Build Vue 3 apps with RDF data
- **Data editors**: Build RDF data editors
- **Knowledge explorers**: Browse RDF graphs interactively
- **Semantic UIs**: Build UIs from RDF schemas

## Documentation

- **[API Reference](./docs/API.md)** - Complete API documentation
- **[User Guide](./docs/GUIDE.md)** - Vue 3 development guide
- **[Examples](./examples/)** - Code examples
- **[Contributing](./docs/CONTRIBUTING.md)** - How to contribute

## Status

**Optional Extension** - Web applications only.

## Depends On

- `@unrdf/core` - RDF substrate
- `@unrdf/browser` - Browser SDK
- `@unrdf/streaming` - Real-time updates
- `vue@^3.0.0` - Vue 3 (peer dependency)

## VOC Usage

- VOC-6: App Developer (Vue apps)

## License

MIT
