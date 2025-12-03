# @unrdf/browser

**Browser SDK - Client-side RDF with IndexedDB Storage**

RDF operations in the browser. Persistent storage with IndexedDB, SPARQL execution, and sync.

## Installation

```bash
pnpm add @unrdf/browser
```

## Quick Start

```javascript
import { IndexedDBStore } from '@unrdf/browser'

// Create persistent store in browser
const store = new IndexedDBStore('my-graph')

// Use like normal RDF store
store.addQuad({
  subject: namedNode('http://example.com/alice'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
  object: literal('Alice')
})

// SPARQL queries work in browser
const results = await executeQuery(store, 'SELECT ?name WHERE { ?s foaf:name ?name }')
```

## Features

- ✅ IndexedDB persistent storage
- ✅ SPARQL query execution in browser
- ✅ Polyfills for Node.js APIs
- ✅ Offline-first architecture
- ✅ Automatic sync with server
- ✅ Service worker integration

## Use Cases

- **Progressive web apps**: Offline-capable RDF apps
- **Real-time collaboration**: Sync changes between clients
- **Local-first apps**: Store data locally, sync to server
- **Performance**: Avoid repeated server queries

## Documentation

- **[API Reference](./docs/API.md)** - Complete API documentation
- **[User Guide](./docs/GUIDE.md)** - Browser development guide
- **[Examples](./examples/)** - Code examples
- **[Contributing](./docs/CONTRIBUTING.md)** - How to contribute

## Depends On

- `@unrdf/core` - RDF substrate
- `@unrdf/streaming` - Change feeds

## VOC Usage

- VOC-6: App Developer (web apps with RDF)

## License

MIT
