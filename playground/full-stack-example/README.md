# UNRDF Full-Stack Integration Example

A comprehensive demonstration of all UNRDF packages working together in a modern full-stack application.

## 🎯 Overview

This example implements a **Knowledge Graph Browser** that showcases:

- **Server-side RDF processing** with Node.js
- **Client-side RDF storage** with IndexedDB
- **Real-time synchronization** via WebSocket
- **Policy validation** with Knowledge Hooks
- **Distributed queries** with Federation
- **Reactive UI** with Vue 3 Composables

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    UNRDF Full-Stack Example                 │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌─────────────────────┐        ┌─────────────────────┐   │
│  │   Web Application   │◄──────►│  Server Application │   │
│  │     (Vue 3)         │  HTTP  │    (Node.js)        │   │
│  │                     │  WS    │                     │   │
│  │  @unrdf/browser     │        │  @unrdf/core        │   │
│  │  @unrdf/composables │        │  @unrdf/hooks       │   │
│  │  @unrdf/streaming   │        │  @unrdf/federation  │   │
│  │                     │        │  @unrdf/streaming   │   │
│  └─────────────────────┘        └─────────────────────┘   │
│           │                               │                │
│           │                               │                │
│      ┌────▼────┐                    ┌────▼────┐          │
│      │IndexedDB│                    │  N3.js  │          │
│      │  Store  │                    │  Store  │          │
│      └─────────┘                    └─────────┘          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 📦 Packages Demonstrated

### Server-Side (@unrdf/*)

| Package | Purpose | Features Shown |
|---------|---------|----------------|
| `@unrdf/core` | RDF operations | Triple storage, SPARQL execution, graph operations |
| `@unrdf/hooks` | Policy validation | Email validation, timestamp enrichment |
| `@unrdf/federation` | Distributed queries | Peer discovery, cross-node coordination |
| `@unrdf/streaming` | Change tracking | Real-time notifications, WebSocket integration |
| `@unrdf/cli` | Graph management | Command-line utilities |
| `@unrdf/knowledge-engine` | Inference | Reasoning capabilities |
| `@unrdf/dark-matter` | Optimization | Query performance enhancement |

### Client-Side (@unrdf/*)

| Package | Purpose | Features Shown |
|---------|---------|----------------|
| `@unrdf/browser` | IndexedDB storage | Client-side persistence, offline capability |
| `@unrdf/composables` | Vue 3 reactivity | Reactive graph state, change tracking |
| `@unrdf/streaming` | Real-time sync | WebSocket client, auto-updates |

## 🚀 Quick Start

### Prerequisites

- Node.js 18+
- pnpm 8+

### Installation

```bash
# Navigate to example directory
cd playground/full-stack-example

# Install all dependencies
pnpm install
```

### Running

```bash
# Start both server and web (recommended)
pnpm dev

# Or start individually:
pnpm dev:server  # Start server on http://localhost:3000
pnpm dev:web     # Start web on http://localhost:5173
```

### Access

- **Web UI**: http://localhost:5173
- **API**: http://localhost:3000
- **WebSocket**: ws://localhost:3001

## 📚 Features Walkthrough

### 1. Statistics Dashboard

Real-time statistics about the RDF store:

```
Total Triples: 9
Unique Subjects: 3
Unique Predicates: 4
WebSocket: ✓ Connected
```

### 2. SPARQL Query Interface

Execute queries against the backend:

```sparql
SELECT * WHERE {
  ?person <http://xmlns.com/foaf/0.1/name> ?name
} LIMIT 10
```

### 3. Triple Browser

View and manage RDF triples with actions:

- **🔄 Reload from Server**: Fetch latest data
- **💾 Save to IndexedDB**: Persist locally
- **📂 Load from IndexedDB**: Load local copy

### 4. Add New Triple

Interactive form to add RDF triples:

```
Subject:   http://example.org/Person4
Predicate: http://xmlns.com/foaf/0.1/name
Object:    David Miller
```

Validation hooks automatically check email format!

### 5. Real-time Change Log

Watch changes happen in real-time:

```
ADD    14:32:45
  http://example.org/Person4
  http://xmlns.com/foaf/0.1/name
  "David Miller"
```

## 🔧 Technical Details

### Server Architecture

**HTTP API** (`/api/...`)
- `GET /api/quads` - Retrieve all triples
- `GET /api/query` - Execute SPARQL
- `POST /api/quads` - Add new triple
- `GET /api/stats` - Get statistics

**WebSocket Server** (`ws://localhost:3001`)
- Initial data broadcast
- Change notifications
- Auto-reconnect support

**Knowledge Hooks**
```javascript
// Email validation hook
hookManager.define({
  name: 'validateEmail',
  type: 'validation',
  trigger: 'before:add',
  handler: async (context) => {
    // Validate email format
    if (quad.predicate === 'foaf:mbox') {
      // Check format...
    }
    return { valid: true };
  }
});

// Timestamp enrichment hook
hookManager.define({
  name: 'addTimestamp',
  type: 'transformation',
  trigger: 'after:add',
  handler: async (context) => {
    // Add timestamp quad
    store.addQuad({
      subject: quad.subject,
      predicate: 'ex:addedAt',
      object: new Date().toISOString()
    });
  }
});
```

### Client Architecture

**Vue 3 Reactive State**
```javascript
const quads = ref([]);
const stats = ref({});
const wsConnected = ref(false);
```

**IndexedDB Integration**
```javascript
// @unrdf/browser
const localStore = new IndexedDBStore('unrdf-example');
await localStore.open();

// Save locally
await localStore.addQuad(quad);

// Load from local
const quads = await localStore.getQuads();
```

**WebSocket Real-time**
```javascript
ws = new WebSocket('ws://localhost:3001');

ws.onmessage = (event) => {
  const message = JSON.parse(event.data);

  if (message.type === 'change') {
    // Update UI reactively
    changes.value.push(message);
    loadFromServer();
  }
};
```

## 🧪 Testing

```bash
# Run all tests
pnpm test

# Test server
pnpm -C apps/server test

# Test web
pnpm -C apps/web test
```

## 🔍 Use Cases

### 1. Offline-First Knowledge Graph

1. Load data from server
2. Save to IndexedDB
3. Work offline
4. Sync when back online

### 2. Collaborative Editing

1. Multiple clients connect
2. Changes propagate via WebSocket
3. Real-time updates in all browsers
4. Conflict resolution via hooks

### 3. Policy-Driven Validation

1. Define validation hooks
2. Enforce data quality
3. Transform data automatically
4. Audit trail via change log

### 4. Distributed Queries

1. Federation coordinator discovers peers
2. Query spans multiple nodes
3. Results aggregated
4. Performance optimized

## 📁 Directory Structure

```
full-stack-example/
├── package.json                  # Workspace root
├── README.md                     # This file
├── apps/
│   ├── server/                   # Node.js backend
│   │   ├── package.json
│   │   ├── src/
│   │   │   └── index.mjs        # Server entry point
│   │   └── README.md
│   └── web/                      # Vue 3 frontend
│       ├── package.json
│       ├── vite.config.mjs
│       ├── index.html
│       ├── src/
│       │   ├── main.mjs         # Web entry point
│       │   └── App.vue          # Main component
│       └── README.md
└── pnpm-workspace.yaml           # Workspace config
```

## 🎓 Learning Path

1. **Start with Server** (`apps/server/README.md`)
   - Understand RDF storage
   - Learn Knowledge Hooks
   - Explore Federation

2. **Move to Client** (`apps/web/README.md`)
   - IndexedDB integration
   - Vue composables
   - Real-time updates

3. **Integrate Both**
   - HTTP API calls
   - WebSocket sync
   - State management

## 🐛 Debugging

### Server Logs

```bash
# Watch server logs
pnpm dev:server

# Look for:
📊 Loaded 9 triples into RDF store
🔗 Knowledge Hooks configured
🌊 WebSocket server listening on port 3001
🌐 Federation coordinator initialized
```

### Client Logs

```bash
# Open browser console
# Look for:
✅ Loaded 9 triples from server
🔌 WebSocket connected
✅ Query returned 10 results
```

### Common Issues

**WebSocket not connecting?**
- Check server is running on port 3001
- Verify firewall settings
- Check browser console for errors

**IndexedDB not saving?**
- Check browser storage permissions
- Clear IndexedDB in DevTools
- Verify `@unrdf/browser` installed

**API calls failing?**
- Verify server running on port 3000
- Check Vite proxy configuration
- Review CORS headers

## 🚀 Next Steps

### Extend the Example

1. **Add Authentication**
   - User login/logout
   - Scoped RDF graphs
   - Permission hooks

2. **Enhanced Visualization**
   - Graph visualization library
   - Force-directed layout
   - Interactive exploration

3. **Advanced Queries**
   - SPARQL query builder UI
   - Query history and favorites
   - Query optimization hints

4. **Federation UI**
   - Peer discovery interface
   - Node health monitoring
   - Distributed query execution

5. **Performance Monitoring**
   - Query timing
   - WebSocket latency
   - IndexedDB performance

## 📖 Resources

- [UNRDF Documentation](https://github.com/unrdf/unrdf)
- [RDF Primer](https://www.w3.org/TR/rdf-primer/)
- [SPARQL Tutorial](https://www.w3.org/TR/sparql11-query/)
- [Vue 3 Guide](https://vuejs.org/guide/)
- [IndexedDB API](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API)

## 🤝 Contributing

This example is part of the UNRDF project. Contributions welcome!

## 📄 License

MIT License - see [LICENSE](../../LICENSE)
