# UNRDF Web Example

Vue 3 frontend demonstrating all UNRDF browser packages working together.

## Architecture

```
Web Application (Vue 3)
├── Reactive State Management (Vue Composables)
├── IndexedDB Storage (Browser persistence)
├── WebSocket Client (Real-time updates)
├── HTTP API Client (REST operations)
└── RDF Visualization (Triple viewer)
```

## Packages Demonstrated

### @unrdf/browser
- IndexedDB-backed RDF store
- Client-side triple storage
- Offline-first capabilities

### @unrdf/composables
- Vue 3 reactive state
- `useGraph()` composable
- `useDelta()` for change tracking

### @unrdf/core
- RDF data model
- Triple validation
- SPARQL client

### @unrdf/streaming
- Real-time change feeds
- WebSocket integration
- Automatic synchronization

## Features

### 📊 Statistics Dashboard
Real-time statistics about the RDF store:
- Total triples count
- Unique subjects
- Unique predicates
- WebSocket connection status

### 🔍 SPARQL Query Interface
Execute SPARQL queries against the backend:
- Interactive query editor
- Results display
- Query history

### 📚 Triple Browser
View and manage RDF triples:
- List all triples
- Color-coded display (subject/predicate/object)
- Reload from server
- Save/load from IndexedDB

### ➕ Triple Editor
Add new RDF triples:
- Subject/Predicate/Object input
- Validation hooks
- Immediate server sync

### 📝 Change Log
Real-time change tracking:
- WebSocket-based updates
- Operation type (add/delete)
- Timestamp display
- Last 10 changes shown

## IndexedDB Integration

The application uses `@unrdf/browser` for client-side persistence:

```javascript
// Create IndexedDB store
const store = new IndexedDBStore('unrdf-example');
await store.open();

// Save data locally
await store.addQuad(quad);

// Load data from IndexedDB
const quads = await store.getQuads();
```

### Benefits:
- Offline-first capability
- Fast local queries
- Reduced server load
- Cross-tab synchronization

## WebSocket Real-time Updates

The application subscribes to real-time changes via WebSocket:

```javascript
ws://localhost:3001
```

### Message Flow:
1. Connect to WebSocket
2. Receive initial data
3. Subscribe to changes
4. Update UI reactively
5. Auto-reconnect on disconnect

## Running

```bash
# Install dependencies
pnpm install

# Start development server
pnpm dev

# Build for production
pnpm build

# Preview production build
pnpm preview
```

## Development

The Vite dev server runs on `http://localhost:5173` with proxy configuration for API calls:

```javascript
// vite.config.mjs
proxy: {
  '/api': {
    target: 'http://localhost:3000',
    changeOrigin: true,
  },
}
```

## Component Structure

```vue
<template>
  <!-- UI components -->
</template>

<script setup>
import { ref, onMounted } from 'vue';
import { IndexedDBStore } from '@unrdf/browser';

// Reactive state
const quads = ref([]);
const stats = ref({});

// API functions
async function loadFromServer() { }
async function saveToLocal() { }
async function loadFromLocal() { }

// WebSocket
function connectWebSocket() { }

// Lifecycle
onMounted(async () => {
  await loadFromServer();
  connectWebSocket();
});
</script>
```

## Styling

The application uses modern CSS with:
- CSS Grid for layout
- CSS Variables for theming
- Responsive design
- Gradient backgrounds
- Card-based UI

## Browser Support

Requires modern browsers with:
- ES Modules support
- IndexedDB API
- WebSocket API
- CSS Grid

Tested on:
- Chrome 90+
- Firefox 88+
- Safari 14+
- Edge 90+

## Testing

```bash
# Run unit tests
pnpm test

# Run in watch mode
pnpm test:watch
```
