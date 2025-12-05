# How To: Distribute Across Node.js and Browser

**Problem:** You want to run KGC 4D in both server and browser environments with the same code.

**Solution:** KGC 4D is isomorphic—it works in Node.js, browsers, and hybrid setups using `isomorphic-git` and `lightning-fs`.

## When to Use This

- **Full-stack apps**: Server-side store with browser sync
- **Offline-first**: Browser-based store that syncs to server
- **Client-side validation**: Browser-based query against cached data
- **Distributed systems**: Multiple clients sharing knowledge graphs
- **Mobile web**: React Native or mobile browsers

## Runtime Differences

| Feature | Node.js | Browser |
|---------|---------|---------|
| **Time** | Nanoseconds via `process.hrtime.bigint()` | Milliseconds (converted to ns) |
| **File system** | Native fs module | `lightning-fs` (virtual FS) |
| **Git** | `isomorphic-git` pure JS | `isomorphic-git` pure JS |
| **Performance** | Faster disk I/O | Slower (IndexedDB-backed) |

## Browser Setup

```javascript
// install.mjs
import { KGCStore } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import FS from 'lightning-fs';

// Lightning-fs provides a virtual file system in IndexedDB
const fs = new FS('kgc-demo');

// Initialize store
const store = new KGCStore();
console.log('✓ KGC Store ready in browser');

// Everything else works the same
const alice = dataFactory.namedNode('http://example.org/alice');
// ... rest of code
```

## Node.js Setup

```javascript
// server.mjs
import { KGCStore } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();
console.log('✓ KGC Store ready on server');

// Use native fs for snapshots
const git = new GitBackbone('./snapshots');
```

## Time Handling

Handle time differently in each runtime:

```javascript
import { now } from '@unrdf/kgc-4d';

// This function works in both Node.js and browser
function getCurrentTimestamp() {
  // Node.js: Nanoseconds via process.hrtime.bigint()
  // Browser: Milliseconds converted to nanoseconds
  return now();
}

// Normalized to nanoseconds in both environments
const timestamp = getCurrentTimestamp();
console.log('Timestamp (ns):', timestamp.toString());
```

**Important**: All timestamps are BigInt nanoseconds. Browser milliseconds are automatically converted.

## Client-Server Architecture

### Server (Node.js)

```javascript
// server.mjs
import express from 'express';
import { KGCStore, GitBackbone, freezeUniverse } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const app = express();
const store = new KGCStore();
const git = new GitBackbone('./server-snapshots');

// Endpoint: Get current state
app.get('/api/universe', (req, res) => {
  const query = `
    PREFIX ex: <http://example.org/>
    SELECT ?s ?p ?o
    WHERE {
      GRAPH <kgc:Universe> { ?s ?p ?o }
    }
  `;

  const quads = store.querySync(query);
  const nquads = quads
    .map(q => `${q.get('s').value} ${q.get('p').value} ${q.get('o').value} .`)
    .join('\n');

  res.setHeader('Content-Type', 'application/n-quads');
  res.send(nquads);
});

// Endpoint: Append event
app.post('/api/events', express.json(), async (req, res) => {
  const { eventType, payload, mutations } = req.body;

  try {
    const receipt = await store.appendEvent(
      { type: eventType, payload },
      mutations
    );

    res.json({ success: true, receipt });
  } catch (error) {
    res.status(400).json({ error: error.message });
  }
});

// Endpoint: Freeze snapshot
app.post('/api/freeze', async (req, res) => {
  const frozen = await freezeUniverse(store, git);

  res.json({
    snapshotId: frozen.snapshotId,
    hash: frozen.hash,
    timestamp: frozen.tNs.toString(),
  });
});

app.listen(3000, () => console.log('Server running on port 3000'));
```

### Client (Browser)

```javascript
// client.mjs
import { KGCStore } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();
const serverUrl = 'http://localhost:3000';

// Load initial state from server
async function syncWithServer() {
  const response = await fetch(`${serverUrl}/api/universe`);
  const nquads = await response.text();

  // Parse N-Quads and load into browser store
  // (Implementation depends on N3 parser)
  console.log('✓ Synced with server');
}

// Append event locally, then sync to server
async function createLocalEvent(eventType, payload, mutations) {
  // Create event locally
  const receipt = await store.appendEvent(
    { type: eventType, payload },
    mutations
  );

  // Sync to server
  const response = await fetch(`${serverUrl}/api/events`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ eventType, payload, mutations }),
  });

  if (response.ok) {
    console.log('✓ Event synced to server');
  }

  return receipt;
}

// Initialize
await syncWithServer();
```

## Real-Time Sync with Server-Sent Events

### Server

```javascript
app.get('/api/tether', (req, res) => {
  res.setHeader('Content-Type', 'text/event-stream');
  res.setHeader('Cache-Control', 'no-cache');

  // Send current state
  const state = getCurrentUniverse();
  res.write(`data: ${JSON.stringify(state)}\n\n`);

  // Send updates as events are appended
  const handleEvent = (event) => {
    res.write(`data: ${JSON.stringify(event)}\n\n`);
  };

  store.on('event-appended', handleEvent);

  req.on('close', () => {
    store.off('event-appended', handleEvent);
  });
});
```

### Client

```javascript
// Subscribe to server updates
const eventSource = new EventSource('http://localhost:3000/api/tether');

eventSource.onmessage = (event) => {
  const data = JSON.parse(event.data);

  // Apply update to local store
  store.appendEvent(data.event, data.mutations);

  console.log('✓ Received update from server');
};

eventSource.onerror = () => {
  console.error('Server connection lost');
};
```

## Offline-First Pattern

Handle network disconnection gracefully:

```javascript
class OfflineFirstStore {
  constructor() {
    this.localStore = new KGCStore();
    this.pendingEvents = [];
    this.isOnline = navigator.onLine;

    window.addEventListener('online', () => this.goOnline());
    window.addEventListener('offline', () => this.goOffline());
  }

  async appendEvent(event, mutations) {
    // Always append locally
    const receipt = await this.localStore.appendEvent(event, mutations);

    if (this.isOnline) {
      // Try to sync immediately
      await this.syncToServer(receipt);
    } else {
      // Queue for later sync
      this.pendingEvents.push({ receipt, event, mutations });
    }

    return receipt;
  }

  async goOnline() {
    console.log('Going online, syncing...');
    this.isOnline = true;

    // Sync all pending events
    for (const { event, mutations } of this.pendingEvents) {
      try {
        await this.syncToServer({ event, mutations });
      } catch (error) {
        console.error('Sync failed:', error);
      }
    }

    this.pendingEvents = [];
    console.log('✓ Sync complete');
  }

  goOffline() {
    console.log('Going offline, queueing updates');
    this.isOnline = false;
  }

  async syncToServer(data) {
    const response = await fetch('/api/events', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(data),
    });

    if (!response.ok) throw new Error('Sync failed');
  }
}
```

## Cross-Runtime Snapshot Verification

Verify snapshots across Node.js and browser:

```javascript
// Node.js: Create snapshot
const frozen = await freezeUniverse(nodeStore, git);

// Send to browser
fetch('/api/freeze', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify(frozen),
});

// Browser: Receive and verify
app.post('/api/freeze', async (req, res) => {
  const frozen = req.body;

  // Verify in browser (without Git access)
  // Browser can verify the hash against local copy
  const browserCopy = browserStore.querySync('SELECT ...');
  const hash = computeHash(browserCopy);

  if (hash === frozen.hash) {
    console.log('✓ Browser snapshot matches server');
  }

  res.json({ verified: true });
});
```

## Time Synchronization

Handle time drift between client and server:

```javascript
class SyncedTime {
  constructor(serverUrl) {
    this.serverUrl = serverUrl;
    this.clockOffset = BigInt(0);
    this.syncInterval = null;
  }

  async sync() {
    // Server returns its current time
    const serverResponse = await fetch(`${this.serverUrl}/api/time`);
    const { serverTime } = await serverResponse.json();

    // Calculate offset
    const clientTime = now();
    this.clockOffset = BigInt(serverTime) - clientTime;

    console.log(`Clock offset: ${this.clockOffset}ns`);
  }

  now() {
    // Return time adjusted for server clock
    return now() + this.clockOffset;
  }
}

// Initialize
const syncedTime = new SyncedTime('http://localhost:3000');
await syncedTime.sync();

// Use synced time for events
const timestamp = syncedTime.now();
```

## Build Configuration

### Vite (Browser)

```javascript
// vite.config.js
import { defineConfig } from 'vite';

export default defineConfig({
  optimizeDeps: {
    exclude: ['@unrdf/kgc-4d', 'isomorphic-git'],
  },
  ssr: {
    external: ['@unrdf/kgc-4d'],
  },
});
```

### webpack/esbuild (Browser)

Ensure these polyfills are available:
- `util` (Node.js utilities)
- `fs` (lightning-fs)
- `path` (path-browserify)

## Troubleshooting

**Q: "process.hrtime.bigint() is not defined" in browser**
A: Use `now()` function which handles both runtimes. Don't call `process.hrtime.bigint()` directly in browser code.

**Q: Git operations fail in browser**
A: Use `isomorphic-git` which works in browsers. Standard git CLI won't work in browsers.

**Q: Time is different between client and server**
A: Implement clock synchronization (see SyncedTime example above).

**Q: Browser store is too slow**
A: IndexedDB (lightning-fs backend) is slower than disk I/O. Consider:
- Using smaller stores
- Caching query results
- Limiting history on client

**Q: How do I debug isomorphic code?**
A: Use environment detection:

```javascript
const isNode = typeof process !== 'undefined' && process.versions?.node;
const isBrowser = typeof window !== 'undefined';

if (isNode) {
  console.log('Running on Node.js');
} else if (isBrowser) {
  console.log('Running in browser');
}
```

## Summary

- KGC 4D works identically in Node.js and browsers
- Use `isomorphic-git` for Git operations in both runtimes
- Handle time with `now()` function (automatically adjusted)
- Implement server syncing for client-server architectures
- Use offline-first patterns for resilient applications
- Always use `lightning-fs` in browsers, native fs in Node.js
