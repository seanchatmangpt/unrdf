# @unrdf/streaming - Quick Start Guide

**80/20 Guide**: Real-time RDF change feeds in 5 minutes.

## Quick Start

### 1. Install

```bash
pnpm add @unrdf/streaming
```

### 2. Create Change Feed

```javascript
import { createChangeFeed } from '@unrdf/streaming'

const feed = createChangeFeed({
  store: myStore,
  filter: quad => quad.predicate.value.includes('foaf:name')
})

// Subscribe to changes
feed.subscribe(change => {
  console.log('Change detected:', change)
})
```

### 3. Real-Time Sync

```javascript
import { createSyncManager } from '@unrdf/streaming'

const sync = createSyncManager({
  localStore: localStore,
  remoteEndpoint: 'ws://example.com/sync'
})

// Start synchronization
await sync.start()

// Changes propagate automatically
localStore.add(quad(...))
// → Remote receives change
```

### 4. WebSocket Server

```javascript
import { createStreamingServer } from '@unrdf/streaming'

const server = createStreamingServer({
  port: 8080,
  store: myStore
})

server.listen(() => {
  console.log('Streaming server listening on :8080')
})
```

### 5. Subscribe to Changes

```javascript
// Client-side
import WebSocket from 'ws'

const ws = new WebSocket('ws://localhost:8080/changes')

ws.on('message', data => {
  const change = JSON.parse(data)
  console.log('Change received:', change.operation, change.quad)
})
```

## Change Feed Events

```javascript
feed.on('add', quad => {
  console.log('Triple added:', quad)
})

feed.on('delete', quad => {
  console.log('Triple deleted:', quad)
})

feed.on('update', (oldQuad, newQuad) => {
  console.log('Triple updated')
})
```

## Use Cases

**Real-Time Dashboard**:
```javascript
const feed = createChangeFeed({ store })
feed.subscribe(change => {
  updateDashboard(change)
})
```

**Multi-User Collaboration**:
```javascript
const sync = createSyncManager({
  localStore,
  peers: ['ws://peer1', 'ws://peer2']
})

await sync.start()
// All peers stay synchronized
```

**Audit Trail**:
```javascript
feed.subscribe(change => {
  auditLog.write({
    timestamp: Date.now(),
    operation: change.operation,
    quad: change.quad
  })
})
```

## Support

- Issues: https://github.com/unrdf/unrdf/issues
- Examples: See [examples/](./examples/)
