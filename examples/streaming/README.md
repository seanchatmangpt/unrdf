# Streaming Examples

**Real-time change feeds and synchronization examples.**

## Examples in this Directory

### basic-stream.mjs
**Real-time Change Feeds Basics**

Learn how to:
- Subscribe to quad additions and removals
- Process changes in real-time
- Implement change handlers
- Unsubscribe from streams

**Run it:**
```bash
node examples/streaming/basic-stream.mjs
```

**Expected output:**
```
Streaming started
Change detected: quad-added { subject, predicate, object }
Change detected: quad-removed { subject, predicate, object }
```

**Key concepts:**
- Change subscriptions
- Event handlers
- Stream lifecycle

### advanced-filters.mjs
**Stream Filtering and Transformation**

Learn how to:
- Filter changes by subject, predicate, or object
- Transform changes before processing
- Implement custom filters
- Batch change processing

**Run it:**
```bash
node examples/streaming/advanced-filters.mjs
```

**Use cases:**
- Selective change processing
- Change transformation pipelines
- Performance optimization

## Common Patterns

### Pattern 1: Subscribe to All Changes
```javascript
import { ChangeStream } from '@unrdf/streaming'

const stream = new ChangeStream(store)

stream.on('quad-added', (quad) => {
  console.log('New quad:', quad)
})

stream.on('quad-removed', (quad) => {
  console.log('Removed quad:', quad)
})
```

### Pattern 2: Filter by Predicate
```javascript
stream.on('quad-added', (quad) => {
  if (quad.predicate.value === 'http://xmlns.com/foaf/0.1/name') {
    console.log('Name changed:', quad.object.value)
  }
})
```

### Pattern 3: Batch Processing
```javascript
const batch = []

stream.on('quad-added', (quad) => {
  batch.push(quad)

  if (batch.length >= 100) {
    processBatch(batch)
    batch.length = 0
  }
})
```

## Integration with Other Packages

### With @unrdf/browser
```javascript
import { IndexedDBStore } from '@unrdf/browser'
import { ChangeStream } from '@unrdf/streaming'

const store = new IndexedDBStore('my-graph')
const stream = new ChangeStream(store)

// Sync changes to server
stream.on('quad-added', (quad) => {
  syncToServer(quad)
})
```

### With @unrdf/hooks
```javascript
import { defineHook } from '@unrdf/hooks'
import { ChangeStream } from '@unrdf/streaming'

defineHook('validate-name', {
  type: 'validate-before-write',
  check: (quad) => isValidName(quad)
})

const stream = new ChangeStream(store)

// Only valid changes are streamed
stream.on('quad-added', (quad) => {
  // Guaranteed valid by hook
})
```

## Troubleshooting

### Issue: Memory leak with long-running streams
**Solution:** Always unsubscribe when done:
```javascript
const unsubscribe = stream.on('quad-added', handler)

// Later
unsubscribe()
```

### Issue: Missing changes
**Solution:** Subscribe before making changes:
```javascript
// ✅ Right
const stream = new ChangeStream(store)
stream.on('quad-added', handler)
store.addQuad(quad) // Change detected

// ❌ Wrong
store.addQuad(quad) // Change missed
const stream = new ChangeStream(store)
```

## API Reference

See [@unrdf/streaming](../../packages/streaming/README.md) for complete API documentation.

## Next Steps

- Read [ARCHITECTURE.md](../ARCHITECTURE.md) for system design
- See [comprehensive-feature-test.mjs](../comprehensive-feature-test.mjs) for integration
- Explore [browser examples](../browser/) for client-side streaming
