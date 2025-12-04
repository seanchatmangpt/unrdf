# Browser Examples

**Client-side RDF storage and querying examples.**

## Examples in this Directory

### indexeddb-store.html
**IndexedDB Persistent Storage**

Learn how to:
- Create persistent RDF stores in the browser
- Store and retrieve quads from IndexedDB
- Execute SPARQL queries client-side
- Handle offline scenarios

**Run it:**
```bash
# Open in browser
open examples/browser/indexeddb-store.html
```

**Expected behavior:**
- Data persists across page reloads
- Queries execute entirely client-side
- No server required for basic operations

**Key concepts:**
- IndexedDB integration
- Persistent storage
- Offline-first architecture

### ../browser-react.jsx
**React Integration**

Learn how to:
- Use UNRDF in React applications
- Implement React hooks for RDF data
- Update UI on quad changes
- Manage RDF state in React

**Run it:**
```bash
cd examples/browser/react-example
npm install
npm start
```

**Key concepts:**
- React hooks
- State management
- Real-time UI updates

### ../browser-vue.vue
**Vue 3 Integration**

Learn how to:
- Use UNRDF in Vue 3 applications
- Implement Composition API patterns
- Reactive RDF data
- Vue component integration

**Run it:**
```bash
cd examples/browser/vue-example
npm install
npm run dev
```

**Key concepts:**
- Vue Composition API
- Reactive data
- Component patterns

## Common Patterns

### Pattern 1: Create Persistent Store
```javascript
import { IndexedDBStore } from '@unrdf/browser'

// Create or open existing store
const store = new IndexedDBStore('my-graph')

// Add data (persists automatically)
await store.addQuad({
  subject: namedNode('http://example.com/alice'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
  object: literal('Alice')
})

// Data survives page reload
```

### Pattern 2: Query Client-Side
```javascript
import { executeQuery } from '@unrdf/browser'

// SPARQL queries execute in browser
const results = await executeQuery(
  store,
  'SELECT ?name WHERE { ?s foaf:name ?name }'
)

// No server roundtrip
```

### Pattern 3: Sync with Server
```javascript
import { IndexedDBStore } from '@unrdf/browser'
import { ChangeStream } from '@unrdf/streaming'

const store = new IndexedDBStore('my-graph')
const stream = new ChangeStream(store)

// Push changes to server
stream.on('quad-added', async (quad) => {
  await fetch('/api/sync', {
    method: 'POST',
    body: JSON.stringify(quad)
  })
})

// Pull changes from server
const ws = new WebSocket('ws://server/changes')
ws.onmessage = (event) => {
  const quad = JSON.parse(event.data)
  store.addQuad(quad)
}
```

## React Patterns

### Pattern 1: useQuery Hook
```javascript
import { useQuery, useStore } from '@unrdf/composables'

function MyComponent() {
  const store = useStore()
  const results = useQuery(
    store,
    'SELECT ?name WHERE { ?s foaf:name ?name }'
  )

  return (
    <ul>
      {results.map((binding) => (
        <li key={binding.name}>{binding.name.value}</li>
      ))}
    </ul>
  )
}
```

### Pattern 2: useChangeStream Hook
```javascript
import { useChangeStream } from '@unrdf/composables'

function MyComponent() {
  const [changes, setChanges] = useState([])

  useChangeStream((change) => {
    setChanges((prev) => [...prev, change])
  })

  return <div>{changes.length} changes detected</div>
}
```

## Vue Patterns

### Pattern 1: Reactive Store
```javascript
import { reactive } from 'vue'
import { IndexedDBStore } from '@unrdf/browser'

export default {
  setup() {
    const store = reactive(new IndexedDBStore('my-graph'))

    const results = computed(() => {
      return executeQuery(store, sparql)
    })

    return { results }
  }
}
```

## Browser Compatibility

| Feature | Chrome | Firefox | Safari | Edge |
|---------|--------|---------|--------|------|
| IndexedDB | ✅ | ✅ | ✅ | ✅ |
| SPARQL | ✅ | ✅ | ✅ | ✅ |
| WebWorkers | ✅ | ✅ | ✅ | ✅ |
| ServiceWorkers | ✅ | ✅ | ✅ | ✅ |

**Minimum versions:**
- Chrome 87+
- Firefox 78+
- Safari 14+
- Edge 87+

## Storage Limits

| Browser | Limit | Notes |
|---------|-------|-------|
| Chrome | ~60% of disk space | Per origin |
| Firefox | ~50% of disk space | Per origin |
| Safari | ~1GB | Per origin, may prompt user |
| Edge | ~60% of disk space | Per origin |

**Best practices:**
- Monitor storage usage
- Implement cleanup strategies
- Handle quota exceeded errors
- Use compression for large datasets

## Troubleshooting

### Issue: IndexedDB not available
**Solution:** Check browser compatibility:
```javascript
if (!('indexedDB' in window)) {
  console.error('IndexedDB not supported')
  // Fallback to in-memory store
}
```

### Issue: Quota exceeded
**Solution:** Handle gracefully:
```javascript
try {
  await store.addQuad(quad)
} catch (error) {
  if (error.name === 'QuotaExceededError') {
    // Clean up old data or notify user
    await store.cleanup()
  }
}
```

### Issue: Stale data after page reload
**Solution:** Wait for store to initialize:
```javascript
const store = new IndexedDBStore('my-graph')
await store.ready() // Wait for IndexedDB to open

// Now safe to query
const results = await executeQuery(store, sparql)
```

## Security Considerations

1. **Same-origin policy**: IndexedDB is scoped to origin
2. **No encryption**: Data stored in plaintext
3. **User can delete**: IndexedDB can be cleared by user
4. **Quota limits**: Don't rely on unlimited storage

**Sensitive data:** Encrypt before storing:
```javascript
import { encrypt } from 'crypto'

const encryptedValue = encrypt(sensitiveData)
store.addQuad({
  subject,
  predicate,
  object: literal(encryptedValue)
})
```

## Performance Tips

1. **Batch operations**: Use transactions for multiple writes
2. **Index frequently queried predicates**: Faster lookups
3. **Lazy load data**: Don't load entire graph at once
4. **Use WebWorkers**: Offload heavy queries to worker threads
5. **Cache query results**: Avoid repeated SPARQL execution

## API Reference

See [@unrdf/browser](../../packages/browser/README.md) for complete API documentation.

## Next Steps

- Read [QUICKSTART.md](../QUICKSTART.md) for basics
- See [comprehensive-feature-test.mjs](../comprehensive-feature-test.mjs) for integration
- Explore [streaming examples](../streaming/) for real-time updates
