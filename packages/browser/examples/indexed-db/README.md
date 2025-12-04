# IndexedDB Persistence Example

This example demonstrates how to use `@unrdf/browser` with IndexedDB for persistent RDF data storage in the browser.

## Features

- **IndexedDB Storage**: Persistent browser-based RDF quad storage
- **CRUD Operations**: Create, read, update, and delete RDF quads
- **Pattern Matching**: Query quads using subject/predicate/object/graph patterns
- **Storage Quota**: Check and monitor browser storage usage
- **Session Persistence**: Data persists across browser sessions and page reloads

## Installation

```bash
cd packages/browser/examples/indexed-db
pnpm install
```

## Usage

### Run the Example

```bash
node src/index.mjs
```

### Run Tests

```bash
pnpm test
```

### Watch Mode

```bash
pnpm test:watch
```

## Code Examples

### Create Store

```javascript
import { IndexedDBStore } from '@unrdf/browser';

const store = new IndexedDBStore({
  name: 'unrdf-example',
  version: 1,
  storeName: 'quads'
});

await store.open();
```

### Add Data

```javascript
import { quad, namedNode, literal } from '@unrdf/core';

const q = quad(
  namedNode('http://example.org/user/1'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
);

await store.add(q);
```

### Query Data

```javascript
// Query all quads
const allQuads = await store.match();

// Query by predicate
const nameQuads = await store.match(
  null,
  namedNode('http://xmlns.com/foaf/0.1/name'),
  null,
  null
);
```

### Check Storage Quota

```javascript
const estimate = await navigator.storage.estimate();
console.log(`Usage: ${estimate.usage} / ${estimate.quota} bytes`);
```

### Clear Data

```javascript
await store.clear();
```

## API Reference

### `createIndexedDBStore()`
Creates and initializes an IndexedDB store.

### `addSampleData(store)`
Adds sample FOAF (Friend of a Friend) data to the store.

### `queryQuads(store, pattern)`
Queries quads matching the given pattern.

### `verifyPersistence(store)`
Verifies that data persists across store close/reopen cycles.

### `checkStorageQuota()`
Checks browser storage quota and current usage.

### `clearAllData(store)`
Removes all quads from the store.

## Browser Compatibility

- Chrome/Edge: Full support (IndexedDB + Storage API)
- Firefox: Full support (IndexedDB + Storage API)
- Safari: Partial support (IndexedDB only, no Storage API)

## Storage Limits

IndexedDB storage limits vary by browser:

- **Chrome**: Up to 60% of total disk space
- **Firefox**: Up to 50% of total disk space
- **Safari**: Up to 1 GB (prompts user after 50 MB)

## Best Practices

1. **Error Handling**: Always wrap IndexedDB operations in try-catch blocks
2. **Quota Checks**: Monitor storage quota before large writes
3. **Cleanup**: Close store connections when done
4. **Indexing**: Use appropriate indexes for query patterns
5. **Transactions**: Use transactions for batch operations

## Troubleshooting

### "QuotaExceededError"
- Check available storage with `navigator.storage.estimate()`
- Clear old data with `store.clear()`
- Request persistent storage with `navigator.storage.persist()`

### Data Not Persisting
- Ensure `store.close()` is called before page unload
- Check that IndexedDB is not disabled in browser settings
- Verify no private/incognito mode restrictions

## Learn More

- [IndexedDB API](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API)
- [Storage API](https://developer.mozilla.org/en-US/docs/Web/API/Storage_API)
- [@unrdf/browser Documentation](../../README.md)


## Testing

Run the test suite:

```bash
pnpm test
```

Run tests in watch mode:

```bash
pnpm test:watch
```

Generate coverage report:

```bash
pnpm test:coverage
```

Test coverage: 80%+ (minimum requirement)
