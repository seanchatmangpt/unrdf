# Offline Support Example

This example demonstrates how to use `@unrdf/browser` with offline support, including operation queuing, network detection, and service worker integration.

## Features

- **Offline Operation Queuing**: Queue RDF operations when offline
- **Automatic Sync**: Sync queued operations when back online
- **Network Detection**: Detect online/offline status changes
- **Service Worker**: Cache app resources for offline use
- **Cached Queries**: Query RDF data while offline
- **Background Sync**: Sync data in the background when connectivity returns

## Installation

```bash
cd packages/browser/examples/offline-support
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

### Development Server

```bash
pnpm dev
```

### Build for Production

```bash
pnpm build
pnpm serve
```

## Code Examples

### Setup Offline Store

```javascript
import { setupOfflineSupport } from './src/index.mjs';

const store = await setupOfflineSupport({
  name: 'my-offline-store',
  version: 1
});
```

### Add Data (Online/Offline)

```javascript
import { quad, namedNode, literal } from '@unrdf/core';

// Works both online and offline
await store.add(quad(
  namedNode('http://example.org/user/1'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
));

// If offline, operation is queued
// If online, operation is executed immediately
```

### Check Network Status

```javascript
const status = handleNetworkDetection(store);
console.log(status.online); // true/false
console.log(status.queueSize); // Number of pending operations
```

### Manual Sync

```javascript
// Sync queued operations
const results = await store.syncQueuedOperations();
console.log(`Synced: ${results.synced}, Failed: ${results.failed}`);
```

### Cache Data for Offline Use

```javascript
const quads = [
  // ... your RDF quads
];

await cacheRDFData(store, quads);
```

### Query While Offline

```javascript
// Works even when offline (queries cached data)
const results = await store.match({
  predicate: namedNode('http://xmlns.com/foaf/0.1/name')
});
```

## Service Worker Integration

### Register Service Worker

```javascript
if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('/service-worker.mjs')
    .then(reg => console.log('Service Worker registered'))
    .catch(err => console.error('Registration failed:', err));
}
```

### Service Worker Features

- **Cache First Strategy**: Serves cached resources when offline
- **Background Sync**: Syncs queued data when connectivity returns
- **Cache Management**: Automatically cleans up old caches
- **Offline Fallback**: Serves offline page when resources unavailable

## Network Detection

The store automatically detects network changes:

```javascript
// Automatically triggered on online/offline events
window.addEventListener('online', () => {
  // Store automatically syncs queued operations
});

window.addEventListener('offline', () => {
  // Store switches to queue mode
});
```

## API Reference

### `OfflineRDFStore`

Main class for offline-capable RDF storage.

#### Methods

- `open()` - Open store connection
- `close()` - Close store connection
- `add(quad)` - Add quad (queues if offline)
- `remove(quad)` - Remove quad (queues if offline)
- `match(pattern)` - Query cached quads (works offline)
- `syncQueuedOperations()` - Sync pending operations
- `getNetworkStatus()` - Get current online/offline status
- `getQueueSize()` - Get number of queued operations
- `clearCache()` - Clear all cached data and queue

### Helper Functions

- `setupOfflineSupport(config)` - Initialize offline store
- `cacheRDFData(store, quads)` - Cache quads for offline use
- `handleNetworkDetection(store)` - Check network status

## Browser Compatibility

- Chrome/Edge: Full support (Service Workers + Background Sync)
- Firefox: Full support (Service Workers + Background Sync)
- Safari: Partial support (Service Workers only, no Background Sync)

## Best Practices

1. **Always Cache Critical Data**: Cache essential RDF data on first load
2. **Monitor Queue Size**: Alert users when queue grows large
3. **Periodic Sync**: Attempt sync periodically, not just on network change
4. **Error Handling**: Handle sync failures gracefully
5. **Storage Limits**: Monitor storage quota and warn users
6. **User Feedback**: Show clear indicators for online/offline state

## Troubleshooting

### Operations Not Syncing

- Check network status with `store.getNetworkStatus()`
- Manually trigger sync with `store.syncQueuedOperations()`
- Verify service worker is registered and active

### Service Worker Not Loading

- Ensure served over HTTPS (or localhost)
- Check browser console for registration errors
- Verify service worker file path is correct
- Clear browser cache and re-register

### Queue Growing Too Large

- Implement queue size limits
- Clear old operations after timeout
- Prioritize operations (sync important ones first)

### Queries Returning Stale Data

- Clear cache periodically with `store.clearCache()`
- Implement cache invalidation strategy
- Show "cached data" indicator to users

## Performance Tips

1. **Batch Operations**: Group multiple adds/removes before syncing
2. **Lazy Sync**: Don't sync on every network change, debounce sync calls
3. **Selective Caching**: Only cache frequently accessed data
4. **Compression**: Compress large RDF datasets before caching
5. **Incremental Sync**: Sync in batches, not all at once

## Security Considerations

- Service workers have full access to cached data
- Use HTTPS to prevent service worker hijacking
- Implement proper authentication for sync operations
- Don't cache sensitive data without encryption
- Validate all data before syncing to server

## Learn More

- [Service Worker API](https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API)
- [Background Sync API](https://developer.mozilla.org/en-US/docs/Web/API/Background_Sync_API)
- [Network Information API](https://developer.mozilla.org/en-US/docs/Web/API/Network_Information_API)
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
