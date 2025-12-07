# Cross-Origin-Isolation Explained

## What is Cross-Origin-Isolation?

Cross-Origin-Isolation (COI) is a browser security feature that isolates a page from cross-origin resources, enabling access to powerful APIs like `SharedArrayBuffer` that require additional security guarantees.

## Why is COI Needed?

### The SharedArrayBuffer Problem

`SharedArrayBuffer` allows JavaScript to share memory between threads, which is essential for:
- Multi-threaded WebAssembly
- High-performance computing
- Real-time collaboration features

However, `SharedArrayBuffer` can be used for timing attacks:
1. Attacker loads malicious page
2. Page creates SharedArrayBuffer
3. Attacker measures timing of memory access
4. Can infer data from other origins (Spectre attack)

### The Solution: Isolation

COI prevents timing attacks by:
- Isolating the page from cross-origin resources
- Requiring explicit opt-in for cross-origin access
- Enabling browser to provide security guarantees

## How COI Works

### Required Headers

COI requires two HTTP headers:

1. **Cross-Origin-Opener-Policy (COOP):** `same-origin`
   - Prevents cross-origin windows from accessing the page
   - Isolates the browsing context

2. **Cross-Origin-Embedder-Policy (COEP):** `require-corp`
   - Requires all resources to opt-in to cross-origin access
   - Blocks resources without proper CORS headers

### Browser Behavior

When both headers are present:
1. Browser sets `crossOriginIsolated = true`
2. `SharedArrayBuffer` becomes available
3. WebAssembly threading works
4. Some cross-origin features are restricted

### Verification

Check COI status:
```javascript
console.log('COI enabled:', crossOriginIsolated);
console.log('SharedArrayBuffer:', typeof SharedArrayBuffer !== 'undefined');
```

Both should be `true` when COI is enabled.

## The Service Worker Approach

### The Challenge

Setting HTTP headers from JavaScript is impossible:
- Headers must come from server
- Static hosting can't set headers
- Client-side JavaScript can't modify responses

### The Solution: Service Workers

Service workers can intercept requests and modify responses:
1. Service worker registers
2. Intercepts all fetch requests
3. Adds COOP/COEP headers to responses
4. Browser recognizes COI context

### How coi-serviceworker Works

The `coi-serviceworker` library:
1. Registers a service worker automatically
2. Intercepts all fetch requests
3. Adds required headers to responses
4. Enables COI without server configuration

**Code flow:**
```javascript
// Import triggers registration
import('coi-serviceworker');

// Service worker intercepts requests
self.addEventListener('fetch', (event) => {
  event.respondWith(
    fetch(event.request).then(response => {
      // Add headers
      const newHeaders = new Headers(response.headers);
      newHeaders.set('Cross-Origin-Opener-Policy', 'same-origin');
      newHeaders.set('Cross-Origin-Embedder-Policy', 'require-corp');
      return new Response(response.body, {
        status: response.status,
        headers: newHeaders,
      });
    })
  );
});
```

## Tradeoffs and Limitations

### Benefits

✅ **No server configuration needed**
- Works with static hosting
- No backend changes required
- Easy deployment

✅ **Automatic activation**
- Service worker handles everything
- Transparent to application code
- Works in development and production

### Limitations

⚠️ **Page reload required**
- Service worker activation needs reload
- First load may not have COI
- Automatic reload handles this

⚠️ **Cross-origin resource restrictions**
- Some resources may break
- Requires proper CORS headers
- Images, fonts, etc. need opt-in

⚠️ **HTTPS or localhost required**
- Service workers need secure context
- Can't use `http://0.0.0.0` or `http://127.0.0.1`
- Must use `http://localhost` or HTTPS

## Browser Compatibility

### Support Matrix

| Browser | Version | COI Support | SharedArrayBuffer |
|---------|---------|-------------|-------------------|
| Chrome  | 92+     | ✅          | ✅                |
| Edge    | 92+     | ✅          | ✅                |
| Firefox | 95+     | ✅          | ✅                |
| Safari  | 15.2+   | ✅          | ✅                |

### Detection

Check browser support:
```javascript
const supported = 
  'serviceWorker' in navigator &&
  typeof SharedArrayBuffer !== 'undefined';

console.log('COI supported:', supported);
```

## Security Considerations

### What COI Protects Against

- **Spectre attacks:** Timing attacks via shared memory
- **Cross-origin data leakage:** Prevents inference attacks
- **Side-channel attacks:** Isolates execution context

### What COI Doesn't Protect Against

- **XSS attacks:** Still vulnerable to script injection
- **CSRF attacks:** Still need CSRF tokens
- **Other web vulnerabilities:** COI is not a general security solution

### Best Practices

1. **Always verify COI before using SharedArrayBuffer:**
   ```javascript
   if (!crossOriginIsolated) {
     throw new Error('COI not enabled');
   }
   ```

2. **Handle COI failures gracefully:**
   ```javascript
   if (!crossOriginIsolated) {
     // Fallback behavior
     console.warn('COI not available, using fallback');
   }
   ```

3. **Test in multiple browsers:**
   - Different browsers may behave differently
   - Test COI activation flow
   - Verify SharedArrayBuffer availability

## Related Documentation

- [Service Worker Strategy Explanation](./service-worker-strategy.md)
- [Architecture Overview](./architecture.md)
- [How-To: Enable SharedArrayBuffer Support](../how-to/enable-sharedarraybuffer.md)

