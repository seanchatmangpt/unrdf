# Service Worker Manager API Reference

Complete API documentation for the service worker manager module.

## Module: service-worker-manager

```javascript
import {
  registerServiceWorker,
  checkCrossOriginIsolation,
  getCOIStatus,
  waitForCOI,
} from '@unrdf/atomvm/service-worker-manager';
```

## Functions

### registerServiceWorker()

Registers the `coi-serviceworker` service worker to enable Cross-Origin-Isolation.

**Signature:**
```javascript
async registerServiceWorker(): Promise<boolean>
```

**Returns:**
- `Promise<boolean>` - Resolves to `true` if registration successful, `false` otherwise

**Behavior:**
1. Checks if service workers are supported
2. Imports `coi-serviceworker` (auto-registers)
3. Waits for service worker registration
4. Waits for activation if installing
5. Returns registration status

**Example:**
```javascript
const registered = await registerServiceWorker();
if (registered) {
  console.log('Service worker registered successfully');
} else {
  console.error('Service worker registration failed');
}
```

**Errors:**
- Logs warnings if service workers not supported
- Returns `false` on registration failure
- Does not throw (returns `false` instead)

### checkCrossOriginIsolation()

Checks if Cross-Origin-Isolation is currently enabled.

**Signature:**
```javascript
checkCrossOriginIsolation(): boolean
```

**Returns:**
- `boolean` - `true` if COI is enabled, `false` otherwise

**Detection Strategy:**
1. Checks `crossOriginIsolated` global property (if available)
2. Falls back to SharedArrayBuffer availability test
3. Returns `false` if neither available

**Example:**
```javascript
if (checkCrossOriginIsolation()) {
  console.log('Cross-Origin-Isolation is enabled');
  // Safe to use SharedArrayBuffer
} else {
  console.log('COI not enabled, page reload may be needed');
}
```

**Note:** This is a synchronous check. For async detection with reload handling, use `waitForCOI()`.

### getCOIStatus()

Gets detailed status of Cross-Origin-Isolation setup.

**Signature:**
```javascript
getCOIStatus(): {
  crossOriginIsolated: boolean;
  sharedArrayBufferAvailable: boolean;
  serviceWorkerSupported: boolean;
  headers: {
    coep: string;
    coop: string;
  };
}
```

**Returns:**
- Object with COI status details:
  - `crossOriginIsolated` (boolean) - Whether COI is enabled
  - `sharedArrayBufferAvailable` (boolean) - Whether SharedArrayBuffer is available
  - `serviceWorkerSupported` (boolean) - Whether service workers are supported
  - `headers` (object) - Header information:
    - `coep` (string) - COEP header value
    - `coop` (string) - COOP header value

**Example:**
```javascript
const status = getCOIStatus();
console.log('COI Status:', status);
// {
//   crossOriginIsolated: true,
//   sharedArrayBufferAvailable: true,
//   serviceWorkerSupported: true,
//   headers: {
//     coep: 'require-corp (via service worker)',
//     coop: 'same-origin (via service worker)'
//   }
// }

if (!status.crossOriginIsolated) {
  console.warn('COI not enabled, SharedArrayBuffer unavailable');
}
```

### waitForCOI(timeout)

Waits for Cross-Origin-Isolation to be enabled, with automatic page reload if needed.

**Signature:**
```javascript
async waitForCOI(timeout?: number): Promise<boolean>
```

**Parameters:**
- `timeout` (number, optional) - Timeout in milliseconds (default: 5000)

**Returns:**
- `Promise<boolean>` - Resolves to `true` if COI is enabled, `false` on timeout

**Behavior:**
1. Polls `checkCrossOriginIsolation()` every 100ms
2. If service worker is controlling but COI not enabled, triggers page reload
3. Returns `true` if COI becomes enabled
4. Returns `false` on timeout

**Note:** May trigger page reload. After reload, this function will not return (page reloads).

**Example:**
```javascript
// Wait up to 10 seconds for COI
const enabled = await waitForCOI(10000);
if (enabled) {
  console.log('COI enabled, ready to use SharedArrayBuffer');
} else {
  console.warn('COI timeout - may need manual reload');
}
```

**Use Cases:**
- Initial page load (wait for service worker activation)
- After service worker registration
- Before using SharedArrayBuffer

## Implementation Details

### Service Worker Registration Flow

1. **Check Support:**
   ```javascript
   if (!('serviceWorker' in navigator)) {
     return false;
   }
   ```

2. **Import coi-serviceworker:**
   ```javascript
   await import('coi-serviceworker');
   // coi-serviceworker auto-registers on import
   ```

3. **Wait for Registration:**
   ```javascript
   let registration = await navigator.serviceWorker.getRegistration();
   if (!registration) {
     await new Promise(resolve => setTimeout(resolve, 200));
     registration = await navigator.serviceWorker.getRegistration();
   }
   ```

4. **Wait for Activation:**
   ```javascript
   if (registration.installing) {
     await new Promise((resolve) => {
       registration.installing.addEventListener('statechange', (e) => {
         if (e.target.state === 'activated') {
           resolve();
         }
       });
     });
   }
   ```

### COI Detection Strategy

1. **Primary Check:**
   ```javascript
   if (typeof crossOriginIsolated !== 'undefined') {
     return crossOriginIsolated;
   }
   ```

2. **Fallback Check:**
   ```javascript
   if (typeof SharedArrayBuffer !== 'undefined') {
     try {
       new SharedArrayBuffer(1);
       return true;
     } catch (e) {
       return false;
     }
   }
   ```

3. **Default:**
   ```javascript
   return false;
   ```

## Error Handling

All functions are designed to be non-throwing:

- `registerServiceWorker()` - Returns `false` on error
- `checkCrossOriginIsolation()` - Returns `false` if not available
- `getCOIStatus()` - Returns status object with `false` values on failure
- `waitForCOI()` - Returns `false` on timeout

Errors are logged to console but do not throw exceptions.

## Related Documentation

- [How-To: Enable SharedArrayBuffer Support](../how-to/enable-sharedarraybuffer.md)
- [How-To: Debug Service Worker Issues](../how-to/debug-service-worker.md)
- [Explanation: Service Worker Strategy](../explanation/service-worker-strategy.md)


