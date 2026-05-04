# How-To: Enable SharedArrayBuffer Support

## Problem

You're getting errors like "SharedArrayBuffer not available" or "COI not properly configured" when trying to use AtomVM in the browser.

## Solution

SharedArrayBuffer requires Cross-Origin-Isolation (COI), which is enabled via service workers. The AtomVM package handles this automatically, but you may need to troubleshoot.

## Quick Fix

1. **Hard reload the page:**
   - Chrome/Edge: `Ctrl+Shift+R` (Windows/Linux) or `Cmd+Shift+R` (Mac)
   - Firefox: `Ctrl+F5` or `Cmd+Shift+R`
   - Safari: `Cmd+Option+R`

2. **Check service worker status:**
   - Open DevTools (F12)
   - Go to Application → Service Workers
   - Verify service worker is registered and active

3. **Verify COI is enabled:**
   - Open browser console
   - Run: `console.log(crossOriginIsolated)`
   - Should be `true`

## Detailed Steps

### Step 1: Verify Service Worker Registration

Open DevTools → Application → Service Workers. You should see:
- Service worker registered
- Status: "activated and is running"
- Scope: matches your page URL

**If not registered:**
1. Check browser console for errors
2. Ensure you're on `http://localhost` (not `0.0.0.0` or `127.0.0.1`)
3. Clear service workers: Application → Clear storage → Clear site data
4. Reload page

### Step 2: Check Cross-Origin-Isolation

In browser console:
```javascript
console.log('COI:', crossOriginIsolated);
console.log('SharedArrayBuffer:', typeof SharedArrayBuffer !== 'undefined');
```

Both should be `true`.

**If COI is false:**
- Service worker may not be controlling the page
- Hard reload the page (service worker activation requires reload)
- Check for mixed content (HTTPS required for SharedArrayBuffer)

### Step 3: Verify Headers

Check that COOP/COEP headers are present:
1. Open DevTools → Network tab
2. Reload page
3. Click on the main document request
4. Check Response Headers:
   - `Cross-Origin-Opener-Policy: same-origin`
   - `Cross-Origin-Embedder-Policy: require-corp`

**If headers missing:**
- Service worker may not be intercepting requests
- Check service worker registration
- Verify `coi-serviceworker` is installed: `pnpm list coi-serviceworker`

### Step 4: Test SharedArrayBuffer

Try creating a SharedArrayBuffer:
```javascript
try {
  const buffer = new SharedArrayBuffer(1024);
  console.log('✅ SharedArrayBuffer works!', buffer.byteLength);
} catch (e) {
  console.error('❌ SharedArrayBuffer failed:', e);
}
```

**If it fails:**
- COI is not properly enabled
- Follow steps above to fix service worker
- Ensure you're using HTTPS or `localhost`

## Common Issues

### Issue: Service Worker Not Registering

**Symptoms:** No service worker in DevTools → Application → Service Workers

**Solutions:**
1. Check browser support: `'serviceWorker' in navigator` should be `true`
2. Verify `coi-serviceworker` is installed: `pnpm install`
3. Check console for import errors
4. Clear browser cache and reload

### Issue: COI Still False After Reload

**Symptoms:** `crossOriginIsolated` is `false` even after reload

**Solutions:**
1. Wait a few seconds after reload (service worker needs time to activate)
2. Check service worker is controlling: `navigator.serviceWorker.controller` should not be `null`
3. Verify headers are being set (Network tab)
4. Try incognito/private window (eliminates cache issues)

### Issue: Mixed Content Warnings

**Symptoms:** Console shows mixed content warnings

**Solutions:**
1. Use `http://localhost` instead of `http://0.0.0.0`
2. For production, use HTTPS
3. Check all resources are same-origin or have proper CORS headers

## Verification

After following these steps, verify everything works:

```javascript
// Run in browser console
const checks = {
  serviceWorker: 'serviceWorker' in navigator,
  controller: !!navigator.serviceWorker.controller,
  coi: crossOriginIsolated,
  sab: typeof SharedArrayBuffer !== 'undefined',
};

console.table(checks);

// All should be true
if (Object.values(checks).every(v => v === true)) {
  console.log('✅ All checks passed! SharedArrayBuffer is available.');
} else {
  console.error('❌ Some checks failed. See table above.');
}
```

## Related Documentation

- [Tutorial: Getting Started](../tutorials/01-getting-started.md) - Learn the basics
- [How-To: Debug Service Worker Issues](./debug-service-worker.md) - More troubleshooting
- [Explanation: Cross-Origin-Isolation](../explanation/cross-origin-isolation.md) - Understand why COI is needed


