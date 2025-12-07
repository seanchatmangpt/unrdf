# How-To: Debug Service Worker Issues

## Problem

Service worker isn't registering, activating, or intercepting requests properly. This prevents Cross-Origin-Isolation from working.

## Solution

Systematically debug service worker registration, activation, and request interception.

## Quick Diagnosis

Run this in browser console:

```javascript
const diagnostics = {
  supported: 'serviceWorker' in navigator,
  controller: !!navigator.serviceWorker.controller,
  registrations: null,
};

navigator.serviceWorker.getRegistrations().then(regs => {
  diagnostics.registrations = regs.length;
  console.table(diagnostics);
});
```

**Expected:**
- `supported`: `true`
- `controller`: `true` (after reload)
- `registrations`: `1` or more

## Step 1: Check Service Worker Support

Verify browser supports service workers:

```javascript
if ('serviceWorker' in navigator) {
  console.log('✅ Service workers supported');
} else {
  console.error('❌ Service workers not supported');
  // Use a modern browser (Chrome 92+, Firefox 95+, Safari 15.2+)
}
```

**If not supported:** Update your browser or use a supported browser.

## Step 2: Check Registration Status

Open DevTools → Application → Service Workers. You should see:

- **Registered service worker**
- **Status:** "activated and is running"
- **Scope:** Matches your page URL

**If not registered:**

1. **Check console for errors:**
   ```javascript
   navigator.serviceWorker.getRegistration().then(reg => {
     if (reg) {
       console.log('Registration found:', reg);
     } else {
       console.error('No registration found');
     }
   });
   ```

2. **Check for import errors:**
   - Open Network tab
   - Look for failed requests to `coi-serviceworker`
   - Verify `node_modules/coi-serviceworker` exists

3. **Clear and re-register:**
   ```javascript
   // Unregister all
   navigator.serviceWorker.getRegistrations().then(regs => {
     regs.forEach(reg => reg.unregister());
   });
   
   // Reload page to re-register
   location.reload();
   ```

## Step 3: Verify Service Worker Activation

Service worker must be **activated** to intercept requests:

```javascript
navigator.serviceWorker.ready.then(registration => {
  console.log('Service worker ready:', registration.active);
  
  if (registration.active) {
    console.log('✅ Service worker is active');
  } else {
    console.error('❌ Service worker not active');
  }
});
```

**If not activating:**

1. **Check service worker script:**
   - Open DevTools → Sources
   - Find service worker script
   - Check for syntax errors

2. **Force activation:**
   ```javascript
   navigator.serviceWorker.getRegistration().then(reg => {
     if (reg.waiting) {
       reg.waiting.postMessage({ type: 'skipWaiting' });
     }
   });
   ```

3. **Check for errors in service worker:**
   - DevTools → Application → Service Workers
   - Click on service worker
   - Check "Error" section

## Step 4: Verify Request Interception

Service worker must intercept requests to add COOP/COEP headers:

1. **Open Network tab** in DevTools
2. **Reload page**
3. **Click on main document request**
4. **Check Response Headers:**
   - `Cross-Origin-Opener-Policy: same-origin`
   - `Cross-Origin-Embedder-Policy: require-corp`

**If headers missing:**

1. **Check service worker is controlling:**
   ```javascript
   console.log('Controller:', navigator.serviceWorker.controller);
   ```
   Should not be `null`.

2. **Verify service worker scope:**
   - Service worker scope must match page origin
   - Check scope in DevTools → Application → Service Workers

3. **Check fetch event listener:**
   - Service worker must have `fetch` event listener
   - `coi-serviceworker` handles this automatically

## Step 5: Debug coi-serviceworker

If `coi-serviceworker` isn't working:

1. **Verify installation:**
   ```bash
   cd packages/atomvm
   pnpm list coi-serviceworker
   ```
   Should show version `^0.1.7` or similar.

2. **Check import:**
   ```javascript
   // In browser console
   import('coi-serviceworker').then(() => {
     console.log('✅ coi-serviceworker loaded');
   }).catch(err => {
     console.error('❌ Failed to load:', err);
   });
   ```

3. **Check service worker file:**
   - DevTools → Application → Service Workers
   - Click on service worker
   - Check "Source" - should show `coi-serviceworker` code

## Step 6: Common Issues and Fixes

### Issue: Service Worker Stuck in "Installing"

**Fix:**
```javascript
// Force skip waiting
navigator.serviceWorker.getRegistration().then(reg => {
  if (reg.waiting) {
    reg.waiting.postMessage({ type: 'skipWaiting' });
    reg.waiting.addEventListener('statechange', () => {
      if (reg.waiting.state === 'activated') {
        location.reload();
      }
    });
  }
});
```

### Issue: Service Worker Not Intercepting

**Fix:**
1. Ensure service worker is **controlling** the page
2. Check scope matches page origin
3. Verify `fetch` event listener is registered
4. Hard reload page (Ctrl+Shift+R)

### Issue: Multiple Service Workers

**Fix:**
```javascript
// Unregister all
navigator.serviceWorker.getRegistrations().then(regs => {
  regs.forEach(reg => reg.unregister());
  console.log(`Unregistered ${regs.length} service workers`);
});

// Reload to register fresh
location.reload();
```

### Issue: Service Worker Updates Not Applying

**Fix:**
1. Clear browser cache
2. Hard reload (Ctrl+Shift+R)
3. Unregister old service worker
4. Reload page

## Verification Checklist

After debugging, verify:

- [ ] Service worker is registered
- [ ] Service worker is activated
- [ ] Service worker is controlling the page
- [ ] COOP/COEP headers are present
- [ ] `crossOriginIsolated` is `true`
- [ ] `SharedArrayBuffer` is available
- [ ] No errors in console
- [ ] No errors in service worker

## Related Documentation

- [How-To: Enable SharedArrayBuffer Support](./enable-sharedarraybuffer.md) - Fix COI issues
- [Explanation: Service Worker Strategy](../explanation/service-worker-strategy.md) - How it works
- [Reference: Service Worker Manager API](../reference/service-worker-manager.md) - API docs

