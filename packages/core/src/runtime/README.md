# Cross-Runtime Utilities

**Purpose**: Enable UNRDF code to run in Node.js, browsers, Deno, Bun, and Web Workers

## Quick Start

```javascript
import { detectRuntime, isNode, isBrowser } from '@unrdf/core/runtime/detect';

const runtime = detectRuntime();
console.log(runtime.type); // 'node', 'browser', 'deno', 'bun', 'worker'

if (isNode()) {
  // Node-specific code
}

if (runtime.features.webCrypto) {
  // Use Web Crypto API
}
```

## Files

- **detect.mjs**: Runtime detection and feature checking
- **proofs/demo-1-isomorphic-crypto.mjs**: Crypto that works everywhere
- **proofs/demo-2-universal-store.mjs**: Pure JS RDF store
- **proofs/demo-3-cross-runtime-rpc.mjs**: JSON-RPC message passing

## Run the Demos

```bash
# Demo 1: Isomorphic Crypto (SHA-256 hashing)
node packages/core/src/runtime/proofs/demo-1-isomorphic-crypto.mjs

# Demo 2: Universal Store (in-memory RDF)
node packages/core/src/runtime/proofs/demo-2-universal-store.mjs

# Demo 3: Cross-Runtime RPC (JSON-RPC pattern)
node packages/core/src/runtime/proofs/demo-3-cross-runtime-rpc.mjs
```

## Patterns

See `/home/user/unrdf/docs/cross-runtime-bridging-patterns.md` for full documentation.

### Pattern Summary

1. **Runtime Detection**: `isNode()`, `isBrowser()`, `detectRuntime()`
2. **Conditional Loading**: `const fs = isNode() ? await import('node:fs') : polyfill`
3. **Conditional Exports**: `package.json` exports field with `/client` entry
4. **Polyfill Injection**: BrowserFileSystem, path utilities, crypto wrappers
5. **Isomorphic Libraries**: isomorphic-git, oxigraph (WASM)
6. **Web Crypto API**: Universal crypto via `crypto.subtle`

## API Reference

### detectRuntime()

Returns runtime information:

```javascript
{
  type: 'node' | 'browser' | 'deno' | 'bun' | 'worker' | 'unknown',
  version: string,
  features: {
    fs: boolean,
    crypto: boolean,
    webCrypto: boolean,
    worker: boolean,
    wasm: boolean,
    indexedDB: boolean,
    localStorage: boolean
  }
}
```

### isNode()

Returns `true` if running in Node.js.

### isBrowser()

Returns `true` if running in a browser.

### isDeno()

Returns `true` if running in Deno.

### isBun()

Returns `true` if running in Bun.

### getCrypto()

Returns the global crypto object (Web Crypto API) or `null`.

### hasFeature(feature)

Check if a specific feature is available:

```javascript
hasFeature('webCrypto'); // true in modern runtimes
hasFeature('indexedDB'); // true in browsers
hasFeature('fs'); // true in Node/Deno/Bun
```

## Migration Guide

**Making existing code cross-runtime**:

1. **Replace node:crypto**:

   ```javascript
   // Before
   import { createHash } from 'node:crypto';

   // After
   import { getCrypto } from '@unrdf/core/runtime/detect';
   const crypto = getCrypto();
   await crypto.subtle.digest('SHA-256', data);
   ```

2. **Replace node:fs**:

   ```javascript
   // Before
   import fs from 'node:fs';

   // After
   import { isNode } from '@unrdf/core/runtime/detect';
   const fs = isNode() ? await import('node:fs').then(m => m.default) : new InMemoryFS(); // Polyfill
   ```

3. **Replace node:path**:

   ```javascript
   // Before
   import { join } from 'node:path';

   // After
   const join = (...parts) => parts.filter(Boolean).join('/').replace(/\/+/g, '/');
   ```

## Testing

All demos include self-tests and produce deterministic output.

**Expected results**:

- Demo 1: SHA-256 hash of empty string matches known value
- Demo 2: Store creates 4 triples, queries return 2 results
- Demo 3: 5 RPC calls succeed (including error handling)

## License

MIT
