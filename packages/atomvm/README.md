# @unrdf/atomvm - Browser-Based AtomVM Runtime

Run AtomVM (Erlang/BEAM VM) in the browser using WebAssembly and Service Workers.

## Features

- **Cross-Origin-Isolation**: Uses `coi-serviceworker` to enable SharedArrayBuffer
- **WebAssembly Support**: Ready for AtomVM WASM module integration
- **Service Worker Architecture**: Intercepts requests to add COOP/COEP headers
- **Threading Support**: SharedArrayBuffer enables multi-threaded WASM execution
- **Beautiful UI**: Modern terminal interface for BEAM code execution

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                     Browser Tab                          │
│  ┌────────────────────────────────────────────────────┐ │
│  │           index.html (UI)                          │ │
│  │  ┌──────────────────────────────────────────────┐ │ │
│  │  │  src/index.mjs                               │ │ │
│  │  │  - App orchestration                         │ │ │
│  │  │  - Event handling                            │ │ │
│  │  └──────────────────────────────────────────────┘ │ │
│  │  ┌──────────────────────────────────────────────┐ │ │
│  │  │  src/service-worker-manager.mjs              │ │ │
│  │  │  - Register coi-serviceworker                │ │ │
│  │  │  - Check COI status                          │ │ │
│  │  └──────────────────────────────────────────────┘ │ │
│  │  ┌──────────────────────────────────────────────┐ │ │
│  │  │  src/atomvm-runtime.mjs                      │ │ │
│  │  │  - Load AtomVM WASM                          │ │ │
│  │  │  - Execute BEAM bytecode                     │ │ │
│  │  │  - Manage SharedArrayBuffer                  │ │ │
│  │  └──────────────────────────────────────────────┘ │ │
│  └────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
                           │
                           │ Fetch requests
                           ▼
┌─────────────────────────────────────────────────────────┐
│                  Service Worker                          │
│  ┌────────────────────────────────────────────────────┐ │
│  │  coi-serviceworker                                 │ │
│  │  - Intercepts all requests                         │ │
│  │  - Adds COOP: same-origin header                   │ │
│  │  - Adds COEP: require-corp header                  │ │
│  │  - Enables crossOriginIsolated = true              │ │
│  └────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
                           │
                           │ Modified responses
                           ▼
              ┌──────────────────────────┐
              │  SharedArrayBuffer       │
              │  Available ✓             │
              └──────────────────────────┘
                           │
                           ▼
              ┌──────────────────────────┐
              │  AtomVM WASM Module      │
              │  With threading support  │
              └──────────────────────────┘
```

## How It Works

### Cross-Origin-Isolation (COI)

Modern browsers require two HTTP headers to enable SharedArrayBuffer:

1. **Cross-Origin-Opener-Policy (COOP)**: `same-origin`
2. **Cross-Origin-Embedder-Policy (COEP)**: `require-corp`

These headers ensure the page is isolated from cross-origin resources, preventing timing attacks via SharedArrayBuffer.

### The Service Worker Trick

`coi-serviceworker` is a tiny library that:

1. Registers a service worker that intercepts ALL fetch requests
2. Adds the COOP and COEP headers to responses
3. Effectively enables `crossOriginIsolated = true`
4. Makes SharedArrayBuffer available

This works even when you can't control the server headers (e.g., static hosting).

### Why This Matters for AtomVM

AtomVM compiled to WebAssembly needs:
- **Threading**: For concurrent BEAM processes
- **SharedArrayBuffer**: For shared memory between threads
- **Atomic operations**: For synchronization

Without COI, these features are disabled by browsers for security reasons.

## Usage

### Development

```bash
# Install dependencies
pnpm install

# Start dev server
pnpm run dev

# Open http://localhost:3000
```

### Production Build

```bash
# Build for production
pnpm run build

# Preview production build
pnpm run preview

# Or serve with Python
pnpm run serve
```

### Integrating Real AtomVM WASM

To use the actual AtomVM WebAssembly module:

1. **Build AtomVM for WASM** (from AtomVM source):
   ```bash
   emcc -O3 -s WASM=1 -s ALLOW_MEMORY_GROWTH=1 \
        -s PTHREAD_POOL_SIZE=4 \
        -o atomvm.wasm atomvm.c
   ```

2. **Place WASM binary** in `public/`:
   ```bash
   cp atomvm.wasm packages/atomvm/public/
   ```

3. **Update `atomvm-runtime.mjs`**:
   ```javascript
   async loadWASM() {
     const response = await fetch('/atomvm.wasm');
     const wasmBinary = await response.arrayBuffer();
     this.wasmModule = await WebAssembly.compile(wasmBinary);

     const imports = {
       env: {
         // Provide required imports
       }
     };

     this.wasmInstance = await WebAssembly.instantiate(
       this.wasmModule,
       imports
     );
   }
   ```

## API

### AtomVMRuntime

```javascript
import { AtomVMRuntime } from '@unrdf/atomvm';

const runtime = new AtomVMRuntime(terminalUI);

// Load WASM module
await runtime.loadWASM();

// Execute BEAM bytecode
const result = await runtime.executeBeam(beamBytes);

// Run example
await runtime.runExample();

// Cleanup
runtime.destroy();
```

### Service Worker Manager

```javascript
import {
  registerServiceWorker,
  checkCrossOriginIsolation,
  getCOIStatus
} from '@unrdf/atomvm/service-worker-manager';

// Register COI service worker
await registerServiceWorker();

// Check if COI is enabled
const isIsolated = checkCrossOriginIsolation();

// Get detailed status
const status = getCOIStatus();
```

## Browser Compatibility

- **Chrome/Edge**: 92+ (SharedArrayBuffer support)
- **Firefox**: 95+ (with COI)
- **Safari**: 15.2+ (with COI)

All modern browsers support Service Workers and SharedArrayBuffer with proper headers.

## Security Considerations

- Service worker runs on same origin only
- COOP/COEP headers isolate the page
- No access to cross-origin resources without CORS
- SharedArrayBuffer only available within isolated context

## Troubleshooting

### SharedArrayBuffer not available

1. Check service worker is registered: DevTools → Application → Service Workers
2. Verify `crossOriginIsolated = true` in console
3. Hard reload (Ctrl+Shift+R) to activate service worker
4. Check for mixed content (HTTPS required for SharedArrayBuffer)

### Service worker not registering

1. Ensure you're on `http://localhost` or HTTPS (not `http://0.0.0.0`)
2. Check browser console for errors
3. Clear service workers: DevTools → Application → Clear storage
4. Verify `coi-serviceworker` is installed

### WASM module fails to load

1. Check WASM file is in `public/` directory
2. Verify Content-Type: `application/wasm`
3. Check WASM was compiled with `-s PTHREAD_POOL_SIZE=N`
4. Ensure SharedArrayBuffer is available

## References

- [AtomVM](https://github.com/atomvm/AtomVM) - Erlang/BEAM VM for embedded devices
- [coi-serviceworker](https://github.com/gzuidhof/coi-serviceworker) - Cross-Origin-Isolation via Service Worker
- [WebAssembly Threading](https://emscripten.org/docs/porting/pthreads.html) - Emscripten pthreads guide
- [SharedArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer) - MDN documentation

## License

MIT
