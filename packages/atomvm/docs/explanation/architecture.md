# Architecture Overview

## System Design

The AtomVM browser runtime is designed to enable Erlang/BEAM bytecode execution in web browsers through WebAssembly, while handling the complex browser security requirements for SharedArrayBuffer support.

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────┐
│                     Browser Tab                          │
│  ┌────────────────────────────────────────────────────┐ │
│  │           index.html (UI)                          │ │
│  │  ┌──────────────────────────────────────────────┐ │ │
│  │  │  src/index.mjs                               │ │ │
│  │  │  - App orchestration                         │ │ │
│  │  │  - Event handling                            │ │ │
│  │  │  - UI updates                                │ │ │
│  │  └──────────────────────────────────────────────┘ │ │
│  │  ┌──────────────────────────────────────────────┐ │ │
│  │  │  src/service-worker-manager.mjs              │ │ │
│  │  │  - Register coi-serviceworker                │ │ │
│  │  │  - Check COI status                          │ │ │
│  │  │  - Handle reload logic                        │ │ │
│  │  └──────────────────────────────────────────────┘ │ │
│  │  ┌──────────────────────────────────────────────┐ │ │
│  │  │  src/atomvm-runtime.mjs                      │ │ │
│  │  │  - Load AtomVM WASM                          │ │ │
│  │  │  - Execute BEAM bytecode                     │ │ │
│  │  │  - Manage SharedArrayBuffer                  │ │ │
│  │  └──────────────────────────────────────────────┘ │ │
│  │  ┌──────────────────────────────────────────────┐ │ │
│  │  │  src/terminal-ui.mjs                        │ │ │
│  │  │  - Display logs                              │ │ │
│  │  │  - Format output                             │ │ │
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

## Component Responsibilities

### index.mjs (App Orchestration)

**Responsibilities:**
- Initialize application lifecycle
- Coordinate service worker registration
- Manage UI state and updates
- Handle user interactions
- Error handling and recovery

**Key Design Decisions:**
- Single App class manages entire application state
- Async initialization with proper error handling
- Automatic page reload for service worker activation
- Clear separation of concerns

### service-worker-manager.mjs (COI Management)

**Responsibilities:**
- Register `coi-serviceworker` service worker
- Detect Cross-Origin-Isolation status
- Handle service worker activation
- Provide COI status information

**Key Design Decisions:**
- Non-throwing API (returns false on errors)
- Automatic service worker registration
- Polling-based COI detection
- Automatic reload handling

### atomvm-runtime.mjs (WASM Execution)

**Responsibilities:**
- Load AtomVM WebAssembly module
- Execute BEAM bytecode
- Manage shared memory
- Handle WASM lifecycle

**Key Design Decisions:**
- Lazy loading (WASM loaded on demand)
- Shared memory for threading support
- Placeholder implementation for demo
- Extensible for real AtomVM integration

### terminal-ui.mjs (Output Display)

**Responsibilities:**
- Display logs and output
- Format messages by type
- Auto-scroll terminal
- Console fallback

**Key Design Decisions:**
- DOM-based rendering
- Console integration for debugging
- Type-based styling (info/success/error)
- Simple, focused API

## Data Flow

### Initialization Flow

```
1. Page Load
   ↓
2. App.init() called
   ↓
3. registerServiceWorker()
   ↓
4. Service worker registered
   ↓
5. checkCrossOriginIsolation()
   ↓
6. If false → reload page
   If true → continue
   ↓
7. Initialize AtomVMRuntime
   ↓
8. Enable UI controls
   ↓
9. Ready state
```

### Execution Flow

```
1. User clicks "Initialize AtomVM"
   ↓
2. runtime.loadWASM()
   ↓
3. Verify SharedArrayBuffer
   ↓
4. Allocate shared memory
   ↓
5. Load WASM module (if available)
   ↓
6. Ready for execution
   ↓
7. User clicks "Run Example"
   ↓
8. runtime.runExample() or runtime.executeBeam()
   ↓
9. Execute BEAM bytecode
   ↓
10. Display results in terminal
```

## Security Architecture

### Cross-Origin-Isolation (COI)

**Why needed:**
- SharedArrayBuffer requires COI for security
- Prevents timing attacks via shared memory
- Required for WebAssembly threading

**How achieved:**
- Service worker intercepts all requests
- Adds COOP/COEP headers to responses
- Browser enables `crossOriginIsolated` flag
- SharedArrayBuffer becomes available

**Tradeoffs:**
- Requires service worker (adds complexity)
- Page reload needed for activation
- Some cross-origin resources may break
- HTTPS or localhost required

### Service Worker Strategy

**Why service worker:**
- Can't set headers from JavaScript
- Service workers can intercept requests
- Works with static hosting
- No server configuration needed

**How it works:**
- `coi-serviceworker` registers automatically
- Intercepts all fetch requests
- Adds required headers to responses
- Browser recognizes COI context

## Build Architecture

### Vite Configuration

**Build process:**
1. Entry: `index.html`
2. Bundle JavaScript modules
3. Include `coi-serviceworker`
4. Minify and optimize
5. Output to `dist/`

**Key features:**
- ES modules support
- Code splitting
- Asset optimization
- Development server with HMR

### Production Build

**Output structure:**
```
dist/
├── index.html                    # Main HTML
└── assets/
    ├── main-*.js                 # Application bundle
    └── coi-serviceworker-*.js    # Service worker bundle
```

**Optimizations:**
- Minification
- Tree-shaking
- Asset hashing
- Gzip compression

## Extension Points

### Integrating Real AtomVM WASM

**Steps:**
1. Compile AtomVM to WASM
2. Place WASM binary in `public/`
3. Update `loadWASM()` to load actual binary
4. Implement `executeBeam()` with WASM calls
5. Handle WASM memory management

### Custom Terminal UI

**Options:**
1. Extend `TerminalUI` class
2. Replace with custom implementation
3. Use different DOM structure
4. Add custom formatting

### Additional Features

**Possible extensions:**
- File upload for BEAM files
- Multiple BEAM module support
- Persistent storage
- Network communication
- Debugging tools

## Design Principles

### 1. Progressive Enhancement

- Works without WASM (demo mode)
- Graceful degradation
- Clear error messages
- Fallback behaviors

### 2. Security First

- COI required for SharedArrayBuffer
- Service worker for header injection
- No unsafe eval or dynamic code
- Sandboxed execution

### 3. Developer Experience

- Clear error messages
- Helpful logging
- Easy debugging
- Good documentation

### 4. Performance

- Lazy loading
- Efficient memory usage
- Optimized builds
- Fast initialization

## Related Documentation

- [Cross-Origin-Isolation Explanation](./cross-origin-isolation.md)
- [Service Worker Strategy Explanation](./service-worker-strategy.md)
- [How-To: Integrate Real AtomVM WASM](../how-to/integrate-wasm.md)

