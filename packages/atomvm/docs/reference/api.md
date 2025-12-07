# API Reference

Complete API documentation for the AtomVM browser runtime package.

## Package Exports

```javascript
import {
  AtomVMRuntime,
  TerminalUI,
  registerServiceWorker,
  checkCrossOriginIsolation,
  getCOIStatus,
  waitForCOI,
} from '@unrdf/atomvm';
```

## AtomVMRuntime

Main runtime class for executing BEAM bytecode in the browser.

### Constructor

```javascript
new AtomVMRuntime(terminal: TerminalUI, moduleName: string): AtomVMRuntime
```

**Parameters:**
- `terminal` (TerminalUI) - Terminal UI instance for logging
- `moduleName` (string) - Name of module to execute (required, no defaults)

**Returns:** AtomVMRuntime instance

**Throws:** Error if `moduleName` is not provided

**Example:**
```javascript
const terminal = new TerminalUI();
const runtime = new AtomVMRuntime(terminal, 'mymodule');
```

### Methods

#### loadWASM()

Loads the AtomVM WebAssembly module.

```javascript
async loadWASM(): Promise<void>
```

**Returns:** Promise that resolves when WASM is loaded

**Throws:**
- `Error` - If SharedArrayBuffer is not available
- `Error` - If WASM loading fails

**Example:**
```javascript
try {
  await runtime.loadWASM();
  console.log('WASM loaded successfully');
} catch (error) {
  console.error('Failed to load WASM:', error);
}
```

#### executeBeam(avmPath)

Executes .avm file using AtomVM.

```javascript
async executeBeam(avmPath: string): Promise<any>
```

**Parameters:**
- `avmPath` (string) - Path to .avm file to execute (e.g., '/mymodule.avm')

**Returns:** Promise resolving to execution result with `status` and `exitCode`

**Throws:**
- `Error` - If WASM module is not loaded
- `Error` - If avmPath is not provided
- `Error` - If execution fails or times out

**Example:**
```javascript
const result = await runtime.executeBeam('/mymodule.avm');
console.log('Execution result:', result);
```

#### runExample()

Runs example Erlang code (demo mode).

```javascript
async runExample(): Promise<void>
```

**Returns:** Promise that resolves when example completes

**Throws:**
- `Error` - If runtime is not initialized

**Example:**
```javascript
await runtime.loadWASM();
await runtime.runExample();
```

#### destroy()

Cleans up resources.

```javascript
destroy(): void
```

**Example:**
```javascript
runtime.destroy();
```

### Properties

- `terminal` (TerminalUI) - Terminal UI instance
- `wasmModule` (WebAssembly.Module | null) - Compiled WASM module
- `wasmInstance` (WebAssembly.Instance | null) - Instantiated WASM module
- `memory` (Uint8Array | null) - Shared memory buffer

## TerminalUI

Terminal UI component for displaying logs and output.

### Constructor

```javascript
new TerminalUI(): TerminalUI
```

**Returns:** TerminalUI instance

**Example:**
```javascript
const terminal = new TerminalUI();
```

### Methods

#### log(message, type)

Logs a message to the terminal.

```javascript
log(message: string, type?: 'info' | 'success' | 'error'): void
```

**Parameters:**
- `message` (string) - Message to log
- `type` ('info' | 'success' | 'error') - Message type (default: 'info')

**Example:**
```javascript
terminal.log('Initializing...', 'info');
terminal.log('Success!', 'success');
terminal.log('Error occurred', 'error');
```

#### clear()

Clears the terminal.

```javascript
clear(): void
```

**Example:**
```javascript
terminal.clear();
```

#### logMultiple(messages, type)

Logs multiple messages.

```javascript
logMultiple(messages: string[], type?: 'info' | 'success' | 'error'): void
```

**Parameters:**
- `messages` (string[]) - Array of messages
- `type` ('info' | 'success' | 'error') - Message type (default: 'info')

**Example:**
```javascript
terminal.logMultiple(['Line 1', 'Line 2', 'Line 3'], 'info');
```

#### separator()

Creates a separator line.

```javascript
separator(): void
```

**Example:**
```javascript
terminal.separator();
```

### Properties

- `terminalEl` (HTMLElement | null) - Terminal DOM element
- `lines` (HTMLElement[]) - Array of log line elements

## Service Worker Manager

Functions for managing service worker registration and Cross-Origin-Isolation.

### registerServiceWorker()

Registers the coi-serviceworker.

```javascript
async registerServiceWorker(): Promise<boolean>
```

**Returns:** Promise resolving to `true` if registration successful, `false` otherwise

**Example:**
```javascript
const registered = await registerServiceWorker();
if (registered) {
  console.log('Service worker registered');
} else {
  console.error('Service worker registration failed');
}
```

### checkCrossOriginIsolation()

Checks if Cross-Origin-Isolation is enabled.

```javascript
checkCrossOriginIsolation(): boolean
```

**Returns:** `true` if COI is enabled, `false` otherwise

**Example:**
```javascript
if (checkCrossOriginIsolation()) {
  console.log('COI is enabled');
} else {
  console.log('COI is not enabled');
}
```

### getCOIStatus()

Gets detailed Cross-Origin-Isolation status.

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

**Returns:** Status object with COI details

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
```

### waitForCOI(timeout)

Waits for Cross-Origin-Isolation to be enabled.

```javascript
async waitForCOI(timeout?: number): Promise<boolean>
```

**Parameters:**
- `timeout` (number) - Timeout in milliseconds (default: 5000)

**Returns:** Promise resolving to `true` if COI is enabled, `false` otherwise

**Note:** May trigger page reload if service worker needs activation.

**Example:**
```javascript
const enabled = await waitForCOI(10000);
if (enabled) {
  console.log('COI enabled');
} else {
  console.log('COI timeout');
}
```

## App Class

Main application class (internal use, exported for testing).

### Constructor

```javascript
new App(): App
```

### Methods

#### init()

Initializes the application.

```javascript
async init(): Promise<void>
```

**Example:**
```javascript
const app = new App();
await app.init();
```

## Type Definitions

### Message Types

```typescript
type MessageType = 'info' | 'success' | 'error';
```

### COI Status

```typescript
interface COIStatus {
  crossOriginIsolated: boolean;
  sharedArrayBufferAvailable: boolean;
  serviceWorkerSupported: boolean;
  headers: {
    coep: string;
    coop: string;
  };
}
```

## Related Documentation

- [Service Worker Manager API](./service-worker-manager.md) - Detailed service worker API
- [AtomVM Runtime API](./atomvm-runtime.md) - Detailed runtime API
- [Terminal UI API](./terminal-ui.md) - Detailed UI API

