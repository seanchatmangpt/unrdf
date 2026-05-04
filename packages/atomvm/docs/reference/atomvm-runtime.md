# AtomVM Runtime API Reference

Complete API documentation for the AtomVM runtime class.

## Class: AtomVMRuntime

Main runtime class for executing BEAM bytecode in the browser using WebAssembly.

```javascript
import { AtomVMRuntime } from '@unrdf/atomvm';
```

## Constructor

### new AtomVMRuntime(terminal, moduleName)

Creates a new AtomVM runtime instance.

**Signature:**
```javascript
new AtomVMRuntime(terminal: TerminalUI, moduleName: string): AtomVMRuntime
```

**Parameters:**
- `terminal` (TerminalUI) - Terminal UI instance for logging output
- `moduleName` (string) - Name of module to execute (required, no defaults)

**Returns:** AtomVMRuntime instance

**Throws:** Error if `moduleName` is not provided

**Example:**
```javascript
import { AtomVMRuntime, TerminalUI } from '@unrdf/atomvm';

const terminal = new TerminalUI();
const runtime = new AtomVMRuntime(terminal, 'mymodule');
```

## Methods

### loadWASM()

Loads the AtomVM WebAssembly module and prepares shared memory.

**Signature:**
```javascript
async loadWASM(): Promise<void>
```

**Returns:** Promise that resolves when WASM is loaded

**Throws:**
- `Error` - If SharedArrayBuffer is not available
- `Error` - If WASM loading fails

**Behavior:**
1. Verifies SharedArrayBuffer is available
2. Loads AtomVM.js script dynamically (Emscripten pattern)
3. Waits for `window.Module.ready` to become true
4. Logs progress to terminal

**Example:**
```javascript
try {
  await runtime.loadWASM();
  console.log('WASM loaded successfully');
} catch (error) {
  console.error('Failed to load WASM:', error.message);
}
```

**Note:** Loads AtomVM.js script which contains the full Emscripten module. The module is available as `window.Module` after loading.

### executeBeam(avmPath)

Executes .avm file using AtomVM.

**Signature:**
```javascript
async executeBeam(avmPath: string): Promise<any>
```

**Parameters:**
- `avmPath` (string) - Path to .avm file to execute (e.g., '/mymodule.avm')

**Returns:** Promise resolving to execution result object with `status` and `exitCode`

**Throws:**
- `Error` - If WASM module is not loaded
- `Error` - If avmPath is not provided
- `Error` - If execution fails or times out

**Example:**
```javascript
// Execute .avm file
const result = await runtime.executeBeam('/mymodule.avm');
console.log('Execution result:', result);
// { status: 'ok', exitCode: 0 }
```

**Note:** Requires `loadWASM()` to be called first. Uses AtomVM.js script (Emscripten pattern) to execute .avm files.

### runExample()

Runs example Erlang code in demo mode.

**Signature:**
```javascript
async runExample(): Promise<void>
```

**Returns:** Promise that resolves when example completes

**Throws:**
- `Error` - If runtime is not initialized (memory not allocated)

**Behavior:**
1. Fetches .avm file using moduleName from constructor
2. Executes .avm file via AtomVM
3. Displays output in terminal
4. Handles completion via Module.onExit

**Example:**
```javascript
const runtime = new AtomVMRuntime(terminal, 'mymodule');
await runtime.loadWASM();
await runtime.runExample();
// Terminal shows AtomVM execution output
```

### destroy()

Cleans up resources and resets runtime state.

**Signature:**
```javascript
destroy(): void
```

**Behavior:**
- Sets `atomvmModule` to `null`
- Sets `wasmInstance` to `null`
- Sets `memory` to `null`
- Logs destruction message to terminal

**Example:**
```javascript
runtime.destroy();
// Runtime resources cleaned up
```

## Properties

### terminal

Terminal UI instance for logging.

**Type:** `TerminalUI`

**Access:** Read-only

**Example:**
```javascript
runtime.terminal.log('Custom message', 'info');
```

### wasmModule

Compiled WebAssembly module.

**Type:** `WebAssembly.Module | null`

**Access:** Read-only

**Note:** Set after `loadWASM()` completes (in production with real WASM).

### wasmInstance

Instantiated WebAssembly module.

**Type:** `WebAssembly.Instance | null`

**Access:** Read-only

**Note:** Set after `loadWASM()` completes (in production with real WASM).

### memory

Shared memory buffer (Uint8Array view of SharedArrayBuffer).

**Type:** `Uint8Array | null`

**Access:** Read-only

**Note:** Set after `loadWASM()` completes. Size is 1MB in demo mode.

**Example:**
```javascript
if (runtime.memory) {
  console.log('Memory size:', runtime.memory.byteLength);
}
```

## Usage Example

Complete example:

```javascript
import { AtomVMRuntime, TerminalUI } from '@unrdf/atomvm';

async function runAtomVM() {
  // Create terminal and runtime
  const terminal = new TerminalUI();
  const runtime = new AtomVMRuntime(terminal, 'mymodule');

  try {
    // Load WASM module
    terminal.log('Loading WASM...', 'info');
    await runtime.loadWASM();
    terminal.log('WASM loaded ✓', 'success');

    // Run example
    terminal.log('Running example...', 'info');
    await runtime.runExample();

    // Execute .avm file
    const result = await runtime.executeBeam('/mymodule.avm');
    terminal.log(`Result: ${JSON.stringify(result)}`, 'success');

  } catch (error) {
    terminal.log(`Error: ${error.message}`, 'error');
  } finally {
    // Cleanup
    runtime.destroy();
  }
}
```

## Integration with Real AtomVM WASM

To use actual AtomVM WASM, update `loadWASM()`:

```javascript
async loadWASM() {
  // Load WASM binary
  const response = await fetch('/atomvm.wasm');
  const wasmBinary = await response.arrayBuffer();

  // Compile and instantiate
  this.wasmModule = await WebAssembly.compile(wasmBinary);
  this.wasmInstance = await WebAssembly.instantiate(
    this.wasmModule,
    imports
  );

  // Get WASM memory
  this.wasmMemory = this.wasmInstance.exports.memory;
}
```

And update `executeBeam()`:

```javascript
async executeBeam(avmPath) {
  // Execute .avm file via AtomVM
  // Uses Module.callMain() from Emscripten pattern
  this.atomvmModule.arguments = [avmPath];
  
  if (this.atomvmModule.callMain) {
    this.atomvmModule.callMain([avmPath]);
  } else {
    this.atomvmModule.run();
  }

  return result;
}
```

## Related Documentation

- [How-To: Integrate Real AtomVM WASM](../how-to/integrate-wasm.md)
- [Tutorial: Running Your First BEAM Code](../tutorials/02-first-beam-code.md)
- [Terminal UI API](./terminal-ui.md)

