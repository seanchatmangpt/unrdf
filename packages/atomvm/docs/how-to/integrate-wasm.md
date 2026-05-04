# How-To: Integrate Real AtomVM WASM

## Problem

You want to execute actual BEAM bytecode, but the current implementation is in demo mode. You need to integrate the real AtomVM WebAssembly module.

## Solution

Replace the demo WASM loading with actual AtomVM WASM binary and connect it to the runtime.

## Prerequisites

- AtomVM WASM binary compiled for browser
- Understanding of WebAssembly imports/exports
- AtomVM source code (for compilation)

## Step 1: Build AtomVM for WASM

Compile AtomVM to WebAssembly using Emscripten:

```bash
# Install Emscripten SDK
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk
./emsdk install latest
./emsdk activate latest
source ./emsdk_env.sh

# Build AtomVM
cd /path/to/atomvm
emcc -O3 \
  -s WASM=1 \
  -s ALLOW_MEMORY_GROWTH=1 \
  -s PTHREAD_POOL_SIZE=4 \
  -s SHARED_MEMORY=1 \
  -s EXPORTED_FUNCTIONS='["_execute_beam","_malloc","_free"]' \
  -o atomvm.js \
  atomvm.c
```

This creates:
- `atomvm.wasm` - WebAssembly binary
- `atomvm.js` - JavaScript loader

**Key flags:**
- `PTHREAD_POOL_SIZE=4` - Enables threading (requires SharedArrayBuffer)
- `SHARED_MEMORY=1` - Enables shared memory
- `EXPORTED_FUNCTIONS` - Functions accessible from JavaScript

## Step 2: Place WASM Files

Copy WASM files to the public directory:

```bash
# Development
cp atomvm.wasm packages/atomvm/public/
cp atomvm.js packages/atomvm/public/

# Production (after build)
cp atomvm.wasm packages/atomvm/dist/
cp atomvm.js packages/atomvm/dist/
```

## Step 3: Update Runtime to Load WASM

Modify `src/atomvm-runtime.mjs`:

```javascript
async loadWASM() {
  try {
    this.terminal.log('Checking SharedArrayBuffer support...', 'info');

    if (typeof SharedArrayBuffer === 'undefined') {
      throw new Error('SharedArrayBuffer not available. COI not properly configured.');
    }

    this.terminal.log('SharedArrayBuffer confirmed available ✓', 'success');
    this.terminal.log('Loading AtomVM WASM module...', 'info');

    // Load WASM binary
    const wasmResponse = await fetch('/atomvm.wasm');
    if (!wasmResponse.ok) {
      throw new Error(`Failed to load WASM: ${wasmResponse.statusText}`);
    }
    
    const wasmBinary = await wasmResponse.arrayBuffer();
    this.terminal.log(`Loaded WASM binary: ${wasmBinary.byteLength} bytes`, 'info');

    // Prepare imports for WASM
    const imports = {
      env: {
        // Memory management
        emscripten_memcpy_big: (dest, src, num) => {
          new Uint8Array(this.wasmMemory.buffer).set(
            new Uint8Array(this.wasmMemory.buffer, src, num),
            dest
          );
        },
        // Add other required imports based on AtomVM needs
      },
      wasi_snapshot_preview1: {
        // WASI functions if needed
      },
    };

    // Compile and instantiate WASM
    this.wasmModule = await WebAssembly.compile(wasmBinary);
    this.wasmInstance = await WebAssembly.instantiate(this.wasmModule, imports);

    // Get WASM memory
    this.wasmMemory = this.wasmInstance.exports.memory;
    this.terminal.log('AtomVM WASM module loaded successfully ✓', 'success');

  } catch (error) {
    this.terminal.log(`WASM load error: ${error.message}`, 'error');
    throw error;
  }
}
```

## Step 4: Implement BEAM Execution

Update `executeBeam` to call actual WASM functions:

```javascript
async executeBeam(beamBytes) {
  if (!this.wasmInstance) {
    throw new Error('WASM instance not available. Call loadWASM() first.');
  }

  try {
    this.terminal.log('Executing BEAM bytecode...', 'info');
    this.terminal.log(`Bytecode size: ${beamBytes.length} bytes`, 'info');

    // Allocate memory in WASM
    const beamPtr = this.wasmInstance.exports._malloc(beamBytes.length);
    if (!beamPtr) {
      throw new Error('Failed to allocate memory in WASM');
    }

    // Copy bytecode to WASM memory
    const wasmMemory = new Uint8Array(this.wasmMemory.buffer);
    wasmMemory.set(beamBytes, beamPtr);

    // Execute BEAM code
    const resultPtr = this.wasmInstance.exports._execute_beam(
      beamPtr,
      beamBytes.length
    );

    // Read result from WASM memory
    // (Implementation depends on AtomVM's result format)
    const result = this.readResultFromMemory(resultPtr);

    // Free allocated memory
    this.wasmInstance.exports._free(beamPtr);

    this.terminal.log('Execution completed successfully ✓', 'success');
    return result;

  } catch (error) {
    this.terminal.log(`Execution error: ${error.message}`, 'error');
    throw error;
  }
}
```

## Step 5: Handle WASM Memory

Add helper methods for memory management:

```javascript
/**
 * Read string from WASM memory
 * @param {number} ptr - Pointer to string in WASM memory
 * @returns {string}
 */
readStringFromMemory(ptr) {
  const memory = new Uint8Array(this.wasmMemory.buffer);
  let length = 0;
  while (memory[ptr + length] !== 0) {
    length++;
  }
  const bytes = memory.slice(ptr, ptr + length);
  return new TextDecoder().decode(bytes);
}

/**
 * Read result structure from WASM memory
 * @param {number} resultPtr - Pointer to result structure
 * @returns {Object}
 */
readResultFromMemory(resultPtr) {
  // Implementation depends on AtomVM's result format
  // This is a placeholder - adjust based on actual AtomVM API
  const memory = new Uint32Array(this.wasmMemory.buffer);
  return {
    status: memory[resultPtr / 4],
    value: this.readStringFromMemory(memory[(resultPtr / 4) + 1]),
  };
}
```

## Step 6: Test Integration

1. **Start dev server:**
   ```bash
   pnpm dev
   ```

2. **Open browser console** and verify:
   ```javascript
   // Check WASM loaded
   console.log('WASM module:', typeof window.atomvmRuntime?.wasmInstance);
   
   // Test execution
   const runtime = window.app?.runtime;
   if (runtime) {
     await runtime.loadWASM();
     console.log('✅ WASM loaded');
   }
   ```

3. **Load and execute BEAM:**
   - Place BEAM file in `public/`
   - Use the runtime to load and execute it
   - Check terminal output for results

## Troubleshooting

### WASM Fails to Load

**Check:**
- File path is correct (`/atomvm.wasm`)
- File is in `public/` directory
- Content-Type header is `application/wasm`
- No CORS errors in console

### Execution Fails

**Check:**
- WASM instance is created successfully
- Memory allocation succeeds
- BEAM bytecode is valid
- Imports match AtomVM's requirements

### Memory Errors

**Check:**
- SharedArrayBuffer is available
- WASM memory is properly initialized
- Memory is freed after use
- No buffer overflows

## Related Documentation

- [Tutorial: Running Your First BEAM Code](../tutorials/02-first-beam-code.md) - Learn BEAM execution
- [Reference: AtomVM Runtime API](../reference/atomvm-runtime.md) - Complete API
- [Explanation: Architecture Overview](../explanation/architecture.md) - System design


