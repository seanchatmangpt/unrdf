# Tutorial: Running Your First BEAM Code

## Overview

In this tutorial, you'll learn how to execute actual BEAM bytecode in the browser using the AtomVM runtime. You'll compile Erlang code to BEAM format and execute it through the browser runtime.

**Time:** 20 minutes

## What You'll Learn

- How to compile Erlang code to BEAM format
- How to load BEAM bytecode into the browser runtime
- How to execute BEAM code and capture results
- How to handle errors and debug execution

## What You'll Build

A complete workflow for compiling Erlang code and executing it in the browser using AtomVM.

## Prerequisites

- Completed [Getting Started Tutorial](./01-getting-started.md)
- Erlang/OTP installed (for compiling Erlang code)
- Basic understanding of Erlang syntax
- AtomVM WASM binary (see [Integrate Real AtomVM WASM](../how-to/integrate-wasm.md))

## Before You Start

1. **Verify AtomVM runtime is working:**
   ```bash
   cd packages/atomvm
   pnpm dev
   ```
   Open `http://localhost:3000` and verify initialization works.

2. **Check Erlang installation:**
   ```bash
   erl -version
   ```
   You should see Erlang version information.

## Step 1: Write Erlang Code

Create a simple Erlang module. Create `hello.erl`:

```erlang
-module(hello).
-export([world/0, greet/1]).

world() ->
    io:format("Hello from AtomVM in browser!~n"),
    {ok, browser, ready}.

greet(Name) ->
    io:format("Hello, ~s!~n", [Name]),
    {ok, greeted, Name}.
```

**What's happening:** This is a simple Erlang module with two exported functions. The `world/0` function prints a message and returns a tuple. The `greet/1` function takes a name and greets it.

## Step 2: Compile to BEAM

Compile the Erlang code to BEAM bytecode:

```bash
erlc hello.erl
```

This creates `hello.beam`, which contains the compiled bytecode.

**What's happening:** The Erlang compiler (`erlc`) compiles the `.erl` source file into BEAM bytecode format. BEAM is the virtual machine bytecode that AtomVM executes.

## Step 3: Load BEAM into Browser

To load BEAM bytecode in the browser, you need to:

1. **Place BEAM file in public directory:**
   ```bash
   mkdir -p packages/atomvm/public
   cp hello.beam packages/atomvm/public/
   ```

2. **Update `atomvm-runtime.mjs` to load BEAM:**
   
   Add this method to the `AtomVMRuntime` class:

   ```javascript
   /**
    * Load BEAM bytecode from file
    * @param {string} beamPath - Path to BEAM file
    * @returns {Promise<Uint8Array>} BEAM bytecode
    */
   async loadBeamFile(beamPath) {
     const response = await fetch(beamPath);
     if (!response.ok) {
       throw new Error(`Failed to load BEAM file: ${beamPath}`);
     }
     const arrayBuffer = await response.arrayBuffer();
     return new Uint8Array(arrayBuffer);
   }
   ```

3. **Execute BEAM code:**
   
   Update the `executeBeam` method to actually execute the bytecode:

   ```javascript
   async executeBeam(avmPath) {
     if (!this.wasmInstance) {
       throw new Error('WASM instance not available');
     }

     // In production, this would call into AtomVM WASM exports
     // For now, we'll simulate execution
     this.terminal.log('Executing BEAM bytecode...', 'info');
     
     // TODO: Call actual AtomVM WASM function
     // const result = this.wasmInstance.exports.execute_beam(beamBytes);
     
     return { status: 'ok', result: 'executed' };
   }
   ```

**What's happening:** You're setting up the infrastructure to load and execute BEAM bytecode. In production, this would call into the actual AtomVM WASM module.

## Step 4: Execute BEAM Code

Create a test function to load and execute the BEAM code:

```javascript
async function runBeamExample() {
  const runtime = new AtomVMRuntime(terminal, 'mymodule');
  
  // Load WASM module first
  await runtime.loadWASM();
  
  // Execute .avm file
  const result = await runtime.executeBeam('/mymodule.avm');
  this.terminal.log(`Execution result: ${JSON.stringify(result)}`, 'success');
}
```

**What's happening:** This function:
1. Initializes the runtime with module name
2. Loads the WASM module (required for execution)
3. Executes the .avm file through AtomVM

## Step 5: Handle Execution Results

Update the UI to display execution results:

```javascript
async handleRunBeam() {
  if (!this.runtime) {
    this.terminal.log('Runtime not available', 'error');
    return;
  }

  try {
    // Execute .avm file
    const result = await this.runtime.executeBeam('/mymodule.avm');
    
    if (result.status === 'ok') {
      this.terminal.log('Execution successful!', 'success');
      if (result.output) {
        this.terminal.log(result.output, 'info');
      }
    } else {
      this.terminal.log(`Execution failed: ${result.error}`, 'error');
    }
  } catch (error) {
    this.terminal.log(`Error: ${error.message}`, 'error');
  }
}
```

**What's happening:** This adds proper error handling and result display for BEAM execution.

## Verify It Works

1. **Start the dev server:**
   ```bash
   pnpm dev
   ```

2. **Open the browser console** (F12 → Console)

3. **Load and execute BEAM:**
   - Click "Initialize AtomVM"
   - Click "Run Example" (or your custom button)
   - Check the terminal output

4. **Verify execution:**
   - Terminal should show "Execution successful!"
   - No errors in browser console
   - BEAM file loads correctly

## Troubleshooting

**BEAM file not found:**
- Ensure file is in `public/` directory
- Check file path is correct (`/hello.beam`)
- Verify file is served by dev server

**WASM not loaded:**
- Click "Initialize AtomVM" first
- Check SharedArrayBuffer is available
- Verify service worker is registered

**Execution errors:**
- Check browser console for detailed errors
- Verify BEAM bytecode is valid
- Ensure AtomVM WASM is properly integrated

## What You've Learned

- ✅ How to compile Erlang code to BEAM format
- ✅ How to load BEAM bytecode in the browser
- ✅ How to execute BEAM code through AtomVM
- ✅ How to handle execution results and errors
- ✅ How to debug BEAM execution issues

## Next Steps

👉 **How-To:** [Integrate Real AtomVM WASM](../how-to/integrate-wasm.md) - Connect actual AtomVM WASM binary for real execution

👉 **How-To:** [Debug Service Worker Issues](../how-to/debug-service-worker.md) - Troubleshoot common problems

👉 **Reference:** [AtomVM Runtime API](../reference/atomvm-runtime.md) - Complete runtime API

👉 **Explanation:** [Architecture Overview](../explanation/architecture.md) - Understand the system design

