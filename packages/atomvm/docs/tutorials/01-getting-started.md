# Tutorial: Getting Started with AtomVM Browser

## Overview

In this tutorial, you'll set up the AtomVM browser runtime and run your first Erlang/BEAM code in the browser. You'll learn how to initialize the service worker, enable Cross-Origin-Isolation, and execute BEAM bytecode.

**Time:** 15 minutes

## What You'll Learn

- How to set up the AtomVM browser runtime
- How service workers enable SharedArrayBuffer
- How to initialize and use the AtomVM runtime
- How to execute example BEAM code

## What You'll Build

A working browser application that can execute Erlang/BEAM code using AtomVM in the browser.

## Prerequisites

- Node.js 18+ installed
- Basic understanding of JavaScript
- A modern browser (Chrome 92+, Firefox 95+, Safari 15.2+)
- pnpm package manager

## Before You Start

1. **Install dependencies:**
   ```bash
   cd packages/atomvm
   pnpm install
   ```

2. **Verify installation:**
   ```bash
   pnpm build
   ```
   You should see build output with no errors.

## Step 1: Start the Development Server

Start the Vite development server:

```bash
pnpm dev
```

The server will start on `http://localhost:3000`. Open this URL in your browser.

**What's happening:** Vite serves the application with development features like hot module replacement. The server also sets COOP/COEP headers to help with Cross-Origin-Isolation during development.

## Step 2: Observe the Initialization

When you open the page, you'll see:

1. **Status indicator** showing "🔄 Initializing service worker and AtomVM runtime..."
2. **Terminal console** displaying initialization messages
3. **Service worker registration** happening automatically

Watch the terminal console. You should see messages like:
```
Starting AtomVM browser initialization...
Registering coi-serviceworker...
Service worker registered successfully
Cross-Origin-Isolation enabled ✓
SharedArrayBuffer available ✓
Initializing AtomVM runtime...
🚀 AtomVM is ready to execute BEAM code!
```

**What's happening:** The app automatically:
- Registers the `coi-serviceworker` service worker
- Waits for Cross-Origin-Isolation to be enabled
- Initializes the AtomVM runtime
- Enables the control buttons

**Note:** The page may reload once to activate the service worker. This is normal and automatic.

## Step 3: Initialize the WASM Module

Once the status shows "✅ AtomVM ready!", click the **"Initialize AtomVM"** button.

**What you'll see:**
```
Initializing AtomVM WASM module...
Checking SharedArrayBuffer support...
SharedArrayBuffer confirmed available ✓
Creating shared memory buffer...
Allocated 1048576 bytes shared memory
AtomVM WASM module ready (demo mode)
Note: Connect actual AtomVM WASM binary for real execution
```

**What's happening:** The runtime:
- Verifies SharedArrayBuffer is available (required for WASM threading)
- Allocates shared memory (1MB in demo mode)
- Prepares for WASM module loading

**Note:** This is demo mode. To use actual AtomVM WASM, see the [Integrate Real AtomVM WASM](../how-to/integrate-wasm.md) guide.

## Step 4: Run Example Code

Click the **"Run Example"** button to execute example Erlang code.

**What you'll see:**
```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Running example BEAM code...

% Example Erlang module:
-module(hello).
-export([world/0]).

world() ->
  io:format("Hello from AtomVM in browser!~n"),
  {ok, browser, ready}.

Executing: hello:world()

Hello from AtomVM in browser!
{ok, browser, ready}

✓ Execution completed successfully
Memory used: 1048576 bytes
```

**What's happening:** The runtime simulates executing Erlang code. In production, this would execute actual BEAM bytecode through the AtomVM WASM module.

## Verify It Works

Check the browser console (F12 → Console tab). You should see:
- No errors
- Service worker registered
- `crossOriginIsolated` is `true`

Run this in the console:
```javascript
console.log('COI:', crossOriginIsolated);
console.log('SharedArrayBuffer:', typeof SharedArrayBuffer !== 'undefined');
```

Both should be `true`.

## What You've Learned

- ✅ How to start the AtomVM browser runtime
- ✅ How service workers enable Cross-Origin-Isolation
- ✅ How to initialize the WASM module
- ✅ How to execute example BEAM code
- ✅ How to verify SharedArrayBuffer is available

## Next Steps

👉 **Tutorial:** [Running Your First BEAM Code](./02-first-beam-code.md) - Learn to execute actual BEAM bytecode

👉 **How-To:** [Integrate Real AtomVM WASM](../how-to/integrate-wasm.md) - Connect actual AtomVM WASM binary

👉 **Reference:** [API Reference](../reference/api.md) - Complete API documentation

👉 **Explanation:** [Architecture Overview](../explanation/architecture.md) - Understand the system design


