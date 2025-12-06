# Phase 4.3: Browser Simulation - AtomVM + WASM

## Overview

Run your Erlang system in the browser for offline development using:

1. **AtomVM**: Erlang bytecode virtual machine designed for embedded systems
2. **WASM**: WebAssembly for safe browser execution
3. **JS Bridge**: Lightweight interface between JS and Erlang

---

## What Works in Browser

### ✓ Supported

- **Core Erlang**: Pattern matching, recursion, list operations
- **gen_server**: Stateful processes
- **gen_statem**: State machines
- **supervisor**: Supervision trees
- **Maps, lists, binaries**: Core data structures
- **ETS**: Basic set/get operations (limited)
- **WebSocket**: Browser native API
- **In-memory storage**: Maps and lists

### ✗ Not Supported

- **NIFs**: Native code
- **File I/O**: No file system access
- **Distribution**: No clustering
- **SSL/crypto**: Limited cryptographic functions
- **gen_tcp**: Only WebSocket in browser
- **mnesia**: No persistence
- **External ports**: Cannot spawn external processes

---

## Architecture

```
┌─────────────────────────────────────┐
│     JavaScript Application           │
│   (React, Vue, or vanilla)           │
└────────┬────────────────────────────┘
         │
    ┌────▼────────────────────┐
    │   Protocol Client        │
    │  (WebSocket to port)     │
    └────┬────────────────────┘
         │ WebSocket
         │
┌────────▼──────────────────────────────┐
│   Browser / WASM Environment           │
│  ┌──────────────────────────────────┐ │
│  │   AtomVM Virtual Machine         │ │
│  │  ┌────────────────────────────┐ │ │
│  │  │  Erlang Application Code   │ │ │
│  │  │  (Protocol + Domain)       │ │ │
│  │  └────────────────────────────┘ │ │
│  │                                  │ │
│  │  ┌────────────────────────────┐ │ │
│  │  │  AtomVM Runtime            │ │ │
│  │  │  - Processes               │ │ │
│  │  │  - Memory management       │ │ │
│  │  │  - Bytecode execution      │ │ │
│  │  └────────────────────────────┘ │ │
│  │                                  │ │
│  │  ┌────────────────────────────┐ │ │
│  │  │  JS↔Erlang Bridge          │ │ │
│  │  │  - WebSocket handler       │ │ │
│  │  │  - Message routing         │ │ │
│  │  │  - Port API                │ │ │
│  │  └────────────────────────────┘ │ │
│  └──────────────────────────────────┘ │
└───────────────────────────────────────┘
```

---

## Setup

### Step 1: Get AtomVM WASM

```bash
# Build AtomVM for WASM
git clone https://github.com/atomvm/atomvm.git
cd atomvm
./tools/build-wasm.sh
# Creates: build/wasm/atomvm.js, atomvm.wasm
```

Or use pre-built:

```html
<!-- In your HTML -->
<script src="https://cdn.jsdelivr.net/npm/@atomvm/atomvm@latest/atomvm.js"></script>
```

### Step 2: Compile Erlang to BEAM

```bash
# Standard compilation for AtomVM
rebar3 compile

# AtomVM bytecode (BEAM files in ebin/)
ls -la _build/default/lib/*/ebin/*.beam
```

### Step 3: Package for Browser

```bash
# Create AVM bundle (Atom Virtual Machine format)
atomvm_packager create \
    --output atomvm_bundle.avm \
    --entry unrdf_app \
    _build/default/lib/*/ebin/*.beam
```

Or programmatically:

```javascript
// In your build script
const { execSync } = require('child_process');
execSync(`atomvm_packager create \
    --output public/atomvm_bundle.avm \
    --entry unrdf_app \
    _build/default/lib/*/ebin/*.beam`);
```

### Step 4: Load in Browser

```javascript
// JavaScript side
import init, { atomvm_run } from '@atomvm/atomvm';

async function startAtomVMApp() {
  await init();

  // Load the AVM bundle
  const response = await fetch('/atomvm_bundle.avm');
  const buffer = await response.arrayBuffer();

  // Start the Erlang application
  atomvm_run({
    avm_data: new Uint8Array(buffer),
    entry_point: 'unrdf_app',
    // Will call JavaScript callbacks registered below
  });
}

startAtomVMApp().catch(console.error);
```

---

## JS ↔ Erlang Bridge

### JavaScript Side (Calling Erlang)

```javascript
// Register callback that Erlang can call
class ErlangBridge {
  constructor() {
    this.handlers = new Map();
  }

  // Register a handler for Erlang to call
  register(name, handler) {
    this.handlers.set(name, handler);
  }

  // Erlang calls this to invoke JavaScript
  call(name, args) {
    const handler = this.handlers.get(name);
    if (!handler) {
      throw new Error(`No handler for ${name}`);
    }
    return handler(...args);
  }
}

const bridge = new ErlangBridge();

// Example: Register WebSocket sending
bridge.register('send_websocket', (message) => {
  if (ws.readyState === WebSocket.OPEN) {
    ws.send(JSON.stringify(message));
  }
});

// Example: Register logging
bridge.register('log', (level, message) => {
  console[level](message);
});
```

### Erlang Side (Calling JavaScript)

```erlang
% Erlang module: atomvm_js_bridge.erl
-module(atomvm_js_bridge).

% Call JavaScript function from Erlang
call_js(Function, Args) ->
    % JavaScript bridge API (AtomVM-specific)
    atomvm_js:call(Function, Args).

% Example: Send WebSocket message
send_ws_message(Message) ->
    call_js(send_websocket, [Message]).

% Example: Log from Erlang
log(Level, Msg) ->
    call_js(log, [Level, Msg]).
```

### Full Example: Command Execution

```javascript
// JavaScript initiates command
const commandId = 42;
const command = {
  type: 'command',
  id: commandId,
  payload: {
    action: 'executeQuery',
    params: { query: 'SELECT * WHERE { ?s ?p ?o }' }
  }
};

// Send to AtomVM via port
atomvm.port_command('command', command);
```

```erlang
% Erlang receives and processes
-module(unrdf_command_handler).

% Port receives commands
handle_command(CommandMsg) ->
    case CommandMsg of
        #{<<"action">> := <<"executeQuery">>,
          <<"params">> := Params} ->
            Result = execute_query(Params),
            % Send response back to JavaScript
            atomvm_js_bridge:send_ws_message({
                response,
                Result
            });
        _ ->
            error_unknown_action
    end.
```

```javascript
// JavaScript receives response
ws.onmessage = (event) => {
  const message = JSON.parse(event.data);
  if (message.type === 'response') {
    // Handle response
    console.log('Query result:', message.payload.data);
  }
};
```

---

## Memory and Performance

### Memory Limits

```
Typical AtomVM in browser:
  - Initial WASM module: ~5-10 MB
  - Erlang heap: 10-50 MB (configurable)
  - Default allocation: 16 MB

Set in AVM bundle config:
  heap_size: 50 * 1024 * 1024  // 50 MB max
```

### Performance Characteristics

```
Operation                   Time (AtomVM)
List matching               < 1ms (100 items)
Map lookup                  < 1ms (1000 keys)
Simple query                1-5ms
Complex query               10-50ms
WebSocket message           < 1ms (overhead)
Garbage collection          < 10ms (typical)
```

Comparable to native JavaScript performance for typical operations.

---

## Debugging

### Enable Logging

```javascript
// Increase verbosity
const app = atomvm_run({
  avm_data: buffer,
  debug: true,
  log_level: 'debug'
});
```

### WebSocket Debugging

```javascript
// Log all messages
const originalSend = WebSocket.prototype.send;
WebSocket.prototype.send = function(data) {
  console.log('→ Sent to Erlang:', JSON.parse(data));
  originalSend.call(this, data);
};

ws.onmessage = (event) => {
  console.log('← Received from Erlang:', JSON.parse(event.data));
};
```

### Erlang Debugging

```erlang
% Add debug tracing in Erlang
-module(rdf_store).

handle_call(Request, State) ->
    io:format("DEBUG: handle_call(~p)~n", [Request]),
    % ... actual logic
    io:format("DEBUG: responding with ~p~n", [Response]),
    {reply, Response, State}.
```

Output appears in browser console (via log handler).

---

## Limitations and Workarounds

### Limitation 1: No File I/O

**Problem:** Erlang code tries to read files
```erlang
{ok, File} = file:read_file("config.json"),  % ✗ Fails in browser
```

**Solution:** Inject data from JavaScript

```erlang
% Erlang expects configuration passed at startup
init(Config) ->
    % Config comes from JavaScript, not filesystem
    {ok, Config}.
```

```javascript
// JavaScript provides config
const config = {
  graphs: [],
  rules: [...]
};

atomvm_run({
  avm_data: buffer,
  init_args: [config]  // Passed to Erlang init/1
});
```

### Limitation 2: No Real Distribution

**Problem:** Erlang code tries to contact other nodes
```erlang
rpc:call('node@host', mod, fun, [])  % ✗ Fails in browser
```

**Solution:** Mock RPC via JavaScript

```erlang
% unrdf_atomvm/src/atomvm_rpc.erl
-module(rpc).

call(Node, Mod, Fun, Args) ->
    % Call JavaScript to reach real node
    atomvm_js_bridge:call_remote_node(Node, Mod, Fun, Args).
```

```javascript
// JavaScript makes actual HTTP call to real node
bridge.register('call_remote_node', async (node, mod, fun, args) => {
  const response = await fetch(`http://localhost:8080/rpc`, {
    method: 'POST',
    body: JSON.stringify({ node, mod, fun, args })
  });
  return response.json();
});
```

### Limitation 3: Limited ETS

**Problem:** Erlang uses many ETS features
```erlang
ets:select(cache, MatchSpec)  % Limited support
```

**Solution:** Use maps in process state

```erlang
% Instead of ETS
-record(state, {
    cache :: #{term() => term()}  % Map instead
}).

get_cached(Key, State) ->
    case maps:find(Key, State#state.cache) of
        {ok, Value} -> Value;
        error -> not_found
    end.
```

---

## Production Use

### Not Recommended For

- Handling production traffic (simulation only)
- Cryptographic operations (limited SSL support)
- Persistent storage (no disk access)
- Long-running operations (browser limitations)

### Good For

- **Development**: Full offline development
- **Testing**: Unit and integration tests
- **Demos**: Live demonstrations
- **Education**: Learning Erlang and UNRDF
- **Validation**: Pre-flight checks before real deployment

---

## See Also

- **04-DUAL-ADAPTERS.md** - How JS adapters switch between real and simulated
- **06-SETUP-JS-SIDE.md** - JavaScript integration details
- **07-INTEGRATION-TESTING.md** - Testing AtomVM simulation
