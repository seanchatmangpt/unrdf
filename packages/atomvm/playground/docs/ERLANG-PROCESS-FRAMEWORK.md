# Erlang-Like Process Framework - Erlang State + JavaScript Callbacks

**Architecture**: All process state stored in Erlang, JavaScript provides callbacks only.

## Overview

The framework provides an Erlang-like process model where:
- **State Management**: All process state (mailbox, links, monitors) stored in Erlang ETS tables
- **JavaScript Callbacks**: Init and message handler functions registered in JavaScript, invoked by Erlang via bridge
- **Thin JS API**: JavaScript API is a thin wrapper that communicates with Erlang via bridge commands

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│ JavaScript Layer (Thin API Wrapper)                    │
│ - spawn(name, initFn, handleFn)                        │
│ - send(name, message)                                  │
│ - link(pid1, pid2)                                     │
│ - monitor(pid)                                          │
│ - Supervisor class                                      │
│                                                         │
│ Callback Registry:                                     │
│ - Map<callbackId, { initFn, handleFn }>                │
└─────────────────┬─────────────────────────────────────┘
                   │ Bridge Commands (bidirectional)
                   │ JS→Erlang: PROCESS_FRAMEWORK:command:args
                   │ Erlang→JS: JS_CALLBACK:type:callbackId:pid:args
┌─────────────────▼─────────────────────────────────────┐
│ Erlang Layer (State Management)                        │
│ - process_framework.erl (core implementation)          │
│ - process_supervisor.erl (supervision)                  │
│ - ETS tables: mailbox, links, monitors, states        │
│ - Process registry: name → pid mapping                 │
│ - Callback registry: callbackId → pid (for routing)   │
└─────────────────────────────────────────────────────────┘
```

## Core Concepts

### Process State (Erlang)

All process state is stored in Erlang ETS tables:
- **Mailbox**: `process_mailboxes` ETS table `{pid, queue}`
- **Links**: `process_links` ETS table `{pid, set of linked pids}`
- **Monitors**: `process_monitors` ETS table `{pid, set of {ref, monitor_pid}}`
- **States**: `process_states` ETS table `{pid, state}`
- **Registry**: `process_registry` ETS table `{name, pid}`

### JavaScript Callbacks

JavaScript provides only callback functions:
- **Init Callback**: Called when process starts
- **Handle Callback**: Called when process receives a message

Callbacks are registered in JavaScript and invoked by Erlang via bridge commands.

### Bridge Communication

**JavaScript → Erlang**:
- `PROCESS_FRAMEWORK:spawn:name:callbackId:options` - Spawn process
- `PROCESS_FRAMEWORK:send:name:message` - Send message
- `PROCESS_FRAMEWORK:link:pid1:pid2` - Link processes
- `PROCESS_FRAMEWORK:monitor:pid:monitorPid` - Monitor process
- `PROCESS_FRAMEWORK:exit:pid:reason` - Exit process

**Erlang → JavaScript**:
- `JS_CALLBACK:init:callbackId:pid:` - Invoke init callback
- `JS_CALLBACK:handle:callbackId:pid:message` - Invoke message handler

**JavaScript → Erlang (Results)**:
- `PROCESS_FRAMEWORK:callback_result:callbackId:pid:result` - Callback result
- `PROCESS_FRAMEWORK:callback_error:callbackId:pid:error` - Callback error

## API Reference

### spawn(name, initFn, handleFn, options)

Spawn a new process. State is stored in Erlang, callbacks are registered in JavaScript.

```javascript
import { spawn } from './erlang-process.mjs';

const process = spawn(
  'my_process',
  async () => {
    // Initialization callback (invoked by Erlang)
    return { initialized: true };
  },
  async (message) => {
    // Message handler callback (invoked by Erlang)
    if (message.type === 'ping') {
      return { type: 'pong' };
    }
  },
  {
    mailboxMaxSize: 1000, // Optional
  }
);
```

**Poka-Yoke**:
- Validates `name` is non-empty string
- Validates `initFn` and `handleFn` are functions
- Prevents duplicate process names (validated in Erlang)

**Architecture**:
1. JavaScript registers callbacks in `process-callback-registry.mjs`
2. JavaScript sends `PROCESS_FRAMEWORK:spawn:name:callbackId:options` to Erlang
3. Erlang spawns process and stores state in ETS tables
4. Erlang invokes init callback via `JS_CALLBACK:init:callbackId:pid:`
5. JavaScript callback executes and returns result
6. Result routed back to Erlang process via `PROCESS_FRAMEWORK:callback_result:callbackId:pid:result`

### send(name, message)

Send message to process by name. Message is stored in Erlang mailbox.

```javascript
import { send } from './erlang-process.mjs';

send('my_process', { type: 'ping', data: 'hello' });
```

**Poka-Yoke**:
- Throws error if process not found (validated in Erlang)
- Throws error if process is dead (validated in Erlang)
- Throws error if mailbox is full (validated in Erlang)

**Architecture**:
1. JavaScript sends `PROCESS_FRAMEWORK:send:name:message` to Erlang
2. Erlang looks up process by name in `process_registry` ETS table
3. Erlang validates process is alive (checks `process_states` ETS table)
4. Erlang adds message to mailbox in `process_mailboxes` ETS table
5. Erlang process receives message and invokes JavaScript handle callback

### Process Methods

#### process.send(message)

Send message to process (same as `send(name, message)`).

#### process.link(targetProcess)

Link to another process. Links are stored in Erlang `process_links` ETS table.

**Poka-Yoke**:
- Throws error if either process is dead (validated in Erlang)
- Creates bidirectional link (both processes linked)

#### process.monitor(targetProcess)

Monitor another process. Monitors are stored in Erlang `process_monitors` ETS table.

**Poka-Yoke**:
- Throws error if target process is dead (validated in Erlang)
- Returns monitor reference

#### process.exit(reason)

Exit process. State updated in Erlang `process_states` ETS table.

#### process.kill()

Kill process (forceful termination).

### Supervisor

Supervisor manages child processes. Supervision state stored in Erlang.

```javascript
import { Supervisor } from './erlang-process.mjs';

const supervisor = new Supervisor('my_supervisor', 'one_for_one');

const child = supervisor.startChild({
  name: 'child_process',
  initFn: async () => ({ initialized: true }),
  handleFn: async (message) => {
    // Handle message
  },
});
```

**Restart Strategies**:
- `one_for_one`: Restart only the failed child
- `one_for_all`: Restart all children
- `rest_for_one`: Restart failed child and all children started after it

**Architecture**:
- Supervision state stored in Erlang `process_supervisor.erl`
- Child processes spawned via `process_framework:spawn_process/3`
- Restart logic handled in Erlang supervisor loop

## Poka-Yoke Design

### JavaScript Layer

**Input Validation**:
- Non-empty strings for process names
- Functions for callbacks
- Valid process instances for links/monitors

**State Validation**: None (state is in Erlang)

### Erlang Layer

**State Validation**:
- Process states: `initialized`, `running`, `waiting`, `terminated`, `error`
- Mailbox size limits
- Process existence checks
- Link/monitor validity checks

**Operation Validation**:
- Cannot send to dead process
- Cannot link to dead process
- Cannot monitor dead process
- Cannot spawn duplicate names

## Callback Result Routing

**Problem**: When Erlang invokes JavaScript callbacks, results must be routed back to the specific Erlang process waiting for them.

**Solution**:
1. Erlang registers callback in `callback_registry` ETS table: `{callbackId, pid}`
2. Erlang sends `JS_CALLBACK:init:callbackId:pid:` or `JS_CALLBACK:handle:callbackId:pid:message`
3. JavaScript bridge interceptor invokes callback
4. JavaScript sends result via `PROCESS_FRAMEWORK:callback_result:callbackId:pid:result`
5. Erlang process loop receives result and routes to waiting process via `callback_registry`
6. Erlang process receives `{js_callback_result, CallbackId, Result}` message
7. Erlang unregisters callback from `callback_registry`

## State Storage Details

### Erlang ETS Tables

- `process_registry`: `{name, pid}` - Process name to PID mapping
- `process_mailboxes`: `{pid, queue}` - Process mailboxes (queue of messages)
- `process_links`: `{pid, set}` - Process links (set of linked PIDs)
- `process_monitors`: `{pid, set}` - Process monitors (set of {ref, monitor_pid})
- `process_monitored_by`: `{pid, set}` - Processes monitoring this process
- `process_states`: `{pid, state}` - Process states (initialized, running, waiting, terminated, error)
- `callback_registry`: `{callbackId, pid}` - Callback ID to waiting process PID mapping

### JavaScript State

**Minimal State** (thin wrapper):
- Process: `{ name, pid, callbackId }`
- Supervisor: `{ name, strategy, children: Map<name, Process> }`

**No State Storage**:
- No mailboxes
- No links
- No monitors
- No process states

## Usage Examples

### Basic Process

```javascript
import { spawn, send } from './erlang-process.mjs';

// Spawn process
const process = spawn(
  'echo_server',
  async () => {
    console.log('Process initialized');
    return { ready: true };
  },
  async (message) => {
    if (message.type === 'echo') {
      console.log('Echo:', message.data);
      return { echoed: message.data };
    }
  }
);

// Send message
send('echo_server', { type: 'echo', data: 'Hello, World!' });
```

### Process Links

```javascript
import { spawn } from './erlang-process.mjs';

const p1 = spawn('process1', async () => ({}), async () => {});
const p2 = spawn('process2', async () => ({}), async () => {});

// Link processes (state stored in Erlang)
p1.link(p2);

// If p1 dies, p2 dies (and vice versa)
p1.kill(); // p2 also dies
```

### Supervision

```javascript
import { Supervisor } from './erlang-process.mjs';

const supervisor = new Supervisor('app_supervisor', 'one_for_one');

// Start child
const worker = supervisor.startChild({
  name: 'worker',
  initFn: async () => ({ initialized: true }),
  handleFn: async (message) => {
    if (message.type === 'work') {
      // Do work
      return { done: true };
    }
  },
});

// If worker crashes, supervisor restarts it automatically
```

## Building Erlang Modules

Erlang modules must be compiled before use:

```bash
cd packages/atomvm/playground
pnpm build:erlang process_framework
pnpm build:erlang process_supervisor
```

**Requirements**:
- Erlang installed (via `asdf` or system package manager)
- `erlc` command available
- `packbeam` command available

**Output**: `.avm` files in `public/` directory

## Testing

Stress tests validate the framework under load:

```bash
cd packages/atomvm/playground
pnpm test
```

**Note**: Tests use simplified state checks (state is in Erlang, not directly accessible from JavaScript).

## Limitations

1. **State Access**: JavaScript cannot directly access Erlang state (mailbox, links, monitors). State queries would require additional bridge commands.

2. **Callback Routing**: Callback results are routed via ETS table lookups. Timeout handling cleans up stale registrations.

3. **Process Discovery**: Finding processes by state or properties requires Erlang queries (not yet implemented in JS API).

## Future Enhancements

- State query API (get mailbox size, check if process is alive, etc.)
- Process discovery (list processes by state, find processes by property)
- Direct Erlang process communication (without JavaScript callbacks)
- Process groups and namespaces
- Distributed process registry
