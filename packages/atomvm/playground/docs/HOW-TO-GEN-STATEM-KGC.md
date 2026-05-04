# How to Use gen_statem with KGC-4D and JavaScript Bridge

## Overview

The `gen_statem` module is fully integrated with:
- **KGC-4D**: All state transitions emit events to KGC-4D
- **JavaScript Bridge**: Control state machine from JavaScript
- **Event Logging**: Complete history of state machine behavior

## Architecture

```
┌─────────────┐         ┌──────────────┐         ┌─────────────┐
│  JavaScript │─────────▶│ Bridge       │─────────▶│  Erlang    │
│             │◀────────│ Interceptor  │◀────────│ State       │
│             │         │              │         │ Machine     │
└─────────────┘         └──────────────┘         └─────────────┘
                              │
                              ▼
                        ┌─────────────┐
                        │  KGC-4D     │
                        │  Store      │
                        └─────────────┘
```

## KGC-4D Events

The state machine emits the following events to KGC-4D:

### 1. `STATE_TRANSITION`
Emitted when state changes (e.g., `locked` → `open`).

**Payload:**
```json
{
  "from": "locked",
  "to": "open",
  "trigger": "button",
  "digit": 4,
  "buttons": [1, 2, 3, 4]
}
```

### 2. `CODE_CORRECT`
Emitted when correct code is entered.

**Payload:**
```json
{
  "code": [1, 2, 3, 4],
  "buttons": [1, 2, 3, 4]
}
```

### 3. `CODE_INCORRECT`
Emitted when wrong code is entered.

**Payload:**
```json
{
  "attempted": [9, 9, 9, 9],
  "expected": [1, 2, 3, 4]
}
```

### 4. `BUTTON_PRESSED`
Emitted on each button press (while locked).

**Payload:**
```json
{
  "digit": 1,
  "sequence": [1],
  "state": "locked"
}
```

### 5. `BUTTON_IGNORED`
Emitted when button is pressed while door is open.

**Payload:**
```json
{
  "state": "open",
  "reason": "door_already_open"
}
```

### 6. `STATE_MACHINE_STOPPED`
Emitted when state machine stops.

**Payload:**
```json
{
  "state": "open",
  "buttons": []
}
```

## JavaScript Bridge API

### Import Bridge

```javascript
import { getGenStatemBridge } from './gen-statem-bridge.mjs';

const genStatemBridge = getGenStatemBridge({
  log: console.log,
  sendCommand: (cmd) => {
    // Commands sent via Module.print
    window.Module.print(`${cmd}\n`);
  }
});
```

### Press Button

```javascript
// Press button 1
genStatemBridge.button(1);

// Press button 2
genStatemBridge.button(2);
```

### Get State

```javascript
// Get current state
const { state, buttons } = await genStatemBridge.getState();
console.log(`State: ${state}, Buttons: ${buttons}`);
```

### Stop State Machine

```javascript
// Stop state machine
genStatemBridge.stop();
```

## Erlang API

### Start State Machine

```erlang
{ok, Pid} = gen_statem_simple:start().
```

### Press Button

```erlang
gen_statem_simple:button(1).
gen_statem_simple:button(2).
gen_statem_simple:button(3).
gen_statem_simple:button(4).  % Correct code unlocks
```

### Get State

```erlang
{State, Buttons} = gen_statem_simple:get_state().
```

### Stop

```erlang
gen_statem_simple:stop().
```

## Complete Example

### Erlang Module

```erlang
-module(my_app).
-export([start/0]).

start() ->
    %% Start state machine
    {ok, _Pid} = gen_statem:start(),
    
    %% Press buttons
    gen_statem:button(1),
    gen_statem:button(2),
    gen_statem:button(3),
    gen_statem:button(4),  % Unlocks
    
    %% Get state
    {ok, State, Data} = gen_statem:get_state(),
    io:format("State: ~p, Data: ~p~n", [State, Data]),
    
    ok.
```

### JavaScript Module

```javascript
import { getGenStatemBridge } from './gen-statem-bridge.mjs';

// Initialize bridge
const genStatemBridge = getGenStatemBridge();

// Control state machine
genStatemBridge.button(1);
genStatemBridge.button(2);
genStatemBridge.button(3);
genStatemBridge.button(4);  // Unlocks

// Get state
const { state, buttons } = await genStatemBridge.getState();
console.log(`State: ${state}, Buttons: ${buttons}`);
```

## Testing

Run the integration test:

```bash
# Build test module
pnpm run build:erlang gen_statem_kgc

# Run test
node src/cli.mjs public/gen_statem_kgc.avm
```

Or use the test suite:

```bash
pnpm test gen-statem-integration
```

## KGC-4D Event Query

After running the state machine, query KGC-4D for events:

```javascript
import { KGCStore } from '@unrdf/kgc-4d';

const store = new KGCStore();
const events = await store.query({
  type: 'STATE_TRANSITION'
});

console.log('State transitions:', events);
```

## See Also

- `gen_statem.erl` - Production state machine implementation
- `gen_statem_kgc.erl` - Integration test module
- `gen-statem-bridge.mjs` - JavaScript bridge API
- `HOW-TO-GEN-STATEM.md` - Basic gen_statem usage

