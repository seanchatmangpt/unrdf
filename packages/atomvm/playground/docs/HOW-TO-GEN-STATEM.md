# How to Use gen_statem in AtomVM

## Overview

Production-ready `gen_statem` implementation for AtomVM with full KGC-4D integration and JavaScript bridge support.

## Production Implementation

### `gen_statem.erl` - Production State Machine

A production-grade state machine implementation with:
- **KGC-4D Integration**: All state transitions emit events to KGC-4D
- **JavaScript Bridge**: Full control from JavaScript
- **Supervision Support**: `start_link` for supervision trees
- **Error Handling**: Robust error handling and recovery
- **Lockout Protection**: Configurable max attempts and lockout duration
- **Auto-relock**: Configurable timeout for automatic relocking

**Features:**
- Two states: `locked` and `open`
- Configurable code, timeouts, and lockout behavior
- Complete KGC-4D event logging
- JavaScript bridge API
- Production-ready error handling

**Usage:**
```erlang
%% Start state machine (standalone)
{ok, Pid} = gen_statem:start().

%% Start state machine (linked for supervision)
{ok, Pid} = gen_statem:start_link().

%% Start with custom configuration
Config = #config{
    code = [1, 2, 3, 4],
    timeout_ms = 10000,
    max_attempts = 3,
    lockout_duration_ms = 60000
},
{ok, Pid} = gen_statem:start(Config).

%% Press buttons
gen_statem:button(1).
gen_statem:button(2).
gen_statem:button(3).
gen_statem:button(4).  % Correct code unlocks

%% Get current state
{ok, State, Data} = gen_statem:get_state().
%% Data contains: buttons, attempts, lockout_until

%% Stop
gen_statem:stop().
```

## Testing

Run the test module to see the state machine in action:

```bash
# Build the test module
pnpm run build:erlang gen_statem_test

# Run in Node.js
node src/cli.mjs public/gen_statem_test.avm
```

Or use the test suite:

```bash
pnpm test gen-statem
```

## Architecture

Both implementations use:

1. **Process-based State Machine**: Uses Erlang processes and message passing (AtomVM-compatible)
2. **State Loop**: Main loop receives messages and transitions states
3. **Event Handlers**: Functions handle events based on current state
4. **State Querying**: Can query current state via messages

## Differences from Full OTP gen_statem

- **No OTP Behavior**: Doesn't use `-behaviour(gen_statem)`
- **Manual Process Management**: Uses `spawn` instead of `gen_statem:start_link`
- **Simpler Callbacks**: Event handlers are regular functions, not OTP callbacks
- **AtomVM Compatible**: Works with AtomVM's subset of Erlang/OTP

## Example: Code Lock State Machine

The provided examples implement a code lock:

- **States**: `locked` (default), `open`
- **Code**: `[1, 2, 3, 4]` (hardcoded, can be configured)
- **Events**: Button presses (integers)
- **Transitions**:
  - `locked` + correct code → `open`
  - `locked` + wrong code → `locked` (reset)
  - `open` + any button → `open` (ignored)

## Extending for Your Use Case

To create your own state machine:

1. **Define States**: Use atoms or constants
2. **Define State Data**: Use records or maps
3. **Implement Event Handlers**: Functions that take (State, Event, Data) → (NewState, NewData)
4. **Implement Main Loop**: `receive` block that dispatches to handlers
5. **Export API**: Functions to start, send events, query state, stop

Example structure:
```erlang
-module(my_statem).
-export([start/0, event/1, get_state/0, stop/0]).

-define(STATE1, state1).
-define(STATE2, state2).

start() ->
    Pid = spawn(fun() -> loop(?STATE1, #{}) end),
    register(?MODULE, Pid),
    {ok, Pid}.

event(Event) ->
    ?MODULE ! {event, Event},
    ok.

loop(State, Data) ->
    receive
        {event, Event} ->
            {NewState, NewData} = handle_event(State, Event, Data),
            loop(NewState, NewData);
        {get_state, From} ->
            From ! {state, State, Data},
            loop(State, Data);
        stop ->
            ok
    end.

handle_event(?STATE1, Event, Data) ->
    %% Transition logic
    {?STATE2, Data#{event => Event}};
handle_event(?STATE2, Event, Data) ->
    {?STATE1, Data}.
```

## See Also

- `gen_statem.erl` - Production implementation
- `gen_statem_kgc.erl` - Integration test with KGC-4D
- `gen_statem_bridge.erl` - Bridge handler for JavaScript
- `HOW-TO-GEN-STATEM-KGC.md` - KGC-4D and JavaScript bridge integration guide

