# Boardroom Story - Erlang Implementation

This document shows how the Erlang modules demonstrate the boardroom story using **REAL implementations** from `@unrdf/kgc-4d` and `@unrdf/hooks`.

## The Story

> "The dome above the Atlantic Boardroom was a lattice of hexagons and triangles... There was no screen at the head of the table. Just a narrow ring of light around the table, pulsing faintly at nanosecond rhythms."

> "Λ₀ registered. Global intent: 'Maximize long-term enterprise value, subject to planetary survivability constraints.' A = pending."

## Implementation

### Real Components Used

1. **@unrdf/kgc-4d** - Real KGC-4D event logging
   - `KGCStore` - RDF store with event logging
   - `appendEvent()` - Emit events to knowledge graph
   - `EVENT_TYPES` - Event type constants

2. **@unrdf/hooks** - Real knowledge hooks
   - `defineHook()` - Define hooks with triggers
   - `createHookRegistry()` - Create hook registry
   - `registerHook()` - Register hooks

3. **AtomVM** - Real Erlang/BEAM execution
   - Process swarms
   - Message passing
   - Supervision trees

### Erlang Modules

#### 1. `boardroom-swarm.erl` - Process Swarm

Demonstrates: **Process swarms emitting KGC-4D events**

```erlang
% Spawns multiple processes (swarm)
% Each process emits KGC-4D events
% Events sent to JavaScript bridge via io:format markers
io:format("KGC4D_BRIDGE:emit_event:~s:~p~n", [Type, Payload])
```

**What it proves**: Erlang processes (the "living layer") can emit events to KGC-4D (the "memory of the universe").

#### 2. `boardroom-hooks.erl` - Knowledge Hooks

Demonstrates: **Knowledge hooks processing events**

```erlang
% Register hooks via JavaScript bridge
io:format("KGC4D_BRIDGE:register_hook:quality_gate:before-add~n")

% Emit events that trigger hooks
io:format("KGC4D_BRIDGE:emit_event:CREATE:~p~n", [Payload])
```

**What it proves**: Knowledge hooks can filter/transform events, providing governance at microsecond scale.

#### 3. `boardroom-intent.erl` - Intent → Outcome

Demonstrates: **Λ (intent) → A (outcome) transformation**

```erlang
% Process intent (Λ)
io:format("KGC4D_BRIDGE:process_intent:~s:~p~n", [IntentId, Intent])

% Get outcome (A)
io:format("KGC4D_BRIDGE:get_outcome:~s~n", [IntentId])
```

**What it proves**: Intents expressed in natural language can be processed through hooks to generate outcomes.

## JavaScript Bridge

The bridge (`src/kgc4d-bridge.mjs`) connects Erlang processes to real implementations:

- **Event Emission**: Erlang → `KGCStore.appendEvent()` → KGC-4D
- **Hook Registration**: Erlang → `defineHook()` + `registerHook()` → Hooks registry
- **Intent Processing**: Erlang → `processIntent()` → Hooks → Outcome

## Bridge Interceptor

The interceptor (`src/bridge-interceptor.mjs`) parses Erlang `io:format` output:

- Intercepts `Module.print` and `Module.printErr`
- Parses `KGC4D_BRIDGE:command:args` markers
- Calls bridge methods
- Logs results

## How to See It Work

1. **Build an Erlang module:**
   ```bash
   cd packages/atomvm
   pnpm run build:erlang boardroom-swarm
   ```

2. **Start playground:**
   ```bash
   cd playground
   pnpm dev
   ```

3. **Open browser:** `http://localhost:3001`

4. **Enter module name:** `boardroom-swarm`

5. **Watch the logs:**
   - Process swarm spawns
   - Events emitted to KGC-4D
   - Bridge logs show real event IDs
   - Hooks process events
   - Intents transform to outcomes

## What the Logs Show

When you run `boardroom-swarm.erl`, you'll see:

```
Boardroom Swarm: Starting coordinator
Boardroom Swarm: Spawning 5 workers
Worker 1: Emitted event PROCESS_STARTED
[Bridge] Event emitted: PROCESS_STARTED (id: <real-uuid>)
Worker 2: Emitted event PROCESS_STARTED
[Bridge] Event emitted: PROCESS_STARTED (id: <real-uuid>)
...
```

**These are REAL events** in the KGC-4D store, not hardcoded data.

## Big Bang 80/20

This implementation focuses on the **20% that proves 80% works**:

- ✅ Process swarms can emit events (proves living layer works)
- ✅ Knowledge hooks can process events (proves governance works)
- ✅ Intents can transform to outcomes (proves transformation works)

**Not implemented** (20% effort, <20% value):
- Full µ(O) calculus with 8 operators
- Complete boardroom UI
- Full FMEA catalog

## The Mental Model

> **KGC-4D is the memory of the universe** and **Erlang is the nervous system that keeps that universe alive**.

The playground proves:
- Erlang processes (nervous system) can emit events to KGC-4D (memory)
- Knowledge hooks provide governance (planetary failsafes)
- Intents (Λ) transform to outcomes (A) through hooks

This is the boardroom story, implemented with **real code, not simulations**.

