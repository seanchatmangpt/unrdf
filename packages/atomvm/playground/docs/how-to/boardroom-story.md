# How-To: Run Boardroom Story Modules

**Task-oriented**: Run the boardroom story Erlang modules that demonstrate process swarms, knowledge hooks, and intent → outcome transformation using REAL implementations.

## Problem

You want to see the boardroom story in action:
- Process swarms emitting KGC-4D events
- Knowledge hooks processing events
- Intent (Λ) → Outcome (A) transformation

## Solution

### 1. Build Boardroom Module

```bash
cd packages/atomvm/playground
pnpm run build:erlang boardroom-swarm
```

This creates `playground/public/boardroom-swarm.avm`.

**Poka-Yoke**: The build script will fail if the module doesn't exist - you must create the `.erl` file first.

### 2. Start Playground

```bash
cd playground
pnpm dev
```

### 3. Open Browser

Navigate to `http://localhost:3001`

### 4. Run Module

1. Enter module name: `boardroom-swarm`
2. Click "Run Validation Suite" (optional - validates system)
3. Or use AtomVM runtime directly (if integrated)

### 5. Watch Logs

You'll see:
- Process swarm spawning workers
- Events being emitted to KGC-4D
- Bridge logs showing real event IDs
- Hooks processing events
- Intents transforming to outcomes

## What You'll See

### Process Swarm (`boardroom-swarm.erl`)

```
Boardroom Swarm: Starting coordinator
Boardroom Swarm: Spawning 5 workers
Worker 1: Emitted event PROCESS_STARTED
[Bridge] Event emitted: PROCESS_STARTED (id: <real-uuid>)
Worker 2: Emitted event PROCESS_STARTED
[Bridge] Event emitted: PROCESS_STARTED (id: <real-uuid>)
...
```

### Knowledge Hooks (`boardroom-hooks.erl`)

```
Boardroom Hooks: Registering hooks...
[Bridge] Hook registered: quality_gate
[Bridge] Hook registered: validate_intent
Boardroom Hooks: Emitting events to trigger hooks...
[Bridge] Event emitted: CREATE (id: <real-uuid>)
[Bridge] Event emitted: INTENT (id: <real-uuid>)
```

### Intent → Outcome (`boardroom-intent.erl`)

```
Boardroom Intent: Processing intent intent-1
[Bridge] Event emitted: INTENT (id: <real-uuid>)
[Bridge] Intent processed: intent-1 → Outcome: accepted
Boardroom Intent: Outcome for intent-1: {"accepted":true,...}
```

## Real Implementations

All events are **REAL**:
- Events are stored in KGC-4D store (not hardcoded)
- Hooks are registered in real hook registry (not simulated)
- Intent outcomes are generated through real processing (not fake)

## Validate with OTEL

The playground is instrumented with OpenTelemetry spans. Validate the system works:

```bash
# From project root
node validation/atomvm-playground.mjs
```

This validates:
- Bridge operations (event emission, hook registration, intent processing)
- Runtime operations (WASM load, execution, state transitions)
- Erlang process lifecycle (process spawn, event emission)
- Complete boardroom story flow

**OTEL spans are the only source of truth** - they prove the system actually works, not just that code exists.

## See Also

- [Boardroom Story Documentation](../BOARDROOM-STORY.md)
- [Validate KGC-4D Integration](./validate-kgc4d.md)
- [Validation API Reference](../reference/validation-api.md)

