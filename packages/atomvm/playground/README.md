# AtomVM Playground

Production validation playground for AtomVM - validates processes, supervision, KGC-4D integration.

## Quick Start

1. **Install dependencies:**
   ```bash
   cd packages/atomvm/playground
   pnpm install
   ```

2. **Start playground:**
   ```bash
   pnpm dev
   ```

3. **Open browser:**
   ```
   http://localhost:3001
   ```

4. **Enter module name (e.g., `hello_world`) and click "Run Validation Suite"**

   Or run validation from CLI:
   ```bash
   pnpm validate hello_world
   ```

## What This Validates

**80/20 Core Validations** (proves 80% of system works):

1. **Process Lifecycle** - Spawn, message, crash, restart
2. **State Machine Integrity** - All state transitions valid
3. **KGC-4D Event Integration** - Events reach 4D engine
4. **Dual Runtime** - Browser and Node.js both work
5. **Supervision Tree** - Supervisor restart behavior

## Innovative Layer: Hook Primitives

**Opposite of Big Bang 80/20**: Instead of using existing patterns, this creates a new paradigm where **hooks are first-class primitives in Erlang/AtomVM**.

### Key Innovations

1. **Hooks as Erlang Primitives**: Hooks are defined in Erlang, not JavaScript
2. **800ns Execution**: Hook execution at 800ns via JIT-compiled chains
3. **Process-Hook Fusion**: Processes can BE hooks, enabling supervision and fault tolerance
4. **Native Performance**: Hook execution is native to Erlang process model, not external calls

### Usage

```erlang
%% Define a hook in Erlang
QualityGateHook = hook_primitives:define(
    quality_gate,
    before_add,
    fun(Event) -> maps:get(<<"value">>, Event) > 0 end
),

%% Register the hook
hook_primitives:register(QualityGateHook),

%% Execute hooks from Erlang (800ns execution)
hook_primitives:execute(before_add, EventData).
```

### Process-Hook Fusion

```erlang
%% Spawn a process that IS a hook
hook_process:spawn_hook_process(quality_gate, before_add, ValidateFun),

%% Execute hook via process message (enables supervision)
hook_process:execute_hook(quality_gate, EventData).
```

See `hook_example.erl` and `hook_process.erl` for complete examples.

## Production Modules

The playground includes production Erlang modules (not toys):

- `process-test.erl` - Process spawn, message passing, crash handling
- `supervision-test.erl` - Supervisor restart behavior
- `kgc4d-test.erl` - KGC-4D event emission
- `boardroom-swarm.erl` - Process swarm emitting KGC-4D events (demonstrates boardroom story)
- `boardroom-hooks.erl` - Knowledge hooks processing events (demonstrates governance)
- `boardroom-intent.erl` - Intent (Λ) → Outcome (A) transformation (demonstrates boardroom story)
- `hook_primitives.erl` - **Innovative Layer**: Hooks as first-class Erlang primitives (800ns execution)
- `hook_example.erl` - Demonstrates Erlang hook primitives (hooks defined in Erlang, executed at 800ns)
- `hook_process.erl` - **Process-Hook Fusion**: Processes that ARE hooks (enables supervision)

### Boardroom Story Modules

These modules demonstrate the boardroom story using **REAL implementations** from `@unrdf/kgc-4d` and `@unrdf/hooks`:

1. **Build a boardroom module:**
   ```bash
   cd packages/atomvm/playground
   pnpm run build:erlang boardroom-swarm
   ```

2. **Run in playground:**
   - Enter module name: `boardroom-swarm`
   - Watch logs show real KGC-4D events being emitted

3. **See the story:**
   - Process swarms spawn and emit events
   - Knowledge hooks process events
   - Intents transform to outcomes
   - All using real implementations, not hardcoded data

4. **Validate with OTEL:**
   ```bash
   node validation/atomvm-playground.mjs
   ```
   This validates the system using OTEL spans from running Erlang/JS code.

See [BOARDROOM-STORY.md](./BOARDROOM-STORY.md) for details.

## Documentation

Complete documentation is organized using the [Diataxis](https://diataxis.fr/) framework:

- **[Tutorials](./docs/tutorials/)** - Learn how to validate the system works
- **[How-To Guides](./docs/how-to/)** - Solve specific validation problems
- **[Reference](./docs/reference/)** - Complete API documentation and production scenarios
- **[Explanations](./docs/explanation/)** - Understand the playground purpose and methodology

Start with: [Getting Started Tutorial](./docs/tutorials/01-validate-system.md)

## Development

```bash
# Run tests
pnpm test

# Run tests in watch mode
pnpm test:watch

# Run validation suite
pnpm validate

# Build for production
pnpm build

# Preview production build
pnpm preview
```

## Architecture

- **Validation Suite**: Orchestrates all validations
- **Validators**: Process, supervision, KGC-4D, runtime validators
- **Production Modules**: Real Erlang modules (not toys)
- **Browser UI**: Minimal UI showing validation results
- **Diataxis Docs**: Complete documentation structure

## Requirements

- **Browser**: Module name required, Cross-Origin-Isolation enabled
- **Node.js**: Node.js 18+, Erlang toolchain for building modules
- **Build Tools**: `erlc` and `packbeam` in PATH

## License

MIT

