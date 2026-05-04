# AtomVM Playground Documentation

Production validation playground for AtomVM - validates processes, supervision, KGC-4D integration.

## Documentation Structure

This documentation follows the [Diataxis](https://diataxis.fr/) framework:

- **[Tutorials](./tutorials/)** - Learn how to validate the system works
- **[How-To Guides](./how-to/)** - Solve specific validation problems
- **[Reference](./reference/)** - Complete API documentation and production scenarios
- **[Explanations](./explanation/)** - Understand the playground purpose and methodology

## Quick Start

1. **Build an Erlang validation module:**
   ```bash
   cd packages/atomvm
   pnpm run build:erlang process-test
   ```

2. **Start playground:**
   ```bash
   cd playground
   pnpm dev
   ```

3. **Open browser:**
   ```
   http://localhost:3001
   ```

4. **Enter module name and click "Run Validation Suite"**

## What This Playground Validates

**80/20 Core Validations** (proves 80% of system works):

1. **Process Lifecycle** - Spawn, message, crash, restart
2. **State Machine Integrity** - All state transitions valid
3. **KGC-4D Event Integration** - Events reach 4D engine
4. **Dual Runtime** - Browser and Node.js both work
5. **Supervision Tree** - Supervisor restart behavior

## Production Modules

The playground includes production Erlang modules (not toys):

- `process-test.erl` - Process spawn, message passing, crash handling
- `supervision-test.erl` - Supervisor restart behavior
- `kgc4d-test.erl` - KGC-4D event emission

## Next Steps

- Start with: [Getting Started Tutorial](./tutorials/01-validate-system.md)
- Learn: [How to Validate Processes](./how-to/validate-processes.md)
- Reference: [Validation API](./reference/validation-api.md)

