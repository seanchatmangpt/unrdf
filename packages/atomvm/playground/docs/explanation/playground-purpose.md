# Playground Purpose

**Understanding-oriented**: Why the playground exists and what problem it solves.

## The Problem

AtomVM runs Erlang/BEAM code in the browser and Node.js via WebAssembly. For production use, we need to validate that:

1. **Process lifecycle works** - Processes can be spawned, messages sent/received, crashes handled
2. **Supervision works** - Supervisors can restart failed processes
3. **KGC-4D integration works** - Processes can emit events to the knowledge graph
4. **State machine works** - Invalid operations are prevented (poka-yoke)
5. **Dual runtime works** - Both browser and Node.js runtimes work identically

## The Solution

The playground provides a **production validation suite** that proves the system cannot fail in production scenarios.

## What Makes It Production-Grade

**Not toys**: The Erlang modules are real production patterns:
- `process-test.erl` - Real process spawn, message passing, crash handling
- `supervision-test.erl` - Real supervisor restart behavior
- `kgc4d-test.erl` - Real KGC-4D event emission

**80/20 validation**: Focuses on the 20% that proves 80% of the system works:
- Process lifecycle (proves Erlang model works)
- State machine integrity (proves poka-yoke works)
- KGC-4D integration (proves integration works)
- Dual runtime (proves portability works)

**Poka-yoke design**: System cannot fail:
- All inputs validated
- State machine prevents invalid operations
- No defaults (explicit module names required)
- Resource validation before use

## The Mental Model

The playground validates that:

> **KGC-4D is the memory of the universe** and **Erlang is the nervous system that keeps that universe alive**.

The playground proves that:
- Erlang processes can live in the browser (via AtomVM WASM)
- Processes can emit events to KGC-4D
- Supervision trees can manage process lifecycles
- The system cannot fail (poka-yoke design)

## See Also

- [Big Bang 80/20 Methodology](./big-bang-80-20.md)
- [How-To Guides](../how-to/)
- [Reference](../reference/)

