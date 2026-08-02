# Introduction: Build Your Project in Layers

You know JavaScript. Now learn to think in isolated processes.

This book is a one-for-one AtomVM/JavaScript adaptation of the JOTP pattern language. It preserves the original five-layer progression and the thirty named FleetPulse patterns while replacing Java virtual-thread examples with `@unrdf/atomvm` processes, mailboxes, supervisors, state machines, timers, event managers, receipts, and a real AtomVM verification probe.

```text
┌─────────────────────────────────────────┐
│ Part V: Workers & Assembly              │ StateMachine, EventManager,
│ Assemble components into applications   │ ProcTimer, Parallel
├─────────────────────────────────────────┤
│ Part IV: Lifecycle                      │ Supervisor, ProcLib,
│ Plan fault tolerance and recovery       │ ProcLink, ProcMonitor
├─────────────────────────────────────────┤
│ Part III: Process Boundaries            │ Proc, ProcRef, ProcRegistry,
│ Wrap pure logic in concurrent actors    │ ExitSignal
├─────────────────────────────────────────┤
│ Part II: Functional Core                │ pure `(state, message) => state`,
│ Pure functions, testable logic          │ Result
├─────────────────────────────────────────┤
│ Part I: Shape Your Data                 │ frozen values, tagged protocols,
│ Immutable types and message protocols   │ exhaustive dispatch
└─────────────────────────────────────────┘
```

## The running example: FleetPulse

FleetPulse monitors vehicle telemetry, routes, health, alerts, and lifecycle transitions. Each chapter adds one capability without discarding the working core built by earlier chapters.

## Executable-book contract

Every chapter maps to three concrete artifacts:

1. A real JavaScript example in `../examples/part-N.mjs`.
2. A Chicago/Detroit state-based test named `Pattern N — ...` in `../../../test/otp-patterns-chicago.test.mjs`.
3. A real AtomVM marker emitted by `../../../examples/erlang/otp_patterns_probe.erl`.

The test suite contains no mocks, spies, fake timers, interaction assertions, or injected process runners. Tests observe domain state, process state, restart topology, receipts, and real runtime output.

Run the complete JavaScript proof:

```bash
node --test packages/atomvm/test/otp-patterns-chicago.test.mjs
```

The GitHub workflow builds AtomVM v0.6.6, packages the Erlang probe, executes it, and requires thirty distinct `{otp_pattern, Name, ok}` markers plus `{atomvm_otp_patterns_alive,30}`.
