# Pattern 17: Let It Crash

## Context

FleetPulse has reached the layer where **let it crash** becomes necessary. The preceding patterns remain intact; this chapter adds one reversible capability to the working core.

## Problem

The system needs this capability without introducing shared mutable state, ambient execution authority, hidden failure, or tests that verify mock interactions instead of running behavior.

## Therefore

A `tell` handler exception terminates the child. The supervisor observes abnormal termination, discards corrupt state, and manufactures a fresh child.

## Executable AtomVM/JavaScript example

```javascript
const ref = supervisor.supervise('counter', 0, counterHandler);
ref.tell(CounterMsg.crash());
// The same ref later targets a fresh process at state 0.
```

The complete example is implemented in [`../../examples/part-4.mjs`](../../examples/part-4.mjs). The public runtime API is exported from `@unrdf/atomvm` through `src/otp/index.mjs`.

## Chicago TDD proof

Run only this chapter’s state-based test:

```bash
node --test \
  --test-name-pattern="Pattern 17" \
  packages/atomvm/test/otp-patterns-chicago.test.mjs
```

The test constructs real domain objects and real runtime components. It contains no mock, spy, fake timer, scripted process runner, or interaction-count assertion. The admitted success condition is an observed state, lifecycle, receipt, or refusal.

## AtomVM proof

The real Generic UNIX AtomVM probe executes the equivalent BEAM semantics and emits:

```erlang
{otp_pattern, let_it_crash, ok}
```

The workflow requires all thirty markers and rejects any missing chapter marker.

## Resulting Context

The capability is now part of the same process graph as the earlier chapters. Its effects are bounded by explicit APIs, observed by receipts or state, and replayed by the executable test suite. Continue to the next pattern without replacing the working core.
