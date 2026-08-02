# Pattern 13: Ask with Timeout

## Context

FleetPulse has reached the layer where **ask with timeout** becomes necessary. The preceding patterns remain intact; this chapter adds one reversible capability to the working core.

## Problem

The system needs this capability without introducing shared mutable state, ambient execution authority, hidden failure, or tests that verify mock interactions instead of running behavior.

## Therefore

`ask(message, timeoutMs)` bounds the entire request, including handler execution—not merely queue admission. Timeout produces `ASK_TIMEOUT_REFUSED`.

## Executable AtomVM/JavaScript example

```javascript
await vehicle.ask(VehicleMsg.getState(), 500);
// A slow handler rejects with ASK_TIMEOUT_REFUSED.
```

The complete example is implemented in [`../../examples/part-3.mjs`](../../examples/part-3.mjs). The public runtime API is exported from `@unrdf/atomvm` through `src/otp/index.mjs`.

## Chicago TDD proof

Run only this chapter’s state-based test:

```bash
node --test \
  --test-name-pattern="Pattern 13" \
  packages/atomvm/test/otp-patterns-chicago.test.mjs
```

The test constructs real domain objects and real runtime components. It contains no mock, spy, fake timer, scripted process runner, or interaction-count assertion. The admitted success condition is an observed state, lifecycle, receipt, or refusal.

## AtomVM proof

The real Generic UNIX AtomVM probe executes the equivalent BEAM semantics and emits:

```erlang
{otp_pattern, ask_with_timeout, ok}
```

The workflow requires all thirty markers and rejects any missing chapter marker.

## Resulting Context

The capability is now part of the same process graph as the earlier chapters. Its effects are bounded by explicit APIs, observed by receipts or state, and replayed by the executable test suite. Continue to the next pattern without replacing the working core.
