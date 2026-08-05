# Pattern 9: Test Without a Framework

## Context

FleetPulse has reached the layer where **test without a framework** becomes necessary. The preceding patterns remain intact; this chapter adds one reversible capability to the working core.

## Problem

The system needs this capability without introducing shared mutable state, ambient execution authority, hidden failure, or tests that verify mock interactions instead of running behavior.

## Therefore

Node’s built-in test runner and strict assertions call the real pure function. No DI container, mock bean, or scripted collaborator exists.

## Executable AtomVM/JavaScript example

```javascript
test('fuel preserves coordinates', () => {
  const next = pureVehicleHandler(state, TelemetryMsg.fuel(0.5));
  assert.equal(next.lat, state.lat);
});
```

The complete example is implemented in [`../../examples/part-2.mjs`](../../examples/part-2.mjs). The public runtime API is exported from `@unrdf/atomvm` through `src/otp/index.mjs`.

## Chicago TDD proof

Run only this chapter’s state-based test:

```bash
node --test \
  --test-name-pattern="Pattern 9" \
  packages/atomvm/test/otp-patterns-chicago.test.mjs
```

The test constructs real domain objects and real runtime components. It contains no mock, spy, fake timer, scripted process runner, or interaction-count assertion. The admitted success condition is an observed state, lifecycle, receipt, or refusal.

## AtomVM proof

The real Generic UNIX AtomVM probe executes the equivalent BEAM semantics and emits:

```erlang
{otp_pattern, test_without_framework, ok}
```

The workflow requires all thirty markers and rejects any missing chapter marker.

## Resulting Context

The capability is now part of the same process graph as the earlier chapters. Its effects are bounded by explicit APIs, observed by receipts or state, and replayed by the executable test suite. Continue to the next pattern without replacing the working core.
