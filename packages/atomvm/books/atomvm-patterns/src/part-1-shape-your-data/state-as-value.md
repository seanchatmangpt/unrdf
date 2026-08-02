# Pattern 3: State as Value

## Context

FleetPulse has reached the layer where **state as value** becomes necessary. The preceding patterns remain intact; this chapter adds one reversible capability to the working core.

## Problem

The system needs this capability without introducing shared mutable state, ambient execution authority, hidden failure, or tests that verify mock interactions instead of running behavior.

## Therefore

State is a deeply frozen value. Wither functions return new state objects; they never mutate the prior snapshot.

## Executable AtomVM/JavaScript example

```javascript
const before = initialVehicleState(VehicleId('V-1003'));
const after = telemetryHandler(
  before,
  Telemetry.fuelLevel(before.id, FuelPercent(48)),
);
// before.fuel.value === 100; after.fuel.value === 48
```

The complete example is implemented in [`../../examples/part-1.mjs`](../../examples/part-1.mjs). The public runtime API is exported from `@unrdf/atomvm` through `src/otp/index.mjs`.

## Chicago TDD proof

Run only this chapter’s state-based test:

```bash
node --test \
  --test-name-pattern="Pattern 3" \
  packages/atomvm/test/otp-patterns-chicago.test.mjs
```

The test constructs real domain objects and real runtime components. It contains no mock, spy, fake timer, scripted process runner, or interaction-count assertion. The admitted success condition is an observed state, lifecycle, receipt, or refusal.

## AtomVM proof

The real Generic UNIX AtomVM probe executes the equivalent BEAM semantics and emits:

```erlang
{otp_pattern, state_as_value, ok}
```

The workflow requires all thirty markers and rejects any missing chapter marker.

## Resulting Context

The capability is now part of the same process graph as the earlier chapters. Its effects are bounded by explicit APIs, observed by receipts or state, and replayed by the executable test suite. Continue to the next pattern without replacing the working core.
