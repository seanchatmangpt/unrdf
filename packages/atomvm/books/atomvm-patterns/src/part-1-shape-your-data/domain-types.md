# Pattern 5: Domain Types Over Primitives

## Context

FleetPulse has reached the layer where **domain types over primitives** becomes necessary. The preceding patterns remain intact; this chapter adds one reversible capability to the working core.

## Problem

The system needs this capability without introducing shared mutable state, ambient execution authority, hidden failure, or tests that verify mock interactions instead of running behavior.

## Therefore

Validated constructors such as `VehicleId`, `Position`, and `FuelPercent` manufacture frozen domain values and reject illegal ranges at admission.

## Executable AtomVM/JavaScript example

```javascript
const id = VehicleId('V-1005');
const position = Position(34.1, -118.2, Date.now());
const fuel = FuelPercent(75);
```

The complete example is implemented in [`../../examples/part-1.mjs`](../../examples/part-1.mjs). The public runtime API is exported from `@unrdf/atomvm` through `src/otp/index.mjs`.

## Chicago TDD proof

Run only this chapter’s state-based test:

```bash
node --test \
  --test-name-pattern="Pattern 5" \
  packages/atomvm/test/otp-patterns-chicago.test.mjs
```

The test constructs real domain objects and real runtime components. It contains no mock, spy, fake timer, scripted process runner, or interaction-count assertion. The admitted success condition is an observed state, lifecycle, receipt, or refusal.

## AtomVM proof

The real Generic UNIX AtomVM probe executes the equivalent BEAM semantics and emits:

```erlang
{otp_pattern, domain_types_over_primitives, ok}
```

The workflow requires all thirty markers and rejects any missing chapter marker.

## Resulting Context

The capability is now part of the same process graph as the earlier chapters. Its effects are bounded by explicit APIs, observed by receipts or state, and replayed by the executable test suite. Continue to the next pattern without replacing the working core.
