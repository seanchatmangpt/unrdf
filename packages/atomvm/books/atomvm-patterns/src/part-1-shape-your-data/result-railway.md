# Pattern 4: Result Railway

## Context

FleetPulse has reached the layer where **result railway** becomes necessary. The preceding patterns remain intact; this chapter adds one reversible capability to the working core.

## Problem

The system needs this capability without introducing shared mutable state, ambient execution authority, hidden failure, or tests that verify mock interactions instead of running behavior.

## Therefore

`Result.ok` and `Result.err` expose `map`, `flatMap`, `peek`, `recover`, `fold`, and extraction operations. Exceptions are captured at the edge with `Result.of`.

## Executable AtomVM/JavaScript example

```javascript
const address = coordinatePipeline('[34.1478,-118.1445]')
  .map(value => value.label)
  .orElseThrow();
```

The complete example is implemented in [`../../examples/part-1.mjs`](../../examples/part-1.mjs). The public runtime API is exported from `@unrdf/atomvm` through `src/otp/index.mjs`.

## Chicago TDD proof

Run only this chapter’s state-based test:

```bash
node --test \
  --test-name-pattern="Pattern 4" \
  packages/atomvm/test/otp-patterns-chicago.test.mjs
```

The test constructs real domain objects and real runtime components. It contains no mock, spy, fake timer, scripted process runner, or interaction-count assertion. The admitted success condition is an observed state, lifecycle, receipt, or refusal.

## AtomVM proof

The real Generic UNIX AtomVM probe executes the equivalent BEAM semantics and emits:

```erlang
{otp_pattern, result_railway, ok}
```

The workflow requires all thirty markers and rejects any missing chapter marker.

## Resulting Context

The capability is now part of the same process graph as the earlier chapters. Its effects are bounded by explicit APIs, observed by receipts or state, and replayed by the executable test suite. Continue to the next pattern without replacing the working core.
