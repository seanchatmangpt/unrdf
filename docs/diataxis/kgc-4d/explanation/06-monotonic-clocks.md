# Explanation: Monotonic Clock Guarantees

**Question:** How does KGC-4D ensure timestamps never go backward?

---

## The Problem

System clocks can jump backward:
- NTP adjustments
- Daylight saving time
- Manual clock changes
- VM migration

**Result:** Events appear to happen "before" earlier events:

```javascript
// Without monotonic guarantee
const t1 = Date.now(); // 1735297800000
// [NTP adjusts clock -5 seconds]
const t2 = Date.now(); // 1735297795000 (went backward!)

console.assert(t1 < t2); // ❌ FAILS
```

**Breaks causality and event ordering.**

---

## The Solution: Monotonic Clock

KGC-4D enforces monotonic ordering:

```javascript
let lastTime = 0n;

export function now() {
  let current = process.hrtime.bigint();
  
  // Enforce monotonic ordering
  if (current <= lastTime) {
    current = lastTime + 1n; // Force increment
  }
  
  lastTime = current;
  return current;
}
```

**Guarantee:** Even if system clock goes backward, `now()` always increases.

---

## How It Works

### Normal Case (Clock Advances)

```javascript
const t1 = now(); // 1735297800123456789n
const t2 = now(); // 1735297800123456790n
const t3 = now(); // 1735297800123456791n

// t1 < t2 < t3 ✅
```

### Clock Jump Backward

```javascript
const t1 = now(); // 1735297800123456789n
// [System clock jumps backward -5 seconds]
const t2 = now(); // 1735297800123456790n (forced increment, ignores clock)
const t3 = now(); // 1735297800123456791n

// Still: t1 < t2 < t3 ✅
```

**System clock says t2 < t1, but monotonic guarantee fixes it.**

---

## Clock Jump Detection

KGC-4D detects large time jumps:

```javascript
const CLOCK_JUMP_THRESHOLD = 1_000_000_000_000n; // 1 second

if (jump > CLOCK_JUMP_THRESHOLD) {
  console.warn(`Clock jump detected: ${Number(jump / 1_000_000_000n)}s`);
  clockJumpDetected = true;
}
```

**Use `hasClockJumpDetected()` to check if clock was unreliable.**

---

## Rationale

### Why Monotonic?

**Without monotonic guarantee:**
- Events can appear out-of-order
- Causality broken (event A "caused" event B but B has earlier timestamp)
- Time travel reconstruction fails

**With monotonic guarantee:**
- Events always ordered correctly
- Causality preserved
- Deterministic replay

---

## Tradeoffs

**Advantages:**
- Events never out-of-order
- Causality guaranteed
- Deterministic reconstruction

**Disadvantages:**
- Timestamps may drift from wall-clock time (if clock jumps backward)
- Detection needed for large jumps

**Mitigation:** Use vector clocks for distributed causality (tolerates clock drift).

---

## Implementation Details

### Node.js

Uses `process.hrtime.bigint()` (monotonic hardware timer):

```javascript
const t = process.hrtime.bigint();
// Returns nanoseconds since arbitrary point (not Unix epoch)
// Monotonic: never goes backward
```

### Browser

Uses `performance.now()` (monotonic relative timer):

```javascript
const t = BigInt(Math.floor(performance.now() * 1_000_000));
// Monotonic: never goes backward
// Converted to nanoseconds
```

---

## Evidence

**Source:** `/home/user/unrdf/packages/kgc-4d/src/time.mjs:26` (now function)  
**Source:** `/home/user/unrdf/packages/kgc-4d/src/time.mjs:42` (clock jump detection)  
**Tests:** `/home/user/unrdf/packages/kgc-4d/test/time.test.mjs` (monotonic tests)

---

## Related

- [Tutorial 01: Nanosecond Timestamps](../tutorials/01-nanosecond-timestamps.md)
- [Explanation 01: Why Nanosecond Precision](./01-why-nanosecond-precision.md)
- [Reference: Time API](../reference/time-api.md)
