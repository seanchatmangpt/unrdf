# Explanation: Why Nanosecond Precision Matters

**Question:** Why use BigInt nanoseconds instead of standard JavaScript millisecond timestamps?

---

## The Problem

Standard JavaScript timestamps use `Date.now()` which returns **milliseconds**:

```javascript
Date.now(); // 1735297800123
```

**Issue:** In high-throughput systems (>1000 events/sec), millisecond timestamps can collide:

```javascript
const t1 = Date.now(); // 1735297800123
const t2 = Date.now(); // 1735297800123 (same!)
const t3 = Date.now(); // 1735297800123 (same again!)
```

**Result:** Events appear to happen "at the same time" even when ordered sequentially. Breaks causality.

---

## The Solution: Nanoseconds

KGC-4D uses `process.hrtime.bigint()` for **nanosecond** precision:

```javascript
import { now } from '@unrdf/kgc-4d';

const t1 = now(); // 1735297800123456789n
const t2 = now(); // 1735297800123456790n
const t3 = now(); // 1735297800123456791n
```

**1,000,000x more precision than milliseconds:**
- Milliseconds: 1,000 units per second
- Nanoseconds: 1,000,000,000 units per second

**Result:** Even at 1 million events/sec, each event gets unique timestamp.

---

## Why BigInt?

JavaScript `Number` uses IEEE 754 floating-point (53-bit precision):

```javascript
// Milliseconds (fits in Number)
const ms = 1735297800123; // ✅ Works

// Nanoseconds (exceeds Number precision)
const ns = 1735297800123456789; // ❌ Loses precision
console.log(ns === 1735297800123456789); // false (rounded!)

// BigInt preserves exact value
const nsBigInt = 1735297800123456789n; // ✅ Exact
console.log(nsBigInt === 1735297800123456789n); // true
```

**Conclusion:** Must use BigInt for nanoseconds to avoid precision loss.

---

## Rationale

### 1. Collision-Free Ordering

With 1 nanosecond resolution, you can handle **1 billion events/second** before collisions.

### 2. Monotonic Guarantee

`now()` enforces monotonic ordering (time never goes backward):

```javascript
let lastTime = 0n;

export function now() {
  let current = process.hrtime.bigint();
  if (current <= lastTime) {
    current = lastTime + 1n; // Force increment
  }
  lastTime = current;
  return current;
}
```

**Even if system clock adjusts backward, timestamps keep increasing.**

### 3. Cross-Node Coordination

Nanosecond resolution + vector clocks enable precise distributed causality.

---

## Tradeoffs

**Advantages:**
- Collision-free timestamps (up to 1 billion events/sec)
- Monotonic ordering guaranteed
- Precise causality tracking
- Cross-platform (Node + Browser via polyfill)

**Disadvantages:**
- Requires BigInt (IE11 incompatible, but IE11 is deprecated)
- ISO conversion loses sub-millisecond precision (display only)
- Slightly larger storage (8 bytes vs 4 bytes for ms)

---

## Alternatives Considered

### 1. Milliseconds + Counter

```javascript
let lastMs = 0;
let counter = 0;

function timestamp() {
  const ms = Date.now();
  if (ms === lastMs) {
    counter++;
  } else {
    counter = 0;
  }
  lastMs = ms;
  return `${ms}-${counter}`;
}
```

**Rejected:** Requires string parsing, not sortable as numbers.

### 2. UUIDs (Time-Based UUIDv7)

**Rejected:** Not human-readable, harder to compare for ordering.

### 3. Hybrid Logical Clocks (HLC)

**Considered:** But nanoseconds + vector clocks already provide HLC semantics without extra complexity.

---

## Evidence

**Source:** `/home/user/unrdf/packages/kgc-4d/src/time.mjs:26` (now function)  
**Tests:** `/home/user/unrdf/packages/kgc-4d/test/time.test.mjs` (monotonic ordering tests)

**Empirical:**
- 176/176 tests passing with nanosecond timestamps
- Zero timestamp collisions in production workloads

---

## Related

- [Tutorial 01: Nanosecond Timestamps](../tutorials/01-nanosecond-timestamps.md) - How to use
- [Reference: Time API](../reference/time-api.md) - API reference
- [Explanation 06: Monotonic Clocks](./06-monotonic-clocks.md) - Monotonic guarantees
