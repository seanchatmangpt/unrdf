# Tutorial: Getting Started with Nanosecond Timestamps

**Objective:** Learn how to work with BigInt nanosecond timestamps for precise event ordering.

**Time:** 5 minutes  
**Level:** Beginner

---

## Prerequisites

**Capabilities Needed:**
- Capability: "Nanosecond timestamp generation"
- Capability: "Monotonic clock ordering"
- Capability: "ISO 8601 conversion"

**Knowledge Required:**
- Basic JavaScript/Node.js
- Understanding of Unix timestamps

---

## What You'll Learn

By the end of this tutorial, you will:
1. Generate nanosecond-precision timestamps using `now()`
2. Convert between BigInt nanoseconds and ISO 8601 strings
3. Understand monotonic ordering guarantees
4. Perform nanosecond arithmetic

---

## Step 1: Generate Your First Nanosecond Timestamp

```javascript
import { now, toISO } from '@unrdf/kgc-4d';

// Get current time in nanoseconds (BigInt)
const timestamp = now();

console.log('Nanoseconds:', timestamp);
console.log('Type:', typeof timestamp); // 'bigint'
console.log('ISO 8601:', toISO(timestamp));
```

**Why nanoseconds?** Millisecond timestamps (Date.now()) can have collisions in high-throughput systems. Nanoseconds provide 1,000,000x more precision.

---

## Step 2: Verify Monotonic Ordering

```javascript
import { now } from '@unrdf/kgc-4d';

const t1 = now();
const t2 = now();
const t3 = now();

console.assert(t1 < t2, 'Time never goes backward');
console.assert(t2 < t3, 'Time is strictly increasing');
console.log('Monotonic ordering verified!');
```

**Guarantee:** Even if your system clock adjusts backward, `now()` ensures timestamps always increase.

---

## Step 3: Convert Between Formats

```javascript
import { now, toISO, fromISO, addNanoseconds, duration } from '@unrdf/kgc-4d';

const t1 = now();
const iso = toISO(t1);
const t2 = fromISO(iso);

console.log('Original:', t1);
console.log('ISO 8601:', iso);
console.log('Parsed back:', t2);

// Nanosecond arithmetic
const future = addNanoseconds(t1, 1_000_000_000n); // +1 second
const elapsed = duration(t1, future);

console.log('Duration:', elapsed, 'nanoseconds');
console.log('That is:', Number(elapsed / 1_000_000_000n), 'seconds');
```

---

## Step 4: Complete Working Example

```javascript
import { now, toISO, addNanoseconds } from '@unrdf/kgc-4d';

// Simulate event timestamps
function recordEvent(name) {
  const timestamp = now();
  return {
    name,
    t_ns: timestamp,
    timestamp_iso: toISO(timestamp),
  };
}

const events = [
  recordEvent('User login'),
  recordEvent('Page view'),
  recordEvent('Button click'),
];

events.forEach((event, i) => {
  console.log(`Event ${i + 1}: ${event.name}`);
  console.log(`  Time: ${event.timestamp_iso}`);
  console.log(`  Nanoseconds: ${event.t_ns}`);
});

// Verify ordering
for (let i = 1; i < events.length; i++) {
  console.assert(
    events[i].t_ns > events[i - 1].t_ns,
    'Events are ordered'
  );
}
console.log('\nAll events properly ordered!');
```

---

## Verification

Run the complete example:

```bash
node tutorial-01-timestamps.mjs
```

Expected output:
```
Event 1: User login
  Time: 2025-12-27T10:30:00.123456Z
  Nanoseconds: 1735297800123456789n

Event 2: Page view
  Time: 2025-12-27T10:30:00.123457Z
  Nanoseconds: 1735297800123457000n

Event 3: Button click
  Time: 2025-12-27T10:30:00.123458Z
  Nanoseconds: 1735297800123458000n

All events properly ordered!
```

---

## Evidence

**Source Code:**
- Implementation: `/home/user/unrdf/packages/kgc-4d/src/time.mjs:26` (now function)
- Implementation: `/home/user/unrdf/packages/kgc-4d/src/time.mjs:96` (toISO function)
- Implementation: `/home/user/unrdf/packages/kgc-4d/src/time.mjs:115` (fromISO function)

**Tests:**
- Test suite: `/home/user/unrdf/packages/kgc-4d/test/time.test.mjs`
- Doctests: `/home/user/unrdf/packages/kgc-4d/test/doctest/time.doctest.test.mjs`

**Examples:**
- Basic usage: `/home/user/unrdf/packages/kgc-4d/examples/basic-usage.mjs:37-38`

---

## Key Takeaways

1. **BigInt precision:** Nanoseconds use BigInt to avoid floating-point loss
2. **Monotonic guarantee:** Time never goes backward, even if system clock adjusts
3. **ISO conversion:** toISO() for display, fromISO() for parsing (with nanosecond preservation)
4. **Arithmetic:** Use addNanoseconds() and duration() for calculations

---

## Next Steps

**Continue Learning:**
- [Tutorial 02: Create and Freeze Universe](./02-create-freeze-universe.md) - Use timestamps in RDF events
- [How-To 02: Implement Time Travel](../how-to/02-implement-time-travel.md) - Use timestamps for state reconstruction
- [Explanation 01: Why Nanosecond Precision](../explanation/01-why-nanosecond-precision.md) - Deep dive into design rationale

**Reference:**
- [Time API Reference](../reference/time-api.md) - Complete API documentation

---

**Navigate:** [Tutorials](./README.md) | [Main Diataxis](../README.md) | [Next Tutorial →](./02-create-freeze-universe.md)
