# Reference: Time API

**Source:** `/home/user/unrdf/packages/kgc-4d/src/time.mjs`

---

## Functions

### now()

Get current time in nanoseconds (BigInt).

**Signature:**
```javascript
now(): bigint
```

**Returns:** Nanoseconds since Unix epoch (monotonic)

**Example:**
```javascript
const t = now();
console.log(typeof t); // 'bigint'
```

---

### toISO()

Convert nanoseconds to ISO 8601 string.

**Signature:**
```javascript
toISO(t_ns: bigint): string
```

**Warning:** Truncates to millisecond precision (sub-ms nanoseconds lost).

---

### fromISO()

Parse ISO 8601 string to nanoseconds.

**Signature:**
```javascript
fromISO(iso: string): bigint
```

**Preserves:** Nanosecond precision if present in string.

---

### addNanoseconds()

Add nanoseconds to timestamp.

**Signature:**
```javascript
addNanoseconds(t_ns: bigint, delta: bigint): bigint
```

---

### duration()

Calculate elapsed time.

**Signature:**
```javascript
duration(start_ns: bigint, end_ns: bigint): bigint
```

**Returns:** Elapsed nanoseconds

---

## Related

- [Tutorial 01: Nanosecond Timestamps](../tutorials/01-nanosecond-timestamps.md)
- [Explanation: Why Nanosecond Precision](../explanation/01-why-nanosecond-precision.md)
