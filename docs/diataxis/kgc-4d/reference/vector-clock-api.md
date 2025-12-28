# Reference: VectorClock API

**Source:** `/home/user/unrdf/packages/kgc-4d/src/time.mjs:210`

---

## Class: VectorClock

Causal ordering for distributed systems.

### Constructor

```javascript
new VectorClock(nodeId: string)
```

---

## Methods

### increment()

Increment this node's counter.

**Signature:**
```javascript
increment(): void
```

---

### merge()

Merge another vector clock (receive message).

**Signature:**
```javascript
merge(otherClock: VectorClock): void
```

---

### compare()

Compare with another clock for causality.

**Signature:**
```javascript
compare(otherClock: VectorClock): number | null
```

**Returns:**
- `-1` - This clock happened-before other
- `1` - This clock happened-after other
- `0` - Clocks are equal
- `null` - Concurrent (no causal relationship)

---

## Related

- [Tutorial 05: Vector Clocks](../tutorials/05-vector-clocks.md)
- [How-To 04: Distributed Systems](../how-to/04-vector-clocks.md)
