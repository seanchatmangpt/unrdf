# Explanation: Why BLAKE3 Hashing?

**Question:** Why BLAKE3 for universe hashing instead of SHA-256 or other algorithms?

---

## The Decision

KGC-4D uses **BLAKE3** (via `hash-wasm`) for universe snapshot hashing.

---

## Comparison

| Algorithm | Speed (MB/s) | Security | Hardware Accel | Output Size |
|-----------|--------------|----------|----------------|-------------|
| MD5       | 400          | ❌ Broken | ✅ x86 SSE     | 128 bits    |
| SHA-1     | 350          | ⚠️ Weak   | ✅ x86 SHA-NI  | 160 bits    |
| SHA-256   | 150          | ✅ Strong | ✅ x86 SHA-NI  | 256 bits    |
| **BLAKE3**| **1000+**    | **✅ Strong** | **✅ SIMD** | **256 bits** |

**BLAKE3 is 6-7x faster than SHA-256 with equal security.**

---

## Rationale

### 1. Speed

Freezing a large universe (100K quads) requires hashing megabytes of N-Quads:

```javascript
// 100K quads ≈ 10MB N-Quads

// SHA-256: 10MB / 150 MB/s = 66ms
// BLAKE3:  10MB / 1000 MB/s = 10ms

// Result: 6.6x faster freeze
```

**Production impact:** Faster freezes = less blocking, higher throughput.

### 2. Security

BLAKE3 is a cryptographic hash function:
- **Collision resistance:** ~2^128 operations to find collision
- **Preimage resistance:** Cannot reverse hash to find input
- **Second preimage resistance:** Cannot find different input with same hash

**Same security level as SHA-256.**

### 3. Determinism

BLAKE3 is deterministic:
```javascript
blake3('alice is a person') === blake3('alice is a person')
// Always produces same hash for same input
```

**Critical for receipt verification:** Anyone can recompute hash and verify.

---

## Tradeoffs

**Advantages:**
- 6-7x faster than SHA-256
- Parallelizable (uses SIMD instructions)
- Cryptographically secure
- 256-bit output (collision-resistant)

**Disadvantages:**
- Less widely known than SHA-256 (newer algorithm)
- Not NIST standardized (but widely trusted)
- Requires WASM for speed (pure JS fallback slower)

---

## Alternatives Considered

### 1. SHA-256

**Why not?**
- Too slow (150 MB/s vs BLAKE3's 1000+ MB/s)
- Freezing large universes would block UI/server

### 2. xxHash (Non-Cryptographic)

**Why not?**
- Fast (10GB/s) but **not cryptographically secure**
- Subject to collision attacks
- Cannot use for tamper detection

### 3. SHA-3 (Keccak)

**Why not?**
- Slower than BLAKE3 (~200 MB/s)
- Less parallel (BLAKE3 better uses SIMD)

---

## Implementation: hash-wasm

KGC-4D uses `hash-wasm` library for BLAKE3:

```javascript
import { blake3 } from 'hash-wasm';

const hash = await blake3(nquadsString);
console.log(hash); // "abc123def456..." (64 hex chars = 256 bits)
```

**Why hash-wasm?**
- WASM-based (10x faster than pure JS)
- Works in Node + Browser
- ARD compliant (no native dependencies)
- Supports BLAKE3, SHA-256, and others

---

## Evidence

**ARD Requirement:** `/home/user/unrdf/packages/kgc-4d/docs/ARD.md` (mandates hash-wasm)  
**Source:** `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs:6` (BLAKE3 import)  
**Benchmarks:** hash-wasm benchmarks show 1000+ MB/s on modern CPUs

---

## Related

- [Reference: Receipt Schema](../reference/receipt-schema.md) - Where hash is stored
- [How-To 01: Freeze and Verify](../how-to/01-freeze-and-verify.md) - Using hashes
