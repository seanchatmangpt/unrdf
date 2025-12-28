# Reference: Receipt Schema

**Source:** `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs`

---

## Freeze Receipt

Created by `freezeUniverse()`. Cryptographically proves universe state at specific time.

### Schema

```javascript
{
  id: string,               // UUID v4
  t_ns: string,            // Nanoseconds (BigInt as string)
  timestamp_iso: string,   // ISO 8601
  universe_hash: string,   // BLAKE3 hash of N-Quads
  git_ref: string,         // Git commit SHA
  event_count: number,     // Total events up to freeze
  nquad_count: number      // Number of quads in Universe
}
```

### Field Descriptions

**id:** Unique receipt identifier (UUID v4)  
**t_ns:** Freeze timestamp in nanoseconds (BigInt as string for JSON)  
**timestamp_iso:** Human-readable timestamp  
**universe_hash:** BLAKE3 hash of sorted Universe N-Quads (deterministic)  
**git_ref:** Git commit hash where snapshot is stored  
**event_count:** Total events in EventLog at freeze time  
**nquad_count:** Number of RDF quads in Universe graph

### Verification

Use `verifyReceipt()` to cryptographically verify:
1. Fetch Git commit by `git_ref`
2. Recompute BLAKE3 hash of N-Quads
3. Compare with `universe_hash`

---

## Related

- [Tutorial 02: Freeze Universe](../tutorials/02-create-freeze-universe.md)
- [How-To 01: Freeze and Verify](../how-to/01-freeze-and-verify.md)
