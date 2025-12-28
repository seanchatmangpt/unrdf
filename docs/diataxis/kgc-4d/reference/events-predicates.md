# Reference: Events & Predicates

**Source:** `/home/user/unrdf/packages/kgc-4d/src/constants.mjs`

---

## Event Types

```javascript
export const EVENT_TYPES = {
  CREATE: 'CREATE',
  UPDATE: 'UPDATE',
  DELETE: 'DELETE',
  SNAPSHOT: 'SNAPSHOT',
};
```

### Descriptions

- **CREATE** - Entity creation
- **UPDATE** - Entity modification
- **DELETE** - Entity removal (logical delete)
- **SNAPSHOT** - Universe freeze event (auto-generated)

---

## RDF Predicates

```javascript
export const PREDICATES = {
  TYPE: 'http://kgc.io/type',
  T_NS: 'http://kgc.io/t_ns',
  TIMESTAMP_ISO: 'http://kgc.io/timestamp_iso',
  PAYLOAD: 'http://kgc.io/payload',
  EVENT_COUNT: 'http://kgc.io/event_count',
  UNIVERSE_HASH: 'http://kgc.io/universe_hash',
  GIT_REF: 'http://kgc.io/git_ref',
};
```

---

## Related

- [Tutorial 04: Query Event Logs](../tutorials/04-query-event-logs.md)
- [Named Graphs](./named-graphs.md)
