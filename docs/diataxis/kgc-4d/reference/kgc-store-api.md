# Reference: KGCStore API

**Source:** `/home/user/unrdf/packages/kgc-4d/src/store.mjs`

---

## Class: KGCStore

Extended UnrdfStore with 4D capabilities: event logging, universe freeze, time travel.

### Constructor

```javascript
new KGCStore()
```

Creates a new KGC-4D store with three named graphs: Universe, EventLog, System.

---

## Methods

### appendEvent()

Append immutable event to EventLog with atomic state delta to Universe.

**Signature:**
```javascript
appendEvent(
  event: { type: string, payload: object },
  deltas: Array<{ type: 'add'|'delete', subject, predicate, object }>
): Promise<{ receipt: Receipt }>
```

**Parameters:**
- `event.type` - Event type (see [Events & Predicates](./events-predicates.md))
- `event.payload` - Arbitrary JSON payload
- `deltas` - Array of RDF quad changes (add/delete)

**Returns:**
- Promise resolving to `{ receipt }` with timestamp, event count, etc.

**Example:**
```javascript
const receipt = await store.appendEvent(
  { type: EVENT_TYPES.CREATE, payload: { subject: 'Alice' } },
  [{ type: 'add', subject: alice, predicate: rdfType, object: person }]
);
```

**Source:** `/home/user/unrdf/packages/kgc-4d/src/store.mjs`

---

### queryUniverse()

Query the hot Universe graph (current state).

**Signature:**
```javascript
queryUniverse(sparql: string): Promise<Array<Bindings>>
```

---

### queryEventLog()

Query the immutable EventLog graph (history).

**Signature:**
```javascript
queryEventLog(sparql: string): Promise<Array<Bindings>>
```

---

### getEventCount()

Get total number of events in EventLog.

**Signature:**
```javascript
getEventCount(): number
```

---

## Related

- [Events & Predicates](./events-predicates.md) - Event types
- [Receipt Schema](./receipt-schema.md) - Receipt structure
- [Named Graphs](./named-graphs.md) - Graph URIs
