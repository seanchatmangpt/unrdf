# Explanation: Change Feed Architecture

**Why the change feed is designed the way it is.**

---

## What Problem the Change Feed Solves

An RDF knowledge graph is not static. Quads are inserted, removed, and updated continuously — by query results being cached, by federation syncing remote data, by user edits. Downstream components (dashboards, validators, other nodes) need to react to those mutations without polling the store.

The change feed provides a single point of observation: emit to the feed when a mutation happens, subscribe from anywhere to receive it.

---

## Core Layers

```
┌────────────────────────────────────────────┐
│  Consumer Layer                            │
│  (addEventListener · subscribe · replay)   │
└────────────────────────────────────────────┘
                     ↓
┌────────────────────────────────────────────┐
│  Event Dispatch                            │
│  EventTarget · CustomEvent                 │
└────────────────────────────────────────────┘
                     ↓
┌────────────────────────────────────────────┐
│  Validation (Zod)                          │
│  ChangeEventSchema · type enum · timestamp │
└────────────────────────────────────────────┘
                     ↓
┌────────────────────────────────────────────┐
│  History (Ring Buffer)                     │
│  maxHistorySize · getHistory · replay      │
└────────────────────────────────────────────┘
                     ↓
┌────────────────────────────────────────────┐
│  Source Hooks (Optional)                   │
│  store.addQuad · store.removeQuad patches  │
└────────────────────────────────────────────┘
```

---

## Why EventTarget (not EventEmitter)

The feed uses the web-standard `EventTarget` API internally (Node.js has supported it since v14) rather than the Node.js-specific `EventEmitter`. This choice has two consequences:

1. **DOM compatibility**: Code written against the feed can run in browsers or service workers without changes, using the same `addEventListener` / `removeEventListener` API.
2. **CustomEvent detail**: The change payload is carried in `event.detail`, which is the standard pattern for structured event data in DOM environments.

For the simpler callback use case, `feed.subscribe(callback)` is layered on top of the internal `Set<Function>` — it does not use the `EventTarget` at all. This means `subscribe` bypasses the DOM event wrapping and delivers the validated `ChangeEvent` object directly, which is slightly more ergonomic for Node.js consumers.

---

## Why Zod Validation on Every Emit

Every call to `emitChange` passes through `ChangeEventSchema.parse()`. This has a small runtime cost but provides two guarantees:

- **Type correctness**: `type` can only be `'add'`, `'remove'`, or `'update'`. Misspellings (`'added'`, `'delete'`) throw immediately rather than silently delivering bad data.
- **Timestamp injection**: If the caller omits `timestamp`, Zod's `.default()` does not apply here — instead `change.timestamp ?? Date.now()` is set before parsing, so every stored event has a valid numeric timestamp.

The schema is also the source of truth for what a `ChangeEvent` looks like, which means the ring buffer always contains valid, typed objects.

---

## Why a Ring Buffer

The history is stored as a plain JavaScript array used in FIFO ring buffer style: when `changes.length > maxHistorySize`, `changes.shift()` evicts the oldest entry. This means:

- **Memory is bounded**: A feed emitting 100 events/sec with `maxHistorySize: 10000` uses at most 10 000 event objects plus array overhead, regardless of how long it runs.
- **Replay is cheap**: `replay(callback)` simply iterates the array — no secondary data structure needed.
- **History queries are linear**: `getHistory({ since, limit })` filters by iterating. For large histories with frequent queries, callers should use the `limit` option or maintain their own index.

The ring buffer has three modes:

| `maxHistorySize`       | Behavior                                          |
| ---------------------- | ------------------------------------------------- |
| `0`                    | No events are stored (emit-only)                  |
| `N` (positive integer) | Ring buffer of N entries                          |
| `Infinity`             | Unbounded growth — use only for short-lived feeds |

---

## How Store Patching Works

When you pass an N3 `Store` to `createChangeFeed`, the feed replaces `store.addQuad`, `store.removeQuad`, and `store.removeQuads` with wrapper functions. Each wrapper calls the original method, then calls `feed.emitChange()` with the quad(s) involved.

This is a monkey-patch, not a proxy. Consequences:

- The original store behavior is fully preserved — the patch only adds a side effect.
- Callers who hold a reference to the original `addQuad` function before `createChangeFeed` is called will bypass the feed. Always create the feed before calling `addQuad`.
- N3 `Store` and Oxigraph both support `addQuad` / `removeQuad`. For Oxigraph's `removeQuads` (bulk removal), the feed iterates the quad array and emits one `'remove'` event per quad.

---

## SubscriptionManager: Filters Over Events

`SubscriptionManager` adds quad-pattern filtering on top of the bare feed. Internally, each subscription registers a DOM-style event listener on the `EventTarget`. When a `'change'` event arrives, the listener checks the quad against the stored `FilterSpec` before calling the subscriber's callback.

This means filtering is per-subscriber. If 10 subscribers all filter on different subjects, the feed dispatches the event once, and each of the 10 listeners independently checks whether to call their callback. There is no pre-routing or indexing by subject.

For workloads with many subscribers and highly selective filters, this O(n subscribers) check per event is usually fine. If you have thousands of subscriptions and performance is a concern, consider partitioning by creating separate feeds per subject domain.

---

## StreamProcessor: Chainable Operators

`StreamProcessor` provides an operator chain pattern similar to RxJS, but implemented without a full reactive library:

```
createStreamProcessor(feed)
  .filter(predicate)   ← accumulates ops in a plain array
  .map(mapper)         ← accumulates ops in the same array
  .subscribe(cb)       ← attaches a single addEventListener; applies all ops inline
```

The `operations` array is evaluated synchronously for each event. `filter` pushes `change => predicate(change) ? change : null`; `map` pushes `change => mapper(change)`. The terminal `subscribe` call walks the array, short-circuits on `null`, and calls the callback.

`batch` and `debounce` are terminal operators that also register an event listener but manage their own state (a buffer array for `batch`, a `setTimeout` handle for `debounce`). They do not share the `operations` array — they start from the raw event and apply all accumulated ops before adding to their own buffer.

---

## OpenTelemetry Instrumentation

`emitChange`, `subscribe`, and `getHistory` each start an OTel span via `tracer.startSpan()`. Span attributes include `change.type`, `subscribers.count`, `history.size`, and `history.trimmed`. This lets you trace the full event path from emission through all subscribers in any distributed tracing backend (Jaeger, Tempo, etc.).

The tracer name is `'@unrdf/streaming'`. Spans are always ended in a `finally` block, even when a Zod error is thrown.

---

## See Also

- [Tutorial: Subscribe to a Change Feed](../tutorials/01-subscribe-to-change-feed.md)
- [Reference: Change Feed API](../reference/change-feed-api.md)
- [Explanation: Backpressure and Flow Control](./02-backpressure-and-flow-control.md)
