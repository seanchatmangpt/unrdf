# Change Feed API Reference

Complete API for the change feed subsystem exported from `@unrdf/streaming`.

---

## createChangeFeed(store?, config?)

```typescript
createChangeFeed(store?: Object, config?: { maxHistorySize?: number }): ChangeFeed
```

Create a change feed emitter.

| Parameter               | Type                 | Default     | Description                                                                   |
| ----------------------- | -------------------- | ----------- | ----------------------------------------------------------------------------- |
| `store`                 | N3 Store             | `undefined` | Optional store to monitor. Patches `addQuad` / `removeQuad` / `removeQuads`.  |
| `config.maxHistorySize` | number or `Infinity` | `10000`     | Maximum changes in the ring buffer. `0` = no history, `Infinity` = unbounded. |

Returns a `ChangeFeed` object (see methods below).

Throws a Zod `ZodError` if `config` fails schema validation.

```javascript
// Standalone feed
const feed = createChangeFeed();

// Monitor an N3 Store
import { Store } from 'n3';
const store = new Store();
const monitoredFeed = createChangeFeed(store);

// Custom history size
const limitedFeed = createChangeFeed(null, { maxHistorySize: 500 });
```

---

## ChangeFeed Methods

### emitChange(change)

```typescript
emitChange(change: {
  type: 'add' | 'remove' | 'update';
  quad: { subject: any; predicate: any; object: any; graph?: any };
  timestamp?: number;
  metadata?: any;
}): void
```

Emit a validated change event. If `timestamp` is omitted, `Date.now()` is used.

The change is stored in the ring buffer, dispatched via the `EventTarget`, and delivered to all subscribers registered with `subscribe()`.

Throws a Zod `ZodError` if the change fails schema validation (invalid `type`, missing `quad` fields).

```javascript
feed.emitChange({
  type: 'add',
  quad: { subject, predicate, object, graph },
  metadata: { source: 'import-job' },
});
```

---

### subscribe(callback)

```typescript
subscribe(callback: (change: ChangeEvent) => void): () => void
```

Register a callback to receive all change events. Returns an unsubscribe function.

```javascript
const unsubscribe = feed.subscribe(change => {
  console.log(change.type, change.quad.subject.value);
});

// Later:
unsubscribe();
```

---

### addEventListener(type, callback, options?)

```typescript
addEventListener(type: 'change', callback: (event: CustomEvent) => void, options?: EventListenerOptions): void
```

DOM-style event listener. `event.detail` holds the `ChangeEvent`.

```javascript
feed.addEventListener('change', event => {
  const change = event.detail; // { type, quad, timestamp, metadata? }
});
```

---

### removeEventListener(type, callback, options?)

```typescript
removeEventListener(type: 'change', callback: Function, options?: EventListenerOptions): void
```

Remove a previously registered DOM-style listener.

---

### getHistory(options?)

```typescript
getHistory(options?: { since?: number; limit?: number }): ChangeEvent[]
```

Return a filtered view of the history ring buffer.

| Option  | Description                                                               |
| ------- | ------------------------------------------------------------------------- |
| `since` | Include only changes with `timestamp >= since`                            |
| `limit` | Return at most this many changes (from the beginning of the filtered set) |

```javascript
const lastMinute = feed.getHistory({ since: Date.now() - 60_000 });
const first10 = feed.getHistory({ limit: 10 });
const recent10 = feed.getHistory({ since: t0, limit: 10 });
```

---

### getChanges()

```typescript
getChanges(): ChangeEvent[]
```

Return a shallow copy of all changes in the ring buffer (no filtering).

---

### replay(callback)

```typescript
replay(callback: (change: ChangeEvent) => void): void
```

Synchronously deliver every change in history to `callback` in insertion order.

```javascript
feed.replay(change => store.addQuad(change.quad));
```

---

### clearChanges()

```typescript
clearChanges(): void
```

Remove all entries from the ring buffer. Subscribers are unaffected.

---

### destroy()

```typescript
destroy(): void
```

Clear the ring buffer, remove all subscribers, and replace the internal `EventTarget` with a fresh instance. After `destroy()`, the feed emits no further events.

---

## ChangeEvent Schema

```typescript
{
  type:      'add' | 'remove' | 'update';
  quad:      { subject: any; predicate: any; object: any; graph?: any };
  timestamp: number; // Unix ms
  metadata?: any;
}
```

Validated by Zod on every `emitChange()` call.

---

## createSubscriptionManager(storeOrFeed)

```typescript
createSubscriptionManager(storeOrFeed: N3Store | ChangeFeed): SubscriptionManager
```

Create a subscription manager. If given an N3 `Store`, it is automatically wrapped in a `ChangeFeed`.

```javascript
const manager = createSubscriptionManager(feed);
const manager2 = createSubscriptionManager(store); // store is wrapped in a new ChangeFeed
```

---

## SubscriptionManager Methods

### subscribe(callbackOrFilter, filterOrCallback?)

```typescript
// Old API
subscribe(callback: Function, filter?: FilterSpec): string

// New API
subscribe(filter: FilterSpec, callback: Function): string
```

Register a filtered subscription. Returns an opaque subscription ID.

**FilterSpec fields** (all optional, omitted = wildcard):

| Field       | Description                     |
| ----------- | ------------------------------- |
| `subject`   | Match by `quad.subject.value`   |
| `predicate` | Match by `quad.predicate.value` |
| `object`    | Match by `quad.object.value`    |
| `graph`     | Match by `quad.graph.value`     |

```javascript
const subId = manager.subscribe(change => { ... }, { subject: namedNode('...') });

// New API
const subId2 = manager.subscribe({ predicate: namedNode('...') }, change => { ... });
```

---

### unsubscribe(subscriptionId)

```typescript
unsubscribe(subscriptionId: string): boolean
```

Remove a subscription by ID. Returns `true` if found, `false` if not found.

---

### listSubscriptions()

```typescript
listSubscriptions(): Array<{ id: string; filter: FilterSpec }>
```

Return metadata for all active subscriptions.

---

### clearSubscriptions()

```typescript
clearSubscriptions(): void
```

Remove all subscriptions and their event listeners.

---

### destroy()

```typescript
destroy(): void
```

Alias for `clearSubscriptions()`.

---

## createStreamProcessor(feed)

```typescript
createStreamProcessor(feed: ChangeFeed): StreamProcessor
```

Create a chainable stream processor over a change feed.

---

## StreamProcessor Methods

### filter(predicate)

```typescript
filter(predicate: (change: ChangeEvent) => boolean): StreamProcessor
```

Add a filter operation to the chain. Changes that return `false` are dropped.

### map(mapper)

```typescript
map(mapper: (change: ChangeEvent) => any): StreamProcessor
```

Add a mapping operation to the chain. The return value replaces the change for downstream operations.

### batch(batchSize)

```typescript
batch(batchSize: number): BatchStream
```

Accumulate changes into arrays of `batchSize`. Returns a `BatchStream` with:

- `subscribe(callback: (changes: any[]) => void): void`
- `unsubscribe(callback: Function): void`
- `destroy(): void`

### debounce(delayMs)

```typescript
debounce(delayMs: number): DebounceStream
```

Emit only the last change after `delayMs` milliseconds of quiet. Returns a `DebounceStream` with:

- `subscribe(callback: (change: any) => void): void`
- `unsubscribe(callback: Function): void`
- `destroy(): void`

### subscribe(callback)

```typescript
subscribe(callback: (change: any) => void): void
```

Subscribe to processed changes after all `filter` / `map` operations have been applied.

---

## Zod Schemas

| Schema                   | Validates                            |
| ------------------------ | ------------------------------------ |
| `ChangeEventSchema`      | Individual change event objects      |
| `ChangeFeedConfigSchema` | `createChangeFeed()` config          |
| `FilterSchema`           | `SubscriptionManager` filter objects |
