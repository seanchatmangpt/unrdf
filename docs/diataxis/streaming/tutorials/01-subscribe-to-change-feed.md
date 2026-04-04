# Tutorial: Subscribe to a Change Feed

In this tutorial you will create a change feed, emit quad changes, subscribe to them, replay history, and clean up. By the end you will understand the core event loop and be ready to build filtered subscriptions and stream processors.

**Time**: 15–20 minutes
**Prerequisite**: `pnpm add @unrdf/streaming`

---

## Part 1: Hello Change Feed

Create `hello-feed.mjs` with the simplest possible feed:

```javascript
import { createChangeFeed } from '@unrdf/streaming';
import { DataFactory } from 'n3';

const { namedNode, literal } = DataFactory;

// 1. Create the feed
const feed = createChangeFeed();

// 2. Listen for changes before emitting
feed.addEventListener('change', event => {
  const { type, quad, timestamp } = event.detail;
  console.log(`[${type}] ${quad.subject.value} (at ${timestamp})`);
});

// 3. Emit a change — the feed validates it with Zod
feed.emitChange({
  type: 'add',
  quad: {
    subject: namedNode('http://example.org/Alice'),
    predicate: namedNode('http://schema.org/name'),
    object: literal('Alice'),
  },
});

feed.emitChange({
  type: 'remove',
  quad: {
    subject: namedNode('http://example.org/Alice'),
    predicate: namedNode('http://schema.org/name'),
    object: literal('Alice'),
  },
});
```

Run it:

```bash
node hello-feed.mjs
```

Expected output:

```
[add] http://example.org/Alice (at 1712345678901)
[remove] http://example.org/Alice (at 1712345678902)
```

**What happened:**

- `createChangeFeed()` creates a feed backed by an `EventTarget` and an internal ring buffer
- `addEventListener('change', ...)` is the DOM-style listener — `event.detail` holds the validated `ChangeEvent`
- `emitChange()` validates the object with Zod (type must be `'add'`, `'remove'`, or `'update'`), adds a `timestamp` if absent, pushes to history, then dispatches to all listeners
- The feed stores changes in memory up to `maxHistorySize` (default 10 000)

---

## Part 2: Use the Subscribe API

`addEventListener` gives you DOM-style events. The `subscribe` method is a simpler callback API that returns an unsubscribe function:

```javascript
import { createChangeFeed } from '@unrdf/streaming';
import { DataFactory } from 'n3';

const { namedNode, literal } = DataFactory;

const feed = createChangeFeed();

// subscribe returns a cleanup function
const unsubscribe = feed.subscribe(change => {
  console.log('Received:', change.type, change.quad.subject.value);
});

const quad = {
  subject: namedNode('http://example.org/Bob'),
  predicate: namedNode('http://schema.org/age'),
  object: literal('30'),
};

feed.emitChange({ type: 'add', quad });
feed.emitChange({ type: 'update', quad });

// Stop receiving — subscriber is removed from the internal Set
unsubscribe();

feed.emitChange({ type: 'remove', quad }); // this is NOT delivered
console.log('Total changes in history:', feed.getChanges().length); // 3 (all stored)
```

---

## Part 3: Monitor an N3 Store Automatically

Pass an N3 `Store` to `createChangeFeed` and every `addQuad` / `removeQuad` call on that store automatically emits a change event:

```javascript
import { Store } from 'n3';
import { createChangeFeed } from '@unrdf/streaming';
import { DataFactory } from 'n3';

const { namedNode, literal } = DataFactory;

const store = new Store();
const feed = createChangeFeed(store); // hooks into store.addQuad / removeQuad

feed.addEventListener('change', event => {
  console.log('Store changed:', event.detail.type);
});

// Normal N3 usage — changes are automatically observed
store.addQuad(namedNode('http://example.org/s'), namedNode('http://example.org/p'), literal('v'));
store.removeQuad(
  namedNode('http://example.org/s'),
  namedNode('http://example.org/p'),
  literal('v')
);
```

Expected output:

```
Store changed: add
Store changed: remove
```

The feed patches `store.addQuad` and `store.removeQuad` at creation time. Your existing store code does not need to change.

---

## Part 4: Query History and Replay

The feed maintains a ring buffer of all changes. You can query it after the fact:

```javascript
import { createChangeFeed } from '@unrdf/streaming';
import { DataFactory } from 'n3';

const { namedNode, literal } = DataFactory;

const feed = createChangeFeed(null, { maxHistorySize: 1000 });

const quad = {
  subject: namedNode('http://example.org/x'),
  predicate: namedNode('http://example.org/y'),
  object: literal('z'),
};

const t0 = Date.now();

// Emit some events over time
feed.emitChange({ type: 'add', quad, timestamp: t0 });
feed.emitChange({ type: 'update', quad, timestamp: t0 + 100 });
feed.emitChange({ type: 'remove', quad, timestamp: t0 + 200 });

// Get all changes
console.log('All:', feed.getChanges().length); // 3

// Get changes after a specific timestamp
const recent = feed.getHistory({ since: t0 + 50 });
console.log('Recent (after t0+50):', recent.length); // 2

// Get the first N changes
const limited = feed.getHistory({ limit: 2 });
console.log('Limited:', limited.length); // 2

// Replay to a fresh subscriber
const replayed = [];
feed.replay(change => replayed.push(change.type));
console.log('Replayed:', replayed); // ['add', 'update', 'remove']
```

---

## Part 5: Clean Up

Always destroy a feed when you no longer need it to release all listeners and clear the history buffer:

```javascript
import { createChangeFeed } from '@unrdf/streaming';

const feed = createChangeFeed();

const unsubscribe = feed.subscribe(() => {});

// Option A: unsubscribe individual callbacks
unsubscribe();

// Option B: destroy everything at once
feed.destroy();
// After destroy: all subscribers cleared, history cleared, EventTarget reset
```

---

## What You Learned

You now know the essential change feed patterns:

1. **Create** — `createChangeFeed()` or `createChangeFeed(store)` for automatic store monitoring
2. **Listen** — `feed.addEventListener('change', handler)` (DOM-style) or `feed.subscribe(callback)` (simple callback)
3. **Emit** — `feed.emitChange({ type, quad })` with Zod validation and automatic timestamp
4. **Query history** — `feed.getChanges()`, `feed.getHistory({ since, limit })`, `feed.replay(callback)`
5. **Clean up** — `unsubscribe()` for individual callbacks, `feed.destroy()` for everything

## Next Steps

- [Tutorial: Stream a Large RDF Graph](./02-stream-large-rdf-graph.md) — parse large files with the RDFStreamParser
- [How-To: Filter Change Feed Events](../how-to/01-filter-change-feed-events.md) — subscribe to specific quad patterns
- [How-To: Handle Backpressure](../how-to/02-handle-backpressure.md) — avoid memory overruns under high load
- [Reference: Change Feed API](../reference/change-feed-api.md) — complete method signatures
