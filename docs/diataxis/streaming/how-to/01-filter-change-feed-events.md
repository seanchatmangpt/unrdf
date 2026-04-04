# How To: Filter Change Feed Events

Subscribe only to changes that match a specific quad pattern, or chain filter / map operators over a stream processor.

---

## Filter by Subject Using SubscriptionManager

`createSubscriptionManager` wraps a feed and applies quad-pattern filters before delivering events to each subscriber.

```javascript
import { createChangeFeed, createSubscriptionManager } from '@unrdf/streaming';
import { DataFactory } from 'n3';

const { namedNode, literal } = DataFactory;

const feed = createChangeFeed();
const manager = createSubscriptionManager(feed);

const alice = namedNode('http://example.org/Alice');
const bob = namedNode('http://example.org/Bob');

// Only receive changes where the subject is Alice
const aliceSubId = manager.subscribe(change => console.log('Alice change:', change.type), {
  subject: alice,
});

feed.emitChange({
  type: 'add',
  quad: {
    subject: alice,
    predicate: namedNode('http://schema.org/name'),
    object: literal('Alice'),
  },
});
feed.emitChange({
  type: 'add',
  quad: { subject: bob, predicate: namedNode('http://schema.org/name'), object: literal('Bob') },
});
// Output: "Alice change: add"   (Bob's change is silently dropped for aliceSubId)

manager.unsubscribe(aliceSubId);
```

The filter object accepts `subject`, `predicate`, `object`, and `graph`. Any omitted field acts as a wildcard.

---

## Filter by Predicate

```javascript
const nameNode = namedNode('http://schema.org/name');

const nameSubId = manager.subscribe(
  change => console.log('Name changed:', change.quad.object.value),
  { predicate: nameNode }
);

// Emitting with a different predicate — not delivered
feed.emitChange({
  type: 'add',
  quad: {
    subject: namedNode('http://example.org/Alice'),
    predicate: namedNode('http://schema.org/age'),
    object: literal('30'),
  },
});

// Emitting with schema:name — delivered
feed.emitChange({
  type: 'add',
  quad: {
    subject: namedNode('http://example.org/Alice'),
    predicate: nameNode,
    object: literal('Alice'),
  },
});
// Output: "Name changed: Alice"
```

---

## Filter by Change Type Using StreamProcessor

`createStreamProcessor` gives you chainable operators. Use `filter()` to restrict by change type:

```javascript
import { createChangeFeed, createStreamProcessor } from '@unrdf/streaming';
import { DataFactory } from 'n3';

const { namedNode, literal } = DataFactory;

const feed = createChangeFeed();
const processor = createStreamProcessor(feed);

// Only receive 'add' events
processor
  .filter(change => change.type === 'add')
  .subscribe(change => {
    console.log('Added:', change.quad.subject.value);
  });

const quad = {
  subject: namedNode('http://example.org/x'),
  predicate: namedNode('http://example.org/y'),
  object: literal('z'),
};

feed.emitChange({ type: 'add', quad }); // delivered
feed.emitChange({ type: 'remove', quad }); // filtered out
feed.emitChange({ type: 'add', quad }); // delivered
```

---

## Combine Filter and Map

Chain `filter` and `map` before subscribing to transform incoming changes:

```javascript
import { createChangeFeed, createStreamProcessor } from '@unrdf/streaming';
import { DataFactory } from 'n3';

const { namedNode, literal } = DataFactory;

const feed = createChangeFeed();
const processor = createStreamProcessor(feed);

processor
  .filter(change => change.type === 'add')
  .map(change => ({
    subjectValue: change.quad.subject.value,
    objectValue: change.quad.object.value,
    receivedAt: Date.now(),
  }))
  .subscribe(item => {
    console.log(`${item.subjectValue} = ${item.objectValue} at ${item.receivedAt}`);
  });

feed.emitChange({
  type: 'add',
  quad: {
    subject: namedNode('http://example.org/Alice'),
    predicate: namedNode('http://schema.org/name'),
    object: literal('Alice'),
  },
});
// Output: "http://example.org/Alice = Alice at 1712345678901"
```

---

## Use the New API Signature (filter first)

`SubscriptionManager.subscribe` accepts two signatures for backward compatibility:

```javascript
// Old API: subscribe(callback, filter)
manager.subscribe(callback, { subject: alice });

// New API: subscribe(filter, callback)
manager.subscribe({ subject: alice }, callback);
```

Both are equivalent. Prefer the new API in new code because the filter is the primary differentiator.

---

## List and Remove Subscriptions

```javascript
const id1 = manager.subscribe(cbA, { subject: alice });
const id2 = manager.subscribe(cbB, { predicate: namedNode('http://schema.org/name') });

// Inspect what is registered
const subs = manager.listSubscriptions();
// [{ id: 'sub_1', filter: { subject: ... } }, { id: 'sub_2', filter: { predicate: ... } }]

// Remove one
manager.unsubscribe(id1);

// Remove all
manager.clearSubscriptions();
```

## See Also

- [How-To: Handle Backpressure](./02-handle-backpressure.md)
- [Reference: Change Feed API](../reference/change-feed-api.md)
