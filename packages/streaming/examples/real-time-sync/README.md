# Real-Time Sync Example

Demonstrates subscription management, real-time graph monitoring, concurrent updates, conflict resolution, and multi-peer synchronization in `@unrdf/streaming`.

## Features

- **Subscription Management**: Subscribe to specific graph patterns
- **Real-time Monitoring**: Monitor graph changes as they happen
- **Concurrent Updates**: Handle simultaneous modifications
- **Conflict Resolution**: Detect and resolve update conflicts
- **Multi-Peer Sync**: Synchronize across distributed stores
- **Subscription Patterns**: Advanced pattern matching

## Usage

```bash
# Install dependencies
pnpm install

# Run the example
pnpm start

# Run tests
pnpm test
```

## Examples

### 1. Basic Subscription

Subscribe to specific graph patterns:

```javascript
import { createSubscriptionManager } from '@unrdf/streaming';

const store = new Store();
const manager = createSubscriptionManager(store);

const id = manager.subscribe({
  subject: alice,
  predicate: name
}, (quads) => {
  console.log(`Name updated: ${quads[0].object.value}`);
});

store.addQuad(quad(alice, name, literal('Alice')));
```

### 2. Real-Time Monitoring

Monitor all graph changes:

```javascript
// Monitor all changes
manager.subscribe({}, (quads) => {
  console.log(`Graph updated: ${quads.length} quads`);
});

// Monitor specific predicate
manager.subscribe({
  predicate: knows
}, (quads) => {
  console.log('Social relationship updated');
});
```

### 3. Concurrent Updates

Handle concurrent modifications with conflict detection:

```javascript
manager.subscribe({
  subject: alice,
  predicate: age
}, (quads) => {
  if (quads.length > 1) {
    console.log('Conflict detected!');
    // Resolve with last-write-wins
    const latest = quads[quads.length - 1];
    quads.slice(0, -1).forEach(q => store.removeQuad(q));
  }
});

// Concurrent updates
store.addQuad(quad(alice, age, literal('30')));
store.addQuad(quad(alice, age, literal('31'))); // Conflict!
```

### 4. Multi-Peer Sync

Synchronize across distributed stores:

```javascript
const peer1 = new Store();
const peer2 = new Store();

const feed1 = createChangeFeed(peer1);

// Sync peer1 -> peer2
feed1.subscribe((change) => {
  if (change.type === 'add') {
    peer2.addQuad(change.quad);
  } else if (change.type === 'remove') {
    peer2.removeQuad(change.quad);
  }
});

peer1.addQuad(quad(alice, name, literal('Alice')));
// Change automatically propagates to peer2
```

### 5. Subscription Patterns

Advanced pattern matching:

```javascript
// All properties of a subject
manager.subscribe({ subject: alice }, callback);

// All subjects with a predicate
manager.subscribe({ predicate: name }, callback);

// Specific triple pattern
manager.subscribe({
  predicate: knows,
  object: bob
}, callback);
```

## API Reference

### `createSubscriptionManager(store)`

Creates a subscription manager for the given store.

**Parameters:**
- `store` - N3.Store instance to manage

**Returns:** SubscriptionManager instance

### `SubscriptionManager.subscribe(pattern, callback)`

Subscribe to changes matching a pattern.

**Parameters:**
- `pattern` - Triple pattern to match (subject, predicate, object)
- `callback(quads)` - Function called when matching changes occur

**Returns:** Subscription ID (string)

### `SubscriptionManager.unsubscribe(id)`

Unsubscribe from changes.

**Parameters:**
- `id` - Subscription ID returned from `subscribe()`

## Pattern Matching

Subscription patterns support:

- **Exact match**: `{ subject: alice }` - Matches specific subject
- **Predicate filter**: `{ predicate: name }` - Matches specific predicate
- **Object filter**: `{ object: bob }` - Matches specific object
- **Wildcard**: `{}` - Matches all changes
- **Combinations**: `{ subject: alice, predicate: name }` - Multiple filters

## Conflict Resolution

Common strategies:

- **Last-Write-Wins**: Keep most recent value
- **First-Write-Wins**: Keep first value
- **Merge**: Combine values (application-specific)
- **User Prompt**: Ask user to resolve
- **Timestamp**: Use explicit timestamps

## Testing

The example includes comprehensive tests:

- Subscription management
- Real-time monitoring
- Concurrent updates
- Multi-peer sync
- Subscription patterns
- Circular update prevention

Run tests with `pnpm test`.

## Learn More

- [UNRDF Documentation](https://unrdf.dev)
- [Streaming Package](../../README.md)
- [Change Feeds Example](../change-feeds/)
