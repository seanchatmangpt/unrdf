# Change Feeds Example

Demonstrates change feed creation, subscription, history tracking, and replay capabilities in `@unrdf/streaming`.

## Features

- **Change Feed Creation**: Create feeds that track all store modifications
- **Real-time Subscriptions**: Subscribe to changes as they happen
- **Change History**: Query historical changes with timestamps
- **Replay Capability**: Replay changes to reconstruct store state
- **Filtered Subscriptions**: Filter changes by type (add/remove)
- **Time-based Queries**: Query changes within time ranges

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

### 1. Basic Change Feed

Creates a change feed and subscribes to all changes:

```javascript
import { createChangeFeed } from '@unrdf/streaming';

const store = new Store();
const feed = createChangeFeed(store);

const unsubscribe = feed.subscribe((change) => {
  console.log(`Change: ${change.type}`);
  console.log(`Quad: ${change.quad}`);
  console.log(`Timestamp: ${change.timestamp}`);
});

store.addQuad(quad(alice, name, literal('Alice')));
```

### 2. Change History

Track and query historical changes:

```javascript
store.addQuad(quad(alice, name, literal('Alice')));
store.addQuad(quad(alice, age, literal('30')));

const history = feed.getHistory();
console.log(`Total changes: ${history.length}`);
```

### 3. Replay Changes

Replay changes to reconstruct store state:

```javascript
const newStore = new Store();

feed.replay((change) => {
  if (change.type === 'add') {
    newStore.addQuad(change.quad);
  } else if (change.type === 'remove') {
    newStore.removeQuad(change.quad);
  }
});
```

### 4. Filtered Subscriptions

Subscribe to specific change types:

```javascript
feed.subscribe((change) => {
  if (change.type === 'add') {
    console.log(`Added: ${change.quad.object.value}`);
  }
});
```

### 5. Time-based Queries

Query changes within time ranges:

```javascript
const allChanges = feed.getHistory();
const recentChanges = feed.getHistory({ since: Date.now() - 60000 });
```

## API Reference

### `createChangeFeed(store)`

Creates a change feed for the given store.

**Parameters:**
- `store` - N3.Store instance to track

**Returns:** ChangeFeed instance

### `ChangeFeed.subscribe(callback)`

Subscribe to changes.

**Parameters:**
- `callback(change)` - Function called for each change

**Returns:** Unsubscribe function

### `ChangeFeed.getHistory(options?)`

Get change history.

**Parameters:**
- `options.since` - Optional timestamp to filter changes

**Returns:** Array of changes

### `ChangeFeed.replay(callback)`

Replay all changes.

**Parameters:**
- `callback(change)` - Function called for each historical change

## Change Object

Each change has:

```typescript
{
  type: 'add' | 'remove',
  quad: Quad,
  timestamp: number
}
```

## Testing

The example includes comprehensive tests:

- Change feed creation and subscription
- History tracking
- Replay functionality
- Filtered subscriptions
- Time-based queries

Run tests with `pnpm test`.

## Learn More

- [UNRDF Documentation](https://unrdf.dev)
- [Streaming Package](../../README.md)
- [Real-time Sync Example](../real-time-sync/)
