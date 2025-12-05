# Reference: Streaming Hooks API

Complete API reference for real-time streaming hooks.

**Package:** `unrdf-react`
**Category:** `streaming`

---

## useChangeFeed

Subscribe to real-time changes in the knowledge graph.

### Import

```jsx
import { useChangeFeed } from 'unrdf-react';
```

### Signature

```typescript
function useChangeFeed(options?: ChangeFeedOptions): ChangeEvent[]
```

### Parameters

```typescript
interface ChangeFeedOptions {
  // Filter changes
  filter?: (change: ChangeEvent) => boolean;
  predicateFilter?: string[];
  subjectFilter?: string[];

  // Update callbacks
  onUpdate?: (change: ChangeEvent) => void;
  onAdd?: (change: ChangeEvent) => void;
  onDelete?: (change: ChangeEvent) => void;

  // Performance
  throttle?: number;      // Throttle updates (ms)
  debounce?: number;      // Debounce updates (ms)
  maxHistory?: number;    // Max changes to keep in memory

  // Subscription control
  enabled?: boolean;      // Enable/disable subscription
}
```

### Returns

```typescript
type ChangeEvent = {
  type: 'add' | 'delete';
  triple: Triple;
  timestamp: number;
  source?: string;
}

// Returns array of recent changes
ChangeEvent[]
```

### Example

```jsx
import { useChangeFeed } from 'unrdf-react';

function LiveFeed() {
  const changes = useChangeFeed({
    // Only watch person-related changes
    filter: (change) => change.triple.subject.includes('/people/'),

    // Callback on updates
    onUpdate: (change) => {
      console.log('Change detected:', change);
      if (change.type === 'add') {
        showNotification('New person added!');
      }
    },

    // Performance tuning
    throttle: 100,
    maxHistory: 50,
  });

  return (
    <div>
      <h3>Recent Changes ({changes.length})</h3>
      <ul>
        {changes.slice(-10).map((change, idx) => (
          <li key={idx}>
            <span className={change.type === 'add' ? 'text-green-600' : 'text-red-600'}>
              {change.type === 'add' ? '+' : '-'}
            </span>
            {' '}
            {change.triple.predicate.split('/').pop()}: {change.triple.object}
          </li>
        ))}
      </ul>
    </div>
  );
}
```

---

## useSubscription

Subscribe to specific triple patterns with real-time updates.

### Import

```jsx
import { useSubscription } from 'unrdf-react';
```

### Signature

```typescript
function useSubscription(
  pattern: TriplePattern,
  options?: SubscriptionOptions
): SubscriptionResult
```

### Parameters

```typescript
interface SubscriptionOptions {
  onData?: (triples: Triple[]) => void;
  onError?: (error: Error) => void;
  initialData?: Triple[];
  enabled?: boolean;
}
```

### Returns

```typescript
interface SubscriptionResult {
  data: Triple[];
  isSubscribed: boolean;
  error: Error | null;
  unsubscribe: () => void;
}
```

### Example

```jsx
import { useSubscription } from 'unrdf-react';

function LivePeopleList() {
  const { data: people, isSubscribed } = useSubscription({
    predicate: 'http://xmlns.com/foaf/0.1/name'
  }, {
    onData: (triples) => {
      console.log('People updated:', triples.length);
    }
  });

  return (
    <div>
      {isSubscribed && <span className="text-green-600">● Live</span>}
      <ul>
        {people.map((triple, idx) => (
          <li key={idx}>{triple.object}</li>
        ))}
      </ul>
    </div>
  );
}
```

---

## useStreamProcessor

Process streaming RDF data with custom transformations.

### Import

```jsx
import { useStreamProcessor } from 'unrdf-react';
```

### Signature

```typescript
function useStreamProcessor<T>(
  processor: StreamProcessor<T>,
  options?: ProcessorOptions
): ProcessorResult<T>
```

### Parameters

```typescript
interface StreamProcessor<T> {
  process: (change: ChangeEvent) => T | null;
  aggregate?: (accumulated: T[], newItem: T) => T[];
}

interface ProcessorOptions {
  bufferSize?: number;
  flushInterval?: number;
}
```

### Returns

```typescript
interface ProcessorResult<T> {
  data: T[];
  flush: () => void;
  clear: () => void;
}
```

### Example

```jsx
import { useStreamProcessor } from 'unrdf-react';

function AggregatedStats() {
  const { data: stats } = useStreamProcessor({
    process: (change) => {
      if (change.type !== 'add') return null;

      return {
        timestamp: change.timestamp,
        type: change.triple.object,
      };
    },
    aggregate: (accumulated, newItem) => {
      // Keep only last 100 items
      return [...accumulated, newItem].slice(-100);
    }
  });

  const typeCounts = stats.reduce((acc, item) => {
    acc[item.type] = (acc[item.type] || 0) + 1;
    return acc;
  }, {});

  return (
    <div>
      {Object.entries(typeCounts).map(([type, count]) => (
        <p key={type}>{type}: {count}</p>
      ))}
    </div>
  );
}
```

---

## useRealtimeQuery

Execute queries that automatically re-run when relevant data changes.

### Import

```jsx
import { useRealtimeQuery } from 'unrdf-react';
```

### Signature

```typescript
function useRealtimeQuery(
  query: string,
  options?: RealtimeQueryOptions
): QueryResult
```

### Parameters

```typescript
interface RealtimeQueryOptions extends QueryOptions {
  // Invalidation triggers
  invalidateOn?: (change: ChangeEvent) => boolean;

  // Auto-refresh
  refetchInterval?: number;
}
```

### Example

```jsx
import { useRealtimeQuery } from 'unrdf-react';

function LiveStatistics() {
  const { data } = useRealtimeQuery(
    `
      SELECT (COUNT(?person) as ?count) WHERE {
        ?person a foaf:Person
      }
    `,
    {
      // Re-run query when person added/removed
      invalidateOn: (change) => {
        return change.triple.object === 'http://xmlns.com/foaf/0.1/Person';
      }
    }
  );

  return <div>Total people: {data?.[0]?.count || 0}</div>;
}
```

---

## useDeltaTracking

Track deltas between graph states for undo/redo functionality.

### Import

```jsx
import { useDeltaTracking } from 'unrdf-react';
```

### Signature

```typescript
function useDeltaTracking(options?: DeltaOptions): DeltaTracker
```

### Parameters

```typescript
interface DeltaOptions {
  maxHistory?: number;
  autoTrack?: boolean;
}
```

### Returns

```typescript
interface DeltaTracker {
  deltas: Delta[];
  undo: () => void;
  redo: () => void;
  canUndo: boolean;
  canRedo: boolean;
  clear: () => void;
}

interface Delta {
  added: Triple[];
  deleted: Triple[];
  timestamp: number;
}
```

### Example

```jsx
import { useDeltaTracking } from 'unrdf-react';

function UndoableEditor() {
  const { undo, redo, canUndo, canRedo, deltas } = useDeltaTracking({
    maxHistory: 50
  });

  return (
    <div>
      <button onClick={undo} disabled={!canUndo}>Undo</button>
      <button onClick={redo} disabled={!canRedo}>Redo</button>
      <p>History: {deltas.length} changes</p>
    </div>
  );
}
```

---

## usePresence

Track presence of multiple users/sessions.

### Import

```jsx
import { usePresence } from 'unrdf-react';
```

### Signature

```typescript
function usePresence(options: PresenceOptions): PresenceContext
```

### Parameters

```typescript
interface PresenceOptions {
  channel: string;
  metadata?: Record<string, any>;
  heartbeatInterval?: number;
}
```

### Returns

```typescript
interface PresenceContext {
  users: PresenceUser[];
  announce: (metadata?: Record<string, any>) => void;
  leave: () => void;
  isPresent: boolean;
}

interface PresenceUser {
  id: string;
  metadata: Record<string, any>;
  lastSeen: number;
}
```

### Example

```jsx
import { usePresence } from 'unrdf-react';

function CollaborativeEditor() {
  const { users, announce } = usePresence({
    channel: 'document-123',
    metadata: {
      name: 'Alice',
      color: '#3b82f6'
    },
    heartbeatInterval: 5000
  });

  useEffect(() => {
    announce();
  }, [announce]);

  return (
    <div>
      <h3>Active Users ({users.length})</h3>
      {users.map(user => (
        <div key={user.id} style={{ color: user.metadata.color }}>
          {user.metadata.name}
        </div>
      ))}
    </div>
  );
}
```

---

## Best Practices

1. **Filter changes to reduce overhead:**
   ```jsx
   useChangeFeed({
     filter: (change) => change.triple.subject.startsWith('http://my-domain/'),
   })
   ```

2. **Throttle high-frequency updates:**
   ```jsx
   useChangeFeed({
     throttle: 100, // Max 10 updates/second
   })
   ```

3. **Limit history size:**
   ```jsx
   useChangeFeed({
     maxHistory: 100, // Keep last 100 changes only
   })
   ```

4. **Unsubscribe when not needed:**
   ```jsx
   useChangeFeed({
     enabled: isActive, // Conditionally subscribe
   })
   ```

5. **Handle offline scenarios:**
   ```jsx
   const { isOnline } = useOnlineStatus();
   const changes = useChangeFeed({
     enabled: isOnline,
   });
   ```

---

## Performance Considerations

| Operation | Cost | Recommendation |
|-----------|------|----------------|
| `useChangeFeed` (no filter) | High | Always filter if possible |
| `useChangeFeed` (with filter) | Medium | Good for most use cases |
| `useSubscription` (specific pattern) | Low | Best for targeted updates |
| `useStreamProcessor` | Medium | Buffer and batch updates |
| `useDeltaTracking` | High | Limit maxHistory |

---

## Related

- [Tutorial: Real-time Streaming](../tutorials/03-real-time-streaming.md)
- [How-to: Implement Offline Sync](../how-to/implement-offline-sync.md)
- [Explanation: Streaming Architecture](../explanation/streaming-architecture.md)
