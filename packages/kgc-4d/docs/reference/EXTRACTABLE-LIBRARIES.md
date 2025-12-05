# Extractable Library Modules from KGC 4D Playground

Ready-to-use library modules that can be extracted and published as npm packages for use in other projects.

---

## 1. Hook Registry (`@unrdf/hook-registry`)

A generic validation hook system for implementing extensible business logic.

### Export Structure
```
src/
├── HookRegistry.mjs      # Core registry class
├── hooks/
│   ├── built-ins.mjs     # Common validators (range, format, etc.)
│   └── index.mjs         # Hook exports
└── index.mjs             # Main export
```

### API
```javascript
// @unrdf/hook-registry

export class HookRegistry {
  constructor(options = {})

  // Register a validation hook
  register(field, validator, options = {})

  // Run validator for a field
  validate(field, value, context = {})

  // Run all validators
  validateAll(object, context = {})

  // Get validator for field
  getValidator(field)

  // List all registered fields
  listFields()
}

// Built-in validators
export const BuiltIn = {
  // Number validators
  range: (min, max) => (value) => { ... }
  min: (min) => (value) => { ... }
  max: (max) => (value) => { ... }
  integer: (value) => { ... }
  nonNegative: (value) => { ... }

  // String validators
  required: (value) => { ... }
  minLength: (len) => (value) => { ... }
  maxLength: (len) => (value) => { ... }
  pattern: (regex) => (value) => { ... }
  email: (value) => { ... }

  // Enum/choice validators
  oneOf: (allowed) => (value) => { ... }
  enum: (values) => (value) => { ... }

  // Composite
  and: (...validators) => (value, context) => { ... }
  or: (...validators) => (value, context) => { ... }
  custom: (fn) => fn
}

// Usage
const registry = new HookRegistry();

registry.register('budget', BuiltIn.range(0, 100000));
registry.register('email', BuiltIn.email);
registry.register('status', BuiltIn.oneOf(['active', 'paused', 'completed']));

const validation = registry.validate('budget', 50000);
console.log(validation); // { valid: true }
```

### Package Contents
- `HookRegistry` class with fluent API
- 15+ built-in validators
- TypeScript definitions
- Full test suite (95%+ coverage)
- Examples for 10 domains

### Publish To
- npm as `@unrdf/hook-registry`
- GitHub Releases with CHANGELOG

---

## 2. Delta State Machine (`@unrdf/delta-sync`)

Generic client-side state management for remote data sync with optimistic updates.

### Export Structure
```
src/
├── useDeltaSync.js       # React hook
├── DeltaReducer.mjs      # Reducer factory
├── states.mjs            # Connection states
└── index.mjs
```

### API
```javascript
// @unrdf/delta-sync

export const ConnectionState = {
  DISCONNECTED: 'disconnected',
  CONNECTING: 'connecting',
  CONNECTED: 'connected',
  SYNCING: 'syncing',
  ERROR: 'error',
};

export function useDeltaSync(options = {}) {
  // Returns state + actions

  return {
    // State
    state,
    connection,
    data,
    error,
    pendingDeltas,

    // Actions
    connect: (url, query) => Promise,
    disconnect: () => void,
    submitDelta: (operations, options) => Promise,
    refresh: () => Promise,

    // Helpers
    isConnected: boolean,
    isSyncing: boolean,
    hasError: boolean,
    pendingCount: number,
  };
}

// Reducer factory for custom implementations
export function createDeltaReducer(initialState, customHandlers) {
  return (state, action) => { ... }
}

// Usage in React
function MyComponent() {
  const {
    data,
    submitDelta,
    isConnected,
    error,
  } = useDeltaSync({
    url: '/api/sync',
    reconnectDelay: 3000,
    maxReconnectDelay: 30000,
  });

  const handleUpdate = async (changes) => {
    const result = await submitDelta(changes);
    if (!result.success) {
      showError(result.error);
    }
  };

  return (
    <div>
      {isConnected ? '🟢 Connected' : '🔴 Offline'}
      {error && <Alert>{error}</Alert>}
      {/* ... */}
    </div>
  );
}
```

### Package Contents
- `useDeltaSync` hook with auto-reconnect
- TypeScript definitions
- 5+ example implementations
- Full test suite
- Performance optimizations (batching, debouncing)

### Publish To
- npm as `@unrdf/delta-sync`

---

## 3. SSE Client (`@unrdf/sse-client`)

Flexible Server-Sent Events client with automatic reconnection and filtering.

### Export Structure
```
src/
├── SSEClient.mjs         # Core client class
├── useSSE.js             # React hook
├── useSubscription.js    # Subscription management
└── index.mjs
```

### API
```javascript
// @unrdf/sse-client

export class SSEClient {
  constructor(url, options = {})

  connect(handlers = {})
  disconnect()
  isConnected()
  reconnect()

  // Event methods
  on(eventType, handler)
  once(eventType, handler)
  off(eventType, handler)
  emit(eventType, data)
}

export function useSSE(url, options = {}) {
  const client = useRef(null);
  const [isConnected, setIsConnected] = useState(false);
  const [error, setError] = useState(null);

  return {
    isConnected,
    error,
    on: (eventType, handler) => client.current?.on(eventType, handler),
    once: (eventType, handler) => client.current?.once(eventType, handler),
    reconnect: () => client.current?.reconnect(),
  };
}

// Usage
const client = new SSEClient('/api/stream', {
  reconnectDelay: 3000,
  maxReconnectDelay: 30000,
  heartbeatTimeout: 60000,
});

client.on('connected', (data) => {
  console.log('Connected:', data);
});

client.on('update', (data) => {
  console.log('Update:', data);
});

client.onerror(() => {
  console.log('Connection error, reconnecting...');
});

client.connect();

// Or in React
function Dashboard() {
  const { isConnected, on } = useSSE('/api/dashboard/stream');

  useEffect(() => {
    on('metric', (metric) => {
      updateChart(metric);
    });
  }, [on]);

  return (
    <div>
      Status: {isConnected ? 'Live' : 'Connecting...'}
    </div>
  );
}
```

### Package Contents
- `SSEClient` class with event emitter pattern
- React hooks (`useSSE`, `useSubscription`)
- Auto-reconnection with exponential backoff
- Heartbeat-based connection health
- TypeScript definitions
- Examples with Vite, Next.js, SvelteKit

### Publish To
- npm as `@unrdf/sse-client`

---

## 4. Vector Clocks (`@unrdf/vector-clock`)

Causality tracking for distributed systems with conflict detection.

### Export Structure
```
src/
├── VectorClock.mjs
├── operations.mjs
└── index.mjs
```

### API
```javascript
// @unrdf/vector-clock

export class VectorClock {
  constructor(nodeId, initialCounters = {})

  // Get/set operations
  get(nodeId)
  increment(nodeId)
  merge(other)

  // Comparison (-1: behind, 0: concurrent, 1: ahead)
  compare(other)

  // Utility
  copy()
  toJSON()
  static fromJSON(data)

  // String representation
  toString()
}

// Usage
const clock1 = new VectorClock('node1');
clock1.increment('node1'); // { node1: 1 }

const clock2 = new VectorClock('node2');
clock2.increment('node2'); // { node2: 1 }

clock2.merge(clock1);      // { node1: 1, node2: 1 }

const clock3 = new VectorClock('node1');
clock3.increment('node1');
clock3.increment('node1'); // { node1: 2 }

clock1.compare(clock3);    // -1 (clock1 is behind)
clock2.compare(clock3);    // 0 (concurrent)
clock3.compare(clock1);    // 1 (clock3 is ahead)

// Detect conflicts
function detectConflict(clock1, clock2) {
  return clock1.compare(clock2) === 0;
}

// Merge causally
function mergeCausally(clock1, clock2) {
  return clock1.copy().merge(clock2);
}
```

### Package Contents
- `VectorClock` class with full causality semantics
- Comparison operators
- Serialization/deserialization
- Distributed system patterns
- Conflict detection examples
- TypeScript definitions

### Publish To
- npm as `@unrdf/vector-clock`

---

## 5. Event Store (`@unrdf/event-store`)

Simple immutable event log with time-travel reconstruction support.

### Export Structure
```
src/
├── EventStore.mjs
├── Event.mjs
├── reconstruction.mjs
└── index.mjs
```

### API
```javascript
// @unrdf/event-store

export class EventStore {
  constructor(options = {})

  // Append immutable event
  async append(event)

  // Query events
  async getEvents(query)
  async getEventsBetween(startTime, endTime)
  async getEventsSince(timestamp)

  // Time-travel: reconstruct state at time T
  async reconstructAt(targetTime, reconstructor)

  // Statistics
  async getEventCount()
  async getTimeRange()
}

export class Event {
  constructor(data)

  static create(type, payload, metadata = {})
}

// Reconstruction function
async function reconstructState(eventStore, targetTime) {
  let state = {};

  const events = await eventStore.getEventsBetween(0, targetTime);

  for (const event of events) {
    state = applyEvent(state, event);
  }

  return state;
}

// Usage
const store = new EventStore({
  storage: './events.db', // or memory, or custom
});

// Append events
await store.append(Event.create('USER_CREATED', {
  userId: 'user-1',
  name: 'Alice',
}));

await store.append(Event.create('USER_UPDATED', {
  userId: 'user-1',
  field: 'email',
  newValue: 'alice@example.com',
}));

// Query
const events = await store.getEventsSince(Date.now() - 3600000);

// Reconstruct at point in time
const stateAtNoon = await store.reconstructAt(
  new Date('2024-12-05T12:00:00Z'),
  (state, event) => {
    if (event.type === 'USER_CREATED') {
      state[event.payload.userId] = event.payload;
    } else if (event.type === 'USER_UPDATED') {
      state[event.payload.userId][event.payload.field] = event.payload.newValue;
    }
    return state;
  }
);
```

### Package Contents
- `EventStore` class with pluggable storage
- In-memory and file-based storage adapters
- Time-travel reconstruction patterns
- Event filtering and querying
- Full test suite
- Examples for audit logs, version control

### Publish To
- npm as `@unrdf/event-store`

---

## 6. Shard Projection (`@unrdf/shard-projection`)

Query-based projection system for deriving client-specific views from central data.

### Export Structure
```
src/
├── ShardProjector.mjs
├── Query.mjs
├── filters.mjs
└── index.mjs
```

### API
```javascript
// @unrdf/shard-projection

export class ShardProjector {
  constructor(universe)

  // Project subset of data based on query
  project(query)

  // Stream updates matching query
  subscribe(query, callback)
  unsubscribe(subscriptionId)
}

export class Query {
  constructor(filters = {})

  // Filter builders
  where(field, operator, value)
  select(...fields)
  limit(count)
  offset(count)

  // Aggregations
  count()
  sum(field)
  average(field)
}

// Usage
const projector = new ShardProjector(universeStore);

// Simple projection
const activeTasks = projector.project({
  entity: 'task',
  status: 'active',
});

// With Query builder
const query = new Query()
  .where('status', 'equals', 'completed')
  .where('assignee', 'equals', userId)
  .select('id', 'title', 'completedAt')
  .limit(10);

const shard = projector.project(query);

// Subscribe to changes
const subId = projector.subscribe(
  { entity: 'task', status: 'active' },
  (update) => {
    console.log('Task updated:', update);
  }
);

// Usage in React
function TaskList() {
  const [tasks, setTasks] = useState([]);

  useEffect(() => {
    const projector = new ShardProjector(universeStore);
    const query = { entity: 'task', status: 'active' };

    setTasks(projector.project(query));

    const subId = projector.subscribe(query, (update) => {
      setTasks(prev => applyUpdate(prev, update));
    });

    return () => projector.unsubscribe(subId);
  }, []);

  return (
    <ul>
      {tasks.map(task => <TaskItem key={task.id} task={task} />)}
    </ul>
  );
}
```

### Package Contents
- `ShardProjector` class with query builder
- Pre-built filters (equals, contains, range, etc.)
- Aggregation support (count, sum, average)
- Real-time subscription management
- Performance optimizations (indexing, caching)
- TypeScript definitions

### Publish To
- npm as `@unrdf/shard-projection`

---

## Publishing Strategy

### Repository Structure
```
@unrdf monorepo/
├── packages/
│   ├── hook-registry/
│   ├── delta-sync/
│   ├── sse-client/
│   ├── vector-clock/
│   ├── event-store/
│   ├── shard-projection/
│   └── ...
├── examples/
│   ├── e-commerce/
│   ├── document-editor/
│   ├── task-manager/
│   └── ...
└── docs/
```

### Release Checklist
- [ ] Run full test suite (>95% coverage)
- [ ] Update CHANGELOG.md
- [ ] Bump version (semver)
- [ ] Build TypeScript/JavaScript
- [ ] Check for breaking changes
- [ ] Publish to npm: `npm publish`
- [ ] Create GitHub release
- [ ] Update documentation
- [ ] Add examples to repo

### Package.json Template
```json
{
  "name": "@unrdf/hook-registry",
  "version": "1.0.0",
  "description": "Extensible validation hook system for business logic",
  "type": "module",
  "exports": {
    ".": "./dist/index.js",
    "./built-ins": "./dist/hooks/built-ins.js"
  },
  "keywords": ["validation", "hooks", "rules-engine"],
  "repository": "github:unrdf/unrdf",
  "license": "MIT",
  "files": ["dist", "README.md", "LICENSE"]
}
```

### README Template
```markdown
# @unrdf/hook-registry

Extensible validation hook system for implementing business logic and domain rules.

## Installation

npm install @unrdf/hook-registry

## Quick Start

import { HookRegistry, BuiltIn } from '@unrdf/hook-registry';

const registry = new HookRegistry();
registry.register('budget', BuiltIn.range(0, 100000));
const result = registry.validate('budget', 50000);

## Applicable To
- API validation
- Form submission
- Business rule engines
- Workflow engines

## Examples
- [E-commerce Inventory](../examples/e-commerce)
- [Task Management](../examples/task-manager)

## License
MIT
```

---

## Integration Examples

### Using in Next.js Project
```javascript
// app/api/update/route.js
import { HookRegistry } from '@unrdf/hook-registry';
import { SSEClient } from '@unrdf/sse-client';

const registry = new HookRegistry();
// ... register hooks ...

export async function POST(request) {
  const body = await request.json();

  // Validate using registry
  for (const [field, value] of Object.entries(body)) {
    const validation = registry.validate(field, value);
    if (!validation.valid) {
      return Response.json(
        { error: validation.reason },
        { status: 400 }
      );
    }
  }

  // ... process ...
}

// app/components/Dashboard.jsx
import { useSSE } from '@unrdf/sse-client';

export function Dashboard() {
  const { isConnected } = useSSE('/api/stream');
  return <div>Status: {isConnected ? 'Live' : 'Offline'}</div>;
}
```

### Using in SvelteKit Project
```javascript
// src/lib/stores/delta.js
import { useDeltaSync } from '@unrdf/delta-sync';

export const deltaStore = writable(null);

export async function initDeltaSync() {
  const { data, submitDelta, isConnected } = useDeltaSync({
    url: '/api/sync',
  });

  // Wire to Svelte store
  data.subscribe(v => deltaStore.set(v));
}
```

---

## Summary

| Package | Purpose | Size | Dependencies |
|---------|---------|------|---|
| hook-registry | Extensible validation | 8KB | 0 |
| delta-sync | Client state sync | 12KB | 1 (react) |
| sse-client | Real-time updates | 10KB | 0 |
| vector-clock | Causality tracking | 6KB | 0 |
| event-store | Immutable event log | 15KB | 1 (lmdb optional) |
| shard-projection | Data projection | 14KB | 0 |

All packages:
- Zero dependencies (or optional peer deps only)
- Tree-shakeable
- Full TypeScript support
- <50KB total with all 6

These can be published individually or as a suite under `@unrdf` organization on npm.
