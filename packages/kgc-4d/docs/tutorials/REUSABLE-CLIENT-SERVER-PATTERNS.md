# Reusable Client/Server Patterns from KGC 4D Playground

Extracted architectural patterns and components from the KGC 4D playground that can be applied to different contexts beyond RDF/event-sourced systems.

## Table of Contents

1. [Core Patterns](#core-patterns)
2. [Server Components](#server-components)
3. [Client Components](#client-components)
4. [Real-Time Communication](#real-time-communication)
5. [Validation & Hooks](#validation--hooks)
6. [State Management](#state-management)
7. [Adaptation Guide](#adaptation-guide)

---

## Core Patterns

### 1. Delta-Based Mutation Architecture

**Problem Solved**: How to handle user updates with validation, causality tracking, and server consensus.

**Pattern**: Users submit deltas (operations describing changes) instead of direct mutations. Server validates, applies atomically, and broadcasts confirmations.

**Generic Flow**:
```
Client → Submit Delta (batch of operations)
           ↓
Server → Validate via hooks
           ↓
           Apply atomically
           ↓
           Broadcast to subscribers
           ↓
Client ← ACK/REJECT response
```

**Applicable To**:
- Task management systems (submit batch task updates)
- Collaborative editing (operations-based collaboration)
- Financial ledgers (transaction deltas)
- Workflow systems (state transition operations)
- Document versioning (change-set operations)

**Implementation Abstraction**:
```javascript
// Generic delta submission pattern
export async function submitDelta(delta) {
  // 1. Causality check (vector clocks, timestamps, etc.)
  // 2. Validate each operation through hooks
  // 3. Convert operations to internal format
  // 4. Atomic commit
  // 5. Broadcast to subscribers
  // 6. Return ACK/REJECT
}
```

---

## Server Components

### 2. Validation Hooks System

**Problem Solved**: How to implement extensible validation for domain-specific rules without hardcoding them into the core logic.

**Pattern**: Hooks are domain-agnostic predicates (or fields) that map to validation functions. Each hook declares what it validates and the validation rule.

**Generic Structure**:
```javascript
const HOOKS = {
  VALIDATE_BUDGET: {
    id: 'validate-budget',
    field: 'budget',  // or predicate in RDF
    validate: (value) => {
      // Your domain logic here
      if (value > 100000) {
        return { valid: false, reason: 'Budget exceeded' };
      }
      return { valid: true };
    },
  },

  VALIDATE_STATUS: {
    id: 'validate-status',
    field: 'status',
    validate: (value) => {
      const allowed = ['active', 'paused', 'completed'];
      if (!allowed.includes(value)) {
        return { valid: false, reason: 'Invalid status' };
      }
      return { valid: true };
    },
  },
};

// Generic runner
function runValidationHooks(operation) {
  for (const hook of Object.values(HOOKS)) {
    if (hook.field === operation.field) {
      return hook.validate(operation.value);
    }
  }
  return { valid: true };
}
```

**Applicable To**:
- API request validation
- Form submission validation
- Data import/export validation
- Configuration validation
- Workflow state transitions

**Adaptable Properties**:
- Replace `predicate` (RDF term) with `field` (JSON property)
- Replace `object.value` with property value
- Extend with metadata: `reason`, `severity`, `autoFix`

---

### 3. Universe Store Pattern (Centralized Source of Truth)

**Problem Solved**: How to maintain a single consistent view of data that all clients sync with.

**Pattern**: Central server store acts as the "Universe" - single source of truth. All clients subscribe to projections of it called "Shards".

**Generic Components**:
```javascript
// Central Universe store
class Universe {
  async getState() { /* return full state */ }
  async appendEvent(event, deltas) { /* atomic append */ }
  subscribe(query) { /* return stream of updates */ }
}

// Shard projection (client view)
function projectShard(query) {
  // Filter Universe state based on query
  // Return subset relevant to client
  // Include version info for sync
}
```

**Applicable To**:
- Microservices architectures (shared domain model)
- Distributed caching (consistent cache invalidation)
- Multi-tenant systems (per-tenant shards)
- Real-time dashboards (live data projections)
- Collaborative apps (shared document with views)

**Benefits**:
- Single point of truth eliminates sync conflicts
- Projections reduce data transfer
- Easy audit trail (all events in Universe)

---

### 4. Causality Tracking with Vector Clocks

**Problem Solved**: How to detect concurrent modifications and maintain causal ordering in distributed systems.

**Pattern**: Each entity or update carries a vector clock showing which clients have seen which versions. When merging concurrent changes, compare clocks to detect conflicts.

**Generic Implementation**:
```javascript
// Vector clock: { nodeId, counters: { node1: 5, node2: 3, ... } }
class VectorClock {
  compare(other) {
    // Returns: -1 (behind), 0 (concurrent), 1 (ahead)
  }

  increment(nodeId) {
    // Increment this node's counter
  }

  merge(other) {
    // Merge with another clock (take max of each counter)
  }
}

// Usage in delta submission
if (delta.client_vector_clock) {
  const comparison = clientClock.compare(serverClock);
  if (comparison === -1) {
    // Client is behind - okay, we'll merge
  } else if (comparison === 0) {
    // Concurrent modification - conflict
  }
}
```

**Applicable To**:
- Multi-region databases
- Mobile app sync (offline changes)
- Collaborative editing (CRDTs)
- Event replication
- Blockchain consensus

---

### 5. Event Sourcing (Immutable Event Log)

**Problem Solved**: How to maintain a complete audit trail and enable time-travel reconstruction.

**Pattern**: Never delete data. Instead, append immutable events. Reconstruct any state by replaying events up to a target time.

**Generic Event Structure**:
```javascript
{
  id: 'event-uuid',
  t_ns: 1702834560000000000n,  // Nanosecond timestamp
  type: 'UPDATE',  // Event type
  source: 'client-1',
  payload: {
    // Domain-specific data
  },
  deltas: [
    { type: 'add', /* change */ },
    { type: 'delete', /* change */ },
  ],
  vector_clock: { /* causality info */ },
}
```

**Applicable To**:
- Financial audit trails (regulatory compliance)
- Git-like version control
- Bug reproduction (replay event sequence)
- Data recovery (restore from any point)
- Analytics (reanalyze historical data)

---

## Client Components

### 6. React Context for Remote State Sync

**Problem Solved**: How to sync complex remote state with React components while handling connection loss, optimistic updates, and error rollback.

**Pattern**: React Context stores connection state + remote data. Reducer handles optimistic updates and server confirmations.

**Generic Actions**:
```javascript
const Actions = {
  CONNECT: 'CONNECT',              // Initiating connection
  CONNECTED: 'CONNECTED',          // Connection established
  DISCONNECT: 'DISCONNECT',        // Lost connection
  SET_STATE: 'SET_STATE',          // Received server state
  APPLY_DELTA: 'APPLY_DELTA',      // Local change (optimistic)
  QUEUE_DELTA: 'QUEUE_DELTA',      // Waiting for ACK
  ACK: 'ACK',                      // Server confirmed
  REJECT: 'REJECT',                // Server rejected (rollback)
  ERROR: 'ERROR',                  // Connection/validation error
};
```

**Generic Reducer Pattern**:
```javascript
function reducer(state, action) {
  switch (action.type) {
    case Actions.APPLY_DELTA:
      // Optimistic: apply immediately, hope server agrees
      return { ...state, data: applyDelta(state.data, action.delta) };

    case Actions.ACK:
      // Server confirmed: update version, remove from pending
      return { ...state, version: action.version };

    case Actions.REJECT:
      // Server rejected: refresh from server (conflict resolution)
      return { ...state, error: action.reason };
  }
}
```

**Applicable To**:
- Chat applications
- Real-time forms
- Collaborative documents
- Live dashboards
- Multiplayer games

---

### 7. SSE-Based Real-Time Subscription

**Problem Solved**: How to stream server updates to clients in real-time with automatic reconnection and filtering.

**Pattern**: Client opens Server-Sent Events connection with query filter. Server maintains subscription registry and pushes updates matching filter.

**Generic Events**:
```javascript
// Client → Server: SSE connection with query
GET /api/stream?subject=user-123&type=message

// Server → Client: Event stream
event: connected
data: { subscriptionId: "...", timestamp: "..." }

event: delta
data: { type: 'message', subject: 'user-123', ... }

event: heartbeat
data: { timestamp: "..." }
```

**Client-Side Pattern**:
```javascript
const eventSource = new EventSource('/api/stream?filter=...');

eventSource.addEventListener('connected', (e) => {
  const data = JSON.parse(e.data);
  setState({ isConnected: true });
});

eventSource.addEventListener('delta', (e) => {
  const data = JSON.parse(e.data);
  applyUpdate(data);
});

eventSource.onerror = () => {
  // Auto-reconnect after backoff
};
```

**Applicable To**:
- Live notifications
- Real-time dashboards
- Activity feeds
- Sensor data streaming
- Chat applications

**Advantages Over WebSocket**:
- HTTP-based (simpler infrastructure)
- Automatic reconnection
- Built-in keepalive via heartbeat
- Easier to debug (plain text protocol)

---

### 8. Optimistic Updates with Rollback

**Problem Solved**: How to provide instant UI feedback while waiting for server confirmation, with automatic rollback on rejection.

**Pattern**:
1. User makes change
2. Apply locally immediately (optimistic)
3. Send to server
4. If ACK: keep change
5. If REJECT: rollback and show error

**Generic Implementation**:
```javascript
async function submitChange(change) {
  const changeId = uuid();

  // Step 1: Optimistic update
  dispatch({ type: 'APPLY_CHANGE', change, changeId });

  // Step 2: Queue for server
  dispatch({ type: 'QUEUE_CHANGE', changeId });

  try {
    // Step 3: Send to server
    const response = await fetch('/api/change', {
      method: 'POST',
      body: JSON.stringify(change),
    });

    const result = await response.json();

    if (result.status === 'ACK') {
      // Step 4a: Confirm
      dispatch({ type: 'ACK', changeId, newVersion: result.version });
      return { success: true };
    } else {
      // Step 4b: Reject - rollback
      dispatch({ type: 'REJECT', changeId, reason: result.reason });
      return { success: false, error: result.reason };
    }
  } catch (error) {
    dispatch({ type: 'REJECT', changeId, reason: error.message });
    return { success: false, error: error.message };
  }
}
```

**Applicable To**:
- Any web app requiring responsive UI
- Mobile apps with unreliable connections
- Real-time collaborative tools

---

## Real-Time Communication

### 9. Subscription Registry Pattern

**Problem Solved**: How to efficiently route server updates to specific clients based on their interests.

**Pattern**: Maintain a registry of active subscriptions with query filters. When server event happens, find matching subscriptions and push to those clients.

**Generic Implementation**:
```javascript
class SubscriptionRegistry {
  constructor() {
    this.subscriptions = new Map(); // subscriptionId → { query, callback }
  }

  register(subscriptionId, query, callback) {
    this.subscriptions.set(subscriptionId, { query, callback });
  }

  unregister(subscriptionId) {
    this.subscriptions.delete(subscriptionId);
  }

  // Find subscribers interested in this event
  async broadcast(event) {
    for (const [id, { query, callback }] of this.subscriptions) {
      if (this.matches(event, query)) {
        await callback(event);
      }
    }
  }

  matches(event, query) {
    // Check if event matches query filter
    if (query.subject && event.subject !== query.subject) return false;
    if (query.type && event.type !== query.type) return false;
    return true;
  }
}
```

**Applicable To**:
- Notification systems
- Real-time feeds
- Pub/sub messaging
- Event-driven architectures

---

### 10. Heartbeat/Keep-Alive Pattern

**Problem Solved**: How to detect stale connections and prevent proxy/firewall timeouts.

**Pattern**: Server sends periodic heartbeat events. Client uses them to detect disconnection. Both parties benefit from keepalive traffic.

**Server-Side**:
```javascript
setInterval(async () => {
  for (const [subscriptionId, { callback }] of subscriptions) {
    await callback({
      event: 'heartbeat',
      timestamp: new Date().toISOString(),
    });
  }
}, 30000); // Every 30 seconds
```

**Client-Side**:
```javascript
let lastHeartbeat = Date.now();

eventSource.addEventListener('heartbeat', (e) => {
  lastHeartbeat = Date.now();
});

// Detect missed heartbeats
setInterval(() => {
  if (Date.now() - lastHeartbeat > 60000) {
    // Heartbeat missed - connection likely dead
    reconnect();
  }
}, 10000);
```

---

## Validation & Hooks

### 11. Predicate-Based Validation Dispatch

**Problem Solved**: How to map arbitrary fields/predicates to validation rules without central switch statements.

**Pattern**: Create hook registry indexed by field/predicate. Validator looks up rule and applies it.

**Benefits**:
- Decoupled: validators don't need to know about each other
- Extensible: add new validators by adding to registry
- Testable: each validator is isolated
- Type-safe: can use TypeScript to validate hook structure

**Example Domain Adaptations**:

```javascript
// E-commerce context
const HOOKS = {
  VALIDATE_PRICE: {
    field: 'price',
    validate: (value) => {
      if (value < 0) return { valid: false, reason: 'Price cannot be negative' };
      if (value > 999999) return { valid: false, reason: 'Price too high' };
      return { valid: true };
    },
  },
  VALIDATE_SKU: {
    field: 'sku',
    validate: (value) => {
      if (!/^[A-Z0-9]{6,12}$/.test(value)) {
        return { valid: false, reason: 'Invalid SKU format' };
      }
      return { valid: true };
    },
  },
};

// Project management context
const HOOKS = {
  VALIDATE_PRIORITY: {
    field: 'priority',
    validate: (value) => {
      if (!['LOW', 'MEDIUM', 'HIGH', 'CRITICAL'].includes(value)) {
        return { valid: false, reason: 'Invalid priority' };
      }
      return { valid: true };
    },
  },
  VALIDATE_DEADLINE: {
    field: 'deadline',
    validate: (value) => {
      const date = new Date(value);
      if (date < new Date()) {
        return { valid: false, reason: 'Deadline must be in future' };
      }
      return { valid: true };
    },
  },
};
```

---

## State Management

### 12. Reducer-Based Sync State Machine

**Problem Solved**: How to model complex connection states and transitions.

**Pattern**: Explicit states and actions make state transitions clear and testable.

**Generic State Machine**:
```javascript
const States = {
  DISCONNECTED: 'disconnected',  // No connection attempt
  CONNECTING: 'connecting',      // Connection in progress
  CONNECTED: 'connected',        // Connected to server
  SYNCING: 'syncing',           // Waiting for server confirmation
  ERROR: 'error',               // Connection or validation error
};

const Actions = {
  CONNECT: 'CONNECT',           // User initiates connection
  CONNECTING: 'CONNECTING',     // Transition: connecting
  CONNECTED: 'CONNECTED',       // Transition: connected
  DISCONNECT: 'DISCONNECT',     // User initiated disconnect
  ERROR: 'ERROR',               // Error occurred
  ACK: 'ACK',                   // Server ACK received
  REJECT: 'REJECT',             // Server REJECT received
};

// State transitions (reducer)
function reducer(state, action) {
  switch (action.type) {
    case Actions.CONNECT:
      return { ...state, connection: States.CONNECTING };
    case Actions.CONNECTED:
      return { ...state, connection: States.CONNECTED };
    case Actions.DISCONNECT:
      return { ...state, connection: States.DISCONNECTED };
    case Actions.ERROR:
      return { ...state, connection: States.ERROR, error: action.error };
    default:
      return state;
  }
}
```

**Applicable To**:
- WebSocket connections
- Authentication flows
- Async form submissions
- Multi-step workflows

---

## Adaptation Guide

### How to Adapt These Patterns to Your Domain

#### Step 1: Identify Your "Universe"
The central source of truth in your system. Examples:
- RDF store (KGC 4D)
- SQL database
- Document store
- Configuration server

#### Step 2: Define Your "Delta"
The unit of change in your system. Examples:
- RDF triple operations (add/delete)
- SQL INSERT/UPDATE/DELETE
- JSON patch operations
- State machine transitions

#### Step 3: Define Your Validation Hooks
Domain-specific rules for your deltas. Examples:
- Budget constraints
- Email format validation
- Workflow state rules
- Business logic constraints

#### Step 4: Choose Communication Method
- SSE (stateless, HTTP, simple) ← KGC 4D uses this
- WebSocket (stateful, bidirectional, complex)
- Polling (simple, inefficient)
- gRPC (typed, complex setup)

#### Step 5: Implement Shards
Projections of Universe optimized for specific client needs. Examples:
- User can only see their own data
- Mobile app sees reduced dataset
- Admin sees everything
- Dashboard sees aggregated data

#### Step 6: Add Vector Clocks (If Distributed)
Only needed if:
- Multiple writers exist
- Network partitions possible
- Conflict resolution needed

Otherwise, simple version numbers often suffice.

#### Step 7: Implement Event Sourcing (If Needed)
Add immutable event log if you need:
- Audit trail
- Time-travel queries
- Compliance requirements
- Analytics on historical data

---

## Reusable Code Modules

### Generic Delta Validation
```javascript
// validators/base.mjs
export class HookRegistry {
  constructor() {
    this.hooks = new Map();
  }

  register(field, validator) {
    this.hooks.set(field, validator);
  }

  validate(field, value) {
    const validator = this.hooks.get(field);
    if (!validator) return { valid: true };
    return validator(value);
  }
}

// Usage
const registry = new HookRegistry();
registry.register('budget', (value) => {
  if (value > 100000) {
    return { valid: false, reason: 'Exceeds budget' };
  }
  return { valid: true };
});

const result = registry.validate('budget', 50000);
```

### Generic Reducer Factory
```javascript
// state/reducer-factory.mjs
export function createSyncReducer(defaultState, handlers) {
  return function reducer(state = defaultState, action) {
    const handler = handlers[action.type];
    if (handler) {
      return handler(state, action);
    }
    return state;
  };
}

// Usage
const reducer = createSyncReducer(
  { connection: 'DISCONNECTED', data: null },
  {
    CONNECT: (state) => ({ ...state, connection: 'CONNECTING' }),
    CONNECTED: (state, action) => ({ ...state, connection: 'CONNECTED', data: action.data }),
    ERROR: (state, action) => ({ ...state, connection: 'ERROR', error: action.error }),
  }
);
```

### Generic SSE Client
```javascript
// client/sse-client.mjs
export class SSEClient {
  constructor(url, options = {}) {
    this.url = url;
    this.options = options;
    this.eventSource = null;
    this.reconnectDelay = options.reconnectDelay || 3000;
    this.maxReconnectDelay = options.maxReconnectDelay || 30000;
  }

  connect(handlers = {}) {
    this.eventSource = new EventSource(this.url);

    for (const [eventType, handler] of Object.entries(handlers)) {
      this.eventSource.addEventListener(eventType, (e) => {
        try {
          const data = JSON.parse(e.data);
          handler(data);
        } catch (error) {
          console.error(`Error handling ${eventType}:`, error);
        }
      });
    }

    this.eventSource.onerror = () => {
      this.reconnect(handlers);
    };
  }

  disconnect() {
    if (this.eventSource) {
      this.eventSource.close();
      this.eventSource = null;
    }
  }

  reconnect(handlers) {
    this.disconnect();
    setTimeout(() => this.connect(handlers), this.reconnectDelay);
  }
}
```

---

## Summary

| Pattern | Purpose | Applicable To |
|---------|---------|---------------|
| Delta-Based Mutations | Batch + validate changes | Any system with updates |
| Validation Hooks | Extensible validation | Form submission, APIs |
| Universe Store | Single source of truth | Distributed systems |
| Vector Clocks | Conflict detection | Multi-region databases |
| Event Sourcing | Complete audit trail | Financial systems, version control |
| React Context Sync | Remote state in React | Real-time web apps |
| SSE Subscriptions | Server-to-client push | Real-time notifications |
| Optimistic Updates | Responsive UI | Web/mobile apps |
| Subscription Registry | Efficient routing | Pub/sub systems |
| Heartbeat | Connection health | Always-on connections |
| Predicate Validation | Extensible rules | Any domain system |
| Reducer State Machine | Clear state transitions | Complex async flows |

Each pattern can be used independently or combined with others depending on your system's needs.
