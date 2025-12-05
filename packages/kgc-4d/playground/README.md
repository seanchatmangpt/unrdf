# KGC-4D Playground

Interactive demonstration of the **Shard-Based Architecture** with perfect client/server separation.

## The 80/20 Architecture

This playground implements the pragmatic pivot from theoretical hyperdimensional consistency to practical server-authoritative shards:

```
┌─────────────────────────────────────────────────────────────────┐
│                    SERVER (The Universe)                         │
│                                                                   │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐           │
│  │   Oxigraph   │  │   KGCStore   │  │ GitBackbone  │           │
│  │  (SPARQL)    │  │ (Event Log)  │  │ (Snapshots)  │           │
│  └──────────────┘  └──────────────┘  └──────────────┘           │
│                           │                                       │
│  ┌────────────────────────▼────────────────────────────────┐    │
│  │              Knowledge Hooks (μ validation)              │    │
│  └──────────────────────────────────────────────────────────┘    │
└──────────────────────────────┬──────────────────────────────────┘
                               │
                    ═══════════╪═══════════  THE TETHER (SSE)
                               │
┌──────────────────────────────┴──────────────────────────────────┐
│                    BROWSER (The Shard)                           │
│                                                                   │
│  ┌──────────────────┐  ┌──────────────────┐                     │
│  │  Transient Store │  │   React Hooks    │                     │
│  │   (In-Memory)    │  │  (useShard, etc) │                     │
│  └──────────────────┘  └──────────────────┘                     │
└─────────────────────────────────────────────────────────────────┘
```

## Quick Start

```bash
# From the monorepo root
pnpm install

# Start the playground
cd packages/kgc-4d/playground
pnpm dev

# Open http://localhost:3001
```

## Architecture

### Server (The Universe)

The server holds the complete 4D Graph with:

- **Oxigraph**: SPARQL-compliant RDF store
- **Event Log**: Immutable append-only history
- **Git Backbone**: Content-addressed snapshots
- **Knowledge Hooks**: μ(O) validation rules

### Client (The Shard)

The browser receives a **projected view** of the Universe:

- **Transient Store**: In-memory filtered quads
- **Optimistic Updates**: Immediate UI feedback
- **Vector Clock**: Causality tracking
- **Rollback**: Automatic on REJECT

### The Tether (Connection)

Real-time synchronization via Server-Sent Events:

- **Subscribe**: Request a filtered Shard
- **Push**: Receive initial Shard + real-time deltas
- **Heartbeat**: Keep-alive every 30 seconds

## API Endpoints

### `GET /api/shard`

**Check-Out**: Project a filtered view of the Universe.

Query Parameters:
- `subject`: Filter by subject IRI
- `type`: Filter by rdf:type
- `belongsTo`: Filter by relationship
- `stats=true`: Return Universe statistics

### `POST /api/delta`

**Check-In**: Submit user intent for validation and commit.

Request Body:
```json
{
  "operations": [
    {
      "type": "add",
      "subject": { "value": "http://...", "termType": "NamedNode" },
      "predicate": { "value": "http://...", "termType": "NamedNode" },
      "object": { "value": "...", "termType": "Literal" }
    }
  ],
  "client_vector_clock": { "nodeId": "...", "counters": {} }
}
```

Response:
- **ACK**: `{ status: "ACK", t_ns, vector_clock, event_id }`
- **REJECT**: `{ status: "REJECT", reason }`

### `GET /api/tether`

**SSE Stream**: Subscribe to real-time Shard updates.

Events:
- `connected`: Connection established
- `shard`: Initial Shard projection
- `delta`: Real-time update
- `heartbeat`: Keep-alive ping

## React Hooks

```jsx
import { useKGC, useShard, useEntity, useDelta } from './lib/client/hooks.mjs';

// Subscribe to Shard
const { quads, loading, refresh } = useShard({ type: 'http://kgc.io/ontology/Project' });

// Get entity properties
const { properties, update } = useEntity('http://example.org/project/alpha');

// Submit delta
const { submit, pending, lastResult } = useDelta();
await submit([{ type: 'add', subject, predicate, object }]);
```

## Validation Rules (Knowledge Hooks)

The server enforces these validation rules:

| Predicate | Rule |
|-----------|------|
| `budget` | 0 - 100,000 |
| `status` | `active`, `paused`, `completed`, `cancelled` |
| `name` | Non-empty, max 100 chars |
| `title` | Non-empty |

Try violating these rules in the Entity Editor to see REJECT responses!

## Key Concepts

### Check-Out (Projection)
1. User requests "Project Alpha"
2. Server executes temporal SPARQL query
3. Filtered Shard sent to Browser

### Manipulation (Cognition)
1. User changes budget to $50k
2. Browser updates local Shard (optimistic)
3. Delta sent to Server

### Reconciliation (The Law)
1. Server receives Delta
2. Knowledge Hooks validate
3. ACK or REJECT returned

### Alignment
- **ACK**: Update with official t_ns and Vector Clock
- **REJECT**: Rollback to Server state

## Why This Works

| Dropped (20% effort → 80% complexity) | Kept (80% value → 20% complexity) |
|---------------------------------------|-----------------------------------|
| Full Git clone in browser | Server-side Git backbone |
| Offline-first P2P sync | Active connection required |
| CRDT merge conflicts | Server is authoritative |
| Browser-side μ validation | Server validates, browser trusts |

The result: **A thin client that subscribes, renders, fires intents, and aligns.**

## License

MIT
