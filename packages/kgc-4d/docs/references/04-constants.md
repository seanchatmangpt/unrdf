# Constants Reference

Constants used throughout KGC 4D for consistency and to prevent typos.

## Import

```javascript
import {
  GRAPHS,
  EVENT_TYPES,
  PREDICATES,
} from '@unrdf/kgc-4d';
```

## GRAPHS

Named graph URIs as RDF NamedNodes.

```javascript
import { GRAPHS } from '@unrdf/kgc-4d';

GRAPHS.UNIVERSE   // <kgc:Universe>
GRAPHS.EVENT_LOG  // <kgc:EventLog>
GRAPHS.SYSTEM     // <kgc:System>
```

### GRAPHS.UNIVERSE

**URI:** `<kgc:Universe>`

**Purpose:** Current observable state

**Usage in SPARQL:**
```sparql
GRAPH <kgc:Universe> {
  ?person ex:name "Alice" .
}
```

**Usage in code:**
```javascript
const quads = store.querySync(`
  SELECT * WHERE {
    GRAPH <kgc:Universe> { ?s ?p ?o }
  }
`);

// Or using constant:
const universeName = GRAPHS.UNIVERSE.value; // 'kgc:Universe'
```

**Characteristics:**
- Mutable (via events)
- Contains active triples only
- Queryable
- Single version (no history)

### GRAPHS.EVENT_LOG

**URI:** `<kgc:EventLog>`

**Purpose:** Immutable append-only history

**Usage in SPARQL:**
```sparql
GRAPH <kgc:EventLog> {
  ?event kgc:type kgc:CREATE ;
         kgc:T_NS ?timestamp .
}
```

**Characteristics:**
- Append-only (immutable)
- Queryable
- Contains all historical events
- Never deleted or modified

### GRAPHS.SYSTEM

**URI:** `<kgc:System>`

**Purpose:** System metadata and configuration

**Stores:**
- Current vector clock
- Last snapshot reference
- Configuration metadata

**Characteristics:**
- Mutable
- Internal use primarily
- Not usually queried by users
- Contains optimization data

---

## EVENT_TYPES

Event type constants for `eventDesc.type`.

```javascript
import { EVENT_TYPES } from '@unrdf/kgc-4d';

EVENT_TYPES.CREATE            // 'CREATE'
EVENT_TYPES.UPDATE            // 'UPDATE'
EVENT_TYPES.DELETE            // 'DELETE'
EVENT_TYPES.SNAPSHOT          // 'SNAPSHOT'
EVENT_TYPES.HOOK_EXECUTION    // 'HOOK_EXECUTION'
```

### EVENT_TYPES.CREATE

**Value:** `'CREATE'`

**Usage:** Creating new entities or initial data

```javascript
await store.appendEvent(
  {
    type: EVENT_TYPES.CREATE,
    payload: { source: 'user-import' }
  },
  [
    { type: 'add', subject: alice, predicate: rdfType, object: person },
    { type: 'add', subject: alice, predicate: name, object: literal('Alice') },
  ]
);
```

**Semantics:** Something new came into existence

### EVENT_TYPES.UPDATE

**Value:** `'UPDATE'`

**Usage:** Modifying existing values

```javascript
await store.appendEvent(
  {
    type: EVENT_TYPES.UPDATE,
    payload: { reason: 'user-edit', field: 'name' }
  },
  [
    { type: 'delete', subject: alice, predicate: name, object: literal('Alice') },
    { type: 'add', subject: alice, predicate: name, object: literal('Alicia') },
  ]
);
```

**Semantics:** An existing fact changed

### EVENT_TYPES.DELETE

**Value:** `'DELETE'`

**Usage:** Removing entities or facts

```javascript
await store.appendEvent(
  {
    type: EVENT_TYPES.DELETE,
    payload: { reason: 'user-requested' }
  },
  [
    { type: 'delete', subject: alice, predicate: rdfType, object: person },
    { type: 'delete', subject: alice, predicate: name, object: literal('Alice') },
  ]
);
```

**Semantics:** Something ceased to exist or became false

### EVENT_TYPES.SNAPSHOT

**Value:** `'SNAPSHOT'`

**Usage:** System event when freezing universe

```javascript
// Automatically recorded by freezeUniverse()
// Usually don't create manually, but if you do:

await store.appendEvent(
  {
    type: EVENT_TYPES.SNAPSHOT,
    payload: {
      snapshotId: '...',
      gitRef: '...',
      hash: '...'
    }
  },
  [] // Snapshots don't include mutations
);
```

**Semantics:** Point-in-time capture for optimization

### EVENT_TYPES.HOOK_EXECUTION

**Value:** `'HOOK_EXECUTION'`

**Usage:** Knowledge hook execution (integration hooks)

```javascript
// When a knowledge hook executes and modifies state:

await store.appendEvent(
  {
    type: EVENT_TYPES.HOOK_EXECUTION,
    payload: {
      hookName: 'validation-hook',
      result: 'passed'
    }
  },
  [/* mutations from hook */]
);
```

**Semantics:** Automated system action

---

## PREDICATES

RDF predicate URIs for system properties.

```javascript
import { PREDICATES } from '@unrdf/kgc-4d';

PREDICATES.T_NS              // Timestamp predicate
PREDICATES.TYPE              // Event type predicate
PREDICATES.GIT_REF           // Git reference predicate
PREDICATES.PAYLOAD           // Event payload predicate
PREDICATES.VECTOR_CLOCK      // Vector clock predicate
```

### PREDICATES.T_NS

**URI:** `<kgc:T_NS>`

**Purpose:** Event timestamp in nanoseconds

**Usage in events:**
```sparql
PREFIX kgc: <kgc:>
SELECT ?timestamp
WHERE {
  GRAPH <kgc:EventLog> {
    ?event kgc:T_NS ?timestamp .
  }
}
```

**Value type:** RDF Literal (integer/BigInt)

**Range:** BigInt nanoseconds (0 to 2^63-1)

**Example:**
```javascript
const eventTimestamp = BigInt('1701734400000000000');
// This represents: 2023-12-05T00:00:00.000000000Z
```

### PREDICATES.TYPE

**URI:** `<kgc:type>`

**Purpose:** Event type (CREATE, UPDATE, DELETE, etc.)

**Usage in events:**
```sparql
PREFIX kgc: <kgc:>
SELECT ?type
WHERE {
  GRAPH <kgc:EventLog> {
    ?event kgc:type ?type .
  }
}
```

**Value type:** RDF Literal (string)

**Valid values:**
- `'CREATE'`
- `'UPDATE'`
- `'DELETE'`
- `'SNAPSHOT'`
- `'HOOK_EXECUTION'`

### PREDICATES.GIT_REF

**URI:** `<kgc:gitRef>`

**Purpose:** Git commit reference for snapshot

**Usage in system graph:**
```sparql
PREFIX kgc: <kgc:>
SELECT ?gitRef
WHERE {
  GRAPH <kgc:System> {
    ?snapshot kgc:gitRef ?gitRef .
  }
}
```

**Value type:** RDF Literal (string)

**Example value:** `'abc123def456...'` (Git commit hash)

### PREDICATES.PAYLOAD

**URI:** `<kgc:payload>`

**Purpose:** Event metadata as JSON string

**Usage in events:**
```sparql
PREFIX kgc: <kgc:>
SELECT ?payload
WHERE {
  GRAPH <kgc:EventLog> {
    ?event kgc:payload ?payload .
  }
}
ORDER BY ?event
```

**Value type:** RDF Literal (JSON string)

**Example:**
```javascript
// Payload stored as string:
'{"userId":"user-123","source":"api","reason":"bulk-import"}'

// Parse in SPARQL/code:
const payload = JSON.parse(payloadString);
console.log(payload.userId); // 'user-123'
```

### PREDICATES.VECTOR_CLOCK

**URI:** `<kgc:vectorClock>`

**Purpose:** Logical clock for causality

**Usage in system graph:**
```sparql
PREFIX kgc: <kgc:>
SELECT ?vc
WHERE {
  GRAPH <kgc:System> {
    <kgc:Clock> kgc:vectorClock ?vc .
  }
}
```

**Value type:** RDF Literal (JSON string)

**Example:**
```javascript
// Vector clock stored as JSON:
'{"node1":5,"node2":3,"node3":7}'

// Parse:
const vc = JSON.parse(vcString);
console.log(vc.node1); // 5
```

---

## Namespace Prefixes

Recommended SPARQL prefixes:

```sparql
PREFIX kgc: <kgc:>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
```

### KGC Namespace

**Prefix:** `kgc:`
**URI:** `<kgc:>`

Used for all KGC 4D system properties:
- `kgc:Universe`
- `kgc:EventLog`
- `kgc:System`
- `kgc:T_NS`
- `kgc:type`
- `kgc:CREATE`
- `kgc:UPDATE`
- `kgc:DELETE`
- etc.

### Standard RDF Namespaces

For application data, use standard RDF namespaces:

```javascript
import { dataFactory } from '@unrdf/oxigraph';

// RDF type
const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

// RDFS label
const rdfsLabel = dataFactory.namedNode('http://www.w3.org/2000/01/rdf-schema#label');

// Custom namespace
const exPerson = dataFactory.namedNode('http://example.org/Person');
```

---

## String vs. Constant Usage

### Prefer Constants

```javascript
// ✓ GOOD: Use constants for KGC system properties
await store.appendEvent(
  { type: EVENT_TYPES.CREATE, payload: {...} },
  mutations
);

// ✓ GOOD: Constants prevent typos
GRAPHS.UNIVERSE    // No typos possible
GRAPHS.EVENT_LOG

// ✗ BAD: String literals can have typos
await store.appendEvent(
  { type: 'CREAT', payload: {...} },  // ← Typo!
  mutations
);
```

### String Literals for Custom Data

```javascript
// ✓ OK: Use strings for custom application data
const exName = dataFactory.namedNode('http://example.org/name');
const exAge = dataFactory.namedNode('http://example.org/age');

// Strings for literal values
const nameValue = dataFactory.literal('Alice');
const ageValue = dataFactory.literal('30');
```

---

## Constants in Runtime

All constants are exported from the main module:

```javascript
import {
  GRAPHS,
  EVENT_TYPES,
  PREDICATES,
} from '@unrdf/kgc-4d';

// Constants are frozen objects (immutable)
Object.isFrozen(GRAPHS);        // true
Object.isFrozen(EVENT_TYPES);   // true
Object.isFrozen(PREDICATES);    // true

// Trying to modify throws error in strict mode
GRAPHS.UNIVERSE = 'something';  // TypeError (in strict mode)
```

---

## Complete Constants Reference

```javascript
// GRAPHS
{
  UNIVERSE: NamedNode('kgc:Universe'),
  EVENT_LOG: NamedNode('kgc:EventLog'),
  SYSTEM: NamedNode('kgc:System'),
}

// EVENT_TYPES
{
  CREATE: 'CREATE',
  UPDATE: 'UPDATE',
  DELETE: 'DELETE',
  SNAPSHOT: 'SNAPSHOT',
  HOOK_EXECUTION: 'HOOK_EXECUTION',
}

// PREDICATES
{
  T_NS: NamedNode('kgc:T_NS'),
  TYPE: NamedNode('kgc:type'),
  GIT_REF: NamedNode('kgc:gitRef'),
  PAYLOAD: NamedNode('kgc:payload'),
  VECTOR_CLOCK: NamedNode('kgc:vectorClock'),
}
```

---

## Usage Examples

### Query Event History

```javascript
import { GRAPHS, EVENT_TYPES, PREDICATES } from '@unrdf/kgc-4d';

const query = `
  PREFIX kgc: <kgc:>
  SELECT ?eventId ?type ?timestamp
  WHERE {
    GRAPH <kgc:EventLog> {
      ?eventId kgc:type ?type ;
               kgc:T_NS ?timestamp .
    }
  }
  ORDER BY DESC(?timestamp)
  LIMIT 10
`;

const results = store.querySync(query);
```

### Filter by Event Type

```javascript
const query = `
  PREFIX kgc: <kgc:>
  SELECT ?eventId
  WHERE {
    GRAPH <kgc:EventLog> {
      ?eventId kgc:type kgc:UPDATE ;
               kgc:T_NS ?timestamp .
    }
  }
  ORDER BY DESC(?timestamp)
`;

const updates = store.querySync(query);
```

### Access Constants Programmatically

```javascript
// Get graph URI as string
const universeName = GRAPHS.UNIVERSE.value;  // 'kgc:Universe'

// Check event type
if (receipt.type === EVENT_TYPES.CREATE) {
  console.log('Entity was created');
}

// Use predicate in code
const payloadPredicate = PREDICATES.PAYLOAD;
```
