# UNRDF Server Example

Node.js backend demonstrating all UNRDF server-side packages working together.

## Architecture

```
Server Application
├── HTTP API (Node.js)
├── WebSocket Server (Real-time updates)
├── RDF Store (N3.js)
├── Knowledge Hooks (Validation & Transformation)
├── Streaming Processor (Change tracking)
└── Federation Coordinator (Distributed queries)
```

## Packages Demonstrated

### @unrdf/core
- RDF triple storage
- SPARQL query execution
- Graph operations

### @unrdf/hooks
- Validation hooks (email format)
- Transformation hooks (timestamp enrichment)
- Policy enforcement

### @unrdf/federation
- Peer discovery
- Distributed query coordination
- Cross-node synchronization

### @unrdf/streaming
- Change feed tracking
- Real-time notifications
- WebSocket integration

### @unrdf/cli
- Graph management utilities
- Command-line operations

### @unrdf/knowledge-engine
- Inference capabilities
- Reasoning support

### @unrdf/dark-matter
- Query optimization
- Performance enhancements

## API Endpoints

### GET /api/quads
Get all RDF triples in the store.

**Response:**
```json
{
  "count": 9,
  "quads": [
    {
      "subject": "http://example.org/Person1",
      "predicate": "http://xmlns.com/foaf/0.1/name",
      "object": "Alice Smith",
      "objectType": "Literal"
    }
  ]
}
```

### GET /api/query?q={sparql}
Execute a SPARQL query.

**Parameters:**
- `q`: SPARQL query string

**Response:**
```json
{
  "query": "SELECT * WHERE { ?s ?p ?o }",
  "results": [...]
}
```

### POST /api/quads
Add a new RDF triple.

**Request:**
```json
{
  "subject": "http://example.org/Person4",
  "predicate": "http://xmlns.com/foaf/0.1/name",
  "object": "David Miller"
}
```

**Response:**
```json
{
  "success": true
}
```

### GET /api/stats
Get store statistics.

**Response:**
```json
{
  "totalTriples": 9,
  "uniqueSubjects": 3,
  "uniquePredicates": 4,
  "hooksRegistered": 2,
  "wsConnections": 1
}
```

## WebSocket Protocol

Connect to `ws://localhost:3001` for real-time updates.

### Message Types

**Initial Data:**
```json
{
  "type": "initial",
  "quads": [...]
}
```

**Change Notification:**
```json
{
  "type": "change",
  "operation": "add",
  "quad": {
    "subject": "...",
    "predicate": "...",
    "object": "..."
  }
}
```

## Knowledge Hooks

### validateEmail Hook
Validates email format before adding triples.

**Trigger:** `before:add`

**Validation:**
- Must start with `mailto:`
- Must contain `@` symbol

### addTimestamp Hook
Adds timestamp to newly created triples.

**Trigger:** `after:add`

**Behavior:**
- Adds `http://example.org/addedAt` predicate
- Value: ISO 8601 timestamp

## Running

```bash
# Install dependencies
pnpm install

# Start development server
pnpm dev

# Start production server
pnpm start
```

## Environment Variables

- `PORT`: HTTP server port (default: 3000)
- `WS_PORT`: WebSocket server port (default: 3001)

## Sample RDF Data

The server starts with sample FOAF (Friend of a Friend) data:

```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:Person1 a foaf:Person ;
  foaf:name "Alice Smith" ;
  foaf:knows ex:Person2 ;
  foaf:mbox <mailto:alice@example.org> .

ex:Person2 a foaf:Person ;
  foaf:name "Bob Johnson" ;
  foaf:knows ex:Person1 ;
  foaf:mbox <mailto:bob@example.org> .

ex:Person3 a foaf:Person ;
  foaf:name "Carol White" ;
  foaf:knows ex:Person1 ;
  foaf:mbox <mailto:carol@example.org> .
```

## Testing

```bash
# Run tests
pnpm test

# Test API endpoints
curl http://localhost:3000/api/quads
curl http://localhost:3000/api/stats
curl -X POST http://localhost:3000/api/quads \
  -H "Content-Type: application/json" \
  -d '{"subject":"http://example.org/Person4","predicate":"http://xmlns.com/foaf/0.1/name","object":"David"}'
```
