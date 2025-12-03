# @unrdf/federation

**Peer Discovery and Distributed Query Execution**

Federate RDF graphs across multiple peers. Discover peers, route queries, and execute distributed operations.

## Installation

```bash
pnpm add @unrdf/federation
```

## Quick Start

```javascript
import { connectToPeer, remoteQuery } from '@unrdf/federation'

// Discover and connect to peers
const peers = await discoverPeers({ capability: 'sparql' })

// Execute query on remote peer
const results = await remoteQuery(
  peers[0],
  'SELECT ?s ?p ?o WHERE { ?s ?p ?o }'
)
```

## Features

- ✅ Peer discovery (DNS-SD, mDNS)
- ✅ Remote SPARQL execution
- ✅ Query routing and optimization
- ✅ Connection pooling
- ✅ Peer health monitoring
- ✅ Fallback peer selection

## Use Cases

- **Multi-graph queries**: Query across multiple RDF sources
- **Distributed systems**: Coordinate RDF operations
- **Data federation**: Combine datasets from multiple sources
- **High availability**: Route to healthy peers

## Documentation

- **[API Reference](./docs/API.md)** - Complete API documentation
- **[User Guide](./docs/GUIDE.md)** - Federation patterns
- **[Examples](./examples/)** - Code examples
- **[Contributing](./docs/CONTRIBUTING.md)** - How to contribute

## Depends On

- `@unrdf/core` - RDF substrate
- `@unrdf/hooks` - Policy enforcement

## VOC Usage

- VOC-1: Knowledge Agent (discover agent peers)
- VOC-2: Sync Agent (peer discovery and sync)
- VOC-5: Data Engineer (federate data sources)

## License

MIT
