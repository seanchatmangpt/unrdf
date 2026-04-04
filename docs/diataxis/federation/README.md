# @unrdf/federation Documentation

This directory follows the [Diataxis](https://diataxis.fr/) documentation framework. Each section
serves a distinct reader need.

## Sections

| Section                                | Purpose               | Start here if…                                         |
| -------------------------------------- | --------------------- | ------------------------------------------------------ |
| [Tutorials](./tutorials/README.md)     | Step-by-step learning | You are new to the federation package                  |
| [How-To Guides](./how-to/README.md)    | Task-oriented recipes | You know what you want to accomplish                   |
| [Reference](./reference/README.md)     | API facts             | You need exact signatures or field names               |
| [Explanation](./explanation/README.md) | Concepts and design   | You want to understand why things work the way they do |

## What is @unrdf/federation?

`@unrdf/federation` distributes SPARQL queries across multiple RDF stores over HTTP. It ships
two distinct query APIs and several supporting subsystems:

1. **Simple coordinator** — `createCoordinator()` is the primary API. Register peers by URL,
   call `coordinator.query(sparql)`, get aggregated results back. Suitable for most use cases.

2. **Advanced federation engine** — `createAdvancedFederationEngine()` integrates the
   [Comunica](https://comunica.dev/) query engine for true cross-source SPARQL federation,
   including joins across endpoints. Useful when you need standards-compliant SERVICE clause
   semantics.

Supporting subsystems (v6):

- **`FederationCoordinator`** — store-level coordinator with RAFT consensus integration and
  weighted load balancing. Use when stores are long-lived services rather than ephemeral peers.
- **`DistributedQueryEngine`** — query planner with pushdown optimisation and parallel/sequential
  execution strategies.
- **`ConsensusManager`** — RAFT implementation for leader election and distributed log
  replication.
- **`DataReplicationManager`** — multi-master replication with conflict resolution and version
  vectors.

## Quick install

```bash
pnpm add @unrdf/federation
```

Node.js >= 18 is required.

## Quick example

```javascript
import { createCoordinator } from '@unrdf/federation';

const coordinator = createCoordinator({
  peers: [
    { id: 'store-a', endpoint: 'http://store-a:3000/sparql' },
    { id: 'store-b', endpoint: 'http://store-b:3000/sparql' },
  ],
});

const result = await coordinator.query('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
console.log(result.results); // deduplicated bindings from both stores
```
