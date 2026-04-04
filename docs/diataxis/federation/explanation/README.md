# Explanation — @unrdf/federation

Explanation documents the concepts and design decisions behind the federation package. Read
these when you want to understand _why_ things work the way they do, not just _how_ to use them.

## Index

| Document                                                                 | Topic                                                                                         |
| ------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------- |
| [01-distributed-query-execution.md](./01-distributed-query-execution.md) | How federated queries fan out, aggregate, and deduplicate across peers                        |
| [02-source-selection-algorithm.md](./02-source-selection-algorithm.md)   | How routing strategies, health tracking, and load balancing determine which peers are queried |

## Background

`@unrdf/federation` solves a fundamental problem in knowledge graph systems: data lives in
multiple stores and queries should span all of them transparently. The package addresses this
at two levels:

1. **HTTP peer federation** (`createCoordinator`) — the pragmatic, operational layer. It fans
   out SPARQL over HTTP, aggregates bindings, and tracks peer health. This layer makes no
   assumptions about the internal structure of the stores.

2. **Comunica integration** (`createAdvancedFederationEngine`) — the standards layer. It uses
   the [Comunica](https://comunica.dev/) query engine, which understands SPARQL 1.1 federation
   (`SERVICE` clauses) and can push down filters and joins to individual sources. This is the
   right tool when the correctness of cross-source joins matters more than operational simplicity.

The v6 subsystems (`FederationCoordinator`, `ConsensusManager`, `DataReplicationManager`) sit
at a higher level still: they are designed for long-running federations that require distributed
consensus, leader election, and data replication rather than just query fanout. They are more
complex and are covered in the explanation documents.
