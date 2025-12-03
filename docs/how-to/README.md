# UNRDF How-to Guides

**Task-oriented recipes for solving specific problems**

How-to guides are practical, step-by-step instructions for accomplishing specific tasks. They assume you have basic knowledge from the tutorials.

## Core Operations

### Data Management
- [How to Import Large RDF Datasets](./import-large-datasets.md)
- [How to Export Knowledge Graphs](./export-knowledge-graphs.md)
- [How to Batch Insert Triples Efficiently](./batch-insert-triples.md)
- [How to Deduplicate RDF Data](./deduplicate-rdf-data.md)
- [How to Merge Multiple Knowledge Graphs](./merge-knowledge-graphs.md)

### Querying
- [How to Optimize SPARQL Query Performance](./optimize-sparql-queries.md)
- [How to Write Complex JOIN Queries](./complex-join-queries.md)
- [How to Implement Full-Text Search](./full-text-search.md)
- [How to Query Across Federated Sources](./query-federated-sources.md)
- [How to Use SPARQL Property Paths](./sparql-property-paths.md)

---

## Knowledge Hooks

### Hook Development
- [How to Create Validation Hooks](./create-validation-hooks.md)
- [How to Build Transformation Hooks](./build-transformation-hooks.md)
- [How to Implement Audit Trail Hooks](./implement-audit-trail-hooks.md)
- [How to Chain Multiple Hooks](./chain-multiple-hooks.md)
- [How to Debug Hook Execution](./debug-hook-execution.md)

### Hook Patterns
- [How to Implement Hook Conditions](./implement-hook-conditions.md)
- [How to Use Hook Context](./use-hook-context.md)
- [How to Handle Hook Errors](./handle-hook-errors.md)
- [How to Test Knowledge Hooks](./test-knowledge-hooks.md)

---

## Browser & Client-Side

### Browser Setup
- [How to Configure IndexedDB Storage](./configure-indexeddb-storage.md)
- [How to Enable Browser Caching](./enable-browser-caching.md)
- [How to Handle Cross-Origin Requests](./handle-cors.md)
- [How to Implement Offline Support](./implement-offline-support.md)

### React Integration
- [How to Use React Hooks with UNRDF](./use-react-hooks.md)
- [How to Build Reactive Components](./build-reactive-components.md)
- [How to Implement Optimistic Updates](./implement-optimistic-updates.md)
- [How to Handle Loading States](./handle-loading-states.md)

---

## Policy & Validation

### Policy Management
- [How to Define SHACL Constraints](./define-shacl-constraints.md)
- [How to Create Custom Validators](./create-custom-validators.md)
- [How to Implement Access Control](./implement-access-control.md)
- [How to Compose Policy Packs](./compose-policy-packs.md)

### Validation Strategies
- [How to Validate on Insert](./validate-on-insert.md)
- [How to Validate Batch Operations](./validate-batch-operations.md)
- [How to Handle Validation Errors](./handle-validation-errors.md)

---

## Streaming & Real-time

### Stream Processing
- [How to Set Up Change Feeds](./setup-change-feeds.md)
- [How to Implement Windowing](./implement-windowing.md)
- [How to Create Stream Aggregators](./create-stream-aggregators.md)
- [How to Process Event Streams](./process-event-streams.md)

### Real-time Updates
- [How to Subscribe to Data Changes](./subscribe-to-changes.md)
- [How to Broadcast Updates](./broadcast-updates.md)
- [How to Implement WebSocket Sync](./implement-websocket-sync.md)

---

## Distributed Systems

### Federation
- [How to Connect Multiple Nodes](./connect-multiple-nodes.md)
- [How to Implement Consensus Protocols](./implement-consensus.md)
- [How to Handle Network Partitions](./handle-network-partitions.md)
- [How to Query Distributed Data](./query-distributed-data.md)

### Scaling
- [How to Shard Your Knowledge Graph](./shard-knowledge-graph.md)
- [How to Replicate Data Across Nodes](./replicate-data.md)
- [How to Load Balance Queries](./load-balance-queries.md)

---

## Observability & Monitoring

### OpenTelemetry
- [How to Configure OTEL Exporters](./configure-otel-exporters.md)
- [How to Create Custom Spans](./create-custom-spans.md)
- [How to Track Performance Metrics](./track-performance-metrics.md)
- [How to Implement Distributed Tracing](./implement-distributed-tracing.md)

### Debugging
- [How to Enable Debug Logging](./enable-debug-logging.md)
- [How to Profile Query Performance](./profile-query-performance.md)
- [How to Trace Transaction Execution](./trace-transaction-execution.md)

---

## Deployment & Production

### Infrastructure
- [How to Deploy with Docker](./deploy-with-docker.md)
- [How to Set Up Kubernetes](./setup-kubernetes.md)
- [How to Configure Terraform](./configure-terraform.md)
- [How to Use Testcontainers](./use-testcontainers.md)

### Production Operations
- [How to Backup Knowledge Graphs](./backup-knowledge-graphs.md)
- [How to Restore from Backups](./restore-from-backups.md)
- [How to Monitor Production Health](./monitor-production-health.md)
- [How to Handle Production Incidents](./handle-production-incidents.md)

---

## Integration

### AI & ML
- [How to Integrate with NLP Services](./integrate-nlp-services.md)
- [How to Generate Embeddings](./generate-embeddings.md)
- [How to Implement Semantic Search](./implement-semantic-search.md)

### External Systems
- [How to Connect to GraphQL APIs](./connect-to-graphql.md)
- [How to Sync with SQL Databases](./sync-with-sql.md)
- [How to Integrate with Message Queues](./integrate-message-queues.md)

---

## Performance Optimization

- [How to Optimize Query Performance](./optimize-query-performance.md) - QueryEngine singleton, LRU caching, delta-aware optimization
- [How to Optimize Memory Usage](./optimize-memory-usage.md)
- [How to Cache Query Results](./cache-query-results.md)
- [How to Tune Transaction Performance](./tune-transaction-performance.md)
- [How to Reduce Bundle Size](./reduce-bundle-size.md)

---

## Compliance & Audit Trails

- [How to Implement Audit Trails](./implement-audit-trails.md) - GDPR, SOC2, HIPAA compliance with LockchainWriter
- [How to Create Knowledge Hooks](./create-knowledge-hooks.md) - Autonomic event-driven functions
- [How to Use Knowledge Hooks in React](./use-hooks-in-react.md) - React integration patterns
- [How to Handle Transactions](./handle-transactions.md) - TransactionManager lifecycle

---

## Need Something Else?

Can't find what you're looking for?
- Check [Tutorials](../tutorials/README.md) for learning basics
- See [Reference](../reference/README.md) for API documentation
- Read [Explanations](../explanation/README.md) for conceptual understanding
- Ask in [GitHub Discussions](https://github.com/unrdf/unrdf/discussions)
