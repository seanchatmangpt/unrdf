# UNRDF Reference Documentation

**Information-oriented technical specifications and API documentation**

Reference documentation provides detailed, accurate information about UNRDF's APIs, configuration options, and technical specifications.

## Core API

### Knowledge Engine
- **[KnowledgeEngine](./api/knowledge-engine.md)** - Main engine class for RDF operations
- **[TransactionManager](./api/transaction-manager.md)** - ACID transaction handling
- **[Store](./api/store.md)** - RDF quad storage interface
- **[QueryEngine](./api/query-engine.md)** - SPARQL query execution

### Knowledge Hooks
- **[KnowledgeHookManager](./api/knowledge-hook-manager.md)** - Hook registration and execution
- **[defineHook](./api/define-hook.md)** - Hook definition API
- **[HookContext](./api/hook-context.md)** - Hook execution context
- **[EffectSandbox](./api/effect-sandbox.md)** - Isolated-VM sandbox for effects

### Policy & Validation
- **[PolicyPackManager](./api/policy-pack-manager.md)** - Policy pack management
- **[ShaclValidator](./api/shacl-validator.md)** - SHACL shape validation
- **[ValidationEngine](./api/validation-engine.md)** - Custom validation rules
- **[RealTimeValidator](./api/real-time-validator.md)** - Streaming validation

---

## Browser Integration

### Storage
- **[IndexedDBStore](./api/indexeddb-store.md)** - Browser-based RDF storage
- **[BrowserShims](./api/browser-shims.md)** - Node.js API compatibility layer
- **[StorageAdapter](./api/storage-adapter.md)** - Unified storage interface

### React Hooks
- **[useKnowledgeEngine](./api/use-knowledge-engine.md)** - React integration hook
- **[useStore](./api/use-store.md)** - Store management hook
- **[useSPARQLQuery](./api/use-sparql-query.md)** - Query execution hook
- **[useKnowledgeHook](./api/use-knowledge-hook.md)** - Hook subscription

---

## Streaming & Real-time

### Change Feeds
- **[ChangeFeed](./api/change-feed.md)** - Change event streaming
- **[SubscriptionManager](./api/subscription-manager.md)** - Subscription handling
- **[ChangeType](./api/change-type.md)** - Change event types

### Stream Processing
- **[StreamProcessor](./api/stream-processor.md)** - Event stream processing
- **[WindowType](./api/window-type.md)** - Windowing strategies
- **[Aggregators](./api/aggregators.md)** - Built-in aggregation functions

---

## Distributed Systems

### Federation
- **[FederationManager](./api/federation-manager.md)** - Multi-node coordination
- **[ConsensusManager](./api/consensus-manager.md)** - Consensus protocols
- **[DistributedQueryEngine](./api/distributed-query-engine.md)** - Federated queries
- **[NetworkManager](./api/network-manager.md)** - Node communication

---

## Observability

### OpenTelemetry
- **[ObservabilityManager](./api/observability-manager.md)** - OTEL integration
- **[SpanBuilder](./api/span-builder.md)** - Custom span creation
- **[MetricsCollector](./api/metrics-collector.md)** - Performance metrics
- **[TraceExporter](./api/trace-exporter.md)** - Trace export configuration

### Monitoring
- **[PerformanceOptimizer](./api/performance-optimizer.md)** - Performance analysis
- **[HealthMonitor](./api/health-monitor.md)** - System health checks
- **[Logger](./api/logger.md)** - Logging configuration

---

## AI & Semantic

### NLP Integration
- **[NLPQueryBuilder](./api/nlp-query-builder.md)** - Natural language to SPARQL
- **[SemanticAnalyzer](./api/semantic-analyzer.md)** - Semantic similarity
- **[EmbeddingsManager](./api/embeddings-manager.md)** - Vector embeddings
- **[EntityExtractor](./api/entity-extractor.md)** - Named entity recognition

---

## Configuration

### Engine Configuration
- **[EngineConfig](./config/engine-config.md)** - Main engine options
- **[StoreConfig](./config/store-config.md)** - Storage configuration
- **[QueryConfig](./config/query-config.md)** - Query optimization
- **[HookConfig](./config/hook-config.md)** - Hook system settings

### Deployment Configuration
- **[DockerConfig](./config/docker-config.md)** - Docker setup
- **[KubernetesConfig](./config/kubernetes-config.md)** - K8s deployment
- **[TerraformConfig](./config/terraform-config.md)** - Infrastructure as Code
- **[OTELConfig](./config/otel-config.md)** - Observability settings

---

## RDF Specifications

### Supported Standards
- **[RDF 1.1](./specs/rdf-1.1.md)** - RDF data model
- **[SPARQL 1.1](./specs/sparql-1.1.md)** - Query language
- **[SHACL](./specs/shacl.md)** - Shapes validation
- **[JSON-LD](./specs/json-ld.md)** - JSON serialization
- **[Turtle](./specs/turtle.md)** - Turtle syntax
- **[N-Triples](./specs/n-triples.md)** - N-Triples format

### UNRDF Extensions
- **[Knowledge Hooks](./specs/knowledge-hooks.md)** - Hook specification
- **[Policy Packs](./specs/policy-packs.md)** - Policy specification
- **[Lockchain](./specs/lockchain.md)** - Audit trail format
- **[Streaming Protocol](./specs/streaming-protocol.md)** - Real-time updates

---

## CLI Reference

### Commands
- **[unrdf query](./cli/query.md)** - Execute SPARQL queries
- **[unrdf load](./cli/load.md)** - Load RDF data
- **[unrdf validate](./cli/validate.md)** - Validate against SHACL
- **[unrdf export](./cli/export.md)** - Export knowledge graphs

### Options
- **[Global Options](./cli/global-options.md)** - CLI configuration
- **[Environment Variables](./cli/environment-variables.md)** - ENV configuration

---

## Type Definitions

### TypeScript/JSDoc Types
- **[Core Types](./types/core-types.md)** - Fundamental types
- **[Quad Types](./types/quad-types.md)** - RDF quad interfaces
- **[Hook Types](./types/hook-types.md)** - Hook definitions
- **[Query Types](./types/query-types.md)** - Query interfaces
- **[Config Types](./types/config-types.md)** - Configuration schemas

### Zod Schemas
- **[Schema Definitions](./types/zod-schemas.md)** - Runtime validation schemas

---

## Error Reference

### Error Codes
- **[Error Catalog](./errors/error-catalog.md)** - Complete error list
- **[Transaction Errors](./errors/transaction-errors.md)** - Transaction failures
- **[Query Errors](./errors/query-errors.md)** - Query execution errors
- **[Validation Errors](./errors/validation-errors.md)** - Validation failures
- **[Hook Errors](./errors/hook-errors.md)** - Hook execution errors

---

## Performance Benchmarks

### Baseline Metrics
- **[Query Performance](./benchmarks/query-performance.md)** - SPARQL execution times
- **[Transaction Performance](./benchmarks/transaction-performance.md)** - ACID throughput
- **[Storage Performance](./benchmarks/storage-performance.md)** - I/O benchmarks
- **[Memory Usage](./benchmarks/memory-usage.md)** - Memory profiles

---

## Changelog & Migration

- **[Changelog](../../CHANGELOG.md)** - Version history
- **[v3.x → v4.0 Migration](../migration-v3-to-v4.md)** - Upgrade guide
- **[Breaking Changes](./breaking-changes.md)** - API changes

---

## Additional Resources

- **[Tutorials](../tutorials/README.md)** - Learn by building
- **[How-to Guides](../how-to/README.md)** - Solve specific problems
- **[Explanations](../explanation/README.md)** - Understand concepts
- **[API Source Code](../../src/)** - Read the implementation
