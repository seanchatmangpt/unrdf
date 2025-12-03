# UNRDF How-To Guides

**Task-oriented recipes. Start with [ESSENTIAL], skip [ADVANCED] until needed.**

---

## [ESSENTIAL] Core Operations (Start Here)

These cover 80% of use cases:

### Parsing & Serialization
- **[ESSENTIAL]** [How to Parse RDF Formats](./parse-rdf-formats.md) - Turtle, JSON-LD, N-Triples
- **[ESSENTIAL]** [How to Serialize RDF](./serialize-rdf.md) - Back to Turtle/JSON-LD

### Querying
- **[ESSENTIAL]** [How to Execute SPARQL Queries](./execute-sparql-queries.md) - SELECT, CONSTRUCT, ASK
- **[ESSENTIAL]** [How to Use SPARQL Property Paths](./sparql-property-paths.md) - Path expressions

### Validation
- **[ESSENTIAL]** [How to Define SHACL Constraints](./define-shacl-constraints.md) - Shape validation
- **[ESSENTIAL]** [How to Handle Validation Errors](./handle-validation-errors.md) - Error handling

---

## STOP HERE if you just need basic RDF operations.

The guides below are for specialized requirements.

---

## [ADVANCED] Knowledge Hooks

**When you need it:** Autonomous behaviors on every data change.
**Skip if:** You can validate/transform in your code.

- [ADVANCED] [How to Create Validation Hooks](./create-validation-hooks.md)
- [ADVANCED] [How to Build Transformation Hooks](./build-transformation-hooks.md)
- [ADVANCED] [How to Chain Multiple Hooks](./chain-multiple-hooks.md)
- [ADVANCED] [How to Debug Hook Execution](./debug-hook-execution.md)
- [ADVANCED] [How to Test Knowledge Hooks](./test-knowledge-hooks.md)

---

## [ADVANCED] Transactions

**When you need it:** Hook-driven atomic operations.
**Skip if:** Basic add/remove is sufficient.

- [ADVANCED] [How to Handle Transactions](./handle-transactions.md) - TransactionManager lifecycle
- [ADVANCED] [How to Use Hook Context](./use-hook-context.md)
- [ADVANCED] [How to Handle Hook Errors](./handle-hook-errors.md)

---

## [ADVANCED] Compliance & Audit

**When you need it:** GDPR, SOC2, HIPAA compliance.
**Skip if:** No regulatory requirements.

- [ADVANCED] [How to Implement Audit Trails](./implement-audit-trails.md) - LockchainWriter
- [ADVANCED] [How to Create Knowledge Hooks](./create-knowledge-hooks.md) - Autonomic functions

---

## [ADVANCED] Performance Optimization

**When you need it:** Queries taking >100ms.
**Skip if:** Performance is acceptable.

- [ADVANCED] [How to Optimize Query Performance](./optimize-query-performance.md) - Singleton, caching
- [ADVANCED] [How to Cache Query Results](./cache-query-results.md)
- [ADVANCED] [How to Optimize Memory Usage](./optimize-memory-usage.md)

---

## [ADVANCED] Browser & React

**When you need it:** Client-side applications.
**Skip if:** Node.js only.

### Browser Setup
- [ADVANCED] [How to Configure IndexedDB Storage](./configure-indexeddb-storage.md)
- [ADVANCED] [How to Implement Offline Support](./implement-offline-support.md)

### React Integration
- [ADVANCED] [How to Use React Hooks with UNRDF](./use-react-hooks.md)
- [ADVANCED] [How to Use Knowledge Hooks in React](./use-hooks-in-react.md)
- [ADVANCED] [How to Build Reactive Components](./build-reactive-components.md)

---

## [ADVANCED] Policy & Governance

**When you need it:** Enterprise governance workflows.
**Skip if:** Basic SHACL is enough.

- [ADVANCED] [How to Create Custom Validators](./create-custom-validators.md)
- [ADVANCED] [How to Implement Access Control](./implement-access-control.md)
- [ADVANCED] [How to Compose Policy Packs](./compose-policy-packs.md)

---

## [ADVANCED] Streaming & Real-time

**When you need it:** Real-time data pipelines.
**Skip if:** Batch processing is sufficient.

- [ADVANCED] [How to Set Up Change Feeds](./setup-change-feeds.md)
- [ADVANCED] [How to Implement Windowing](./implement-windowing.md)
- [ADVANCED] [How to Subscribe to Data Changes](./subscribe-to-changes.md)

---

## [ADVANCED] Distributed Systems

**When you need it:** Multi-node deployments.
**Skip if:** Single store is sufficient.

- [ADVANCED] [How to Connect Multiple Nodes](./connect-multiple-nodes.md)
- [ADVANCED] [How to Query Distributed Data](./query-distributed-data.md)
- [ADVANCED] [How to Handle Network Partitions](./handle-network-partitions.md)

---

## [ADVANCED] Deployment & Production

**When you need it:** Production infrastructure.
**Skip if:** Development only.

- [ADVANCED] [How to Deploy with Docker](./deploy-with-docker.md)
- [ADVANCED] [How to Set Up Kubernetes](./setup-kubernetes.md)
- [ADVANCED] [How to Configure OTEL Exporters](./configure-otel-exporters.md)
- [ADVANCED] [How to Monitor Production Health](./monitor-production-health.md)

---

## Quick Reference

| I want to... | Guide |
|--------------|-------|
| Parse RDF | [ESSENTIAL] Parse RDF Formats |
| Query data | [ESSENTIAL] Execute SPARQL Queries |
| Validate | [ESSENTIAL] Define SHACL Constraints |
| Auto-validate on change | [ADVANCED] Create Validation Hooks |
| Audit trail | [ADVANCED] Implement Audit Trails |
| Optimize performance | [ADVANCED] Optimize Query Performance |
| Use React | [ADVANCED] Use React Hooks |

---

## Need Something Else?

- [Tutorials](../tutorials/README.md) - Learning paths
- [Reference](../reference/README.md) - API documentation
- [Which Features?](../WHICH-FEATURES.md) - Decision trees
