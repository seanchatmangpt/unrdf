# UNRDF v6.0 Receipt & Audit EPICs

**Domain**: Receipts, Audit Trails & Cryptographic Proofs  
**Version**: 6.0.0  
**Date**: 2025-12-28  
**Status**: Planning Phase

---

## Overview

This document defines the epic-level feature sets for UNRDF v6's receipt and audit trail system. The system provides cryptographic proof of all governance decisions, mutations, and state transitions with sub-millisecond performance.

**Architecture Summary**:
- 9-field receipt model (ID, type, timestamps, hashes, payload)
- BLAKE3 hash chains (linear + Merkle batching)
- 4 specialized receipt types (execution, allocation, compile, verification)
- Performance targets: <1ms creation, <0.5ms verification
- 100% tamper detection via hash verification

---

## EPIC-RCPT-001: Core Receipt Infrastructure

**Goal**: Implement the foundational receipt model, BLAKE3 hashing, and linear chain architecture.

**Value**: Provides the cryptographic foundation for all audit trails, enabling tamper-evident records of every system operation with provable integrity.

**Scope**: Base receipt schema, hash computation (BLAKE3), chain linking, Zod validation schemas, receipt generation API.

### Acceptance Criteria
- [ ] Base receipt schema implemented with all 9 required fields (id, receiptType, t_ns, timestamp_iso, previousHash, payloadHash, receiptHash, payload, signature)
- [ ] BLAKE3 hash function integrated and deterministic across all receipt operations
- [ ] Linear chain linking enforced: `receipt[N].previousHash === receipt[N-1].receiptHash`
- [ ] Genesis receipt creation (previousHash = null) validated
- [ ] Temporal ordering enforced: `receipt[N].t_ns > receipt[N-1].t_ns`
- [ ] Zod schemas defined for all receipt types with runtime validation
- [ ] Receipt creation performance: P95 < 1ms (target: 0.02ms based on current measurements)
- [ ] Receipt verification performance: P95 < 0.5ms
- [ ] Branded types implemented to prevent ID confusion (ReceiptId, UniverseId)
- [ ] Object.freeze() applied to all receipts after creation (immutability guarantee)
- [ ] 100% test coverage for receipt creation, hashing, and chaining
- [ ] OTEL spans instrumented: 'receipt.create', 'receipt.verify'

### Key Stories
1. **Implement Base Receipt Schema** - Define Zod schema for 9 core fields, validate at runtime
2. **Integrate BLAKE3 Hashing** - Add BLAKE3 library, implement deterministic hash computation for payloads and chains
3. **Build Linear Chain Logic** - Implement previousHash linking, genesis detection, temporal validation
4. **Add Receipt Immutability** - Object.freeze() all receipts, prevent post-creation mutation
5. **Create Receipt Generation API** - `createReceipt(type, payload)` with automatic hashing and chaining
6. **Build Receipt Verification API** - `verifyReceipt(receipt)` recomputes hashes and validates integrity
7. **Instrument OTEL Spans** - Add tracing for creation/verification with P95 latency metrics
8. **Write Core Receipt Tests** - Unit tests for all hash computation, chain linking, validation edge cases

### Dependencies
- **Blocked by**: None (foundational epic)
- **Blocks**: RCPT-002 (Execution Receipts), RCPT-003 (Mutation Receipts), RCPT-004 (Freeze Receipts), RCPT-005 (Merkle Batching), RCPT-006 (Audit API), RCPT-007 (Tamper Detection)

### Estimated Effort
- **T-shirt size**: L
- **Weeks**: 2-3
- **Rationale**: Core infrastructure requires careful design for cryptographic correctness, performance optimization, and extensive testing. BLAKE3 integration and chain logic are straightforward but critical path.

---

## EPIC-RCPT-002: Execution Receipts (Workflow Tasks)

**Goal**: Implement execution receipts for YAWL workflow task lifecycle events (started, completed, failed, etc.).

**Value**: Provides auditable records of all workflow execution decisions, enabling reconstruction of decision chains and compliance audits.

**Scope**: Execution receipt schema, YAWL integration hooks, task event capture (TASK_STARTED, TASK_COMPLETED, TASK_FAILED), decision justification storage.

### Acceptance Criteria
- [ ] Execution receipt schema defined with required fields: eventType, caseId, taskId, decision, justification, context
- [ ] YAWL task lifecycle events automatically generate execution receipts
- [ ] Event types supported: TASK_STARTED, TASK_COMPLETED, TASK_FAILED, TASK_CANCELLED, TASK_SUSPENDED
- [ ] Decision field captures actual outcome: 'approve', 'reject', 'escalate', etc.
- [ ] Justification field stores reasoning with structured format (JSON)
- [ ] Context field captures task inputs/outputs for audit reconstruction
- [ ] Receipt generated synchronously with task state transition (no async delays)
- [ ] Integration with @unrdf/workflows package via hooks
- [ ] Performance: Execution receipt creation adds <0.5ms overhead to task execution
- [ ] Case-level audit trail API: `getExecutionTrail(caseId)` returns all receipts for a workflow case
- [ ] Receipts stored in RDF event log with SPARQL queryability
- [ ] 100% test coverage for all task lifecycle transitions

### Key Stories
1. **Define Execution Receipt Schema** - Extend base receipt with execution-specific fields (eventType, caseId, taskId, decision)
2. **Integrate with YAWL Engine** - Hook into task state machine transitions to capture lifecycle events
3. **Capture Decision Justifications** - Store structured reasoning data in justification field (JSON schema)
4. **Build Task Context Capture** - Extract task inputs/outputs and store in context field for audit reconstruction
5. **Implement Case Audit Trail** - API to retrieve all execution receipts for a specific workflow case
6. **Add SPARQL Query Support** - Store execution receipts in RDF event log with optimized indexes
7. **Write Execution Receipt Tests** - Test all task lifecycle events, decision capture, justification storage
8. **Performance Optimization** - Ensure receipt creation adds <0.5ms overhead to task execution

### Dependencies
- **Blocked by**: RCPT-001 (Core Receipt Infrastructure)
- **Blocks**: RCPT-006 (Audit API)

### Estimated Effort
- **T-shirt size**: M
- **Weeks**: 1.5-2
- **Rationale**: Builds on core infrastructure, primary work is YAWL integration and schema definition. Test coverage is critical for all task lifecycle transitions.

---

## EPIC-RCPT-003: Mutation Receipts (Delta Operations)

**Goal**: Implement mutation receipts for all RDF graph changes (adds, deletes, governance decisions).

**Value**: Provides tamper-evident audit trail of all knowledge graph mutations, enabling compliance, accountability, and state reconstruction.

**Scope**: Mutation receipt schema, delta operation capture, hook integration (@unrdf/hooks), governance decision records, receipt-gated mutations.

### Acceptance Criteria
- [ ] Mutation receipt schema defined with fields: operation (ADD/DELETE), quads (array), actor, policy, decision
- [ ] All graph mutations automatically generate mutation receipts via hooks
- [ ] Delta operations captured: quad additions, quad deletions, batch operations
- [ ] Actor field records who initiated the mutation (user ID, system component)
- [ ] Policy field records which governance policy was evaluated (if any)
- [ ] Decision field records policy outcome: 'approved', 'rejected', 'conditional'
- [ ] Receipt-gated mutation pattern: mutation only committed if receipt generation succeeds
- [ ] Integration with @unrdf/hooks policy evaluation framework
- [ ] Performance: Mutation receipt creation adds <1ms overhead to graph operations
- [ ] Batch operations generate single receipt with array of deltas (not N receipts)
- [ ] Receipts queryable via SPARQL: "show all mutations by actor X in time range Y"
- [ ] 100% test coverage for add/delete operations, batch mutations, policy integration

### Key Stories
1. **Define Mutation Receipt Schema** - Extend base receipt with mutation-specific fields (operation, quads, actor, policy)
2. **Integrate with Graph Store Hooks** - Hook into Oxigraph add/delete operations to capture mutations
3. **Capture Delta Operations** - Serialize quad arrays in canonical N-Quads format for hashing
4. **Implement Receipt-Gated Mutations** - Pattern: validate → create receipt → commit mutation (atomic)
5. **Add Policy Decision Capture** - Store governance policy evaluation results in decision field
6. **Build Actor Attribution** - Capture actor context (user ID, API key, system component)
7. **Optimize Batch Operations** - Generate single receipt for batch mutations (not N receipts)
8. **Add SPARQL Query Support** - Index receipts by actor, time range, operation type
9. **Write Mutation Receipt Tests** - Test add/delete, batch ops, policy integration, atomic commits

### Dependencies
- **Blocked by**: RCPT-001 (Core Receipt Infrastructure)
- **Blocks**: RCPT-006 (Audit API), RCPT-007 (Tamper Detection)

### Estimated Effort
- **T-shirt size**: M-L
- **Weeks**: 2-2.5
- **Rationale**: Complex integration with graph store and hooks. Receipt-gated mutation pattern requires careful atomic transaction design. Batch optimization is critical for performance.

---

## EPIC-RCPT-004: Freeze Receipts (Universe Snapshots)

**Goal**: Implement freeze receipts for KGC-4D universe snapshots with Git-backed immutability.

**Value**: Provides time-travel capability and long-term provenance via Git, enabling compliance audits, disaster recovery, and state reconstruction.

**Scope**: Freeze receipt schema, KGC-4D integration, Git backbone commits, snapshot hashing, verification against Git blobs.

### Acceptance Criteria
- [ ] Freeze receipt schema defined with fields: universeHash, nquadCount, gitRef, epoch
- [ ] KGC-4D freezeUniverse() automatically generates freeze receipt
- [ ] Universe hash computed via BLAKE3 of canonical N-Quads (sorted deterministically)
- [ ] Git backbone commits snapshot to immutable blob storage (isomorphic-git)
- [ ] Git reference (SHA-1) stored in receipt.gitRef field
- [ ] Freeze receipt stored in RDF event log with SNAPSHOT event type
- [ ] Verification API: `verifyFreezeReceipt(receipt)` fetches Git blob and recomputes hash
- [ ] Performance: Universe freeze (1K quads) completes in <100ms including Git commit
- [ ] Snapshot compression via Git zlib (40-60% size reduction)
- [ ] Temporal ordering: freeze receipts enforce monotonic epoch progression
- [ ] Integration with @unrdf/governance package
- [ ] 100% test coverage for freeze, verification, Git blob retrieval, hash computation

### Key Stories
1. **Define Freeze Receipt Schema** - Extend base receipt with freeze-specific fields (universeHash, nquadCount, gitRef, epoch)
2. **Integrate with KGC-4D Freeze** - Hook into freezeUniverse() to auto-generate receipt
3. **Implement Canonical N-Quads Serialization** - Deterministic quad sorting for consistent hashing
4. **Add Git Backbone Commits** - Commit snapshot to Git blob storage via isomorphic-git
5. **Build Verification API** - Fetch Git blob, recompute hash, compare with receipt
6. **Optimize Snapshot Compression** - Use Git zlib compression (40-60% reduction)
7. **Add Epoch Validation** - Enforce monotonic epoch progression across freeze receipts
8. **Write Freeze Receipt Tests** - Test freeze, verification, Git integration, tamper detection

### Dependencies
- **Blocked by**: RCPT-001 (Core Receipt Infrastructure)
- **Blocks**: RCPT-006 (Audit API), RCPT-007 (Tamper Detection)

### Estimated Effort
- **T-shirt size**: M
- **Weeks**: 1.5-2
- **Rationale**: KGC-4D and Git integration already exist, primary work is receipt schema and verification API. Performance optimization for large snapshots may require iteration.

---

## EPIC-RCPT-005: Merkle Batching & External Anchoring

**Goal**: Implement Merkle tree batching for efficient external anchoring (blockchain, timestamp services).

**Value**: Enables scalable external verification with O(log N) proof size, reducing cost of blockchain anchoring and enabling third-party audit.

**Scope**: Merkle tree construction (BLAKE3), batch receipt generation, proof path computation, verification API, anchor metadata storage.

### Acceptance Criteria
- [ ] Merkle tree construction implemented with BLAKE3 hashing
- [ ] Batch size configurable (default: 1000 receipts per tree)
- [ ] Tree construction handles odd leaf counts (promote unpaired nodes)
- [ ] Merkle root computed and stored in batch receipt
- [ ] Proof path generation: `getProofPath(tree, receiptId)` returns array of {hash, position}
- [ ] Proof verification: `verifyInclusion(root, receipt, proof)` validates membership
- [ ] Performance: Merkle tree construction (1000 receipts) completes in <50ms
- [ ] Proof size: O(log N) = ~640 bytes for 1000-receipt batch
- [ ] Anchor metadata storage: blockchain transaction ID, timestamp service response
- [ ] Verification receipt type for merkle proof validation
- [ ] Batch anchor interval configurable (default: 1 hour)
- [ ] Integration with @unrdf/v6-core receipt chain
- [ ] 100% test coverage for tree construction, proof generation, verification, edge cases (1 leaf, odd counts)

### Key Stories
1. **Implement Merkle Tree Construction** - Build tree from receipt hashes using BLAKE3
2. **Add Proof Path Generation** - Compute sibling path from leaf to root
3. **Build Proof Verification API** - Validate inclusion proof against merkle root
4. **Handle Odd Leaf Counts** - Promote unpaired nodes to next level (no duplication)
5. **Create Batch Receipt Schema** - Store merkle root, batch metadata, anchor references
6. **Add Anchor Metadata Storage** - Record blockchain TX ID or timestamp service response
7. **Implement Auto-Batching** - Trigger merkle tree construction every N receipts or T minutes
8. **Optimize Tree Construction** - Ensure <50ms for 1000-receipt batch
9. **Write Merkle Tests** - Test tree construction, proof generation, verification, edge cases

### Dependencies
- **Blocked by**: RCPT-001 (Core Receipt Infrastructure)
- **Blocks**: None

### Estimated Effort
- **T-shirt size**: M
- **Weeks**: 1.5-2
- **Rationale**: Merkle tree logic is straightforward but requires careful testing for correctness. Blockchain/timestamp anchoring is out of scope (metadata storage only).

---

## EPIC-RCPT-006: Audit Trail API & Query Interface

**Goal**: Implement high-level audit trail API for common compliance and forensic queries.

**Value**: Provides user-friendly interface for auditors, eliminates need for manual SPARQL queries, standardizes audit report generation.

**Scope**: AuditTrail class, decision chain reconstruction, completeness verification, export formats (JSON, CSV, RDF, PDF), SPARQL query templates.

### Acceptance Criteria
- [ ] AuditTrail class implemented with methods: getDecisionChain(), verifyCompleteness(), exportAuditTrail()
- [ ] getDecisionChain(caseId) returns complete receipt chain for workflow case
- [ ] Decision chain includes: receipts, decisions, actors, timeline, merkle root, verification status
- [ ] verifyCompleteness(startTime, endTime) detects gaps in audit trail
- [ ] Gap detection identifies: missing receipts, temporal jumps, expected vs actual counts
- [ ] exportAuditTrail() supports formats: 'json', 'csv', 'rdf', 'pdf'
- [ ] Export includes: receipts, merkle proofs, Git references, signatures
- [ ] SPARQL query templates defined for common patterns: by actor, by time range, by operation type
- [ ] Performance: Decision chain retrieval (100 receipts) completes in <50ms
- [ ] API returns structured errors for invalid queries (404, 400, 500)
- [ ] Integration with @unrdf/cli for command-line audit queries
- [ ] 100% test coverage for all query patterns, export formats, error handling

### Key Stories
1. **Define AuditTrail Class Interface** - Design API methods for common audit queries
2. **Implement getDecisionChain()** - Retrieve complete receipt chain for workflow case
3. **Build verifyCompleteness()** - Detect gaps in audit trail based on temporal ordering
4. **Add Export Functionality** - Generate JSON, CSV, RDF, PDF audit reports
5. **Create SPARQL Query Templates** - Pre-optimized queries for common patterns
6. **Integrate with CLI** - Command-line interface for audit queries: `unrdf audit case-123`
7. **Add Error Handling** - Structured error responses with HTTP-compatible codes
8. **Optimize Query Performance** - Index receipts by caseId, actor, timestamp for <50ms queries
9. **Write Audit API Tests** - Test all query patterns, export formats, gap detection, error cases

### Dependencies
- **Blocked by**: RCPT-002 (Execution Receipts), RCPT-003 (Mutation Receipts), RCPT-004 (Freeze Receipts)
- **Blocks**: None

### Estimated Effort
- **T-shirt size**: M
- **Weeks**: 1.5-2
- **Rationale**: API design and SPARQL query optimization are primary work. PDF export may require additional library integration. CLI integration is straightforward.

---

## EPIC-RCPT-007: Tamper Detection & Verification System

**Goal**: Implement comprehensive tamper detection with automated verification, alerting, and forensic analysis.

**Value**: Provides real-time detection of data corruption or malicious tampering, enabling immediate response and forensic investigation.

**Scope**: TamperDetector class, chain verification, hash mismatch detection, temporal anomaly detection, automated verification schedules, alerting integration.

### Acceptance Criteria
- [ ] TamperDetector class implemented with methods: verifyReceipt(), verifyChain(), detectAnomalies()
- [ ] verifyReceipt(receipt) recomputes all hashes and validates integrity
- [ ] Detects tampering scenarios: payload modification, timestamp changes, hash tampering, reordering, deletion, injection
- [ ] verifyChain(receipts) validates complete chain: genesis, hash links, temporal ordering
- [ ] detectAnomalies() identifies: temporal violations, chain gaps, duplicate receipts, invalid signatures
- [ ] Performance: Single receipt verification completes in <0.5ms (target: <0.001ms)
- [ ] Chain verification (100 receipts) completes in <50ms
- [ ] Automated verification schedules: hourly, daily, on-demand
- [ ] Alerting integration: log errors, emit OTEL spans, trigger webhooks
- [ ] Forensic analysis: tamper report generation with affected receipts, timestamps, suspected attack vectors
- [ ] Integration with @unrdf/observability for monitoring
- [ ] 100% test coverage for all tamper scenarios, detection accuracy, false positive rate

### Key Stories
1. **Implement TamperDetector Class** - Core verification logic with hash recomputation
2. **Add Single Receipt Verification** - Validate payload hash and chain hash
3. **Build Chain Verification** - Validate genesis, hash links, temporal ordering across entire chain
4. **Implement Anomaly Detection** - Identify temporal violations, gaps, duplicates, invalid signatures
5. **Add Automated Verification Schedules** - Cron-style verification: hourly, daily, on-demand
6. **Integrate Alerting** - Log errors, emit OTEL spans, trigger webhooks on tamper detection
7. **Build Forensic Analysis** - Generate tamper reports with affected receipts, timeline, attack vectors
8. **Optimize Verification Performance** - Ensure <0.5ms single receipt, <50ms chain (100 receipts)
9. **Write Tamper Detection Tests** - Test all tamper scenarios, detection accuracy, false positive rate

### Dependencies
- **Blocked by**: RCPT-001 (Core Receipt Infrastructure)
- **Blocks**: None

### Estimated Effort
- **T-shirt size**: M
- **Weeks**: 1.5-2
- **Rationale**: Core verification logic already exists (from RCPT-001), primary work is anomaly detection, alerting, and forensic reporting. Automated scheduling requires cron integration.

---

## Epic Dependencies Graph

```
RCPT-001 (Core Infrastructure)
    ├─→ RCPT-002 (Execution Receipts)
    │       └─→ RCPT-006 (Audit API)
    ├─→ RCPT-003 (Mutation Receipts)
    │       └─→ RCPT-006 (Audit API)
    │       └─→ RCPT-007 (Tamper Detection)
    ├─→ RCPT-004 (Freeze Receipts)
    │       └─→ RCPT-006 (Audit API)
    │       └─→ RCPT-007 (Tamper Detection)
    ├─→ RCPT-005 (Merkle Batching)
    └─→ RCPT-007 (Tamper Detection)
```

**Critical Path**: RCPT-001 → RCPT-002/003/004 → RCPT-006  
**Parallel Tracks**: RCPT-005 (Merkle), RCPT-007 (Tamper Detection) can run concurrently with RCPT-002/003/004

---

## Implementation Roadmap

### Phase 1: Foundation (Weeks 1-3)
- **Week 1-2**: RCPT-001 (Core Infrastructure)
- **Week 3**: RCPT-007 (Tamper Detection) - Basic verification

### Phase 2: Receipt Types (Weeks 4-7)
- **Week 4-5**: RCPT-002 (Execution Receipts) + RCPT-003 (Mutation Receipts) in parallel
- **Week 6**: RCPT-004 (Freeze Receipts)
- **Week 7**: RCPT-005 (Merkle Batching)

### Phase 3: API & Polish (Weeks 8-10)
- **Week 8-9**: RCPT-006 (Audit API)
- **Week 10**: RCPT-007 (Tamper Detection) - Advanced features (alerting, forensics)

**Total Duration**: 10 weeks  
**Team Size**: 2-3 developers + 1 tester  
**Parallel Tracks**: Up to 3 EPICs can run concurrently after week 2

---

## Success Metrics

### Performance Targets
| Metric | Target | Baseline | Gate |
|--------|--------|----------|------|
| Receipt creation | <1ms | 0.017ms | Block if >1ms |
| Receipt verification | <0.5ms | 0.000ms | Block if >0.5ms |
| Chain verification (10) | <50ms | 0.347ms | Block if >50ms |
| Merkle tree (1000) | <50ms | TBD | Block if >50ms |
| Audit query (100 receipts) | <50ms | TBD | Block if >50ms |

### Quality Gates
- **Test Coverage**: ≥80% (lines, branches, functions, statements)
- **OTEL Validation**: ≥80/100 score
- **Lint**: 0 violations
- **Tamper Detection**: 100% accuracy (all scenarios detected)
- **No Regressions**: All existing KGC-4D tests pass

### Production Readiness
- [ ] All 7 EPICs completed with acceptance criteria met
- [ ] Performance benchmarks within targets
- [ ] Security audit passed (no HIGH/CRITICAL vulnerabilities)
- [ ] Documentation complete (API reference, tutorials, examples)
- [ ] 2 runnable proofs validated: tamper detection, audit trail reconstruction
- [ ] Migration path from v5 defined (if applicable)

---

## Risk Register

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| BLAKE3 performance regression | 20% | Medium | Pin library version, add regression tests |
| Git commit overhead | 40% | High | Implement batching, async commits |
| SPARQL query performance | 50% | High | Add indexes, prepared queries, LRU cache |
| Circular dependencies (receipts ↔ store) | 30% | Medium | Dependency injection, clear boundaries |
| Merkle tree edge cases | 35% | Medium | Extensive property-based testing |
| Export format compatibility | 25% | Low | Use standard libraries (csv-parse, jsPDF) |

---

## Open Questions

1. **Signature Algorithm**: Ed25519 (recommended) or ECDSA-secp256k1 (blockchain-compatible)?
2. **Blockchain Anchoring**: Ethereum, Bitcoin, or custom PoA chain?
3. **Timestamp Service**: RFC 3161 TSA or custom solution?
4. **Export Formats**: PDF generation library (jsPDF vs puppeteer)?
5. **Storage Strategy**: RDF-only, Git-only, or hybrid (RDF index + Git blobs)?
6. **Vector Clock**: Keep for distributed scenarios or remove entirely?

**Resolution Deadline**: End of Week 1 (before RCPT-001 implementation starts)

---

**END OF DOCUMENT**
