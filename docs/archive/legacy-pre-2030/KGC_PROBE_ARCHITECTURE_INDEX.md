# @unrdf/kgc-probe Architecture - Complete Documentation Index

**Generated**: 2025-12-27
**Agent**: Agent-2 (System Architect)
**Status**: ✅ Architecture Phase Complete - Ready for Implementation

---

## Documentation Structure (4,203 lines)

### 📘 1. KGC_PROBE_ARCHITECTURE.md (53 KB - Main Reference)

**Complete architectural specification** - Use this as your primary reference document.

**Sections**:
1. Executive Summary
2. Directory Structure (complete tree)
3. Integration Points (Dependency Matrix)
4. Module Entry Point Design (index.mjs)
5. File Specifications (all 44 modules)
   - Schemas (5 modules)
   - Agents (11 modules)
   - Guards (6 modules)
   - Storage (5 modules)
   - Orchestrator (4 modules)
   - CLI (5 modules)
   - Receipts (4 modules)
   - Utils (4 modules)
6. Integration with Existing Packages (@unrdf/*)
7. Dependency Matrix (Summary Table)
8. Deployment Architecture (Monorepo)
9. Component Interaction Diagram
10. Implementation Roadmap (6 Phases)

**Best for**: Deep understanding of architecture, implementation planning

---

### 📋 2. KGC_PROBE_VISUAL_GUIDE.md (29 KB - Quick Reference)

**Visual aids and tabular references** - Use for quick lookups and diagrams.

**Sections**:
1. Complete Directory Tree (ASCII art)
2. Dependency Graph (Detailed layer-by-layer)
3. Data Flow Diagram (Mermaid - with flow arrows)
4. Guard Enforcement Points (Table)
5. Probe Domains (10 agents - table)
6. Merge Strategies Comparison
7. RDF Namespace Mappings
8. Receipt Structure (JSON example)
9. CLI Command Syntax Reference
10. Integration Test Scenarios
11. Performance Targets (SLA table)
12. Error Handling Matrix

**Best for**: Quick reference, diagrams, visual understanding

---

### 💻 3. KGC_PROBE_MODULE_SIGNATURES.md (43 KB - Developer Reference)

**Complete API signatures in JSDoc format** - Use for coding against the API.

**Sections**:
1. Main Entry Point (index.mjs)
2. Schemas Directory (5 Zod schemas)
3. Agents Directory (11 probes)
4. Guards Directory (5 guards + composer)
5. Storage Directory (probe-store, graph-builder, triple-generator)
6. Orchestrator Directory (orchestrator, merge-engine, conflict-resolver, aggregator)
7. Receipts Directory (receipt-builder, merkle-integrator, verifier)
8. Utils Directory (logger, error-handler, types)
9. Module Count Summary (44 modules, ~3,800 LoC)

**Each module includes**:
- Complete JSDoc type definitions
- Function signatures with parameters and return types
- Usage examples
- Implementation notes
- Constructor parameters documented

**Best for**: Implementation, IDE autocomplete reference, API contracts

---

### 🎯 4. KGC_PROBE_ARCHITECTURE_SUMMARY.md (21 KB - Executive Overview)

**High-level summary and roadmap** - Use for planning and stakeholder communication.

**Sections**:
1. Overview (What & Why)
2. Architecture Layers (Bottom-Up - 9 layers)
3. Data Flow Diagram (Executive version)
4. Key Design Principles (5 core principles)
5. Integration Points (External packages)
6. Implementation Roadmap (6 Phases, 6 weeks)
7. Success Criteria (Code, Functionality, Performance, Quality, Integration)
8. File Organization Summary
9. Quick Start (Developer code example)
10. Documentation Files (this index)
11. SPARC Methodology Alignment
12. Next Phase (Implementation Phase)

**Best for**: Project planning, stakeholder updates, progress tracking

---

### 📑 5. This File: KGC_PROBE_ARCHITECTURE_INDEX.md

Navigation guide and quick links to all documentation.

---

## Quick Navigation

### By Role

**Architects / Technical Leads**
→ Start with: SUMMARY → ARCHITECTURE (main) → VISUAL_GUIDE for diagrams

**Implementers / Developers**
→ Start with: MODULE_SIGNATURES → ARCHITECTURE (modules section) → VISUAL_GUIDE (dependency graph)

**Project Managers**
→ Start with: SUMMARY → ARCHITECTURE_INDEX (this file) → VISUAL_GUIDE (timelines)

**Quality Assurance**
→ Start with: VISUAL_GUIDE (test scenarios) → MODULE_SIGNATURES (APIs) → SUMMARY (success criteria)

---

### By Information Type

**System Design**
- ARCHITECTURE.md: Sections 2-9 (structure, integration, design)
- VISUAL_GUIDE.md: Sections 1-3 (tree, dependency, dataflow)
- SUMMARY.md: Section 2 (layers)

**File Structure**
- ARCHITECTURE.md: Section 2 (directory structure)
- VISUAL_GUIDE.md: Section 1 (ASCII tree)
- MODULE_SIGNATURES.md: Section 9 (LOC estimates)

**Module APIs**
- MODULE_SIGNATURES.md: All sections (complete API reference)
- ARCHITECTURE.md: Section 5 (file specifications)

**Data Flows**
- VISUAL_GUIDE.md: Section 3 (Mermaid diagram)
- ARCHITECTURE.md: Section 3 (data flow description)
- SUMMARY.md: Section 3 (executive version)

**Integration Points**
- ARCHITECTURE.md: Section 3 & 6 (integration matrix and details)
- SUMMARY.md: Section 4 (integration summary)

**Implementation Plan**
- SUMMARY.md: Section 6 (6-phase roadmap)
- ARCHITECTURE.md: Section 10 (detailed roadmap)

**Guard Enforcement**
- VISUAL_GUIDE.md: Section 4 (guard matrix)
- ARCHITECTURE.md: Section latest (guards specification)

**Merge Strategies**
- VISUAL_GUIDE.md: Section 6 (comparison table)
- ARCHITECTURE.md: Section latest (merge-engine spec)

**Performance SLAs**
- VISUAL_GUIDE.md: Section 11 (performance targets)
- SUMMARY.md: Section 5 (success criteria)

**Error Handling**
- VISUAL_GUIDE.md: Section 12 (error matrix)
- MODULE_SIGNATURES.md: Section 8 (error-handler API)

---

## Key Numbers

```
Architecture Metrics
├─ Layers: 9 (schemas, agents, guards, storage, orchestration, receipts, CLI, utils, main)
├─ Modules: 44 (implementation modules)
├─ Probe Agents: 10 (security, performance, correctness, structure, completeness, consistency, compliance, coverage, mutation, integration)
├─ Guards: 5 (observation, result, graph, merge, receipt)
├─ Merge Strategies: 4 (consensus, max, min, weighted_sum)
├─ CLI Commands: 4 (run, validate, merge, export)
└─ External Integrations: 6 packages (@unrdf/v6-core, kgc-substrate, oxigraph, kgc-cli, hooks, yawl)

Code Metrics
├─ Implementation: ~3,800 LoC
├─ Tests: ~5,600 LoC (target)
├─ Total: ~9,400 LoC
└─ Code-to-Test Ratio: 1:latest (target)

Documentation
├─ Main Document: 1,200 lines (ARCHITECTURE.md)
├─ Visual Guide: 800 lines (VISUAL_GUIDE.md)
├─ Signatures: 1,300 lines (MODULE_SIGNATURES.md)
├─ Summary: 600 lines (SUMMARY.md)
└─ Total: 4,203 lines

Timeline
├─ Phase 1: Week 1 (Foundation: schemas, guards, utils)
├─ Phase 2: Week 2 (Storage backend)
├─ Phase 3: Week 3 (10 probe agents)
├─ Phase 4: Week 4 (Orchestration + receipts)
├─ Phase 5: Week 5 (CLI integration)
└─ Phase 6: Week 6 (Validation + documentation)
```

---

## Design Highlights

### 1. Defense in Depth
5 guard gates protect data at every transformation:
- Observation Guard (input validation)
- Result Guard (output validation)
- Graph Guard (RDF validation)
- Merge Guard (merge validation)
- Receipt Guard (receipt validation)

### 2. Immutable Audit Trail
All proof results stored in append-only RDF log (KnowledgeStore):
- Deterministic snapshots (BLAKE3)
- Complete audit trail
- Supports forensics and replay

### 3. Cryptographic Certification
Every result produces tamper-evident receipt:
- RSA-4096 signatures
- Merkle chain linking
- Proof-of-work puzzles
- v6-core integration

### 4. 10-Domain Validation
Orthogonal probes validate across:
- Security (auth, encryption, secrets)
- Performance (latency, throughput, memory)
- Correctness (logic, invariants)
- Structure (RDF ontology, terms)
- Completeness (missing properties)
- Consistency (semantic consistency)
- Compliance (standards adherence)
- Coverage (code/test coverage)
- Mutation (fault injection)
- Integration (service integration)

### 5. Flexible Merging
4 merge strategies for different requirements:
- Consensus: All must pass
- Max: Best-case scenario
- Min: Safety margin
- Weighted Sum: Balanced approach

---

## Integration Points

```
@unrdf/v6-core
  ├─ receipt-builder extends BaseReceipt
  └─ merkle-integrator uses merkle/* modules

@unrdf/kgc-substrate
  ├─ probe-store wraps KnowledgeStore
  └─ merkle-integrator uses ReceiptChain

@unrdf/oxigraph
  └─ triple-generator uses dataFactory

@unrdf/kgc-cli
  └─ cli/* registers commands

@unrdf/hooks (proposed)
  └─ guards/* implement GuardPolicy

@unrdf/yawl (optional)
  └─ probe-orchestrator can extend WorkflowBuilder
```

---

## Success Criteria

### ✅ Code Quality
- 100% JSDoc type coverage
- 0 eslint violations (400+ rules)
- 100% test pass rate
- ~3,800 LoC implementation

### ✅ Functionality
- 10 probes all scoreable [latest.0]
- 5 guards enforcing policies
- 4 merge strategies working
- Receipt generation + verification
- CLI commands functional

### ✅ Performance (SLA)
- Single probe: <500ms
- All 10 probes: <2000ms
- Merge: <300ms
- Receipt: <500ms
- Full pipeline: <5000ms

### ✅ Quality (OTEL)
- OTEL validation: ≥80/100
- All major operations spanned
- Metrics for counters + histograms
- Trace propagation through layers

### ✅ Integration
- v6-core receipts verified
- KnowledgeStore append-only verified
- oxigraph dataFactory verified
- kgc-cli command registration verified

---

## File Locations

All files are at project root:

```
/home/user/unrdf/
├── KGC_PROBE_ARCHITECTURE.md              (53 KB) ← Main reference
├── KGC_PROBE_VISUAL_GUIDE.md              (29 KB) ← Quick reference
├── KGC_PROBE_MODULE_SIGNATURES.md         (43 KB) ← Developer reference
├── KGC_PROBE_ARCHITECTURE_SUMMARY.md      (21 KB) ← Executive summary
└── KGC_PROBE_ARCHITECTURE_INDEX.md        (this file)
```

---

## How to Use This Documentation

### Before Implementation Starts
1. Read SUMMARY.md (15 min) - Understand scope and timeline
2. Read ARCHITECTURE.md Sections 1-3 (30 min) - Understand design
3. Scan VISUAL_GUIDE.md (15 min) - Review diagrams

### During Implementation
1. Keep MODULE_SIGNATURES.md open - Reference API contracts
2. Use VISUAL_GUIDE.md Section 2 (dependency graph) for module ordering
3. Refer to ARCHITECTURE.md Section 5 (file specs) for detailed requirements
4. Check VISUAL_GUIDE.md Section 4 (guards) during guard implementation

### During Testing
1. Use VISUAL_GUIDE.md Section 10 (integration scenarios) for test cases
2. Reference VISUAL_GUIDE.md Section 11 (performance targets) for benchmarks
3. Check VISUAL_GUIDE.md Section 12 (error matrix) for error handling tests

### During Code Review
1. Validate against MODULE_SIGNATURES.md (API contracts)
2. Check VISUAL_GUIDE.md Section 2 (dependency rules - no circular deps)
3. Verify SUMMARY.md Section 5 (success criteria)

---

## Document Maintenance

**Last Updated**: 2025-12-27
**Version**: latest (Architecture Phase)
**Status**: Ready for Implementation Phase

**Updates Required When**:
- Module count changes (update counts in all docs)
- New probe domain added (add to VISUAL_GUIDE Section 5)
- Merge strategy changes (update VISUAL_GUIDE Section 6)
- Integration points change (update SUMMARY Section 4)
- Performance SLAs change (update VISUAL_GUIDE Section 11)

---

## Summary

This architecture document provides **complete specification** for the @unrdf/kgc-probe package:

✅ **9 layers** with clear separation of concerns
✅ **44 modules** with single responsibility
✅ **10 probe domains** for orthogonal validation
✅ **5 defensive guards** for quality enforcement
✅ **Complete API documentation** (JSDoc signatures)
✅ **Integration points** with 6 @unrdf packages
✅ **6-week implementation roadmap** with success criteria
✅ **4,200 lines of specification** covering all aspects

**Ready to proceed to Implementation Phase**. Use the architecture as blueprint; implement each module to specification; build comprehensive test suite; validate against OTEL criteria; publish to npm.

---

**Architecture Designed by**: Agent-2 (System Architect)
**Date**: 2025-12-27
**Status**: ✅ COMPLETE - Ready for Implementation
