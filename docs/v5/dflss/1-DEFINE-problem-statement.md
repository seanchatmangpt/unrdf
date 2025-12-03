# DEFINE Phase: Problem Statement & Scope

## Executive Summary

UNRDF v4 has accumulated feature bloat that contradicts its core mission: being a lightweight, powerful RDF client for browsers and Node.js. The v5 refactoring will eliminate this bloat through strategic feature separation and API simplification, enabling UNRDF to compete in the "blue ocean" of lightweight-but-powerful RDF systems instead of the red ocean of heavyweight frameworks.

---

## Problem Statement

### Primary Problem
**UNRDF currently exposes 300+ functions across 280+ files, causing:**
- User confusion: "Which 10 functions do I actually need?"
- Feature bloat: Advanced features mixed with core functionality
- Maintenance burden: Too much surface area to maintain
- Documentation challenge: Can't guide users to "pit of success"
- Package bloat: 2.91 MB when core should be <1 MB

**Impact**: Users reinvent the wheel instead of using recommended patterns because the simplest path is hidden under layers of advanced features.

### Root Causes (Why This Happened)

1. **Accidental Feature Accumulation**
   - v3.0: Added observability (OTEL spans + metrics)
   - v3.1: Added streaming + federation consensus
   - v4.0: Added React hooks (UI integration pattern)
   - v4.1: Added dark matter optimization (80/20 analysis)
   - v4.2: Added HTF framework experiments
   - **Result**: Feature creep without strategic prioritization

2. **Mixed Design Patterns**
   - Direct API (store.addQuad) competing with Composables (useGraph)
   - Knowledge Hooks (autonomous behaviors) optional but prominent
   - Advanced features documented before basic usage
   - No clear beginner → intermediate → advanced progression

3. **Package Structure Confusion**
   - React code in core package (should be separate: unrdf-react)
   - Enterprise features in core (should be separate: unrdf-enterprise)
   - Advanced frameworks in core (should be separate: unrdf-advanced)
   - No clear API boundaries between tiers

4. **Documentation Architecture**
   - 186 markdown files (now 64 after archival)
   - 70 "How-To" guides without priority signals
   - Quick Start shows hooks before query operations
   - No "STOP HERE - you're done" markers for 80% of users

---

## Current State Analysis (v4.2.3)

### Code Metrics
- **Total Lines**: 65,867 across 280+ files
- **Exports**: 300+ functions/objects
- **Test Count**: 2,594 tests (now optimized to 737 in test:fast)
- **Coverage**: 95% thresholds enforced
- **Dependencies**: 35 packages

### Package Metrics
- **Package Size**: 2.91 MB
- **Core Functionality**: ~30 functions (parse, query, validate, hook)
- **Advanced Features**: ~270 functions (streaming, federation, dark matter, etc.)
- **Bloat Ratio**: 90% of code for 10% of users

### Feature Distribution
```
Core RDF Features (Essential):
  - N3 parsing/serialization (n3.js)
  - SPARQL query execution (@comunica/query-sparql)
  - SHACL validation (rdf-validate-shacl)
  - Knowledge Hooks (autonomic behaviors)
  - Composables pattern (useGraph, useTurtle, etc.)

Observability (v3.0+):
  - OTEL tracing (@opentelemetry/sdk-node)
  - Metrics export (Prometheus, Jaeger)
  - Andon signals (visual error reporting)

Advanced Features (v3.1+):
  - Streaming (async iterators)
  - Federation consensus (Raft, Byzantine)
  - Real-time replication

React Integration (v4.0+):
  - useRDFStore hook
  - useRDFQuery hook
  - useRDFValidation hook
  - 3,000+ lines, 20 test files

Optimization (v4.1+):
  - Dark matter 80/20 analysis
  - Code complexity metrics
  - Rule-based optimization

Enterprise (v4.2+):
  - Lockchain audit trails
  - Policy packs framework
  - Vault integration (experimental)

Experimental (v4.2+):
  - HTF framework (Hierarchical Task Framework)
  - DSPy integration (AI/ML reasoning)
```

### Test Suite Status
- **Full Suite**: 2,594 tests, 2-5 minute execution
- **Fast Suite**: 737 tests (test:fast), <30 seconds
- **80/20 Reduction**: 72% fewer tests, same functionality coverage
- **Critical Tests**: 11 files covering parse, query, hooks, security

### Documentation Status
- **Active Docs**: 64 markdown files (after archival)
- **Archived Docs**: 122 files (v2.4-v3.1 historical)
- **Removed**: Quick Start examples showing hooks before queries

---

## Strategic Intent: Blue Ocean Positioning

### Red Ocean (Heavyweight Frameworks)
- Large packages (>5 MB)
- 300+ functions to learn
- Complex configuration
- Steep learning curve
- Competing on features

### Blue Ocean (UNRDF v5 Vision)
- Lightweight core (<1 MB)
- 20-30 essential functions
- Zero configuration
- 5-minute learning curve
- Competing on simplicity + power

### Market Positioning
**Target User**: Browser/Node.js developer who wants to:
- Parse RDF quickly
- Query SPARQL without setup
- Validate with SHACL in 5 lines
- Add autonomous behaviors (Knowledge Hooks) if needed
- NOT worry about enterprise features

**Current Reality**: Users see 300+ functions and build their own solutions.

**Target Reality**: Users find 30 functions, use them, ship product.

---

## Scope Definition

### IN SCOPE (Core Package)
**What stays in unrdf/core**:

1. **RDF Parsing & Querying** (Essential)
   - N3 Turtle/JSON-LD parsing
   - In-memory N3.Store
   - SPARQL query execution (@comunica)
   - Results iteration (bindings, booleans, quads)

2. **Data Validation** (Essential)
   - SHACL shape validation
   - Schema validation (Zod for internal)
   - Error reporting

3. **Composables Pattern** (Essential)
   - useGraph() - query operations
   - useTurtle() - parsing/serialization
   - useStore() - store management
   - Context-based (unctx) composition

4. **Knowledge Hooks** (Essential)
   - Hook definition system (defineHook)
   - Hook execution engine
   - Dependency resolution
   - Basic policy packs (validation, transformation)

5. **Basic Observability** (Essential)
   - OTEL span reporting
   - Error logging
   - Performance metrics
   - Health checks

6. **Lockchain** (Essential)
   - Cryptographic audit trail
   - Merkle tree verification
   - Immutable transaction log
   - Compliance reporting

### OUT OF SCOPE (Move to Separate Packages)
**What leaves core**:

1. **React Integration** → `unrdf-react`
   - useRDFStore hook
   - useRDFQuery hook
   - React component wrappers
   - 3,000+ lines of code
   - 20 test files
   - **Status**: Already moved (v4.2.2+)

2. **Advanced Features** → `unrdf-advanced`
   - Dark matter 80/20 analysis
   - Code complexity metrics
   - Rule-based optimization
   - Performance tuning utilities

3. **Streaming & Real-time** → `unrdf-streaming`
   - Async streaming iterators
   - WebSocket integration
   - Reactive subscriptions
   - Change feeds

4. **Federation & Consensus** → `unrdf-federation`
   - Distributed consensus (Raft, Byzantine)
   - Multi-node replication
   - Eventually consistent stores
   - Peer-to-peer synchronization

5. **Enterprise Features** → `unrdf-enterprise`
   - Advanced policy packs (vault, compliance)
   - Complex rule engines
   - Enterprise RBAC
   - Advanced audit trails

6. **Experimental/Research** → `unrdf-experimental`
   - HTF framework (Hierarchical Task Framework)
   - DSPy integration (AI/ML reasoning)
   - Advanced federation patterns
   - Emerging research features

### DEFER (Post-v5)
**What we evaluate later**:
- GraphQL integration
- Multi-language bindings
- Browser extension support
- AI/ML pipeline integration

---

## Success Criteria

### Quantitative Metrics
| Metric | v4 Current | v5 Target | Success |
|--------|----------|-----------|---------|
| Core Package Size | 2.91 MB | <1 MB | Reduce by 65% |
| Exported Functions (Core) | 300+ | 20-30 | Reduce by 90% |
| Lines of Code (Core) | 65,867 | ~35,000 | Reduce by 47% |
| Dependencies (Core) | 35 | 20 | Reduce by 43% |
| Test Execution (Fast) | <30s | <15s | 2x faster |
| Documentation Files | 64 | 40-50 | Clear & focused |
| User Onboarding Time | 30+ min | 5 min | 6x faster |

### Qualitative Metrics
- ✅ New users immediately find "pit of success" (11-line example)
- ✅ 80% of users don't need to know advanced features exist
- ✅ Documentation never mentions hooks before query operations
- ✅ "STOP HERE - you have a working RDF system" appears after core examples
- ✅ Users implement recommended patterns, not homegrown solutions
- ✅ Feature packages cleanly separate from core

### Technical Quality Gates
- ✅ 100% type coverage (JSDoc + Zod validation)
- ✅ 95%+ test coverage on core
- ✅ Zero critical security vulnerabilities
- ✅ Composables pattern as primary API (direct API secondary)
- ✅ Browser-parity with Node.js (same core functionality)
- ✅ Pre-commit hooks <10 seconds (format, lint only)
- ✅ test:fast suite <15 seconds for pre-push validation

---

## Scope Boundaries (RACI)

| Item | Responsible | Accountable | Consulted | Informed |
|------|-------------|-------------|-----------|----------|
| Core API Design | Architecture | Product | Engineering | Security |
| Feature Separation | Engineering | Product | Architecture | QA |
| Documentation | Docs | Product | Engineering | Community |
| Package Creation | Engineering | Product | QA | Release |
| Testing Strategy | QA | Architecture | Engineering | Product |
| Release Process | Release | Product | QA | Community |

---

## Assumptions & Constraints

### Assumptions
1. React hooks will be maintained in separate unrdf-react package
2. Enterprise features have lower adoption than core features
3. 80/20 principle applies (20% of features for 80% of users)
4. Streaming/federation are advanced features, not core
5. Backwards compatibility not required (major version bump)

### Constraints
1. **Backwards Compatibility**: v5 is breaking change; users need migration guide
2. **Timeline**: 12-week delivery target (phases 1-5)
3. **Resources**: 1 architect + 2 developers minimum
4. **Browser Support**: Must maintain browser parity with Node.js
5. **Performance**: Core bundle must stay <1 MB (gzipped)
6. **Quality**: 95%+ test coverage minimum on core

### Dependencies
- OpenTelemetry infrastructure (existing)
- N3.js for RDF parsing (existing)
- Comunica for SPARQL (existing)
- RDFJS spec compatibility (existing)

---

## Approval & Alignment

### Stakeholder Alignment Required
- [ ] Product: Agrees blue ocean positioning
- [ ] Architecture: Confirms scope boundaries
- [ ] Engineering: Commits to timeline
- [ ] QA: Reviews testing strategy
- [ ] Security: Approves lockchain retention in core

### Sign-Off
- **Defined By**: SPARC Refactoring Team
- **Approved By**: Product Leadership
- **Target Release**: UNRDF v5.0.0 (Q2 2025)
- **Reviewed**: 2025-12-03

---

## Next Steps

1. **Proceed to MEASURE Phase**: Establish baseline metrics for all categories
2. **Create Package Structure**: Plan unrdf-react, unrdf-advanced, unrdf-enterprise separation
3. **Design Core API**: Finalize which 20-30 functions to expose
4. **Documentation Strategy**: Plan beginner-first guide hierarchy

---

**Document Version**: 1.0
**Last Updated**: 2025-12-03
**Status**: Ready for MEASURE Phase
