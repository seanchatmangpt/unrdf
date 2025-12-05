# UNRDF + Erlang/OTP System Architecture

## Phase 4: Complete System Integration Documentation

Phase 4 documents the end-to-end architecture for integrating UNRDF (RDF graph library) with Erlang/OTP systems, achieving reliable JS ↔ Erlang interoperability through:

1. **Shared Protocol Layer** - Binary-level compatibility
2. **Multi-Runtime Support** - Real OTP vs Browser AtomVM simulation
3. **Dual Adapters** - Transparent switching between runtimes
4. **Golden Test Strategy** - Confidence in production behavior

---

## Overview

### The Problem

UNRDF provides excellent RDF capabilities for JavaScript, but production systems often require Erlang/OTP for:
- Distributed fault-tolerance
- Hot code reloading
- Supervision trees and process management
- Mature ecosystem (cowboy, gen_server, mnesia, etc.)

However, most development happens locally in browsers, where spinning up a full Erlang cluster is costly.

### The Solution

This architecture enables:
- **Fast local development** with offline RDF simulation in the browser (AtomVM + WASM)
- **Protocol equivalence** ensuring behavior matches real OTP nodes
- **Transparent adapter switching** - same JS code works offline and online
- **CI gating** - real OTP nodes validate production readiness

### The 80/20 Insight

You don't need perfect OTP emulation. You need:

1. **Transport contract** - How connections work (WebSocket, HTTP, TCP)
2. **Message contract** - Envelope shape, encoding, versioning
3. **Failure contract** - Timeouts, errors, backoff/retry behavior

If these three contracts match between browser simulation and real OTP, your JS will work in production.

---

## Architecture Layers

### Layer 1: Protocol Layer (Shared)

The protocol layer is **identical** for both runtimes and contains:

- **Message encoding/decoding** (JSON/BERT/protobuf)
- **Session tracking** (request/response correlation)
- **Error encoding** (standardized error codes and messages)
- **Versioning** (how to handle unknown fields)

This is where most JS bugs occur, so keeping it unified is critical.

### Layer 2: Domain Layer (Mostly Shared)

Business logic that works on both runtimes:

- **Erlang side**: gen_server/gen_statem behaviors, supervisors, state machines
- **Shared subset**: Pure Erlang, no NIFs, AtomVM-compatible
- **JS side**: RDF queries, transformations, validation logic

Key constraint: Code must work in both full OTP and AtomVM (WASM browser).

### Layer 3: Environment Layer (Swapped Per Target)

Runtime-specific implementations:

**For Real OTP:**
- `gen_tcp` for connections
- `cowboy` for HTTP/WebSocket
- `ets` for caching
- `mnesia` for distribution
- File I/O, SSL, NIFs, etc.

**For Browser AtomVM:**
- WebSocket (browser API)
- In-memory maps/lists (no ets)
- Browser storage (localStorage/IndexedDB)
- Stubs for distribution

---

## Files in Phase 4

### 1. Core Architecture Documents

- **01-PROTOCOL-DESIGN.md** (5,000 words)
  - Message envelope format
  - Encoding standards
  - Versioning strategy
  - Error codes and surfaces

- **02-RUNTIME-ARCHITECTURE.md** (6,000 words)
  - Core/env layer split
  - Code organization
  - Dependency injection patterns
  - Compilation targets

- **03-BROWSER-SIMULATION.md** (5,000 words)
  - AtomVM + WASM setup
  - JS ↔ Erlang bridge
  - Limitations and workarounds

- **04-DUAL-ADAPTERS.md** (4,000 words)
  - Real backend adapter (WebSocket/HTTP)
  - Simulated backend adapter (AtomVM)
  - Adapter interface
  - Swapping mechanism

### 2. Implementation Guides

- **05-SETUP-ERLANG-SIDE.md** (6,000 words)
  - Project structure
  - Code factoring for dual-runtime
  - Building both runtimes
  - Testing locally

- **06-SETUP-JS-SIDE.md** (5,000 words)
  - Protocol client interface
  - Implementing both adapters
  - Configuration and switching
  - Type safety

- **07-INTEGRATION-TESTING.md** (7,000 words)
  - Golden test vectors
  - Fault injection framework
  - Running against both runtimes
  - Real node gating in CI

### 3. Reference & Migration

- **08-PROTOCOL-REFERENCE.md** (4,000 words)
  - Complete message types
  - Error codes
  - Version compatibility matrix

- **09-MIGRATION-GUIDE.md** (5,000 words)
  - Adopting this pattern for existing systems
  - Refactoring checklist
  - Deployment strategy

- **10-TROUBLESHOOTING.md** (3,000 words)
  - Common issues
  - Debugging strategies
  - Performance tuning

---

## Total Deliverables

- **10 comprehensive documents** (~50,000 words)
- **Complete working examples** (code snippets)
- **Testing patterns and templates**
- **CI/CD integration guides**

---

## How to Use These Documents

### For Architects
Start with: **01-PROTOCOL-DESIGN.md** + **02-RUNTIME-ARCHITECTURE.md**

Understand the contract-based approach and overall layering.

### For Backend Team
Follow: **02-RUNTIME-ARCHITECTURE.md** → **05-SETUP-ERLANG-SIDE.md** → **07-INTEGRATION-TESTING.md**

Set up the Erlang side with proper code split and testing.

### For Frontend Team
Follow: **04-DUAL-ADAPTERS.md** → **06-SETUP-JS-SIDE.md** → **07-INTEGRATION-TESTING.md**

Implement dual adapters and ensure local/online equivalence.

### For DevOps/QA
Focus on: **07-INTEGRATION-TESTING.md** + **08-PROTOCOL-REFERENCE.md**

Set up golden tests, fault injection, and real node gating.

### For New Contributors
Start with: **01-PROTOCOL-DESIGN.md** (understand contracts), then **09-MIGRATION-GUIDE.md** (understand system)

---

## Next Steps

1. Read this overview
2. Choose your role above
3. Follow the recommended path
4. Implement section by section
5. Run integration tests against both runtimes
6. Gate production deployments on real OTP node tests

---

**Status**: ✅ Phase 4 ready for full implementation

**Completion Target**: All 10 documents with working code examples

**Expected Outcome**: Production-ready UNRDF + Erlang/OTP integration system with local browser development capability
