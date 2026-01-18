# Conventions-Preserving Migration Façade: Master Plan (Phase A)

**Status**: Phase A Complete - Ready for Phase B Implementation Approval
**Date**: 2025-12-26
**Session**: claude/migration-facade-conventions-8UBh6

---

## 🎯 Executive Summary

A 10-agent coordinated system to implement a **Conventions-Preserving Migration Façade** for seamless legacy → KGC-4D migration. The façade makes KGC (Knowledge Graph Core) look exactly like a company's existing service conventions while capturing all state evolution as deterministic RDF deltas with cryptographic receipts.

**Key Invariants**:
- ✅ Zero new npm dependencies (use existing @unrdf/* packages only)
- ✅ ESM only (.mjs + JSDoc + zod)
- ✅ Deterministic end-to-end (identical runs → identical hashes)
- ✅ Shadow modes (write + read) enable zero-downtime migration
- ✅ All outputs verifiable with 5-second timeout SLA

---

## 🏗️ Architecture Overview

```
Input: Legacy Service + Company Conventions + RDF Lens Maps
   ↓
┌─────────────────────────────────────────────────────┐
│   CONVENTIONS-FACADE SYSTEM (7 Core Modules)       │
├─────────────────────────────────────────────────────┤
│ Agent 1: Profile Compiler      → Conventions as Code
│ Agent 2: Lens Registry         → DTO ↔ Graph Mapping
│ Agent 3: Δ Capsule Format      → Atomic State Deltas
│ Agent 4: Store Adapter         → Graph Store Ops
│ Agent 5: Façade Generator      → Convention-Exact Code Gen
│ Agent 6: Shadow Modes          → Zero-Downtime Migration
│ Agent 7: Scenario Harness      → Integration Tests
└─────────────────────────────────────────────────────┘
   ↓
Output: KGC-Backed Services + Receipts + Mismatch Reports + Demo
```

---

## 📦 Directory Structure (Final Layout)

```
/packages/conventions-facade/
├── src/
│   ├── index.mjs                    # Root exports (16 public APIs)
│   ├── profile/                     # Agent 2: Profile Compiler
│   │   ├── index.mjs
│   │   ├── schema.mjs               # Zod profile schema
│   │   ├── compiler.mjs             # Profile compilation + Blake3 digest
│   │   └── validators.mjs           # Rule enforcement
│   ├── lens/                        # Agent 3: Lens Registry
│   │   ├── index.mjs
│   │   ├── registry.mjs             # Lens storage + lookup
│   │   ├── id-rules.mjs             # IRI generation (skolem/template/hash)
│   │   └── normalizer.mjs           # Deterministic field ordering
│   ├── capsule/                     # Agent 4: Δ Capsules & Receipts
│   │   ├── index.mjs
│   │   ├── format.mjs               # Capsule Zod schema + validation
│   │   ├── canonicalize.mjs         # Deterministic N-Quads serialization
│   │   ├── signer.mjs               # BLAKE3 receipt generation
│   │   └── verifier.mjs             # Receipt verification + parent chain
│   ├── adapter/                     # Agent 5: Store Adapter Layer
│   │   ├── index.mjs
│   │   ├── interface.mjs            # StoreAdapter type hints (JSDoc)
│   │   ├── kgc-store-adapter.mjs    # KGCStore wrapper (atomic apply)
│   │   ├── memory-adapter.mjs       # Testing-only in-memory store
│   │   ├── receipt-bridge.mjs       # Agent 4 integration
│   │   └── error-handler.mjs        # Adapter error wrapping
│   ├── generator/                   # Agent 6: Façade Generator
│   │   ├── index.mjs
│   │   ├── generator.mjs            # Service module generation
│   │   ├── templates.mjs            # String templates (imports, functions)
│   │   └── validators.mjs           # Output validation vs profile
│   ├── modes/                       # Agent 7: Shadow Modes
│   │   ├── index.mjs
│   │   ├── shadow-writer.mjs        # Write-both compare logic
│   │   ├── shadow-reader.mjs        # Read-both compare logic
│   │   ├── mismatch-comparator.mjs  # Diff + severity classification
│   │   ├── mismatch-store.mjs       # Ring buffer + query API
│   │   └── router.mjs               # Mode routing (LEGACY/SHADOW/DUAL)
│   └── utils/                       # Agent 1: Shared utilities
│       ├── index.mjs
│       ├── determinism.mjs          # Determinism validation helpers
│       ├── hashing.mjs              # BLAKE3 wrapper + canonical sort
│       ├── validation.mjs           # Common validators (Zod, etc.)
│       └── assertions.mjs           # Test assertion helpers (Agent 8)
├── test/
│   ├── profile.test.mjs             # Agent 2: Profile tests
│   ├── lens.test.mjs                # Agent 3: Lens tests
│   ├── capsule.test.mjs             # Agent 4: Capsule tests
│   ├── adapter.test.mjs             # Agent 5: Adapter tests
│   ├── generator.test.mjs           # Agent 6: Generator tests
│   ├── modes.test.mjs               # Agent 7: Mode tests
│   ├── scenarios.test.mjs           # Agent 8: Scenario harness
│   ├── integration.test.mjs         # Agent 10: Full e2e test
│   ├── boundaries.test.mjs          # Agent 10: Boundary enforcement
│   ├── determinism.test.mjs         # Agent 10: Determinism validation
│   └── fixtures/                    # Agent 8: Test data
│       ├── scenarios.mjs
│       ├── customers.json
│       ├── profiles.json
│       ├── lenses.json
│       └── outputs/
├── scripts/
│   ├── validate-boundaries.mjs      # Agent 10: Circular import detection
│   └── validate-exports.mjs         # Agent 10: Export surface audit
├── examples/
│   └── company-like-service/        # Agent 9: Runnable demo
│       ├── src/
│       │   ├── controllers/         # Legacy CRUD handlers
│       │   ├── dtos/                # Zod schemas
│       │   ├── errors/              # Custom error types
│       │   └── services/            # (Generated façade)
│       ├── test/
│       │   └── example.test.mjs
│       ├── demo.mjs                 # Single-command entry point
│       ├── package.json
│       └── README.md
├── package.json                     # Workspace package (no new deps)
├── vitest.config.mjs                # Inherited from root (5s timeout)
├── README.md                        # Package overview (Agent 9)
├── RUNBOOK.md                       # Exact commands (Agent 9)
└── PLAN.md                          # This file (Agent 1)
```

---

## 🔗 Agent Roles & Ownership (Strict Boundaries)

### **Agent 1: Architect/Integrator** (9 files, 600 LoC)
**Responsibility**: Package scaffold, vitest config, shared utilities, root demo script

**Outputs**:
- `package.json` - workspace package config (zero new deps)
- `vitest.config.mjs` - inherit root (5s timeout, v8 coverage)
- `src/index.mjs` - root exports (16 public APIs)
- `src/utils/` (4 modules) - determinism, hashing, validation, assertions
- `demo.mjs` - single entry point for full demo
- `README.md`, `RUNBOOK.md`, `PLAN.md`

**Does NOT touch**: Profile logic, lens implementation, capsule signing, generator, modes

---

### **Agent 2: Conventions Profile Compiler** (5 files, 400 LoC)
**Responsibility**: Capture company conventions as data, compile to rules, validate artifacts

**Outputs**:
- `src/profile/schema.mjs` - Zod profile schema (7 rule categories)
- `src/profile/compiler.mjs` - Profile compilation + Blake3 digest
- `src/profile/validators.mjs` - Rule enforcement functions
- `src/profile/index.mjs` - Public API (2 exports: `defineProfile`, `compileProfile`)
- `test/profile.test.mjs` - 5 tests (parse, compile, violations, digest, determinism)

**Constraints**:
- ✅ Uses `zod` (existing dependency)
- ✅ Uses `hash-wasm` via Agent 1 utilities
- ❌ NO imports from lens, capsule, adapter, generator, modes
- Output: Immutable, frozen profile objects with Blake3 digest

---

### **Agent 3: Lens Core (API ↔ Graph)** (4 files, 350 LoC)
**Responsibility**: Bidirectional mapping between DTOs and RDF graphs with determinism guarantees

**Outputs**:
- `src/lens/registry.mjs` - Lens storage + lookup (Map-based, O(1))
- `src/lens/id-rules.mjs` - IRI generation (skolem, template, hash-based)
- `src/lens/normalizer.mjs` - Stable field ordering, blank node handling
- `src/lens/index.mjs` - Public API (1 export: `defineLens`)
- `test/lens.test.mjs` - 5 tests (define, toGraph, fromGraph, roundtrip, determinism)

**Constraints**:
- ✅ No circular imports (uses profile only for schema validation)
- ✅ Pure functions (no side effects)
- ❌ NO imports from adapter, generator, modes
- Output: Frozen lens objects with deterministic IRI generation

---

### **Agent 4: Δ Capsules & Receipts** (5 files, 400 LoC)
**Responsibility**: Atomic delta format + cryptographic receipts with chain verification

**Outputs**:
- `src/capsule/format.mjs` - Capsule Zod schema (intent, delta, meta, receipt)
- `src/capsule/canonicalize.mjs` - Deterministic N-Quads + JSON serialization
- `src/capsule/signer.mjs` - BLAKE3 receipt generation (via hash-wasm)
- `src/capsule/verifier.mjs` - Hash verification + parent chain checks
- `test/capsule.test.mjs` - 5 tests (create, tamper, chain, determinism, roundtrip)

**Constraints**:
- ✅ Uses `hash-wasm` for BLAKE3 (existing)
- ❌ NO imports from adapter, generator, modes
- Output: Deterministic hashes (same input → same hash always)
- Parent chain: Optional, enables temporal verification

---

### **Agent 5: Façade Generator** (4 files, 350 LoC)
**Responsibility**: Generate service modules matching conventions exactly

**Outputs**:
- `src/generator/generator.mjs` - Main generator (input: profile + lens + spec → output: files)
- `src/generator/templates.mjs` - String templates (imports, JSDoc, functions, validation)
- `src/generator/validators.mjs` - Output validation vs profile rules
- `src/generator/index.mjs` - Public API (1 export: `generateModule`)
- `test/generator.test.mjs` - 5 tests (spec→code, validation, determinism, JSDoc, imports)

**Constraints**:
- ✅ Deterministic (same inputs → byte-identical files)
- ✅ Validates output with Agent 2 validators
- ❌ NO imports from modes (only interface)
- Output: Convention-compliant service modules (one run, no rework)

---

### **Agent 6: Store Adapter Layer** (6 files, 350 LoC)
**Responsibility**: Abstract store operations, guarantee atomicity, integrate receipts

**Outputs**:
- `src/adapter/interface.mjs` - StoreAdapter interface (JSDoc type hints)
- `src/adapter/kgc-store-adapter.mjs` - KGCStore wrapper (atomic delta apply)
- `src/adapter/memory-adapter.mjs` - Testing in-memory adapter (no freezing)
- `src/adapter/receipt-bridge.mjs` - Agent 4 integration (hash before/after)
- `src/adapter/error-handler.mjs` - Error wrapping + remediation
- `test/adapter.test.mjs` - 5 tests (apply, atomicity, projection, freeze, chain)

**Constraints**:
- ✅ Atomic: All-or-nothing delta application
- ✅ No store internals leaked
- ❌ NO imports from generator, modes
- Output: Clean error messages with remediation hints

---

### **Agent 7: Shadow Write/Read Modes** (6 files, 400 LoC)
**Responsibility**: Zero-downtime migration (execute both systems, compare results)

**Outputs**:
- `src/modes/shadow-writer.mjs` - Write to legacy (primary), KGC (shadow)
- `src/modes/shadow-reader.mjs` - Read from KGC (primary), legacy (shadow)
- `src/modes/mismatch-comparator.mjs` - Field-by-field diff + severity
- `src/modes/mismatch-store.mjs` - Ring buffer (10k) + query API
- `src/modes/router.mjs` - Mode selector (LEGACY_ONLY → KGC_ONLY progression)
- `test/modes.test.mjs` - 5 tests (write, read, mismatch, query, performance)

**Constraints**:
- ✅ Shadow overhead <50ms (P95 latency)
- ✅ Zero client-visible impact (failures don't abort)
- ❌ NO direct store access (go through Agent 5 adapter)
- Output: Mismatch reports with stable ordering (deterministic)

---

### **Agent 8: Scenario Harness & Fixtures** (7 files, 300 LoC)
**Responsibility**: Integration tests + scenario runner + test fixtures

**Outputs**:
- `test/scenarios.test.mjs` - Scenario harness (load, execute, assert, report)
- `test/fixtures/scenarios.mjs` - 5 scenario objects (create, update, query, idempotent, mismatch)
- `test/fixtures/customers.json` - 5 predefined customers
- `test/fixtures/transactions.json` - 10 operations
- `test/fixtures/profiles.json` - 2 convention profiles
- `test/fixtures/lenses.json` - 2 lens definitions
- `test/fixtures/outputs/` - Golden standard JSON

**Constraints**:
- ✅ 5 essential scenarios (100% pass required)
- ✅ Determinism validation (run twice, compare hashes)
- Output: Scenario reports JSON + console summary

---

### **Agent 9: Docs & Example App** (10 files, 500 LoC)
**Responsibility**: Runnable example + documentation

**Outputs**:
- `examples/company-like-service/src/controllers/` - Legacy CRUD handlers
- `examples/company-like-service/src/dtos/` - Zod schemas
- `examples/company-like-service/src/errors/` - Error types
- `examples/company-like-service/demo.mjs` - Single-command entry point
- `examples/company-like-service/test/example.test.mjs` - Shadow mode tests
- `examples/company-like-service/README.md` - Example walkthrough
- `README.md` - Package overview (Agent 1)
- `RUNBOOK.md` - Exact commands (Agent 1)

**Constraints**:
- ✅ Single command: `node examples/company-like-service/demo.mjs`
- ✅ Zero external dependencies (no DB, no network)
- Output: Console demo showing 3 operations + receipt chain + 0 mismatches

---

### **Agent 10: Quality & Boundary Enforcement** (6 files, 300 LoC)
**Responsibility**: Audit boundaries, enforce exports, validate determinism

**Outputs**:
- `test/integration.test.mjs` - Full e2e test (profile→lens→capsule→adapter→façade)
- `test/boundaries.test.mjs` - Boundary enforcement (no circular imports)
- `test/determinism.test.mjs` - Determinism validation (run twice, diff = 0)
- `scripts/validate-boundaries.mjs` - AST parser, circular import detection
- `scripts/validate-exports.mjs` - Export surface audit (exactly 16 exports)
- `scripts/validate-jsdoc.mjs` - JSDoc 100% coverage check

**Constraints**:
- ✅ 0 boundary violations
- ✅ 0 ESLint violations
- ✅ 100% JSDoc coverage on public APIs
- Output: Integration test report + boundary validation log

---

## 📊 Cross-Module Dependencies (Dependency Flow)

```
        Profile (Agent 2)
           ↓ (input)
     ┌─────────────────┐
     │ Lens (Agent 3)  │
     └────────┬────────┘
              ↓ (input)
     ┌─────────────────────┐
     │ Capsule (Agent 4)   │
     └────────┬────────────┘
              ↓ (input)
     ┌─────────────────────┐
     │ Adapter (Agent 5)   │
     └────────┬────────────┘
              ↓ (input)
  ┌───────────────────────────┐
  │ Generator (Agent 6)       │
  └───────────┬───────────────┘
              ↓ (input)
  ┌────────────────────────┐
  │ Modes (Agent 7)        │
  └────────────────────────┘

Shared: Utils (Agent 1) - determinism, hashing, validation, assertions
Test: Scenarios (Agent 8) - integrates all agents
Quality: Boundaries (Agent 10) - audits all agents
```

**NO CIRCULAR IMPORTS**: Unidirectional dependency graph (acyclic).

---

## 🎯 Success Criteria (Adversarial PM Checklist)

### Phase A (Planning) ✅
- [x] 10 agents produced detailed PLAN.md files
- [x] Boundary enforcement rules defined
- [x] Determinism strategy documented
- [x] Integration points specified

### Phase B (Implementation)

#### Code Quality
- [ ] 0 ESLint violations (run `npm run lint`)
- [ ] 100% JSDoc coverage on 16 public APIs
- [ ] 0 boundary violations (run `scripts/validate-boundaries.mjs`)
- [ ] 0 N3 imports outside n3-justified-only
- [ ] 0 direct `@unrdf/oxigraph` imports in app code

#### Testing
- [ ] 32 tests, 100% pass rate (5 per agent × 6 agents + integration + boundaries + scenarios)
- [ ] <5 seconds total runtime (`timeout 5s npm test`)
- [ ] ≥80% line coverage
- [ ] 5/5 scenarios pass

#### Determinism
- [ ] Identical run twice → identical generated files (diff = 0)
- [ ] Identical run twice → identical receipt hashes
- [ ] Profile digest deterministic (same input → same hash)
- [ ] Capsule hashes deterministic

#### Functionality
- [ ] Demo runs: `node examples/company-like-service/demo.mjs`
- [ ] Output shows 3 operations + 0 mismatches + valid receipt chain
- [ ] Shadow modes work (write + read comparison)
- [ ] Mismatch detection functional (scenario 5 detects expected mismatch)

#### Integration
- [ ] All 16 public APIs exported from root `index.mjs`
- [ ] No unused imports or dead code
- [ ] RUNBOOK.md commands work exactly as written
- [ ] OTEL validation ≥80/100 (if applicable)

---

## 🚀 Phase B: Execution Plan

### Step 1: Agent 1 Scaffold (Parallel with Steps 2-7)
- Create `/packages/conventions-facade/` directory structure
- Write `package.json` (zero new deps)
- Write `vitest.config.mjs`
- Implement `src/utils/` (4 modules)
- Create `demo.mjs` skeleton
- **Expected**: <30 minutes, 3 files written, 200 LoC

### Step 2-7: Agents 2-7 Implement Core Modules (Parallel)
- Each agent implements in their directory
- 5 tests per agent (30 tests total)
- No cross-agent code sharing (utilities only from Agent 1)
- **Expected**: ~45 minutes per agent, 2000 LoC combined

### Step 8: Agent 8 Test Infrastructure
- Implement scenario harness (200 LoC)
- Write 5 scenario definitions (100 LoC)
- Create test fixtures (300 LoC)
- **Expected**: ~30 minutes, all tests runnable

### Step 9: Agent 9 Example App
- Create example service (200 LoC)
- Write demo.mjs (150 LoC)
- Write documentation (150 LoC)
- **Expected**: ~30 minutes, demo runnable

### Step 10: Agent 10 Quality Audit
- Run all tests (assert 100% pass, <5s total)
- Run boundary validation (assert 0 violations)
- Run determinism check (assert diff = 0)
- **Expected**: ~20 minutes, validation complete

### Final: Agent 1 Integration
- Wire all exports to root `index.mjs`
- Create RUNBOOK.md + README.md
- Verify demo runs end-to-end
- **Expected**: ~10 minutes, demo working

---

## 📦 Package Dependencies (Zero New Additions)

**Existing Workspace Packages Used**:
- `@unrdf/kgc-4d` - Store, freezing, BLAKE3 hashing
- `@unrdf/oxigraph` - Graph operations (createStore, dataFactory)
- `@unrdf/core` - RDF utilities, canonicalization
- `@unrdf/yawl` - Receipt utilities (existing, for future integration)

**External Dependencies** (Already in workspace):
- `hash-wasm` - BLAKE3 hashing (WASM-based)
- `zod` - Runtime validation (already used throughout repo)
- Standard Node.js modules (no fs-extra, lodash, etc.)

**Forbidden**:
- ❌ TypeScript source files (use .mjs only)
- ❌ New npm dependencies
- ❌ Direct N3 imports (use n3-justified-only)

---

## ✅ Validation Gates (Before Sign-Off)

### Adversarial PM Questions (Must Answer YES)
1. **Did you RUN every command?** (Not just read code)
2. **Did you read FULL output?** (Not assume success from first ✅)
3. **What BREAKS if you're wrong?** (Be specific)
4. **What's the EVIDENCE?** (Show test output, logs, hashes)

### Specific Checks (Before "Done")
- [ ] `timeout 5s npm test` → 32/32 pass (SHOW OUTPUT)
- [ ] `time timeout 5s npm test` → shows duration <5s
- [ ] `node examples/company-like-service/demo.mjs` → shows 3 ops + 0 mismatches
- [ ] `npm run lint` → 0 violations
- [ ] `scripts/validate-boundaries.mjs` → 0 violations
- [ ] `scripts/validate-determinism.mjs` → `diff` output = 0 bytes
- [ ] Generated files exist + have correct content (spot-check 3 files)
- [ ] Receipts valid (Blake3 hashes recompute to same value)

---

## 🎬 Next Action: Phase B Ready?

**APPROVAL REQUIRED**:
This master plan represents Phase A complete. All 10 agents have provided detailed, non-overlapping plans.

To proceed with **Phase B (Implementation)**:
1. User reviews this master plan
2. User approves proceeding to Phase B
3. Agent 1 creates scaffold
4. Agents 2-7 implement in parallel
5. Agents 8-10 complete validation

**Estimated Total Time Phase B**: ~3 hours (parallel execution)
**Total LoC Estimate**: ~4000-5000 (heavily commented, 100% JSDoc)

---

## 📋 Key Decisions Made (Phase A)

| Decision | Rationale | Impact |
|----------|-----------|--------|
| Use KGCStore from @unrdf/kgc-4d | Already has BLAKE3, freezing, git-backed | Zero new deps, proven implementation |
| Deterministic via N-Quads + JSON sort | Minimal implementation, no external lib | Stable hashes across runs, <10ms |
| Shadow modes async, fire-and-forget | Client latency SLA, transparency | <50ms overhead, zero request failures |
| 5 scenario fixtures, not 50 | 80/20 methodology, essential cases | Fast tests (<3s), high coverage |
| Generator uses string templates | No handlebars/ejs, keep deps minimal | Deterministic output, <500ms per op |
| 10 agents, strict boundaries | No overlap, parallel execution | <3 hours total, clear ownership |

---

**STATUS**: ✅ Phase A COMPLETE - Awaiting user approval for Phase B execution.
