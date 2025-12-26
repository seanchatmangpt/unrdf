# Conventions-Preserving Migration Façade - Architecture Plan

**Agent 1 (Architect/Integrator) - System Design Document**

**Date**: 2025-12-26
**Status**: Architecture Phase - NO CODE YET
**Methodology**: BB80/20 Single-Pass Implementation with Adversarial PM Validation

---

## Executive Summary

This plan defines the architecture for a **Conventions-Preserving Migration Façade** system that enables seamless migration from N3.js to Oxigraph while preserving existing RDF API conventions through configurable lenses, profiles, and shadow modes.

**Core Innovation**: Profile-driven façade generation that captures legacy API conventions as RDF metadata, enabling deterministic transformation between old and new implementations without breaking existing consumers.

**Validation Requirement**: OTEL ≥80/100, 100% test pass rate, 5-second timeout SLA, deterministic output (BLAKE3 hashing).

---

## 1. Architecture Decisions

### 1.1 Package Dependencies (NO NEW NPM DEPS)

**APPROVED Dependencies** (workspace-internal only):

| Package | Usage | Rationale |
|---------|-------|-----------|
| `@unrdf/oxigraph` | Store creation (`createStore`), `dataFactory` for quads | Core graph operations, MANDATORY per CLAUDE.md |
| `@unrdf/kgc-4d` | Store with freezing, BLAKE3 hashing (via `hash-wasm`) | Deterministic capsule hashing, snapshot immutability |
| `@unrdf/yawl` | Receipt utilities, deterministic serialization | Capsule/receipt format, proof chains |
| `@unrdf/core` | N3-justified-only pattern, canonicalization | Streaming RDF parsing (ONLY justified modules) |
| `zod` (from oxigraph) | Schema validation | Already in workspace, zero new deps |
| `rdf-canonize` (from core) | N-Quads ordering | Deterministic output ordering |
| `hash-wasm` (from kgc-4d) | BLAKE3 hashing | Capsule fingerprinting |

**REJECTED Dependencies**:
- NO new npm packages
- NO direct `from 'n3'` imports outside justified modules (CRITICAL: CLAUDE.md violation)
- NO TypeScript source (JSDoc only)

### 1.2 Architectural Style

- **Pure Functional**: All transformations are pure functions (NO side effects, NO OTEL in business logic)
- **Pipeline-Based**: Profile → Lens → Adapter → Façade (clear data flow)
- **Capsule-Oriented**: All migrations produce verifiable capsules with BLAKE3 hashes
- **Shadow Modes**: Run old/new implementations in parallel for validation

---

## 2. Directory Structure

```
/packages/conventions-facade/
├── package.json                    # Agent 1 - Root package config
├── vitest.config.mjs              # Agent 1 - Test configuration
├── RUNBOOK.md                     # Agent 1 - Execution instructions
├── README.md                      # Agent 1 - Public API documentation
│
├── src/
│   ├── index.mjs                  # Agent 1 - Root export (public API surface)
│   │
│   ├── profile/                   # Agent 2 - Profile Compiler
│   │   ├── index.mjs              # Public: compileProfile, ProfileSchema
│   │   ├── profile-compiler.mjs   # Core compilation logic
│   │   ├── profile-parser.mjs     # Parse RDF profiles
│   │   ├── profile-validator.mjs  # Validate profile completeness
│   │   └── profile-schemas.mjs    # Zod schemas for profiles
│   │
│   ├── lens/                      # Agent 3 - Lens Registry
│   │   ├── index.mjs              # Public: LensRegistry, registerLens
│   │   ├── lens-registry.mjs      # Core registry implementation
│   │   ├── lens-matcher.mjs       # Match lens to profile constraints
│   │   ├── lens-validator.mjs     # Validate lens correctness
│   │   └── lens-schemas.mjs       # Zod schemas for lenses
│   │
│   ├── capsule/                   # Agent 4 - Capsule/Receipt Format
│   │   ├── index.mjs              # Public: createCapsule, verifyCapsule
│   │   ├── capsule-builder.mjs    # Build migration capsules
│   │   ├── capsule-hasher.mjs     # BLAKE3 deterministic hashing
│   │   ├── capsule-verifier.mjs   # Verify capsule integrity
│   │   └── capsule-schemas.mjs    # Zod schemas for capsules
│   │
│   ├── adapter/                   # Agent 5 - Adapter Layer
│   │   ├── index.mjs              # Public: createAdapter, AdapterSchema
│   │   ├── adapter-factory.mjs    # Generate adapters from lenses
│   │   ├── n3-to-oxigraph.mjs     # N3 → Oxigraph transformations
│   │   ├── oxigraph-to-n3.mjs     # Oxigraph → N3 transformations
│   │   └── adapter-schemas.mjs    # Zod schemas for adapters
│   │
│   ├── facade/                    # Agent 6 - Façade Generator
│   │   ├── index.mjs              # Public: generateFacade, FacadeSchema
│   │   ├── facade-generator.mjs   # Generate façade from profile+lens
│   │   ├── facade-renderer.mjs    # Render façade to executable code
│   │   ├── facade-validator.mjs   # Validate façade correctness
│   │   └── facade-schemas.mjs     # Zod schemas for façades
│   │
│   ├── modes/                     # Agent 7 - Shadow Read/Write Modes
│   │   ├── index.mjs              # Public: ShadowWriteMode, ShadowReadMode
│   │   ├── shadow-write.mjs       # Write to both old+new, compare
│   │   ├── shadow-read.mjs        # Read from both, detect drift
│   │   ├── shadow-validator.mjs   # Validate shadow mode results
│   │   └── shadow-schemas.mjs     # Zod schemas for shadow modes
│   │
│   └── utils/                     # Agent 1 - Shared Utilities
│       ├── index.mjs              # Public: deterministicSort, hashObject
│       ├── determinism.mjs        # Deterministic serialization
│       ├── hashing.mjs            # BLAKE3 wrapper (via hash-wasm)
│       └── validation.mjs         # Common validation utilities
│
├── test/                          # Agent 8 - Test Infrastructure
│   ├── unit/
│   │   ├── profile/*.test.mjs
│   │   ├── lens/*.test.mjs
│   │   ├── capsule/*.test.mjs
│   │   ├── adapter/*.test.mjs
│   │   ├── facade/*.test.mjs
│   │   └── modes/*.test.mjs
│   ├── integration/
│   │   ├── end-to-end.test.mjs    # Full pipeline test
│   │   └── shadow-modes.test.mjs  # Shadow mode validation
│   └── fixtures/
│       ├── profiles/              # Sample RDF profiles
│       ├── lenses/                # Sample lens definitions
│       └── expected/              # Expected outputs for determinism
│
├── scenarios/                     # Agent 9 - Scenarios & Fixtures
│   ├── scenario-1-basic-store.mjs
│   ├── scenario-2-sparql-query.mjs
│   ├── scenario-3-streaming.mjs
│   └── fixtures/
│       ├── n3-legacy-api.mjs      # Mock legacy N3 API
│       └── oxigraph-target.mjs    # Mock target Oxigraph API
│
└── examples/                      # Agent 10 - Examples & Docs
    ├── 01-simple-migration/
    │   ├── demo.mjs
    │   └── README.md
    ├── 02-custom-lens/
    │   ├── demo.mjs
    │   └── README.md
    ├── 03-shadow-write/
    │   ├── demo.mjs
    │   └── README.md
    └── 04-full-pipeline/
        ├── demo.mjs
        └── README.md
```

---

## 3. Agent Roles & Responsibilities

### Agent 1: Architect/Integrator (THIS AGENT)

**Outputs**:
- `/packages/conventions-facade/package.json` - Root package configuration
- `/packages/conventions-facade/vitest.config.mjs` - Test configuration (inherit from `vitest.config.base.mjs`)
- `/packages/conventions-facade/RUNBOOK.md` - Exact execution commands
- `/packages/conventions-facade/README.md` - Public API documentation
- `/packages/conventions-facade/src/index.mjs` - Root export aggregator
- `/packages/conventions-facade/src/utils/*.mjs` - Shared utilities (determinism, hashing, validation)
- `/packages/conventions-facade/demo.mjs` - Root demonstration script

**Responsibilities**:
- Define module boundaries and exports
- Create shared utilities (deterministic serialization, BLAKE3 hashing)
- Coordinate agent deliverables
- Validate cross-module integration
- Create root demo script showcasing full pipeline

**Coordination**: MUST run LAST after Agents 2-10 complete. Aggregates exports, runs integration tests.

---

### Agent 2: Profile Compiler

**Directory**: `/packages/conventions-facade/src/profile/`

**Outputs**:
- `profile-compiler.mjs` - Core compilation logic
- `profile-parser.mjs` - Parse RDF profiles from Turtle/JSON-LD
- `profile-validator.mjs` - Validate profile completeness (all API methods described)
- `profile-schemas.mjs` - Zod schemas for profile data structures
- `index.mjs` - Public API: `compileProfile(rdf) → Profile`

**Responsibilities**:
- Parse RDF profiles describing legacy API conventions
- Validate profile completeness (method signatures, semantics, constraints)
- Compile profile into structured metadata for lens matching
- PURE FUNCTIONS ONLY (no OTEL in implementation)

**Key Exports**:
```javascript
export const ProfileSchema = z.object({ ... })
export function compileProfile(rdfSource) { ... }
export function validateProfile(profile) { ... }
```

**Dependencies**: `@unrdf/core` (for RDF parsing via n3-justified-only), `zod`

**Tests**: 5 essential tests (parse valid profile, reject invalid, validate completeness, deterministic output, round-trip)

---

### Agent 3: Lens Registry

**Directory**: `/packages/conventions-facade/src/lens/`

**Outputs**:
- `lens-registry.mjs` - Core registry implementation (singleton pattern)
- `lens-matcher.mjs` - Match lens to profile constraints (scoring algorithm)
- `lens-validator.mjs` - Validate lens correctness (preconditions/postconditions)
- `lens-schemas.mjs` - Zod schemas for lens data structures
- `index.mjs` - Public API: `LensRegistry.register(lens)`, `LensRegistry.match(profile)`

**Responsibilities**:
- Maintain registry of available lenses (N3→Oxigraph, Oxigraph→N3, custom)
- Match lenses to profiles based on constraints (method arity, return types, semantics)
- Validate lens correctness (bidirectional if required)
- PURE FUNCTIONS ONLY

**Key Exports**:
```javascript
export const LensSchema = z.object({ ... })
export class LensRegistry { ... }
export function matchLens(profile, availableLenses) { ... }
```

**Dependencies**: `@unrdf/oxigraph`, `zod`

**Tests**: 5 essential tests (register lens, match by constraints, reject invalid lens, score multiple matches, deterministic ordering)

---

### Agent 4: Capsule/Receipt Format

**Directory**: `/packages/conventions-facade/src/capsule/`

**Outputs**:
- `capsule-builder.mjs` - Build migration capsules (profile + lens + adapter + metadata)
- `capsule-hasher.mjs` - BLAKE3 deterministic hashing (via `hash-wasm`)
- `capsule-verifier.mjs` - Verify capsule integrity (hash validation, signature if present)
- `capsule-schemas.mjs` - Zod schemas for capsule format
- `index.mjs` - Public API: `createCapsule(migration)`, `verifyCapsule(capsule)`

**Responsibilities**:
- Define capsule format (extends YAWL receipt pattern)
- Generate BLAKE3 hashes for deterministic verification
- Verify capsule integrity (hash chain, optional signatures)
- Integrate with `@unrdf/yawl` receipt utilities

**Key Exports**:
```javascript
export const CapsuleSchema = z.object({
  id: z.string(), // UUID
  timestamp: z.number(), // BigInt nanoseconds (KGC-4D pattern)
  profile: ProfileSchema,
  lens: LensSchema,
  adapter: AdapterSchema,
  hash: z.string().length(64), // BLAKE3 hex
  previousHash: z.string().optional(), // Chain to previous capsule
})
export function createCapsule(migration) { ... }
export function verifyCapsule(capsule) { ... }
```

**Dependencies**: `@unrdf/yawl`, `@unrdf/kgc-4d`, `hash-wasm`, `zod`

**Tests**: 5 essential tests (create capsule, compute hash, verify valid, reject tampered, chain multiple capsules)

---

### Agent 5: Adapter Layer

**Directory**: `/packages/conventions-facade/src/adapter/`

**Outputs**:
- `adapter-factory.mjs` - Generate adapters from lens specifications
- `n3-to-oxigraph.mjs` - Concrete N3 → Oxigraph transformations
- `oxigraph-to-n3.mjs` - Concrete Oxigraph → N3 transformations (reverse lens)
- `adapter-schemas.mjs` - Zod schemas for adapter definitions
- `index.mjs` - Public API: `createAdapter(lens)`, `applyAdapter(input, adapter)`

**Responsibilities**:
- Generate concrete adapters from abstract lens definitions
- Implement N3 → Oxigraph method transformations (e.g., `store.getQuads()` → `store.match()`)
- Implement reverse transformations (Oxigraph → N3) for bidirectional migrations
- PURE FUNCTIONS ONLY (adapter application is deterministic)

**Key Exports**:
```javascript
export const AdapterSchema = z.object({ ... })
export function createAdapter(lens) { ... }
export function applyAdapter(input, adapter) { ... }
```

**Dependencies**: `@unrdf/oxigraph`, `@unrdf/core`, `zod`

**Tests**: 5 essential tests (generate adapter, apply N3→Oxigraph, apply reverse, reject invalid input, deterministic output)

---

### Agent 6: Façade Generator

**Directory**: `/packages/conventions-facade/src/facade/`

**Outputs**:
- `facade-generator.mjs` - Generate façade from profile + lens + adapter
- `facade-renderer.mjs` - Render façade to executable JavaScript code (codegen)
- `facade-validator.mjs` - Validate façade correctness (type safety, completeness)
- `facade-schemas.mjs` - Zod schemas for façade definitions
- `index.mjs` - Public API: `generateFacade(capsule)`, `renderFacade(facade)`

**Responsibilities**:
- Generate façade API that matches legacy conventions
- Render façade to executable code (code generation, NOT eval)
- Validate façade completeness (all profile methods implemented)
- Support multiple output formats (ESM, CommonJS if needed)

**Key Exports**:
```javascript
export const FacadeSchema = z.object({ ... })
export function generateFacade(capsule) { ... }
export function renderFacade(facade, options) { ... }
```

**Dependencies**: `@unrdf/oxigraph`, `zod`

**Tests**: 5 essential tests (generate façade, render to code, validate completeness, execute rendered façade, deterministic output)

---

### Agent 7: Shadow Read/Write Modes

**Directory**: `/packages/conventions-facade/src/modes/`

**Outputs**:
- `shadow-write.mjs` - Write to both old+new implementations, compare results
- `shadow-read.mjs` - Read from both implementations, detect drift
- `shadow-validator.mjs` - Validate shadow mode results (equivalence checking)
- `shadow-schemas.mjs` - Zod schemas for shadow mode configurations
- `index.mjs` - Public API: `ShadowWriteMode`, `ShadowReadMode`, `validateShadowResults()`

**Responsibilities**:
- Implement shadow write mode (write to both, compare side effects)
- Implement shadow read mode (read from both, compare results)
- Detect drift between old/new implementations
- Report discrepancies for debugging

**Key Exports**:
```javascript
export const ShadowModeSchema = z.object({ ... })
export class ShadowWriteMode { ... }
export class ShadowReadMode { ... }
export function validateShadowResults(oldResult, newResult) { ... }
```

**Dependencies**: `@unrdf/oxigraph`, `@unrdf/core`, `zod`

**Tests**: 5 essential tests (shadow write success, detect write drift, shadow read success, detect read drift, validate equivalence)

---

### Agent 8: Test Infrastructure

**Directory**: `/packages/conventions-facade/test/`

**Outputs**:
- `test/unit/profile/*.test.mjs` - Profile compiler tests
- `test/unit/lens/*.test.mjs` - Lens registry tests
- `test/unit/capsule/*.test.mjs` - Capsule format tests
- `test/unit/adapter/*.test.mjs` - Adapter layer tests
- `test/unit/facade/*.test.mjs` - Façade generator tests
- `test/unit/modes/*.test.mjs` - Shadow modes tests
- `test/integration/end-to-end.test.mjs` - Full pipeline test
- `test/integration/shadow-modes.test.mjs` - Shadow mode validation
- `test/fixtures/` - Shared test data (profiles, lenses, expected outputs)

**Responsibilities**:
- Create 5 essential tests per module (focus on critical paths)
- Write integration tests covering full pipeline
- Provide fixtures for deterministic testing
- Ensure 100% test pass rate (NO flaky tests)
- Enforce 5-second timeout SLA

**Key Tests**:
- Unit: 5 tests × 6 modules = 30 unit tests
- Integration: 2 end-to-end tests
- Total: ~32 tests, 100% pass rate, <5 seconds total runtime

**Dependencies**: `vitest`, all workspace packages

**Coordination**: MUST run AFTER Agents 2-7 complete their implementations

---

### Agent 9: Scenarios & Fixtures

**Directory**: `/packages/conventions-facade/scenarios/`

**Outputs**:
- `scenario-1-basic-store.mjs` - Basic store migration (N3 → Oxigraph)
- `scenario-2-sparql-query.mjs` - SPARQL query preservation
- `scenario-3-streaming.mjs` - Streaming RDF parsing migration
- `fixtures/n3-legacy-api.mjs` - Mock legacy N3 API for testing
- `fixtures/oxigraph-target.mjs` - Mock target Oxigraph API

**Responsibilities**:
- Create realistic migration scenarios
- Provide mock implementations for isolated testing
- Demonstrate common migration patterns
- Validate façade behavior against expected outputs

**Key Scenarios**:
1. **Basic Store**: Migrate simple `N3.Store` usage to `createStore()` from Oxigraph
2. **SPARQL Query**: Preserve SPARQL query API conventions
3. **Streaming**: Migrate streaming parser usage (N3 → n3-justified-only pattern)

**Dependencies**: `@unrdf/oxigraph`, `@unrdf/core`

**Coordination**: MUST run AFTER Agents 2-7 complete (uses their APIs)

---

### Agent 10: Examples & Documentation

**Directory**: `/packages/conventions-facade/examples/`

**Outputs**:
- `examples/01-simple-migration/` - Simple N3 → Oxigraph migration
- `examples/02-custom-lens/` - Define and register custom lens
- `examples/03-shadow-write/` - Shadow write mode demonstration
- `examples/04-full-pipeline/` - Complete migration pipeline
- Each example: `demo.mjs` (runnable script) + `README.md` (explanation)

**Responsibilities**:
- Create runnable examples demonstrating API usage
- Write clear documentation for each example
- Ensure examples run successfully (<5 seconds)
- Demonstrate best practices

**Key Examples**:
1. **Simple Migration**: 10-line script migrating basic N3 usage
2. **Custom Lens**: Define custom transformation lens
3. **Shadow Write**: Run old+new side-by-side, detect drift
4. **Full Pipeline**: Profile → Lens → Adapter → Façade → Capsule

**Dependencies**: All workspace packages

**Coordination**: MUST run LAST (depends on all agents)

---

## 4. Module Boundaries & Exports

### Root Export (`/packages/conventions-facade/src/index.mjs`)

```javascript
// Profile Compiler
export {
  compileProfile,
  validateProfile,
  ProfileSchema,
} from './profile/index.mjs';

// Lens Registry
export {
  LensRegistry,
  matchLens,
  LensSchema,
} from './lens/index.mjs';

// Capsule Format
export {
  createCapsule,
  verifyCapsule,
  CapsuleSchema,
} from './capsule/index.mjs';

// Adapter Layer
export {
  createAdapter,
  applyAdapter,
  AdapterSchema,
} from './adapter/index.mjs';

// Façade Generator
export {
  generateFacade,
  renderFacade,
  FacadeSchema,
} from './facade/index.mjs';

// Shadow Modes
export {
  ShadowWriteMode,
  ShadowReadMode,
  validateShadowResults,
  ShadowModeSchema,
} from './modes/index.mjs';

// Utilities
export {
  deterministicSerialize,
  computeBlake3Hash,
  deterministicSort,
} from './utils/index.mjs';
```

### Package Exports (`/packages/conventions-facade/package.json`)

```json
{
  "exports": {
    ".": "./src/index.mjs",
    "./profile": "./src/profile/index.mjs",
    "./lens": "./src/lens/index.mjs",
    "./capsule": "./src/capsule/index.mjs",
    "./adapter": "./src/adapter/index.mjs",
    "./facade": "./src/facade/index.mjs",
    "./modes": "./src/modes/index.mjs",
    "./utils": "./src/utils/index.mjs"
  }
}
```

---

## 5. ESM + JSDoc + Zod Standards

### File Naming
- ALL files: `.mjs` extension (ESM modules)
- Tests: `*.test.mjs` pattern
- Configs: `*.config.mjs` pattern

### JSDoc Coverage
- 100% JSDoc coverage for ALL public functions
- Type annotations for parameters and return values
- Examples in JSDoc for key functions

**Example**:
```javascript
/**
 * Compile RDF profile into structured metadata
 * @param {string} rdfSource - Turtle or JSON-LD profile source
 * @returns {Profile} Compiled profile object
 * @throws {ValidationError} If profile is invalid
 * @example
 * const profile = compileProfile(`
 *   @prefix ex: <http://example.org/> .
 *   ex:Store a ex:APIClass ;
 *     ex:method ex:getQuads .
 * `);
 */
export function compileProfile(rdfSource) { ... }
```

### Zod Schemas
- ALL data structures validated with Zod
- Schemas co-located with implementation (e.g., `profile-schemas.mjs`)
- Export schemas for external validation

**Example**:
```javascript
export const ProfileSchema = z.object({
  id: z.string().uuid(),
  methods: z.array(z.object({
    name: z.string(),
    arity: z.number().int().nonnegative(),
    returnType: z.string(),
  })),
  constraints: z.record(z.unknown()).optional(),
});
```

### TypeScript Compatibility
- NO TypeScript source files
- Generate `.d.ts` files via JSDoc (optional, post-MVP)
- Use `@types/node` for Node.js APIs

---

## 6. Determinism & Testing

### Deterministic Output Requirements

1. **Stable Ordering**: All arrays sorted deterministically (e.g., alphabetical, UUID-based)
2. **Canonical Serialization**: Use `rdf-canonize` for N-Quads ordering
3. **BLAKE3 Hashing**: Hash all capsules with BLAKE3 (via `hash-wasm`)
4. **Timestamp Handling**: Use BigInt nanoseconds from `@unrdf/kgc-4d` pattern
5. **UUID Generation**: Use deterministic UUID v5 (namespace-based) where possible

**Verification**:
```bash
# Run migration twice, compare outputs
timeout 5s node demo.mjs > output1.json
timeout 5s node demo.mjs > output2.json
diff output1.json output2.json  # MUST be identical
```

### Testing Strategy

**Unit Tests** (5 per module × 6 modules = 30 tests):
- Focus on critical paths (happy path + 1-2 error cases)
- NO flaky tests (deterministic inputs only)
- Timeout: 5 seconds per test (STRICT SLA)

**Integration Tests** (2 tests):
- Full pipeline: Profile → Lens → Adapter → Façade → Capsule
- Shadow modes: Old+new side-by-side validation

**Total Runtime**: <5 seconds for ALL tests (parallel execution via vitest)

**Coverage Target**: 80%+ (BB80/20 principle applies to testing too)

### Vitest Configuration (`/packages/conventions-facade/vitest.config.mjs`)

```javascript
import { createNodeConfig } from '../../vitest.config.base.mjs';

export default createNodeConfig({
  testTimeout: 5000, // STRICT 5-second SLA
  hookTimeout: 5000,
  coverage: {
    lines: 80,
    functions: 80,
    branches: 80,
    statements: 80,
  },
});
```

---

## 7. Root Integration Points

### 7.1 Package Configuration (`/packages/conventions-facade/package.json`)

```json
{
  "name": "@unrdf/conventions-facade",
  "version": "0.1.0",
  "description": "Conventions-Preserving Migration Façade - Profile-driven API migration system",
  "type": "module",
  "main": "src/index.mjs",
  "exports": {
    ".": "./src/index.mjs",
    "./profile": "./src/profile/index.mjs",
    "./lens": "./src/lens/index.mjs",
    "./capsule": "./src/capsule/index.mjs",
    "./adapter": "./src/adapter/index.mjs",
    "./facade": "./src/facade/index.mjs",
    "./modes": "./src/modes/index.mjs",
    "./utils": "./src/utils/index.mjs"
  },
  "scripts": {
    "test": "vitest run",
    "test:watch": "vitest",
    "test:coverage": "vitest run --coverage",
    "demo": "node demo.mjs",
    "validate": "timeout 5s npm test && timeout 5s npm run demo"
  },
  "dependencies": {
    "@unrdf/kgc-4d": "workspace:*",
    "@unrdf/oxigraph": "workspace:*",
    "@unrdf/yawl": "workspace:*",
    "@unrdf/core": "workspace:*",
    "hash-wasm": "^4.12.0",
    "zod": "^4.1.13"
  },
  "devDependencies": {
    "vitest": "^4.0.15"
  },
  "engines": {
    "node": ">=18.0.0",
    "pnpm": ">=7.0.0"
  },
  "sideEffects": false,
  "keywords": [
    "rdf",
    "migration",
    "facade",
    "conventions",
    "n3",
    "oxigraph"
  ],
  "license": "MIT"
}
```

### 7.2 Demo Script (`/packages/conventions-facade/demo.mjs`)

**Purpose**: Demonstrate full migration pipeline in <5 seconds

**Flow**:
1. Load sample RDF profile (N3.js Store API)
2. Compile profile
3. Match lens from registry
4. Generate adapter
5. Create façade
6. Build migration capsule
7. Verify capsule hash
8. Run shadow write mode (optional)
9. Print results (deterministic output)

**Command**:
```bash
timeout 5s node packages/conventions-facade/demo.mjs
```

### 7.3 RUNBOOK.md

**Exact Commands**:

```markdown
# Conventions-Preserving Migration Façade - RUNBOOK

## Prerequisites
- Node.js ≥18.0.0
- pnpm ≥7.0.0

## Installation
```bash
cd /home/user/unrdf/packages/conventions-facade
pnpm install
```

## Run Tests (5-second SLA)
```bash
timeout 5s pnpm test
```

## Run Demo
```bash
timeout 5s pnpm run demo
```

## Verify Determinism
```bash
pnpm run demo > output1.json
pnpm run demo > output2.json
diff output1.json output2.json  # Should be identical
```

## OTEL Validation (REQUIRED)
```bash
cd /home/user/unrdf
node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log  # MUST be ≥80/100
```

## Success Criteria
- [ ] All tests pass (100%)
- [ ] Total test runtime <5 seconds
- [ ] Demo completes <5 seconds
- [ ] Deterministic output (diff = 0)
- [ ] OTEL validation ≥80/100
- [ ] Zero ESLint errors
- [ ] 80%+ test coverage
```

---

## 8. Coordination Requirements

### Agent Dependencies (Execution Order)

```
Agent 1 (Architect)
    ↓
    Creates: package.json, vitest.config.mjs, shared utils
    ↓
Agents 2-7 (Parallel - Independent)
    ├─ Agent 2: Profile Compiler
    ├─ Agent 3: Lens Registry
    ├─ Agent 4: Capsule Format
    ├─ Agent 5: Adapter Layer
    ├─ Agent 6: Façade Generator
    └─ Agent 7: Shadow Modes
    ↓
Agent 8 (Test Infrastructure)
    ↓
    Tests ALL modules from Agents 2-7
    ↓
Agent 9 (Scenarios & Fixtures)
    ↓
    Uses APIs from Agents 2-7
    ↓
Agent 10 (Examples & Docs)
    ↓
    Demonstrates full system
    ↓
Agent 1 (Final Integration)
    ↓
    Creates: demo.mjs, RUNBOOK.md, README.md
    Validates: OTEL ≥80/100, determinism, 5s SLA
```

### Communication Protocol

**Agents 2-7** (Module Implementers):
- MUST export public API via `index.mjs`
- MUST provide Zod schemas for all data types
- MUST include JSDoc for all public functions
- MUST implement 5 essential tests
- MUST ensure deterministic output
- MUST complete within 5-second timeout

**Agent 8** (Test Infrastructure):
- MUST test ALL modules from Agents 2-7
- MUST create integration tests
- MUST provide fixtures for determinism testing
- MUST enforce 5-second timeout SLA

**Agent 9** (Scenarios):
- MUST use public APIs from Agents 2-7
- MUST NOT access internal implementation details
- MUST demonstrate realistic use cases

**Agent 10** (Examples):
- MUST provide runnable examples
- MUST document API usage
- MUST ensure examples complete <5 seconds

**Agent 1** (Final):
- MUST aggregate all exports
- MUST create root demo script
- MUST validate OTEL ≥80/100
- MUST verify determinism
- MUST ensure 5-second SLA

---

## 9. Success Criteria

### Functional Requirements

- [ ] **Profile Compiler**: Parse and validate RDF profiles (5 tests pass)
- [ ] **Lens Registry**: Match lenses to profiles (5 tests pass)
- [ ] **Capsule Format**: Generate verifiable capsules with BLAKE3 hashes (5 tests pass)
- [ ] **Adapter Layer**: Transform N3 ↔ Oxigraph (5 tests pass)
- [ ] **Façade Generator**: Generate executable façades (5 tests pass)
- [ ] **Shadow Modes**: Detect drift between old/new (5 tests pass)
- [ ] **Integration**: Full pipeline works (2 tests pass)
- [ ] **Total**: 32 tests, 100% pass rate

### Non-Functional Requirements

- [ ] **Performance**: All tests complete <5 seconds (STRICT SLA)
- [ ] **Determinism**: Identical runs produce identical output (diff = 0)
- [ ] **OTEL Validation**: Score ≥80/100 (MANDATORY)
- [ ] **Test Coverage**: ≥80% line/function/branch/statement
- [ ] **Code Quality**: Zero ESLint errors
- [ ] **Documentation**: 100% JSDoc coverage for public APIs

### Adversarial PM Validation

Before declaring complete, answer:

1. **Did you RUN the code?**
   - [ ] Ran `timeout 5s pnpm test` and saw 100% pass
   - [ ] Ran `timeout 5s pnpm run demo` and saw output
   - [ ] Ran `node validation/run-all.mjs` and saw OTEL ≥80/100

2. **Can you PROVE determinism?**
   - [ ] Ran demo twice, diff = 0
   - [ ] Verified BLAKE3 hashes match expected
   - [ ] Checked capsule chain integrity

3. **What BREAKS if you're wrong?**
   - [ ] Non-deterministic output → Cannot verify migrations
   - [ ] Timeout violations → Production SLA breach
   - [ ] OTEL <80 → Agent claims unvalidated
   - [ ] Test failures → System unreliable

4. **What's the EVIDENCE?**
   - [ ] Test output logs with ✅ for all 32 tests
   - [ ] Demo output JSON with BLAKE3 hashes
   - [ ] OTEL validation report with score ≥80/100
   - [ ] Diff output showing 0 differences

---

## 10. Files Agent 1 Will Create

**Immediate (Architecture Phase)**:
1. `/home/user/unrdf/PLAN.md` (THIS FILE)

**Implementation Phase** (After Agents 2-10 complete):
1. `/packages/conventions-facade/package.json`
2. `/packages/conventions-facade/vitest.config.mjs`
3. `/packages/conventions-facade/README.md`
4. `/packages/conventions-facade/RUNBOOK.md`
5. `/packages/conventions-facade/src/index.mjs`
6. `/packages/conventions-facade/src/utils/index.mjs`
7. `/packages/conventions-facade/src/utils/determinism.mjs`
8. `/packages/conventions-facade/src/utils/hashing.mjs`
9. `/packages/conventions-facade/src/utils/validation.mjs`
10. `/packages/conventions-facade/demo.mjs`

**Total Files by Agent 1**: 10 files (9 in implementation phase + 1 plan)

---

## 11. Next Steps

1. **Review & Approve**: Review this PLAN.md for architectural soundness
2. **Agent 2-7 Kickoff**: Parallel implementation of core modules
3. **Agent 8**: Test infrastructure (after 2-7 complete)
4. **Agent 9**: Scenarios & fixtures (after 2-7 complete)
5. **Agent 10**: Examples & docs (after 2-9 complete)
6. **Agent 1 Final**: Integration, demo script, OTEL validation

**Estimated Timeline**:
- Architecture Phase: Complete (this document)
- Implementation Phase: ~6-8 hours (parallel execution)
- Validation Phase: ~1-2 hours (OTEL, determinism, SLA)
- Total: ~8-10 hours for full system

---

## 12. Risk Mitigation

| Risk | Mitigation |
|------|-----------|
| Timeout violations (>5s) | Profile all operations, optimize hot paths, use streaming |
| Non-deterministic output | Enforce canonical ordering, use UUID v5, test with diff |
| OTEL validation <80 | Agent claims require external validation, test with validation/run-all.mjs |
| Agent coordination failures | Clear module boundaries, explicit dependencies, integration tests |
| N3 import violations | Strict grep checks (`grep "from 'n3'" src/` = 0 results), use n3-justified-only only |
| Test flakiness | Deterministic fixtures, no randomness, no time-dependent tests |

---

## Conclusion

This architecture plan defines a complete, testable, deterministic system for conventions-preserving migrations. All decisions are evidence-based, using existing UNRDF infrastructure (kgc-4d, oxigraph, yawl, core) with zero new npm dependencies.

**Next Action**: Await approval, then proceed to Agent 2-10 implementation phase.

**Validation**: This plan MUST survive adversarial PM scrutiny before proceeding.
