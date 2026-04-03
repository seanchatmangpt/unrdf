# Claude Code Configuration - UNRDF v6.0.0

> **UNRDF**: RDF Knowledge Graph Substrate Platform
> **Version**: 6.0.0-rc.1
> **Packages**: 56 packages in pnpm monorepo
> **Language**: JavaScript ESM (.mjs) + JSDoc + Zod

---

## 🤔 Adversarial PM - The Core Principle

**CRITICAL**: Before declaring ANY work complete, question everything. Separate claims from reality. Demand evidence, not assertions.

**The Core Questions**:

- **Did you RUN it?** Or just read the code?
- **Can you PROVE it?** Or are you assuming?
- **What BREAKS if you're wrong?** Be specific.
- **What's the EVIDENCE?** Show the output, logs, metrics.

| Claim | Adversarial Question | Proof Required |
|-------|----------------------|----------------|
| "Tests pass" | Did you RUN `timeout 5s npm test`? | Show full output with pass count |
| "100% coverage" | Did type checker RUN? | `npm run lint` with 0 errors |
| "Production ready" | What FAILS? How handled? | Error paths + OTEL spans |
| "Files migrated" | COUNT correct? Cross-refs valid? | `ls -1 *.md \| wc -l` |

**Adversarial PM is not pessimism** - it's intellectual honesty. Self-deception is the enemy.

---

## 🏗️ Architecture Overview

### 5-Layer Architecture
```
┌─────────────────────────────────────────────────────────────┐
│ Layer 5: APPLICATION                                        │
│   CLI (@unrdf/cli), APIs, React/Vue integrations           │
├─────────────────────────────────────────────────────────────┤
│ Layer 4: KNOWLEDGE SUBSTRATE                                │
│   Hooks, Federation, Streaming, Knowledge Engine            │
├─────────────────────────────────────────────────────────────┤
│ Layer 3: KGC (Knowledge Graph Governance)                   │
│   Temporal event sourcing, Receipts, Cryptographic proofs   │
├─────────────────────────────────────────────────────────────┤
│ Layer 2: RDF CORE                                           │
│   SPARQL, SHACL validation, Parsers (pure functions)        │
├─────────────────────────────────────────────────────────────┤
│ Layer 1: INFRASTRUCTURE                                     │
│   Oxigraph (Rust/WASM), Raft consensus, OpenTelemetry       │
└─────────────────────────────────────────────────────────────┘
```

### Package Tiers (63 Total)

**Essential Tier** (7 packages - always needed):
- `@unrdf/core` - RDF Graph Operations, SPARQL, Foundational Substrate
- `@unrdf/oxigraph` - Oxigraph SPARQL engine binding (10-100x faster than N3)
- `@unrdf/kgc-4d` - KGC 4D Datum & Universe Freeze Engine
- `@unrdf/yawl` - Core YAWL workflow engine
- `@unrdf/hooks` - Policy Definition and Execution Framework
- `@unrdf/streaming` - Change Feeds and Real-time Synchronization
- `@unrdf/v6-core` - ΔGate control plane, unified receipts, delta contracts

**Extended Tier** (8 packages - common use cases):
- `@unrdf/federation` - Distributed RDF Query with RAFT Consensus
- `@unrdf/knowledge-engine` - Rule Engine, Inference, Pattern Matching
- `@unrdf/cli` - Command-line Tools for Graph Operations
- `@unrdf/kgc-runtime` - KGC governance runtime with Zod schemas
- `@unrdf/kgc-substrate` - Deterministic KnowledgeStore
- `@unrdf/receipts` - Batch receipt generation with Merkle trees
- `@unrdf/consensus` - Production-grade Raft consensus
- `@unrdf/v6-compat` - V5 to V6 migration bridge

**Orphan Tier** (43 packages - zero internal dependents):
- `ai-ml-innovations`, `atomvm`, `blockchain`, `caching`, `chatman-equation`, `cli`, `codegen`, `collab`, `composables`, `consensus`, `dark-matter`, `decision-fabric`, `diataxis-kit`, `domain`, `engine-gateway`, `event-automation`, `fusion`, `geosparql`, `kgc-claude`, `kgc-docs`, `kgc-probe`, `kgc-swarm`, `kgc-tools`, `ml-versioning`, `observability`, `project-engine`, `rdf-graphql`, `react`, `self-healing-workflows`, `serverless`, `spatial-kg`, `temporal-discovery`, `yawl-ai`, `yawl-api`, `yawl-durable`, `yawl-kafka`, `yawl-langchain`, `yawl-observability`, `yawl-queue`, `yawl-realtime`, `yawl-viz`, `zkp`
- **Note**: Many may be scaffolding or have zero real implementation. See repo-report.md for detailed analysis.

**Optional Tier** - Performance, AI, visualization packages
**Internal Tier** - Validation, test-utils, docs packages

---

## 🎯 Execution Pattern (Canonical)

```javascript
// Single message - all operations concurrent
Task("Backend Dev", "Implement feature...", "backend-dev")
Task("Tester", "Write tests...", "tester")
TodoWrite { todos: [...10-15 items, ONE call...] }
Bash "timeout 5s npm run build && timeout 5s npm test"
Write "src/feature.mjs"
```

**Before "Done"**:

- ❓ Did I RUN every command or just write them?
- ❓ Did I read output or assume success?
- ❓ What SPECIFIC tests verify the feature?
- ❓ Can user reproduce from scratch?

---

## 🎯 Big Bang 80/20 Methodology

**Single-pass feature implementation using Pareto optimization + information-theoretic correctness guarantees.**

**Core Insight**: In well-specified domains, 20% of features = 80% of value. Implement those ONCE, correctly, using proven patterns.

**When to Use**:

- ✅ Well-defined specs (RDF, APIs, DSLs) + existing patterns + H_spec ≤ 16 bits
- ❌ Exploratory domains, user feedback needed, uncertain requirements

**Results** (Git-verified empirical):
- KGC-4D: 6,327 LoC developed over 20+ days
- 99.8% test pass rate (443/444), OTEL validation 100/100
- Pattern reuse ~64%, 98% static coverage
- P(Correctness): 99.8% measured, 99.997% theoretical bound

**The Litmus Test**: *Can I re-implement RIGHT NOW in ONE pass with ZERO rework using ONLY patterns + static analysis?*

---

## 🚨 CRITICAL RULES

1. **ALL operations concurrent** - Single message = complete work unit
2. **Batch everything** - TodoWrite, files, bash ALL in one message
3. **Timeout all commands** - `timeout 5s npm test` (default), 10-20s only if justified
4. **MEASURE, don't assume** - Run commands. Read output. Prove it.
5. **Pattern reuse** - Copy exactly, don't improve
6. **OTEL is truth** - Agent claims require validation ≥80/100

---

## ⏱️ Timeout SLAs (Andon & Poka Yoke)

**Default**: **5 seconds** for all operations. If exceeded → investigate root cause.

```bash
# ✅ CORRECT
timeout 5s npm test && echo "Tests passed"
timeout 5s pnpm -r test:fast

# ✅ Extended (MUST justify)
timeout 15s npm run test:integration  # DB setup 3-8s + margin
timeout 60s pnpm install              # Initial install

# ❌ WRONG
npm test                  # No timeout = silent hang risk
timeout 60s npm run lint  # Why 60s? Hiding performance issue?
```

**Andon Principle**: When timeout fires, STOP and fix root cause. Don't just increase timeout.

---

## 🛡️ OTEL Validation

**NEVER trust agent claims without OTEL validation.** Agents optimized to appear successful, not be honest.

```bash
node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log  # MUST be ≥80/100
grep "FAILED\|Error" validation-output.log  # MUST be 0 results
```

**Trust Model**:
| Source | Trust | Verification |
|--------|-------|--------------|
| Agent claims | 0% | OTEL ≥80/100 required |
| OTEL spans | 95% | External truth |
| Test output | 90% | Ran + read output |
| "It should work" | 10% | No evidence |

---

## 🚀 Available Agents (54 Total)

**Use Hyper-Advanced First**: `production-validator`, `code-analyzer`, `system-architect`, `performance-benchmarker`, `backend-dev`, `task-orchestrator`

**Core**: `coder`, `reviewer`, `tester`, `planner`, `researcher`

**Specialized**:
- `cicd-engineer` - GitHub Actions workflows
- `security-manager` - Security audit and validation
- `mobile-dev` - React Native development
- `ml-developer` - Machine learning model development

**Verification**:

- ❓ Is THIS agent RIGHT for the task? (Match expertise)
- ❓ Did agent RUN command or just write code?
- ❓ Did I verify output independently?

---

## 📦 Package Structure

### Monorepo Layout
```
/home/user/unrdf/
├── packages/                  # 56 packages
│   ├── core/                 # @unrdf/core - Foundation
│   ├── oxigraph/             # @unrdf/oxigraph - SPARQL engine
│   ├── hooks/                # @unrdf/hooks - Policy framework
│   ├── streaming/            # @unrdf/streaming - Change feeds
│   ├── federation/           # @unrdf/federation - Distributed queries
│   ├── cli/                  # @unrdf/cli - Command-line tools
│   ├── kgc-4d/               # @unrdf/kgc-4d - Time-travel engine
│   ├── kgc-runtime/          # @unrdf/kgc-runtime - Governance runtime
│   ├── v6-core/              # @unrdf/v6-core - v6 control plane
│   ├── v6-compat/            # @unrdf/v6-compat - Migration bridge
│   ├── yawl/                 # @unrdf/yawl - Workflow engine
│   ├── knowledge-engine/     # @unrdf/knowledge-engine - Inference
│   └── ...                   # 44 more packages
├── test/                      # 547 test files
├── benchmarks/                # Performance benchmarks
├── docs/                      # 429 documentation files (Diataxis)
├── examples/                  # 53 example files
└── scripts/                   # Development workflow scripts
```

### Key Dependencies
```json
{
  "oxigraph": "^0.5.2",           // Rust-based SPARQL engine
  "zod": "^3.25.76",              // Runtime validation
  "@opentelemetry/api": "^1.9.0", // Observability
  "vitest": "^4.0.16",            // Test framework
  "pnpm": ">=7.0.0"               // Package manager (REQUIRED)
}
```

---

## 🧪 Testing Infrastructure

### Configuration
- **Framework**: Vitest 4.0.16
- **Default Timeout**: 5 seconds (Andon principle)
- **Coverage Requirement**: 80% (lines, functions, branches, statements)
- **Parallel Execution**: 10 max forks

### Test Commands
```bash
# Core test commands
pnpm test                     # Run all tests
pnpm test:fast                # Fast pre-push suite (<30s)
pnpm test:coverage            # With coverage reports

# Package-specific
pnpm test:core               # Test @unrdf/core
pnpm test:hooks              # Test @unrdf/hooks
pnpm test:cli                # Test @unrdf/cli

# Watch mode
pnpm test:watch              # Watch all tests
pnpm test:watch:pkg core     # Watch specific package
```

### Test Organization
```
test/
├── diff.test.mjs             # 685 lines - Core diff engine
├── project-engine.test.mjs   # 487 lines - Domain inference
├── dark-matter-80-20.test.mjs # 362 lines - Optimization
├── e2e-integration.test.mjs  # 112 lines - End-to-end
├── knowledge-engine/         # Knowledge engine tests
│   └── test-infrastructure/  # Test utilities
├── utils/
│   └── otel-validator.mjs    # OTEL validation
└── vitest-helpers.mjs        # Shared helpers
```

### 80/20 Fast Suite
10 critical tests, ~900 lines, <30 seconds execution:
- `test/diff.test.mjs`
- `test/project-engine.test.mjs`
- `test/dark-matter-80-20.test.mjs`
- `test/e2e-integration.test.mjs`
- `test/knowledge-engine/utils/*.test.mjs`

---

## 🔧 Development Workflow

### Essential Commands
```bash
# Development
pnpm dev                      # Start all dev servers
pnpm build                    # Build all packages
pnpm clean                    # Clean build artifacts

# Quality
pnpm lint                     # ESLint (0 violations target)
pnpm lint:fix                 # Auto-fix lint issues
pnpm format                   # Prettier formatting
pnpm quality                  # Generate quality report

# Validation
./scripts/dev-workflow.sh validate     # Full validation
./scripts/dev-workflow.sh fix          # Auto-fix issues
./scripts/dev-workflow.sh validate:commit # Pre-commit

# Benchmarks
pnpm benchmark                # Run all benchmarks
pnpm benchmark:core           # Core benchmarks
pnpm benchmark:regression     # Regression detection
pnpm profile:cpu              # CPU profiling
pnpm profile:mem              # Memory profiling
```

### CI/CD Workflows
- **ci.yml** - Main CI (TypeScript gate, lint, test matrix Node 18/20/22)
- **quality.yml** - Quality gates (80% coverage, 70+ quality score)
- **release.yml** - Release automation (npm, Docker, GitHub Release)
- **v6-tests.yml** - V6 determinism and performance tests
- **performance-tracking.yml** - Performance regression detection

### Quality Thresholds
| Metric | Threshold | Gate |
|--------|-----------|------|
| Test Coverage | 80% | Block |
| Lint Violations | 0 | Block |
| Quality Score | 70/100 | Block |
| Regression (Latency) | +20% | Block |
| Regression (Memory) | +30% | Block |

---

## 📊 Performance Benchmarks

### Benchmark Suites
```
benchmarks/
├── core/                    # 5 core benchmarks (80/20)
│   ├── 01-hook-registration.bench.mjs
│   ├── 02-hook-execution-latency.bench.mjs
│   ├── 03-concurrent-execution.bench.mjs
│   ├── 04-memory-footprint.bench.mjs
│   └── 05-condition-evaluation.bench.mjs
├── integration/             # Federation, streaming, knowledge-engine
├── v6/                      # v6-specific performance
├── regression/              # Baseline comparison
└── baselines/baseline.json  # Performance baselines
```

### Performance Targets (v6)
| Operation | P95 Target | Actual | Status |
|-----------|------------|--------|--------|
| Receipt Creation | <1ms | 0.017ms | PASS |
| Delta Validation | <5ms | 0.005ms | PASS |
| Receipt Verification | <0.5ms | 0.000ms | PASS |
| Receipt Chain (10) | <50ms | 0.347ms | PASS |
| SPARQL Query (simple) | <10ms | - | - |

---

## 🔒 Security & Validation

### Zod Schema Validation
- **953 Zod imports** across codebase
- **321 schema definition files** (`.schema.mjs`)
- All public APIs validated with Zod

### Validation Patterns
```javascript
// Strict validation (throws on invalid)
const validated = MySchema.parse(data);

// Safe validation (returns result object)
const result = MySchema.safeParse(data);
if (!result.success) { /* handle error */ }

// Custom refinements for security
z.string().refine(
  path => !path.includes('..'),
  'Path traversal not allowed'
);
```

### Security Checks (605 lines)
- Secret detection (API keys, AWS credentials, JWT tokens)
- Injection vulnerability detection (SQL, command, XSS)
- Path traversal prevention
- Error sanitization (removes sensitive info from errors)

### Production Validation Gates
1. Code Quality (20%) - JSDoc, linting, complexity
2. Testing (25%) - Coverage, pass rate, benchmarks
3. Security (20%) - Credential detection, injection patterns
4. Dependencies (15%) - Circular deps, vulnerabilities
5. Documentation (5%) - README, examples, API docs
6. Performance (10%) - Execution benchmarks, memory

---

## 💻 Code Style Essentials

### RDF/Triple Store (MANDATORY)
```javascript
// ✅ CORRECT - Use Oxigraph
import { createStore, dataFactory } from '@unrdf/oxigraph';
const store = createStore();

// ❌ WRONG - Never import N3 directly
import { Store } from 'n3';  // FORBIDDEN in app code

// ✅ CORRECT - Streaming only via justified module
import { Parser } from '@unrdf/core/rdf/n3-justified-only';
```

### File Conventions
- **Extension**: `.mjs` (100% ESM)
- **Naming**: kebab-case (`query-cache.mjs`)
- **Schemas**: Co-located (`.schema.mjs` suffix)
- **Max Lines**: 500 per file

### JSDoc Pattern
```javascript
/**
 * @file [Purpose/Component Name]
 * @module [module-name]
 * @description [Detailed description]
 */

/**
 * Brief description
 * @param {Type} paramName - Description
 * @param {Object} [options] - Optional config
 * @returns {ReturnType} Description
 * @throws {Error} Error conditions
 * @example
 * const result = functionName(arg1);
 */
export function functionName(param, options = {}) { }
```

### Export Pattern
```javascript
// Named exports (primary)
export function createStore() { }
export function executeQuery() { }

// Re-export aggregation in index.mjs
export { createStore, executeQuery } from './store.mjs';
export * from './schemas.mjs';
```

---

## 🎓 Counter-Practice Lessons

### 🚫 DON'T DO (Will fail)

1. Add OTEL to implementation modules (observability ≠ business logic)
2. Add defensive code (guards hide real bugs)
3. Try to improve working patterns (copy exactly)
4. Create complex test suites (5 essential > 95 complex)
5. Trust claims without evidence
6. Import from `'n3'` directly (use `@unrdf/oxigraph`)

### ✅ WHAT WORKS (Evidence-Based)

- Pure functions with NO OTEL in implementation
- Zod validation + simple try-catch
- Batch refactoring in phases
- Centralize library migrations (2 modules first)
- 5 focused tests (100% pass) > 95 flaky (60% pass)
- **MEASURE before claiming success**

**Golden Rule**: Centralize old API in 2 modules first → Result: 100% migration, 40% faster, 60% less memory, 330/330 tests passing.

---

## 🧠 Working With Claude: Internal Patterns

### Token Generation

- Sequential generation - early tokens lock direction
- Hard to backtrack mid-response
- **Action**: Clarify intent upfront

### Context Window

- Sweet spot: 50K-100K tokens
- Degradation: >150K tokens
- **Action**: Summarize periodically

### Uncertainty Calibration

- "Not sure" = 70% confident
- "Let me check" = 20-30% confident
- Confident ≠ 95%+ (often 60-80%)
- **Action**: Use OTEL for claims

### Failure Modes

- **Weak**: Exact counts, nested logic >3 levels
- **Strong**: Pattern matching, refactoring, planning
- **Check**: File counts (`ls | wc -l`), exact numbers

### Quality Degradation

- First response = best thought-through
- Rapid iteration = patch-over-patch
- After ~15 messages = coherence drops
- **Action**: Restart for complex problems

---

## 📚 Documentation (Diataxis Framework)

### Structure
```
docs/
├── diataxis/
│   ├── tutorials/       # Learning-oriented (10-30 min)
│   ├── how-to/          # Task-oriented (3-10 min)
│   ├── reference/       # API documentation
│   └── explanation/     # Conceptual deep-dives
├── substrate/           # Package-specific docs
└── README.md            # Master navigation
```

### Examples
```
examples/
├── 01-minimal-parse-query.mjs  # Beginner (3-5 min)
├── basic-knowledge-hook.mjs    # Intermediate (15-20 min)
├── dark-matter-80-20.mjs       # Advanced (25-30 min)
└── README.md                   # Selection guide
```

---

## 🤔 Session Quality Checklist

**Before declaring complete**:

### Claims vs Reality

- [ ] Did I RUN code or just read it?
- [ ] Did I read FULL output or stop at first pass?
- [ ] What BREAKS if claim is wrong?
- [ ] Can I REPRODUCE from scratch?

### Evidence Quality

- [ ] Test output showing success? (Not "tests pass")
- [ ] File counts with `ls | wc -l`? (Not "~X files")
- [ ] OTEL spans/logs? (Not "should work")
- [ ] Before/after metrics? (Not "faster")

### Process Quality

- [ ] Batched operations in ONE message?
- [ ] Timeout all commands?
- [ ] Verified cross-references?
- [ ] Measured performance?

### Red Flags (Stop if ANY apply)

- ❌ "I think..." / "should be..." → No evidence
- ❌ "Mostly works" / "almost done" → Not acceptable
- ❌ "Code looks good" → Didn't run it
- ❌ Agent says "done" → Didn't verify

---

## Key Rules (Enforcement)

1. **MJS + JSDoc + Zod** - NO TypeScript in source
2. **Pnpm only** - No `package-lock.json` / `yarn.lock`
3. **No analyst agent** - Use `code-analyzer` instead
4. **OTEL is truth** - Agent claims need OTEL ≥80/100
5. **Batch operations** - All in one message
6. **Hyper-advanced first** - Review all 54 agents, pick best match
7. **Pure functions** - No OTEL in business logic
8. **Oxigraph primary** - Never import from `'n3'` in app code

---

## 🏆 Final Truth

**Core Principle**: Claude Flow coordinates, Claude Code creates. **OTEL spans + test output = ONLY validation.**

**The Adversarial PM Question**: _If someone challenged EVERY claim today, which would survive scrutiny?_

Answer honestly. That's your real quality level.

---

## Quick Reference

### Verify Commands
```bash
# Test (5s default timeout)
timeout 5s pnpm test:fast

# Lint (0 violations)
timeout 30s pnpm lint

# Build (verify artifacts)
timeout 60s pnpm build

# OTEL Validation (≥80/100)
node validation/run-all.mjs comprehensive

# Check N3 imports (MUST be 0)
grep -r "from 'n3'" packages/*/src --include="*.mjs" | grep -v n3-justified | wc -l

# File sizes (<500 lines)
find packages/*/src -name "*.mjs" -exec wc -l {} + | awk '$1 > 500'
```

### Package Manager
```bash
pnpm install          # Install dependencies
pnpm -r test          # Run all package tests
pnpm -C packages/core test  # Test specific package
pnpm add -D <pkg>     # Add dev dependency
```

### Git Workflow
```bash
git status            # Check changes
git add -A && git commit -m "feat: description"
git push -u origin <branch>
```
