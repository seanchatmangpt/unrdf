# UNRDF V6 Capsule Backlog

**Definition**: A "capsule" is a scoped, constraint-bound work unit with:
- **Δ Scope**: Exact inputs/outputs, no scope creep
- **Constraints**: Time, resources, dependencies
- **Expected Receipts**: Proof of completion (KGC-4D)
- **Tests**: Pass/fail criteria

**Principle**: Capsules are designed for parallel execution where possible, sequential where dependencies exist.

---

## Priority Tiers

### P0: Critical Path (Blocks all other work)
Must complete before any other capsules can start.

### P1: High Value (80/20 focus)
20% of work that delivers 80% of value.

### P2: Standard (Required for v6.0.0)
Essential for release, but not blocking.

### P3: Nice-to-Have (Post-v6.0.0)
Improves DX but not required for stable release.

---

## Capsule Template

```yaml
capsule_id: "V6-001"
title: "Implement KGC-4D Receipt Wrapper"
priority: P0
status: pending
assignee: null
dependencies: []

scope:
  inputs:
    - Existing function signatures
    - KGC-4D freeze API
  outputs:
    - Higher-order function: wrapWithReceipt()
    - Receipt schema: ReceiptSchema (Zod)
    - 5 example usages

constraints:
  time: 8 hours
  max_loc: 150 lines
  tests: ≥5 passing tests

expected_receipts:
  - type: "code_commit"
    content: "Git commit hash + diff"
  - type: "test_results"
    content: "Vitest output (100% pass)"
  - type: "lint_check"
    content: "ESLint 0 warnings"

tests:
  - name: "Wraps sync function"
    input: "(x) => x * 2"
    output: "Receipt with result"
  - name: "Wraps async function"
    input: "async (x) => x * 2"
    output: "Receipt with promise result"
  - name: "Handles errors"
    input: "() => { throw new Error() }"
    output: "Receipt with error captured"

success_criteria:
  - [ ] wrapWithReceipt() function exists
  - [ ] 5 tests pass
  - [ ] Example in docs/v6/examples/
  - [ ] ESLint passes
  - [ ] Receipt matches schema
```

---

## P0: Critical Path (4 capsules)

### V6-001: KGC-4D Receipt Wrapper HOF
**Status**: Pending
**Dependencies**: None
**Effort**: 8 hours
**Parallel**: Yes

Create `wrapWithReceipt()` higher-order function to add receipts to existing functions.

**Deliverables**:
- `/packages/kgc-4d/src/wrapper.mjs`
- `/packages/kgc-4d/test/wrapper.test.mjs`
- 5 passing tests

**Success**: Any function can be wrapped with zero code changes.

---

### V6-002: Zod Schema Generator
**Status**: Pending
**Dependencies**: None
**Effort**: 10 hours
**Parallel**: Yes

Generate Zod schemas from TypeScript types and JSDoc comments.

**Deliverables**:
- `/packages/v6-compat/src/schema-generator.mjs`
- CLI: `unrdf schema-gen src/**/*.mjs`
- 10 example conversions

**Success**: Run on @unrdf/core, generate 100% coverage schemas.

---

### V6-003: @unrdf/v6-compat Package
**Status**: Pending
**Dependencies**: V6-001, V6-002
**Effort**: 16 hours
**Parallel**: No (depends on V6-001, V6-002)

Implement full compatibility layer for v5 → v6 migration.

**Deliverables**:
- Package structure (see Migration Plan)
- Adapters for all breaking changes
- ESLint rules for deprecations
- Migration guide with examples

**Success**: v5 code runs on v6 with adapters, no changes.

---

### V6-004: Update Root Workspace to 6.0.0-alpha.1
**Status**: Pending
**Dependencies**: V6-003
**Effort**: 2 hours
**Parallel**: No

Update root package.json and workspace config.

**Deliverables**:
- Version bump to 6.0.0-alpha.1
- Add v6-compat to workspace
- Update pnpm-lock.yaml

**Success**: `pnpm install` works, all packages resolve.

---

## P1: High Value - Core Packages (10 capsules)

### V6-010: @unrdf/oxigraph L3 → L4
**Status**: Pending
**Dependencies**: V6-001
**Effort**: 10 hours
**Parallel**: Yes (after V6-001)

**Tasks**:
- Add timeout guards to all SPARQL queries (default 5s)
- Add Zod validation to query inputs
- Add adversarial tests (100+ invalid inputs)

**Success**: All L4 criteria pass (see Maturity Ladder).

---

### V6-011: @unrdf/oxigraph L4 → L5
**Status**: Pending
**Dependencies**: V6-010, [all other L5 packages]
**Effort**: 7 hours
**Parallel**: No (sequential after all L4)

**Tasks**:
- Integration tests with @unrdf/core, @unrdf/hooks, @unrdf/streaming
- Performance benchmarks (no regression)
- OTEL validation ≥80/100

**Success**: L5 checklist complete.

---

### V6-012: @unrdf/core L2 → L3
**Status**: Pending
**Dependencies**: V6-001
**Effort**: 14 hours
**Parallel**: Yes (after V6-001)

**Tasks**:
- Remove Date.now() / Math.random() from utils
- Integrate receipts for all operations
- Create reproducible fixtures (50+ tests)

**Success**: Deterministic test passes 10 runs.

---

### V6-013: @unrdf/core L3 → L4
**Status**: Pending
**Dependencies**: V6-012
**Effort**: 12 hours
**Parallel**: Yes (after V6-012)

**Tasks**:
- Add timeout guards to I/O operations
- Zod validation on all external inputs
- Adversarial tests

**Success**: L4 checklist complete.

---

### V6-014: @unrdf/core L4 → L5
**Status**: Pending
**Dependencies**: V6-013, [all other L5]
**Effort**: 10 hours
**Parallel**: No

**Tasks**:
- Integration tests
- Performance benchmarks
- OTEL validation

**Success**: L5 checklist complete.

---

### V6-015: @unrdf/kgc-4d L4 → L5
**Status**: Pending
**Dependencies**: V6-001
**Effort**: 7 hours
**Parallel**: Yes (after V6-001)

**Tasks**:
- Integration tests with all L5 packages
- Receipt composition tests
- Performance benchmarks

**Success**: L5 checklist complete.

---

### V6-016: @unrdf/hooks L2 → L5
**Status**: Pending
**Dependencies**: V6-001, V6-012
**Effort**: 25 hours
**Parallel**: Yes (after deps)

**Tasks**:
- Enforce receipts on all hooks
- Deterministic fixtures
- Timeout guards
- Adversarial tests
- Integration tests

**Success**: L5 checklist complete.

---

### V6-017: @unrdf/streaming L2 → L5
**Status**: Pending
**Dependencies**: V6-001, V6-012
**Effort**: 27 hours
**Parallel**: Yes (after deps)

**Tasks**:
- AsyncIterator migration
- Receipt integration
- Deterministic replay tests
- Timeout guards
- Integration tests

**Success**: L5 checklist complete.

---

### V6-018: @unrdf/federation L2 → L5
**Status**: Pending
**Dependencies**: V6-001, V6-012
**Effort**: 28 hours
**Parallel**: Yes (after deps)

**Tasks**:
- SPARQL template literals
- Receipt composition
- Timeout enforcement
- Adversarial tests
- Integration tests

**Success**: L5 checklist complete.

---

### V6-019: @unrdf/cli L2 → L5
**Status**: Pending
**Dependencies**: V6-001, V6-012
**Effort**: 22 hours
**Parallel**: Yes (after deps)

**Tasks**:
- Zod validation on all args
- Receipt generation for commands
- Timeout guards
- Integration tests

**Success**: L5 checklist complete.

---

## P2: Standard - Remaining Core (5 capsules)

### V6-020: @unrdf/yawl L2 → L5
**Status**: Pending
**Dependencies**: V6-015, V6-016
**Effort**: 30 hours
**Parallel**: Yes (after deps)

**Tasks**:
- Workflow receipt composition
- Deterministic execution
- Timeout guards
- Integration tests

**Success**: L5 checklist complete.

---

### V6-021: @unrdf/knowledge-engine L1 → L5
**Status**: Pending
**Dependencies**: V6-001, V6-014
**Effort**: 50 hours
**Parallel**: Yes (after deps)

**Tasks**:
- Full maturity ladder (L1 → L5)
- Reasoning receipts
- Deterministic inference
- Integration tests

**Success**: L5 checklist complete.

---

### V6-022: @unrdf/project-engine L1 → L5
**Status**: Pending
**Dependencies**: V6-001, V6-014
**Effort**: 45 hours
**Parallel**: Yes (after deps)

**Tasks**:
- Full maturity ladder
- Project receipts
- Deterministic builds
- Integration tests

**Success**: L5 checklist complete.

---

### V6-023: @unrdf/observability L1 → L5
**Status**: Pending
**Dependencies**: V6-001
**Effort**: 40 hours
**Parallel**: Yes (after V6-001)

**Tasks**:
- Full maturity ladder
- OTEL receipt integration
- Deterministic metrics
- Integration tests

**Success**: L5 checklist complete.

---

### V6-024: @unrdf/validation L1 → L5
**Status**: Pending
**Dependencies**: V6-001, V6-014
**Effort**: 35 hours
**Parallel**: Yes (after deps)

**Tasks**:
- Full maturity ladder
- Validation receipts
- Deterministic SHACL
- Integration tests

**Success**: L5 checklist complete.

---

## P3: Remaining 37 Packages (Batched)

### V6-030 to V6-066: Batch Migration
**Status**: Pending
**Dependencies**: V6-001 to V6-024
**Effort**: 15-30 hours each (avg 22 hours)
**Total**: 814 hours (~20 weeks for 5 devs)
**Parallel**: Yes (highly parallelizable)

**Packages** (alphabetical):
1. @unrdf/atomvm
2. @unrdf/blockchain
3. @unrdf/caching
4. @unrdf/collab
5. @unrdf/composables
6. @unrdf/consensus
7. @unrdf/dark-matter
8. @unrdf/diataxis-kit
9. @unrdf/docs
10. @unrdf/domain
11. @unrdf/engine-gateway
12. @unrdf/fusion
13. @unrdf/graph-analytics
14. @unrdf/integration-tests
15. @unrdf/kgc-claude
16. @unrdf/kgc-cli (+ 26 CLI extensions)
17. @unrdf/kgc-substrate
18. @unrdf/kgn
19. @unrdf/ml-inference
20. @unrdf/ml-versioning
21. @unrdf/nextra
22. @unrdf/react
23. @unrdf/rdf-graphql
24. @unrdf/semantic-search
25. @unrdf/serverless
26. @unrdf/test-utils
27. @unrdf/yawl-ai
28. @unrdf/yawl-api
29. @unrdf/yawl-durable
30. @unrdf/yawl-kafka
31. @unrdf/yawl-langchain
32. @unrdf/yawl-observability
33. @unrdf/yawl-queue
34. @unrdf/yawl-realtime
35. @unrdf/yawl-viz

**Tasks per package**:
- JSDoc + Zod schemas (L1 → L2)
- Receipt integration (L2 → L3)
- Timeout guards + validation (L3 → L4)
- Integration tests (L4 → L5)

**Success**: All 47 packages at L5.

---

## Execution Timeline

### Week 1-2: Foundation (P0)
- [x] V6-001: Receipt wrapper (Week 1)
- [x] V6-002: Schema generator (Week 1)
- [x] V6-003: v6-compat package (Week 2)
- [x] V6-004: Workspace update (Week 2)

### Week 3-8: Core 10 (P1)
Parallel execution of V6-010 to V6-019 (6 weeks).

**Week 3-4**: L2 → L3 (deterministic)
**Week 5-6**: L3 → L4 (adversarial safety)
**Week 7-8**: L4 → L5 (integration)

### Week 9-14: Extended Core (P2)
Parallel execution of V6-020 to V6-024 (6 weeks).

### Week 15-35: Batch Migration (P3)
5 developers × 4 packages/week = 20 packages/week.
37 packages ÷ 20/week ≈ 2 weeks (staggered start).

**Accelerated**: Week 15-17 (3 weeks).

---

## Parallel vs Sequential Capsules

### Parallel-Safe (Independent)
- All L1 → L2 migrations (schema generation)
- All L2 → L3 migrations (receipt integration)
- All L3 → L4 migrations (adversarial safety)

**Concurrency**: Up to 47 capsules in parallel (limited by dev count).

### Sequential (Dependency Chains)
- L4 → L5 requires ALL other L4 → L5 complete (integration tests)
- v6-compat depends on receipt wrapper + schema generator
- Workspace update depends on v6-compat

**Concurrency**: 1 capsule at a time for sequential chains.

---

## Tracking Capsule Progress

### Status Values
- `pending`: Not started
- `in_progress`: Actively worked on
- `blocked`: Waiting on dependency
- `review`: Code complete, awaiting review
- `done`: Merged + receipts verified

### Receipt Verification
Each capsule generates receipts:
1. **Git commit**: Proof of code change
2. **Test results**: Proof tests pass
3. **Lint check**: Proof ESLint passes
4. **OTEL trace**: Proof execution succeeded

**Validation**:
```bash
# Check capsule completion
node scripts/validate-capsule.mjs V6-001
# Output: ✅ V6-001 COMPLETE (receipt hash: sha256:...)
```

---

## Risk Mitigation

### Risk 1: Scope Creep
**Mitigation**: Strict Δ scope in capsule definition. Reject PRs outside scope.

### Risk 2: Dependency Hell
**Mitigation**: Explicit dependency graph. Block capsules until deps complete.

### Risk 3: Developer Availability
**Mitigation**: Highly parallelizable design. Any dev can pick any capsule.

### Risk 4: Regression Bugs
**Mitigation**: Mandatory tests + OTEL validation ≥80/100 per capsule.

---

## 80/20 Fast-Track

**Hypothesis**: Core 10 packages = 80% of user value.

**Capsules**: V6-001 to V6-019 (19 capsules)
**Effort**: 196 hours (≈5 weeks for 5 devs)
**Release**: `v6.0.0-core` (subset release)

**Remaining 37**: Backfill over 3-6 months post-core release.

---

## Success Criteria

- [ ] All P0 capsules complete (Week 2)
- [ ] All P1 capsules complete (Week 8)
- [ ] Core 10 packages at L5 (Week 8)
- [ ] `v6.0.0-core` released (Week 9)
- [ ] All P2 capsules complete (Week 14)
- [ ] All P3 capsules complete (Week 35)
- [ ] `v6.0.0` stable released (Week 36)

**Final Validation**: OTEL ≥80/100 for ALL 47 packages.

---

## References

- **Migration Plan**: `/docs/v6/MIGRATION_PLAN.md`
- **Maturity Ladder**: `/docs/v6/MATURITY_LADDER.md`
- **BB80/20 Methodology**: `/docs/bb80-20-methodology.md`
