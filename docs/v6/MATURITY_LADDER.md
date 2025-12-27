# UNRDF V6 Maturity Ladder

**Purpose**: Define concrete, measurable maturity levels for all 47 UNRDF packages.

**Principle**: Each level builds on the previous. A package cannot be L3 without satisfying L1 + L2.

---

## Level Definitions

### L1: Compiles, Runs, Minimal Examples

**Criteria**:
- [ ] Package builds without errors (`pnpm build`)
- [ ] At least 1 working example in `examples/` directory
- [ ] Basic `README.md` with installation + usage
- [ ] Main exports are importable and executable
- [ ] Tests exist (≥1 passing test)

**Verification**:
```bash
cd packages/<name>
timeout 5s pnpm build && echo "✅ L1: Builds"
timeout 5s pnpm test && echo "✅ L1: Tests pass"
timeout 5s node examples/basic.mjs && echo "✅ L1: Example runs"
```

**Current L1 Packages** (47/47):
All packages compile and run. No blockers.

---

### L2: Stable Public Contracts

**Criteria**:
- [ ] All public APIs documented with JSDoc (100% coverage)
- [ ] Zod schemas for all inputs/outputs
- [ ] Semantic versioning followed strictly
- [ ] Breaking changes listed in CHANGELOG.md
- [ ] CLI args stable (if applicable) with `--help` docs
- [ ] Receipt schemas defined (if applicable)

**Verification**:
```bash
# Check JSDoc coverage
timeout 5s grep -r "@param\|@returns" src/ --include="*.mjs" | wc -l

# Check Zod schemas exist
timeout 5s grep -r "z\.object\|z\.string" src/ --include="*.mjs" | wc -l

# Check receipts
timeout 5s grep -r "receiptSchema" src/ --include="*.mjs" | wc -l
```

**Current L2 Packages** (12/47):
- `@unrdf/oxigraph` ✅
- `@unrdf/core` ✅
- `@unrdf/hooks` ✅
- `@unrdf/kgc-4d` ✅
- `@unrdf/yawl` ✅
- `@unrdf/streaming` ✅
- `@unrdf/federation` ✅
- `@unrdf/cli` ✅
- `@unrdf/knowledge-engine` (partial)
- `@unrdf/project-engine` (partial)
- `@unrdf/observability` (partial)
- `@unrdf/validation` (partial)

**Blockers for remaining 35**:
- Missing Zod schemas (20 packages)
- Incomplete JSDoc coverage (15 packages)
- No receipt schemas (18 packages)

---

### L3: Deterministic Outputs + Replayability

**Criteria**:
- [ ] Same input → same output (no hidden state)
- [ ] All operations produce receipts (KGC-4D)
- [ ] Receipts are reproducible (fixtures in `test/fixtures/`)
- [ ] No Date.now() / Math.random() in business logic
- [ ] All side effects isolated (I/O, network, filesystem)

**Verification**:
```bash
# Run twice, compare outputs
timeout 5s node examples/deterministic-test.mjs > output1.json
timeout 5s node examples/deterministic-test.mjs > output2.json
diff output1.json output2.json && echo "✅ L3: Deterministic"

# Check for non-deterministic calls
timeout 5s grep -r "Date\.now\|Math\.random" src/ --include="*.mjs" | wc -l
# Should be 0
```

**Current L3 Packages** (5/47):
- `@unrdf/kgc-4d` ✅ (Receipt engine itself)
- `@unrdf/oxigraph` ✅ (Pure SPARQL execution)
- `@unrdf/yawl` ✅ (Workflow receipts)
- `@unrdf/core` (partial - some utils use Date.now)
- `@unrdf/hooks` (partial - receipts exist but not enforced)

**Blockers for remaining 42**:
- No receipt integration (30 packages)
- Non-deterministic utils (12 packages)
- Missing reproducible fixtures (40 packages)

---

### L4: Adversarial Misuse Safety (Poka-Yoke)

**Criteria**:
- [ ] Invalid inputs rejected with clear errors (no silent failures)
- [ ] Timeout guards on all I/O operations (default 5s)
- [ ] Resource limits enforced (memory, CPU, file size)
- [ ] No SQL/SPARQL injection vectors
- [ ] No eval() or Function() constructor usage
- [ ] Zod validation on ALL external inputs

**Verification**:
```bash
# Test invalid inputs
timeout 5s node test/adversarial/invalid-inputs.test.mjs
# All should throw with descriptive errors

# Check for eval usage (should be 0)
timeout 5s grep -r "eval\|Function(" src/ --include="*.mjs" | wc -l

# Check timeout enforcement
timeout 5s grep -r "timeout.*5[0-9]{3}" src/ --include="*.mjs" | wc -l
```

**Current L4 Packages** (3/47):
- `@unrdf/kgc-4d` ✅ (Hardened receipt validation)
- `@unrdf/federation` (partial - timeout guards exist)
- `@unrdf/streaming` (partial - Zod validation exists)

**Blockers for remaining 44**:
- No timeout guards (35 packages)
- Missing input validation (28 packages)
- No resource limits (40 packages)

---

### L5: Cross-Package Compositional Closure

**Criteria**:
- [ ] Works with ALL other L5 packages (integration tests)
- [ ] Receipts compose (chain of custody preserved)
- [ ] No circular dependencies
- [ ] Shared context propagates correctly (hooks, OTEL spans)
- [ ] Performance benchmarks exist and pass regression tests

**Verification**:
```bash
# Run integration tests
timeout 30s pnpm test:integration

# Check circular deps (should be 0)
timeout 5s pnpm -r exec madge --circular src/

# Benchmark regression
timeout 10s pnpm benchmark:compare
# No regressions >10%
```

**Current L5 Packages** (0/47):
None yet. This is the V6 goal.

**Blockers**:
- All packages must reach L4 first
- Integration test suite incomplete (est. 200+ tests needed)
- Receipt composition not implemented (needs KGC-4D v6 core)

---

## Current Maturity Distribution

| Level | Count | Percentage | Packages |
|-------|-------|------------|----------|
| L1    | 47    | 100%       | All packages |
| L2    | 12    | 26%        | Core + YAWL + Hooks + Federation + Streaming + CLI |
| L3    | 5     | 11%        | KGC-4D, Oxigraph, YAWL, Core (partial), Hooks (partial) |
| L4    | 3     | 6%         | KGC-4D, Federation (partial), Streaming (partial) |
| L5    | 0     | 0%         | (Target for v6.0.0 stable) |

**Average Maturity**: L1.4 (weighted)

---

## Required Work to Reach L5

### Phase 1: L1 → L2 (35 packages)
**Effort**: 3-5 days per package (175 days total, parallelizable)

**Tasks per package**:
1. Add JSDoc to all public functions (2-3 hours)
2. Create Zod schemas for inputs/outputs (3-4 hours)
3. Define receipt schemas (2-3 hours)
4. Write CHANGELOG.md (1 hour)
5. Add CLI `--help` docs if applicable (1-2 hours)

**80/20 Optimization**: Use schema generator + templates.

---

### Phase 2: L2 → L3 (42 packages)
**Effort**: 5-7 days per package (294 days total, parallelizable)

**Tasks per package**:
1. Integrate KGC-4D receipts (4-6 hours)
2. Remove non-deterministic calls (2-3 hours)
3. Create reproducible fixtures (6-8 hours)
4. Add deterministic tests (4-5 hours)
5. Document replay procedures (2-3 hours)

**80/20 Optimization**: Receipt wrapper HOF, fixture generator.

---

### Phase 3: L3 → L4 (44 packages)
**Effort**: 4-6 days per package (264 days total, parallelizable)

**Tasks per package**:
1. Add timeout guards to all I/O (3-4 hours)
2. Add Zod validation to external inputs (2-3 hours)
3. Remove eval/Function usage (1-2 hours)
4. Add resource limits (2-3 hours)
5. Write adversarial tests (4-6 hours)

**80/20 Optimization**: Timeout HOF, validation middleware.

---

### Phase 4: L4 → L5 (47 packages)
**Effort**: 7-10 days per package (470 days total, sequential)

**Tasks per package**:
1. Write integration tests with all L5 packages (6-8 hours)
2. Fix circular dependencies (2-4 hours)
3. Implement receipt composition (4-6 hours)
4. Add performance benchmarks (3-4 hours)
5. Run regression tests (2-3 hours)

**80/20 Optimization**: Integration test matrix, automated benchmarking.

---

## Estimated Timeline (Parallelized)

**Assuming 5 developers working concurrently**:

| Phase | Duration | Packages/Week | Completion |
|-------|----------|---------------|------------|
| L1→L2 | 7 weeks  | 5 packages    | Week 7     |
| L2→L3 | 12 weeks | 3-4 packages  | Week 19    |
| L3→L4 | 11 weeks | 4 packages    | Week 30    |
| L4→L5 | 15 weeks | 3 packages    | Week 45    |

**Total**: ~45 weeks (11 months) to L5 for all packages.

**Fast-Track (10 core packages only)**: ~12 weeks (3 months).

---

## Package-Specific Assessments

### Tier 1: Core (Target L5 by Week 12)

#### @unrdf/oxigraph
- **Current**: L3
- **Blockers**: L4 (timeout guards), L5 (integration tests)
- **Effort**: 10 days

#### @unrdf/core
- **Current**: L2 (partial L3)
- **Blockers**: Deterministic utils, receipt integration
- **Effort**: 14 days

#### @unrdf/kgc-4d
- **Current**: L4
- **Blockers**: L5 (integration tests only)
- **Effort**: 7 days

#### @unrdf/v6-compat
- **Current**: L0 (not yet created)
- **Blockers**: Full implementation
- **Effort**: 10 days

---

### Tier 2: Essential (Target L5 by Week 24)

#### @unrdf/hooks
- **Current**: L2 (partial L3)
- **Blockers**: Receipt enforcement, fixtures
- **Effort**: 12 days

#### @unrdf/streaming
- **Current**: L2 (partial L4)
- **Blockers**: Deterministic fixtures, integration tests
- **Effort**: 13 days

#### @unrdf/federation
- **Current**: L2 (partial L4)
- **Blockers**: Receipt composition, benchmarks
- **Effort**: 14 days

#### @unrdf/cli
- **Current**: L2
- **Blockers**: Zod validation, receipts
- **Effort**: 10 days

---

### Tier 3-6: Remaining 39 Packages
**Current**: L1 (most), L2 (some)
**Effort**: 15-25 days each (avg 18 days)
**Total**: 702 developer-days (~140 weeks / 5 devs = 28 weeks)

---

## Accelerated Path: Core-Only V6

**Hypothesis**: 80% of users only need 10 core packages.

**Packages**:
1. @unrdf/oxigraph
2. @unrdf/core
3. @unrdf/kgc-4d
4. @unrdf/v6-compat
5. @unrdf/hooks
6. @unrdf/streaming
7. @unrdf/federation
8. @unrdf/cli
9. @unrdf/yawl
10. @unrdf/knowledge-engine

**Timeline**: 12 weeks (3 months) to L5 for these 10.

**Release**: `v6.0.0-core` (subset release, full v6 follows later).

---

## Validation Checklist

Before declaring a package at level X:

### L2 Checklist
- [ ] JSDoc coverage ≥95% (run coverage tool)
- [ ] Zod schemas exist for all inputs/outputs
- [ ] Receipt schema defined (if applicable)
- [ ] CHANGELOG.md exists and is up-to-date
- [ ] `pnpm lint` passes with 0 warnings

### L3 Checklist
- [ ] Deterministic test passes 10 consecutive runs
- [ ] No `Date.now()` or `Math.random()` in business logic
- [ ] Receipts generated and match fixtures
- [ ] Replay test passes (given receipt, reproduce output)

### L4 Checklist
- [ ] All I/O operations have timeout guards (default 5s)
- [ ] Adversarial input tests pass (100+ invalid inputs)
- [ ] No eval or Function constructor usage
- [ ] Zod validation on ALL external inputs
- [ ] Resource limits enforced (tested with large inputs)

### L5 Checklist
- [ ] Integration tests with all other L5 packages pass
- [ ] No circular dependencies (madge check)
- [ ] Receipt composition works (chain of custody)
- [ ] Performance benchmarks pass regression tests (<10% slower)
- [ ] OTEL validation ≥80/100

---

## Next Steps

1. **Create L2 templates**: Auto-generate JSDoc + Zod schemas (1 week)
2. **Build receipt wrapper**: HOF to add receipts to existing functions (1 week)
3. **Implement v6-compat**: Migration bridge package (2 weeks)
4. **Fast-track core 10**: Parallel work on Tier 1-2 packages (12 weeks)
5. **Release v6-core**: Subset release for early adopters (Week 15)
6. **Backfill remaining**: Gradually promote L1→L5 over 6 months

---

## References

- **Migration Plan**: `/docs/v6/MIGRATION_PLAN.md`
- **Capsule Backlog**: `/docs/v6/CAPSULE_BACKLOG.md`
- **BB80/20 Methodology**: `/docs/bb80-20-methodology.md`
