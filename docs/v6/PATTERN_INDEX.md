# v6 Pattern Library - Quick Reference Index

**Version**: 6.0.0-alpha.1
**Last Updated**: 2025-12-27

## Documentation Map

This index helps you navigate the v6 pattern library documentation.

### 📚 Main Documents

| Document | Purpose | When to Use |
|----------|---------|-------------|
| [PATTERNS.md](./PATTERNS.md) | Complete pattern specifications | Reference implementation details |
| [PATTERN_TUTORIALS.md](./PATTERN_TUTORIALS.md) | Step-by-step learning guides | Learn patterns hands-on |
| [MIGRATION_RUNBOOKS.md](./MIGRATION_RUNBOOKS.md) | Package migration procedures | Migrate P1 packages to v6 |
| [RESEARCH_FINDINGS_SUMMARY.md](./RESEARCH_FINDINGS_SUMMARY.md) | Research results & metrics | Understand research process |

### 🎯 Quick Access

#### I want to...

**Learn a pattern from scratch**
→ [PATTERN_TUTORIALS.md](./PATTERN_TUTORIALS.md)
- Tutorial 1: Receipt HOF (15 min)
- Tutorial 2: Delta Contract (20 min)
- Tutorial 3: Zod Validation (15 min)
- Tutorial 4: Determinism (25 min)
- Tutorial 5: Composition (30 min)

**Look up pattern implementation details**
→ [PATTERNS.md](./PATTERNS.md)
- Receipt HOF Pattern (template + examples)
- Delta Contract Pattern (SPARQL examples)
- Zod Validation Envelope (composable validators)
- Determinism Proof Pattern (100x testing)
- Composition Layer Pattern (L5 criteria)

**Migrate a specific package**
→ [MIGRATION_RUNBOOKS.md](./MIGRATION_RUNBOOKS.md)
- @unrdf/oxigraph (20h)
- @unrdf/core (24h)
- @unrdf/kgc-4d (16h)
- @unrdf/hooks (18h)
- ... 6 more packages

**Understand research methodology**
→ [RESEARCH_FINDINGS_SUMMARY.md](./RESEARCH_FINDINGS_SUMMARY.md)
- Pattern extraction process
- Codebase analysis (63 packages, 150+ files)
- Documentation metrics (66KB, 2,600 lines)
- Time savings estimates (48% reduction)

---

## Pattern Quick Reference

### Receipt HOF Pattern

**One-Liner**: Wrap any function to generate cryptographic receipts.

```javascript
import { withReceipt } from '@unrdf/v6-compat/adapters';

const fn = withReceipt(async (x) => x * 2, { operation: 'double' });
const { result, receipt } = await fn(21); // { result: 42, receipt: {...} }
```

**When**: State-changing operations, L3+ packages
**File**: `/home/user/unrdf/packages/v6-compat/src/adapters.mjs`
**Tutorial**: [Tutorial 1](./PATTERN_TUTORIALS.md#tutorial-1-add-receipt-to-your-first-operation-15-min)

---

### Delta Contract Pattern

**One-Liner**: All state changes must flow through explicit Delta proposals.

```javascript
import { createDelta, DeltaGate } from '@unrdf/v6-core/delta';

const delta = createDelta('add', 'http://ex.org/s', 'http://ex.org/p', 'value');
const gate = new DeltaGate();
const receipt = await gate.proposeDelta(delta, store);
```

**When**: RDF mutations (mandatory), workflow transitions, policy enforcement
**File**: `/home/user/unrdf/packages/v6-core/src/delta/`
**Tutorial**: [Tutorial 2](./PATTERN_TUTORIALS.md#tutorial-2-create-and-apply-a-delta-20-min)

---

### Zod Validation Envelope Pattern

**One-Liner**: Validate inputs/outputs at module boundaries.

```javascript
import { z } from 'zod';

const Schema = z.object({ id: z.string().uuid(), value: z.number() });

function process(data) {
  const validated = Schema.parse(data); // Throws on invalid
  return validated;
}
```

**When**: Public APIs, data ingestion, inter-package communication
**File**: Throughout codebase (9+ packages)
**Tutorial**: [Tutorial 3](./PATTERN_TUTORIALS.md#tutorial-3-validate-inputs-with-zod-15-min)

---

### Determinism Proof Pattern

**One-Liner**: Guarantee identical outputs for identical inputs (100x test).

```javascript
import { deterministicSerialize, computeBlake3 } from '@unrdf/v6-core/receipts/base-receipt';

const hash = await computeBlake3(deterministicSerialize(data));
// Run 100x → all hashes identical
```

**When**: L3+ packages (required), receipts, snapshots, tests
**File**: `/home/user/unrdf/packages/v6-core/src/receipts/base-receipt.mjs`
**Tutorial**: [Tutorial 4](./PATTERN_TUTORIALS.md#tutorial-4-prove-determinism-25-min)

---

### Composition Layer Pattern (L5)

**One-Liner**: Compose packages via schema compatibility checking.

```javascript
class CompositionChecker {
  canCompose(moduleA, moduleB) {
    const testData = generateTestData(moduleA.outputSchema);
    return moduleB.inputSchema.safeParse(testData).success;
  }
}
```

**When**: Cross-package workflows, L5 maturity, federated systems
**File**: Inferred from `/home/user/unrdf/docs/v6/PROGRAM_CHARTER.md`
**Tutorial**: [Tutorial 5](./PATTERN_TUTORIALS.md#tutorial-5-compose-cross-package-workflows-30-min)

---

## Pattern Compatibility Matrix

|  | Receipt HOF | Delta Contract | Zod Validation | Determinism Proof | Composition Layer |
|--|-------------|----------------|----------------|-------------------|-------------------|
| **Receipt HOF** | — | ✅ | ✅ | ✅ | ✅ |
| **Delta Contract** | ✅ | — | ✅ | ✅ | ✅ |
| **Zod Validation** | ✅ | ✅ | — | ✅ | ✅ |
| **Determinism Proof** | ✅ | ✅ | ✅ | — | ✅ |
| **Composition Layer** | ✅ | ✅ | ✅ | ✅ | — |

**Result**: All patterns compose (100% compatibility)

---

## Migration Roadmap

### P0: Foundation (Complete ✅)
- v6-compat: Receipt HOF
- v6-core: Delta Contract + Zod
- kgc-4d: Determinism

### P1: Core Packages (196 hours)
- [ ] @unrdf/oxigraph (20h)
- [ ] @unrdf/core (24h)
- [ ] @unrdf/kgc-4d (16h)
- [ ] @unrdf/hooks (18h)
- [ ] @unrdf/streaming (20h)
- [ ] @unrdf/federation (22h)
- [ ] @unrdf/cli (16h)
- [ ] @unrdf/yawl (24h)
- [ ] @unrdf/knowledge-engine (18h)
- [ ] @unrdf/graph-analytics (18h)

### P2: Extended Core (200 hours)
- 5 packages (planned)

### P3: Batch Migration (814 hours)
- 37 remaining packages

**Total**: 1,246 hours → 48% savings with patterns = **653 hours effective**

---

## Common Tasks

### How do I...

**Add receipts to my operation?**
```javascript
import { withReceipt } from '@unrdf/v6-compat/adapters';
const wrapped = withReceipt(myFunction, { operation: 'my.op' });
const { result, receipt } = await wrapped(args);
```

**Create a delta for RDF mutation?**
```javascript
import { createDelta } from '@unrdf/v6-core/delta';
const delta = createDelta('add', subject, predicate, object, {
  package: '@unrdf/my-package',
  actor: 'user-id'
});
```

**Validate input with Zod?**
```javascript
import { z } from 'zod';
const Schema = z.object({ id: z.string().uuid() });
const validated = Schema.parse(input); // Throws if invalid
```

**Prove determinism?**
```javascript
// Run 100 times
const hashes = await Promise.all(
  Array.from({ length: 100 }, () => computeBlake3(data))
);
console.log(new Set(hashes).size); // Should be 1
```

**Check if two modules can compose?**
```javascript
const checker = new CompositionChecker();
const compatible = checker.canCompose(moduleA, moduleB);
console.log(compatible); // true if A.output matches B.input
```

---

## Testing Checklists

### Receipt HOF
- [ ] Receipt contains all required fields (timestamp, duration, operation)
- [ ] Async functions work
- [ ] Error handling preserves stack traces

### Delta Contract
- [ ] Delta validates against DeltaSchema
- [ ] Receipts generated for all outcomes (success/failure)
- [ ] All-or-none atomicity enforced

### Zod Validation
- [ ] Invalid inputs throw ZodError
- [ ] Error messages are readable
- [ ] Schemas compose (input → output)

### Determinism Proof
- [ ] 100x runs produce identical hashes
- [ ] Key ordering deterministic (sorted)
- [ ] Timestamps deterministic in DETERMINISTIC=1 mode

### Composition Layer
- [ ] Output schema of A matches input schema of B
- [ ] Receipt chains span packages
- [ ] Compatibility matrix updated

---

## File Locations

### Pattern Implementations

| Pattern | Location |
|---------|----------|
| Receipt HOF | `/home/user/unrdf/packages/v6-compat/src/adapters.mjs` |
| Delta Contract | `/home/user/unrdf/packages/v6-core/src/delta/` |
| Zod Validation | Throughout (9+ packages) |
| Determinism Proof | `/home/user/unrdf/packages/v6-core/src/receipts/base-receipt.mjs` |
| Composition Layer | Inferred from charter |

### Documentation

| Document | Location |
|----------|----------|
| Pattern Library | `/home/user/unrdf/docs/v6/PATTERNS.md` |
| Tutorials | `/home/user/unrdf/docs/v6/PATTERN_TUTORIALS.md` |
| Runbooks | `/home/user/unrdf/docs/v6/MIGRATION_RUNBOOKS.md` |
| Research Summary | `/home/user/unrdf/docs/v6/RESEARCH_FINDINGS_SUMMARY.md` |

---

## Next Steps

### For Learners
1. Start with [Tutorial 1](./PATTERN_TUTORIALS.md#tutorial-1-add-receipt-to-your-first-operation-15-min)
2. Complete all 5 tutorials (~2 hours total)
3. Apply patterns to a real package

### For Migrators
1. Read runbook for your package
2. Follow step-by-step commands
3. Run testing checklist
4. Submit PR with receipt proofs

### For Architects
1. Review [Pattern Library](./PATTERNS.md)
2. Study [Composition Layer](./PATTERNS.md#composition-layer-pattern-l5)
3. Update [Compatibility Matrix](./PATTERNS.md#pattern-compatibility-matrix)

---

## References

- **v6 Program Charter**: [/docs/v6/PROGRAM_CHARTER.md](/home/user/unrdf/docs/v6/PROGRAM_CHARTER.md)
- **Migration Plan**: [/docs/v6/MIGRATION_PLAN.md](/home/user/unrdf/docs/v6/MIGRATION_PLAN.md)
- **Maturity Ladder**: [/docs/v6/MATURITY_LADDER.md](/home/user/unrdf/docs/v6/MATURITY_LADDER.md)
- **BB80/20 Methodology**: [/docs/bb80-20-methodology.md](/home/user/unrdf/docs/bb80-20-methodology.md)

---

**Quick Index Version**: 1.0.0
**Last Updated**: 2025-12-27
**Maintained By**: Researcher Agent
