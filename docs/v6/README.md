# UNRDF V6 Migration - Complete Documentation

**Version**: 6.0.0-alpha.1
**Status**: Planning Phase Complete
**Last Updated**: 2025-12-27

## 📋 Documentation Index

### Core Documents
1. **[MIGRATION_PLAN.md](./MIGRATION_PLAN.md)** - Breaking changes, timelines, verification
2. **[MATURITY_LADDER.md](./MATURITY_LADDER.md)** - L1→L5 criteria, package assessments
3. **[CAPSULE_BACKLOG.md](./CAPSULE_BACKLOG.md)** - Work breakdown, priorities, dependencies

### Implementation
4. **[@unrdf/v6-compat](../../packages/v6-compat/)** - Compatibility layer package
   - API adapters (v5 → v6)
   - ESLint rules for deprecations
   - Zod schema generator

## 🚀 Quick Start

### For Users (Migration)

```bash
# Install compatibility layer
pnpm add @unrdf/v6-compat

# Use adapters
import { createStore, wrapWorkflow } from '@unrdf/v6-compat/adapters';

# Run migration tracker
import { migrationTracker } from '@unrdf/v6-compat/adapters';
// ... your app ...
migrationTracker.summary();
```

### For Contributors (Development)

```bash
# Check current maturity levels
cat docs/v6/MATURITY_LADDER.md

# Pick a capsule from backlog
cat docs/v6/CAPSULE_BACKLOG.md

# Follow migration plan
cat docs/v6/MIGRATION_PLAN.md
```

## 📊 Summary Statistics

### Breaking Changes
- **7 major breaking changes** identified
- **47 packages** to migrate
- **5 maturity levels** (L1 → L5)

### Current State
- **L1**: 47/47 packages (100%) - All compile and run
- **L2**: 12/47 packages (26%) - Stable contracts
- **L3**: 5/47 packages (11%) - Deterministic
- **L4**: 3/47 packages (6%) - Adversarial safety
- **L5**: 0/47 packages (0%) - Full composition

### Work Breakdown
- **P0**: 4 capsules (critical path) - 36 hours
- **P1**: 10 capsules (core packages) - 196 hours
- **P2**: 5 capsules (extended core) - 200 hours
- **P3**: 37 packages (batch) - 814 hours

**Total Effort**: ~1,246 hours (31 weeks for 5 devs, parallelized)

**Fast-Track (Core 10)**: ~196 hours (5 weeks for 5 devs)

## 🎯 Critical Path (Must Complete First)

1. **V6-001**: KGC-4D Receipt Wrapper HOF (8 hours)
2. **V6-002**: Zod Schema Generator (10 hours)
3. **V6-003**: @unrdf/v6-compat Package (16 hours)
4. **V6-004**: Workspace Update (2 hours)

**Total P0**: 36 hours (~1 week for 5 devs)

## 📈 Migration Timeline

### Phase 1: Alpha (Jan 2025)
- Core 10 packages at L5
- v6.0.0-alpha.1 released
- Compatibility layer functional

### Phase 2: Beta (Feb 2025)
- All 47 packages migrated
- Deprecation warnings active
- Production testing

### Phase 3: RC (Mar 2025)
- Feature freeze
- Bug fixes only
- Migration deadline announced

### Phase 4: Stable (Apr 2025)
- v6.0.0 official release
- v5.x security-only support

### Phase 5: EOL (Oct 2025)
- v5.x end-of-life
- v6.x standard support

## 🔍 Validation Checklist

Before declaring v6.0.0 stable:

- [ ] All 47 packages at L5
- [ ] 100% test pass rate
- [ ] OTEL validation ≥80/100 for all packages
- [ ] Zero direct N3 imports (outside justified modules)
- [ ] All operations produce receipts
- [ ] Documentation updated
- [ ] 3+ external users test migration

## 🛡️ Compatibility Approach

### API Adapters
- `createStore()` - v5 Store → v6 Oxigraph
- `wrapWorkflow()` - Add receipts to v5 workflows
- `wrapFederation()` - Add timeouts + typed queries
- `streamToAsync()` - EventEmitter → AsyncIterator
- `withReceipt()` - Wrap any function with receipts

### ESLint Rules
- `no-n3-imports` - Prevent direct N3 usage
- `no-workflow-run` - Require .execute() not .run()
- `require-zod-validation` - Enforce schema validation
- `require-timeout` - Require timeout guards
- `no-date-now` - Prevent non-deterministic code

### Migration Tracking
- Deprecation warnings with hints
- Migration report (count, APIs, suggestions)
- Receipt verification

## 📦 Package Structure

```
packages/v6-compat/
├── package.json          # v6.0.0-alpha.1
├── README.md             # Usage guide
└── src/
    ├── index.mjs         # Main exports
    ├── adapters.mjs      # API adapters (500+ LoC)
    ├── lint-rules.mjs    # ESLint plugin (300+ LoC)
    └── schema-generator.mjs  # Zod generator (200+ LoC)
```

**Total**: ~1,000 LoC for compatibility layer

## 🎓 Next Steps

1. **Review documentation** (this folder)
2. **Pick P0 capsule** from CAPSULE_BACKLOG.md
3. **Implement + test** following maturity ladder
4. **Generate receipt** for completion proof
5. **Repeat** for next capsule

## 🔗 References

- **BB80/20 Methodology**: `/docs/bb80-20-methodology.md`
- **CLAUDE.md**: `/CLAUDE.md` (execution patterns)
- **Package Index**: `/packages/` (47 packages)

---

**Questions?** See MIGRATION_PLAN.md or open an issue.
