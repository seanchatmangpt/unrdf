# UNRDF Vitest Testing Standard - Documentation Index

> **Comprehensive testing standard for all 37 UNRDF subprojects**

## 🚀 Quick Start (5 minutes)

**New to UNRDF testing?** Start here:

1. Read: [Quick Reference](./VITEST_QUICK_REFERENCE.md) (5 min)
2. Copy: Template for your package type
3. Write: Your first test
4. Run: `pnpm test`

## 📚 Documentation Suite

### 🎯 [Executive Summary](./VITEST_SUMMARY.md)
**File:** VITEST_SUMMARY.md | **Size:** 7KB | **Time:** 10 min

**Quick overview of the entire standard:**
- What was delivered (4 comprehensive guides)
- Technical architecture overview
- Scope (11 packages + 26 examples)
- Implementation timeline (3-5 days)
- Success criteria and metrics

**Best for:** Project managers, stakeholders, quick overview

---

### 📖 [Vitest Standard](./VITEST_STANDARD.md)
**File:** VITEST_STANDARD.md | **Size:** 23KB | **Time:** 30 min

**Complete testing reference guide:**
- ✅ Configuration templates (Node.js, jsdom, examples)
- ✅ Test file structure and organization
- ✅ Test categories (unit, integration, example, error)
- ✅ Assertion patterns and best practices
- ✅ Coverage standards (80% core, 70% examples)
- ✅ Package-specific standards
- ✅ Anti-patterns to avoid
- ✅ Setup/cleanup patterns
- ✅ Migration guide

**Best for:** All developers, comprehensive reference

---

### 🔍 [Quick Reference](./VITEST_QUICK_REFERENCE.md)
**File:** VITEST_QUICK_REFERENCE.md | **Size:** latestKB | **Time:** 5 min

**Essential information at a glance:**
- ⚡ Quick start (3 steps)
- 📋 Configuration templates
- 🎯 Common assertions cheat sheet
- 🏃 Running tests commands
- 📊 Coverage thresholds
- ✅ New package checklist

**Best for:** Quick answers, daily reference

---

### 🏗️ [Architecture Guide](./VITEST_ARCHITECTURE.md)
**File:** VITEST_ARCHITECTURE.md | **Size:** 37KB | **Time:** 45 min

**System design and architecture:**
- 🏛️ System architecture diagram
- 🔄 Test execution flow
- 🧬 Test lifecycle visualization
- 📊 Coverage architecture
- 📦 Package hierarchy
- 🔗 Configuration inheritance
- 📈 Reporting architecture
- ⚡ Execution optimization
- 📈 Scaling strategy

**Best for:** System architects, technical leads

---

### 🗺️ [Implementation Roadmap](./VITEST_IMPLEMENTATION_ROADMAP.md)
**File:** VITEST_IMPLEMENTATION_ROADMAP.md | **Size:** 14KB | **Time:** 20 min

**Step-by-step implementation plan:**
- 📅 5-phase implementation plan
- ✅ Day-by-day task breakdown
- 📝 Core package migration checklist (11 packages)
- 📦 Example project migration checklist (26 projects)
- ✔️ Validation & QA procedures
- 🔀 Parallel execution strategy
- 📊 Success metrics
- ⚠️ Risk mitigation

**Best for:** Project managers, implementation team

---

## 🎯 Choose Your Path

### 👤 I'm a Developer

**Goal:** Write tests for my package

1. **Quick Start:** [Quick Reference](./VITEST_QUICK_REFERENCE.md) (5 min)
2. **Copy Template:** See "Configuration Templates" section
3. **Write Tests:** Follow test file structure
4. **Verify:** Run `pnpm test:coverage`
5. **Deep Dive:** [Vitest Standard](./VITEST_STANDARD.md) (optional)

### 🏗️ I'm an Architect

**Goal:** Understand system design

1. **Architecture:** [Architecture Guide](./VITEST_ARCHITECTURE.md) (45 min)
2. **Standards:** [Vitest Standard](./VITEST_STANDARD.md) (30 min)
3. **Implementation:** [Implementation Roadmap](./VITEST_IMPLEMENTATION_ROADMAP.md) (20 min)
4. **Summary:** [Executive Summary](./VITEST_SUMMARY.md) (10 min)

### 📊 I'm a Project Manager

**Goal:** Track implementation progress

1. **Summary:** [Executive Summary](./VITEST_SUMMARY.md) (10 min)
2. **Roadmap:** [Implementation Roadmap](./VITEST_IMPLEMENTATION_ROADMAP.md) (20 min)
3. **Track Progress:** Use roadmap checklists
4. **Reference:** [Quick Reference](./VITEST_QUICK_REFERENCE.md) (5 min)

### 🚀 I'm Implementing This

**Goal:** Migrate all packages

1. **Roadmap:** [Implementation Roadmap](./VITEST_IMPLEMENTATION_ROADMAP.md) (20 min)
2. **Standards:** [Vitest Standard](./VITEST_STANDARD.md) (30 min)
3. **Execute:** Follow phase-by-phase checklist
4. **Validate:** Run tests and verify coverage
5. **Reference:** [Quick Reference](./VITEST_QUICK_REFERENCE.md) (ongoing)

## 📊 Package Coverage Matrix

| Package | Environment | Coverage Target | Status | Tests |
|---------|-------------|----------------|--------|-------|
| @unrdf/core | node | 80/80/75/80 | ⏳ | TBD |
| @unrdf/hooks | node | 80/80/75/80 | ⏳ | TBD |
| @unrdf/browser | jsdom | 80/80/75/80 | ⏳ | TBD |
| @unrdf/streaming | node | 80/80/75/80 | ⏳ | TBD |
| @unrdf/composables | jsdom | 80/80/75/80 | ⏳ | TBD |
| @unrdf/federation | node | 80/80/75/80 | ⏳ | TBD |
| @unrdf/knowledge-engine | node | 80/80/75/80 | ⏳ | TBD |
| @unrdf/dark-matter | node | 80/80/75/80 | ⏳ | TBD |
| @unrdf/cli | node | 80/80/75/80 | ⏳ | TBD |
| @unrdf/project-engine | node | 80/80/75/80 | ⏳ | TBD |
| Examples (26) | mixed | 70/70/60/70 | ⏳ | TBD |

**Legend:**
- ✅ Complete (tests passing, coverage met)
- ⏳ Pending (not yet migrated)
- ⚠️ In Progress (migration underway)
- ❌ Failed (tests failing or coverage below threshold)

## 🎓 Learning Resources

### Templates

**Node.js Package Template:**
```javascript
import { defineConfig } from 'vitest/config';
export default defineConfig({
  test: {
    environment: 'node',
    globals: true,
    coverage: {
      thresholds: { lines: 80, functions: 80, branches: 75, statements: 80 }
    }
  }
});
```

**jsdom Package Template:**
```javascript
import { defineConfig } from 'vitest/config';
export default defineConfig({
  test: {
    environment: 'jsdom', // ← Only difference
    globals: true,
    coverage: {
      thresholds: { lines: 80, functions: 80, branches: 75, statements: 80 }
    }
  }
});
```

**Example Template:**
```javascript
import { defineConfig } from 'vitest/config';
export default defineConfig({
  test: {
    environment: 'node',
    globals: true,
    coverage: {
      thresholds: { lines: 70, functions: 70, branches: 60, statements: 70 }
    }
  }
});
```

### Example Test

```javascript
/**
 * @fileoverview @unrdf/core - Store Operations Test Suite
 * @vitest-environment node
 */
import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, addQuad } from '../src/index.mjs';

describe('@unrdf/core - Store', () => {
  let store;

  beforeEach(() => {
    store = createStore();
  });

  it('should create empty store', () => {
    expect(store).toBeDefined();
  });

  it('should add quad', () => {
    const quad = { subject: 's', predicate: 'p', object: 'o' };
    addQuad(store, quad);
    expect(store.size).toBe(1);
  });
});
```

## 🛠️ Common Commands

```bash
# Run tests
pnpm test

# Run with coverage
pnpm test:coverage

# Watch mode
pnpm test:watch

# UI mode
pnpm test:ui

# Specific package
pnpm --filter @unrdf/core test

# All packages
pnpm -r test

# CI mode
pnpm test:ci
```

## 📈 Implementation Status

### Phase 1: Foundation ✅ COMPLETE
- [x] VITEST_STANDARD.md created (23KB)
- [x] VITEST_QUICK_REFERENCE.md created (latestKB)
- [x] VITEST_ARCHITECTURE.md created (37KB)
- [x] VITEST_IMPLEMENTATION_ROADMAP.md created (14KB)
- [x] VITEST_SUMMARY.md created (7KB)
- [x] VITEST_INDEX.md created (this file)
- [x] Stored in memory: `unrdf/vitest/standard`

### Phase 2: Core Packages ⏳ PENDING
- [ ] Migrate 11 core packages
- [ ] Run tests and verify coverage
- [ ] Commit changes

### Phase 3: Examples ⏳ PENDING
- [ ] Migrate 26 example projects
- [ ] Run tests and verify coverage
- [ ] Commit changes

### Phase 4: Validation ⏳ PENDING
- [ ] Run full test suite
- [ ] Verify coverage across all packages
- [ ] CI/CD integration

### Phase 5: Monitoring ⏳ PENDING
- [ ] Weekly test runs
- [ ] Coverage tracking
- [ ] Performance monitoring

## 🤝 Contributing

### Adding Tests to Existing Package

1. Follow patterns in existing tests
2. Maintain coverage threshold
3. Use AAA pattern (Arrange-Act-Assert)
4. Add descriptive test names
5. Run `pnpm test:coverage` before committing

### Creating New Package

1. Copy appropriate template
2. Create `test/` directory
3. Add test scripts to `package.json`
4. Write initial tests
5. Verify coverage ≥ 80% (or 70% for examples)
6. See: [Vitest Standard - New Package Checklist](./VITEST_STANDARD.md#new-package-checklist)

### Reporting Issues

- Test failures: Check environment directive
- Coverage issues: Add more tests
- Performance issues: Check single-threaded config
- Documentation issues: Update relevant doc file

## 📞 Support

### Documentation
- Comprehensive: [Vitest Standard](./VITEST_STANDARD.md)
- Quick: [Quick Reference](./VITEST_QUICK_REFERENCE.md)
- Visual: [Architecture Guide](./VITEST_ARCHITECTURE.md)
- Planning: [Implementation Roadmap](./VITEST_IMPLEMENTATION_ROADMAP.md)

### Resources
- [Vitest Official Docs](https://vitest.dev)
- [UNRDF GitHub](https://github.com/unrdf/unrdf)
- [Testing Guide](./TESTING.md)
- [Contributing Guide](./CONTRIBUTING.md)

### Memory
- Key: `unrdf/vitest/standard`
- Command: `npx claude-flow@alpha hooks memory-get --key "unrdf/vitest/standard"`

## 🎯 Success Metrics

### Documentation
- ✅ Total Size: 87KB (6 files)
- ✅ Completeness: 100%
- ✅ Accessibility: docs/ + memory
- ✅ Maintenance: Single source of truth

### Implementation
- ✅ Phase 1: Complete (Foundation)
- ⏳ Phase 2: Pending (Core Packages)
- ⏳ Phase 3: Pending (Examples)
- ⏳ Phase 4: Pending (Validation)
- ⏳ Phase 5: Pending (Monitoring)

### Quality
- Target: 80% coverage (core)
- Target: 70% coverage (examples)
- Target: 100% test pass rate
- Target: 0 flaky tests

## 🔗 File Navigation

### All Documentation Files

1. **[VITEST_INDEX.md](./VITEST_INDEX.md)** (this file) - Documentation index
2. **[VITEST_SUMMARY.md](./VITEST_SUMMARY.md)** - Executive summary
3. **[VITEST_STANDARD.md](./VITEST_STANDARD.md)** - Complete standard
4. **[VITEST_QUICK_REFERENCE.md](./VITEST_QUICK_REFERENCE.md)** - Quick reference
5. **[VITEST_ARCHITECTURE.md](./VITEST_ARCHITECTURE.md)** - Architecture guide
6. **[VITEST_IMPLEMENTATION_ROADMAP.md](./VITEST_IMPLEMENTATION_ROADMAP.md)** - Implementation plan

### Related Documentation

- [README.md](../README.md) - Project overview
- [CONTRIBUTING.md](./CONTRIBUTING.md) - Contributing guide
- [ARCHITECTURE.md](./ARCHITECTURE.md) - System architecture
- [TESTING.md](./TESTING.md) - Testing overview

---

## 🎉 Ready to Start?

**Choose your next step:**

- 👨‍💻 **Developer:** [Quick Reference](./VITEST_QUICK_REFERENCE.md)
- 🏗️ **Architect:** [Architecture Guide](./VITEST_ARCHITECTURE.md)
- 📊 **Manager:** [Executive Summary](./VITEST_SUMMARY.md)
- 🚀 **Implementer:** [Implementation Roadmap](./VITEST_IMPLEMENTATION_ROADMAP.md)

---

**Index Version:** latest
**Last Updated:** 2025-12-04
**Status:** Phase 1 Complete
**Next:** Begin Phase 2 (Core Package Migration)
