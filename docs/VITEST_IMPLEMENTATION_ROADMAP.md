# Vitest Implementation Roadmap for UNRDF

## 📋 Executive Summary

**Objective:** Standardize Vitest testing across all 37 UNRDF subprojects (11 packages + 26 examples)

**Timeline:** 3-5 days with parallel agent execution

**Success Criteria:**
- ✅ All packages have standard vitest.config.mjs
- ✅ All packages meet coverage thresholds (80% core, 70% examples)
- ✅ All tests pass consistently
- ✅ Documentation complete and accessible
- ✅ CI/CD integration verified

## 🎯 Phase 1: Foundation (Day 1)

### latest Documentation Setup ✅ COMPLETE

**Status:** Complete
- ✅ Created VITEST_STANDARD.md (comprehensive guide)
- ✅ Created VITEST_QUICK_REFERENCE.md (quick start)
- ✅ Created VITEST_ARCHITECTURE.md (system design)
- ✅ Created VITEST_IMPLEMENTATION_ROADMAP.md (this document)

**Deliverables:**
- Complete documentation suite in `/docs`
- Stored in Claude Flow memory: `unrdf/vitest/standard`

### latest Template Creation ✅ COMPLETE

**Status:** Complete
- ✅ Node.js template (core packages)
- ✅ jsdom template (browser packages)
- ✅ Example template (simplified config)

**Location:** Templates embedded in VITEST_STANDARD.md

### latest Audit Current State

**Tasks:**
- [ ] List all packages with existing vitest.config.mjs
- [ ] List all packages missing vitest.config.mjs
- [ ] Identify non-standard configs
- [ ] Document current coverage levels
- [ ] Identify packages with no tests

**Command:**
```bash
# Find existing configs
find packages -name "vitest.config.mjs"

# Find packages missing configs
for pkg in packages/*/; do
  if [ ! -f "${pkg}vitest.config.mjs" ]; then
    echo "Missing: ${pkg}"
  fi
done

# Check test coverage
pnpm -r test:coverage 2>&1 | grep "Coverage"
```

**Deliverable:** Audit report (audit-report.md)

## 🏗️ Phase 2: Core Package Migration (Day 2)

### latest Priority: Production-Critical Packages

**Order:**
1. @unrdf/core (foundation)
2. @unrdf/hooks (Knowledge Hooks)
3. @unrdf/knowledge-engine (inference)
4. @unrdf/browser (browser support)
5. @unrdf/streaming (real-time)

### latest Migration Steps (Per Package)

**For each package:**

1. **Backup existing config**
   ```bash
   cp vitest.config.mjs vitest.config.mjs.backup
   ```

2. **Apply standard template**
   - Use Node.js template for most packages
   - Use jsdom template for @unrdf/browser, @unrdf/composables

3. **Update package.json scripts**
   ```json
   {
     "scripts": {
       "test": "vitest run --coverage",
       "test:fast": "vitest run",
       "test:watch": "vitest --coverage",
       "test:ui": "vitest --ui"
     }
   }
   ```

4. **Run tests and verify**
   ```bash
   pnpm test
   ```

5. **Fix any failures**
   - Update environment directive if needed
   - Add missing dependencies
   - Fix import paths

6. **Verify coverage**
   ```bash
   pnpm test:coverage
   ```

7. **Commit changes**
   ```bash
   git add vitest.config.mjs package.json test/
   git commit -m "feat(package): standardize vitest configuration"
   ```

### latest Package Migration Checklist

**@unrdf/core:**
- [ ] Apply Node.js template
- [ ] Update test scripts
- [ ] Run tests (expect: all pass)
- [ ] Verify coverage ≥ 80%
- [ ] Commit changes

**@unrdf/hooks:**
- [ ] Apply Node.js template
- [ ] Update test scripts
- [ ] Run tests (expect: all pass)
- [ ] Verify coverage ≥ 80%
- [ ] Commit changes

**@unrdf/knowledge-engine:**
- [ ] Apply Node.js template
- [ ] Set testTimeout: 60_000 (for inference)
- [ ] Update test scripts
- [ ] Run tests (expect: all pass)
- [ ] Verify coverage ≥ 80%
- [ ] Commit changes

**@unrdf/browser:**
- [ ] Apply jsdom template
- [ ] Update test scripts
- [ ] Run tests (expect: all pass)
- [ ] Verify coverage ≥ 80%
- [ ] Commit changes

**@unrdf/streaming:**
- [ ] Apply Node.js template
- [ ] Update test scripts
- [ ] Run tests (expect: all pass)
- [ ] Verify coverage ≥ 80%
- [ ] Commit changes

**@unrdf/composables:**
- [ ] Apply jsdom template
- [ ] Update test scripts
- [ ] Run tests (expect: all pass)
- [ ] Verify coverage ≥ 80%
- [ ] Commit changes

**@unrdf/federation:**
- [ ] Apply Node.js template
- [ ] Update test scripts
- [ ] Run tests (expect: all pass)
- [ ] Verify coverage ≥ 80%
- [ ] Commit changes

**@unrdf/dark-matter:**
- [ ] Apply Node.js template
- [ ] Update test scripts
- [ ] Run tests (expect: all pass)
- [ ] Verify coverage ≥ 80%
- [ ] Commit changes

**@unrdf/cli:**
- [ ] Apply Node.js template
- [ ] Add citty-test-utils config if needed
- [ ] Update test scripts
- [ ] Run tests (expect: all pass)
- [ ] Verify coverage ≥ 80%
- [ ] Commit changes

**@unrdf/project-engine:**
- [ ] Apply Node.js template
- [ ] Update test scripts
- [ ] Run tests (expect: all pass)
- [ ] Verify coverage ≥ 80%
- [ ] Commit changes

## 📦 Phase 3: Example Projects Migration (Day 3-4)

### latest Example Categories

**Core Examples (3):**
- packages/core/examples/basic-store
- packages/core/examples/sparql-queries
- packages/core/examples/rdf-parsing

**Browser Examples (2):**
- packages/browser/examples/indexed-db
- packages/browser/examples/offline-support

**Hooks Examples (2):**
- packages/hooks/examples/hook-chains
- packages/hooks/examples/policy-hooks

**Streaming Examples (2):**
- packages/streaming/examples/change-feeds
- packages/streaming/examples/real-time-sync

**Composables Examples (2):**
- packages/composables/examples/query-integration
- packages/composables/examples/reactive-graphs

**Federation Examples (2):**
- packages/federation/examples/peer-discovery
- packages/federation/examples/distributed-queries

**Knowledge-Engine Examples (2):**
- packages/knowledge-engine/examples/basic-inference
- packages/knowledge-engine/examples/sparql-rules

**Dark-Matter Examples (2):**
- packages/dark-matter/examples/index-advisor
- packages/dark-matter/examples/query-optimization

**CLI Examples (2):**
- packages/cli/examples/format-conversion
- packages/cli/examples/graph-commands

### latest Batch Migration Strategy

**Approach:** Process examples in parallel by category

**For each example:**
1. Apply example template
2. Set environment (node or jsdom)
3. Update test scripts
4. Run tests
5. Verify coverage ≥ 70%
6. Commit

**Command:**
```bash
# Migrate all examples in a package
for example in packages/core/examples/*/; do
  echo "Migrating: ${example}"
  cp templates/vitest.config.example.mjs "${example}vitest.config.mjs"
  cd "${example}"
  pnpm test
  cd -
done
```

### latest Example Migration Checklist

**Core Examples:**
- [ ] basic-store (node)
- [ ] sparql-queries (node)
- [ ] rdf-parsing (node)

**Browser Examples:**
- [ ] indexed-db (jsdom)
- [ ] offline-support (jsdom)

**Hooks Examples:**
- [ ] hook-chains (node)
- [ ] policy-hooks (node)

**Streaming Examples:**
- [ ] change-feeds (node)
- [ ] real-time-sync (node)

**Composables Examples:**
- [ ] query-integration (jsdom)
- [ ] reactive-graphs (jsdom)

**Federation Examples:**
- [ ] peer-discovery (node)
- [ ] distributed-queries (node)

**Knowledge-Engine Examples:**
- [ ] basic-inference (node)
- [ ] sparql-rules (node)

**Dark-Matter Examples:**
- [ ] index-advisor (node)
- [ ] query-optimization (node)

**CLI Examples:**
- [ ] format-conversion (node)
- [ ] graph-commands (node)

## 🔄 Phase 4: Validation & Quality Assurance (Day 5)

### latest Comprehensive Testing

**Run all tests:**
```bash
# Root level
pnpm test

# All packages
pnpm -r --filter './packages/*' test

# All examples
pnpm -r --filter './packages/*/examples/*' test
```

**Expected Results:**
- All tests pass (100% success rate)
- Core packages: ≥ 80% coverage
- Examples: ≥ 70% coverage

### latest Coverage Verification

**Generate coverage reports:**
```bash
# Individual packages
pnpm --filter @unrdf/core test:coverage
pnpm --filter @unrdf/browser test:coverage

# All packages
pnpm -r test:coverage

# Review HTML reports
open packages/core/coverage/index.html
```

**Coverage Matrix:**
| Package | Lines | Functions | Branches | Statements | Status |
|---------|-------|-----------|----------|------------|--------|
| @unrdf/core | ?% | ?% | ?% | ?% | ⏳ |
| @unrdf/hooks | ?% | ?% | ?% | ?% | ⏳ |
| @unrdf/browser | ?% | ?% | ?% | ?% | ⏳ |
| ... | ... | ... | ... | ... | ... |

### latest CI/CD Integration

**Update CI workflow:**
```yaml
# .github/workflows/test.yml
name: Test

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: pnpm/action-setup@v2
      - uses: actions/setup-node@v3
        with:
          node-version: 18
          cache: 'pnpm'
      - run: pnpm install
      - run: pnpm test:ci
      - uses: codecov/codecov-action@v3
        with:
          files: ./coverage/coverage-final.json
```

**Verify CI:**
- [ ] All tests pass in CI
- [ ] Coverage reports uploaded
- [ ] No flaky tests
- [ ] Consistent performance

### latest Documentation Review

**Update project documentation:**
- [ ] README.md (mention testing standards)
- [ ] CONTRIBUTING.md (link to VITEST_STANDARD.md)
- [ ] Package READMEs (add test instructions)

**Create links:**
```markdown
## Testing

See [VITEST_STANDARD.md](./docs/VITEST_STANDARD.md) for comprehensive testing standards.

Quick reference: [VITEST_QUICK_REFERENCE.md](./docs/VITEST_QUICK_REFERENCE.md)
```

## 📊 Phase 5: Monitoring & Maintenance (Ongoing)

### latest Regular Monitoring

**Weekly checks:**
- [ ] Run full test suite
- [ ] Check coverage trends
- [ ] Identify flaky tests
- [ ] Review test performance

**Command:**
```bash
# Weekly test run
pnpm test:ci > test-report-$(date +%Y%m%d).log 2>&1
```

### latest Coverage Tracking

**Track coverage over time:**
```bash
# Generate coverage badge
pnpm test:coverage
cat coverage/coverage-summary.json | jq '.total.lines.pct'
```

**Coverage goals:**
- Core packages: Maintain ≥ 80%
- Examples: Maintain ≥ 70%
- Overall: Trend towards 85%+

### latest New Package Onboarding

**When adding new package:**
1. Copy appropriate template
2. Create test/ directory
3. Add test scripts to package.json
4. Write initial tests
5. Verify coverage ≥ 80% (or 70% for examples)
6. Add to CI/CD

**Checklist:** See VITEST_STANDARD.md "New Package Checklist"

## 🚀 Parallel Execution Strategy

### Agent Assignment

**Use Claude Code Task tool for parallel execution:**

```javascript
// Phase 2: Core Packages (Parallel)
Task("Core Package Agent", "Migrate @unrdf/core, @unrdf/hooks", "coder")
Task("Browser Package Agent", "Migrate @unrdf/browser, @unrdf/composables", "coder")
Task("Advanced Package Agent", "Migrate @unrdf/knowledge-engine, @unrdf/dark-matter", "coder")
Task("Infrastructure Agent", "Migrate @unrdf/cli, @unrdf/project-engine", "coder")

// Phase 3: Examples (Parallel by Category)
Task("Core Examples Agent", "Migrate core examples (3)", "coder")
Task("Browser Examples Agent", "Migrate browser examples (2)", "coder")
Task("Hooks Examples Agent", "Migrate hooks examples (2)", "coder")
Task("Streaming Examples Agent", "Migrate streaming examples (2)", "coder")
Task("Composables Examples Agent", "Migrate composables examples (2)", "coder")
Task("Federation Examples Agent", "Migrate federation examples (2)", "coder")
Task("Knowledge Examples Agent", "Migrate knowledge-engine examples (2)", "coder")
Task("DarkMatter Examples Agent", "Migrate dark-matter examples (2)", "coder")
Task("CLI Examples Agent", "Migrate CLI examples (2)", "coder")
```

### Coordination Protocol

**Each agent MUST:**
1. Run `npx claude-flow@alpha hooks pre-task` before starting
2. Apply standard template
3. Update package.json scripts
4. Run tests and verify
5. Commit changes
6. Report results via `npx claude-flow@alpha hooks post-task`

## 📈 Success Metrics

### Quantitative Metrics

**Coverage:**
- Core packages: ≥ 80% (lines, functions, statements)
- Core packages: ≥ 75% (branches)
- Examples: ≥ 70% (lines, functions, statements)
- Examples: ≥ 60% (branches)

**Test Success:**
- 100% test pass rate
- 0 flaky tests
- < 5s average test execution time per package

**Standardization:**
- 100% packages with standard vitest.config.mjs
- 100% packages with standard test scripts
- 100% packages with test/ directory structure

### Qualitative Metrics

**Developer Experience:**
- Consistent test commands across packages
- Clear documentation accessible
- Fast test feedback loop
- Easy to add new tests

**Maintainability:**
- Single source of truth (VITEST_STANDARD.md)
- Templates for easy replication
- CI/CD integration working
- Coverage trends visible

## 🎯 Risk Mitigation

### Potential Risks

**Risk 1: Test Failures During Migration**
- **Mitigation:** Backup existing configs, test incrementally
- **Rollback:** Restore .backup files

**Risk 2: Coverage Drops Below Threshold**
- **Mitigation:** Add tests before enforcing thresholds
- **Fallback:** Temporarily lower thresholds, create improvement plan

**Risk 3: CI/CD Breaks**
- **Mitigation:** Test CI changes in branch first
- **Rollback:** Revert CI config changes

**Risk 4: Inconsistent Environments**
- **Mitigation:** Clear documentation of node vs jsdom
- **Fix:** Add @vitest-environment directive to test files

## 📋 Daily Progress Tracking

### Day 1: Foundation ✅
- [x] Create documentation
- [x] Create templates
- [ ] Audit current state

### Day 2: Core Packages
- [ ] Migrate @unrdf/core
- [ ] Migrate @unrdf/hooks
- [ ] Migrate @unrdf/knowledge-engine
- [ ] Migrate @unrdf/browser
- [ ] Migrate @unrdf/streaming
- [ ] Migrate @unrdf/composables
- [ ] Migrate @unrdf/federation
- [ ] Migrate @unrdf/dark-matter
- [ ] Migrate @unrdf/cli
- [ ] Migrate @unrdf/project-engine

### Day 3: Examples (Batch 1)
- [ ] Core examples (3)
- [ ] Browser examples (2)
- [ ] Hooks examples (2)
- [ ] Streaming examples (2)
- [ ] Composables examples (2)

### Day 4: Examples (Batch 2)
- [ ] Federation examples (2)
- [ ] Knowledge-Engine examples (2)
- [ ] Dark-Matter examples (2)
- [ ] CLI examples (2)
- [ ] Remaining examples

### Day 5: Validation
- [ ] Run all tests
- [ ] Verify coverage
- [ ] CI/CD integration
- [ ] Documentation review
- [ ] Create final report

## 🎉 Completion Criteria

**Definition of Done:**
- ✅ All 37 subprojects have standard vitest.config.mjs
- ✅ All tests pass (100% success rate)
- ✅ Coverage thresholds met (80% core, 70% examples)
- ✅ Documentation complete and accessible
- ✅ CI/CD integration verified
- ✅ Team trained on new standards
- ✅ Monitoring process established

**Final Deliverables:**
1. Migration complete report
2. Coverage report (current vs target)
3. CI/CD dashboard configured
4. Documentation published
5. Team training completed

---

**Roadmap Version:** latest
**Last Updated:** 2025-12-04
**Project Manager:** System Architecture Designer
**Execution Team:** Claude Code + Agent Swarm
