# UNRDF Example Validation - Quick Reference Index

## 📊 Current Status: 9/21 Passing (42.9%)

### Quick Access Files
- **Status Dashboard**: `validation-status.txt` - Visual status overview
- **Detailed Report**: `docs/VALIDATION_REPORT.md` - 400+ line comprehensive analysis
- **Executive Summary**: `docs/VALIDATION_SUMMARY.md` - High-level overview for stakeholders
- **Raw Validation Logs**: `validation-report.txt` and `validation-report-2.txt`

### Automation Scripts
- **Run Validation**: `node scripts/validate-all-examples.mjs`
- **Apply Fixes**: `node scripts/fix-all-examples.mjs`
- **Quick Commands**: `./scripts/validation-commands.sh [command]`

### CLI Quick Commands
```bash
# Run full validation
./scripts/validation-commands.sh full

# View summary
./scripts/validation-commands.sh summary

# See package status
./scripts/validation-commands.sh packages

# View next steps
./scripts/validation-commands.sh next

# Test specific example
./scripts/validation-commands.sh test core basic-store

# Get help
./scripts/validation-commands.sh help
```

## 🎯 Validation Breakdown

### ✅ Fully Validated (9 examples)
1. `packages/core/examples/basic-store` - 18 tests ✅
2. `packages/core/examples/sparql-queries` - 16 tests ✅
3. `packages/core/examples/rdf-parsing` - 19 tests ✅
4. `packages/hooks/examples/hook-chains` - 15 tests ✅
5. `packages/federation/examples/peer-discovery` - 16 tests ✅
6. `packages/federation/examples/distributed-queries` - 18 tests ✅
7. `packages/cli/examples/graph-commands` - 10 tests ✅
8. `packages/cli/examples/format-conversion` - 10 tests ✅
9. `packages/dark-matter/examples/query-optimization` - 10 tests ✅
10. `packages/dark-matter/examples/index-advisor` - 10 tests ✅

### ⚠️ Partially Validated (1 example)
11. `packages/hooks/examples/policy-hooks` - 11 tests (needs 1 more)

### ❌ Blocked - Implementation Issues (7 examples)
12. `packages/streaming/examples/change-feeds` - Missing methods
13. `packages/streaming/examples/real-time-sync` - Missing methods
14. `packages/browser/examples/indexed-db` - Execution errors
15. `packages/browser/examples/offline-support` - Execution errors
16. `packages/knowledge-engine/examples/basic-inference` - Execution errors
17. `packages/knowledge-engine/examples/sparql-rules` - Execution errors
18. `packages/composables/examples/reactive-graphs` - Execution errors

### ❌ Blocked - Missing Configs (2 examples)
19. `playground/full-stack-example/apps/server` - Needs vitest.config.mjs
20. `playground/full-stack-example/apps/web` - Needs vitest.config.mjs

### 📋 Not Yet Created (5 examples)
21. `packages/project-engine/examples/*` - Not created yet
22-26. Additional planned examples

## 📈 Progress Metrics

| Category | Current | Target | %  |
|----------|---------|--------|-----|
| Examples validated | 9 | 21 | 42.9% |
| Vitest configs | 19 | 21 | 90% |
| Test scripts | 19 | 21 | 90% |
| Test files | 21 | 21 | 100% |
| Testing docs | 21 | 21 | 100% |
| Total tests | 274 | 320+ | 85% |

## 🚀 Next Actions

### Phase 1: Quick Wins (1-2 hours)
- Add `vitest.config.mjs` to `playground/full-stack-example/apps/server`
- Add `vitest.config.mjs` to `playground/full-stack-example/apps/web`
- Add 1 test to `packages/hooks/examples/policy-hooks`
- **Expected Result**: 11/21 passing (52%)

### Phase 2: Implementation Fixes (4-6 hours)
- Implement `ChangeFeed.subscribe()` in `@unrdf/streaming`
- Implement `ChangeFeed.getHistory()` in `@unrdf/streaming`
- Debug browser examples jsdom configuration
- Debug knowledge-engine test execution
- Debug composables reactive-graphs execution
- **Expected Result**: 18/21 passing (86%)

### Phase 3: Test Expansion (2-3 hours)
- Add tests to reach minimum counts for all examples
- **Expected Result**: 21/21 passing (100%) 🎯

## 🔍 Validation Commands by Example

### Core Examples
```bash
cd packages/core/examples/basic-store && pnpm test
cd packages/core/examples/sparql-queries && pnpm test
cd packages/core/examples/rdf-parsing && pnpm test
```

### Hooks Examples
```bash
cd packages/hooks/examples/policy-hooks && pnpm test
cd packages/hooks/examples/hook-chains && pnpm test
```

### Federation Examples
```bash
cd packages/federation/examples/peer-discovery && pnpm test
cd packages/federation/examples/distributed-queries && pnpm test
```

### Streaming Examples (FAILING)
```bash
cd packages/streaming/examples/change-feeds && pnpm test
cd packages/streaming/examples/real-time-sync && pnpm test
```

### Browser Examples (FAILING)
```bash
cd packages/browser/examples/indexed-db && pnpm test
cd packages/browser/examples/offline-support && pnpm test
```

### CLI Examples
```bash
cd packages/cli/examples/graph-commands && pnpm test
cd packages/cli/examples/format-conversion && pnpm test
```

### Knowledge Engine Examples (FAILING)
```bash
cd packages/knowledge-engine/examples/basic-inference && pnpm test
cd packages/knowledge-engine/examples/sparql-rules && pnpm test
```

### Dark Matter Examples
```bash
cd packages/dark-matter/examples/query-optimization && pnpm test
cd packages/dark-matter/examples/index-advisor && pnpm test
```

### Composables Examples (FAILING)
```bash
cd packages/composables/examples/reactive-graphs && pnpm test
cd packages/composables/examples/query-integration && pnpm test
```

### Full-Stack Examples (NO CONFIG)
```bash
cd playground/full-stack-example/apps/server && pnpm test
cd playground/full-stack-example/apps/web && pnpm test
```

## 💾 Memory Storage

Validation state stored in Claude Flow memory:
```bash
npx claude-flow@alpha memory retrieve "unrdf/vitest/validation" --reasoningbank
```

Key metrics:
- **Status**: PARTIAL_COMPLETE
- **Validated**: 9/21 (42.9%)
- **Tests Found**: 274
- **Pass Rate**: 100% (12/12 executable)
- **Estimated Time to 100%**: 8-11 hours

## 📞 Support

For questions or issues:
1. Check `docs/VALIDATION_REPORT.md` for detailed analysis
2. Run `./scripts/validation-commands.sh help` for available commands
3. Review individual example README.md files for specific instructions

---

**Last Updated**: 2025-12-04
**Validation Engineer**: Production Validator Agent
**Status**: ⚠️ 42.9% Complete
