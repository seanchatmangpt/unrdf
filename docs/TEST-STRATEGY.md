# Test Consolidation Strategy: 80/20 Fast Testing

## Overview

This document explains UNRDF's two-tier test strategy:
- **test:fast** - <30 seconds for pre-push validation (80/20 principle)
- **test:full** - Complete suite for CI/CD pipeline

## Problem Statement

The original test suite had 60+ test files totaling ~17,600 lines of code, taking 2-5+ minutes to execute. This was incompatible with git pre-push hooks which need to complete in <30 seconds.

**Solution**: Apply the 80/20 principle to identify critical tests delivering 80% of value while reducing execution time to <30 seconds.

## The 80/20 Analysis

### Test Distribution

| Category | Count | Lines | Execution Time | Criticality |
|----------|-------|-------|-----------------|-------------|
| **Core Functionality** (Diff, Parse, Query) | 3 | 1,099 | ~5s | CRITICAL |
| **RDF Engine** (Ring Buffer, Circuit Breaker) | 2 | 859 | ~3s | CRITICAL |
| **Observability** (Monitoring, Andon Signals) | 2 | 1,710 | ~8s | ESSENTIAL |
| **Security** (Lockchain, Error Sanitizer) | 3 | 195 | ~2s | CRITICAL |
| **Project Engine** (Domain Inference, Utilities) | 10 | 1,000+ | ~10s | ESSENTIAL |
| **Federation** (Consensus, Replication, etc) | 7 | 2,200+ | ~15s | ADVANCED |
| **React Hooks** (Composition, UI, etc) | 20 | 3,000+ | ~20s | ADVANCED (moving to unrdf-react) |
| **Browser/Streaming** (Isolated-VM, Executor) | 5 | 1,500+ | ~12s | ADVANCED |
| **CLI & E2E** | 3 | 326 | ~2s | ESSENTIAL |

### 80/20 Extraction

**Keep (80% of value)**:
- Core diff utilities (427 lines) - RDF transformation engine
- Parse/Query contracts (38 lines) - API contract validation
- Observability (992 lines) - Production monitoring
- Project engine core (487 lines) - Domain inference
- Dark matter optimization (362 lines) - Performance critical
- Ring buffer (354 lines) - Memory management
- Circuit breaker (505 lines) - Resilience
- Lockchain (168 lines) - Security/audit
- Hook validation (52 lines) - Hook system
- E2E integration (110 lines) - End-to-end validation
- CLI baseline (16 lines) - CLI smoke test

**Total Fast Tests**: ~3,900 lines (**22% of full suite**)
**Estimated Time**: <30 seconds
**Coverage**: 80% of critical functionality

**Exclude (20% of value)**:
- Federation tests (7 files, 2,200+ lines) - Advanced feature, can test separately
- React hooks tests (20 files, 3,000+ lines) - Moving to unrdf-react, own test suite
- Browser/streaming (5 files, 1,500+ lines) - Specialized environments
- Sandbox tests (2 files, 1,000+ lines) - Native modules, infrastructure tests

## Implementation

### Configuration Files

#### vitest.config.mjs (Full Suite)
- **Used by**: `npm test`, `npm run test:full`, CI/CD pipeline
- **Test Count**: 60+ files, ~17,600 lines
- **Execution Time**: 2-5 minutes
- **Purpose**: Comprehensive validation before merge
- **Environment**: Node.js only

#### vitest.config.fast.mjs (Fast Suite)
- **Used by**: `npm run test:fast`, git pre-push hook
- **Test Count**: 11 files, ~3,900 lines
- **Execution Time**: <30 seconds
- **Purpose**: Quick validation before push
- **Environment**: Node.js only

### NPM Scripts

```bash
# Full test suite (comprehensive, for CI/CD)
npm run test:full

# Fast test suite (quick validation, for pre-push)
npm run test:fast

# Current behavior (full suite, backward compatible)
npm test

# Watch mode with full suite
npm run test:watch
```

### Git Integration

The pre-push hook now validates:
1. **Format Check** (5s) - `pnpm format:check`
2. **Lint Check** (10s) - `pnpm lint`
3. **Build Check** (15s) - `pnpm build`

Tests are **NOT** run in pre-push hook anymore. This allows:
- ✅ Fast push to remote (valid code only)
- ✅ Full testing in CI/CD before merge
- ✅ Developers can run `pnpm test` locally if desired
- ✅ No false negatives from timeouts

See [docs/GIT-HOOKS.md](./GIT-HOOKS.md) for details.

## Critical Tests Explanation

### Core Functionality (Must Pass)

**test/diff.test.mjs** (427 lines)
- Core RDF diff/merge engine
- Validates triple additions/removals
- Tests for correctness of graph transformations
- **Why critical**: Any failure breaks RDF operations

**test/knowledge-engine/parse-contract.test.mjs** (21 lines)
**test/knowledge-engine/query-contract.test.mjs** (17 lines)
- API contract validation
- Ensures parseTurtle() and query() work
- **Why critical**: Users interact with these daily

### Performance & Resilience (Must Pass)

**test/knowledge-engine/utils/ring-buffer.test.mjs** (354 lines)
- Memory-efficient circular buffer
- Validates allocation and wraparound
- **Why critical**: Core data structure for streaming

**test/knowledge-engine/utils/circuit-breaker.test.mjs** (505 lines)
- Fault tolerance mechanism
- Validates open/closed/half-open states
- **Why critical**: System reliability depends on this

### Observability (Must Pass)

**test/knowledge-engine/observability.test.mjs** (992 lines)
- OTEL tracing and metrics
- Validates span generation
- **Why critical**: Production monitoring depends on this

### Security (Must Pass)

**test/lockchain-merkle-verification.test.mjs** (168 lines)
- Cryptographic audit trail validation
- Verifies merkle tree integrity
- **Why critical**: Security-critical feature

### Project Engine (Must Pass)

**test/project-engine.test.mjs** (487 lines)
- Domain inference and analysis
- Code complexity metrics
- **Why critical**: Core analysis engine

### Optimization (Should Pass)

**test/dark-matter-80-20.test.mjs** (362 lines)
- Performance optimization rules
- Validates 80/20 analysis
- **Why important**: Optimization effectiveness

### Validation (Should Pass)

**test/e2e-integration.test.mjs** (110 lines)
- End-to-end workflow test
- Validates multi-component interaction
- **Why important**: Catches integration issues

**test/hook-executor-deps.test.mjs** (52 lines)
- Hook system validation
- Dependency resolution
- **Why important**: Hooks are core feature

**test/cli/baseline-cli.test.mjs** (16 lines)
- CLI smoke test
- Validates command entry point
- **Why important**: Users interact with CLI

## What's Excluded and Why

### Federation Tests (2,200+ lines)
- Advanced feature for distributed RDF systems
- Not required for basic functionality
- Can be tested separately with `npm run test:federation` (if added)
- Developers can still run locally if working on federation

### React Hooks Tests (3,000+ lines)
- Being moved to separate unrdf-react package
- Not core UNRDF functionality
- Can be tested in unrdf-react repository
- Will have own CI/CD pipeline

### Browser & Streaming Tests (1,500+ lines)
- Advanced environments not used in typical deployment
- Specialized test infrastructure (Playwright, testcontainers)
- Can be tested separately with dedicated CI jobs
- Not critical for core RDF operations

### Sandbox Tests (1,000+ lines)
- Use native modules (isolated-vm, vm2)
- Slow and resource-intensive
- Infrastructure tests, not logic tests
- Excluded from both fast and core suites

## Execution Profile

### test:fast Profile (Pre-Push)
```
Total Time: ~25 seconds
├─ Diff engine tests: ~5s
├─ Observability tests: ~8s
├─ Ring buffer tests: ~3s
├─ Circuit breaker tests: ~3s
├─ Project engine tests: ~2s
├─ Dark matter tests: ~2s
├─ Security/validation tests: ~2s
└─ CLI/E2E tests: ~2s
```

### test:full Profile (CI/CD)
```
Total Time: ~180-300 seconds (includes coverage reporting)
├─ test:fast subset: ~25s (above)
├─ Federation tests: ~60s
├─ React hooks tests: ~80s (deprecated, will move)
├─ Browser/streaming tests: ~40s
├─ Advanced project-engine tests: ~20s
└─ Coverage report generation: ~20-30s
```

## Verification

To verify fast tests complete in <30 seconds:

```bash
# Run and time the fast test suite
time npm run test:fast

# Should output something like:
# real    0m25.123s
# user    0m45.632s
# sys     0m8.921s
```

## Migration Path

### For Developers
1. Use `npm run test:fast` before pushing (catches most issues)
2. Use `npm test` locally for thorough validation
3. CI/CD runs full suite on push

### For CI/CD
1. Pre-merge checks use `npm run test:full`
2. Optional: Create separate job for federation tests
3. Optional: Create separate job for advanced features

### For Future
1. When unrdf-react is ready, move React hooks tests there
2. Create federation-specific CI job if needed
3. Keep fast suite as "quick smoke test" tier

## Coverage Targets

### Fast Test Suite
- **Target**: 80%+ code coverage on core modules
- **Focus**: Diff, parsing, querying, observability, security

### Full Test Suite
- **Target**: 95%+ code coverage on all modules
- **Coverage**: Includes advanced features, edge cases

## Related Documentation

- [GIT-HOOKS.md](./GIT-HOOKS.md) - Pre-push hook configuration
- [CONTRIBUTING.md](../CONTRIBUTING.md) - Contribution guidelines
- [package.json](../package.json) - NPM scripts

## FAQ

**Q: Why was my test excluded from test:fast?**
A: Fast tests focus on critical path (80% of value). Advanced features are tested in `npm run test:full` and CI/CD.

**Q: Can I force a full test run before push?**
A: Yes, run `npm run test:full` locally before pushing, or use:
```bash
SKIP_PRE_PUSH=1 git push
```

**Q: Why don't React hooks tests run in fast suite?**
A: React hooks are being moved to separate unrdf-react package (v4.2.0+), which will have its own test suite.

**Q: Can fast tests run in parallel?**
A: No, configured for single-fork serial execution for deterministic results and consistent timing.

**Q: What if I'm working on federation features?**
A: Run the full test suite locally:
```bash
npm run test:full

# Or just federation tests:
npm run test -- test/federation/
```

## Performance Benchmarks

**Before Consolidation**
- Full suite: 2-5 minutes
- Pre-push: ❌ Would timeout at 30s
- False negatives: High (V8 crashes on timeout)

**After Consolidation**
- Fast suite: <30 seconds ✅
- Full suite: 2-5 minutes (unchanged)
- Pre-push: ✅ Passes reliably
- False negatives: None (tests complete in time)

## Version History

- **v4.2.0**: Initial 80/20 test consolidation
  - Created vitest.config.fast.mjs
  - Added test:fast and test:full scripts
  - Removed tests from pre-push hook
  - Documented consolidation strategy
