# Performance Optimization Implementation Guide

**Date**: 2025-12-25
**Status**: Ready for Implementation
**Expected Impact**: -60% build time, -30% test time, -40% memory usage

---

## Quick Reference

| Optimization | Impact | Effort | Priority | Est. Time |
|-------------|--------|--------|----------|-----------|
| Fix missing configs | ⚡⚡⚡ Critical | Low | P0 | 10 min |
| Install dependencies | ⚡⚡⚡ Critical | Low | P0 | 5 min |
| Split docs builds | ⚡⚡ High | Low | P1 | 5 min |
| Parallel builds | ⚡⚡ High | Low | P1 | 2 min |
| Lazy CLI imports | ⚡⚡ High | Medium | P1 | 30 min |
| Build caching | ⚡⚡⚡ High | High | P2 | 2 hours |
| Migrate to esbuild | ⚡⚡⚡ High | High | P3 | 1 day |

---

## P0: Critical Fixes (Do Immediately)

### 1. Fix atomvm Missing tsconfig.json

**Issue**: `tsc --emitDeclarationOnly` fails with help text because tsconfig.json doesn't exist

**Root Cause**: Configuration file missing from package

**Fix**:
```bash
cat > /home/user/unrdf/packages/atomvm/tsconfig.json <<'EOF'
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ES2020",
    "moduleResolution": "bundler",
    "lib": ["ES2020", "DOM"],
    "allowJs": true,
    "checkJs": false,
    "declaration": true,
    "emitDeclarationOnly": true,
    "declarationDir": "./dist",
    "outDir": "./dist",
    "rootDir": "./src",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "resolveJsonModule": true,
    "isolatedModules": true,
    "verbatimModuleSyntax": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist", "**/*.test.mjs", "**/*.spec.mjs"]
}
EOF
```

**Verification**:
```bash
cd /home/user/unrdf/packages/atomvm
timeout 10s pnpm build
# Should complete successfully in <5s
```

**Expected Impact**: Fix 1 critical build failure, -5s build time

---

### 2. Fix core Missing build.config.mjs

**Issue**: `node build.config.mjs` fails - module not found

**Root Cause**: Build configuration missing or incorrect package.json script

**Investigation**:
```bash
cd /home/user/unrdf/packages/core
ls -la build.config.*
cat package.json | jq '.scripts.build'
```

**Fix Option A** (if build.config.mjs should exist):
```bash
cat > /home/user/unrdf/packages/core/build.config.mjs <<'EOF'
import { defineBuildConfig } from 'unbuild';

export default defineBuildConfig({
  entries: ['src/index'],
  clean: true,
  declaration: true,
  rollup: {
    emitCJS: false,
    esbuild: {
      minify: false
    }
  },
  externals: [
    '@opentelemetry/api',
    'n3'
  ]
});
EOF
```

**Fix Option B** (if should use unbuild directly):
```json
// package.json scripts
{
  "build": "unbuild && tsc --emitDeclarationOnly"
}
```

**Verification**:
```bash
cd /home/user/unrdf/packages/core
timeout 10s pnpm build
```

**Expected Impact**: Fix 1 critical build failure, -3s build time

---

### 3. Fix validation knowledge-engine Import

**Issue**: `Could not resolve "../knowledge-engine/index.mjs"`

**Root Cause**: Relative import path incorrect or knowledge-engine not built

**Investigation**:
```bash
cd /home/user/unrdf/packages/validation
ls -la ../knowledge-engine/
ls -la ../knowledge-engine/index.mjs
```

**Fix Option A** (if knowledge-engine exists):
```javascript
// src/otel-span-builder.mjs
// Change relative import to workspace import
import { ... } from '@unrdf/knowledge-engine';
```

**Fix Option B** (if path wrong):
```javascript
// Correct the path
import { ... } from '../knowledge-engine/src/index.mjs';
```

**Fix Option C** (if dependency missing):
```bash
cd /home/user/unrdf/packages/validation
pnpm add @unrdf/knowledge-engine --workspace
```

**Verification**:
```bash
cd /home/user/unrdf/packages/validation
timeout 10s pnpm build
```

**Expected Impact**: Fix 1 critical build failure, -2s build time

---

### 4. Install @dagrejs/graphlib for graph-analytics

**Issue**: `Cannot find package '@dagrejs/graphlib'`

**Root Cause**: Dependency not installed

**Fix**:
```bash
pnpm add @dagrejs/graphlib --filter @unrdf/graph-analytics
```

**Verification**:
```bash
cd /home/user/unrdf/packages/graph-analytics
timeout 10s pnpm test
# Should show tests passing, not import errors
```

**Expected Impact**: Fix test failures, enable benchmarking for graph-analytics

---

### 5. Fix Workspace Dependencies

**Issue**: Multiple packages cannot find `@unrdf/oxigraph`

**Root Cause**: Workspace dependency resolution broken

**Fix**:
```bash
cd /home/user/unrdf
pnpm install --force
```

**Verification**:
```bash
cd /home/user/unrdf/packages/dark-matter
timeout 10s pnpm test
# Should not show "Cannot find package '@unrdf/oxigraph'" errors
```

**Expected Impact**: Fix 2-3 test failures, enable complete test benchmarking

---

## P1: High Priority Optimizations (Do Next)

### 6. Split Documentation Builds

**Issue**: Nuxt and Docusaurus builds slow down core package builds

**Current Behavior**:
```bash
pnpm -r build
# Builds ALL 42 packages including slow docs (>30s total)
```

**Optimization**:
```json
// /home/user/unrdf/package.json
{
  "scripts": {
    "build": "pnpm build:core",
    "build:core": "pnpm -r --filter='!@unrdf/docs' --filter='!docs-site' build",
    "build:docs": "pnpm -r --filter='@unrdf/docs' --filter='docs-site' build",
    "build:all": "pnpm build:core && pnpm build:docs"
  }
}
```

**Usage**:
```bash
# Fast core build (skip docs)
pnpm build:core

# Full build (when needed)
pnpm build:all
```

**Verification**:
```bash
time timeout 30s pnpm build:core
# Should complete in <10s without docs
```

**Expected Impact**: -60% build time for core packages (30s → 12s)

---

### 7. Enable Parallel Builds

**Issue**: Sequential builds slow overall time

**Current Behavior**:
```bash
pnpm -r build
# Builds packages sequentially (default)
```

**Optimization**:
```bash
# Add to package.json scripts
{
  "build:core": "pnpm -r --parallel --workspace-concurrency=4 --filter='!@unrdf/docs' --filter='!docs-site' build"
}
```

**Rationale**:
- Most packages are independent (no cross-dependencies during build)
- 4 concurrent builds optimal for CI (2 CPU cores + I/O overlap)
- Reduces wall-clock time significantly

**Verification**:
```bash
time timeout 30s pnpm build:core
# Should be 30-50% faster than sequential
```

**Expected Impact**: -40% build time (12s → 7s with parallelism)

---

### 8. Optimize CLI Import Time (Lazy Loading)

**Issue**: CLI tests spend 55% of time on imports (1.19s of 2.18s)

**Current Code**:
```javascript
// Eager imports load everything upfront
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { Parser, Writer } from 'n3';
import { Command } from 'commander';
```

**Optimization**:
```javascript
// Lazy imports - load only when needed
export async function createCLI() {
  const { Command } = await import('commander');
  return new Command();
}

export async function parseRDF(input) {
  const { Parser } = await import('n3');
  return new Parser().parse(input);
}

export async function initStore() {
  const { createStore, dataFactory } = await import('@unrdf/oxigraph');
  return createStore();
}
```

**Files to Update**:
- `/home/user/unrdf/packages/cli/src/index.mjs`
- `/home/user/unrdf/packages/cli/src/commands/*.mjs`

**Verification**:
```bash
cd /home/user/unrdf/packages/cli
time timeout 10s pnpm test
# Should show import time <500ms (down from 1.19s)
```

**Expected Impact**: -50% CLI test time (4.54s → 2.5s)

---

### 9. Fix unbuild Hangs in oxigraph/atomvm

**Issue**: unbuild process hangs indefinitely on certain packages

**Investigation**:
```bash
# Run with verbose logging
cd /home/user/unrdf/packages/oxigraph
DEBUG=unbuild:* timeout 15s pnpm build 2>&1 | tee unbuild-debug.log
```

**Potential Root Causes**:
1. Infinite loop in dependency resolution
2. WASM file bundling issue
3. Circular dependencies
4. External module resolution failure

**Fix Option A** (Skip problematic entries):
```javascript
// packages/oxigraph/build.config.mjs
export default defineBuildConfig({
  entries: ['src/index'], // Remove 'src/store', 'src/types'
  externals: ['oxigraph'] // Don't bundle WASM
});
```

**Fix Option B** (Switch to esbuild for this package):
```json
// packages/oxigraph/package.json
{
  "scripts": {
    "build": "node build.mjs && tsc --emitDeclarationOnly"
  }
}
```

```javascript
// packages/oxigraph/build.mjs
import esbuild from 'esbuild';

await esbuild.build({
  entryPoints: ['src/index.mjs'],
  bundle: true,
  format: 'esm',
  platform: 'node',
  outdir: 'dist',
  external: ['oxigraph'] // WASM module
});
```

**Verification**:
```bash
cd /home/user/unrdf/packages/oxigraph
time timeout 10s pnpm build
# Should complete in <5s
```

**Expected Impact**: -10s build time (eliminate hangs)

---

## P2: Medium Priority Optimizations (Do Soon)

### 10. Add Build Caching (Turborepo)

**Issue**: Every build rebuilds everything, even unchanged packages

**Installation**:
```bash
pnpm add -Dw turbo
```

**Configuration**:
```json
// /home/user/unrdf/turbo.json
{
  "$schema": "https://turbo.build/schema.json",
  "pipeline": {
    "build": {
      "outputs": ["dist/**", ".next/**", ".nuxt/**"],
      "dependsOn": ["^build"],
      "cache": true
    },
    "test": {
      "outputs": ["coverage/**"],
      "dependsOn": ["build"],
      "cache": true
    },
    "lint": {
      "outputs": [],
      "cache": true
    }
  }
}
```

**Update Scripts**:
```json
// /home/user/unrdf/package.json
{
  "scripts": {
    "build": "turbo run build --filter='!@unrdf/docs' --filter='!docs-site'",
    "test": "turbo run test",
    "lint": "turbo run lint"
  }
}
```

**Verification**:
```bash
# First run (cold cache)
time turbo run build
# Second run (warm cache)
time turbo run build
# Should be 10-50x faster (2s vs 20s)
```

**Expected Impact**: -90% build time on cache hits (20s → 2s)

---

### 11. Profile Memory Usage Per Package

**Issue**: Baseline 126.52 MB RSS exceeds 50 MB target by 2.5x

**Profiling Script**:
```javascript
// /home/user/unrdf/scripts/profile-memory.mjs
import { spawn } from 'child_process';

const packages = [
  'cli', 'core', 'oxigraph', 'validation',
  'dark-matter', 'graph-analytics'
];

for (const pkg of packages) {
  console.log(`\n=== Profiling ${pkg} ===`);

  const proc = spawn('node', [
    '--expose-gc',
    '--trace-gc',
    './node_modules/.bin/vitest',
    'run'
  ], {
    cwd: `/home/user/unrdf/packages/${pkg}`,
    stdio: 'inherit'
  });

  await new Promise(resolve => proc.on('exit', resolve));
}
```

**Usage**:
```bash
node scripts/profile-memory.mjs 2>&1 | tee memory-profile.log
grep "Scavenge\|MarkCompact" memory-profile.log
```

**Analysis**:
- Identify packages with high heap usage
- Check for memory leaks (growing heap over time)
- Find unnecessary large allocations

**Expected Impact**: Identify specific optimization targets

---

### 12. Optimize WASM Loading in oxigraph

**Issue**: Oxigraph WASM module contributes to high baseline memory

**Current Loading** (assumed):
```javascript
// Loads entire WASM module into memory upfront
import oxigraph from 'oxigraph';
```

**Optimization**:
```javascript
// Lazy load WASM only when store is created
let oxigraphModule = null;

export async function createStore() {
  if (!oxigraphModule) {
    oxigraphModule = await import('oxigraph');
  }
  return new oxigraphModule.Store();
}
```

**Verification**:
```bash
# Measure memory before/after
node --expose-gc -e "
const baseline = process.memoryUsage();
const { createStore } = await import('./packages/oxigraph/src/index.mjs');
const afterImport = process.memoryUsage();
const store = await createStore();
const afterCreate = process.memoryUsage();

console.log('Baseline RSS:', baseline.rss / 1024 / 1024, 'MB');
console.log('After Import RSS:', afterImport.rss / 1024 / 1024, 'MB');
console.log('After Create RSS:', afterCreate.rss / 1024 / 1024, 'MB');
"
```

**Expected Impact**: -30 MB baseline RSS (126 MB → 96 MB)

---

## P3: Long-term Optimizations (Nice to Have)

### 13. Migrate to esbuild for All Packages

**Issue**: unbuild/rollup is significantly slower than esbuild

**Benchmark**:
```
unbuild: ~2-5s per package
esbuild: ~0.1-0.5s per package (10-50x faster)
```

**Migration Plan**:

1. **Create esbuild config template**:
```javascript
// scripts/build-template.mjs
import esbuild from 'esbuild';
import { readFileSync } from 'fs';

const pkg = JSON.parse(readFileSync('./package.json', 'utf-8'));

await esbuild.build({
  entryPoints: ['src/index.mjs'],
  bundle: true,
  format: 'esm',
  platform: 'node',
  target: 'node18',
  outdir: 'dist',
  external: [
    ...Object.keys(pkg.dependencies || {}),
    ...Object.keys(pkg.peerDependencies || {})
  ],
  sourcemap: true,
  minify: false
});

console.log('✅ Build complete');
```

2. **Update package.json scripts**:
```json
{
  "scripts": {
    "build": "node ../../scripts/build-template.mjs && tsc --emitDeclarationOnly"
  }
}
```

3. **Migrate packages incrementally**:
   - Start with simple packages (core, validation)
   - Then move to complex packages (oxigraph, graph-analytics)
   - Finally migrate docs (if beneficial)

**Expected Impact**: -70% build time (20s → 6s)

---

### 14. Add Continuous Performance Monitoring

**Setup**:
```javascript
// scripts/benchmark.mjs
import { performance } from 'perf_hooks';

const results = {
  timestamp: Date.now(),
  buildTime: null,
  testTime: null,
  memory: null
};

// Measure build time
const buildStart = performance.now();
await $`pnpm build:core`;
results.buildTime = performance.now() - buildStart;

// Measure test time
const testStart = performance.now();
await $`pnpm test`;
results.testTime = performance.now() - testStart;

// Measure memory
results.memory = process.memoryUsage();

// Store results
await fs.writeFile(
  `benchmarks/${Date.now()}.json`,
  JSON.stringify(results, null, 2)
);
```

**CI Integration**:
```yaml
# .github/workflows/benchmark.yml
name: Performance Benchmark
on: [push, pull_request]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: pnpm install
      - run: node scripts/benchmark.mjs
      - uses: benchmark-action/github-action-benchmark@v1
        with:
          tool: 'customBiggerIsBetter'
          output-file-path: benchmarks/*.json
```

**Expected Impact**: Prevent future regressions, track improvements

---

## Implementation Checklist

### Phase 1: Critical Fixes (30 minutes)
- [ ] Create `/home/user/unrdf/packages/atomvm/tsconfig.json`
- [ ] Fix `/home/user/unrdf/packages/core/build.config.mjs`
- [ ] Fix validation knowledge-engine import
- [ ] Install `@dagrejs/graphlib` for graph-analytics
- [ ] Run `pnpm install --force`
- [ ] Verify all packages build successfully
- [ ] Run full benchmark suite (3x iterations)

### Phase 2: High Priority Opts (1 hour)
- [ ] Split docs builds in root package.json
- [ ] Add parallel build flags
- [ ] Optimize CLI lazy imports
- [ ] Fix unbuild hangs in oxigraph/atomvm
- [ ] Re-run benchmarks to measure improvements

### Phase 3: Medium Priority Opts (4 hours)
- [ ] Install and configure Turborepo
- [ ] Profile memory usage per package
- [ ] Optimize WASM loading
- [ ] Document findings

### Phase 4: Long-term Opts (1-2 days)
- [ ] Migrate 1-2 packages to esbuild (test)
- [ ] If successful, migrate all packages
- [ ] Set up continuous benchmarking
- [ ] Create regression tests

---

## Success Metrics

### After Phase 1 (Critical Fixes)
- ✅ All packages build without errors
- ✅ Build time <20s (baseline established)
- ✅ Test suite runs completely
- ✅ Full benchmarks possible (3x iterations)

### After Phase 2 (High Priority)
- ✅ Build time <10s (core packages)
- ✅ Test time <3s per package
- ✅ No build hangs or timeouts
- ✅ 90%+ test pass rate

### After Phase 3 (Medium Priority)
- ✅ Build time <8s (with caching)
- ✅ Memory usage <90 MB baseline
- ✅ Cache hit rate >80% on re-builds
- ✅ Memory profiling automated

### After Phase 4 (Long-term)
- ✅ Build time <6s (esbuild migration)
- ✅ Continuous monitoring active
- ✅ Performance regressions caught in CI
- ✅ All targets met or exceeded

---

## Measurement & Validation

### Before Each Change
```bash
# Capture baseline
time timeout 30s pnpm -r build 2>&1 | tee baseline-build.log
time timeout 60s pnpm -r test 2>&1 | tee baseline-test.log
node -e "console.log(JSON.stringify(process.memoryUsage()))" > baseline-memory.json
```

### After Each Change
```bash
# Measure improvement
time timeout 30s pnpm -r build 2>&1 | tee optimized-build.log
time timeout 60s pnpm -r test 2>&1 | tee optimized-test.log
node -e "console.log(JSON.stringify(process.memoryUsage()))" > optimized-memory.json

# Calculate delta
node scripts/compare-benchmarks.mjs baseline-*.log optimized-*.log
```

### Validation Criteria
- Build time improvement ≥10%
- Test time improvement ≥5%
- Memory usage improvement ≥10%
- No regressions in test pass rate
- No new errors or warnings

---

## Rollback Plan

If optimization causes issues:

```bash
# Revert last commit
git revert HEAD

# Or reset to known-good state
git reset --hard <commit-sha>

# Reinstall dependencies
pnpm install --force

# Verify builds work
pnpm -r build
pnpm -r test
```

**Rollback Triggers**:
- Test pass rate drops >5%
- Build time increases
- New critical errors appear
- Memory usage increases >20%

---

## Adversarial PM Validation

### Claims to Verify
- ❓ Did optimizations RUN successfully?
  - **Verify**: Check build/test logs for errors

- ❓ Did performance actually IMPROVE?
  - **Verify**: Compare before/after timing with `time` command

- ❓ What BREAKS if wrong?
  - **Impact**: Slower builds, higher memory, test failures
  - **Mitigation**: Rollback plan documented above

- ❓ Can results be REPRODUCED?
  - **Verify**: Run benchmarks 3x, average results
  - **Document**: Save logs and timing data

### Evidence Required
- [ ] Build logs showing timing improvements
- [ ] Test logs showing no regressions
- [ ] Memory measurements before/after
- [ ] Benchmark results (3x iterations)
- [ ] No new errors or warnings

---

## Next Actions

1. **Immediate**: Implement P0 critical fixes (30 min)
2. **Today**: Run full benchmark suite after fixes (1 hour)
3. **This Week**: Implement P1 high priority opts (4 hours)
4. **Next Sprint**: Implement P2/P3 long-term opts (2 days)

**Owner**: Development team
**Reviewer**: Performance engineer / Tech lead
**Timeline**: Phase 1-2 by end of day, Phase 3-4 by end of sprint
