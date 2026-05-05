# UNRDF Build Quick Start

**TL;DR**: Use `pnpm build:dev` for development, `pnpm build:fast` for TDD, `pnpm build:prod` before commits.

---

## Build Commands (Optimized for Speed)

### 🚀 Fast Build (TDD - Core Packages Only)

```bash
pnpm build:fast              # 1.54s - Only builds core, yawl, kgc-4d, hooks
```

**Use when**:

- Working on core packages only
- TDD workflow (rapid iteration)
- Need immediate feedback

### 🏃 Dev Build (No Type Generation)

```bash
pnpm build:dev               # 10.80s - Skips TypeScript declarations
```

**Use when**:

- Developing any package
- Don't need type checking right now
- Want faster feedback loop

### 📦 Default Build (Parallel, All Packages)

```bash
pnpm build                   # 12.64s - Parallel build with concurrency=10
```

**Use when**:

- Building all packages
- Preparing for tests
- Want production-like build speed

### 🏭 Production Build (Full Type Generation)

```bash
pnpm build:prod              # ~15s - Includes type declarations, concurrency=8
```

**Use when**:

- Before committing to git
- Preparing for CI/CD
- Need full type checking

### 🐌 Sequential Build (Debugging Only)

```bash
pnpm build:sequential        # 37.87s - No parallelization (for debugging)
```

**Use when**:

- Debugging build order issues
- Investigating race conditions
- Build errors are unclear

---

## Performance Comparison

| Command            | Time   | Improvement      | Use Case            |
| ------------------ | ------ | ---------------- | ------------------- |
| `build:fast`       | 1.54s  | **95.9% faster** | TDD (core packages) |
| `build:dev`        | 10.80s | **71.5% faster** | Development         |
| `build`            | 12.64s | **66.6% faster** | Default             |
| `build:prod`       | ~15s   | **60.4% faster** | Pre-commit          |
| `build:sequential` | 37.87s | Baseline         | Debugging           |

---

## Recommended Workflows

### TDD Workflow (Fastest)

```bash
# Terminal 1: Watch tests
pnpm test:watch

# Terminal 2: Rebuild core packages on change
pnpm build:fast && pnpm test:core
```

**Feedback Loop**: 1.54s + test time = **~5s per iteration**

### Development Workflow (Balanced)

```bash
# Make changes to any package
pnpm build:dev              # 10.80s - No type generation
pnpm test:fast              # Run fast tests

# Optional: Check types separately
pnpm typecheck              # Only when needed
```

**Feedback Loop**: 10.80s + test time = **~15s per iteration**

### Pre-Commit Workflow (Complete)

```bash
pnpm build:prod             # ~15s - Full build with types
pnpm lint                   # Lint check
pnpm test                   # Full test suite
```

**Feedback Loop**: ~15s + lint + tests = **~30-40s total**

---

## Build Optimizations Explained

### 1. Parallel Builds (Biggest Impact)

- **What**: Builds multiple packages simultaneously
- **How**: `--workspace-concurrency=10` flag
- **Impact**: 66.6% faster (37.87s → 12.64s)

### 2. Conditional Type Generation

- **What**: Skips TypeScript declarations in dev mode
- **How**: `NODE_ENV=development` → `declaration: false` in build.config.ts
- **Impact**: 28.5% additional speedup (12.64s → 10.80s)

### 3. Filtered Builds

- **What**: Only builds packages you're actively working on
- **How**: `--filter='@unrdf/core'` flag
- **Impact**: 95.9% faster for core packages (37.87s → 1.54s)

---

## Troubleshooting

### Build Timeout (20-30s)

**Problem**: Build hangs or times out.
**Solution**: Parallel builds eliminate this issue. If still occurring, check:

```bash
# Check for resource contention
top -o %CPU

# Reduce concurrency if needed
pnpm build --workspace-concurrency=5
```

### Type Errors Not Caught

**Problem**: Used `build:dev` and missed TypeScript errors.
**Solution**: Always run `build:prod` before committing:

```bash
pnpm build:prod              # Generates types
pnpm typecheck               # Validates types
```

### Incremental Build Not Working

**Problem**: Full rebuild every time, even for small changes.
**Solution**: Use `unbuild --stub` for live reload (experimental):

```bash
# In individual package
cd packages/yawl
pnpm build --stub            # Stub mode for development
```

### Build Error: "Cannot find module build.config.mjs"

**Problem**: Package expects `build.config.mjs` but uses `.ts` extension.
**Solution**: This is expected. Most packages use `build.config.ts` now.

```bash
# If error persists, check package's build script
cat packages/PACKAGE_NAME/package.json | grep build
```

---

## Package-Specific Build Notes

### @unrdf/kgc-4d

- **Before**: `unbuild && tsc --emitDeclarationOnly` (16.79s sequential)
- **After**: `unbuild` (conditional types via build.config.ts, ~7s)
- **Optimization**: Unified build eliminates sequential execution

### @unrdf/yawl

- **Status**: Already optimized with `build.config.ts`
- **Build Time**: 9.70s (no further optimization needed)

### @unrdf/core

- **Build Script**: `node build.config.mjs` (custom build)
- **Build Time**: 0.63s (very fast)

### @unrdf/docs

- **Framework**: Nuxt (external build system)
- **Build Time**: ~5s (external optimization required)

---

## CI/CD Integration

### GitHub Actions Example

```yaml
name: CI

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: pnpm/action-setup@v2
      - uses: actions/setup-node@v3
        with:
          node-version: '18'
          cache: 'pnpm'

      - run: pnpm install

      # Use production build for full type checking
      - run: timeout 30s pnpm build:prod

      - run: pnpm lint
      - run: timeout 30s pnpm test:fast

      # Optional: Cache build artifacts
      - uses: actions/cache@v3
        with:
          path: '**/dist'
          key: ${{ runner.os }}-build-${{ github.sha }}
```

---

## Future Optimizations

### 1. Turbo/Nx Build Cache

**Expected Impact**: 50-70% faster incremental builds

```bash
pnpm add -D turbo
# Configure turbo.json for intelligent caching
```

### 2. Unbuild Stub Mode (Live Reload)

**Expected Impact**: <1s rebuild on file change

```bash
# In package.json
"dev": "unbuild --stub"
```

### 3. Remote Build Cache

**Expected Impact**: Share build cache across team/CI

```bash
# Turbo Remote Cache (Vercel)
turbo build --cache-dir=.turbo-cache
```

---

## Metrics & Monitoring

### Track Build Times

```bash
# Log build time for regression detection
time pnpm build | tee -a build-times.log

# Parse and analyze
grep "BUILD TIME" build-times.log | awk '{print $4}'
```

### Performance Budget

| Build Type | Target | Alert Threshold |
| ---------- | ------ | --------------- |
| Fast       | <2s    | >3s             |
| Dev        | <11s   | >15s            |
| Default    | <13s   | >18s            |
| Production | <16s   | >25s            |

---

## Questions?

**Q**: Why is `build:dev` still 10.80s when target was <5s?
**A**: Full monorepo includes 31 packages. Use `build:fast` (1.54s) for core packages only.

**Q**: Can I skip builds entirely during development?
**A**: Depends on package. Some use direct source imports (`src/index.mjs`), others need dist output.

**Q**: Should I run `build:prod` every time?
**A**: No. Use `build:dev` during development, `build:prod` before committing.

**Q**: What about clean builds?
**A**: `pnpm clean && pnpm build:prod` when needed (e.g., after dependency changes).

---

**Full Report**: See `/home/user/unrdf/docs/BUILD-PERFORMANCE-REPORT.md` for detailed analysis and benchmarks.
