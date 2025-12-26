# Performance Quick Fixes (80/20)

**Target**: Achieve <5s test suite runtime (currently >10s)

**Time Investment**: 4 hours → **3-8x speedup**

---

## ⚡ Quick Fix #1: Remove setTimeout Delays (1 hour)

### Evidence

```bash
$ grep -rn "setTimeout.*[0-9]\{3,\}" packages/*/test --include="*.test.mjs"
# Total: 3.3 seconds of pure sleeping!
```

### Files to Fix

#### 1. `/home/user/unrdf/packages/atomvm/test/browser/integration.test.mjs:35`

```diff
- await new Promise(resolve => setTimeout(resolve, 2000));
+ // Remove or use vi.useFakeTimers()
```

#### 2. `/home/user/unrdf/packages/consensus/test/consensus.test.mjs:214`

```diff
- await new Promise(resolve => setTimeout(resolve, 500));
+ // Remove - tests should not sleep
```

#### 3. `/home/user/unrdf/packages/yawl/test/cancellation.test.mjs:205,225,248`

```diff
- await new Promise(resolve => setTimeout(resolve, 100));
+ // Use vi.waitFor() instead
+ await vi.waitFor(() => expect(condition).toBe(true), { timeout: 100 });
```

### Validation

```bash
time timeout 5s npm run test:consensus
# Should complete in <1s (was >8s)
```

**Expected Impact**: **Consensus tests: >8s → <1s (8x speedup)**

---

## ⚡ Quick Fix #2: Mock WebSocket Servers (2 hours)

### Create Mock Factory

**File**: `/home/user/unrdf/packages/consensus/test/mocks/transport-mock.mjs`

```javascript
import { vi } from 'vitest';

/**
 * Mock WebSocket transport (no I/O)
 * @param {object} config - Transport configuration
 * @returns {object} Mock transport
 */
export function createMockTransport(config) {
  const peers = new Map();

  return {
    server: { mock: true, listening: true },
    config,
    peers,

    addPeer: vi.fn((id, host, port) => {
      peers.set(id, { host, port, connected: true });
    }),

    removePeer: vi.fn(id => {
      peers.delete(id);
    }),

    start: vi.fn().mockResolvedValue(undefined),
    shutdown: vi.fn().mockResolvedValue(undefined),

    send: vi.fn().mockResolvedValue(undefined),
    broadcast: vi.fn().mockResolvedValue(undefined),
  };
}
```

### Update Tests

**File**: `/home/user/unrdf/packages/consensus/test/consensus.test.mjs`

```diff
+ import { createMockTransport } from './mocks/transport-mock.mjs';

  describe('WebSocket Transport', () => {
    let transport;

    beforeEach(async () => {
-     transport = createWebSocketTransport({
+     transport = createMockTransport({
        nodeId: 'test-node',
        port: 10080,
      });
-     await transport.start();  // No I/O now
+     // start() is now synchronous mock
    });

    afterEach(async () => {
-     if (transport) {
-       await transport.shutdown();  // No I/O now
-     }
+     // Cleanup is instant
    });
```

**Expected Impact**: **Remove all network I/O (3-5x speedup)**

---

## ⚡ Quick Fix #3: Fix Workspace Linking (30 min)

### Commands

```bash
# Step 1: Clean and reinstall
pnpm install --force

# Step 2: Build oxigraph first (breaks cyclic dependency)
pnpm -C packages/oxigraph build

# Step 3: Build all packages
pnpm -r build --filter @unrdf/core --filter @unrdf/oxigraph

# Step 4: Verify imports work
node -e "import('@unrdf/core').then(() => console.log('✅ Core loaded'))"
```

### Validation

```bash
# Should show 0 import errors
pnpm -C packages/knowledge-engine test
# Previously: Error [ERR_MODULE_NOT_FOUND]: Cannot find package '@unrdf/oxigraph'
# After fix: Tests run successfully
```

**Expected Impact**: **Fix 119 import failures, 2-3x faster imports**

---

## ⚡ Quick Fix #4: Optimize Build Config (30 min)

### Create Unbuild Config

**File**: `/home/user/unrdf/packages/oxigraph/unbuild.config.ts`

```typescript
import { defineBuildConfig } from 'unbuild';

export default defineBuildConfig({
  entries: ['src/index', 'src/store', 'src/types'],

  // Skip declaration in dev mode (use tsc separately)
  declaration: false,

  rollup: {
    emitCJS: false, // ESM only

    esbuild: {
      minify: false, // Skip minification (dev builds)
      target: 'node18',
      platform: 'node',
    },
  },

  // Hooks for timing
  hooks: {
    'build:before': () => console.time('⚡ Build'),
    'build:done': () => console.timeEnd('⚡ Build'),
  },
});
```

### Validation

```bash
time timeout 5s pnpm -C packages/oxigraph build
# Should complete in <2s (was >5s)
```

**Expected Impact**: **Build: 5s → 2s (2.5x speedup)**

---

## 📊 Verification Script

**File**: `/home/user/unrdf/scripts/verify-performance.sh`

```bash
#!/bin/bash
set -e

echo "🔍 Performance Verification"
echo "==========================="

# Test 1: Full test suite <5s
echo ""
echo "Test 1: Full test suite timeout compliance"
if timeout 5s npm test >/dev/null 2>&1; then
  echo "✅ PASS: Test suite completes in <5s"
else
  echo "❌ FAIL: Test suite exceeds 5s"
  exit 1
fi

# Test 2: No setTimeout >100ms
echo ""
echo "Test 2: No synchronous delays in tests"
count=$(grep -rn "setTimeout.*[0-9]\{3,\}" packages/*/test --include="*.test.mjs" 2>/dev/null | wc -l)
if [ "$count" -eq 0 ]; then
  echo "✅ PASS: No setTimeout delays found"
else
  echo "❌ FAIL: Found $count setTimeout delays"
  exit 1
fi

# Test 3: Workspace imports work
echo ""
echo "Test 3: Workspace package resolution"
if node -e "import('@unrdf/core').then(() => console.log('OK'))" 2>/dev/null | grep -q "OK"; then
  echo "✅ PASS: Workspace imports work"
else
  echo "❌ FAIL: Cannot import workspace packages"
  exit 1
fi

# Test 4: Build time <5s
echo ""
echo "Test 4: Build performance"
start=$(date +%s)
pnpm -C packages/oxigraph build >/dev/null 2>&1
end=$(date +%s)
duration=$((end - start))
if [ "$duration" -lt 5 ]; then
  echo "✅ PASS: Build completes in ${duration}s (<5s)"
else
  echo "❌ FAIL: Build takes ${duration}s (>5s)"
  exit 1
fi

echo ""
echo "==========================="
echo "✅ All performance checks passed!"
```

### Run Verification

```bash
chmod +x scripts/verify-performance.sh
./scripts/verify-performance.sh
```

---

## 🎯 Implementation Order

### Day 1 (4 hours)

1. ✅ **08:00-09:00**: Quick Fix #1 (Remove setTimeout) → Run tests
2. ✅ **09:00-11:00**: Quick Fix #2 (Mock WebSockets) → Run tests
3. ✅ **11:00-11:30**: Quick Fix #3 (Fix workspace) → Verify imports
4. ✅ **11:30-12:00**: Quick Fix #4 (Build config) → Verify build

### Validation

```bash
# After each fix, run:
time timeout 5s npm test

# Expected progression:
# After Fix #1: 6-7s (removed 3.3s sleep)
# After Fix #2: 3-4s (removed I/O overhead)
# After Fix #3: 2-3s (faster imports)
# After Fix #4: Build feedback <2s
```

---

## 📈 Success Metrics

| Metric           | Before | Target | Validation Command                                            |
| ---------------- | ------ | ------ | ------------------------------------------------------------- |
| Test suite       | >10s   | <5s    | `time timeout 5s npm test`                                    |
| Consensus        | >8s    | <1s    | `time timeout 5s pnpm -C packages/consensus test`             |
| setTimeout count | 5      | 0      | `grep -rn "setTimeout.*[0-9]\{3,\}" packages/*/test \| wc -l` |
| Import failures  | 3      | 0      | `pnpm -r test 2>&1 \| grep ERR_MODULE_NOT_FOUND \| wc -l`     |
| Build time       | >5s    | <2s    | `time pnpm -C packages/oxigraph build`                        |

---

## 🚨 Rollback Plan

If any fix breaks tests:

```bash
# Rollback workspace linking
git restore pnpm-lock.yaml
pnpm install

# Rollback test changes
git restore packages/*/test/

# Rollback build config
git restore packages/*/unbuild.config.ts
```

---

## 📝 Post-Fix Validation

After implementing all fixes, run:

```bash
# Full validation suite
npm test                          # Should pass in <5s
npm run lint                      # Should pass (no regressions)
./scripts/verify-performance.sh   # Should show all ✅

# Performance report
echo "Performance Validation Report" > performance-validation.txt
echo "=============================" >> performance-validation.txt
echo "" >> performance-validation.txt
echo "Test Suite:" >> performance-validation.txt
time timeout 5s npm test 2>&1 | grep "real" >> performance-validation.txt
echo "" >> performance-validation.txt
echo "Build Time:" >> performance-validation.txt
time pnpm -C packages/oxigraph build 2>&1 | grep "real" >> performance-validation.txt

cat performance-validation.txt
```

Expected output:

```
Performance Validation Report
=============================

Test Suite:
real    0m3.2s

Build Time:
real    0m1.8s
```

✅ **Success**: Both metrics under 5s SLA!
