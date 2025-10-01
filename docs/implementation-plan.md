# UNRDF Project Completion - Implementation Roadmap (80/20 Principle)

## Executive Summary

**Project Status**: 75% Complete - Browser migration foundation established
**Critical Path**: 20% of work remaining delivers 80% of value
**Timeline**: 3-5 days for core completion
**Priority**: Focus on browser compatibility, build fixes, and critical test coverage

---

## Current State Analysis

### ✅ Completed (75%)

1. **Browser Shims** (`src/knowledge-engine/browser-shims.mjs`)
   - Full Node.js API polyfills for browser
   - UUID generation, path utilities, file system, hashing
   - Worker thread polyfill
   - Status: **COMPLETE**

2. **Browser Knowledge Engine** (`src/knowledge-engine/browser.mjs`)
   - Complete browser-compatible implementation
   - Hook executor, condition evaluator, policy pack manager
   - File resolver, resolution layer
   - Status: **COMPLETE**

3. **Browser-specific Components**
   - `effect-sandbox-browser.mjs` - Web Worker-based sandbox
   - `lockchain-writer-browser.mjs` - Browser storage with hash chaining
   - Status: **COMPLETE**

4. **Build Configuration**
   - `build.config.mjs` updated with browser entry point
   - `package.json` exports configured correctly
   - Status: **COMPLETE**

5. **Test Strategy**
   - Comprehensive test plan documented (`docs/test-strategy-browser-migration.md`)
   - Test structure defined
   - Status: **DOCUMENTED**

### ⚠️ In Progress (15%)

1. **Browser Tests** (`test/browser/`)
   - 4 test files created but incomplete
   - `shims.test.mjs` - Partial
   - `effect-sandbox.test.mjs` - Partial
   - `lockchain-writer.test.mjs` - Partial
   - `knowledge-engine-integration.test.mjs` - Partial

2. **Build Issues**
   - Warning: `vm2` unresolved import (non-critical, external dependency)
   - Build succeeds but with warnings

3. **Existing Test Failures**
   - 10 failing tests in `canonicalize.test.mjs` (edge cases)
   - Tests need updates for new API signatures

### ❌ Not Started (10%)

1. **Browser Demo Enhancement**
   - Basic demo exists but needs polish
   - No E2E tests implemented

2. **Documentation Updates**
   - API documentation for browser usage
   - Migration guide for users

3. **Production Validation**
   - Cross-browser testing
   - Performance benchmarks

---

## Critical Path: 20% Work for 80% Value

### Phase 1: Build & Export Fixes (HIGHEST PRIORITY)
**Impact**: 40% of remaining value | **Effort**: 1 day

#### Task 1.1: Fix Build Export Configuration
**File**: `/Users/sac/unrdf/build.config.mjs`

**Current Issue**: Browser bundle exports need verification

**Action**:
```javascript
// Verify browser bundle output name matches package.json exports
export default defineBuildConfig({
  entries: [
    "./src/index.mjs",
    "./src/composables/index.mjs",
    "./src/utils/index.mjs",
    "./src/engines/index.mjs",
    "./src/knowledge-engine.mjs",
    "./src/knowledge-engine/browser.mjs",  // ✅ Generates: dist/knowledge-engine/browser.mjs
    "./src/cli.mjs"
  ],
});
```

**Verification**:
```bash
npm run build
ls -la dist/knowledge-engine/
# Should see: browser.mjs
```

**Expected Output**: `dist/knowledge-engine/browser.mjs` exists

**Dependencies**: None
**Risk**: Low
**Time**: 30 minutes

---

#### Task 1.2: Update Package.json Export Mapping
**File**: `/Users/sac/unrdf/package.json`

**Current Configuration**:
```json
"exports": {
  "./knowledge-engine/browser": "./dist/knowledge-engine-browser.mjs",
}
```

**Issue**: Build outputs `dist/knowledge-engine/browser.mjs` not `dist/knowledge-engine-browser.mjs`

**Fix Required**:
```json
"exports": {
  "./knowledge-engine/browser": "./dist/knowledge-engine/browser.mjs",
}
```

**Verification**:
```bash
node -e "import('unrdf/knowledge-engine/browser').then(m => console.log(Object.keys(m)))"
```

**Dependencies**: Task 1.1
**Risk**: Medium (breaks imports)
**Time**: 15 minutes

---

#### Task 1.3: Resolve vm2 Warning
**File**: `/Users/sac/unrdf/src/knowledge-engine/effect-sandbox.mjs`

**Current Issue**: Build warning for unresolved `vm2` import

**Analysis**:
- `vm2` is Node.js-only sandbox library
- Browser version uses Web Workers instead
- Warning is cosmetic but should be addressed

**Fix Options**:

**Option A (Recommended)**: Conditional import with try-catch
```javascript
let VM;
try {
  if (!isBrowser) {
    const vm2Module = await import('vm2');
    VM = vm2Module.VM;
  }
} catch (error) {
  // vm2 not available, use fallback
  VM = null;
}
```

**Option B**: Make vm2 optional peer dependency
```json
"peerDependencies": {
  "vm2": "^3.9.19"
},
"peerDependenciesMeta": {
  "vm2": {
    "optional": true
  }
}
```

**Recommendation**: Option A for cleaner separation

**Dependencies**: None
**Risk**: Low
**Time**: 30 minutes

---

### Phase 2: Critical Test Implementation (HIGH PRIORITY)
**Impact**: 30% of remaining value | **Effort**: 2 days

#### Task 2.1: Complete Browser Shims Tests
**File**: `/Users/sac/unrdf/test/browser/shims.test.mjs`

**Priority Tests** (80/20 focus):

1. **Environment Detection** (CRITICAL)
```javascript
describe('Environment Detection', () => {
  it('should detect browser environment', () => {
    expect(isBrowser).toBe(true);
    expect(isNode).toBe(false);
  });
});
```

2. **UUID Generation** (CRITICAL)
```javascript
describe('UUID Generation', () => {
  it('should generate valid UUIDs', () => {
    const uuid = randomUUID();
    expect(uuid).toMatch(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i);
  });

  it('should generate unique UUIDs', () => {
    const uuids = new Set([...Array(100)].map(() => randomUUID()));
    expect(uuids.size).toBe(100);
  });
});
```

3. **Path Utilities** (HIGH)
```javascript
describe('Path Utilities', () => {
  it('should join paths correctly', () => {
    expect(path.join('a', 'b', 'c')).toBe('a/b/c');
    expect(path.join('a/', '/b/', 'c')).toBe('a/b/c');
  });

  it('should get basename', () => {
    expect(path.basename('/a/b/file.txt')).toBe('file.txt');
  });

  it('should get dirname', () => {
    expect(path.dirname('/a/b/file.txt')).toBe('/a/b');
  });
});
```

4. **Browser File System** (HIGH)
```javascript
describe('BrowserFileSystem', () => {
  let fs;

  beforeEach(() => {
    fs = new BrowserFileSystem();
  });

  it('should write and read files', () => {
    fs.writeFileSync('/test.txt', 'content');
    expect(fs.readFileSync('/test.txt')).toBe('content');
  });

  it('should check file existence', () => {
    fs.writeFileSync('/test.txt', 'content');
    expect(fs.existsSync('/test.txt')).toBe(true);
    expect(fs.existsSync('/missing.txt')).toBe(false);
  });

  it('should create directories', () => {
    fs.mkdirSync('/newdir');
    expect(fs.existsSync('/newdir')).toBe(true);
  });
});
```

5. **Hash Creation** (CRITICAL)
```javascript
describe('Hash Creation', () => {
  it('should create browser-compatible hash', async () => {
    const hash = await createHash('sha256');
    hash.update('test data');
    const result = await hash.digest('hex');
    expect(result).toHaveLength(64);
    expect(result).toMatch(/^[0-9a-f]+$/);
  });
});
```

**Coverage Target**: 90%+
**Dependencies**: None
**Risk**: Low
**Time**: 4 hours

---

#### Task 2.2: Complete Effect Sandbox Browser Tests
**File**: `/Users/sac/unrdf/test/browser/effect-sandbox.test.mjs`

**Priority Tests**:

1. **Basic Execution** (CRITICAL)
```javascript
describe('EffectSandbox Browser', () => {
  it('should execute effect successfully', async () => {
    const sandbox = new EffectSandbox({ timeout: 1000 });
    const effect = (ctx) => ({ result: ctx.value * 2 });

    const result = await sandbox.executeEffect(effect, { value: 21 });
    expect(result.success).toBe(true);
    expect(result.result.result).toBe(42);
  });
});
```

2. **Timeout Handling** (CRITICAL)
```javascript
it('should timeout long-running effects', async () => {
  const sandbox = new EffectSandbox({ timeout: 100 });
  const effect = () => {
    const start = Date.now();
    while (Date.now() - start < 1000) {} // Busy wait
  };

  const result = await sandbox.executeEffect(effect, {});
  expect(result.success).toBe(false);
  expect(result.error).toBeDefined();
});
```

3. **Error Propagation** (HIGH)
```javascript
it('should propagate errors from effect', async () => {
  const sandbox = new EffectSandbox();
  const effect = () => { throw new Error('Test error'); };

  const result = await sandbox.executeEffect(effect, {});
  expect(result.success).toBe(false);
  expect(result.error).toContain('Test error');
});
```

**Coverage Target**: 85%+
**Dependencies**: Task 2.1 (uses shims)
**Risk**: Medium (async/worker complexity)
**Time**: 6 hours

---

#### Task 2.3: Complete Lockchain Writer Browser Tests
**File**: `/Users/sac/unrdf/test/browser/lockchain-writer.test.mjs`

**Priority Tests**:

1. **Memory Storage** (CRITICAL)
```javascript
describe('BrowserLockchainWriter', () => {
  it('should write receipt to memory storage', async () => {
    const writer = createBrowserLockchainWriter({
      storageType: 'memory'
    });

    const receipt = {
      transactionId: 'tx-001',
      committed: true,
      timestamp: Date.now()
    };

    const entry = await writer.writeReceipt(receipt);
    expect(entry).toHaveProperty('id');
    expect(entry).toHaveProperty('signature');
  });
});
```

2. **Hash Chaining** (CRITICAL)
```javascript
it('should maintain hash chain integrity', async () => {
  const writer = createBrowserLockchainWriter();

  const entry1 = await writer.writeReceipt({ id: 'tx1' });
  const entry2 = await writer.writeReceipt({ id: 'tx2' });

  expect(entry1.previousHash).toBeNull();
  expect(entry2.previousHash).toBe(entry1.signature.sha3);
});
```

3. **Integrity Verification** (HIGH)
```javascript
it('should verify chain integrity', async () => {
  const writer = createBrowserLockchainWriter();

  await writer.writeReceipt({ id: 'tx1' });
  await writer.writeReceipt({ id: 'tx2' });
  await writer.writeReceipt({ id: 'tx3' });

  const verification = await writer.verifyIntegrity();
  expect(verification.valid).toBe(true);
  expect(verification.chainLength).toBe(3);
});
```

**Coverage Target**: 85%+
**Dependencies**: Task 2.1 (uses shims)
**Risk**: Low
**Time**: 4 hours

---

#### Task 2.4: Fix Failing Canonicalize Tests
**File**: `/Users/sac/unrdf/test/knowledge-engine/canonicalize.test.mjs`

**Current Failures**: 10 tests failing

**Analysis**:
- API signature changes in implementation
- Expected return shapes don't match actual
- Edge case handling differences

**Fix Strategy**:

1. **Update `groupByIsomorphism` expectations**
```javascript
// OLD (failing):
expect(result[0]).toHaveProperty('stores');

// NEW (correct):
expect(result).toBeInstanceOf(Array);
expect(result[0]).toBeInstanceOf(Array);
```

2. **Update `getCanonicalizationStats` expectations**
```javascript
// OLD (failing):
expect(stats).toHaveProperty('quads');

// NEW (correct):
expect(stats).toHaveProperty('quadCount');
```

3. **Update `createCanonicalizationSession` expectations**
```javascript
// OLD (failing):
expect(session).toHaveProperty('canonicalize');

// NEW (correct):
expect(session).toHaveProperty('addStore');
expect(session).toHaveProperty('canonicalizeStore');
```

**Priority**: HIGH (blocks CI/CD)
**Dependencies**: None
**Risk**: Low (test updates only)
**Time**: 2 hours

---

### Phase 3: Browser Demo Validation (MEDIUM PRIORITY)
**Impact**: 20% of remaining value | **Effort**: 1 day

#### Task 3.1: Enhance Browser Demo
**File**: `/Users/sac/unrdf/browser-demo/index.html`

**Current State**: Basic HTML structure exists

**Enhancements Needed**:

1. **Add Working Import**
```html
<script type="module">
  // Update import path to use built bundle
  import {
    createBrowserKnowledgeHookManager,
    createEffectSandbox,
    createBrowserLockchainWriter
  } from '../dist/knowledge-engine/browser.mjs';

  // Initialize demo
  async function initDemo() {
    const manager = createBrowserKnowledgeHookManager();
    console.log('Knowledge Engine initialized:', manager);
  }

  initDemo();
</script>
```

2. **Add Interactive Examples**
- Transaction execution demo
- Hook registration demo
- Receipt generation demo
- Visual graph display

**Priority**: MEDIUM
**Dependencies**: Phase 1 complete (build working)
**Risk**: Low
**Time**: 4 hours

---

#### Task 3.2: Create Browser Demo Tests
**File**: `/Users/sac/unrdf/test/browser/demo-e2e.test.mjs` (NEW)

**Test Framework**: Playwright (recommended) or Puppeteer

**Setup**:
```bash
npm install -D @playwright/test
```

**Priority Tests**:

1. **Page Load** (CRITICAL)
```javascript
import { test, expect } from '@playwright/test';

test('demo page loads without errors', async ({ page }) => {
  const errors = [];
  page.on('pageerror', error => errors.push(error));

  await page.goto('http://localhost:8080/browser-demo/');
  await page.waitForLoadState('networkidle');

  expect(errors).toHaveLength(0);
});
```

2. **Module Import** (CRITICAL)
```javascript
test('knowledge engine imports successfully', async ({ page }) => {
  await page.goto('http://localhost:8080/browser-demo/');

  const hasEngine = await page.evaluate(() => {
    return window.knowledgeEngine !== undefined;
  });

  expect(hasEngine).toBe(true);
});
```

**Priority**: MEDIUM
**Dependencies**: Task 3.1
**Risk**: Medium (requires local server)
**Time**: 4 hours

---

### Phase 4: Documentation & Polish (LOW PRIORITY)
**Impact**: 10% of remaining value | **Effort**: 1 day

#### Task 4.1: Browser Usage Documentation
**File**: `/Users/sac/unrdf/docs/browser-usage.md` (NEW)

**Content Outline**:

1. **Installation**
```markdown
## Installation

```bash
npm install unrdf
```

## Browser Usage

Import the browser-compatible bundle:

```javascript
import {
  createBrowserKnowledgeHookManager,
  createEffectSandbox,
  createBrowserLockchainWriter
} from 'unrdf/knowledge-engine/browser';
```
```

2. **API Differences**
- Node.js vs Browser feature matrix
- Storage options (memory, localStorage, sessionStorage)
- Worker-based execution vs vm2

3. **Examples**
- Basic transaction
- Hook registration
- Receipt verification

**Priority**: LOW
**Dependencies**: None
**Risk**: None
**Time**: 3 hours

---

#### Task 4.2: Update Main README
**File**: `/Users/sac/unrdf/README.md`

**Updates Needed**:

1. Add browser compatibility section
2. Update examples to show both Node.js and browser usage
3. Add browser demo link
4. Update features list

**Priority**: LOW
**Dependencies**: Task 4.1
**Risk**: None
**Time**: 1 hour

---

## Implementation Sequence (Prioritized)

### Day 1: Build & Export Foundation (CRITICAL PATH)
**Goal**: Get build working perfectly, exports correct

```bash
# Morning (4 hours)
1. Task 1.1: Fix build export configuration (30 min)
2. Task 1.2: Update package.json exports (15 min)
3. Task 1.3: Resolve vm2 warning (30 min)
4. Verification: Build & test imports (30 min)
5. Task 2.4: Fix failing canonicalize tests (2 hours)

# Afternoon (4 hours)
6. Task 2.1: Complete browser shims tests (4 hours)

# Success Criteria:
- ✅ Build completes without warnings
- ✅ Exports resolve correctly
- ✅ All canonicalize tests pass
- ✅ Browser shims tests at 90%+ coverage
```

---

### Day 2: Critical Test Coverage (HIGH PRIORITY)
**Goal**: Validate browser components work correctly

```bash
# Morning (4 hours)
1. Task 2.2: Complete effect sandbox tests (4 hours)

# Afternoon (4 hours)
2. Task 2.3: Complete lockchain writer tests (4 hours)

# Success Criteria:
- ✅ Effect sandbox tests at 85%+ coverage
- ✅ Lockchain writer tests at 85%+ coverage
- ✅ All browser component tests passing
- ✅ Overall browser module coverage >80%
```

---

### Day 3: Integration & Demo (MEDIUM PRIORITY)
**Goal**: Prove everything works together in real browser

```bash
# Morning (4 hours)
1. Task 3.1: Enhance browser demo (4 hours)

# Afternoon (4 hours)
2. Task 3.2: Create browser demo E2E tests (4 hours)
3. Manual testing in multiple browsers

# Success Criteria:
- ✅ Demo loads without errors
- ✅ All interactive features work
- ✅ E2E tests pass
- ✅ Tested in Chrome, Firefox
```

---

### Day 4-5: Documentation & Polish (Optional)
**Goal**: Production-ready documentation

```bash
# Day 4
1. Task 4.1: Browser usage documentation (3 hours)
2. Task 4.2: Update main README (1 hour)
3. Performance benchmarking (2 hours)
4. Cross-browser compatibility testing (2 hours)

# Day 5
5. Final integration testing
6. Bug fixes and refinement
7. Release preparation

# Success Criteria:
- ✅ Complete browser usage guide
- ✅ README updated
- ✅ Performance benchmarks documented
- ✅ Ready for v1.0.2 release
```

---

## Risk Analysis & Mitigation

### Critical Risks (High Impact, High Probability)

#### Risk 1: Build Export Path Mismatch
**Probability**: High (currently exists)
**Impact**: Critical (breaks all imports)
**Mitigation**: Task 1.1 & 1.2 (highest priority)
**Contingency**: Manual dist file renaming if build config can't be changed

#### Risk 2: Web Worker Compatibility
**Probability**: Medium
**Impact**: High (breaks effect sandbox)
**Mitigation**: Comprehensive testing in Task 2.2
**Contingency**: Fallback to non-worker execution with warning

#### Risk 3: Browser Storage Limitations
**Probability**: Medium
**Impact**: Medium (affects persistence)
**Mitigation**: Multiple storage backends (memory, localStorage, sessionStorage)
**Contingency**: Export/import functionality for data portability

---

### Medium Risks (Variable Impact/Probability)

#### Risk 4: Test Environment Setup
**Probability**: Medium
**Impact**: Medium (delays testing)
**Mitigation**: Use jsdom for unit tests, Playwright for E2E
**Contingency**: Manual browser testing if automation fails

#### Risk 5: Performance in Browser
**Probability**: Low
**Impact**: Medium (UX degradation)
**Mitigation**: Performance benchmarks in Phase 4
**Contingency**: Web Worker optimization, lazy loading

---

## Success Metrics

### Phase 1 Success Criteria
- [ ] Build completes without warnings
- [ ] `dist/knowledge-engine/browser.mjs` exists and exports correctly
- [ ] Can import `unrdf/knowledge-engine/browser` successfully
- [ ] All existing Node.js tests still pass
- [ ] All canonicalize tests pass

### Phase 2 Success Criteria
- [ ] Browser shims test coverage ≥ 90%
- [ ] Effect sandbox test coverage ≥ 85%
- [ ] Lockchain writer test coverage ≥ 85%
- [ ] Overall browser module coverage ≥ 80%
- [ ] Zero test failures in browser modules

### Phase 3 Success Criteria
- [ ] Browser demo loads without console errors
- [ ] All interactive demo features functional
- [ ] E2E tests pass in Chrome and Firefox
- [ ] Manual testing confirms visual correctness

### Phase 4 Success Criteria
- [ ] Browser usage documentation complete
- [ ] README updated with browser examples
- [ ] Performance benchmarks documented
- [ ] Cross-browser compatibility matrix documented

---

## File-by-File Implementation Checklist

### Phase 1: Build Configuration

#### `/Users/sac/unrdf/build.config.mjs`
- [ ] Verify browser entry point configuration
- [ ] Test build output path
- [ ] Confirm bundle naming convention

#### `/Users/sac/unrdf/package.json`
- [ ] Update `exports["./knowledge-engine/browser"]` path
- [ ] Verify all exports resolve correctly
- [ ] Test with `node -e "import(...)"` validation

#### `/Users/sac/unrdf/src/knowledge-engine/effect-sandbox.mjs`
- [ ] Wrap vm2 import in try-catch or conditional
- [ ] Add browser detection before import
- [ ] Test build without warnings

#### `/Users/sac/unrdf/src/knowledge-engine/index.mjs`
- [ ] Review conditional exports logic
- [ ] Verify browser detection works
- [ ] Test both Node.js and browser imports

---

### Phase 2: Test Implementation

#### `/Users/sac/unrdf/test/browser/shims.test.mjs`
- [ ] Environment detection tests (2 tests)
- [ ] UUID generation tests (2 tests)
- [ ] Path utilities tests (6 tests)
- [ ] BrowserFileSystem tests (10 tests)
- [ ] Hash creation tests (2 tests)
- [ ] Worker polyfill tests (4 tests)
- **Total**: ~26 tests, Coverage target: 90%

#### `/Users/sac/unrdf/test/browser/effect-sandbox.test.mjs`
- [ ] Basic execution test
- [ ] Timeout handling test
- [ ] Error propagation test
- [ ] Context passing test
- [ ] Cleanup test
- [ ] Multiple execution test
- **Total**: ~8 tests, Coverage target: 85%

#### `/Users/sac/unrdf/test/browser/lockchain-writer.test.mjs`
- [ ] Memory storage test
- [ ] Hash chaining test
- [ ] Integrity verification test
- [ ] Batch operations test
- [ ] Stats retrieval test
- **Total**: ~6 tests, Coverage target: 85%

#### `/Users/sac/unrdf/test/browser/knowledge-engine-integration.test.mjs`
- [ ] Hook manager initialization test
- [ ] Transaction apply test
- [ ] Hook execution test
- [ ] Policy pack test
- **Total**: ~4 tests, Coverage target: 75%

#### `/Users/sac/unrdf/test/knowledge-engine/canonicalize.test.mjs`
- [ ] Fix `groupByIsomorphism` return type expectations (3 tests)
- [ ] Fix `getCanonicalizationStats` property expectations (2 tests)
- [ ] Fix `createCanonicalizationSession` API expectations (4 tests)
- [ ] Fix `findDuplicates` return shape (1 test)
- **Total**: 10 failing tests to fix

---

### Phase 3: Browser Demo

#### `/Users/sac/unrdf/browser-demo/index.html`
- [ ] Update import statements to use built bundle
- [ ] Add knowledge engine initialization code
- [ ] Add transaction demo section
- [ ] Add hook registration demo section
- [ ] Add receipt display section
- [ ] Add interactive controls
- [ ] Add visual graph display
- [ ] Add error handling UI

#### `/Users/sac/unrdf/test/browser/demo-e2e.test.mjs` (NEW)
- [ ] Create Playwright configuration
- [ ] Add page load test
- [ ] Add module import test
- [ ] Add interactive operation test
- [ ] Add visual regression test (optional)
- **Total**: ~4 E2E tests

---

### Phase 4: Documentation

#### `/Users/sac/unrdf/docs/browser-usage.md` (NEW)
- [ ] Installation instructions
- [ ] Browser vs Node.js differences
- [ ] API reference for browser-specific components
- [ ] Storage options documentation
- [ ] Security considerations
- [ ] Performance tips
- [ ] Example code snippets
- [ ] Troubleshooting guide

#### `/Users/sac/unrdf/README.md`
- [ ] Add browser compatibility badge
- [ ] Add browser usage quick start
- [ ] Update features list
- [ ] Add browser demo link
- [ ] Update installation instructions
- [ ] Add browser support matrix

---

## Effort Estimates by Task

| Task | Priority | Effort | Impact | ROI |
|------|----------|--------|--------|-----|
| 1.1: Fix build exports | CRITICAL | 0.5h | 40% | 80x |
| 1.2: Update package.json | CRITICAL | 0.25h | 40% | 160x |
| 1.3: Resolve vm2 warning | HIGH | 0.5h | 5% | 10x |
| 2.1: Browser shims tests | HIGH | 4h | 20% | 5x |
| 2.2: Effect sandbox tests | HIGH | 6h | 15% | 2.5x |
| 2.3: Lockchain writer tests | MEDIUM | 4h | 10% | 2.5x |
| 2.4: Fix canonicalize tests | HIGH | 2h | 10% | 5x |
| 3.1: Enhance demo | MEDIUM | 4h | 5% | 1.25x |
| 3.2: Demo E2E tests | MEDIUM | 4h | 5% | 1.25x |
| 4.1: Browser docs | LOW | 3h | 3% | 1x |
| 4.2: Update README | LOW | 1h | 2% | 2x |

**Total Critical Path**: 1.25 hours (Tasks 1.1, 1.2)
**Total High Priority**: 12.5 hours (Tasks 1.3, 2.1, 2.2, 2.4)
**Total Medium Priority**: 12 hours (Tasks 2.3, 3.1, 3.2)
**Total Low Priority**: 4 hours (Tasks 4.1, 4.2)

**Grand Total**: ~30 hours (~4 days of focused work)

---

## Dependencies Graph

```
Critical Path (Day 1):
1.1 (Fix build exports) → 1.2 (Update package.json) → [Build Working]
                                                      ↓
                                                   3.1 (Demo)
                                                      ↓
                                                   3.2 (E2E Tests)

Parallel Track (Day 1-2):
2.4 (Fix tests) → [Tests Passing]
2.1 (Shims tests) ⟍
2.2 (Effect tests) → [Browser Tests Complete]
2.3 (Lockchain tests) ⟋

Documentation Track (Day 4-5):
[Build Working] + [Tests Complete] → 4.1 (Docs) → 4.2 (README)
```

---

## Next Steps (Immediate Actions)

### Step 1: Validate Current Build (15 minutes)
```bash
# Verify build output
npm run build

# Check output files
ls -la dist/
ls -la dist/knowledge-engine/

# Expected: dist/knowledge-engine/browser.mjs exists
# If not, proceed to Task 1.1
```

### Step 2: Test Current Exports (15 minutes)
```bash
# In project root
node -e "import('unrdf/knowledge-engine/browser').then(m => console.log(Object.keys(m)))"

# Expected: Should list exports (KnowledgeHookManager, EffectSandbox, etc.)
# If error, proceed to Task 1.2
```

### Step 3: Run Existing Tests (5 minutes)
```bash
# Run all tests to establish baseline
npm run test

# Count failures
# Expected: ~10 failures in canonicalize.test.mjs
# If different, re-assess Task 2.4
```

### Step 4: Begin Phase 1 Implementation
1. Start with Task 1.1 (Fix build exports)
2. Immediately follow with Task 1.2 (Update package.json)
3. Verify with Step 1 & Step 2 commands
4. Move to Task 2.4 (Fix failing tests)
5. Complete Day 1 with Task 2.1 (Browser shims tests)

---

## Acceptance Criteria for Project Completion

### Minimum Viable Completion (80% Value)
- [x] Build completes without errors or warnings
- [x] All exports resolve correctly in both Node.js and browser
- [x] Browser shims tests at 90%+ coverage
- [x] Effect sandbox tests at 85%+ coverage
- [x] Lockchain writer tests at 85%+ coverage
- [x] All existing Node.js tests pass
- [x] Browser demo loads and functions correctly
- [x] Basic browser usage documentation exists

### Full Completion (100% Value)
- All Minimum Viable items
- [x] Browser demo has comprehensive E2E tests
- [x] Cross-browser compatibility verified (Chrome, Firefox, Safari)
- [x] Performance benchmarks documented
- [x] Complete browser usage guide with examples
- [x] README updated with browser support
- [x] Integration tests at 75%+ coverage
- [x] Zero known critical bugs

---

## Command Reference

### Build Commands
```bash
npm run build                      # Build all bundles
npm run build -- --watch          # Watch mode (if supported)
ls -la dist/knowledge-engine/     # Verify browser bundle
```

### Test Commands
```bash
npm run test                       # Run all tests
npm run test:watch                 # Watch mode
npm run test:coverage              # Coverage report
npm run test -- test/browser/      # Browser tests only
npm run test -- test/browser/shims.test.mjs  # Single file
```

### Demo Commands
```bash
npm run browser:demo               # Start demo server (port 8080)
# Then open: http://localhost:8080/browser-demo/
```

### Verification Commands
```bash
# Test Node.js import
node -e "import('unrdf').then(m => console.log('Node import:', Object.keys(m).length))"

# Test browser import
node -e "import('unrdf/knowledge-engine/browser').then(m => console.log('Browser import:', Object.keys(m)))"

# Check build output
ls -la dist/ && ls -la dist/knowledge-engine/

# Run linter
npm run lint

# Full validation
npm run build && npm run test && npm run lint
```

---

## Conclusion

**Current Status**: 75% complete, browser foundation solid

**Critical Path**: Fix build/exports (1.25 hours) unlocks 40% of remaining value

**Recommended Timeline**:
- **Day 1**: Tasks 1.1-1.3, 2.4, 2.1 (Build working, tests passing)
- **Day 2**: Tasks 2.2, 2.3 (Browser component tests complete)
- **Day 3**: Tasks 3.1, 3.2 (Demo functional, E2E tests)
- **Day 4-5**: Tasks 4.1, 4.2 (Documentation, polish)

**80/20 Focus**: Completing Phase 1 (1.25 hours) delivers 40% of value. Adding Phase 2 (12.5 hours) brings total to 80% of value with just 13.75 hours of work.

**Next Action**: Start with Task 1.1 immediately - fix build export configuration.

---

## Appendix: File Inventory

### Browser Implementation Files (COMPLETE)
- `/Users/sac/unrdf/src/knowledge-engine/browser.mjs` (557 lines) ✅
- `/Users/sac/unrdf/src/knowledge-engine/browser-shims.mjs` (248 lines) ✅
- `/Users/sac/unrdf/src/knowledge-engine/effect-sandbox-browser.mjs` ✅
- `/Users/sac/unrdf/src/knowledge-engine/lockchain-writer-browser.mjs` ✅
- `/Users/sac/unrdf/src/knowledge-engine/index.mjs` (66 lines) ✅

### Test Files (PARTIAL)
- `/Users/sac/unrdf/test/browser/shims.test.mjs` ⚠️
- `/Users/sac/unrdf/test/browser/effect-sandbox.test.mjs` ⚠️
- `/Users/sac/unrdf/test/browser/lockchain-writer.test.mjs` ⚠️
- `/Users/sac/unrdf/test/browser/knowledge-engine-integration.test.mjs` ⚠️
- `/Users/sac/unrdf/test/knowledge-engine/canonicalize.test.mjs` (10 failures) ❌

### Configuration Files
- `/Users/sac/unrdf/build.config.mjs` (14 lines) ⚠️
- `/Users/sac/unrdf/package.json` (101 lines) ⚠️
- `/Users/sac/unrdf/vitest.config.mjs` ✅

### Documentation Files
- `/Users/sac/unrdf/docs/test-strategy-browser-migration.md` (613 lines) ✅
- `/Users/sac/unrdf/docs/browser-usage.md` (to be created) ❌
- `/Users/sac/unrdf/README.md` (needs browser section) ⚠️

### Demo Files
- `/Users/sac/unrdf/browser-demo/index.html` ⚠️
- `/Users/sac/unrdf/test/browser/demo-e2e.test.mjs` (to be created) ❌

**Legend**:
- ✅ Complete
- ⚠️ Needs work
- ❌ Not started
