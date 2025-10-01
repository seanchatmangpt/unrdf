# Browser Migration Test Strategy (80/20 Focus)

## Executive Summary

This test strategy ensures the browser migration of the UNRDF Knowledge Engine succeeds with minimal but highly effective testing. Following the 80/20 principle, we focus on the 20% of tests that verify 80% of functionality.

**Migration Scope:**
- Browser shims for Node.js APIs
- Browser-compatible knowledge engine
- Effect sandbox (Web Workers)
- Lockchain writer (browser storage)
- Browser demo application

**Testing Framework:** Vitest (already configured)
**Test Environment:** jsdom for browser simulation + real browser E2E

## Critical Path Testing (80/20 Priority)

### Tier 1: Core Compatibility (Must Have - 80% Impact)

These tests verify the fundamental browser compatibility that ensures the system works:

#### 1. Browser Shims (`test/browser/shims.test.mjs`)
**Priority: CRITICAL** - Foundation for all browser functionality

Test Cases:
- ✅ Environment detection (isBrowser/isNode)
- ✅ UUID generation (crypto.randomUUID fallback)
- ✅ Path utilities (join, resolve, dirname, basename, extname)
- ✅ File system operations (BrowserFileSystem CRUD)
- ✅ Hash creation (BrowserHash with Web Crypto API)
- ⚠️ Worker thread polyfill (BrowserWorker lifecycle)

**Coverage Goal:** 90%+ (critical infrastructure)

#### 2. Effect Sandbox Browser (`test/browser/effect-sandbox.test.mjs`)
**Priority: CRITICAL** - Secure hook execution in browser

Test Cases:
- ✅ Web Worker creation and termination
- ✅ Effect execution with timeout handling
- ✅ Sandbox security boundaries (allowed globals)
- ✅ Error propagation from worker
- ✅ Message passing (context → result)
- ⚠️ Memory limits enforcement

**Coverage Goal:** 85%+ (security-critical)

#### 3. Lockchain Writer Browser (`test/browser/lockchain-writer.test.mjs`)
**Priority: HIGH** - Data integrity without Git

Test Cases:
- ✅ Storage initialization (memory/localStorage/sessionStorage)
- ✅ Receipt writing and hash chaining
- ✅ Merkle root calculation
- ✅ Integrity verification
- ✅ Batch operations
- ⚠️ Storage persistence and restoration

**Coverage Goal:** 85%+

### Tier 2: Integration Testing (Should Have - 15% Impact)

#### 4. Browser Knowledge Engine (`test/browser/knowledge-engine-integration.test.mjs`)
**Priority: HIGH** - End-to-end functionality

Test Cases:
- ✅ Hook manager with browser components
- ✅ Transaction apply with browser store
- ✅ Condition evaluation in browser
- ✅ Policy pack loading
- ⚠️ Cross-component interaction

**Coverage Goal:** 75%+

#### 5. Browser Demo (`test/browser/demo-e2e.test.mjs`)
**Priority: MEDIUM** - Real-world validation

Test Cases:
- ✅ Page loads without errors
- ✅ Knowledge graph visualization appears
- ✅ Interactive operations work
- ✅ Console has no critical errors

**Coverage Goal:** 60%+ (smoke testing)

### Tier 3: Edge Cases (Nice to Have - 5% Impact)

#### 6. Browser Compatibility Matrix
**Priority: LOW** - Only if resources allow

- Chrome/Edge (Chromium) - PRIMARY TARGET
- Firefox - SECONDARY
- Safari - IF TIME PERMITS

## Test Structure

### Directory Organization

```
test/
├── browser/                          # Browser-specific tests
│   ├── shims.test.mjs               # CRITICAL
│   ├── effect-sandbox.test.mjs      # CRITICAL
│   ├── lockchain-writer.test.mjs    # HIGH
│   ├── knowledge-engine-integration.test.mjs  # HIGH
│   └── demo-e2e.test.mjs           # MEDIUM
└── knowledge-engine/                # Existing tests (ensure no regression)
    └── transaction.test.mjs         # Reference pattern
```

### Test Configuration

**vitest.config.mjs additions:**

```javascript
export default defineConfig({
  test: {
    // Existing config...
    environment: 'jsdom', // Enable browser simulation
    setupFiles: ['./test/setup-browser.mjs'],
    coverage: {
      include: [
        'src/knowledge-engine/browser-shims.mjs',
        'src/knowledge-engine/browser.mjs',
        'src/knowledge-engine/effect-sandbox-browser.mjs',
        'src/knowledge-engine/lockchain-writer-browser.mjs'
      ],
      thresholds: {
        statements: 80,
        branches: 75,
        functions: 80,
        lines: 80
      }
    }
  }
});
```

## Test Scenarios by Priority

### P0: Browser Shims Functionality (CRITICAL)

**Scenario 1: Environment Detection**
```javascript
it('should detect browser environment correctly', () => {
  expect(isBrowser).toBe(true);
  expect(isNode).toBe(false);
});
```

**Scenario 2: UUID Generation**
```javascript
it('should generate valid UUIDs in browser', () => {
  const uuid = randomUUID();
  expect(uuid).toMatch(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i);
});
```

**Scenario 3: File System Operations**
```javascript
it('should perform CRUD operations in memory', () => {
  const browserFs = new BrowserFileSystem();
  browserFs.writeFileSync('/test.txt', 'content');
  expect(browserFs.readFileSync('/test.txt')).toBe('content');
  expect(browserFs.existsSync('/test.txt')).toBe(true);
});
```

**Scenario 4: Hash Generation**
```javascript
it('should create browser-compatible hashes', async () => {
  const hash = await createHash('sha256');
  const result = await hash.update('test').digest('hex');
  expect(result).toHaveLength(64);
});
```

**Scenario 5: Worker Polyfill**
```javascript
it('should create and communicate with worker', () => {
  const worker = new BrowserWorker('self.postMessage("ready")');
  worker.on('message', (msg) => {
    expect(msg).toBe('ready');
    worker.terminate();
  });
});
```

### P1: Effect Sandbox Security (CRITICAL)

**Scenario 6: Worker Execution**
```javascript
it('should execute effect in worker with timeout', async () => {
  const sandbox = new EffectSandbox({ timeout: 1000 });
  const effect = (ctx) => ctx.value * 2;

  const result = await sandbox.executeEffect(effect, { value: 21 });
  expect(result.success).toBe(true);
  expect(result.result).toBe(42);
});
```

**Scenario 7: Timeout Enforcement**
```javascript
it('should timeout long-running effects', async () => {
  const sandbox = new EffectSandbox({ timeout: 100 });
  const effect = () => { while(true) {} }; // Infinite loop

  const result = await sandbox.executeEffect(effect, {});
  expect(result.success).toBe(false);
  expect(result.error).toContain('timeout');
});
```

**Scenario 8: Sandbox Isolation**
```javascript
it('should restrict access to disallowed globals', async () => {
  const sandbox = new EffectSandbox({
    allowedGlobals: ['Math', 'JSON']
  });
  const effect = (ctx) => {
    return typeof window === 'undefined'; // window should not be accessible
  };

  const result = await sandbox.executeEffect(effect, {});
  expect(result.result).toBe(true);
});
```

### P2: Lockchain Writer Storage (HIGH)

**Scenario 9: Memory Storage**
```javascript
it('should write and verify receipts in memory', async () => {
  const writer = new BrowserLockchainWriter({ storageType: 'memory' });
  const receipt = { id: 'tx1', data: 'test' };

  const entry = await writer.writeReceipt(receipt);
  expect(entry).toHaveProperty('id');
  expect(entry).toHaveProperty('signature');
  expect(entry.previousHash).toBeNull();
});
```

**Scenario 10: Hash Chaining**
```javascript
it('should maintain hash chain integrity', async () => {
  const writer = new BrowserLockchainWriter();
  await writer.writeReceipt({ id: 'tx1' });
  await writer.writeReceipt({ id: 'tx2' });

  const verification = await writer.verifyIntegrity();
  expect(verification.valid).toBe(true);
});
```

**Scenario 11: LocalStorage Persistence**
```javascript
it('should persist to localStorage and restore', async () => {
  const writer1 = new BrowserLockchainWriter({
    storageType: 'localStorage',
    storagePrefix: 'test'
  });
  await writer1.writeReceipt({ id: 'tx1' });

  const writer2 = new BrowserLockchainWriter({
    storageType: 'localStorage',
    storagePrefix: 'test'
  });
  const stats = writer2.getStats();
  expect(stats.totalEntries).toBe(1);
});
```

**Scenario 12: Merkle Root Calculation**
```javascript
it('should calculate merkle root for batch', async () => {
  const writer = new BrowserLockchainWriter({ enableMerkle: true });
  const receipts = [{ id: 'tx1' }, { id: 'tx2' }, { id: 'tx3' }];

  const entries = await writer.writeReceiptBatch(receipts);
  expect(entries[0].merkleRoot).toBeDefined();
  expect(entries[0].merkleRoot).toBe(entries[1].merkleRoot);
});
```

### P3: Integration Tests (HIGH)

**Scenario 13: End-to-End Transaction**
```javascript
it('should execute complete browser transaction flow', async () => {
  const manager = createBrowserKnowledgeHookManager();
  const store = new Store();

  // Add hook
  await manager.addKnowledgeHook({
    meta: { name: 'test-hook' },
    run: (event) => ({ success: true })
  });

  // Execute transaction
  const delta = { additions: [/* quads */], removals: [] };
  const result = await manager.apply(store, delta);

  expect(result.receipt.committed).toBe(true);
});
```

**Scenario 14: Policy Pack Loading**
```javascript
it('should load and activate policy pack in browser', async () => {
  const manager = new BrowserPolicyPackManager();
  await manager.loadPolicyPack('test-pack', {
    hooks: [{ id: 'h1', mode: 'pre' }],
    rules: []
  });

  manager.activatePolicyPack('test-pack');
  const activeHooks = manager.getActiveHooks();
  expect(activeHooks).toHaveLength(1);
});
```

### P4: Browser Demo E2E (MEDIUM)

**Scenario 15: Demo Page Loads**
```javascript
it('should load demo page without errors', async () => {
  // Using Playwright or Puppeteer
  const page = await browser.newPage();
  await page.goto('http://localhost:8080/browser-demo/');

  const errors = [];
  page.on('pageerror', error => errors.push(error));

  await page.waitForSelector('#knowledge-graph');
  expect(errors).toHaveLength(0);
});
```

**Scenario 16: Interactive Operations**
```javascript
it('should execute knowledge operations in demo', async () => {
  const page = await browser.newPage();
  await page.goto('http://localhost:8080/browser-demo/');

  await page.click('#add-triple-btn');
  await page.waitForSelector('.triple-added');

  const tripleCount = await page.textContent('#triple-count');
  expect(parseInt(tripleCount)).toBeGreaterThan(0);
});
```

## Non-Functional Testing

### Performance Benchmarks

**Critical Metrics:**
- UUID generation: < 1ms per call
- Hash calculation: < 10ms for 1KB data
- Worker execution: < 50ms overhead
- Receipt writing: < 20ms per receipt
- Integrity verification: < 100ms for 100 entries

**Test Implementation:**
```javascript
it('should generate UUIDs performantly', () => {
  const start = performance.now();
  for (let i = 0; i < 1000; i++) {
    randomUUID();
  }
  const duration = performance.now() - start;
  expect(duration).toBeLessThan(100); // <0.1ms per UUID
});
```

### Memory Management

**Critical Checks:**
- Worker cleanup after execution
- Storage cache limits
- Event listener cleanup

```javascript
it('should cleanup workers after execution', async () => {
  const sandbox = new EffectSandbox();
  await sandbox.executeEffect(() => 42, {});

  const stats = sandbox.getStats();
  expect(stats.activeWorkers).toBe(0);
});
```

### Browser Compatibility

**Minimum Requirements:**
- Chrome/Edge 90+
- Firefox 88+
- Safari 14+ (if Web Crypto API supported)

**Feature Detection Tests:**
```javascript
it('should detect required browser features', () => {
  expect(typeof crypto !== 'undefined').toBe(true);
  expect(typeof crypto.randomUUID !== 'undefined').toBe(true);
  expect(typeof crypto.subtle !== 'undefined').toBe(true);
  expect(typeof Worker !== 'undefined').toBe(true);
});
```

## Test Execution Plan

### Phase 1: Unit Tests (Week 1)
1. Browser shims tests - **2 days**
2. Effect sandbox tests - **2 days**
3. Lockchain writer tests - **1 day**

### Phase 2: Integration Tests (Week 2)
4. Knowledge engine integration - **2 days**
5. Cross-component workflows - **1 day**
6. Performance benchmarks - **1 day**

### Phase 3: E2E & Demo (Week 3)
7. Browser demo E2E tests - **2 days**
8. Cross-browser testing - **1 day**
9. Bug fixes and refinement - **2 days**

### Continuous Integration

**GitHub Actions Workflow:**
```yaml
name: Browser Tests

on: [push, pull_request]

jobs:
  browser-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '18'
      - run: npm install
      - run: npm run test:browser
      - run: npm run test:browser:e2e
      - uses: codecov/codecov-action@v3
```

## Success Metrics

### Coverage Targets
- Browser shims: **90%+**
- Effect sandbox: **85%+**
- Lockchain writer: **85%+**
- Knowledge engine integration: **75%+**
- Overall browser modules: **80%+**

### Quality Gates
- ✅ All P0 tests passing (100%)
- ✅ All P1 tests passing (100%)
- ✅ >90% P2 tests passing
- ✅ >75% P3 tests passing
- ✅ No critical console errors in demo
- ✅ Performance benchmarks met

### Regression Prevention
- All existing Node.js tests must still pass
- No degradation in Node.js performance
- Dual-environment validation

## Risk Mitigation

### High-Risk Areas

**1. Web Worker Security**
- Risk: Escaping sandbox boundaries
- Mitigation: Comprehensive security tests (Scenario 8)
- Fallback: Global mode with warnings

**2. Storage Persistence**
- Risk: Data loss on browser close
- Mitigation: LocalStorage tests + integrity verification
- Fallback: Memory mode with export/import

**3. Browser API Compatibility**
- Risk: Missing APIs in older browsers
- Mitigation: Feature detection + polyfills
- Fallback: Graceful degradation with warnings

### Test Data Management

**Fixtures:**
```javascript
// test/fixtures/browser-test-data.mjs
export const testQuads = [
  quad(namedNode('http://example.org/s'),
       namedNode('http://example.org/p'),
       literal('o'))
];

export const testReceipts = [
  { id: 'tx1', timestamp: 1234567890, data: {...} }
];
```

## Maintenance Plan

### Test Review Cadence
- Weekly: Review failed tests and flaky tests
- Monthly: Review coverage gaps and test effectiveness
- Quarterly: Revisit 80/20 priorities based on defects

### Documentation Updates
- Update test strategy for new browser features
- Document known browser-specific issues
- Maintain compatibility matrix

### Continuous Improvement
- Monitor test execution time (target: <2 minutes)
- Identify and eliminate flaky tests
- Refactor duplicate test code

## Appendix: Test Templates

### Unit Test Template
```javascript
/**
 * @fileoverview Tests for [module]
 */
import { describe, it, expect, beforeEach, afterEach } from 'vitest';

describe('[Module Name]', () => {
  let instance;

  beforeEach(() => {
    // Setup
    instance = new ModuleClass();
  });

  afterEach(() => {
    // Cleanup
    instance?.cleanup?.();
  });

  it('should [expected behavior]', () => {
    // Arrange
    const input = {...};

    // Act
    const result = instance.method(input);

    // Assert
    expect(result).toBe(expected);
  });
});
```

### Integration Test Template
```javascript
import { describe, it, expect } from 'vitest';

describe('[Integration Scenario]', () => {
  it('should complete end-to-end flow', async () => {
    // Setup multiple components
    const componentA = setupA();
    const componentB = setupB();

    // Execute workflow
    const result = await workflow(componentA, componentB);

    // Verify end state
    expect(result).toMatchObject({...});
  });
});
```

### E2E Test Template
```javascript
import { test, expect } from '@playwright/test';

test('[User Scenario]', async ({ page }) => {
  await page.goto('/browser-demo/');

  await page.click('#action-button');
  await page.waitForSelector('.result');

  const text = await page.textContent('.result');
  expect(text).toContain('success');
});
```

## Conclusion

This test strategy prioritizes the **critical 20% of tests that validate 80% of browser migration functionality**:

1. **Browser shims** - Foundation (CRITICAL)
2. **Effect sandbox** - Security (CRITICAL)
3. **Lockchain writer** - Integrity (HIGH)
4. **Integration** - Workflows (HIGH)
5. **Demo E2E** - Validation (MEDIUM)

By focusing on these high-impact areas, we ensure robust browser compatibility while maintaining development velocity.

**Next Steps:**
1. Review and approve test strategy
2. Create test file stubs
3. Begin Phase 1 implementation
4. Setup CI/CD pipeline
5. Execute testing phases
