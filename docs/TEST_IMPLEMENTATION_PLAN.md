# TEST IMPLEMENTATION PLAN - UNRDF Monorepo

**Priority:** CRITICAL
**Timeline:** 12 weeks (3 phases)
**Target:** 35% → 85% coverage
**Status:** Phase 0 (Planning Complete)

---

## PHASE 1: CRITICAL FIXES (Week 1-3) - Target: 60% Coverage

### Week 1: Fix Failing Tests + Zero-Coverage Critical Packages

#### Day 1-2: Fix 24 Failing Tests
```bash
# Priority 1: hooks package (22 failures)
packages/hooks/test/error-handling.test.mjs (13 failures)
packages/hooks/test/effect-sandbox.test.mjs (4 failures)
packages/hooks/test/telemetry.test.mjs (4 failures)
packages/hooks/test/hook-chain-compiler.test.mjs (1 failure)

# Root causes to investigate:
1. Error handling not defensive enough (expecting specific error types)
2. Worker pool management race conditions
3. OTEL span lifecycle issues
4. Null handling inconsistencies

# Priority 2: kgc-4d doctest (19 failures)
packages/kgc-4d/test/doctest-integration.test.mjs (19 failures)
# Likely: Missing generated doctest files

# Priority 3: core performance (1 failure)
packages/core/test/integration/store-integration.test.mjs
# Issue: Flaky performance assertion (bulkAdd timing)
```

**Deliverable:** 0 failing tests, all green CI

#### Day 3-4: Add Tests for @unrdf/validation (0 → 70%)
```javascript
// NEW FILE: packages/validation/test/validation.test.mjs
import { describe, it, expect } from 'vitest';
import { validateQuad, validateStore, validateRDF } from '../src/index.mjs';

describe('RDF Validation', () => {
  // 30 tests covering:
  // - Valid RDF/Turtle parsing
  // - Malformed RDF detection
  // - Encoding errors
  // - Quad structure validation
  // - Schema validation
  // - Large document validation (100K triples)
});

describe('SHACL Validation', () => {
  // 20 tests covering:
  // - Shape constraints
  // - Violation reporting
  // - Malformed shapes
  // - Recursive shapes
});

describe('Zod Schema Validation', () => {
  // 15 tests covering:
  // - Schema composition
  // - Error messages
  // - Type coercion
  // - Custom validators
});

describe('Security Validation', () => {
  // 10 tests covering:
  // - Malicious RDF input (billion laughs, XXE)
  // - Resource exhaustion
  // - Path traversal in file URIs
});
```

**Deliverable:** 75 tests, 70%+ coverage for validation package

#### Day 5: Fix MaxListenersExceededWarning
```javascript
// packages/streaming/test/streaming.test.mjs
import { afterEach } from 'vitest';

let cleanupTasks = [];

afterEach(async () => {
  // Clean up all event listeners
  for (const task of cleanupTasks) {
    await task();
  }
  cleanupTasks = [];
});

describe('Streaming', () => {
  it('should not leak event listeners', () => {
    const processor = createBatchProcessor();
    cleanupTasks.push(() => processor.destroy());
    // ... test code
  });
});
```

**Deliverable:** Zero MaxListenersExceededWarning in test output

### Week 2: Knowledge Engine Critical Modules

#### Day 1-3: Add Tests for Reasoning Engine
```javascript
// NEW FILE: packages/knowledge-engine/test/reason.test.mjs
import { describe, it, expect } from 'vitest';
import { reason } from '../src/reason.mjs';

describe('Reasoning Engine - Correctness', () => {
  // 50 tests covering:

  // Basic inference
  it('should infer transitive closure', () => {
    // If Alice knows Bob, and Bob knows Carol, then Alice knows Carol (transitively)
  });

  it('should infer symmetric properties', () => {
    // If Alice married to Bob, then Bob married to Alice
  });

  it('should infer inverse properties', () => {
    // If Alice parent of Bob, then Bob child of Alice
  });

  // Error handling
  it('should detect infinite loops in rules', () => {});
  it('should validate rule consistency', () => {});
  it('should handle contradictions gracefully', () => {});

  // Performance
  it('should handle 1M+ triple reasoning under 5s', () => {});
  it('should not leak memory during reasoning', () => {});

  // Edge cases
  it('should handle empty rule sets', () => {});
  it('should handle circular type hierarchies', () => {});
});
```

**Deliverable:** 50 tests for reason.mjs

#### Day 4-5: Add Tests for SHACL Validation
```javascript
// NEW FILE: packages/knowledge-engine/test/validate-shacl.test.mjs
import { describe, it, expect } from 'vitest';
import { validateShacl } from '../src/validate.mjs';

describe('SHACL Validation', () => {
  // 40 tests covering:

  // Basic constraints
  it('should validate sh:minCount constraints', () => {});
  it('should validate sh:maxCount constraints', () => {});
  it('should validate sh:datatype constraints', () => {});
  it('should validate sh:pattern (regex) constraints', () => {});

  // Complex shapes
  it('should validate nested shapes', () => {});
  it('should validate recursive shapes', () => {});
  it('should validate qualified value shapes', () => {});

  // Error reporting
  it('should report all violations (not just first)', () => {});
  it('should provide clear violation messages', () => {});
  it('should include violation paths', () => {});

  // Performance
  it('should validate 100K triple graphs under 5s', () => {});

  // Edge cases
  it('should handle malformed SHACL shapes', () => {});
  it('should handle empty shape graphs', () => {});
});
```

**Deliverable:** 40 tests for validate.mjs

### Week 3: Security + CLI Testing

#### Day 1-2: Add Security Tests
```javascript
// NEW FILE: packages/knowledge-engine/test/security/sandbox-escape.test.mjs
import { describe, it, expect } from 'vitest';
import { executeInSandbox } from '../../src/effect-sandbox.mjs';

describe('Sandbox Escape Prevention', () => {
  // 30 tests covering:

  it('should prevent process.exit() calls', async () => {
    const code = 'process.exit(1);';
    await expect(executeInSandbox(code)).rejects.toThrow(/not allowed/);
  });

  it('should prevent require() of dangerous modules', async () => {
    const code = 'require("child_process").exec("rm -rf /");';
    await expect(executeInSandbox(code)).rejects.toThrow(/not allowed/);
  });

  it('should prevent file system access', async () => {
    const code = 'require("fs").readFileSync("/etc/passwd");';
    await expect(executeInSandbox(code)).rejects.toThrow(/not allowed/);
  });

  it('should prevent network access', async () => {
    const code = 'require("net").connect(80, "evil.com");';
    await expect(executeInSandbox(code)).rejects.toThrow(/not allowed/);
  });

  it('should prevent __dirname access', async () => {});
  it('should prevent global scope pollution', async () => {});
});

// NEW FILE: packages/knowledge-engine/test/security/malicious-input.test.mjs
describe('Malicious RDF Input Handling', () => {
  // 25 tests covering:

  it('should prevent billion laughs attack (XML bombs)', async () => {
    const maliciousRDF = `
      <!DOCTYPE lolz [
        <!ENTITY lol "lol">
        <!ENTITY lol2 "&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;">
        <!ENTITY lol3 "&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;">
      ]>
      <rdf:RDF>&lol3;</rdf:RDF>
    `;
    await expect(parseRDF(maliciousRDF)).rejects.toThrow(/entity expansion/);
  });

  it('should prevent XXE attacks', async () => {});
  it('should prevent ReDoS in SPARQL queries', async () => {});
  it('should limit memory consumption to 100MB', async () => {});
  it('should timeout long-running operations at 5s', async () => {});
});
```

**Deliverable:** 55 security tests

#### Day 3-5: Expand CLI Testing
```javascript
// NEW FILE: packages/cli/test/cli-commands.test.mjs
import { describe, it, expect } from 'vitest';
import { runCLI } from '../src/index.mjs';

describe('CLI - SPARQL Query Command', () => {
  // 40 tests covering:

  it('should execute SPARQL SELECT from file', async () => {
    const result = await runCLI(['query', 'test.rq', '--format', 'json']);
    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('"results"');
  });

  it('should handle invalid SPARQL syntax', async () => {
    const result = await runCLI(['query', 'invalid.rq']);
    expect(result.exitCode).toBe(1);
    expect(result.stderr).toContain('Syntax error');
  });

  it('should handle file not found', async () => {});
  it('should handle permission denied', async () => {});
  it('should support --timeout flag', async () => {});
  it('should support --limit flag', async () => {});
});

describe('CLI - Format Conversion', () => {
  it('should convert Turtle to N-Triples', async () => {});
  it('should convert RDF/XML to JSON-LD', async () => {});
  it('should handle invalid input gracefully', async () => {});
});

describe('CLI - Error Reporting', () => {
  it('should exit with code 1 on error', async () => {});
  it('should display helpful error messages', async () => {});
  it('should suggest corrections for typos', async () => {});
});
```

**Deliverable:** 40+ CLI tests

---

## PHASE 2: COMPREHENSIVE COVERAGE (Week 4-6) - Target: 80% Coverage

### Week 4: Core Package Deep Coverage

#### Core Utils - Transaction & Concurrency (30 tests)
```javascript
// NEW FILE: packages/core/test/utils/transaction.test.mjs
describe('Transaction ACID Guarantees', () => {
  // Atomicity
  it('should rollback all changes on error', () => {});
  it('should commit all changes on success', () => {});

  // Consistency
  it('should maintain store integrity', () => {});
  it('should validate constraints before commit', () => {});

  // Isolation
  it('should prevent dirty reads', () => {});
  it('should prevent non-repeatable reads', () => {});
  it('should prevent phantom reads', () => {});
  it('should handle 100 concurrent transactions', async () => {});

  // Durability
  it('should persist committed changes', () => {});
  it('should survive process crash', () => {});
});

// NEW FILE: packages/core/test/utils/lockchain-writer.test.mjs
describe('Lockchain Writer - Concurrent Safety', () => {
  it('should serialize concurrent writes', async () => {
    const promises = Array(100).fill(null).map(() =>
      lockchainWrite(store, randomQuad())
    );
    await Promise.all(promises);
    expect(store.size).toBe(100); // No lost updates
  });

  it('should detect write conflicts', () => {});
  it('should prevent lost updates', () => {});
  it('should handle high contention (1000 concurrent writes)', () => {});
});
```

#### Core Utils - Edge Cases (25 tests)
```javascript
// NEW FILE: packages/core/test/utils/edge-case-handler.test.mjs
describe('Edge Case Handler', () => {
  // Null/undefined handling
  it('should handle null quad gracefully', () => {});
  it('should handle undefined subject', () => {});
  it('should handle undefined predicate', () => {});
  it('should handle undefined object', () => {});

  // Boundary values
  it('should handle empty string literal', () => {});
  it('should handle extremely long IRI (10000 chars)', () => {});
  it('should handle extremely large literal (1MB)', () => {});
  it('should handle MIN_SAFE_INTEGER', () => {});
  it('should handle MAX_SAFE_INTEGER', () => {});

  // Unicode/special characters
  it('should handle Unicode emoji in literals', () => {});
  it('should handle RTL text in literals', () => {});
  it('should handle null bytes in strings', () => {});

  // Circular references
  it('should detect circular references', () => {});
  it('should handle self-referential triples', () => {});

  // Type coercion
  it('should handle number-to-string coercion', () => {});
  it('should handle boolean-to-string coercion', () => {});
});
```

#### Core Utils - Memory Management (20 tests)
```javascript
// NEW FILE: packages/core/test/utils/memory-manager.test.mjs
describe('Memory Manager', () => {
  // Leak detection
  it('should not leak memory over 10000 operations', () => {
    const initialMemory = process.memoryUsage().heapUsed;

    for (let i = 0; i < 10000; i++) {
      const store = createStore();
      addQuad(store, createQuad());
    }

    global.gc();
    const finalMemory = process.memoryUsage().heapUsed;
    const leak = finalMemory - initialMemory;

    expect(leak).toBeLessThan(10 * 1024 * 1024); // <10MB
  });

  it('should release memory on store cleanup', () => {});
  it('should handle OOM gracefully', () => {});
  it('should limit store size to configured max', () => {});

  // Cache management
  it('should evict LRU entries when cache is full', () => {});
  it('should not exceed memory budget', () => {});
});
```

**Deliverable:** 75 tests, core package 70%+ coverage

### Week 5: Project Engine & Domain Models

#### Project Engine Tests (50 tests)
```javascript
// NEW FILE: packages/project-engine/test/golden-structure.test.mjs
// NEW FILE: packages/project-engine/test/drift-snapshot.test.mjs
// NEW FILE: packages/project-engine/test/domain-infer.test.mjs
// NEW FILE: packages/project-engine/test/fs-scan.test.mjs
// NEW FILE: packages/project-engine/test/template-infer.test.mjs

// Cover all 37 source files with basic functionality tests
// Focus on:
// - File system operations
// - Template inference logic
// - Drift detection
// - Golden structure validation
```

#### Domain Model Tests (40 tests)
```javascript
// NEW FILE: packages/domain/test/models.test.mjs
describe('Domain Models', () => {
  // 40 tests covering:
  // - Model creation
  // - Validation
  // - Serialization/deserialization
  // - Relationships
  // - Constraints
});
```

**Deliverable:** 90 tests, project-engine 50%+, domain 70%+

### Week 6: Integration & Performance Testing

#### Integration Tests (30 tests)
```javascript
// NEW FILE: test/integration/end-to-end.test.mjs
describe('End-to-End Workflows', () => {
  it('CLI → Core → Oxigraph: Import RDF file', async () => {
    // 1. Use CLI to import file
    await runCLI(['import', 'test.ttl']);

    // 2. Verify data in Oxigraph
    const store = createStore();
    const quads = getQuads(store);
    expect(quads.length).toBeGreaterThan(0);

    // 3. Query via SPARQL
    const results = await executeQuery(store, 'SELECT * WHERE { ?s ?p ?o }');
    expect(results.length).toBeGreaterThan(0);
  });

  it('Knowledge Engine → Hooks → Store: Reasoning pipeline', () => {});
  it('Federation → Streaming → Persistence: Distributed query', () => {});
  it('Browser → IndexedDB → Store: Client-side RDF', () => {});
});
```

#### Performance Tests (25 tests)
```javascript
// NEW FILE: test/performance/store-scaling.test.mjs
describe('Store Scaling', () => {
  it('should handle 1K triples under 100ms', () => {});
  it('should handle 10K triples under 500ms', () => {});
  it('should handle 100K triples under 5s', () => {});
  it('should handle 1M triples under 60s', () => {});

  it('should query 1M triples under 100ms', () => {});
  it('should import 100K triples under 10s', () => {});
});

// NEW FILE: test/performance/query-optimization.test.mjs
describe('Query Optimization', () => {
  it('should use indexes for simple patterns', () => {});
  it('should optimize join order', () => {});
  it('should cache query results', () => {});
});
```

**Deliverable:** 55 integration and performance tests

---

## PHASE 3: EXCELLENCE (Week 7-12) - Target: 85%+ Coverage

### Week 7-8: Property-Based Testing

#### Install fast-check
```bash
pnpm add -D fast-check
```

#### Add Property-Based Tests (50 tests)
```javascript
// packages/core/test/property/quad-invariants.test.mjs
import fc from 'fast-check';

describe('Quad Invariants (Property-Based)', () => {
  it('should handle any valid quad', () => {
    fc.assert(
      fc.property(
        fc.webUrl(),
        fc.webUrl(),
        fc.string(),
        (subject, predicate, object) => {
          const quad = { subject: namedNode(subject), predicate: namedNode(predicate), object: literal(object) };
          expect(validateQuad(quad)).toBe(true);
        }
      )
    );
  });

  it('should round-trip serialize/deserialize', () => {
    fc.assert(
      fc.property(
        arbitraryQuad(), // Custom quad generator
        (quad) => {
          const serialized = toNTriples(quad);
          const deserialized = parseNTriples(serialized)[0];
          expect(isIsomorphic(quad, deserialized)).toBe(true);
        }
      )
    );
  });
});
```

### Week 9-10: Mutation Testing

#### Install Stryker
```bash
pnpm add -D @stryker-mutator/core @stryker-mutator/vitest-runner
```

#### Configure Stryker
```javascript
// stryker.config.mjs
export default {
  packageManager: 'pnpm',
  reporters: ['html', 'clear-text', 'progress'],
  testRunner: 'vitest',
  coverageAnalysis: 'perTest',
  mutate: [
    'packages/*/src/**/*.mjs',
    '!packages/*/src/**/*.test.mjs'
  ],
  thresholds: {
    high: 80,
    low: 60,
    break: 50
  }
};
```

#### Run Mutation Testing
```bash
pnpm stryker run
```

**Goal:** Mutation score >70% (kill 70% of mutants)

### Week 11-12: Load Testing & Browser Testing

#### Load Tests (20 tests)
```javascript
// test/load/24-hour-stability.test.mjs
describe('24-Hour Stability', () => {
  it('should run for 24 hours without crashes', async () => {
    // Continuous operation for 24 hours
    // Monitor memory growth, CPU usage, error rates
  }, 24 * 60 * 60 * 1000); // 24 hour timeout
});

// test/load/large-dataset.test.mjs
describe('Large Dataset Handling', () => {
  it('should handle 100M triple dataset', () => {}, 30 * 60 * 1000); // 30 min timeout
});
```

#### Browser Tests (30 tests)
```javascript
// packages/browser/test/indexeddb.test.mjs (using Playwright)
import { test, expect } from '@playwright/test';

test('IndexedDB persistence', async ({ page }) => {
  await page.goto('http://localhost:3000');

  // Add data
  await page.evaluate(() => {
    store.addQuad({ subject: 'alice', predicate: 'knows', object: 'bob' });
  });

  // Reload page
  await page.reload();

  // Verify data persisted
  const quads = await page.evaluate(() => store.getQuads());
  expect(quads.length).toBe(1);
});
```

---

## TEST QUALITY CHECKLIST

### ✅ Every New Test MUST:

1. **Have Clear Intent**
   ```javascript
   // ❌ BAD
   it('test function', () => { /* ... */ });

   // ✅ GOOD
   it('should reject invalid SPARQL syntax with helpful error message', () => {});
   ```

2. **Use Specific Assertions**
   ```javascript
   // ❌ BAD
   expect(result).toBeDefined();

   // ✅ GOOD
   expect(result).toEqual({ subject: 'alice', predicate: 'knows', object: 'bob' });
   ```

3. **Test Error Paths**
   ```javascript
   // ✅ REQUIRED
   it('should throw ValidationError on invalid input', () => {
     expect(() => validate(null)).toThrow(ValidationError);
     expect(() => validate(null)).toThrow(/Input cannot be null/);
   });
   ```

4. **Handle Edge Cases**
   ```javascript
   // ✅ REQUIRED
   it('should handle empty input', () => {});
   it('should handle null input', () => {});
   it('should handle undefined input', () => {});
   it('should handle extremely large input', () => {});
   ```

5. **Clean Up Resources**
   ```javascript
   // ✅ REQUIRED
   afterEach(async () => {
     await store.close();
     await cleanupTempFiles();
   });
   ```

6. **Run Fast**
   ```javascript
   // ✅ TARGET: <100ms for unit tests
   it('should execute under 100ms', () => {
     const start = performance.now();
     fn();
     expect(performance.now() - start).toBeLessThan(100);
   });
   ```

7. **Be Deterministic**
   ```javascript
   // ❌ BAD: Random data without seed
   const data = faker.random.word();

   // ✅ GOOD: Seeded random or fixed data
   const data = faker.seed(12345).random.word();
   ```

---

## METRICS & TRACKING

### Weekly Progress Report Template
```markdown
# Week X Test Coverage Report

## Metrics
- Overall Coverage: X% → Y% (+Z%)
- Tests Added: N tests
- Tests Fixed: N tests
- Packages Improved: [list]

## Achievements
- ✅ [Achievement 1]
- ✅ [Achievement 2]

## Blockers
- 🚧 [Blocker 1]
- 🚧 [Blocker 2]

## Next Week Plan
- [ ] [Task 1]
- [ ] [Task 2]
```

### Coverage Tracking Dashboard
```javascript
// scripts/coverage-dashboard.mjs
{
  "overall": 65,
  "packages": {
    "core": 70,
    "hooks": 85,
    "knowledge-engine": 60,
    "cli": 60,
    "validation": 70,
    // ...
  },
  "trend": "+5% this week",
  "target": 80,
  "onTrack": true
}
```

---

## SUCCESS CRITERIA

### Phase 1 (Week 3):
- ✅ 0 failing tests
- ✅ 60% overall coverage
- ✅ All critical packages >50% coverage
- ✅ validation package >70%
- ✅ knowledge-engine critical modules >60%

### Phase 2 (Week 6):
- ✅ 80% overall coverage
- ✅ All packages >60% coverage
- ✅ Integration tests for major workflows
- ✅ Performance baselines established

### Phase 3 (Week 12):
- ✅ 85%+ overall coverage
- ✅ Mutation score >70%
- ✅ Zero flaky tests
- ✅ Load tests passing (24-hour stability)
- ✅ Browser tests passing (all major browsers)

---

**Plan Created:** 2025-12-21
**Plan Owner:** QA Specialist (Tester Agent)
**Review Cycle:** Weekly
**Approval Required:** Tech Lead
