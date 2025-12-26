# Agent 10: Quality Gates & End-to-End Testing

**Role**: Production Validation - Ensure all primitives work together deterministically with zero tolerance for approximation

---

## Core Principle: Adversarial Validation

**Every claim requires proof. Every assertion requires measurement.**

```
CLAIM: "Tests pass"
PROOF: Output showing ✅ 100% (X/X tests)

CLAIM: "100% deterministic"
PROOF: SHA256 hashes match byte-for-byte across 2+ runs

CLAIM: "No circular dependencies"
PROOF: Dependency graph with 0 cycles detected
```

---

## Files to Create

### Production Files
1. `./src/quality-gates.mjs` - Gate definitions and execution engine
2. `./src/determinism-audit.mjs` - Determinism verification with byte-level comparison
3. `./src/dependency-analyzer.mjs` - Import resolution and circular dependency detection
4. `./src/index.mjs` - Public API exports

### Test Files
1. `./test/quality-gates.test.mjs` - Quality gate validation tests
2. `./test/determinism.test.mjs` - Determinism audit tests
3. `./test/e2e.test.mjs` - Full end-to-end workflow test

---

## Quality Gates (Zero Tolerance)

Each gate MUST return `{ pass: boolean, evidence: string[], failures: string[] }`

### Gate 1: Import Resolution
**Claim**: All imports from agents 2-9 resolve successfully

**Verification**:
```javascript
async function verifyImports() {
  const agentModules = [
    '../agent-2/src/index.mjs',  // Capsule planner
    '../agent-3/src/index.mjs',  // Lens compiler
    '../agent-4/src/index.mjs',  // Impact set
    '../agent-5/src/index.mjs',  // Commutativity checker
    '../agent-6/src/index.mjs',  // Profile compiler
    '../agent-7/src/index.mjs',  // Facade generator
    '../agent-8/src/index.mjs',  // Atomic store
    '../agent-9/src/index.mjs',  // Shadow mode
  ];

  const failures = [];
  for (const module of agentModules) {
    try {
      const imported = await import(module);
      if (!imported || Object.keys(imported).length === 0) {
        failures.push(`${module}: Empty exports`);
      }
    } catch (error) {
      failures.push(`${module}: ${error.message}`);
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Checked ${agentModules.length} modules`],
    failures
  };
}
```

**Proof Required**: 8/8 imports successful, 0 errors

---

### Gate 2: Circular Dependency Detection
**Claim**: No circular dependencies in module graph

**Verification**:
```javascript
async function detectCircularDeps() {
  // Parse all import statements from all agent modules
  // Build dependency graph
  // Run cycle detection algorithm (DFS-based)
  // Report any cycles found

  const graph = await buildDependencyGraph();
  const cycles = findCycles(graph);

  return {
    pass: cycles.length === 0,
    evidence: [`Analyzed ${graph.nodeCount} modules`],
    failures: cycles.map(cycle => `Circular: ${cycle.join(' → ')}`)
  };
}
```

**Proof Required**: 0 cycles detected, complete graph traversal

---

### Gate 3: JSDoc Coverage (100%)
**Claim**: All exported functions have complete JSDoc

**Verification**:
```javascript
async function verifyJSDoc() {
  const failures = [];

  for (const agentDir of ['agent-2', ..., 'agent-9']) {
    const module = await import(`../${agentDir}/src/index.mjs`);

    for (const [name, fn] of Object.entries(module)) {
      if (typeof fn !== 'function') continue;

      const source = fn.toString();
      const jsdoc = extractJSDoc(source);

      // Check required tags
      const missing = [];
      if (!jsdoc.description) missing.push('@description');
      if (!jsdoc.params && fn.length > 0) missing.push('@param');
      if (!jsdoc.returns) missing.push('@returns');

      if (missing.length > 0) {
        failures.push(`${agentDir}::${name}: Missing ${missing.join(', ')}`);
      }
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Checked JSDoc for all exported functions`],
    failures
  };
}
```

**Proof Required**: 0 missing tags, 100% coverage

---

### Gate 4: No Prohibited Imports
**Claim**: No direct `from 'n3'` imports outside justified modules

**Verification**:
```javascript
async function checkProhibitedImports() {
  const prohibited = [
    { pattern: /from ['"]n3['"]/, name: 'n3 (use @unrdf/oxigraph)' },
    { pattern: /console\.(log|debug|info)/, name: 'console.* (use logging fields)' },
  ];

  const failures = [];

  for (const agentDir of ['agent-2', ..., 'agent-9']) {
    const files = await glob(`../${agentDir}/src/**/*.mjs`);

    for (const file of files) {
      const content = await fs.readFile(file, 'utf-8');

      for (const { pattern, name } of prohibited) {
        if (pattern.test(content)) {
          failures.push(`${file}: Prohibited import/usage: ${name}`);
        }
      }
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Scanned all .mjs files`],
    failures
  };
}
```

**Proof Required**: 0 prohibited patterns found

---

### Gate 5: Zod Validation on External Inputs
**Claim**: All public API functions validate inputs with Zod

**Verification**:
```javascript
async function verifyZodValidation() {
  const failures = [];

  // Check each exported function has Zod schema validation
  // at the entry point (first few lines)

  for (const agentDir of ['agent-2', ..., 'agent-9']) {
    const indexPath = `../${agentDir}/src/index.mjs`;
    const content = await fs.readFile(indexPath, 'utf-8');

    // Look for Zod imports
    if (!content.includes("from 'zod'")) {
      failures.push(`${agentDir}: No Zod import found`);
    }

    // Extract exported functions and check for .parse() or .safeParse()
    const exports = extractExportedFunctions(content);
    for (const exp of exports) {
      if (!exp.body.includes('.parse(') && !exp.body.includes('.safeParse(')) {
        failures.push(`${agentDir}::${exp.name}: No Zod validation detected`);
      }
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Verified Zod usage in ${8} agent modules`],
    failures
  };
}
```

**Proof Required**: 100% of public functions have Zod validation

---

### Gate 6: Deterministic Operations (No Randomness)
**Claim**: No unseeded randomness (Math.random, crypto.randomBytes without seed)

**Verification**:
```javascript
async function checkDeterminism() {
  const randomPatterns = [
    /Math\.random\(\)/,
    /crypto\.randomBytes\(/,
    /Date\.now\(\)/,  // Unless explicitly seeded/controlled
    /new Date\(\)/    // Unless time is controlled
  ];

  const failures = [];

  for (const agentDir of ['agent-2', ..., 'agent-9']) {
    const files = await glob(`../${agentDir}/src/**/*.mjs`);

    for (const file of files) {
      const content = await fs.readFile(file, 'utf-8');

      for (const pattern of randomPatterns) {
        const matches = content.match(pattern);
        if (matches) {
          // Check if there's a seed/deterministic comment nearby
          const lines = content.split('\n');
          const matchLine = lines.findIndex(l => pattern.test(l));
          const context = lines.slice(Math.max(0, matchLine - 2), matchLine + 3);

          if (!context.some(l => /seed|deterministic|controlled/i.test(l))) {
            failures.push(`${file}:${matchLine}: Unseeded randomness: ${matches[0]}`);
          }
        }
      }
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Scanned for randomness patterns`],
    failures
  };
}
```

**Proof Required**: 0 unseeded random operations

---

### Gate 7: No External Network Calls
**Claim**: No HTTP/HTTPS calls to external services (self-contained)

**Verification**:
```javascript
async function checkNetworkCalls() {
  const networkPatterns = [
    /fetch\(/,
    /axios\./,
    /http\.request/,
    /https\.request/,
    /ws:\/\//,
    /wss:\/\//
  ];

  const failures = [];

  for (const agentDir of ['agent-2', ..., 'agent-9']) {
    const files = await glob(`../${agentDir}/src/**/*.mjs`);

    for (const file of files) {
      const content = await fs.readFile(file, 'utf-8');

      for (const pattern of networkPatterns) {
        if (pattern.test(content)) {
          failures.push(`${file}: Network call detected: ${pattern.source}`);
        }
      }
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Scanned for network patterns`],
    failures
  };
}
```

**Proof Required**: 0 network calls detected

---

### Gate 8: File Size Constraints
**Claim**: No file exceeds 500 lines

**Verification**:
```javascript
async function checkFileSizes() {
  const MAX_LINES = 500;
  const failures = [];

  for (const agentDir of ['agent-2', ..., 'agent-9']) {
    const files = await glob(`../${agentDir}/src/**/*.mjs`);

    for (const file of files) {
      const content = await fs.readFile(file, 'utf-8');
      const lines = content.split('\n').length;

      if (lines > MAX_LINES) {
        failures.push(`${file}: ${lines} lines (max ${MAX_LINES})`);
      }
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Checked file sizes`],
    failures
  };
}
```

**Proof Required**: All files ≤500 lines

---

## Determinism Audit Algorithm

**Goal**: Prove byte-for-byte reproducibility across multiple runs

### Phase 1: Capture Output
```javascript
async function captureRun(seed) {
  const outputs = {};

  // Intercept all file writes
  const originalWriteFile = fs.writeFile;
  fs.writeFile = async (path, content, ...args) => {
    outputs[path] = content;
    return originalWriteFile(path, content, ...args);
  };

  // Run the demo
  const result = await demo({ seed });

  // Restore
  fs.writeFile = originalWriteFile;

  return {
    returnValue: result,
    outputs,
    hashes: await computeHashes(outputs)
  };
}
```

### Phase 2: Hash Comparison
```javascript
import { sha256 } from 'hash-wasm';

async function computeHashes(outputs) {
  const hashes = {};

  for (const [path, content] of Object.entries(outputs)) {
    // Ensure content is normalized (line endings, etc.)
    const normalized = normalizeContent(content);
    hashes[path] = await sha256(normalized);
  }

  return hashes;
}

function normalizeContent(content) {
  if (typeof content === 'string') {
    // Normalize line endings to LF
    return content.replace(/\r\n/g, '\n');
  }

  if (Buffer.isBuffer(content)) {
    return content;
  }

  if (typeof content === 'object') {
    // Canonical JSON serialization
    return JSON.stringify(content, Object.keys(content).sort());
  }

  return String(content);
}
```

### Phase 3: Comparison
```javascript
async function validateDeterminism(demo, runs = 2) {
  const seed = 12345;
  const captures = [];

  for (let i = 0; i < runs; i++) {
    captures.push(await captureRun(seed));
  }

  // Compare all hashes
  const baseline = captures[0].hashes;
  const mismatches = [];

  for (let i = 1; i < runs; i++) {
    const current = captures[i].hashes;

    // Check for different files
    const baselinePaths = new Set(Object.keys(baseline));
    const currentPaths = new Set(Object.keys(current));

    for (const path of baselinePaths) {
      if (!currentPaths.has(path)) {
        mismatches.push({
          path,
          issue: `Missing in run ${i + 1}`
        });
      }
    }

    for (const path of currentPaths) {
      if (!baselinePaths.has(path)) {
        mismatches.push({
          path,
          issue: `Extra in run ${i + 1}`
        });
      }
    }

    // Compare hashes for common files
    for (const path of baselinePaths) {
      if (currentPaths.has(path)) {
        if (baseline[path] !== current[path]) {
          mismatches.push({
            path,
            issue: `Hash mismatch`,
            run1: baseline[path],
            run2: current[path]
          });
        }
      }
    }
  }

  return {
    deterministic: mismatches.length === 0,
    runs,
    seed,
    hashes: baseline,
    mismatches,
    evidence: mismatches.length === 0
      ? [`${runs} runs with identical hashes`]
      : []
  };
}
```

**Success Criteria**:
- `deterministic === true`
- `mismatches.length === 0`
- All SHA256 hashes match exactly

---

## End-to-End Workflow

**Scenario**: Company-like Customer Service with Intent Operations

### Step-by-Step E2E Test

```javascript
async function runE2E() {
  const results = {};
  const evidence = [];

  // Step 1: Define Conventions Profile
  evidence.push('Step 1: Define Conventions Profile');
  const profile = await compileProfile({
    namespace: 'http://example.com/schema#',
    entities: {
      Customer: {
        properties: ['name', 'email', 'createdAt']
      }
    }
  });
  results.profileHash = await sha256(JSON.stringify(profile));

  // Step 2: Create Customer Lens
  evidence.push('Step 2: Create Customer Lens');
  const lens = await defineLens({
    name: 'CustomerLens',
    profile,
    focus: 'Customer'
  });
  const compiledLens = await compileLens(lens);
  results.lensHash = await sha256(JSON.stringify(compiledLens));

  // Step 3: Define Intent Operations
  evidence.push('Step 3: Define Intent Operations');
  const createCustomerIntent = {
    type: 'CREATE',
    entity: 'Customer',
    data: { name: 'Alice', email: 'alice@example.com' }
  };

  const updateEmailIntent = {
    type: 'UPDATE',
    entity: 'Customer',
    id: 'customer-1',
    data: { email: 'alice.new@example.com' }
  };

  // Step 4: Plan First Capsule
  evidence.push('Step 4: Plan First Capsule (CREATE)');
  const capsule1 = await planCapsule({
    intent: createCustomerIntent,
    lens: compiledLens,
    profile
  });

  const verified1 = await verifyCapsule(capsule1);
  if (!verified1.valid) {
    throw new Error(`Capsule 1 verification failed: ${verified1.errors}`);
  }
  results.capsule1Hash = await sha256(JSON.stringify(capsule1));

  // Step 5: Plan Second Capsule
  evidence.push('Step 5: Plan Second Capsule (UPDATE)');
  const capsule2 = await planCapsule({
    intent: updateEmailIntent,
    lens: compiledLens,
    profile
  });

  const verified2 = await verifyCapsule(capsule2);
  if (!verified2.valid) {
    throw new Error(`Capsule 2 verification failed: ${verified2.errors}`);
  }
  results.capsule2Hash = await sha256(JSON.stringify(capsule2));

  // Step 6: Check Commutativity (should NOT reorder)
  evidence.push('Step 6: Check Commutativity');
  const commutative = await canReorder(capsule1, capsule2);
  if (commutative) {
    throw new Error('CREATE and UPDATE should NOT be commutative');
  }
  results.commutativityCheck = 'PASS: Not commutative (correct)';

  // Step 7: Apply Capsules to Store (sequentially)
  evidence.push('Step 7: Apply Capsules to Store');
  const store = await createAtomicStore();

  const receipt1 = await applyCapsule(store, capsule1);
  results.receipt1Hash = await sha256(JSON.stringify(receipt1));

  const receipt2 = await applyCapsule(store, capsule2);
  results.receipt2Hash = await sha256(JSON.stringify(receipt2));

  // Step 8: Compute Impact Sets
  evidence.push('Step 8: Compute Impact Sets');
  const impact1 = await computeImpactSet(capsule1, store);
  const impact2 = await computeImpactSet(capsule2, store);

  results.impact1Hash = await sha256(JSON.stringify(impact1));
  results.impact2Hash = await sha256(JSON.stringify(impact2));

  // Step 9: Generate Facade
  evidence.push('Step 9: Generate Facade');
  const facade = await generateFacade({
    profile,
    lens: compiledLens
  });
  results.facadeHash = await sha256(facade);

  // Step 10: Run Shadow Mode Scenario
  evidence.push('Step 10: Run Shadow Mode');
  const shadowResult = await partialServe({
    store,
    intent: createCustomerIntent,
    mode: 'shadow'
  });

  const mismatch = shadowResult.mismatch;
  results.shadowMismatch = mismatch ? 'DETECTED' : 'NONE';

  // Step 11: Verify Receipts
  evidence.push('Step 11: Verify Receipt Chain');
  if (!receipt1.hash || !receipt2.hash) {
    throw new Error('Receipts missing hashes');
  }

  if (!receipt2.parentHash) {
    throw new Error('Receipt 2 missing parent hash');
  }

  if (receipt2.parentHash !== receipt1.hash) {
    throw new Error('Receipt chain broken');
  }

  results.receiptChain = 'VALID';

  // Step 12: Run Determinism Audit
  evidence.push('Step 12: Run Determinism Audit');
  const determinismResult = await validateDeterminism(async ({ seed }) => {
    // Re-run entire workflow with seeded operations
    return runE2E();
  }, 2);

  if (!determinismResult.deterministic) {
    throw new Error(`Determinism audit failed: ${JSON.stringify(determinismResult.mismatches)}`);
  }

  results.determinismAudit = 'PASS';

  return {
    success: true,
    results,
    evidence,
    summary: {
      totalSteps: evidence.length,
      allHashesComputed: Object.keys(results).filter(k => k.endsWith('Hash')).length,
      deterministic: true
    }
  };
}
```

**Success Criteria**:
- All 12 steps complete without errors
- All hashes are valid SHA256 strings (64 hex chars)
- Receipt chain is valid (parentHash linkage)
- Commutativity check returns correct result
- Determinism audit passes (2 runs, identical hashes)

---

## Key Functions & Signatures

### quality-gates.mjs

```javascript
/**
 * Run all quality gates sequentially
 * @returns {Promise<{pass: boolean, gates: Array, failures: Array}>}
 */
export async function runQualityGates();

/**
 * Verify import resolution for all agent modules
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function verifyImports();

/**
 * Detect circular dependencies in module graph
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function detectCircularDeps();

/**
 * Verify 100% JSDoc coverage on exported functions
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function verifyJSDoc();

/**
 * Check for prohibited imports (e.g., direct 'n3' usage)
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function checkProhibitedImports();

/**
 * Verify Zod validation on all public API functions
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function verifyZodValidation();

/**
 * Check for unseeded randomness (Math.random, etc.)
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function checkDeterminism();

/**
 * Check for external network calls
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function checkNetworkCalls();

/**
 * Check file size constraints (<500 lines)
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function checkFileSizes();
```

### determinism-audit.mjs

```javascript
/**
 * Validate determinism by running demo multiple times and comparing hashes
 * @param {Function} demo - Demo function to run (receives { seed })
 * @param {number} runs - Number of runs to compare (default: 2)
 * @returns {Promise<{deterministic: boolean, runs: number, seed: number, hashes: Object, mismatches: Array, evidence: string[]}>}
 */
export async function validateDeterminism(demo, runs = 2);

/**
 * Capture all outputs from a single run
 * @param {number} seed - Deterministic seed
 * @returns {Promise<{returnValue: any, outputs: Object, hashes: Object}>}
 */
async function captureRun(seed);

/**
 * Compute SHA256 hashes for all outputs
 * @param {Object} outputs - Map of path -> content
 * @returns {Promise<Object>} Map of path -> hash
 */
async function computeHashes(outputs);

/**
 * Normalize content for consistent hashing (line endings, JSON keys, etc.)
 * @param {string|Buffer|Object} content - Content to normalize
 * @returns {string|Buffer} Normalized content
 */
function normalizeContent(content);
```

### dependency-analyzer.mjs

```javascript
/**
 * Build dependency graph from all agent modules
 * @returns {Promise<{nodes: Set, edges: Map, nodeCount: number}>}
 */
export async function buildDependencyGraph();

/**
 * Find cycles in dependency graph using DFS
 * @param {Object} graph - Dependency graph
 * @returns {Array<Array<string>>} Array of cycles (each cycle is array of module paths)
 */
export function findCycles(graph);

/**
 * Extract import statements from JavaScript file
 * @param {string} content - File content
 * @returns {Array<{source: string, specifiers: Array}>}
 */
function extractImports(content);
```

---

## Tests (Minimum)

### test/quality-gates.test.mjs

```javascript
import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { runQualityGates } from '../src/quality-gates.mjs';

describe('Quality Gates', () => {
  it('should pass all quality gates', async () => {
    const result = await runQualityGates();

    assert.equal(result.pass, true,
      `Quality gates failed:\n${result.failures.join('\n')}`);

    assert.equal(result.failures.length, 0);
    assert.ok(result.gates.length >= 8, 'Should have at least 8 gates');
  });

  it('should provide evidence for all gates', async () => {
    const result = await runQualityGates();

    for (const gate of result.gates) {
      assert.ok(gate.evidence.length > 0,
        `Gate ${gate.name} has no evidence`);
    }
  });
});
```

### test/determinism.test.mjs

```javascript
import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { validateDeterminism } from '../src/determinism-audit.mjs';

describe('Determinism Audit', () => {
  it('should produce identical hashes across 2 runs', async () => {
    const mockDemo = async ({ seed }) => {
      // Simple deterministic operation
      return { result: seed * 2 };
    };

    const result = await validateDeterminism(mockDemo, 2);

    assert.equal(result.deterministic, true,
      `Determinism failed:\n${JSON.stringify(result.mismatches, null, 2)}`);

    assert.equal(result.mismatches.length, 0);
    assert.ok(result.evidence.length > 0);
  });

  it('should detect non-deterministic operations', async () => {
    const nonDeterministicDemo = async ({ seed }) => {
      return { result: Math.random() };
    };

    const result = await validateDeterminism(nonDeterministicDemo, 2);

    assert.equal(result.deterministic, false,
      'Should detect non-determinism');

    assert.ok(result.mismatches.length > 0);
  });
});
```

### test/e2e.test.mjs

```javascript
import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { runE2E } from '../src/index.mjs';

describe('End-to-End Workflow', () => {
  it('should complete full workflow with all validations', async () => {
    const result = await runE2E();

    assert.equal(result.success, true,
      `E2E failed:\n${result.evidence.join('\n')}`);

    assert.equal(result.summary.totalSteps, 12,
      'Should complete all 12 steps');

    assert.ok(result.summary.allHashesComputed >= 8,
      'Should compute at least 8 hashes');

    assert.equal(result.summary.deterministic, true,
      'Should be deterministic');
  });

  it('should validate receipt chain integrity', async () => {
    const result = await runE2E();

    assert.equal(result.results.receiptChain, 'VALID',
      'Receipt chain should be valid');
  });

  it('should correctly identify non-commutative operations', async () => {
    const result = await runE2E();

    assert.ok(result.results.commutativityCheck.includes('PASS'),
      'Should correctly identify non-commutativity');
  });
});
```

---

## Dependencies

### From Agents 2-9
```javascript
// Agent 2: Capsule Planner
import { planCapsule, verifyCapsule } from '../../agent-2/src/index.mjs';

// Agent 3: Lens Compiler
import { defineLens, compileLens } from '../../agent-3/src/index.mjs';

// Agent 4: Impact Set
import { computeImpactSet } from '../../agent-4/src/index.mjs';

// Agent 5: Commutativity Checker
import { canReorder, conflictCertificate } from '../../agent-5/src/index.mjs';

// Agent 6: Profile Compiler
import { compileProfile } from '../../agent-6/src/index.mjs';

// Agent 7: Facade Generator
import { generateFacade } from '../../agent-7/src/index.mjs';

// Agent 8: Atomic Store
import { applyCapsule, createAtomicStore } from '../../agent-8/src/index.mjs';

// Agent 9: Shadow Mode
import { shadowWrite, shadowRead, partialServe } from '../../agent-9/src/index.mjs';
```

### External Dependencies
```javascript
import { sha256 } from 'hash-wasm';           // Deterministic hashing
import { z } from 'zod';                       // Input validation
import { createStore } from '@unrdf/oxigraph'; // RDF store
import fs from 'node:fs/promises';             // File operations
import path from 'node:path';                  // Path utilities
```

---

## Exports

### Main API (./src/index.mjs)
```javascript
// Quality Gates
export { runQualityGates } from './quality-gates.mjs';

// Determinism Audit
export { validateDeterminism } from './determinism-audit.mjs';

// End-to-End Test
export { runE2E } from './e2e.mjs';

// Dependency Analysis
export { buildDependencyGraph, findCycles } from './dependency-analyzer.mjs';
```

---

## Success Metrics

### Quality Gates
- **Target**: 8/8 gates pass
- **Measurement**: `runQualityGates().pass === true`
- **Evidence**: Console output showing ✅ for each gate

### Determinism Audit
- **Target**: 100% deterministic (2+ runs, identical hashes)
- **Measurement**: `validateDeterminism(demo, 2).deterministic === true`
- **Evidence**: SHA256 hashes printed side-by-side, all matching

### End-to-End Test
- **Target**: 12/12 steps complete, all validations pass
- **Measurement**: `runE2E().success === true`
- **Evidence**: Step-by-step log with ✅ marks

### Test Coverage
- **Target**: 100% test pass rate
- **Measurement**: `npm test` output showing X/X passing
- **Evidence**: Test runner output with no failures

---

## Adversarial PM Checklist

**Before declaring "Quality Gates Complete"**:

### Claims vs Reality
- [ ] Did I RUN `runQualityGates()` and see output?
- [ ] Did I RUN `validateDeterminism()` twice and compare hashes?
- [ ] Did I RUN `runE2E()` and verify 12 steps completed?
- [ ] Can someone else reproduce from scratch?

### Evidence Quality
- [ ] SHA256 hashes shown (64 hex chars each)?
- [ ] Test output showing X/X tests passing?
- [ ] Gate results showing 8/8 passing?
- [ ] Import resolution showing 8/8 modules loaded?

### Process Quality
- [ ] All operations in single pass (no iteration needed)?
- [ ] Zero tolerance enforced (no "close enough")?
- [ ] Byte-level comparison (not line-level)?
- [ ] All files <500 lines verified?

### Red Flags (STOP if any apply)
- ❌ "Gates should pass" → Didn't run them
- ❌ "Mostly deterministic" → Not acceptable
- ❌ "Tests look good" → Didn't run tests
- ❌ "Hashes probably match" → Didn't compare

---

## Implementation Strategy

### Phase 1: Infrastructure (Batch)
1. Create all 4 files in `./src/` in ONE message
2. Create all 3 test files in `./test/` in ONE message
3. Verify imports resolve (`timeout 5s node -e "import('./src/index.mjs')"`)

### Phase 2: Quality Gates (Single Pass)
1. Implement all 8 gates in `quality-gates.mjs`
2. Wire into `runQualityGates()` orchestrator
3. Export from `index.mjs`

### Phase 3: Determinism Audit (Single Pass)
1. Implement capture/hash/compare in `determinism-audit.mjs`
2. Add normalization logic for content types
3. Export from `index.mjs`

### Phase 4: E2E Workflow (Single Pass)
1. Implement 12-step workflow in `e2e.mjs` (or `index.mjs`)
2. Integrate all agent imports
3. Add determinism check as final step

### Phase 5: Tests (Single Pass)
1. Write all 3 test files
2. Run with `timeout 5s node --test test/*.test.mjs`
3. Verify 100% pass rate

### Phase 6: Validation (Batch)
1. Run quality gates: `node -e "import('./src/index.mjs').then(m => m.runQualityGates())"`
2. Run determinism audit: `node -e "..."`
3. Run E2E: `node -e "..."`
4. Collect all evidence (hashes, test output, gate results)

**Total Messages**: Ideally 1-2 for implementation, 1 for validation

---

## Constraints

1. **Zero Tolerance**: No approximations, "mostly works", or "close enough"
2. **Byte-Level**: Hash comparison must be byte-for-byte
3. **No Network**: All operations must be self-contained
4. **No Randomness**: All operations must be deterministic or seeded
5. **Import Resolution**: All 8 agent modules must resolve successfully
6. **File Size**: All files must be <500 lines
7. **JSDoc**: 100% coverage on exported functions
8. **Zod**: All public APIs must validate inputs

---

## Proof of Completion

**To declare Agent 10 complete, provide**:

1. Output from `runQualityGates()` showing:
   - 8/8 gates passed
   - 0 failures
   - Evidence for each gate

2. Output from `validateDeterminism()` showing:
   - `deterministic: true`
   - 0 mismatches
   - Identical SHA256 hashes for both runs

3. Output from `runE2E()` showing:
   - 12/12 steps completed
   - All hashes computed (≥8)
   - Receipt chain valid
   - Determinism audit passed

4. Test output showing:
   - X/X tests passing (100% pass rate)
   - 0 failures
   - Coverage report (if applicable)

**Format**:
```
✅ Quality Gates: 8/8 PASS
  ✅ Import Resolution: 8/8 modules loaded
  ✅ Circular Deps: 0 cycles detected
  ✅ JSDoc Coverage: 100%
  ✅ Prohibited Imports: 0 violations
  ✅ Zod Validation: 100%
  ✅ Determinism Check: 0 unseeded operations
  ✅ Network Calls: 0 detected
  ✅ File Sizes: All <500 lines

✅ Determinism Audit: 2/2 runs identical
  Hash 1: a3f5e8d9c7b2... (profile)
  Hash 2: b4e6f9d0c8b3... (lens)
  Hash 3: c5e7f0d1c9b4... (capsule1)
  ...

✅ E2E Workflow: 12/12 steps complete
  ✅ Profile compiled
  ✅ Lens defined
  ✅ Intents created
  ✅ Capsules planned
  ✅ Commutativity checked
  ✅ Store updated
  ✅ Impact sets computed
  ✅ Facade generated
  ✅ Shadow mode tested
  ✅ Receipts validated
  ✅ Determinism verified

✅ Tests: 8/8 passing
  ✅ quality-gates.test.mjs: 2/2
  ✅ determinism.test.mjs: 2/2
  ✅ e2e.test.mjs: 3/3
```

---

**End of Plan**

This plan is ready for Big Bang 80/20 implementation in a SINGLE PASS with ZERO rework.
