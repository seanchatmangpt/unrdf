# BB8020 Orchestrator Fix Implementation Plan

**Date**: 2025-12-06
**Goal**: Replace all simulated implementations with real functionality using existing packages
**Priority**: CRITICAL - Blocking production release

---

## Overview

The `bb8020-orchestrator.mjs` currently has **4 simulated steps** that need real implementations:

| Step | Current Status | Package to Use | RPN Risk |
|------|---------------|----------------|----------|
| Step 4: Pattern Matching | **SIMULATED** (fake similarity scores) | `@unrdf/project-engine` + `@unrdf/core` | 192 (HIGH) |
| Step 7: Code Generation | **PARTIAL** (placeholder comments) | Template engine + AST | 126 (HIGH) |
| Step 8: Syntax Validation | **SIMULATED** (always returns true) | Node.js `--check` | 144 (MEDIUM) |
| Step 9: Static Analysis | **SIMULATED** (hardcoded 98%) | `@unrdf/project-engine` | 140 (HIGH) |

---

## Fix 1: Step 4 - Pattern Matching (Priority 1)

### Current Implementation (FAKE)

**File**: `packages/decision-fabric/src/bb8020-orchestrator.mjs:212-230`

```javascript
async _step4_patternMatching() {
  // ❌ SIMULATION - Just creates dummy patterns
  const patterns = [];

  for (const feature of this.artifacts.paretoFrontier) {
    patterns.push({
      feature: feature.name,
      pattern: `// Pattern for ${feature.name}`,  // ← FAKE!
      similarity: 0.92,  // ← HARDCODED FAKE
      source: 'pattern-library.mjs',
      reuse_percentage: 64.3
    });
  }

  this.artifacts.patterns = patterns;
  return new WorkflowStepResult({
    success: true,
    step: 'pattern_matching',
    output: { pattern_count: patterns.length }
  });
}
```

### Real Implementation (Using @unrdf/project-engine + @unrdf/core)

```javascript
import { scanFileSystemToStore } from '@unrdf/project-engine/fs-scan.mjs';
import { executeSelectSync, namedNode } from '@unrdf/core';

async _step4_patternMatching() {
  // 1. Scan codebase to RDF graph
  const { store, summary } = await scanFileSystemToStore({
    root: this.codebasePath,
    patterns: ['**/*.mjs', '**/*.js'],
    ignore: ['**/node_modules/**', '**/test/**']
  });

  console.log(`Scanned ${summary.fileCount} files into RDF graph`);

  // 2. For each Pareto feature, find similar code patterns
  const patterns = [];

  for (const feature of this.artifacts.paretoFrontier) {
    // Build SPARQL query to find similar patterns
    const sparql = `
      PREFIX fs: <http://unrdf.io/fs/>
      PREFIX code: <http://unrdf.io/code/>

      SELECT ?file ?function ?complexity WHERE {
        ?file fs:contains ?function .
        ?function code:name ?name .
        ?function code:complexity ?complexity .

        # Filter by feature keywords (e.g., "auth" finds authentication functions)
        FILTER(CONTAINS(LCASE(?name), LCASE("${feature.name}")))

        # Only reuse simple, maintainable code (complexity < 10)
        FILTER(?complexity < 10)
      }
      ORDER BY ?complexity
      LIMIT 5
    `;

    const results = executeSelectSync(store, sparql);

    // 3. Calculate real similarity scores
    const matches = results.map(row => {
      const similarity = this._calculateSimilarity(
        feature,
        row.function.value,
        row.complexity.value
      );

      return {
        file: row.file.value,
        function: row.function.value,
        complexity: parseInt(row.complexity.value),
        similarity
      };
    });

    patterns.push({
      feature: feature.name,
      matches,
      best_match: matches[0],  // Lowest complexity
      reuse_percentage: this._calculateReusePercentage(matches)
    });
  }

  // 4. Validate BB80/20 reuse assumption (≥64.3% code reuse)
  const avgReuse = patterns.reduce((sum, p) => sum + p.reuse_percentage, 0) / patterns.length;

  if (avgReuse < 64.3) {
    console.warn(
      `⚠️  BB80/20 assumption violated: ${avgReuse.toFixed(1)}% reuse < 64.3% expected`
    );
  }

  this.artifacts.patterns = patterns;
  this.artifacts.codebaseStore = store;  // Keep for later steps

  return new WorkflowStepResult({
    success: true,
    step: 'pattern_matching',
    output: {
      pattern_count: patterns.length,
      avg_reuse_percentage: avgReuse,
      files_scanned: summary.fileCount
    }
  });
}

/**
 * Calculate similarity score between feature and existing code
 * Uses Jaccard similarity on tokenized names
 */
_calculateSimilarity(feature, functionName, complexity) {
  // Tokenize feature name and function name
  const featureTokens = new Set(feature.name.toLowerCase().split(/[_\-\s]+/));
  const functionTokens = new Set(functionName.toLowerCase().split(/[_\-\s]+/));

  // Jaccard similarity: |A ∩ B| / |A ∪ B|
  const intersection = new Set([...featureTokens].filter(t => functionTokens.has(t)));
  const union = new Set([...featureTokens, ...functionTokens]);

  const jaccard = intersection.size / union.size;

  // Penalize high complexity (favor simple, reusable code)
  const complexityPenalty = Math.max(0, (complexity - 5) * 0.05);

  return Math.max(0, Math.min(1, jaccard - complexityPenalty));
}

/**
 * Calculate percentage of code that can be reused
 */
_calculateReusePercentage(matches) {
  if (matches.length === 0) return 0;

  // Average similarity across all matches
  const avgSimilarity = matches.reduce((sum, m) => sum + m.similarity, 0) / matches.length;

  // Convert to percentage (0-100)
  return avgSimilarity * 100;
}
```

### Tests

```javascript
// test/bb8020/step4-pattern-matching.test.mjs
import { describe, it, expect } from 'vitest';
import { BB8020Orchestrator } from '../src/bb8020-orchestrator.mjs';

describe('Step 4: Pattern Matching - Real Implementation', () => {
  it('should scan actual codebase and return real patterns', async () => {
    const orchestrator = new BB8020Orchestrator({
      codebasePath: process.cwd()  // Scan current project
    });

    // Set up Pareto frontier
    orchestrator.artifacts.paretoFrontier = [
      { name: 'authentication', value: 10, cost: 2 },
      { name: 'validation', value: 8, cost: 1 }
    ];

    await orchestrator._step4_patternMatching();

    // Verify real patterns (not simulations)
    expect(orchestrator.artifacts.patterns).toBeDefined();
    expect(orchestrator.artifacts.patterns.length).toBeGreaterThan(0);

    const firstPattern = orchestrator.artifacts.patterns[0];

    // ❌ NOT fake values
    expect(firstPattern.best_match.similarity).not.toBe(0.92);
    expect(firstPattern.matches[0].file).not.toContain('pattern-library.mjs');

    // ✅ Real codebase data
    expect(firstPattern.matches[0].file).toMatch(/\.mjs$/);
    expect(firstPattern.matches[0].complexity).toBeGreaterThan(0);
    expect(firstPattern.reuse_percentage).toBeGreaterThanOrEqual(0);
  });

  it('should warn when BB80/20 reuse assumption violated', async () => {
    // Test with features that have low reuse
    // (expect warning when avg < 64.3%)
  });
});
```

---

## Fix 2: Step 8 - Syntax Validation (Priority 1)

### Current Implementation (FAKE)

**File**: `packages/decision-fabric/src/bb8020-orchestrator.mjs:385-395`

```javascript
async _step8_syntaxValidation() {
  // ❌ SIMULATION - Doesn't actually run node --check
  this.artifacts.validationResults.syntax = {
    valid: true,  // ← ALWAYS TRUE!
    errors: []
  };

  return new WorkflowStepResult({
    success: true,
    step: 'syntax_validation'
  });
}
```

### Real Implementation (Using Node.js --check)

```javascript
import { execSync } from 'child_process';
import { readdirSync, statSync } from 'fs';
import { join } from 'path';

async _step8_syntaxValidation() {
  const errors = [];
  const validatedFiles = [];

  // 1. Find all generated .mjs files
  const generatedFiles = this._findGeneratedFiles(this.artifacts.outputPath);

  console.log(`Validating syntax for ${generatedFiles.length} files...`);

  // 2. Run node --check on each file
  for (const file of generatedFiles) {
    try {
      // Run syntax check (throws if syntax error)
      execSync(`node --check "${file}"`, {
        encoding: 'utf8',
        stdio: 'pipe'  // Capture stderr
      });

      validatedFiles.push(file);
    } catch (err) {
      // Syntax error found
      errors.push({
        file,
        error: err.stderr || err.message,
        line: this._extractLineNumber(err.stderr)
      });
    }
  }

  // 3. Store results
  const valid = errors.length === 0;

  this.artifacts.validationResults.syntax = {
    valid,
    errors,
    files_checked: generatedFiles.length,
    files_valid: validatedFiles.length
  };

  // 4. Return failure if syntax errors found
  if (!valid) {
    console.error(`❌ Syntax validation failed: ${errors.length} errors`);
    errors.forEach(e => {
      console.error(`  ${e.file}:${e.line} - ${e.error}`);
    });
  }

  return new WorkflowStepResult({
    success: valid,
    step: 'syntax_validation',
    output: {
      files_checked: generatedFiles.length,
      errors_found: errors.length
    }
  });
}

/**
 * Recursively find all .mjs files in output directory
 */
_findGeneratedFiles(dir) {
  const files = [];

  const entries = readdirSync(dir);

  for (const entry of entries) {
    const fullPath = join(dir, entry);
    const stat = statSync(fullPath);

    if (stat.isDirectory()) {
      // Recurse into subdirectories
      files.push(...this._findGeneratedFiles(fullPath));
    } else if (entry.endsWith('.mjs') || entry.endsWith('.js')) {
      files.push(fullPath);
    }
  }

  return files;
}

/**
 * Extract line number from Node.js syntax error message
 * Example: "SyntaxError: Unexpected token '}'\n    at file:///path/to/file.mjs:42:5"
 */
_extractLineNumber(stderr) {
  const match = stderr.match(/:(\d+):\d+/);
  return match ? parseInt(match[1]) : null;
}
```

### Tests

```javascript
// test/bb8020/step8-syntax-validation.test.mjs
import { describe, it, expect } from 'vitest';
import { writeFileSync, mkdirSync } from 'fs';
import { join } from 'path';

describe('Step 8: Syntax Validation - Real Implementation', () => {
  it('should detect syntax errors in generated code', async () => {
    const tmpDir = '/tmp/bb8020-test';
    mkdirSync(tmpDir, { recursive: true });

    // Write file with syntax error
    writeFileSync(
      join(tmpDir, 'invalid.mjs'),
      'function test() { return }}'  // Extra }
    );

    const orchestrator = new BB8020Orchestrator({
      outputPath: tmpDir
    });

    await orchestrator._step8_syntaxValidation();

    // Verify error was caught
    expect(orchestrator.artifacts.validationResults.syntax.valid).toBe(false);
    expect(orchestrator.artifacts.validationResults.syntax.errors.length).toBe(1);
    expect(orchestrator.artifacts.validationResults.syntax.errors[0].file).toContain('invalid.mjs');
  });

  it('should pass validation for valid code', async () => {
    const tmpDir = '/tmp/bb8020-test-valid';
    mkdirSync(tmpDir, { recursive: true });

    // Write valid file
    writeFileSync(
      join(tmpDir, 'valid.mjs'),
      'export function test() { return true; }'
    );

    const orchestrator = new BB8020Orchestrator({
      outputPath: tmpDir
    });

    await orchestrator._step8_syntaxValidation();

    // Verify passed
    expect(orchestrator.artifacts.validationResults.syntax.valid).toBe(true);
    expect(orchestrator.artifacts.validationResults.syntax.errors).toEqual([]);
  });
});
```

---

## Fix 3: Step 9 - Static Analysis (Priority 1)

### Current Implementation (FAKE)

**File**: `packages/decision-fabric/src/bb8020-orchestrator.mjs:400-415`

```javascript
async _step9_staticAnalysis() {
  // ❌ SIMULATION - Hardcoded coverage percentage
  this.artifacts.validationResults.staticAnalysis = {
    coverage: 0.98,  // ← HARDCODED FAKE 98%!
    errors: [],
    warnings: []
  };

  return new WorkflowStepResult({
    success: true,
    step: 'static_analysis'
  });
}
```

### Real Implementation (Using @unrdf/project-engine)

```javascript
import { analyzeJsComplexity } from '@unrdf/project-engine/code-complexity-js.mjs';

async _step9_staticAnalysis() {
  console.log('Running static analysis on generated code...');

  // 1. Run complexity analysis on generated files
  const { store, summary } = await analyzeJsComplexity({
    projectRoot: this.artifacts.outputPath,
    patterns: ['**/*.mjs', '**/*.js'],
    ignore: ['**/node_modules/**']
  });

  // 2. Extract metrics
  const {
    filesAnalyzed,
    averageCyclomatic,
    maintainabilityIndex,
    topRisks
  } = summary;

  // 3. Calculate coverage score based on maintainability
  // Maintainability Index ranges 0-100, higher is better
  // We use it as a proxy for "static analysis coverage"
  const coverage = maintainabilityIndex / 100;

  // 4. Identify errors (high complexity = error)
  const errors = topRisks
    .filter(r => r.cyclomatic > 20)  // Critical complexity
    .map(r => ({
      file: r.file,
      function: r.function,
      cyclomatic: r.cyclomatic,
      reason: `Cyclomatic complexity ${r.cyclomatic} exceeds threshold 20`
    }));

  // 5. Identify warnings (moderate complexity = warning)
  const warnings = topRisks
    .filter(r => r.cyclomatic > 10 && r.cyclomatic <= 20)
    .map(r => ({
      file: r.file,
      function: r.function,
      cyclomatic: r.cyclomatic,
      reason: `Cyclomatic complexity ${r.cyclomatic} exceeds recommended limit 10`
    }));

  // 6. Store results
  this.artifacts.validationResults.staticAnalysis = {
    coverage,
    errors,
    warnings,
    metrics: {
      filesAnalyzed,
      averageCyclomatic,
      maintainabilityIndex
    }
  };

  // 7. Check against BB80/20 requirement (98% static coverage)
  const success = coverage >= 0.98 && errors.length === 0;

  if (!success) {
    console.warn(
      `⚠️  Static analysis failed: coverage ${(coverage * 100).toFixed(1)}% < 98% required`
    );
  }

  if (errors.length > 0) {
    console.error(`❌ ${errors.length} critical complexity errors found`);
  }

  return new WorkflowStepResult({
    success,
    step: 'static_analysis',
    output: {
      coverage: (coverage * 100).toFixed(1) + '%',
      errors_count: errors.length,
      warnings_count: warnings.length,
      avg_cyclomatic: averageCyclomatic.toFixed(2)
    }
  });
}
```

### Tests

```javascript
// test/bb8020/step9-static-analysis.test.mjs
import { describe, it, expect } from 'vitest';
import { writeFileSync, mkdirSync } from 'fs';
import { join } from 'path';

describe('Step 9: Static Analysis - Real Implementation', () => {
  it('should calculate real coverage from code complexity', async () => {
    const tmpDir = '/tmp/bb8020-static';
    mkdirSync(tmpDir, { recursive: true });

    // Write simple code (low complexity)
    writeFileSync(
      join(tmpDir, 'simple.mjs'),
      `
      export function add(a, b) {
        return a + b;
      }
      `
    );

    const orchestrator = new BB8020Orchestrator({
      outputPath: tmpDir
    });

    await orchestrator._step9_staticAnalysis();

    const analysis = orchestrator.artifacts.validationResults.staticAnalysis;

    // Verify NOT hardcoded 98%
    expect(analysis.coverage).not.toBe(0.98);

    // Verify real metrics
    expect(analysis.metrics.filesAnalyzed).toBe(1);
    expect(analysis.metrics.averageCyclomatic).toBeGreaterThan(0);
    expect(analysis.errors).toEqual([]);  // Simple code has no errors
  });

  it('should detect high complexity as error', async () => {
    const tmpDir = '/tmp/bb8020-complex';
    mkdirSync(tmpDir, { recursive: true });

    // Write complex code (high cyclomatic complexity)
    writeFileSync(
      join(tmpDir, 'complex.mjs'),
      `
      export function complex(x) {
        if (x === 1) return 'a';
        else if (x === 2) return 'b';
        else if (x === 3) return 'c';
        // ... repeat 20 times to exceed threshold
        else return 'z';
      }
      `
    );

    const orchestrator = new BB8020Orchestrator({
      outputPath: tmpDir
    });

    await orchestrator._step9_staticAnalysis();

    const analysis = orchestrator.artifacts.validationResults.staticAnalysis;

    // Verify error detected
    expect(analysis.errors.length).toBeGreaterThan(0);
    expect(analysis.errors[0].reason).toContain('complexity');
  });
});
```

---

## Fix 4: Step 10 - Event Logging (Priority 2)

### Current Implementation (NO PERSISTENCE)

**File**: `packages/decision-fabric/src/bb8020-orchestrator.mjs:450-465`

```javascript
async _step10_logging() {
  // ❌ NO ACTUAL LOGGING - just pushes to array (no persistence)
  this.artifacts.logs.push({
    step: 'logging',
    timestamp: new Date().toISOString(),
    success: true
  });

  return new WorkflowStepResult({
    success: true,
    step: 'logging'
  });
}
```

### Real Implementation (Using @unrdf/kgc-4d)

```javascript
import { KGCStore, freezeUniverse, GitBackbone, EVENT_TYPES } from '@unrdf/kgc-4d';

async _step10_logging() {
  console.log('Creating immutable audit trail with KGC 4D...');

  // 1. Initialize KGC store for this workflow
  const kgcStore = new KGCStore({
    nodeId: `bb8020-${this.workflowId}`
  });

  // 2. Log each completed step as an event
  for (const step of this.completedSteps) {
    const { receipt } = await kgcStore.appendEvent(
      {
        type: EVENT_TYPES.UPDATE,
        payload: {
          workflow_id: this.workflowId,
          step_number: step.number,
          step_name: step.name,
          success: step.success,
          duration_ms: step.duration,
          artifacts: step.artifacts
        }
      },
      []  // No RDF deltas for this event
    );

    console.log(`  ✓ Step ${step.number} logged: event ${receipt.id}`);
  }

  // 3. Freeze universe state with Git snapshot
  const git = new GitBackbone(this.gitPath || '/tmp/bb8020-git');
  await git.init();  // Ensure Git repo exists

  const freezeReceipt = await freezeUniverse(kgcStore, git);

  console.log(`  ✓ Universe frozen: ${freezeReceipt.universe_hash}`);
  console.log(`  ✓ Git commit: ${freezeReceipt.git_ref}`);

  // 4. Store deployment receipt
  this.artifacts.deployment_receipt = {
    event_id: freezeReceipt.id,
    universe_hash: freezeReceipt.universe_hash,
    git_ref: freezeReceipt.git_ref,
    timestamp_iso: freezeReceipt.timestamp_iso,
    event_count: freezeReceipt.event_count
  };

  // 5. Keep KGC store for verification
  this.kgcStore = kgcStore;

  return new WorkflowStepResult({
    success: true,
    step: 'logging',
    output: {
      events_logged: this.completedSteps.length,
      universe_hash: freezeReceipt.universe_hash,
      git_ref: freezeReceipt.git_ref.slice(0, 8)  // Short SHA
    }
  });
}
```

### Tests

```javascript
// test/bb8020/step10-logging.test.mjs
import { describe, it, expect } from 'vitest';
import { KGCStore } from '@unrdf/kgc-4d';

describe('Step 10: Event Logging - Real Implementation', () => {
  it('should create immutable audit trail in KGC store', async () => {
    const orchestrator = new BB8020Orchestrator({
      workflowId: 'test-workflow-123'
    });

    // Simulate completed steps
    orchestrator.completedSteps = [
      { number: 1, name: 'parsing', success: true, duration: 100 },
      { number: 2, name: 'pareto', success: true, duration: 200 }
    ];

    await orchestrator._step10_logging();

    // Verify KGC store created
    expect(orchestrator.kgcStore).toBeInstanceOf(KGCStore);
    expect(orchestrator.kgcStore.getEventCount()).toBe(3n);  // 2 steps + 1 freeze

    // Verify deployment receipt
    const receipt = orchestrator.artifacts.deployment_receipt;
    expect(receipt.universe_hash).toBeDefined();
    expect(receipt.git_ref).toBeDefined();
    expect(receipt.event_count).toBe(3);
  });

  it('should allow time-travel to verify workflow state', async () => {
    // Test that reconstructState() can replay events
  });
});
```

---

## Implementation Order

### Week 1: Critical Fixes
1. ✅ **Step 4 - Pattern Matching** (2 days)
   - Replace fake patterns with SPARQL queries
   - Test with real codebase scan
   - Verify ≥64.3% code reuse

2. ✅ **Step 8 - Syntax Validation** (1 day)
   - Implement `node --check` execution
   - Test with valid/invalid files
   - Verify error reporting

3. ✅ **Step 9 - Static Analysis** (2 days)
   - Integrate `@unrdf/project-engine` complexity analysis
   - Calculate real coverage from maintainability index
   - Test with simple/complex code

### Week 2: Quality Improvements
4. ✅ **Step 10 - Event Logging** (2 days)
   - Implement KGC 4D audit trail
   - Test freeze/time-travel
   - Verify Git snapshots

5. ✅ **Step 2 - Socratic Confidence** (2 days)
   - Replace regex with semantic analysis
   - Add confidence scoring (0-100%)
   - Test with vague/specific statements

6. ✅ **Integration Tests** (1 day)
   - End-to-end BB8020 workflow test
   - OTEL validation (≥80/100)
   - Performance benchmarks

---

## Verification Checklist

Before declaring work complete:

### Pattern Matching (Step 4)
- [ ] Scan runs on actual codebase (not fake)
- [ ] Similarity scores calculated from real data
- [ ] SPARQL queries return matches
- [ ] Reuse percentage measured (not hardcoded)
- [ ] Test output shows real file paths

### Syntax Validation (Step 8)
- [ ] `node --check` executes for each file
- [ ] Syntax errors detected and reported
- [ ] Line numbers extracted from errors
- [ ] Valid files pass without errors
- [ ] Test creates file with syntax error → caught

### Static Analysis (Step 9)
- [ ] `analyzeJsComplexity()` runs on generated code
- [ ] Coverage calculated from maintainability index
- [ ] High complexity flagged as errors
- [ ] Metrics include real cyclomatic complexity
- [ ] Test shows coverage ≠ 0.98 (not hardcoded)

### Event Logging (Step 10)
- [ ] KGCStore initialized with events
- [ ] `freezeUniverse()` creates Git snapshot
- [ ] Deployment receipt includes BLAKE3 hash
- [ ] Time-travel can reconstruct state
- [ ] Test verifies event count matches steps

---

## Success Criteria

**Definition of Done**:
1. All 4 simulated steps replaced with real implementations
2. Tests pass with `timeout 5s npm test`
3. OTEL validation score ≥80/100
4. FMEA re-run shows RPN reduction for all HIGH risks
5. Adversarial PM questions answered with evidence:
   - ❓ Did you RUN the code? → Yes, show test output
   - ❓ Can you PROVE it works? → Yes, show SPARQL results / syntax errors caught / coverage scores
   - ❓ What BREAKS if wrong? → Tests fail, not just "looks good"

**Deliverables**:
- [ ] Updated `bb8020-orchestrator.mjs` with real implementations
- [ ] Test suite with ≥80% coverage
- [ ] Documentation of integration patterns
- [ ] Updated FMEA showing risk mitigation
- [ ] Benchmark showing performance meets SLAs

---

## References

- Package Study: `docs/PACKAGE-STUDY-KGC-HOOKS-CORE.md`
- FMEA Analysis: `CLI-FMEA-ANALYSIS.md`
- BB80/20 Thesis: `packages/kgc-4d/docs/explanation/thesis-bigbang-80-20.tex`
- Project Engine: `packages/project-engine/src/fs-scan.mjs`
- KGC Freeze: `packages/kgc-4d/src/freeze.mjs`
