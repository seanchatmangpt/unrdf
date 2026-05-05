# UNRDF v6 Code Quality Standards

**Version**: 6.0.0
**Status**: CANONICAL - All v6 code MUST conform
**Last Updated**: 2025-12-28

---

## 📊 Executive Summary

This document defines **enforceable, measurable** code quality standards for UNRDF v6 rewrite based on empirical analysis of the current codebase and Adversarial PM principles.

**Core Philosophy**: If you can't measure it, you can't enforce it. If you can't enforce it, it's not a standard.

---

## 1. Complexity Limits

### 1.1 Cyclomatic Complexity

**RULE**: Maximum cyclomatic complexity of **10** per function.

**Measurement**:
```bash
# Using eslint-plugin-complexity
npx eslint --rule 'complexity: ["error", 10]' src/
```

**Rationale**:
- Current codebase has functions with deeply nested conditionals (6+ instances found)
- Functions >10 complexity are hard to test (combinatorial explosion)
- McCabe's original research: >10 = high risk

**Enforcement**: ESLint complexity rule (CI/CD gate)

**Examples**:

```javascript
// ❌ BAD - Complexity ~15
export function processWorkItem(item, config) {
  if (item.state === 'pending') {
    if (config.timeout && item.createdAt < Date.now() - config.timeout) {
      if (item.retryCount < config.maxRetries) {
        if (config.circuitBreaker && config.circuitBreaker.isOpen()) {
          return { status: 'circuit_open' };
        } else {
          return retry(item);
        }
      } else {
        return { status: 'max_retries' };
      }
    } else {
      return { status: 'pending' };
    }
  } else if (item.state === 'executing') {
    // ... more nesting
  }
}

// ✅ GOOD - Complexity 3 (extracted helpers)
export function processWorkItem(item, config) {
  if (item.state === 'pending') {
    return processPendingItem(item, config);
  }
  if (item.state === 'executing') {
    return processExecutingItem(item, config);
  }
  return { status: item.state };
}

function processPendingItem(item, config) {
  if (shouldTimeout(item, config)) {
    return handleTimeout(item, config);
  }
  return { status: 'pending' };
}
```

### 1.2 Nesting Depth

**RULE**: Maximum nesting depth of **3** levels (excluding function body).

**Measurement**: ESLint `max-depth` rule

**Examples**:

```javascript
// ❌ BAD - 4 levels deep
export function validateReceipt(receipt) {
  if (receipt) {                           // Level 1
    if (receipt.type === 'execution') {    // Level 2
      if (receipt.payload) {               // Level 3
        if (receipt.payload.hash) {        // Level 4 - TOO DEEP
          return true;
        }
      }
    }
  }
  return false;
}

// ✅ GOOD - Early returns
export function validateReceipt(receipt) {
  if (!receipt) return false;
  if (receipt.type !== 'execution') return false;
  if (!receipt.payload) return false;
  if (!receipt.payload.hash) return false;
  return true;
}

// ✅ EVEN BETTER - Zod validation
const ReceiptSchema = z.object({
  type: z.literal('execution'),
  payload: z.object({
    hash: z.string().length(64),
  }),
});

export function validateReceipt(receipt) {
  const result = ReceiptSchema.safeParse(receipt);
  return result.success;
}
```

### 1.3 Parameter Count

**RULE**: Maximum **4** parameters per function. Use options object for more.

**Measurement**: ESLint `max-params` rule

```javascript
// ❌ BAD
export function executeQuery(store, query, timeout, retries, cache, debug) {
  // ...
}

// ✅ GOOD
export function executeQuery(store, query, options = {}) {
  const {
    timeout = 5000,
    retries = 3,
    cache = true,
    debug = false,
  } = options;
  // ...
}
```

---

## 2. File Size Limits

### 2.1 Lines Per File

**RULE**: Maximum **500 lines** per `.mjs` file (excluding blank lines and comments).

**Current Violations**: 30+ files over limit (largest: 1779 lines)

**Measurement**:
```bash
find packages -name "*.mjs" -exec awk 'NF && !/^[[:space:]]*\/\// && !/^[[:space:]]*\*/' {} \; -exec wc -l {} + | awk '$1 > 500'
```

**Enforcement**: Pre-commit hook + CI check

**Exemptions**:
- Auto-generated `.schema.mjs` files (with `DO NOT EDIT MANUALLY` header)
- Config files (`*.config.mjs`, `vitest.config.mjs`)
- Test files with extensive fixtures (max 800 lines, requires justification)

### 2.2 Lines Per Function

**RULE**: Maximum **50 lines** per function body.

**Current Violations**: 14 functions >50 lines in `/packages/core/src`

**Measurement**: ESLint `max-lines-per-function`

**Examples**:

```javascript
// ❌ BAD - 62 lines
export function createMetricsCollector(config) {
  // 62 lines of initialization, validation, setup...
}

// ✅ GOOD - Extracted helpers
export function createMetricsCollector(config) {
  const validated = validateMetricsConfig(config);
  const storage = initializeMetricsStorage(validated);
  const collectors = setupCollectors(storage);
  return { storage, collectors };
}
```

### 2.3 Exports Per File

**RULE**: Maximum **15 named exports** per file.

**Current Violations**: 9 files (e.g., `guards.mjs`: 31 exports, `yawl-store.mjs`: 19 exports)

**Rationale**:
- High export count = God Object smell
- Violates Single Responsibility Principle
- Makes tree-shaking ineffective

**Solution**: Split into logical modules

```javascript
// ❌ BAD - guards.mjs with 31 exports
export { guardEnv, guardFile, guardNetwork, guardCommand, ... } // 31 total

// ✅ GOOD - Split into domains
// guards/env.mjs
export { guardEnv, validateEnvAccess };

// guards/file.mjs
export { guardFile, validateFilePath };

// guards/network.mjs
export { guardNetwork, validateUrl };

// guards/index.mjs (aggregator)
export * from './env.mjs';
export * from './file.mjs';
export * from './network.mjs';
```

---

## 3. Dependency Rules

### 3.1 Import Restrictions (Layer Architecture)

**RULE**: Enforce 5-layer architecture with NO upward dependencies.

```
Layer 5: APPLICATION (@unrdf/cli, @unrdf/react)
    ↓ can import ↓
Layer 4: KNOWLEDGE SUBSTRATE (@unrdf/hooks, @unrdf/federation, @unrdf/streaming)
    ↓ can import ↓
Layer 3: KGC (@unrdf/kgc-4d, @unrdf/kgc-runtime, @unrdf/receipts)
    ↓ can import ↓
Layer 2: RDF CORE (@unrdf/core - SPARQL, SHACL, parsers)
    ↓ can import ↓
Layer 1: INFRASTRUCTURE (@unrdf/oxigraph, @unrdf/consensus)
```

**FORBIDDEN**:
- ❌ Layer 2 importing from Layer 3 (core importing from kgc-4d)
- ❌ Layer 1 importing from Layer 2 (oxigraph importing from core)

**Enforcement**:
```javascript
// eslint-plugin-import rules
'import/no-restricted-paths': ['error', {
  zones: [
    { target: './packages/core', from: './packages/kgc-*' },
    { target: './packages/core', from: './packages/hooks' },
    { target: './packages/oxigraph', from: './packages/core' },
  ]
}]
```

### 3.2 N3 Import Policy (CRITICAL)

**RULE**: **ZERO** direct imports from `'n3'` in application code.

**Current State**: 12 violations (11 in docs/examples, 1 in justified module)

**Allowed**:
- ✅ `@unrdf/core/rdf/n3-justified-only.mjs` (streaming parser only)
- ✅ Test fixtures with explicit comment: `// N3 import justified: test fixture`

**Forbidden**:
- ❌ `import { Store } from 'n3';` in any application code
- ❌ `import N3 from 'n3';`

**Use Instead**:
```javascript
// ❌ FORBIDDEN
import { Store, Parser } from 'n3';

// ✅ CORRECT
import { createStore } from '@unrdf/oxigraph';
import { Parser } from '@unrdf/core/rdf/n3-justified-only';
```

**Enforcement**:
```bash
# CI check (must return 0)
grep -r "from 'n3'" packages/*/src --include="*.mjs" | grep -v n3-justified | wc -l
```

### 3.3 Circular Dependencies

**RULE**: **ZERO** circular dependencies between packages.

**Detection**:
```bash
npx madge --circular --extensions mjs packages/*/src
```

**Enforcement**: CI gate (fails on any circular dependency)

### 3.4 External Dependencies

**RULE**: All runtime dependencies must be:
1. **Justified**: Document in `package.json` comments why needed
2. **Pinned**: Use exact versions for critical deps
3. **Audited**: No high/critical vulnerabilities (`pnpm audit`)

**Forbidden Dependencies**:
- ❌ `moment` (use native Date or `Temporal` proposal)
- ❌ `lodash` (use native ES6+ methods)
- ❌ `axios` (use native `fetch`)
- ❌ `bluebird` (use native Promises)

**Approved Core Dependencies**:
- ✅ `oxigraph` - Rust SPARQL engine (10-100x faster than N3)
- ✅ `zod` - Runtime validation (type safety)
- ✅ `hash-wasm` - BLAKE3 hashing (cryptographic receipts)
- ✅ `@opentelemetry/api` - Observability (OTEL validation)

---

## 4. Naming Conventions

### 4.1 File Naming

**RULE**: `kebab-case.mjs` for all files.

```
✅ query-executor.mjs
✅ base-receipt.schema.mjs
✅ executor-sync.test.mjs

❌ queryExecutor.mjs (camelCase)
❌ QueryExecutor.mjs (PascalCase)
❌ query_executor.mjs (snake_case)
```

**Special Suffixes**:
- `.schema.mjs` - Zod schemas
- `.test.mjs` - Test files
- `.bench.mjs` - Benchmark files
- `.config.mjs` - Configuration

### 4.2 Function Naming

**RULE**: `camelCase` for functions, descriptive verb-noun pattern.

```javascript
// ✅ GOOD
export function createStore() { }
export function executeQuery() { }
export function validateReceipt() { }
export async function computeBlake3(data) { }

// ❌ BAD
export function store() { }           // Missing verb
export function query() { }           // Ambiguous (noun or verb?)
export function do_validation() { }   // snake_case
export function Query() { }           // PascalCase (reserved for classes)
```

**Async Functions**: Name MUST NOT include `Async` suffix (redundant with `async` keyword).

```javascript
// ✅ GOOD
export async function fetchData() { }

// ❌ BAD
export async function fetchDataAsync() { }  // Redundant
```

### 4.3 Variable Naming

**RULE**:
- `camelCase` for variables
- `SCREAMING_SNAKE_CASE` for constants
- `PascalCase` for classes and Zod schemas

```javascript
// ✅ GOOD
const queryTimeout = 5000;
const BLAKE3_HEX_LENGTH = 64;
const ReceiptSchema = z.object({ /* ... */ });
class CircuitBreaker { }

// ❌ BAD
const QueryTimeout = 5000;           // Should be camelCase
const blake3HexLength = 64;          // Should be SCREAMING_SNAKE_CASE
const receiptSchema = z.object({});  // Should be PascalCase for schemas
```

### 4.4 Package Naming

**RULE**: `@unrdf/kebab-case`

```
✅ @unrdf/kgc-4d
✅ @unrdf/v6-core
✅ @unrdf/knowledge-engine

❌ @unrdf/kgc4d
❌ @unrdf/v6Core
❌ @unrdf/KnowledgeEngine
```

### 4.5 Boolean Naming

**RULE**: Use `is`, `has`, `should`, `can` prefixes.

```javascript
// ✅ GOOD
const isValid = true;
const hasError = false;
const shouldRetry = config.retries > 0;
const canExecute = state === 'ready';

// ❌ BAD
const valid = true;        // Ambiguous (adjective or noun?)
const error = false;       // Misleading (sounds like error object)
const retry = true;        // Ambiguous (verb or boolean?)
```

---

## 5. Documentation Requirements

### 5.1 File Headers (MANDATORY)

**RULE**: Every `.mjs` file MUST have a JSDoc file header.

```javascript
/**
 * @file [Clear description of file purpose]
 * @module [module-name]
 * @description
 * [Extended description with:
 *  - What this module does
 *  - Key concepts/algorithms
 *  - Dependencies and assumptions]
 */
```

**Example**:
```javascript
/**
 * @file Base Receipt Schema - V6 Unified Receipt System
 * @module @unrdf/v6-core/receipts/base-receipt
 * @description
 * Minimal superset schema for all receipt types with BLAKE3 hashing.
 * Provides cryptographic chain verification and deterministic serialization.
 *
 * Key features:
 * - BLAKE3 hash-chaining (64-char hex)
 * - Deterministic JSON serialization
 * - Vector clock causality tracking
 * - Optional attestation/signatures
 */
```

### 5.2 Function Documentation (MANDATORY for Public API)

**RULE**: All exported functions MUST have JSDoc with:
- Brief description (1 sentence)
- `@param` for each parameter (with type and description)
- `@returns` with type and description
- `@throws` for error conditions
- `@example` with working code

**Template**:
```javascript
/**
 * [Brief description - what does this do?]
 *
 * @param {Type} paramName - Description of parameter
 * @param {Object} [options] - Optional configuration
 * @param {number} [options.timeout=5000] - Timeout in milliseconds
 * @returns {ReturnType} Description of return value
 * @throws {ErrorType} When [specific condition]
 *
 * @example
 * const result = functionName(arg1, { timeout: 3000 });
 * console.log(result); // Expected output
 */
export function functionName(paramName, options = {}) {
  // implementation
}
```

### 5.3 Type Definitions (Zod + JSDoc)

**RULE**: Define types with BOTH Zod schemas AND JSDoc typedefs.

```javascript
// Zod schema (runtime validation)
export const ReceiptSchema = z.object({
  id: z.string().uuid(),
  type: z.enum(['execution', 'allocation']),
  timestamp: z.bigint(),
});

// JSDoc typedef (IDE autocomplete)
/**
 * @typedef {Object} Receipt
 * @property {string} id - UUID identifier
 * @property {'execution'|'allocation'} type - Receipt type
 * @property {bigint} timestamp - Nanosecond timestamp
 */
```

### 5.4 Inline Comments

**RULE**: Comment **WHY**, not **WHAT**. Code should be self-documenting for "what".

```javascript
// ❌ BAD - Comments state the obvious
// Increment counter
counter++;

// Loop through items
for (const item of items) {
  // Process item
  process(item);
}

// ✅ GOOD - Comments explain WHY
// Increment counter to track total operations (needed for circuit breaker threshold)
counter++;

// Process items sequentially to maintain causal ordering (YAWL requirement)
for (const item of items) {
  await process(item);
}
```

### 5.5 TODO/FIXME Policy

**RULE**:
- **ZERO** `TODO` comments in main branch (move to GitHub Issues)
- `FIXME` allowed ONLY with:
  - Issue number: `// FIXME(#123): Description`
  - Assignee: `// FIXME(@username): Description`
  - Deadline: `// FIXME(2025-12-31): Description`

**Current State**: 23 TODO/FIXME comments (ALL must be removed or converted)

```javascript
// ❌ BAD
// TODO: Optimize this
// FIXME: Handle edge case

// ✅ GOOD
// FIXME(#456, @alice, 2025-01-15): Handle race condition when receipt chain forks
// See https://github.com/unrdf/unrdf/issues/456
```

---

## 6. Antipatterns to Ban

### 6.1 Default Exports (BANNED)

**RULE**: **ZERO** `export default` in implementation files.

**Current Violations**: 20+ files (graph-analytics, fusion, yawl-kafka)

**Rationale**:
- Breaks tree-shaking
- Makes refactoring harder (can't find all usages)
- Inconsistent naming across imports

```javascript
// ❌ FORBIDDEN
export default function createStore() { }
export default { createStore, executeQuery };

// ✅ CORRECT
export function createStore() { }
export function executeQuery() { }

// ✅ ALLOWED (config files only)
// vitest.config.mjs
export default defineConfig({ /* ... */ });
```

**Exemptions**:
- Config files: `*.config.mjs`, `next.config.mjs`
- Framework requirements: Next.js pages, Vite config

### 6.2 Console Logging (BANNED in src/)

**RULE**: **ZERO** `console.log/warn/error` in `/packages/*/src/**/*.mjs`

**Current Violations**: 190 instances in `packages/core/src`

**Use Instead**:
```javascript
// ❌ FORBIDDEN
console.log('Query executed');
console.error('Validation failed', error);

// ✅ CORRECT
import { trace } from '@opentelemetry/api';
const span = trace.getActiveSpan();
span?.addEvent('query_executed', { queryId });

// OR use structured logger
import { logger } from './logger.mjs';
logger.info('query_executed', { queryId, duration });
```

**Allowed**:
- ✅ CLI tools (`packages/cli/src/**/*.mjs`)
- ✅ Test files (`*.test.mjs`)
- ✅ Scripts (`scripts/**/*.mjs`)

### 6.3 Defensive Programming (ANTI-PATTERN)

**RULE**: NO redundant null checks. Use Zod validation at boundaries.

```javascript
// ❌ ANTI-PATTERN (current codebase has this)
export function addQuad(store, quadData) {
  if (!store) {
    throw new TypeError('store is required');
  }
  if (!quadData) {
    throw new TypeError('quadData is required');
  }
  // ... every function checks the same thing
}

// ✅ CORRECT (validate at API boundary once)
const AddQuadParamsSchema = z.tuple([
  z.object({ add: z.function() }), // store
  z.object({ subject: z.any(), predicate: z.any(), object: z.any() })
]);

export function addQuad(store, quadData) {
  AddQuadParamsSchema.parse([store, quadData]);
  store.add(quadData);
}

// ✅ EVEN BETTER (pure function, no side effects)
export function addQuad(store, quadData) {
  // Trust caller to validate. Document preconditions.
  return store.add(quadData);
}
```

**From CLAUDE.md**:
> "Add defensive code (guards hide real bugs)" - DON'T DO

### 6.4 Try-Catch Everywhere (ANTI-PATTERN)

**RULE**: Handle errors at boundaries, not every function.

**Current State**: 115 try-catch blocks in `packages/core/src` (many redundant)

```javascript
// ❌ ANTI-PATTERN
export function computeHash(data) {
  try {
    return blake3(data);
  } catch (err) {
    console.error('Hash failed', err);
    throw err;
  }
}

// ✅ CORRECT (let errors propagate)
export function computeHash(data) {
  return blake3(data);
}

// ✅ Handle at boundary (API layer)
export async function createReceipt(payload) {
  try {
    const hash = await computeHash(payload);
    return { hash, payload };
  } catch (error) {
    // Log with context and re-throw enhanced error
    throw new ReceiptError('Failed to create receipt', { cause: error, payload });
  }
}
```

### 6.5 God Objects (ANTI-PATTERN)

**RULE**: Files with >15 exports MUST be split.

**Current Violations**: See section 2.3

### 6.6 Magic Numbers (ANTI-PATTERN)

**RULE**: Extract constants with descriptive names.

```javascript
// ❌ BAD
if (timeout > 300000) { }
const hash = data.slice(0, 64);

// ✅ GOOD
const MAX_TIMEOUT_MS = 300000; // 5 minutes
const BLAKE3_HEX_LENGTH = 64;

if (timeout > MAX_TIMEOUT_MS) { }
const hash = data.slice(0, BLAKE3_HEX_LENGTH);
```

### 6.7 Auto-Generated Weak Schemas (ANTI-PATTERN)

**Current Issue**: `config.schema.mjs` has `z.unknown()` everywhere (weak typing)

**RULE**: Auto-generated schemas MUST be reviewed and tightened.

```javascript
// ❌ WEAK (auto-generated)
export const parseEnvParamsSchema = z.tuple([z.unknown().optional()]);

// ✅ STRONG (manually tightened)
export const parseEnvParamsSchema = z.tuple([
  z.record(z.string(), z.string()).optional()
]);
```

---

## 7. ESLint Configuration

### 7.1 Required ESLint Rules

**RULE**: All packages MUST use this `.eslintrc.json`:

```json
{
  "env": {
    "es2022": true,
    "node": true
  },
  "parserOptions": {
    "ecmaVersion": 2022,
    "sourceType": "module"
  },
  "extends": [
    "eslint:recommended",
    "plugin:jsdoc/recommended",
    "prettier"
  ],
  "plugins": ["jsdoc"],
  "rules": {
    // Complexity
    "complexity": ["error", 10],
    "max-depth": ["error", 3],
    "max-nested-callbacks": ["error", 3],
    "max-params": ["error", 4],
    "max-lines-per-function": ["error", { "max": 50, "skipBlankLines": true, "skipComments": true }],
    "max-lines": ["error", { "max": 500, "skipBlankLines": true, "skipComments": true }],

    // File structure
    "max-statements": ["error", 20],
    "max-classes-per-file": ["error", 1],

    // Naming
    "camelcase": ["error", { "properties": "never" }],
    "id-length": ["error", { "min": 2, "exceptions": ["x", "y", "z", "i", "j", "k", "id", "tx"] }],

    // Imports
    "no-restricted-imports": ["error", {
      "patterns": [{
        "group": ["n3"],
        "message": "Use @unrdf/oxigraph instead. See CLAUDE.md section 3.2"
      }]
    }],

    // Console/Debug
    "no-console": ["error", { "allow": ["warn", "error"] }],
    "no-debugger": "error",

    // Best practices
    "no-var": "error",
    "prefer-const": "error",
    "prefer-arrow-callback": "error",
    "no-implicit-globals": "error",
    "no-eval": "error",
    "no-implied-eval": "error",
    "no-new-func": "error",

    // JSDoc
    "jsdoc/require-jsdoc": ["error", {
      "require": {
        "FunctionDeclaration": true,
        "ClassDeclaration": true,
        "MethodDefinition": true
      },
      "publicOnly": true
    }],
    "jsdoc/require-param": "error",
    "jsdoc/require-param-type": "error",
    "jsdoc/require-returns": "error",
    "jsdoc/require-returns-type": "error",
    "jsdoc/require-example": ["warn", {
      "exemptedBy": ["private", "internal"]
    }],
    "jsdoc/check-types": "error",
    "jsdoc/no-undefined-types": "error"
  }
}
```

### 7.2 Prettier Configuration

**RULE**: Use this `.prettierrc`:

```json
{
  "semi": true,
  "singleQuote": true,
  "tabWidth": 2,
  "useTabs": false,
  "trailingComma": "es5",
  "printWidth": 100,
  "arrowParens": "avoid",
  "endOfLine": "lf"
}
```

### 7.3 CI/CD Gates

**RULE**: ALL checks MUST pass before merge:

```yaml
# .github/workflows/quality.yml
quality-gates:
  steps:
    - name: Lint (zero violations)
      run: pnpm lint --max-warnings=0

    - name: Type check (Zod + JSDoc)
      run: pnpm test:types

    - name: Test coverage (≥80%)
      run: pnpm test:coverage --coverage.threshold.lines=80

    - name: File size check (≤500 lines)
      run: |
        violations=$(find packages -name "*.mjs" -not -path "*/test/*" -not -name "*.config.mjs" \
          -exec awk 'NF && !/^[[:space:]]*\/\// {count++} END {if (count > 500) print FILENAME}' {} \;)
        if [ -n "$violations" ]; then
          echo "Files exceeding 500 lines:"
          echo "$violations"
          exit 1
        fi

    - name: N3 import check (must be 0)
      run: |
        violations=$(grep -r "from 'n3'" packages/*/src --include="*.mjs" | grep -v n3-justified | wc -l)
        if [ "$violations" -ne 0 ]; then
          echo "Found $violations N3 direct imports"
          exit 1
        fi

    - name: Console.log check (must be 0 in src/)
      run: |
        violations=$(grep -r "console\." packages/*/src --include="*.mjs" | wc -l)
        if [ "$violations" -ne 0 ]; then
          echo "Found $violations console statements in src/"
          exit 1
        fi

    - name: Circular dependency check
      run: npx madge --circular --extensions mjs packages/*/src
```

---

## 8. Testing Standards

### 8.1 Test File Structure

**RULE**:
- Test files: `*.test.mjs` (co-located with source or in `/test`)
- Max 800 lines per test file
- Use `describe` blocks for organization

```javascript
/**
 * @file Tests for base-receipt.mjs
 */

import { describe, it, expect } from 'vitest';
import { createReceipt, verifyReceipt } from './base-receipt.mjs';

describe('base-receipt', () => {
  describe('createReceipt', () => {
    it('should create valid receipt with all required fields', () => {
      const receipt = createReceipt({ data: 'test' });
      expect(receipt).toHaveProperty('id');
      expect(receipt).toHaveProperty('receiptHash');
    });

    it('should throw on invalid payload', () => {
      expect(() => createReceipt(null)).toThrow('Invalid payload');
    });
  });

  describe('verifyReceipt', () => {
    it('should verify valid receipt', async () => {
      const receipt = createReceipt({ data: 'test' });
      const result = await verifyReceipt(receipt);
      expect(result.valid).toBe(true);
    });
  });
});
```

### 8.2 Coverage Requirements

**RULE**: Minimum 80% coverage (lines, functions, branches, statements)

```json
// vitest.config.mjs
export default defineConfig({
  test: {
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      lines: 80,
      functions: 80,
      branches: 80,
      statements: 80,
      exclude: [
        '**/*.test.mjs',
        '**/*.config.mjs',
        '**/test/**',
        '**/examples/**'
      ]
    }
  }
});
```

### 8.3 Test Timeout (Andon Principle)

**RULE**: Default 5 seconds, explicit justification for longer.

```javascript
// ✅ GOOD
describe('query executor', () => {
  it('should execute simple query', async () => {
    // Inherits default 5s timeout
    await executeQuery(store, 'SELECT * WHERE { ?s ?p ?o }');
  });

  it('should handle complex query', async () => {
    // Explicit timeout with justification
    await executeQuery(store, complexQuery);
  }, { timeout: 15000 }); // Justification: 10K triple dataset + inference
});
```

---

## 9. Enforcement Strategy

### 9.1 Automated Checks (CI/CD)

**Priority 1 (BLOCK merge)**:
- ✅ ESLint complexity/size violations
- ✅ N3 direct imports
- ✅ Console.log in src/
- ✅ Test coverage <80%
- ✅ Circular dependencies

**Priority 2 (WARN, block after 2025-01-15)**:
- ⚠️ Missing JSDoc on public functions
- ⚠️ Files >500 lines
- ⚠️ Functions >50 lines
- ⚠️ >15 exports per file

### 9.2 Pre-commit Hooks

```bash
# .husky/pre-commit
#!/usr/bin/env sh

pnpm lint --max-warnings=0
pnpm test:fast

# Check file sizes
find packages -name "*.mjs" -not -path "*/test/*" -not -name "*.config.mjs" \
  -exec awk 'NF && !/^[[:space:]]*\/\// {count++} END {if (count > 500) print FILENAME " exceeds 500 lines"}' {} \;

# Check N3 imports
n3_violations=$(grep -r "from 'n3'" packages/*/src --include="*.mjs" | grep -v n3-justified | wc -l)
if [ "$n3_violations" -ne 0 ]; then
  echo "ERROR: Found $n3_violations N3 direct imports. Use @unrdf/oxigraph instead."
  exit 1
fi
```

### 9.3 Code Review Checklist

**Reviewers MUST verify**:
- [ ] JSDoc complete for all public functions
- [ ] Zod schemas for all inputs/outputs
- [ ] No console.log in src/
- [ ] No default exports (except config files)
- [ ] Functions <50 lines
- [ ] Files <500 lines
- [ ] Complexity <10
- [ ] No N3 direct imports
- [ ] Test coverage ≥80%
- [ ] Examples in JSDoc work

---

## 10. Migration Plan (Current → V6)

### 10.1 Prioritized Fixes

**Phase 1 (Week 1-2)**: Critical violations
1. Remove 12 N3 direct imports → Use @unrdf/oxigraph
2. Remove 190 console.log → Use OTEL or structured logger
3. Split 9 God Object files (>15 exports)

**Phase 2 (Week 3-4)**: Structure violations
4. Split 30+ files >500 lines
5. Extract 14+ functions >50 lines
6. Add JSDoc to 200+ undocumented public functions

**Phase 3 (Week 5-6)**: Quality improvements
7. Tighten 100+ auto-generated weak schemas (z.unknown → specific types)
8. Remove 23 TODO/FIXME → Convert to GitHub Issues
9. Remove 20+ default exports

### 10.2 Measurement & Tracking

**Weekly Quality Report**:
```bash
# Run full quality scan
node scripts/quality-report.mjs --json > quality-$(date +%Y%m%d).json

# Track trends
node scripts/quality-trends.mjs quality-*.json
```

**Metrics Dashboard**:
- Total violations by category
- Trend (reducing vs. increasing)
- Top 10 worst files
- Package-level compliance scores

---

## 11. Exceptions & Waivers

### 11.1 Requesting Exception

**Process**:
1. Create GitHub Issue: `[QUALITY-WAIVER] <file>: <rule>`
2. Provide justification (technical reason, not convenience)
3. Get approval from 2+ maintainers
4. Document in file header:
   ```javascript
   /**
    * @file ...
    * QUALITY WAIVER (#456): Exceeds 500 lines due to auto-generated enum (823 variants)
    * Approved: @alice, @bob (2025-01-10)
    */
   ```

### 11.2 Auto-Approved Exceptions

- Config files (`*.config.mjs`) - default exports allowed
- Auto-generated schemas - file size limit relaxed to 1000 lines
- Test fixtures with large datasets - max 1000 lines with justification
- CLI entry points - console.log allowed

---

## 12. Success Metrics

**V6 Launch Criteria (ALL must be met)**:

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| ESLint violations | 0 | TBD | ❌ |
| Files >500 lines | 0 | 30+ | ❌ |
| Functions >50 lines | 0 | 14+ | ❌ |
| N3 direct imports | 0 | 12 | ❌ |
| Console.log in src/ | 0 | 190 | ❌ |
| Test coverage | ≥80% | ~70% | ❌ |
| JSDoc coverage | 100% (public API) | ~60% | ❌ |
| Circular deps | 0 | 0 | ✅ |
| Default exports (non-config) | 0 | 20+ | ❌ |
| Quality Score | ≥90/100 | TBD | ❌ |

---

## Appendix A: Quick Reference

### One-Command Checks

```bash
# Check all quality metrics
pnpm run quality

# Check specific file
npx eslint --rule 'complexity: ["error", 10]' packages/core/src/store.mjs

# Find oversized files
find packages -name "*.mjs" -not -path "*/test/*" \
  -exec awk 'NF && !/^[[:space:]]*\/\// {count++} END {if (count > 500) print FILENAME ": " count " lines"}' {} \;

# Find oversized functions
find packages -name "*.mjs" -exec awk '/^(export )?function/ {name=$NF; lines=0} {lines++} /^}/ && lines>50 {print FILENAME ":" name ":" lines}' {} \;

# Check N3 imports
grep -r "from 'n3'" packages --include="*.mjs" | grep -v n3-justified

# Check console.log
grep -r "console\." packages/*/src --include="*.mjs"

# Check circular deps
npx madge --circular --extensions mjs packages/*/src
```

### Cheat Sheet

**Before committing**:
```bash
✅ Run pnpm lint
✅ Run pnpm test:fast
✅ Check file <500 lines
✅ Check function <50 lines
✅ Add JSDoc to public functions
✅ Use Zod for validation
✅ No console.log in src/
✅ No default exports
```

---

## Appendix B: Tool Recommendations

### Recommended Extensions (VS Code)

```json
{
  "recommendations": [
    "dbaeumer.vscode-eslint",
    "esbenp.prettier-vscode",
    "streetsidesoftware.code-spell-checker",
    "aaron-bond.better-comments",
    "usernamehw.errorlens"
  ]
}
```

### Recommended Scripts

```json
// package.json
{
  "scripts": {
    "quality": "node scripts/quality-report.mjs",
    "quality:fix": "eslint --fix src/ && prettier --write src/",
    "complexity": "npx eslint --rule 'complexity: [\"error\", 10]' src/",
    "size:files": "find src -name '*.mjs' -exec wc -l {} + | sort -rn | head -20",
    "size:functions": "find src -name '*.mjs' -exec awk '/^export function/ {name=$NF; lines=0} {lines++} /^}/ && lines>50 {print FILENAME \":\" name \":\" lines}' {} \\;"
  }
}
```

---

**Document Version**: 1.0.0
**Effective Date**: 2025-01-01
**Review Cycle**: Quarterly
**Next Review**: 2025-04-01
