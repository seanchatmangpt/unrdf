# API Documentation Improvement Action Plan

**Generated:** 2025-12-21
**Target Completion:** Q1 2025
**Owner:** UNRDF Core Team

---

## Overview

This action plan addresses the findings from the comprehensive API documentation audit of all 21 UNRDF packages. Current overall documentation score: **83% (B+)**. Target: **95% (A)**.

---

## Priority 1: CRITICAL GAPS (Week 1-2)

### 1.1 Empty Packages ❌ BLOCKER
**Packages:** `@unrdf/browser`, `@unrdf/react`
**Status:** No README, no documentation, no source files
**Impact:** HIGH - Breaks user trust and discoverability

**Action:**
```bash
# Option A: Create documentation
cd packages/browser && echo "# @unrdf/browser\n\n**Status:** Planned\n\nBrowser SDK for UNRDF (coming in v5.1)" > README.md

# Option B: Deprecate
# Add to package.json: "deprecated": "This package has been merged into @unrdf/core"
```

**Deliverables:**
- [ ] Decision: Document or deprecate
- [ ] Update package.json with status
- [ ] Add README (if keeping)
- [ ] Update monorepo docs to reflect status

---

### 1.2 Error Code Convention 🔴 CRITICAL
**Current State:** No consistent error codes across packages
**Impact:** HIGH - Poor error handling, debugging difficulties
**Affected:** All packages

**Action:**
Create `packages/core/src/errors.mjs`:
```javascript
/**
 * UNRDF Error Codes Convention
 *
 * Format: [PACKAGE]_[CATEGORY]_[SPECIFIC]
 *
 * Categories:
 * - VALIDATION: Input validation errors
 * - RUNTIME: Runtime execution errors
 * - IO: File/network I/O errors
 * - CONFIG: Configuration errors
 * - SPARQL: Query parsing/execution errors
 */

export const ErrorCodes = {
  // Core errors
  CORE_VALIDATION_INVALID_QUAD: 'CORE_VALIDATION_INVALID_QUAD',
  CORE_SPARQL_PARSE_ERROR: 'CORE_SPARQL_PARSE_ERROR',
  CORE_SPARQL_TIMEOUT: 'CORE_SPARQL_TIMEOUT',

  // Streaming errors
  STREAMING_BUFFER_OVERFLOW: 'STREAMING_BUFFER_OVERFLOW',
  STREAMING_SYNC_CHECKSUM_MISMATCH: 'STREAMING_SYNC_CHECKSUM_MISMATCH',

  // Hooks errors
  HOOKS_VALIDATION_FAILED: 'HOOKS_VALIDATION_FAILED',
  HOOKS_TRANSFORM_INVALID_RETURN: 'HOOKS_TRANSFORM_INVALID_RETURN',

  // CLI errors
  CLI_IO_FILE_NOT_FOUND: 'CLI_IO_FILE_NOT_FOUND',
  CLI_SPARQL_INVALID_SYNTAX: 'CLI_SPARQL_INVALID_SYNTAX',

  // Federation errors
  FEDERATION_PEER_UNREACHABLE: 'FEDERATION_PEER_UNREACHABLE',
  FEDERATION_QUERY_TIMEOUT: 'FEDERATION_QUERY_TIMEOUT',
};

export class UnrdfError extends Error {
  constructor(code, message, details = {}) {
    super(message);
    this.name = 'UnrdfError';
    this.code = code;
    this.details = details;
  }
}
```

**Deliverables:**
- [ ] Create errors.mjs with error code enum
- [ ] Update all packages to use error codes
- [ ] Document error codes in each package README
- [ ] Add error code reference to monorepo docs

**Timeline:** 3 days

---

### 1.3 Knowledge Engine API Reference 📚 CRITICAL
**Package:** `@unrdf/knowledge-engine`
**Current State:** 39 exports, minimal documentation
**Impact:** HIGH - Users cannot use advanced features

**Action:**
Create `packages/knowledge-engine/docs/API-REFERENCE.md`:
```markdown
# @unrdf/knowledge-engine API Reference

## Core APIs

### KnowledgeHookManager
Class for managing knowledge hooks.

**Constructor:**
```typescript
constructor(options?: {
  autoExecute?: boolean;
  strict?: boolean;
})
```

**Methods:**
- `defineHook(config: HookConfig): Hook`
- `executeHook(hookName: string, quad: Quad): HookResult`
- `listHooks(): Hook[]`

**Example:**
[Working example here]

### TransactionManager
[Full API documentation]

[... continue for all 39 exports]
```

**Deliverables:**
- [ ] Create API-REFERENCE.md with all 39 exports
- [ ] Add JSDoc to all exports
- [ ] Create 3 working examples
- [ ] Update README to link to API reference

**Timeline:** 5 days

---

## Priority 2: HIGH IMPACT (Week 3-4)

### 2.1 Error Documentation 📝
**Current Coverage:** 45% average
**Target:** 80%+
**Top Priority Packages:** streaming, composables, knowledge-engine

**Template:**
```javascript
/**
 * Execute a SPARQL query
 *
 * @param {Store} store - RDF store
 * @param {string} sparql - SPARQL query string
 * @returns {QueryResult} Query results
 *
 * @throws {UnrdfError} CORE_VALIDATION_INVALID_STORE - If store is null/invalid
 * @throws {UnrdfError} CORE_SPARQL_PARSE_ERROR - If SPARQL syntax is invalid
 * @throws {UnrdfError} CORE_SPARQL_TIMEOUT - If query exceeds timeout
 *
 * @example
 * try {
 *   const results = executeQuery(store, 'SELECT * WHERE { ?s ?p ?o }');
 * } catch (err) {
 *   if (err.code === 'CORE_SPARQL_TIMEOUT') {
 *     console.log('Query timed out, try adding LIMIT');
 *   }
 * }
 */
```

**Deliverables:**
- [ ] Add @throws JSDoc to all public APIs
- [ ] Document error recovery patterns
- [ ] Add error handling examples
- [ ] Create ERROR-HANDLING.md guide

**Timeline:** 4 days

---

### 2.2 Working Examples Verification 🧪
**Current Coverage:** 65%
**Target:** 90%+

**Action:**
```bash
# Create automated example runner
cat > scripts/test-examples.mjs << 'EOF'
#!/usr/bin/env node
import { glob } from 'glob';
import { spawn } from 'child_process';

const examples = await glob('examples/**/*.mjs');
let failed = 0;

for (const example of examples) {
  console.log(`Testing ${example}...`);
  const result = spawn('node', [example], { timeout: 30000 });

  if (result.status !== 0) {
    console.error(`❌ ${example} failed`);
    failed++;
  } else {
    console.log(`✅ ${example} passed`);
  }
}

if (failed > 0) {
  console.error(`\n❌ ${failed}/${examples.length} examples failed`);
  process.exit(1);
} else {
  console.log(`\n✅ All ${examples.length} examples passed`);
}
EOF

chmod +x scripts/test-examples.mjs
```

**Deliverables:**
- [ ] Create automated example runner
- [ ] Fix all broken examples
- [ ] Add examples for undocumented features
- [ ] Add examples to CI/CD pipeline

**Timeline:** 3 days

---

### 2.3 Performance Documentation 📊
**Current Coverage:** 15%
**Target:** 70%+

**Template:**
```markdown
## Performance Characteristics

| Operation | Latency | Throughput | Memory | Notes |
|-----------|---------|------------|--------|-------|
| `addQuad()` | <1ms | 100K ops/sec | O(1) | Oxigraph indexed store |
| `executeQuerySync()` | 1-50ms | 1K queries/sec | O(n) | Depends on result size |
| `executeHookChain()` | <1ms | 1M ops/sec | O(1) | Pooled quad optimization |

**Memory Usage:**
- Base: 10 MB (empty store)
- Per 1M quads: ~200 MB
- Hook registry: <1 MB (100 hooks)

**Optimization Tips:**
- Use `executeQuerySync()` for <10ms queries (UnrdfStore)
- Use pooled quads for transformations (zero-allocation)
- Batch quad operations for better throughput
```

**Deliverables:**
- [ ] Add performance section to 10 core packages
- [ ] Run benchmarks for all core operations
- [ ] Document memory usage patterns
- [ ] Add optimization tips

**Timeline:** 4 days

---

### 2.4 Composables Documentation 🎨
**Package:** `@unrdf/composables`
**Current Coverage:** 72%
**Target:** 90%+

**Action:**
```typescript
// packages/composables/src/composables/use-graph.mjs

/**
 * Reactive RDF graph composable for Vue 3
 *
 * @param {string|Ref<string>} graphUrl - URL or ref to RDF graph
 * @param {Object} options - Configuration options
 * @param {boolean} [options.autoLoad=true] - Auto-load on mount
 * @param {number} [options.pollInterval] - Polling interval (ms)
 * @returns {Object} Reactive graph state
 * @returns {Ref<Store>} return.store - RDF store (reactive)
 * @returns {Ref<boolean>} return.loading - Loading state
 * @returns {Ref<Error|null>} return.error - Error state
 * @returns {Function} return.reload - Manual reload function
 *
 * @example
 * import { useGraph } from '@unrdf/composables';
 *
 * export default {
 *   setup() {
 *     const { store, loading, error, reload } = useGraph('http://api/graph');
 *
 *     return { store, loading, error, reload };
 *   }
 * }
 */
export function useGraph(graphUrl, options = {}) {
  // Implementation
}
```

**Deliverables:**
- [ ] Add comprehensive JSDoc for all 6 composables
- [ ] Create working Vue 3 examples
- [ ] Document reactivity patterns
- [ ] Add TypeScript type definitions

**Timeline:** 3 days

---

## Priority 3: MEDIUM IMPACT (Week 5-6)

### 3.1 CHANGELOG.md for All Packages
**Current State:** No CHANGELOG.md in most packages
**Impact:** MEDIUM - Difficult to track breaking changes

**Template:**
```markdown
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [5.0.0-beta.1] - 2025-01-15

### Added
- New synchronous SPARQL API (executeQuerySync)
- Oxigraph store integration

### Changed
- **BREAKING:** Async APIs now require UnrdfStore instead of N3 Store

### Deprecated
- N3 Store compatibility mode (will be removed in v6.0.0)

### Fixed
- Query timeout not working with abort signals

### Security
- Updated dependencies to fix CVE-2024-XXXXX
```

**Deliverables:**
- [ ] Create CHANGELOG.md for all packages
- [ ] Document all breaking changes since v4.0
- [ ] Add changelog to release process
- [ ] Automate changelog generation (conventional commits)

**Timeline:** 2 days

---

### 3.2 Edge Case Documentation
**Current Coverage:** 40%
**Target:** 80%+

**Examples:**
```javascript
/**
 * Create a change feed
 *
 * @param {Store} store - RDF store to monitor
 * @param {Object} [config] - Configuration
 * @param {number} [config.maxHistorySize=10000] - Max changes to buffer
 * @returns {ChangeFeed} Change feed instance
 *
 * **Edge Cases:**
 * - If maxHistorySize is exceeded, oldest changes are dropped (FIFO)
 * - If store is null, throws TypeError immediately
 * - If config.maxHistorySize is Infinity, buffer grows unbounded (memory leak risk!)
 * - Subscriptions are not automatically cleaned up (call unsubscribe())
 *
 * **Limitations:**
 * - Maximum 1000 concurrent subscriptions
 * - Change events are synchronous (blocking)
 * - No built-in persistence (changes lost on restart)
 */
```

**Deliverables:**
- [ ] Document edge cases for all public APIs
- [ ] Add "Limitations" section to READMEs
- [ ] Document memory limits
- [ ] Document concurrency limits

**Timeline:** 3 days

---

### 3.3 OpenAPI Specification 🌐
**Target:** @unrdf/cli, @unrdf/federation (REST endpoints)

**Action:**
Create `packages/cli/docs/openapi.yaml`:
```yaml
openapi: 3.0.0
info:
  title: UNRDF CLI API
  version: 5.0.0
  description: Command-line interface for RDF operations

paths:
  /graph/{id}:
    get:
      summary: Get graph by ID
      parameters:
        - name: id
          in: path
          required: true
          schema:
            type: string
      responses:
        '200':
          description: Graph data
          content:
            application/json:
              schema:
                type: object
                properties:
                  id: { type: string }
                  quads: { type: array }
        '404':
          description: Graph not found
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/Error'

components:
  schemas:
    Error:
      type: object
      properties:
        code: { type: string }
        message: { type: string }
```

**Deliverables:**
- [ ] Create OpenAPI spec for CLI REST endpoints (if any)
- [ ] Create OpenAPI spec for federation coordinator
- [ ] Generate API docs from OpenAPI spec
- [ ] Add OpenAPI validation to CI/CD

**Timeline:** 3 days

---

### 3.4 TypeScript Type Definitions
**Current State:** JSDoc only, no .d.ts files
**Impact:** MEDIUM - Poor IDE support for TypeScript users

**Action:**
```bash
# Generate .d.ts from JSDoc
pnpm add -D typescript @types/node

# packages/core/tsconfig.json
{
  "compilerOptions": {
    "allowJs": true,
    "declaration": true,
    "emitDeclarationOnly": true,
    "outDir": "dist"
  },
  "include": ["src/**/*.mjs"]
}

# package.json
{
  "types": "dist/index.d.ts",
  "scripts": {
    "build:types": "tsc"
  }
}
```

**Deliverables:**
- [ ] Generate .d.ts files for all packages
- [ ] Add types field to package.json
- [ ] Test TypeScript integration
- [ ] Add type generation to build process

**Timeline:** 2 days

---

## Metrics & Success Criteria

### Documentation Coverage Targets

| Metric | Current | Target | Timeline |
|--------|---------|--------|----------|
| **Overall Documentation Score** | 83% | 95% | 6 weeks |
| **JSDoc Coverage** | 85% | 95% | 4 weeks |
| **Error Documentation** | 45% | 80% | 3 weeks |
| **Working Examples** | 65% | 90% | 4 weeks |
| **Performance Docs** | 15% | 70% | 5 weeks |
| **Edge Case Docs** | 40% | 80% | 5 weeks |

### Package-Specific Targets

| Package | Current | Target | Priority |
|---------|---------|--------|----------|
| @unrdf/browser | 0% | 60% | P1 |
| @unrdf/react | 0% | 60% | P1 |
| @unrdf/knowledge-engine | 68% | 90% | P1 |
| @unrdf/composables | 72% | 90% | P2 |
| @unrdf/streaming | 82% | 92% | P2 |
| @unrdf/dark-matter | 55% | 80% | P3 |
| @unrdf/domain | 58% | 80% | P3 |

---

## Weekly Milestones

### Week 1-2: Critical Gaps
- ✅ Document or deprecate @unrdf/browser & @unrdf/react
- ✅ Implement error code convention
- ✅ Create knowledge-engine API reference

### Week 3-4: High Impact
- ✅ Add error documentation to all packages
- ✅ Verify and fix all examples
- ✅ Add performance characteristics
- ✅ Complete composables documentation

### Week 5-6: Medium Impact
- ✅ Create CHANGELOG.md for all packages
- ✅ Document edge cases
- ✅ Create OpenAPI specifications
- ✅ Generate TypeScript type definitions

---

## Automation & Tools

### 1. Documentation Linter
```bash
# scripts/lint-docs.mjs
# Checks for:
# - Missing @param tags
# - Missing @returns tags
# - Missing @throws tags
# - Missing examples
# - Undocumented exports
```

### 2. Example Runner
```bash
# scripts/test-examples.mjs
# Runs all examples and reports failures
```

### 3. Type Coverage Report
```bash
# scripts/type-coverage.mjs
# Reports JSDoc type coverage per package
```

### 4. Documentation Generator
```bash
# scripts/generate-docs.mjs
# Auto-generates API reference from JSDoc
```

---

## Sign-off

**Created by:** Claude Code (API Documentation Specialist)
**Reviewed by:** [UNRDF Core Team]
**Approved by:** [Project Lead]
**Status:** Draft → Review → Approved → In Progress

**Next Review:** 2025-01-05 (Week 2 milestone check)
