# UNRDF latest - GOAP Implementation Plan

**Version:** latest Minor Release
**Created:** 2025-11-16
**Methodology:** SPARC-Enhanced Goal-Oriented Action Planning (GOAP)
**Timeline:** 23-33 days total
**Priority:** Security → Browser → Observability → Documentation → Testing

---

## Executive Summary

This GOAP plan breaks down UNRDF latest into 4 major milestones with 12 parallel work streams, 47 specific tasks, and clear acceptance criteria based on OTEL span validation.

### Success Criteria (OTEL-Validated)
- ✅ **Security**: vm2 completely removed, isolated-vm operational (OTEL validation score ≥ 90/100)
- ✅ **Browser**: All core features work in browser context (90%+ compatibility)
- ✅ **Observability**: OTEL validation framework score ≥ 90/100 for all v3.1 features
- ✅ **Documentation**: Complete migration guides + browser usage guides
- ✅ **Testing**: Maintain 100% coverage + add browser/security tests

---

## Current State Analysis

### Files Requiring vm2 → isolated-vm Migration
1. `/src/knowledge-engine/effect-sandbox.mjs` - Main sandbox (189 lines, vm2 import on line 194)
2. `/src/security/sandbox-adapter.mjs` - Security adapter (36 lines, vm2 import on line 4)
3. `/src/knowledge-engine/schemas.mjs` - Schema definitions (vm2 enum)
4. `package.json` - Remove vm2 dependency (line 115)

### Reference Implementation (Already Complete)
- `/knowledge-engine/server/utils/secure-sandbox.mjs` - isolated-vm implementation (308 lines) ✅
- `/knowledge-engine/server/utils/sandbox-threat-detector.mjs` - Threat detection ✅

### Browser Compatibility Gaps
1. **Missing polyfills in browser-shims.mjs**:
   - Worker threads API incomplete (line 134-206)
   - No Comunica browser adapter
   - Missing IndexedDB integration for RDF storage
   - No Service Worker support for caching

2. **Incomplete browser implementations**:
   - `/src/knowledge-engine/browser.mjs` - Mock implementations need real logic
   - `/src/knowledge-engine/effect-sandbox-browser.mjs` - Needs Web Worker integration
   - No browser build pipeline (no bundler config)

### OTEL Validation Gaps
1. **Legacy CLI validation** (to be removed):
   - Old CLI tests checking exit codes instead of OTEL spans
   - No validation for isolated-vm security metrics
   - Missing browser-specific OTEL spans

2. **Validation framework needs**:
   - New span definitions for isolated-vm operations
   - Browser-specific performance thresholds
   - Security threat score validation

---

## MILESTONE 1: Security Foundation (vm2 → isolated-vm)

**Duration:** 8-10 days
**Priority:** CRITICAL
**Dependency:** None (can start immediately)
**OTEL Acceptance:** Security validation score ≥ 90/100

### Work Stream 1.1: Core Sandbox Migration (3-4 days)

**Goal:** Replace vm2 with isolated-vm in main library

**Current State:**
- vm2 used in `effect-sandbox.mjs` (line 194)
- vm2 used in `sandbox-adapter.mjs` (line 4)
- Reference implementation exists in `/knowledge-engine/server/utils/secure-sandbox.mjs`

**Target State:**
- isolated-vm fully integrated
- All vm2 imports removed
- Backward-compatible API maintained

**Actions:**

1. **Copy and adapt isolated-vm implementation** (1 day)
   - File: `/src/knowledge-engine/effect-sandbox.mjs`
   - Changes:
     ```javascript
     // BEFORE (line 194):
     const vm2Module = await import('vm2');
     VM = vm2Module.VM;

     // AFTER:
     import ivm from 'isolated-vm';
     // Use reference implementation from /knowledge-engine/server/utils/secure-sandbox.mjs
     ```
   - Copy `createIsolate`, `registerEffect`, `executeEffect` methods from knowledge-engine
   - Maintain existing `_executeInWorker` as fallback
   - Add `_executeInIsolate` implementation (currently throws Error on line 244)

2. **Update sandbox-adapter.mjs** (0.5 days)
   - File: `/src/security/sandbox-adapter.mjs`
   - Replace vm2 VM with isolated-vm Isolate
   - Add environment variable: `UNRDF_SANDBOX_ENGINE` (already mentioned in comments line 32)
   - Maintain same API surface

3. **Update schemas** (0.5 days)
   - File: `/src/knowledge-engine/schemas.mjs`
   - Change enum from `['vm2', 'worker', 'isolate']` to `['worker', 'isolate']`
   - Default to 'isolate' instead of 'worker'

4. **Integration testing** (1 day)
   - Verify all existing tests pass with isolated-vm
   - Add security escape tests (VM breakout attempts)
   - Performance benchmarking: isolated-vm vs vm2 baseline

**Acceptance Criteria (OTEL-Validated):**
- ✅ All unit tests pass with isolated-vm
- ✅ OTEL span `sandbox.isolate.create` exists with `status.ok`
- ✅ OTEL span `sandbox.isolate.execute` has `execution.time < 1000ms`
- ✅ Security validation score ≥ 85/100 (intermediate)
- ✅ No vm2 references in `grep -r "vm2" src/`

**Risk Mitigation:**
- Keep worker fallback for environments without isolated-vm
- Feature flag: `UNRDF_SANDBOX_ENGINE=worker` for rollback
- Extensive security testing before removal of vm2

---

### Work Stream 1.2: Threat Detection Integration (2 days)

**Goal:** Add ML-based threat detection from knowledge-engine to main library

**Current State:**
- Threat detector exists in `/knowledge-engine/server/utils/sandbox-threat-detector.mjs`
- Not integrated into main library

**Target State:**
- Threat detection available in main library
- Optional security layer for effect registration
- OTEL instrumentation for threat scores

**Actions:**

1. **Copy threat detector to main library** (0.5 days)
   - Create: `/src/security/threat-detector.mjs`
   - Copy from `/knowledge-engine/server/utils/sandbox-threat-detector.mjs`
   - Remove h3 dependencies (knowledge-engine-specific)
   - Make standalone module

2. **Integrate with effect-sandbox** (1 day)
   - File: `/src/knowledge-engine/effect-sandbox.mjs`
   - Add `enableThreatDetection` config option
   - Call threat detector before `_executeInIsolate`
   - Block execution if threat score ≥ blockThreshold (default 80)

3. **Add OTEL instrumentation** (0.5 days)
   - Span: `security.threat.analyze` with attributes:
     - `threat.score` (number)
     - `threat.severity` (string)
     - `threat.patterns` (array)
     - `threat.blocked` (boolean)

**Acceptance Criteria (OTEL-Validated):**
- ✅ OTEL span `security.threat.analyze` exists for all effect executions
- ✅ Span attribute `threat.score` < 80 for safe code
- ✅ Span attribute `threat.blocked = true` for malicious code
- ✅ Security validation score ≥ 90/100 (full)

**Files Changed:**
- NEW: `/src/security/threat-detector.mjs` (300 lines)
- EDIT: `/src/knowledge-engine/effect-sandbox.mjs` (+50 lines)
- EDIT: `/src/knowledge-engine/schemas.mjs` (+5 lines for threat config)

---

### Work Stream 1.3: Dependency & Package Updates (1-2 days)

**Goal:** Remove vm2, add isolated-vm, update all dependencies

**Current State:**
- vm2@latest in dependencies (package.json line 115)
- No isolated-vm dependency

**Target State:**
- isolated-vm@latest+ installed
- vm2 removed completely
- All security dependencies updated

**Actions:**

1. **Add isolated-vm** (0.5 days)
   ```bash
   pnpm add isolated-vm@^latest
   ```
   - Verify build on Linux/Mac/Windows
   - Test in Docker containers
   - Document native compilation requirements

2. **Remove vm2** (0.5 days)
   ```bash
   pnpm remove vm2
   ```
   - Verify no breaking changes
   - Update lock files
   - Clean cache

3. **Update security dependencies** (0.5 days)
   - `@noble/hashes@^latest` (cryptographic utilities)
   - OTEL packages to latest stable
   - Audit with `pnpm audit`

4. **CI/CD pipeline updates** (0.5 days)
   - Update GitHub Actions to build isolated-vm
   - Add platform-specific build jobs (Linux, macOS, Windows)
   - Cache native modules

**Acceptance Criteria:**
- ✅ `pnpm install` succeeds on all platforms
- ✅ No vm2 in `package.json` or lock files
- ✅ isolated-vm builds successfully in CI
- ✅ All existing tests pass

**Risk Factors:**
- isolated-vm requires native compilation (C++ toolchain)
- Platform-specific build issues (Windows especially)
- CI build time increase (~2-3 min for native compilation)

---

### Work Stream 1.4: Security Validation & Testing (2-3 days)

**Goal:** Comprehensive security testing for isolated-vm implementation

**Current State:**
- Basic sandbox tests exist
- No VM escape tests
- No threat detection tests in main library

**Target State:**
- 100% coverage for security modules
- VM escape prevention validated
- Threat detection accuracy measured

**Actions:**

1. **VM escape tests** (1 day)
   - File: `test/security/vm-escape.test.mjs`
   - Test cases:
     - Constructor escape attempts
     - Prototype pollution
     - Process access attempts
     - Filesystem access attempts
     - Network access attempts
     - Import/require attempts
   - All should be blocked with proper errors

2. **Threat detector tests** (1 day)
   - File: `test/security/threat-detector.test.mjs`
   - Test all 13 threat patterns
   - Validate scoring algorithm
   - Test caching behavior
   - Benchmark performance (< 10ms per analysis)

3. **Integration tests** (0.5 days)
   - Test effect execution with threat detection enabled
   - Test effect execution with isolated-vm backend
   - Test fallback to worker threads
   - Test resource limits (memory, CPU)

4. **Security documentation** (0.5 days)
   - Document threat detection patterns
   - Document security configuration
   - Document VM escape prevention
   - Add security best practices guide

**Acceptance Criteria (OTEL-Validated):**
- ✅ Security validation score = 95/100
- ✅ All VM escape tests pass (code blocked)
- ✅ Threat detector accuracy > 95%
- ✅ OTEL span `security.validation` passes all checks
- ✅ Code coverage ≥ 100% for security modules

**Files Created:**
- `test/security/vm-escape.test.mjs` (200 lines)
- `test/security/threat-detector.test.mjs` (250 lines)
- `test/security/isolated-vm-integration.test.mjs` (150 lines)
- `docs/security/THREAT-DETECTION.md` (50 lines)

---

## MILESTONE 2: Browser Compatibility (90%+ Feature Parity)

**Duration:** 5-7 days
**Priority:** HIGH
**Dependencies:** None (parallel with Milestone 1)
**OTEL Acceptance:** Browser validation score ≥ 85/100

### Work Stream 2.1: Polyfill & Shim Completion (2-3 days)

**Goal:** Complete all browser polyfills for Node.js APIs

**Current State (browser-shims.mjs analysis):**
- ✅ randomUUID - complete
- ✅ path utilities - complete
- ⚠️ Worker class - incomplete (line 134-206)
- ❌ No Comunica browser integration
- ❌ No IndexedDB for RDF storage
- ❌ No Service Worker caching

**Target State:**
- All Node.js APIs have browser polyfills
- Comunica works in browser
- IndexedDB RDF store available
- Service Worker caching optional

**Actions:**

1. **Complete Worker polyfill** (0.5 days)
   - File: `/src/knowledge-engine/browser-shims.mjs`
   - Fix BrowserWorker class (currently incomplete)
   - Add proper message passing
   - Add terminate() support
   - Test in real browser (Chrome, Firefox, Safari)

2. **Add Comunica browser adapter** (1 day)
   - Install: `pnpm add @comunica/query-sparql-link-traversal`
   - Create: `/src/knowledge-engine/comunica-browser.mjs`
   - Wrapper for browser-compatible Comunica engine
   - Support SPARQL over HTTP/HTTPS
   - Cache query results in memory

3. **Add IndexedDB RDF store** (1 day)
   - Create: `/src/knowledge-engine/indexeddb-store.mjs`
   - Implement RDF quad storage in IndexedDB
   - Add SPARQL query support over IndexedDB
   - Implement quad serialization/deserialization
   - Add cache invalidation

4. **Add Service Worker support** (0.5-1 day)
   - Create: `/src/knowledge-engine/service-worker.mjs`
   - Cache SPARQL queries in Service Worker
   - Cache RDF data files
   - Offline-first strategy
   - Background sync for updates

**Acceptance Criteria (OTEL-Validated):**
- ✅ OTEL span `browser.worker.create` succeeds in Chrome/Firefox/Safari
- ✅ OTEL span `browser.indexeddb.store` succeeds
- ✅ Comunica queries work in browser (OTEL span `browser.sparql.query`)
- ✅ Service Worker installs successfully (if enabled)
- ✅ Browser validation score ≥ 70/100 (intermediate)

**Browser Testing Matrix:**
| Browser | Version | Core | SPARQL | IndexedDB | Worker |
|---------|---------|------|--------|-----------|--------|
| Chrome  | 120+    | ✅   | ✅     | ✅        | ✅     |
| Firefox | 115+    | ✅   | ✅     | ✅        | ✅     |
| Safari  | 16+     | ✅   | ✅     | ✅        | ⚠️     |
| Edge    | 120+    | ✅   | ✅     | ✅        | ✅     |

---

### Work Stream 2.2: Browser Build Pipeline (1-2 days)

**Goal:** Create browser bundles with proper code splitting

**Current State:**
- No browser build configuration
- No bundler setup (esbuild, rollup, or vite)
- No browser entry points

**Target State:**
- Browser bundle with tree-shaking
- Separate bundles for core/full features
- Source maps for debugging
- < 500KB gzipped bundle size

**Actions:**

1. **Add esbuild configuration** (0.5 days)
   - Create: `build.browser.config.mjs`
   - Entry points:
     - `src/browser.mjs` → `dist/unrdf.browser.js` (full)
     - `src/browser-core.mjs` → `dist/unrdf.browser-core.js` (minimal)
   - External dependencies: `@comunica/*` (CDN loaded)
   - Output: ESM + IIFE formats
   - Minification + source maps

2. **Create browser entry points** (0.5 days)
   - File: `src/browser.mjs` (full bundle)
     - Export all browser-compatible modules
     - Exclude Node.js-only features (worker_threads, fs, etc.)
   - File: `src/browser-core.mjs` (minimal bundle)
     - Export only core RDF features
     - Exclude SPARQL, reasoning, validation

3. **Add package.json exports** (0.5 days)
   ```json
   "exports": {
     ".": {
       "browser": "./dist/unrdf.browser.js",
       "node": "./src/index.mjs",
       "default": "./src/index.mjs"
     },
     "./browser": "./dist/unrdf.browser.js",
     "./browser-core": "./dist/unrdf.browser-core.js"
   }
   ```

4. **CI build integration** (0.5 days)
   - Add browser build to `npm run build`
   - Validate bundle size < 500KB gzipped
   - Upload bundles to CDN (jsdelivr, unpkg)

**Acceptance Criteria:**
- ✅ Browser bundle builds successfully
- ✅ Bundle size < 500KB gzipped (core < 200KB)
- ✅ Works in Vite/Webpack/Rollup projects
- ✅ Tree-shaking removes unused code
- ✅ Source maps work in browser DevTools

**Files Created:**
- `build.browser.config.mjs` (100 lines)
- `src/browser.mjs` (50 lines)
- `src/browser-core.mjs` (30 lines)

---

### Work Stream 2.3: Browser Testing Infrastructure (2 days)

**Goal:** Playwright-based browser testing for all features

**Current State:**
- No browser tests
- Only Node.js tests in Vitest

**Target State:**
- Playwright tests for Chrome, Firefox, Safari
- Browser-specific OTEL validation
- Visual regression testing (optional)

**Actions:**

1. **Install Playwright** (0.5 days)
   ```bash
   pnpm add -D @playwright/test playwright
   ```
   - Configure for Chrome, Firefox, Safari
   - Add to CI pipeline

2. **Create browser test suite** (1 day)
   - File: `test/browser/core-features.spec.mjs`
   - Test cases:
     - RDF parsing in browser
     - SPARQL queries in browser
     - IndexedDB storage operations
     - Worker-based execution
     - Service Worker caching
   - OTEL span validation in browser

3. **Add visual regression tests** (0.5 days)
   - File: `test/browser/visual-regression.spec.mjs`
   - Screenshot comparison for UI components
   - Store baseline screenshots in `test/browser/__screenshots__/`

**Acceptance Criteria (OTEL-Validated):**
- ✅ All browser tests pass in Chrome/Firefox/Safari
- ✅ OTEL span `browser.test.execute` succeeds
- ✅ Browser validation score ≥ 85/100 (full)
- ✅ No console errors in browser tests
- ✅ Performance metrics meet thresholds

**Test Matrix:**
| Feature | Chrome | Firefox | Safari | Edge |
|---------|--------|---------|--------|------|
| Parse Turtle | ✅ | ✅ | ✅ | ✅ |
| SPARQL Query | ✅ | ✅ | ✅ | ✅ |
| IndexedDB Store | ✅ | ✅ | ✅ | ✅ |
| Web Workers | ✅ | ✅ | ⚠️ | ✅ |
| Service Worker | ✅ | ✅ | ⚠️ | ✅ |

---

## MILESTONE 3: Observability Enhancement (OTEL Framework)

**Duration:** 4-5 days
**Priority:** MEDIUM
**Dependencies:** Milestone 1 (for security spans), Milestone 2 (for browser spans)
**OTEL Acceptance:** Overall validation score ≥ 90/100

### Work Stream 3.1: New Span Definitions (1-2 days)

**Goal:** Define OTEL spans for all v3.1 features

**Current State:**
- Existing spans: parse, query, validate, reason, hook
- Missing: isolated-vm, browser, threat detection

**Target State:**
- Complete span catalog for v3.1
- Standardized span attributes
- Performance baselines

**Actions:**

1. **Security spans** (0.5 days)
   - `security.isolate.create` - Isolate creation
   - `security.isolate.execute` - Effect execution in isolate
   - `security.threat.analyze` - Threat detection analysis
   - `security.threat.block` - Code blocked by threat detector
   - Attributes: `threat.score`, `threat.severity`, `threat.patterns`, `isolate.id`, `memory.used`, `cpu.time`

2. **Browser spans** (0.5 days)
   - `browser.worker.create` - Web Worker creation
   - `browser.indexeddb.store` - IndexedDB operations
   - `browser.serviceworker.cache` - Service Worker caching
   - `browser.comunica.query` - SPARQL query in browser
   - Attributes: `browser.name`, `browser.version`, `worker.id`, `cache.hit`, `query.cached`

3. **Performance baselines** (0.5 days)
   - Define thresholds for each span
   - Security operations: < 100ms
   - Browser operations: < 500ms
   - IndexedDB operations: < 200ms

4. **Documentation** (0.5 days)
   - File: `docs/observability/OTEL-SPANS-v3.1.md`
   - List all spans with attributes
   - Provide example queries
   - Document threshold values

**Acceptance Criteria:**
- ✅ All new spans documented
- ✅ Span attributes follow semantic conventions
- ✅ Thresholds validated in production-like environment
- ✅ Documentation complete

**Files Created:**
- `docs/observability/OTEL-SPANS-v3.1.md` (100 lines)
- `src/validation/span-definitions-v3.1.mjs` (150 lines)

---

### Work Stream 3.2: Validation Framework Updates (2 days)

**Goal:** Update OTEL validation to include v3.1 features

**Current State (from validation/run-all.mjs):**
- 6 feature validations: knowledge-engine, cli-parse, cli-query, cli-validate, cli-hook, transaction-manager
- Missing: security, browser, threat-detection

**Target State:**
- 9 feature validations (add 3 new)
- Updated thresholds for performance
- 90+/100 overall score achievable

**Actions:**

1. **Add security validation suite** (0.5 days)
   - File: `validation/security.validation.mjs`
   - Features to validate:
     - `isolated-vm-creation` - Isolate creation
     - `isolated-vm-execution` - Effect execution
     - `threat-detection` - Malicious code detection
   - Expected spans: `security.isolate.*`, `security.threat.*`
   - Thresholds: latency < 100ms, error rate < 0.01

2. **Add browser validation suite** (0.5 days)
   - File: `validation/browser.validation.mjs`
   - Features to validate:
     - `browser-parsing` - RDF parsing in browser
     - `browser-query` - SPARQL in browser
     - `browser-storage` - IndexedDB operations
   - Expected spans: `browser.*`
   - Thresholds: latency < 500ms, error rate < 0.05

3. **Update comprehensive suite** (0.5 days)
   - File: `validation/run-all.mjs`
   - Add security and browser to `comprehensiveSuite.features`
   - Update overall score calculation
   - Add feature weights (security 30%, core 40%, browser 20%, other 10%)

4. **Remove legacy CLI checks** (0.5 days)
   - Remove exit code checks in CLI validation
   - Use only OTEL span validation
   - Update CLI validation to use spans exclusively

**Acceptance Criteria (OTEL-Validated):**
- ✅ Security validation score ≥ 90/100
- ✅ Browser validation score ≥ 85/100
- ✅ Overall validation score ≥ 90/100
- ✅ No legacy CLI exit code checks remain
- ✅ `node validation/run-all.mjs comprehensive` succeeds

**Files Changed:**
- NEW: `validation/security.validation.mjs` (200 lines)
- NEW: `validation/browser.validation.mjs` (200 lines)
- EDIT: `validation/run-all.mjs` (+50 lines)
- EDIT: `validation/cli.validation.mjs` (-30 lines, remove legacy checks)

---

### Work Stream 3.3: Metrics & Dashboard (1-2 days)

**Goal:** Production-ready metrics and observability dashboard

**Current State:**
- Basic OTEL instrumentation
- No aggregated metrics
- No dashboard

**Target State:**
- Prometheus metrics exported
- Grafana dashboard for v3.1 features
- Alerting rules configured

**Actions:**

1. **Add Prometheus metrics** (0.5 days)
   - Security metrics:
     - `unrdf_security_isolate_created_total` (counter)
     - `unrdf_security_threat_score` (histogram)
     - `unrdf_security_threats_blocked_total` (counter)
   - Browser metrics:
     - `unrdf_browser_query_duration_seconds` (histogram)
     - `unrdf_browser_indexeddb_operations_total` (counter)
     - `unrdf_browser_cache_hits_total` (counter)

2. **Create Grafana dashboard** (1 day)
   - File: `observability/grafana-dashboard-v3.1.json`
   - Panels:
     - Security threat score over time
     - Isolate creation rate
     - Browser query latency (p50, p95, p99)
     - Cache hit ratio
     - Error rate by feature

3. **Add alerting rules** (0.5 days)
   - File: `observability/alerts-v3.1.yml`
   - Alerts:
     - High threat score (> 80)
     - Isolate creation failures
     - Browser query failures > 5%
     - Memory leaks in isolates

**Acceptance Criteria:**
- ✅ Prometheus metrics scraped successfully
- ✅ Grafana dashboard displays all v3.1 metrics
- ✅ Alerts fire on test failures
- ✅ Documentation for dashboard setup

**Files Created:**
- `observability/grafana-dashboard-v3.1.json` (500 lines)
- `observability/alerts-v3.1.yml` (50 lines)
- `docs/observability/METRICS-v3.1.md` (80 lines)

---

## MILESTONE 4: Documentation & Release Preparation

**Duration:** 3-4 days
**Priority:** MEDIUM
**Dependencies:** All previous milestones (documentation requires completed features)
**Acceptance:** Complete, accurate documentation

### Work Stream 4.1: Migration Guides (1-2 days)

**Goal:** Help users migrate from v3.0.x to latest

**Actions:**

1. **vm2 → isolated-vm migration guide** (0.5 days)
   - File: `docs/migration/v3.0-to-v3.1-security.md`
   - Sections:
     - Why we migrated (vm2 deprecated, security issues)
     - Breaking changes (API differences)
     - Configuration changes
     - Performance implications
     - Troubleshooting
   - Code examples:
     ```javascript
     // v3.0 (vm2)
     const sandbox = new EffectSandbox({ type: 'vm2' })

     // v3.1 (isolated-vm)
     const sandbox = new EffectSandbox({ type: 'isolate' })
     ```

2. **Browser usage guide** (0.5 days)
   - File: `docs/guides/browser-usage.md`
   - Sections:
     - Installing in browser projects
     - Bundle selection (full vs core)
     - IndexedDB setup
     - Service Worker configuration
     - Performance optimization
   - Code examples for React, Vue, Svelte, Vanilla JS

3. **OTEL validation guide** (0.5 days)
   - File: `docs/guides/otel-validation.md`
   - Sections:
     - Understanding OTEL spans
     - Running validations
     - Interpreting scores
     - Custom validation rules
     - CI/CD integration

**Acceptance Criteria:**
- ✅ All migration guides complete
- ✅ Code examples tested and working
- ✅ No broken links in documentation
- ✅ Peer review approved

**Files Created:**
- `docs/migration/v3.0-to-v3.1-security.md` (120 lines)
- `docs/guides/browser-usage.md` (150 lines)
- `docs/guides/otel-validation.md` (100 lines)

---

### Work Stream 4.2: API Documentation Updates (1 day)

**Goal:** Update JSDoc and generated API docs

**Actions:**

1. **Update JSDoc comments** (0.5 days)
   - All modified files get updated JSDoc
   - New parameters documented
   - Examples updated

2. **Generate API documentation** (0.5 days)
   ```bash
   npm run docs
   ```
   - Verify all new APIs documented
   - Check for broken references
   - Update homepage with v3.1 features

**Acceptance Criteria:**
- ✅ JSDoc coverage 100% for new code
- ✅ API docs build without errors
- ✅ Homepage highlights v3.1 features

---

### Work Stream 4.3: Release Notes & Changelog (1 day)

**Goal:** Comprehensive release notes for latest

**Actions:**

1. **Create release notes** (0.5 days)
   - File: `docs/releases/latest-RELEASE-NOTES.md`
   - Sections:
     - Overview
     - Breaking Changes
     - New Features
     - Bug Fixes
     - Performance Improvements
     - Migration Guide (link)
     - Contributors

2. **Update CHANGELOG.md** (0.5 days)
   - Add latest section
   - List all changes with PR/issue links
   - Categorize: Security, Features, Fixes, Docs, Chores

**Acceptance Criteria:**
- ✅ Release notes complete
- ✅ CHANGELOG.md updated
- ✅ Version numbers updated in all files

**Files Changed:**
- NEW: `docs/releases/latest-RELEASE-NOTES.md` (200 lines)
- EDIT: `CHANGELOG.md` (+50 lines)
- EDIT: `package.json` (version: "latest")

---

## MILESTONE 5: Testing & Quality Assurance

**Duration:** 3-4 days (parallel with Milestone 4)
**Priority:** CRITICAL
**Dependencies:** All feature work complete
**Acceptance:** 100% test coverage maintained

### Work Stream 5.1: Test Coverage Maintenance (1-2 days)

**Goal:** Maintain 100% test coverage for all new code

**Actions:**

1. **Security tests** (0.5 days)
   - Unit tests for isolated-vm integration
   - Unit tests for threat detector
   - Integration tests for security flow
   - Coverage target: 100%

2. **Browser tests** (0.5 days)
   - Unit tests for browser shims
   - Unit tests for IndexedDB store
   - Integration tests in Playwright
   - Coverage target: 90%+ (browser tests don't count for Node coverage)

3. **OTEL validation tests** (0.5 days)
   - Unit tests for validation runner
   - Unit tests for new span definitions
   - Integration tests for comprehensive validation

**Acceptance Criteria (OTEL-Validated):**
- ✅ `npm run test` passes with 100% coverage
- ✅ OTEL span `test.coverage` shows `coverage.percent = 100`
- ✅ All browser tests pass in Playwright
- ✅ Validation framework score = 95/100

---

### Work Stream 5.2: Integration Testing (1 day)

**Goal:** End-to-end testing of all v3.1 features

**Actions:**

1. **Create E2E test suite** (0.5 days)
   - File: `test/e2e/v3.1-features.test.mjs`
   - Test scenarios:
     - Create effect with threat detection → execute in isolated-vm
     - Parse RDF in browser → query with SPARQL → store in IndexedDB
     - Run comprehensive OTEL validation → verify score ≥ 90
     - Test worker fallback when isolated-vm unavailable

2. **Performance regression tests** (0.5 days)
   - Benchmark isolated-vm vs vm2 (historical data)
   - Benchmark browser bundle size
   - Benchmark OTEL validation time
   - Set regression thresholds (no more than 10% slower)

**Acceptance Criteria:**
- ✅ All E2E tests pass
- ✅ No performance regressions > 10%
- ✅ Browser bundle size < 500KB
- ✅ OTEL validation time < 60s for comprehensive suite

---

### Work Stream 5.3: Manual QA & User Acceptance (1 day)

**Goal:** Human validation of all features

**Actions:**

1. **Security QA** (0.5 days)
   - Manual testing of threat detector with real malicious code
   - Verify VM escape prevention
   - Test resource limits (memory, CPU)
   - Security review by team

2. **Browser QA** (0.5 days)
   - Manual testing in Chrome, Firefox, Safari
   - Test on mobile browsers (iOS Safari, Chrome Android)
   - Verify UI/UX in example applications
   - Accessibility check (WCAG 2.1 AA)

**Acceptance Criteria:**
- ✅ Security team approves isolated-vm implementation
- ✅ Browser compatibility verified on 4+ browsers
- ✅ No critical bugs found
- ✅ User acceptance testing passed

---

## Critical Path Analysis

### Sequential Dependencies (Critical Path: 23 days minimum)

```
Day 1-4:   Milestone 1.1 (Core Sandbox Migration)
Day 3-4:   Milestone 1.2 (Threat Detection) - depends on 1.1
Day 4-6:   Milestone 1.4 (Security Testing) - depends on 1.1, 1.2
Day 7-9:   Milestone 2.3 (Browser Testing) - depends on 2.1, 2.2
Day 10-12: Milestone 3.2 (Validation Updates) - depends on 1.4, 2.3
Day 13-14: Milestone 3.3 (Metrics) - depends on 3.2
Day 15-17: Milestone 4 (Documentation) - depends on all features
Day 18-20: Milestone 5.1 (Coverage) - depends on all features
Day 20-21: Milestone 5.2 (Integration) - depends on 5.1
Day 21-22: Milestone 5.3 (QA) - depends on 5.2
Day 23:    Final OTEL validation + release prep
```

### Parallel Work Streams (Can reduce timeline to 20 days)

```
Week 1 (Days 1-7):
  Parallel:
    - Milestone 1.1 + 1.2 + 1.3 (Security) → Team A
    - Milestone 2.1 + 2.2 (Browser) → Team B
    - Milestone 3.1 (Span Definitions) → Team C

Week 2 (Days 8-14):
  Parallel:
    - Milestone 1.4 (Security Testing) → Team A
    - Milestone 2.3 (Browser Testing) → Team B
    - Milestone 3.2 + 3.3 (Validation & Metrics) → Team C

Week 3 (Days 15-20):
  Parallel:
    - Milestone 4 (Documentation) → Team A
    - Milestone 5.1 + 5.2 (Testing) → Team B
    - Milestone 5.3 (QA) → Team C
```

---

## Risk Assessment & Mitigation

### High-Risk Items

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| isolated-vm build failures on Windows | 🔴 Critical | 30% | Pre-test on Windows, provide pre-built binaries |
| Browser bundle size > 500KB | 🟡 High | 40% | Code splitting, lazy loading, tree-shaking |
| OTEL validation score < 90 | 🟡 High | 25% | Incremental validation, early feedback loop |
| Safari compatibility issues | 🟡 High | 35% | Extensive Safari testing, polyfills |
| Performance regression > 10% | 🟡 High | 20% | Continuous benchmarking, profiling |

### Medium-Risk Items

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Breaking API changes | 🟡 Medium | 15% | Maintain backward compatibility, deprecation warnings |
| Documentation outdated | 🟡 Medium | 30% | Documentation-driven development, peer review |
| Test coverage drops below 100% | 🟡 Medium | 20% | Automated coverage checks in CI, pre-commit hooks |

---

## Success Metrics (OTEL-Validated)

### Security (Milestone 1)
- ✅ **isolated-vm operational**: OTEL span `security.isolate.execute` success rate ≥ 99%
- ✅ **Threat detection accuracy**: ≥ 95% true positive rate, < 5% false positive rate
- ✅ **VM escape prevention**: 0 successful escapes in security tests
- ✅ **Security validation score**: ≥ 95/100

### Browser (Milestone 2)
- ✅ **Feature parity**: ≥ 90% of core features work in browser
- ✅ **Browser compatibility**: Chrome, Firefox, Safari, Edge (latest 2 versions)
- ✅ **Bundle size**: < 500KB gzipped (full), < 200KB gzipped (core)
- ✅ **Browser validation score**: ≥ 85/100

### Observability (Milestone 3)
- ✅ **OTEL coverage**: 100% of v3.1 features have spans
- ✅ **Validation score**: ≥ 90/100 overall
- ✅ **Dashboard operational**: Grafana dashboard displays all v3.1 metrics
- ✅ **Alerting functional**: Alerts fire on test conditions

### Documentation (Milestone 4)
- ✅ **Migration guides**: Complete for vm2 → isolated-vm
- ✅ **Browser guides**: Complete usage examples
- ✅ **API docs**: 100% coverage of new APIs
- ✅ **Release notes**: Comprehensive latest notes

### Testing (Milestone 5)
- ✅ **Test coverage**: Maintain 100% for Node.js code
- ✅ **Browser tests**: All Playwright tests pass
- ✅ **E2E tests**: All scenarios pass
- ✅ **No regressions**: < 10% performance impact

---

## Resource Requirements

### Team Composition (Optimal)
- **2-3 Senior Developers**: Core implementation (Milestones 1-3)
- **1-2 Browser Specialists**: Browser compatibility (Milestone 2)
- **1 DevOps Engineer**: CI/CD, observability (Milestone 3)
- **1 Technical Writer**: Documentation (Milestone 4)
- **1 QA Engineer**: Testing, validation (Milestone 5)

### Infrastructure
- **CI/CD**: GitHub Actions with Windows/Mac/Linux runners
- **Browser Testing**: Playwright with BrowserStack for mobile
- **Observability**: Prometheus + Grafana stack
- **Storage**: npm registry for package publishing

### Timeline Summary
- **Best Case**: 20 days (with 3 parallel teams)
- **Expected**: 25 days (with 2 parallel teams)
- **Worst Case**: 33 days (with sequential work)

---

## Next Steps (Immediate Actions)

1. **Create feature branches**:
   ```bash
   git checkout -b feature/isolated-vm-migration
   git checkout -b feature/browser-compatibility
   git checkout -b feature/otel-validation-v3.1
   ```

2. **Set up project tracking**:
   - Create GitHub project board for latest
   - Add all 47 tasks as issues
   - Assign to team members

3. **Run baseline OTEL validation**:
   ```bash
   node validation/run-all.mjs comprehensive
   # Record baseline score for comparison
   ```

4. **Begin Milestone 1.1** (Core Sandbox Migration):
   - Start with `/src/knowledge-engine/effect-sandbox.mjs`
   - Copy isolated-vm implementation from knowledge-engine
   - Create feature flag for gradual rollout

---

## Appendix: Complete File Manifest

### Files to Create (26 new files)
1. `/src/security/threat-detector.mjs` (300 lines)
2. `/src/knowledge-engine/comunica-browser.mjs` (150 lines)
3. `/src/knowledge-engine/indexeddb-store.mjs` (250 lines)
4. `/src/knowledge-engine/service-worker.mjs` (200 lines)
5. `/src/browser.mjs` (50 lines)
6. `/src/browser-core.mjs` (30 lines)
7. `/build.browser.config.mjs` (100 lines)
8. `/test/security/vm-escape.test.mjs` (200 lines)
9. `/test/security/threat-detector.test.mjs` (250 lines)
10. `/test/security/isolated-vm-integration.test.mjs` (150 lines)
11. `/test/browser/core-features.spec.mjs` (200 lines)
12. `/test/browser/visual-regression.spec.mjs` (100 lines)
13. `/validation/security.validation.mjs` (200 lines)
14. `/validation/browser.validation.mjs` (200 lines)
15. `/src/validation/span-definitions-v3.1.mjs` (150 lines)
16. `/observability/grafana-dashboard-v3.1.json` (500 lines)
17. `/observability/alerts-v3.1.yml` (50 lines)
18. `/docs/observability/OTEL-SPANS-v3.1.md` (100 lines)
19. `/docs/observability/METRICS-v3.1.md` (80 lines)
20. `/docs/migration/v3.0-to-v3.1-security.md` (120 lines)
21. `/docs/guides/browser-usage.md` (150 lines)
22. `/docs/guides/otel-validation.md` (100 lines)
23. `/docs/security/THREAT-DETECTION.md` (50 lines)
24. `/docs/releases/latest-RELEASE-NOTES.md` (200 lines)
25. `/test/e2e/v3.1-features.test.mjs` (250 lines)
26. `/docs/latest-GOAP-IMPLEMENTATION-PLAN.md` (this file)

### Files to Edit (15 existing files)
1. `/src/knowledge-engine/effect-sandbox.mjs` (+100 lines, replace vm2)
2. `/src/security/sandbox-adapter.mjs` (+20 lines, replace vm2)
3. `/src/knowledge-engine/schemas.mjs` (+10 lines, update enums)
4. `/src/knowledge-engine/browser-shims.mjs` (+50 lines, complete polyfills)
5. `/package.json` (+5 lines deps, -1 line vm2, +exports)
6. `/validation/run-all.mjs` (+50 lines, add new suites)
7. `/validation/cli.validation.mjs` (-30 lines, remove legacy)
8. `/CHANGELOG.md` (+50 lines, latest entry)
9. `/README.md` (+20 lines, v3.1 highlights)
10. `/.github/workflows/ci.yml` (+30 lines, browser tests + isolated-vm build)
11. `/vitest.config.mjs` (+10 lines, coverage config)
12. `/build.config.mjs` (+20 lines, browser build)
13. `/docs/README.md` (+30 lines, v3.1 overview)
14. `/src/index.mjs` (+5 lines, export new APIs)
15. `/src/knowledge-engine/browser.mjs` (+30 lines, real implementations)

### Total LOC Impact
- **New code**: ~4,500 lines
- **Modified code**: ~400 lines
- **Documentation**: ~1,000 lines
- **Tests**: ~1,200 lines
- **Total**: ~7,100 lines added

---

## Conclusion

This GOAP plan provides a comprehensive roadmap for UNRDF latest implementation with:

✅ **4 major milestones** with clear deliverables
✅ **12 parallel work streams** to optimize timeline
✅ **47 specific tasks** with acceptance criteria
✅ **OTEL-validated success metrics** (no agent lies!)
✅ **Risk mitigation strategies** for high-impact issues
✅ **20-33 day timeline** (20 with optimal parallelization)

**Key Takeaway**: By following the critical path and leveraging parallel work streams, latest can be completed in approximately 3-4 weeks with a team of 3-5 developers.

**OTEL Validation is the Source of Truth**: All success metrics are measured via OTEL spans, ensuring objective validation of feature completeness and quality.

---

**Status**: READY FOR IMPLEMENTATION ✅
**Next Action**: Create feature branches and begin Milestone 1.1 (Core Sandbox Migration)
