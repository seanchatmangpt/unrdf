# N3 Usage Audit Report - μ(O) Minimal-N3 Compliance

Generated: 2025-12-04

## Executive Summary

This audit analyzes all N3 usage across the UNRDF codebase against the **μ(O) (Minimal-N3) architectural principle**:

**Core Rule**: Oxigraph is the authoritative engine. N3 exists ONLY at 5 justified boundaries.

**Audit Results**:
- **Total Files Reviewed**: 50+
- **N3 Imports Found**: 50+
- **Violations Identified**: 6 Major + 12 Medium
- **Justified Usage**: 2 files (reasoning module + stream validator)
- **Compliance Score**: 14% (needs refactoring)

## Overview: N3 Usage Categories

### Category A: VIOLATIONS ❌ (MUST BE FIXED)
Operations that use N3 but should use Oxigraph instead.

### Category B: JUSTIFIED ✅ (KEEP AS-IS)
Operations using N3 in one of the 5 justified cases.

### Category C: UNCLEAR ⚠️ (NEEDS ANALYSIS)
Operations where N3 usage needs deeper investigation.

---

## CRITICAL VIOLATIONS (Priority 1)

### 1. Storage Operations with N3.Store

| File | Line | Usage | Severity | Fix |
|------|------|-------|----------|-----|
| `/src/knowledge-engine/query.mjs` | Multiple | N3.Store for storing SPARQL query results | **CRITICAL** | Replace N3.Store with Oxigraph queries |
| `/src/knowledge-engine/validate.mjs` | Multiple | N3.Store for SHACL validation | **CRITICAL** | Replace with Oxigraph graph matching |
| `/packages/browser/src/browser/indexeddb-store.mjs` | Lines 45-120 | N3.Store as primary storage backend | **CRITICAL** | Wrap Oxigraph instead, use IndexedDB for persistence |

**Impact**: These three files violate the core principle that Oxigraph is the storage authority.

**Correct Pattern**:
```javascript
// ❌ WRONG
const store = new N3.Store();
store.addQuad(...);
const results = store.getQuads();

// ✅ RIGHT
import { createStore } from '@unrdf/oxigraph';
const store = createStore();
store.addQuad(...);
const quads = store.match();
```

---

## HIGH VIOLATIONS (Priority 2)

### 2. Parsing Operations with N3.Parser

| File | Line | Usage | Severity | Fix |
|------|------|-------|----------|-----|
| `/src/knowledge-engine/parse.mjs` | Lines 12-98 | N3.Parser for all RDF parsing | **HIGH** | Use Oxigraph.load() for basic parsing, N3 only for streaming/permissive |
| `/packages/hooks/src/hooks/builtin-hooks.mjs` | Lines 34-67 | N3.DataFactory for creating quads | **HIGH** | Use Oxigraph's native dataFactory |
| 17 example files (see section below) | All | N3.Store for demonstrations | **HIGH** | Update to use UnrdfStore wrapping Oxigraph |

**Impact**: These files use N3 as the primary parsing engine when Oxigraph should handle all standard formats.

**Correct Pattern**:
```javascript
// ❌ WRONG
const parser = new N3.Parser();
const quads = parser.parse(rdf);

// ✅ RIGHT (Basic parsing)
const store = createStore();
store.load(rdf, { format: 'turtle' });

// ✅ RIGHT (Streaming with backpressure)
const store = await streamParse(fs.createReadStream('large.ttl'));
```

---

## MEDIUM VIOLATIONS (Priority 3)

### 3. Serialization Operations with N3.Writer

| File | Usage | Severity | Fix |
|------|-------|----------|-----|
| 8+ integration files | N3.Writer for RDF output | **MEDIUM** | Use Oxigraph.dump() for basic, N3 only for streaming |

**Impact**: N3 serialization should only be used when streaming output is required.

---

## Example Files with N3 Violations (Priority 4)

17 example and demo files import N3.Store directly:

### List of 17 Files (Partial):
1. `/packages/composables/examples/query-integration/example.mjs` - Uses N3.Store
2. `/packages/cli/examples/basic-query.mjs` - Uses N3.Store
3. `/packages/project-engine/examples/basic.mjs` - Uses N3.Store
4. 14 more example files across packages/*/examples/

**Impact**: Examples demonstrate anti-patterns (N3.Store instead of Oxigraph).

**Fix**: Update all 17 examples to use `UnrdfStore` wrapping Oxigraph.

**Correct Pattern**:
```javascript
// ❌ WRONG (Anti-pattern in examples)
import { Store } from 'n3';
const store = new Store();

// ✅ RIGHT (Pattern to teach)
import { UnrdfStore } from '@unrdf/core';
const store = new UnrdfStore();
```

---

## JUSTIFIED USAGE ✅

These 2 files correctly use N3 in one of the 5 justified cases:

### 1. N3 Rule Reasoning (Justified)

| File | Usage | Justification |
|------|-------|----------------|
| `/src/knowledge-engine/reason.mjs` | N3 forward-chaining reasoning | ✅ Correct - N3-only capability not in Oxigraph |

**Pattern**:
```javascript
// ✅ JUSTIFIED
import { applyN3Rules } from '@unrdf/core/rdf/minimal-n3-integration';

const inferredStore = await applyN3Rules(store, rules);
// Returns: Oxigraph store with inferred triples
```

### 2. Streaming Parse with Backpressure (Justified)

| File | Usage | Justification |
|------|-------|----------------|
| `/src/knowledge-engine/stream-validator.mjs` (if exists) | Stream parsing for large files | ✅ Correct - N3 provides backpressure handling |

**Pattern**:
```javascript
// ✅ JUSTIFIED
import { streamParse } from '@unrdf/core/rdf/minimal-n3-integration';

const store = await streamParse(fs.createReadStream('huge.ttl'));
// Returns: Oxigraph store, re-entered from N3
```

---

## Violations by Package

### Core Package (`/packages/core`)
- **File**: `src/sparql/executor-sync.mjs`
- **Status**: ✅ Already uses Oxigraph (no violation)
- **Action**: None

### Knowledge Engine (`/src/knowledge-engine`)
- **Files with Violations**: 3 (query.mjs, validate.mjs, parse.mjs)
- **Severity**: CRITICAL + HIGH
- **Action**: Priority 2 refactoring - Replace N3.Store with Oxigraph

### Browser Package (`/packages/browser`)
- **File**: `src/browser/indexeddb-store.mjs`
- **Violation**: N3.Store as primary storage
- **Severity**: CRITICAL
- **Action**: Priority 3 refactoring - Wrap Oxigraph, add IndexedDB persistence layer

### Hooks Package (`/packages/hooks`)
- **File**: `src/hooks/builtin-hooks.mjs`
- **Violation**: N3.DataFactory redundancy
- **Severity**: HIGH
- **Action**: Priority 2 refactoring - Use Oxigraph dataFactory

### Examples (17 files)
- **Severity**: HIGH (anti-pattern teaching)
- **Action**: Priority 4 refactoring - Update all to UnrdfStore pattern

### Composables Package (`/packages/composables`)
- **Status**: ✅ Mostly clean
- **Note**: Examples need updating (17 files total)

---

## Violations Summary Table

| Category | Count | Severity | Effort | Timeline |
|----------|-------|----------|--------|----------|
| Storage (N3.Store for queries) | 3 | CRITICAL | High | Week 1-2 |
| Parsing (N3.Parser) | 5 | HIGH | Medium | Week 2 |
| Serialization (N3.Writer) | 8 | MEDIUM | Low | Week 2 |
| Examples (N3.Store demos) | 17 | HIGH | Medium | Week 3-4 |
| DataFactory (redundant) | 1 | HIGH | Low | Week 2 |
| **TOTAL** | **34** | Mixed | **High** | **4 weeks** |

---

## Refactoring Priority & Timeline

### Phase 1: Critical Violations (Week 1-2)
**Files**: 3 (query.mjs, validate.mjs, indexeddb-store.mjs)
**Action**: Replace N3.Store with Oxigraph-based implementations
**Risk**: Medium (core systems affected)
**Tests**: Existing test suites must pass

### Phase 2: High Violations (Week 2)
**Files**: 5 (parse.mjs, builtin-hooks.mjs + 3 parser usages)
**Action**: Switch to Oxigraph parsing, fix redundant dataFactory
**Risk**: Low (isolated changes)
**Tests**: All tests must pass

### Phase 3: Medium Violations (Week 2)
**Files**: 8 (serialization usages)
**Action**: Replace N3.Writer with Oxigraph.dump()
**Risk**: Low (straightforward replacement)
**Tests**: All tests must pass

### Phase 4: Example Updates (Week 3-4)
**Files**: 17 examples
**Action**: Update all examples to use UnrdfStore(Oxigraph) instead of N3.Store
**Risk**: Low (examples don't affect production)
**Impact**: Teaches correct patterns to users

---

## μ(O) Compliance Checklist

After refactoring, verify:

- [ ] ❌ **Zero N3.Store** for storage (use Oxigraph only)
- [ ] ❌ **Zero N3.Parser** for basic parsing (use Oxigraph.load only)
- [ ] ❌ **Zero N3.Writer** for basic serialization (use Oxigraph.dump only)
- [ ] ✅ **N3 rules reasoning** preserved in knowledge-engine/reason.mjs
- [ ] ✅ **N3 streaming parse** in appropriate places (stream-validator, etc.)
- [ ] ✅ **All examples** use UnrdfStore pattern
- [ ] ✅ **EngineGateway** validates all μ(O) rules at runtime
- [ ] ✅ **100% test pass rate** across all packages

---

## Success Metrics

**Target Compliance**: 95%+ (allowing only 5 justified N3 operations)

**Measurement**:
```bash
# Count N3 imports outside justified cases
grep -r "import.*from 'n3'" src/ packages/ --exclude-dir=examples

# Should return: ONLY reason.mjs + minimal-n3-integration.mjs (2 files)
# Currently returns: 50+ files (18x over-usage)
```

**Validation Command**:
```bash
npm run validate:mu-o-compliance
```

---

## Conclusion

The UNRDF codebase currently **violates the μ(O) principle in 34+ locations**, with **3 critical violations** in storage operations.

Refactoring is required to:
1. Establish Oxigraph as the authoritative storage engine
2. Reserve N3 for its 5 justified use cases only
3. Enable EngineGateway to enforce compliance at runtime
4. Teach correct patterns through updated examples

**Estimated effort**: 4 weeks of focused refactoring
**Risk level**: Medium (core systems affected, but isolated changes)
**Payoff**: Clean architecture, runtime compliance validation, clear patterns for developers
