# @unrdf/common Documentation Roadmap

**Package:** @unrdf/common (Type 1: Foundation-like - Internal Utilities)
**Effort:** 50-64 hours
**Files:** 14
**Words:** 12,000-16,000
**Phase:** 4 (Week 9)
**Audience:** Package developers (internal audience)
**Pattern:** Follow core pattern with internal focus

---

## Overview

`@unrdf/common` provides shared utilities used by all other UNRDF packages. It's internal-facing infrastructure, so documentation assumes familiarity with the codebase and focuses on usage patterns.

### Key Assumption
> Reader is an UNRDF package developer implementing features. These are shared utilities.

This means:
- Assume basic knowledge of package structure
- Focus on "how to use this in your package"
- Show integration patterns with existing code
- Document performance implications

---

## Tutorials (3 files, 10-14 hours total)

### Tutorial 1: Setting Up Logging

**File:** `docs/tutorials/01-setting-up-logging.md`
**Effort:** 4-5 hours
**Words:** 2,500-3,000

**Goal:** Package developers learn to use logging utility

**Structure:**

1. **Hook (Why logging matters, 400 words)**
   - Scenario: Debugging production issues
   - Problem: Console.log scattered everywhere
   - Better approach: Structured logging with levels
   - By end: Proper logging setup

2. **Creating Your Logger (800 words)**
   - Import createLogger: `import { createLogger } from '@unrdf/common'`
   - Initialize per module: `const logger = createLogger('package-name')`
   - Logger interface (debug, info, warn, error methods)
   - Configuration (automatic from env vars)

3. **Using Logging Levels (900 words)**
   - **Debug:** Development information (not in prod)
   - **Info:** Important milestones (normal operation)
   - **Warn:** Potential issues (might need attention)
   - **Error:** Failures (needs immediate action)
   - Examples for each level
   - When to use each level

4. **Structured Logging (500 words)**
   - Log objects with context: `logger.info('query executed', { time, results })`
   - Consistent format for parsing
   - Performance implications (minimal)

5. **Verify and Test (400 words)**
   - Expected output (sample log output)
   - Configure log level in tests
   - Verify in development

**Code Examples:**

```javascript
// Example 1: Basic setup (copy-paste ready)
import { createLogger } from '@unrdf/common';

const logger = createLogger('unrdf:core');

export function queryStore(sparql) {
  logger.debug('executing query', { sparql });

  try {
    const results = store.query(sparql);
    logger.info('query successful', { count: results.length });
    return results;
  } catch (error) {
    logger.error('query failed', { error: error.message });
    throw error;
  }
}
```

```javascript
// Example 2: Development vs production logging
// Development: DEBUG level (verbose)
// Production: INFO level (only important events)

// In code, just use logger normally:
logger.debug('intermediate step');  // Shows in dev, hidden in prod
logger.info('milestone reached');   // Shows always
```

```javascript
// Example 3: With context
logger.info('store operation', {
  operation: 'add',
  quads: quads.length,
  duration: `${elapsed}ms`
});
```

**Learning Outcomes:**
- [ ] Can initialize logger in new module
- [ ] Understand logging levels and when to use
- [ ] Know how to include context in logs
- [ ] Can configure logging in tests

---

### Tutorial 2: Working with Namespaces

**File:** `docs/tutorials/02-working-with-namespaces.md`
**Effort:** 4-5 hours
**Words:** 2,500-3,000

**Goal:** Learn to manage URI prefixes with namespace utilities

**Structure:**

1. **Problem Statement (400 words)**
   - Scenario: Package works with many RDF vocabularies
   - Current approach: Hard-coded URIs everywhere
   - Better approach: Namespace management
   - Use case: Reusable, maintainable code

2. **Creating Namespaces (900 words)**
   - Import createNamespace: `import { createNamespace } from '@unrdf/common'`
   - Define prefix: `const ex = createNamespace('http://example.com/')`
   - Use in code: `ex('Person')` → full URI
   - Common namespaces (RDF, RDFS, OWL provided)

3. **Namespace Utilities (700 words)**
   - `resolveUri(uri, namespaces)` - Expand prefixed URIs
   - `abbreviateUri(uri, namespaces)` - Collapse to prefix
   - Namespace resolution order (local → global)

4. **Best Practices (400 words)**
   - Define namespaces per package
   - Export shared namespaces centrally
   - Use constants for frequent URIs
   - Document namespace purposes

**Code Examples:**

```javascript
// Example 1: Define and use namespaces
import { createNamespace } from '@unrdf/common';

const ex = createNamespace('http://example.com/');
const foaf = createNamespace('http://xmlns.com/foaf/0.1/');
const rdf = createNamespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#');

// Use in quads
const personQuad = {
  subject: ex('person1'),      // http://example.com/person1
  predicate: rdf('type'),       // http://www.w3.org/1999/02/22-rdf-syntax-ns#type
  object: foaf('Person'),       // http://xmlns.com/foaf/0.1/Person
  graph: ex('graph1')
};
```

```javascript
// Example 2: Export namespaces from your package
// lib/namespaces.mjs
export const schemas = {
  ex: createNamespace('http://example.com/'),
  foaf: createNamespace('http://xmlns.com/foaf/0.1/'),
  rdf: createNamespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#')
};

// Other modules import and use
import { schemas } from './namespaces.mjs';
const { ex, foaf } = schemas;
```

**Learning Outcomes:**
- [ ] Understand namespace concept
- [ ] Can create namespaces
- [ ] Can use namespaces in quad construction
- [ ] Know how to export namespaces centrally

---

### Tutorial 3: Merging Configurations Safely

**File:** `docs/tutorials/03-merging-configurations.md`
**Effort:** 2-4 hours
**Words:** 1,500-2,000

**Goal:** Learn to merge user config with defaults

**Structure:**

1. **Problem Statement (300 words)**
   - Scenario: Package has defaults, user provides overrides
   - Current approach: Manual merging (error-prone)
   - Better approach: Use deepMerge utility
   - Use case: Safe configuration inheritance

2. **Basic Merging (700 words)**
   - Import deepMerge: `import { deepMerge } from '@unrdf/common'`
   - Merge objects: `const config = deepMerge(defaults, userConfig)`
   - Nested object handling
   - Array behavior (replace vs merge)

3. **Validation (500 words)**
   - Merge with schema: `deepMerge(defaults, userConfig, schema)`
   - Type safety: Verify merged result
   - Error handling: Invalid config rejection

**Code Examples:**

```javascript
// Example 1: Safe config merge
import { deepMerge } from '@unrdf/common';

const defaultConfig = {
  store: { format: 'turtle' },
  logging: { level: 'info' },
  timeout: 30000
};

const userConfig = {
  logging: { level: 'debug' }
};

const final = deepMerge(defaultConfig, userConfig);
// Result: { store: {...}, logging: { level: 'debug' }, timeout: 30000 }
```

**Learning Outcomes:**
- [ ] Understand deepMerge utility
- [ ] Can safely merge configurations
- [ ] Know how to handle nested objects
- [ ] Can validate merged results

---

## How-To Guides (4 files, 12-16 hours total)

### How-To 1: Debug with Logging

**File:** `docs/how-to/01-debug-with-logging.md`
**Effort:** 3-4 hours
**Words:** 1,500-2,000

**Problem:** Feature is broken, need to understand execution flow

**Solutions:**

1. **Enable Debug Logging (400 words)**
   - Set environment: `DEBUG=unrdf:* node app.js`
   - View detailed execution flow
   - Trace through complex operations
   - Example: Query execution trace

2. **Add Strategic Logging (400 words)**
   - Log at key points: Entry, exit, errors
   - Include context: Input values, state
   - Use appropriate level (debug vs info)
   - Example: Adding logs to existing code

3. **Correlate Logs (300 words)**
   - Use correlation ID for tracking
   - Link related log entries
   - Follow execution path
   - Example: Request ID propagation

4. **Parse Log Output (300 words)**
   - Understand log format
   - Filter by level or component
   - Use tools (grep, jq) for analysis
   - Example: Find all errors in log file

---

### How-To 2: Work with Many Ontologies

**File:** `docs/how-to/02-work-with-many-ontologies.md`
**Effort:** 3-4 hours
**Words:** 1,500-2,000

**Problem:** Application uses 10+ RDF vocabularies

**Solutions:**

1. **Organize Namespaces (400 words)**
   - Create central namespace module
   - Group related vocabularies
   - Export for reuse
   - Example: ontologies.mjs

2. **Create Namespace Shortcuts (400 words)**
   - Common vocabularies pre-imported
   - Avoid repetition in code
   - Maintain consistency
   - Example: Import { ex, foaf, rdf } from '@unrdf/common/ontologies'

3. **Document Namespace Usage (300 words)**
   - Create namespace reference
   - Explain each vocabulary
   - Link to ontology documentation
   - Example: Generated namespace guide

4. **Versioning Vocabularies (300 words)**
   - Track ontology versions
   - Handle vocabulary evolution
   - Migration strategies
   - Example: Version in namespace URI

---

### How-To 3: Handle URI Edge Cases

**File:** `docs/how-to/03-handle-uri-edge-cases.md`
**Effort:** 3-4 hours
**Words:** 1,500-2,000

**Problem:** URIs with special characters, encoding issues

**Solutions:**

1. **Proper URI Encoding (400 words)**
   - Use resolveUri for automatic encoding
   - Understand percent encoding rules
   - Common special characters
   - Example: URI with spaces

2. **Fragment Handling (300 words)**
   - Hash fragments in URIs
   - resolveUri with fragments
   - Performance implications
   - Example: Named graphs with fragments

3. **Relative vs Absolute (300 words)**
   - Resolve relative URIs
   - Base URI configuration
   - Relative reference patterns
   - Example: Ontology imports

4. **Unicode Support (300 words)**
   - Non-ASCII characters
   - IRI vs URI distinction
   - Encoding/decoding strategies
   - Example: International names

---

### How-To 4: Optimize Utility Usage

**File:** `docs/how-to/04-optimize-utility-usage.md`
**Effort:** 3-4 hours
**Words:** 1,500-2,000

**Problem:** Too many logging calls, performance regression

**Solutions:**

1. **Lazy Logging (400 words)**
   - Only create log messages when needed
   - Check level before expensive operations
   - `if (logger.isDebugEnabled()) { ... }`
   - Example: Complex object stringification

2. **Cache Resolved URIs (400 words)**
   - Namespaces are fast, but cache for hot paths
   - Pre-resolve common URIs
   - Measure impact first
   - Example: Caching strategies

3. **Batch Configuration Merges (300 words)**
   - deepMerge once at startup
   - Not in tight loops
   - Measure if concerned
   - Example: Configuration initialization

4. **Profile and Measure (300 words)**
   - Use Node.js profiler
   - Identify actual bottlenecks
   - Don't optimize prematurely
   - Example: Performance analysis

---

## Reference (5 files, 12-16 hours total)

### Reference 1: API Documentation

**File:** `docs/reference/01-api.md`
**Effort:** 4-5 hours
**Words:** 3,000-3,500

Functions documented:

1. **createLogger(name)**
   - Signature, parameters, returns
   - Methods: debug(), info(), warn(), error()
   - Options table (level, format, output)
   - 3 examples (basic, with context, configuration)

2. **createNamespace(baseUri)**
   - Signature, parameters, returns
   - Usage: namespace(localName) → full URI
   - Options (validation, caching)
   - 3 examples (basic, common vocabularies, export pattern)

3. **deepMerge(target, source, options?)**
   - Signature, parameters, returns
   - Options table (array merge strategy, validation)
   - Behavior with nested objects
   - 3 examples (simple, nested, with validation)

4. **resolveUri(uri, namespaces)**
   - Signature, parameters, returns
   - Handles prefixed URIs
   - Options (base URI, strict mode)
   - 3 examples (basic, with base, error cases)

5. **abbreviateUri(uri, namespaces)**
   - Signature, parameters, returns
   - Opposite of resolveUri
   - Options (prefer short, unknown handling)
   - 3 examples

---

### Reference 2: Types Documentation

**File:** `docs/reference/02-types.md`
**Effort:** 2-3 hours
**Words:** 1,500-2,000

**Interfaces:**

```javascript
// Logger interface
interface Logger {
  debug(message: string, context?: object): void;
  info(message: string, context?: object): void;
  warn(message: string, context?: object): void;
  error(message: string, context?: object): void;
  isDebugEnabled(): boolean;
}

// Namespace function type
interface Namespace {
  (localName: string): NamedNode;
}

// Merge options
interface MergeOptions {
  arrayMergeStrategy?: 'replace' | 'concat';
  schema?: object;
}
```

---

### Reference 3: Configuration

**File:** `docs/reference/03-configuration.md`
**Effort:** 2-3 hours
**Words:** 1,000-1,500

Configuration table format:

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| DEBUG | string | '' | Log filter pattern |
| LOG_LEVEL | 'debug' \| 'info' \| 'warn' \| 'error' | 'info' | Minimum log level |
| LOG_FORMAT | 'json' \| 'text' | 'text' | Output format |

---

### Reference 4: Error Reference

**File:** `docs/reference/04-errors.md`
**Effort:** 2-3 hours
**Words:** 1,000-1,500

Error reference table:

| Code | Message | Cause | Solution |
|------|---------|-------|----------|
| INVALID_URI | Invalid URI format | Malformed URI | Check URI syntax |
| MERGE_CONFLICT | Merge conflict | Incompatible types | Check merge config |

---

### Reference 5: Internal Patterns

**File:** `docs/reference/05-internal-patterns.md`
**Effort:** 2-3 hours
**Words:** 1,000-1,500

Usage patterns from actual packages:

- **Query Package:** How @unrdf/core uses logging
- **Storage Package:** How persistence uses deepMerge
- **Federation Package:** Namespace organization patterns
- **CLI Package:** Logging for user-facing tools

---

## Explanation (4 files, 10-14 hours total)

### Explanation 1: Common Package Architecture

**File:** `docs/explanation/01-common-architecture.md`
**Effort:** 3-4 hours
**Words:** 2,000-2,500

**Big Picture:** How common utilities fit into UNRDF architecture

**Components:**
1. Logging subsystem (debug support)
2. URI/namespace management (semantic data handling)
3. Configuration utilities (flexibility)
4. Internal helpers (code reuse)

**Why These?** Solve problems across all packages

---

### Explanation 2: Logging Strategy

**File:** `docs/explanation/02-logging-strategy.md`
**Effort:** 2-3 hours
**Words:** 1,500-2,000

**Purpose:** Troubleshooting and monitoring

**Levels:**
- Debug: Development visibility
- Info: Important milestones
- Warn: Potential issues
- Error: Failures

**Performance:** Structured logging overhead < 1% for most operations

---

### Explanation 3: URI Handling

**File:** `docs/explanation/03-uri-handling.md`
**Effort:** 3-4 hours
**Words:** 2,000-2,500

**Concepts:** RDF URIs vs Web URIs, prefixing, resolution

**Why This Matters:** RDF semantics depend on correct URI handling

---

### Explanation 4: Configuration Philosophy

**File:** `docs/explanation/04-configuration-philosophy.md`
**Effort:** 2-3 hours
**Words:** 1,500-2,000

**Design:** Sensible defaults + user override + validation

**Why Important:** Configuration mistakes = silent failures

---

## Summary

**Total Effort:** 50-64 hours
**Total Files:** 14
**Total Words:** 12,000-16,000

**Content Breakdown:**
- Tutorials: 3 files, 10-14 hours (learn by doing)
- How-To: 4 files, 12-16 hours (solve problems)
- Reference: 5 files, 12-16 hours (look it up)
- Explanation: 4 files, 10-14 hours (understand why)

**Quality Gates:**
- ✅ All code examples tested
- ✅ No TODO/FIXME placeholders
- ✅ Peer reviewed
- ✅ 100% validation score

**Internal Audience Notes:**
- Documentation targets package developers
- Focus on integration patterns
- Show real usage from existing packages
- Document performance implications

---

## Next Phase

Phase 5-6 (Weeks 11-12) covers final integration and publication.

This package is internal infrastructure; not directly used by external users but critical for all public packages.
