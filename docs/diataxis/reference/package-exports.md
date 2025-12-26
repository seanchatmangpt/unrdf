# Package Exports Reference

**Purpose:** Complete reference of all UNRDF packages and their exported modules.

**Audience:** All developers

**Version:** 5.0.1

---

## Overview

UNRDF is organized as a monorepo with 42+ packages. This reference lists all packages and their public exports.

---

## Core Packages

### `@unrdf/core`

**[Placeholder - Content to be filled]**

**Description:** Core RDF operations - parse, query, validate, reason

**Exports:**
```javascript
// Store operations
import { createStore, addTriple, removeTriple } from '@unrdf/core';

// Parsing
import { parseTurtle, parseJsonLd, toTurtle, toJsonLd } from '@unrdf/core';

// Querying
import { query, select, ask, construct, describe } from '@unrdf/core';

// Validation
import { validateShacl, formatValidationReport } from '@unrdf/core';

// Reasoning
import { reason, extractInferred } from '@unrdf/core';
```

**Evidence:** Package at `/home/user/unrdf/packages/core/package.json`
**Source:** `/home/user/unrdf/packages/core/src/index.mjs`

---

### `@unrdf/oxigraph`

**[Placeholder - Oxigraph exports]**

**Evidence:** Package at `/home/user/unrdf/packages/oxigraph/`

---

### `@unrdf/hooks`

**[Placeholder - Hooks exports]**

**Evidence:** Package at `/home/user/unrdf/packages/hooks/`

---

### `@unrdf/kgc-4d`

**[Placeholder - KGC-4D exports]**

**Evidence:** Package at `/home/user/unrdf/packages/kgc-4d/`

---

### `@unrdf/yawl`

**[Placeholder - YAWL exports]**

**Evidence:** Package at `/home/user/unrdf/packages/yawl/`

---

## Extended Packages

### `@unrdf/streaming`

**[Placeholder - Streaming exports]**

**Evidence:** Package at `/home/user/unrdf/packages/streaming/`

---

### `@unrdf/federation`

**[Placeholder - Federation exports]**

**Evidence:** Package at `/home/user/unrdf/packages/federation/`

---

### `@unrdf/validation`

**[Placeholder - Validation exports]**

**Evidence:** Package at `/home/user/unrdf/packages/validation/`

---

### `@unrdf/observability`

**[Placeholder - Observability exports]**

**Evidence:** Package at `/home/user/unrdf/packages/observability/`

---

## Utility Packages

### `@unrdf/test-utils`

**[Placeholder - Test utilities exports]**

**Evidence:** Package at `/home/user/unrdf/packages/test-utils/`

---

### `@unrdf/cli`

**[Placeholder - CLI exports]**

**Evidence:** Package at `/home/user/unrdf/packages/cli/`

---

## Specialized Packages

### `@unrdf/blockchain`

**[Placeholder - Blockchain exports]**

**Evidence:** Package at `/home/user/unrdf/packages/blockchain/`

---

### `@unrdf/ml-inference`

**[Placeholder - ML inference exports]**

**Evidence:** Package at `/home/user/unrdf/packages/ml-inference/`

---

### `@unrdf/semantic-search`

**[Placeholder - Semantic search exports]**

**Evidence:** Package at `/home/user/unrdf/packages/semantic-search/`

---

## YAWL Extension Packages

### `@unrdf/yawl-ai`

**[Placeholder - YAWL AI exports]**

**Evidence:** Package at `/home/user/unrdf/packages/yawl-ai/`

---

### `@unrdf/yawl-kafka`

**[Placeholder - YAWL Kafka exports]**

**Evidence:** Package at `/home/user/unrdf/packages/yawl-kafka/`

---

### `@unrdf/yawl-observability`

**[Placeholder - YAWL observability exports]**

**Evidence:** Package at `/home/user/unrdf/packages/yawl-observability/`

---

## Complete Package List

**[Placeholder - Full package matrix]**

| Package | Description | Status | Version |
|---------|-------------|--------|---------|
| @unrdf/core | Core RDF operations | ✅ Stable | 5.0.1 |
| @unrdf/oxigraph | Rust triple store | ✅ Stable | 5.0.1 |
| @unrdf/hooks | Reactive hooks | ✅ Stable | 5.0.1 |
| ... | ... | ... | ... |

**Evidence:** Package list at `/home/user/unrdf/package.json:workspaces`

---

## Import Patterns

**[Placeholder - Recommended import patterns]**

```javascript
// ESM imports (recommended)
import { createStore } from '@unrdf/core';

// Category imports
import * as Hooks from '@unrdf/hooks';

// Subpath imports
import { FreezeUniverse } from '@unrdf/kgc-4d/freeze';
```

---

## Version Compatibility

**[Placeholder - Version matrix]**

---

## Related References

- **[CLI Commands Reference](./cli-commands.md)** - CLI package details
- **[Hook API Reference](./hook-api.md)** - Hooks package details
- **[RDF Format Notes](./rdf-format-notes.md)** - Core package formats

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
