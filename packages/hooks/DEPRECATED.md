# ⚠️ PACKAGE DEPRECATED

**Package:** `@unrdf/hooks`
**Status:** DEPRECATED as of December 6, 2024
**Reason:** Critical Zod validation error preventing import

---

## Why Deprecated?

Empirical testing revealed a critical bug that prevents this package from being imported:

```
Error: Cannot read properties of undefined (reading '_zod')
at defineHook (file:///home/user/unrdf/packages/hooks/src/hooks/define-hook.mjs:135:38)
at file:///home/user/unrdf/packages/hooks/src/hooks/builtin-hooks.mjs:22:35
```

**Impact:**
- Cannot import the package at all
- Blocks 62.5% of integration tests (5 out of 8 tests)
- Has been broken since beta.1 release (5+ months)

---

## Migration Path

**For policy/validation hooks:**

Use custom validation in your application code with Zod directly:

```javascript
import { z } from 'zod';
import { createStore, dataFactory } from '@unrdf/core';

// Define schema
const personSchema = z.object({
  type: z.literal('http://xmlns.com/foaf/0.1/Person'),
  name: z.string()
});

// Validate before adding to store
function addPerson(store, data) {
  const validated = personSchema.parse(data);
  const quad = dataFactory.quad(/*...*/);
  store.add(quad);
}
```

**For event hooks:**

Use `@unrdf/kgc-4d` event logging instead:

```javascript
import { KGCStore, EVENT_TYPES } from '@unrdf/kgc-4d';

const store = new KGCStore();
const receipt = await store.appendEvent(
  { type: EVENT_TYPES.CREATE, payload: { /* your data */ } },
  [/* deltas */]
);
```

---

## Issue Tracking

Issue created to fix Zod validation error: [Link pending]

This package may be restored in a future release if the Zod error is resolved.

---

## Recommended Packages

Use these production-ready packages instead:

- **`@unrdf/core`** - RDF store, SPARQL queries, canonicalization
- **`@unrdf/kgc-4d`** - Temporal event sourcing with nanosecond precision

Both packages have 100% passing tests and are production-ready.

---

**Last Updated:** December 6, 2024
**Consolidation Analysis:** See `/permutation-tests/EXECUTIVE-SUMMARY.md`
