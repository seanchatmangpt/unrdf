# ⚠️ PACKAGE DEPRECATED

**Package:** `@unrdf/knowledge-engine`
**Status:** DEPRECATED as of December 6, 2024
**Reason:** Workspace import issues + 47% of codebase for 5% of value

---

## Why Deprecated?

1. **Technical Issues:** Source code contains workspace imports that prevent standalone use:

```
Error: Cannot find package '@unrdf/oxigraph' imported from
/home/user/unrdf/packages/knowledge-engine/src/transaction.mjs
```

2. **80/20 Analysis:** This package represents 47% of the total codebase (23,279 LoC) but delivers only ~5% of user value

3. **Maintenance Burden:** Large codebase with optional features creates unnecessary maintenance overhead

---

## Migration Path

**For reasoning capabilities:**

The knowledge-engine package has been **extracted to a separate repository** for independent development and maintenance.

**New repository:** [Link pending - separate knowledge-engine repo]

**For RDF operations:**

Use `@unrdf/core` which provides all foundational RDF capabilities:

```javascript
import { createStore, executeQuerySync } from '@unrdf/core';

const store = createStore();
// All RDF operations available
```

**For inference/reasoning:**

Consider these alternatives:
- [rdflib.js](https://github.com/linkeddata/rdflib.js) - Full RDF library with reasoning
- [Comunica](https://comunica.dev/) - Federated query engine
- [OWL-RL](https://github.com/RDFLib/OWL-RL) - OWL 2 RL reasoner

---

## Extracted Features

The following features have been moved to the separate knowledge-engine repository:

- Transaction support
- Advanced reasoning capabilities
- Inference engines
- Rule-based processing
- Ontology management

---

## Why Extract Instead of Fix?

**Data-driven decision:**

| Metric | Value | Implication |
|--------|-------|-------------|
| Lines of Code | 23,279 (47% of total) | High maintenance cost |
| User Value | ~5% | Low ROI |
| Complexity | High | Requires specialized maintenance |
| Use Cases | Optional/Advanced | Not core functionality |

**Conclusion:** Better served as specialized standalone package for users who need advanced reasoning.

---

## Recommended Packages

Use these production-ready packages for core RDF work:

- **`@unrdf/core`** - RDF store, SPARQL queries, canonicalization (100% tests passing)
- **`@unrdf/kgc-4d`** - Temporal event sourcing with nanosecond precision (100% tests passing)

---

## For Advanced Users

If you need the knowledge-engine features:

1. Install from the new repository: [Link pending]
2. It now has independent versioning and release cycle
3. Optimized for reasoning-focused use cases
4. Can evolve without impacting core UNRDF packages

---

**Last Updated:** December 6, 2024
**Consolidation Analysis:** See `/permutation-tests/EXECUTIVE-SUMMARY.md`
**Extract Reason:** 80/20 optimization (47% code, 5% value)
