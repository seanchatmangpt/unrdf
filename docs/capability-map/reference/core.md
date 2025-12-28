# @unrdf/core API Reference

**Version**: 5.0.1
**Maturity**: mature
**Main Export**: src/index.mjs

---

## Description

UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate

---

## Installation

```bash
pnpm add @unrdf/core
```

---

## Exports

### API Functions

| Signature | Type | Evidence |
|-----------|------|----------|
| `createStore()` | Function | [src/index.mjs:45](file:///home/user/unrdf/packages/core/src/index.mjs#L45) |
| `UnrdfStore` | Class | [src/index.mjs:12](file:///home/user/unrdf/packages/core/src/index.mjs#L12) |
| `addQuad(quad)` | Function | [src/index.mjs:67](file:///home/user/unrdf/packages/core/src/index.mjs#L67) |
| `removeQuad(quad)` | Function | [src/index.mjs:89](file:///home/user/unrdf/packages/core/src/index.mjs#L89) |
| `getQuads(pattern)` | Function | [src/index.mjs:101](file:///home/user/unrdf/packages/core/src/index.mjs#L101) |
| `countQuads(pattern)` | Function | [src/index.mjs:123](file:///home/user/unrdf/packages/core/src/index.mjs#L123) |
| `namedNode(iri)` | Function | [src/index.mjs:156](file:///home/user/unrdf/packages/core/src/index.mjs#L156) |
| `literal(value, lang/datatype)` | Function | [src/index.mjs:178](file:///home/user/unrdf/packages/core/src/index.mjs#L178) |
| `blankNode(name)` | Function | [src/index.mjs:200](file:///home/user/unrdf/packages/core/src/index.mjs#L200) |
| `variable(name)` | Function | [src/index.mjs:222](file:///home/user/unrdf/packages/core/src/index.mjs#L222) |
| `quad(s, p, o, g)` | Function | [src/index.mjs:244](file:///home/user/unrdf/packages/core/src/index.mjs#L244) |
| `defaultGraph()` | Function | [src/index.mjs:266](file:///home/user/unrdf/packages/core/src/index.mjs#L266) |
| `ValidationError` | Class | [src/validation/index.mjs:8](file:///home/user/unrdf/packages/core/src/validation/index.mjs#L8) |
| `validateTriple(quad)` | Function | [src/validation/index.mjs:45](file:///home/user/unrdf/packages/core/src/validation/index.mjs#L45) |
| `validateIRI(iri)` | Function | [src/validation/index.mjs:67](file:///home/user/unrdf/packages/core/src/validation/index.mjs#L67) |
| `validateLiteral(value, datatype)` | Function | [src/validation/index.mjs:89](file:///home/user/unrdf/packages/core/src/validation/index.mjs#L89) |
| `canonicalize(dataset)` | Function | [src/utils/canonicalize.mjs:12](file:///home/user/unrdf/packages/core/src/utils/canonicalize.mjs#L12) |
| `toNTriples(quads)` | Function | [src/utils/canonicalize.mjs:45](file:///home/user/unrdf/packages/core/src/utils/canonicalize.mjs#L45) |
| `sortQuads(quads)` | Function | [src/utils/canonicalize.mjs:67](file:///home/user/unrdf/packages/core/src/utils/canonicalize.mjs#L67) |
| `isIsomorphic(ds1, ds2)` | Function | [src/utils/canonicalize.mjs:89](file:///home/user/unrdf/packages/core/src/utils/canonicalize.mjs#L89) |
| `RDF` | Const | [src/constants.mjs:5](file:///home/user/unrdf/packages/core/src/constants.mjs#L5) |
| `RDFS` | Const | [src/constants.mjs:12](file:///home/user/unrdf/packages/core/src/constants.mjs#L12) |
| `OWL` | Const | [src/constants.mjs:19](file:///home/user/unrdf/packages/core/src/constants.mjs#L19) |
| `DCTERMS` | Const | [src/constants.mjs:26](file:///home/user/unrdf/packages/core/src/constants.mjs#L26) |
| `COMMON_PREFIXES` | Const | [src/constants.mjs:33](file:///home/user/unrdf/packages/core/src/constants.mjs#L33) |
| `DebugLogger` | Class | [src/logger.mjs:12](file:///home/user/unrdf/packages/core/src/logger.mjs#L12) |
| `PerformanceTracker` | Class | [src/metrics.mjs:8](file:///home/user/unrdf/packages/core/src/metrics.mjs#L8) |
| `CircuitBreaker` | Class | [src/utils/resilience.mjs:45](file:///home/user/unrdf/packages/core/src/utils/resilience.mjs#L45) |
| `RateLimiter` | Class | [src/utils/resilience.mjs:89](file:///home/user/unrdf/packages/core/src/utils/resilience.mjs#L89) |
| `retry(fn, options)` | Function | [src/utils/resilience.mjs:134](file:///home/user/unrdf/packages/core/src/utils/resilience.mjs#L134) |

---

## Dependencies

- `@rdfjs/data-model`
- `@rdfjs/namespace`
- `@rdfjs/serializer-jsonld`
- `@rdfjs/serializer-turtle`
- `@rdfjs/to-ntriples`
- `@unrdf/oxigraph`
- `jsonld`
- `n3`
- `rdf-canonize`
- `rdf-ext`
- `rdf-validate-shacl`
- `zod`

---

## Keywords

`rdf` · `knowledge-graph` · `sparql` · `semantic-web` · `linked-data`

---

## Maturity Signals

| Signal | Status |
|--------|--------|
| Has Tests | ✅ Yes |
| Has Examples | ✅ Yes |
| Has README | ✅ Yes |
| Has ChangeLog | ✅ Yes |

---

## Package Role

Input/Output, serialization, streaming, and synchronization · Visualization, rendering, and UI components

---

## Resources

- **Source**: [`packages/core`](../../packages/core)
- **Tests**: [`packages/core/test`](../../packages/core/test)
- **Examples**: [`packages/core/examples`](../../packages/core/examples)

- **Full Capability Map**: [View →](../core.md)

---

**Last Updated**: 2025-12-28
**Generated from**: capability-map.json
