# @unrdf/oxigraph API Reference

**Version**: 5.0.1
**Maturity**: mature
**Main Export**: src/index.mjs

---

## Description

UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine

---

## Installation

```bash
pnpm add @unrdf/oxigraph
```

---

## Exports

### API Functions

| Signature | Type | Evidence |
|-----------|------|----------|
| `createStore()` | Function | [src/store.mjs:8](file:///home/user/unrdf/packages/oxigraph/src/store.mjs#L8) |
| `OxigraphStore` | Class | [src/store.mjs:22](file:///home/user/unrdf/packages/oxigraph/src/store.mjs#L22) |
| `store.load(content, format)` | Method | [src/store.mjs:156](file:///home/user/unrdf/packages/oxigraph/src/store.mjs#L156) |
| `store.dump(format, graph)` | Method | [src/store.mjs:177](file:///home/user/unrdf/packages/oxigraph/src/store.mjs#L177) |
| `store.insert(quad)` | Method | [src/store.mjs:34](file:///home/user/unrdf/packages/oxigraph/src/store.mjs#L34) |
| `store.remove(quad)` | Method | [src/store.mjs:56](file:///home/user/unrdf/packages/oxigraph/src/store.mjs#L56) |
| `store.removeMatches(pattern)` | Method | [src/store.mjs:78](file:///home/user/unrdf/packages/oxigraph/src/store.mjs#L78) |
| `store.query(sparql)` | Method | [src/store.mjs:115](file:///home/user/unrdf/packages/oxigraph/src/store.mjs#L115) |
| `dataFactory.namedNode(iri)` | Method | [src/index.mjs:22](file:///home/user/unrdf/packages/oxigraph/src/index.mjs#L22) |
| `dataFactory.literal(value, lang/datatype)` | Method | [src/index.mjs:34](file:///home/user/unrdf/packages/oxigraph/src/index.mjs#L34) |
| `dataFactory.blankNode(name)` | Method | [src/index.mjs:46](file:///home/user/unrdf/packages/oxigraph/src/index.mjs#L46) |
| `dataFactory.quad(s, p, o, g)` | Method | [src/index.mjs:58](file:///home/user/unrdf/packages/oxigraph/src/index.mjs#L58) |
| `store.match(s, p, o, g)` | Method | [src/store.mjs:93](file:///home/user/unrdf/packages/oxigraph/src/store.mjs#L93) |

---

## Dependencies

- `oxigraph`
- `zod`

---

## Keywords

`rdf` · `sparql` · `graph-database` · `oxigraph` · `knowledge-graph` · `benchmark`

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

RDF triple/quad storage and querying · Input/Output, serialization, streaming, and synchronization · Derivation, inference, reasoning, and knowledge processing · Visualization, rendering, and UI components

---

## Resources

- **Source**: [`packages/oxigraph`](../../packages/oxigraph)
- **Tests**: [`packages/oxigraph/test`](../../packages/oxigraph/test)
- **Examples**: [`packages/oxigraph/examples`](../../packages/oxigraph/examples)

- **Full Capability Map**: [View →](../oxigraph.md)

---

**Last Updated**: 2025-12-28
**Generated from**: capability-map.json
