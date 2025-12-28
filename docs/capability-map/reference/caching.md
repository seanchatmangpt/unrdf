# @unrdf/caching API Reference

**Version**: 1.0.0
**Maturity**: stable
**Main Export**: src/index.mjs

---

## Description

Multi-layer caching system for RDF queries with Redis and LRU

---

## Installation

```bash
pnpm add @unrdf/caching
```

---

## Exports

**Module Exports**:
- `src/index.mjs`
- `src/layers/multi-layer-cache.mjs`
- `src/invalidation/dependency-tracker.mjs`
- `src/query/sparql-cache.mjs`
- `{`

---

## Dependencies

- `@unrdf/oxigraph`
- `ioredis`
- `lru-cache`
- `msgpackr`
- `zod`

---

## Keywords

`cache` · `redis` · `lru` · `rdf` · `sparql` · `performance`

---

## Maturity Signals

| Signal | Status |
|--------|--------|
| Has Tests | ❌ No |
| Has Examples | ✅ Yes |
| Has README | ✅ Yes |
| Has ChangeLog | ❌ No |

---

## Package Role

General utility functions

---

## Resources

- **Source**: [`packages/caching`](../../packages/caching)
- **Tests**: [`packages/caching/test`](../../packages/caching/test)
- **Examples**: [`packages/caching/examples`](../../packages/caching/examples)

- **Full Capability Map**: *Coming soon*

---

**Last Updated**: 2025-12-28
**Generated from**: capability-map.json
