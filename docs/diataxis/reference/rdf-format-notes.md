# RDF Format Notes

**Purpose:** Reference for RDF serialization formats supported by UNRDF.

**Audience:** Developers working with RDF data

**Version:** 5.0.1

---

## Overview

UNRDF supports multiple RDF serialization formats for parsing and serialization:
- Turtle (TTL)
- N-Triples
- N-Quads
- JSON-LD
- RDF/XML (parsing only)

---

## Turtle (TTL)

**[Placeholder - Content to be filled]**

**MIME Type:** `text/turtle`
**File Extension:** `.ttl`

### Syntax Overview

```turtle
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Alice rdf:type ex:Person ;
         ex:name "Alice" ;
         ex:age 30 ;
         ex:knows ex:Bob .
```

**Evidence:** Turtle parser at `/home/user/unrdf/packages/core/src/parsers/turtle.mjs`

---

### UNRDF Extensions

**[Placeholder - UNRDF-specific Turtle extensions]**

---

### Performance Characteristics

**[Placeholder - Performance notes]**

- Parse speed: ~50MB/s
- Serialize speed: ~40MB/s
- Memory overhead: ~2x file size

**Evidence:** Benchmarks at `/home/user/unrdf/benchmarks/parse-turtle.mjs`

---

## N-Quads

**[Placeholder - N-Quads format details]**

**Evidence:** N-Quads parser at `/home/user/unrdf/packages/core/src/parsers/nquads.mjs`

---

## JSON-LD

**[Placeholder - JSON-LD format details]**

**Evidence:** JSON-LD parser at `/home/user/unrdf/packages/core/src/parsers/jsonld.mjs`

---

## RDF/XML

**[Placeholder - RDF/XML format details]**

**Evidence:** RDF/XML parser at `/home/user/unrdf/packages/core/src/parsers/rdfxml.mjs`

---

## Format Comparison

**[Placeholder - Format comparison table]**

| Format | Parse Speed | Size | Human Readable | Streaming |
|--------|-------------|------|----------------|-----------|
| Turtle | Fast | Small | ✅ Yes | ✅ Yes |
| N-Quads | Very Fast | Medium | ⚠️ Partial | ✅ Yes |
| JSON-LD | Medium | Large | ✅ Yes | ⚠️ Partial |
| RDF/XML | Slow | Large | ❌ No | ❌ No |

---

## Conversion Examples

**[Placeholder - Format conversion examples]**

**Evidence:** Examples at `/home/user/unrdf/examples/format-conversion.mjs`

---

## Browser Support

**[Placeholder - Browser format support]**

**Evidence:** Browser support at `/home/user/unrdf/packages/browser/src/format-support.mjs`

---

## Related References

- **[Package Exports Reference](./package-exports.md)** - Core package exports
- **[Tutorial 02: Parse RDF in Browser](../tutorials/02-parse-rdf-in-browser.md)** - Browser parsing

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
