# @unrdf/core Documentation

This directory follows the [Diataxis](https://diataxis.fr/) documentation framework. Each section
serves a distinct reader need.

## Sections

| Section                                | Purpose               | Start here if…                                         |
| -------------------------------------- | --------------------- | ------------------------------------------------------ |
| [Tutorials](./tutorials/README.md)     | Step-by-step learning | You are new to the core package                        |
| [How-To Guides](./how-to/README.md)    | Task-oriented recipes | You know what you want to accomplish                   |
| [Reference](./reference/README.md)     | API facts             | You need exact signatures or field names               |
| [Explanation](./explanation/README.md) | Concepts and design   | You want to understand why things work the way they do |

## What @unrdf/core provides

`@unrdf/core` is the foundational package of UNRDF. It gives you:

- **An RDF store** — create, add, remove, and match quads backed by Oxigraph
- **SPARQL execution** — synchronous and async SELECT, ASK, CONSTRUCT, DESCRIBE
- **SHACL validation** — validate RDF graphs against shape constraints
- **Serialization** — load and dump Turtle, N-Triples, N-Quads, TriG
- **Canonicalization** — URDNA2015 normalisation and isomorphism checks
- **Namespace constants** — pre-built RDF, RDFS, OWL, XSD, FOAF, DCTERMS, SKOS helpers
- **Error types** — structured errors with codes, context, and documentation links

## Quick install

```bash
pnpm add @unrdf/core
```

## The two store styles at a glance

`@unrdf/core` ships two complementary store patterns:

1. **`UnrdfStore` (class-based)** — exported as `createUnrdfStore`. Wraps Oxigraph directly,
   provides synchronous `query()` for use with reactive frameworks, bulk operations, transactions,
   and load/dump serialization. Prefer this for new code.

2. **Functional API** — `createStore` / `addQuad` / `getQuads` / `executeQuery`. Legacy
   functional style retained for backward compatibility. Async wrappers delegate to the
   synchronous executor internally.

See [explanation/02-storage-backends.md](./explanation/02-storage-backends.md) for the design
rationale.
