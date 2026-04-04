# Explanation — @unrdf/core

Explanation documents the concepts and design decisions behind the core package. Read these
when you want to understand _why_ things work the way they do, not just _how_ to use them.

## Index

| Document                                           | Topic                                                                     |
| -------------------------------------------------- | ------------------------------------------------------------------------- |
| [01-rdf-data-model.md](./01-rdf-data-model.md)     | Quads, terms, graphs, and why the RDF data model underpins UNRDF          |
| [02-storage-backends.md](./02-storage-backends.md) | Why there are two store styles, when to use each, and what Oxigraph gives |

## Background

`@unrdf/core` was designed around two requirements that pulled in slightly different directions:

1. **Standards compliance** — RDF is a W3C standard with a precise data model. The store must
   behave according to that model: quads not triples, named graphs, typed literals, blank node
   scoping.

2. **Synchronous execution** — JavaScript reactive frameworks (Vue `computed()`, React `useMemo`,
   MobX `computed`) require synchronous reads. An async SPARQL executor cannot be used inside a
   `computed()` without breaking reactivity. The `UnrdfStore.query()` method exists specifically
   to meet this requirement.

These two requirements explain the two store styles described in
[explanation/02-storage-backends.md](./02-storage-backends.md).
