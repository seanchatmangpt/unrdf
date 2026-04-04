# Explanation — @unrdf/hooks

Explanation documents the concepts and design decisions behind the hooks package. Read these
when you want to understand _why_ things work the way they do, not just _how_ to use them.

## Index

| Document                                           | Topic                                                                                                              |
| -------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| [01-two-hook-systems.md](./01-two-hook-systems.md) | Why there are two distinct hook systems, when to use each, and why `ChainResult` is an object rather than an array |

## Background

`@unrdf/hooks` is the governance layer for `@unrdf/core`. It was designed around two
orthogonal concerns:

1. **Quad-level integrity** — Ensuring every quad that enters the store is structurally valid
   and normalised. This must be fast (sub-microsecond) and happen synchronously at ingestion
   time.

2. **Store-level policy** — Evaluating whole-graph conditions (SPARQL, SHACL, N3 inference)
   and applying RDF-native transformations with cryptographic audit trails. This is inherently
   asynchronous and acts on the store as a whole.

These two concerns have different performance profiles, different author audiences, and
different configuration granularity, which is why they are separate APIs rather than one
unified system.
