# How-To Guides — @unrdf/oxigraph

How-To guides are task-oriented. Each guide solves one specific problem for a practitioner who already has basic familiarity with the package.

## Available Guides

| Guide                                                                       | Problem it solves                                                           |
| --------------------------------------------------------------------------- | --------------------------------------------------------------------------- |
| [01 — Configure persistent backend](01-configure-persistent-backend.md)     | Set cache size, TTL, and pattern analysis options on `CachedQueryStore`     |
| [02 — Switch from memory to Oxigraph](02-switch-from-memory-to-oxigraph.md) | Drop-in replace an existing in-memory N3 or core store with `OxigraphStore` |
| [03 — Bulk-load a Turtle file](03-bulk-load-turtle-file.md)                 | Load large Turtle (and other format) files into the store efficiently       |

## Prerequisites

You have installed `@unrdf/oxigraph` and can create a basic store (see [Tutorial 01](../tutorials/01-persistent-rdf-store.md) if not).

## See Also

- [Reference: OxigraphStore API](../reference/oxigraph-store-api.md) — complete method signatures
- [Reference: Configuration Options](../reference/configuration-options.md) — all options tables
- [Explanation: Why Oxigraph backend](../explanation/01-why-oxigraph-backend.md) — rationale for choosing Oxigraph
