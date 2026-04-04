# @unrdf/hooks Documentation

This directory follows the [Diataxis](https://diataxis.fr/) documentation framework. Each section
serves a distinct reader need.

## Sections

| Section                                | Purpose               | Start here if…                                         |
| -------------------------------------- | --------------------- | ------------------------------------------------------ |
| [Tutorials](./tutorials/README.md)     | Step-by-step learning | You are new to the hooks package                       |
| [How-To Guides](./how-to/README.md)    | Task-oriented recipes | You know what you want to accomplish                   |
| [Reference](./reference/README.md)     | API facts             | You need exact signatures or field names               |
| [Explanation](./explanation/README.md) | Concepts and design   | You want to understand why things work the way they do |

## The two hook systems at a glance

`@unrdf/hooks` ships two distinct, complementary systems:

1. **Low-level hooks** — JavaScript functions created with `defineHook()`, executed synchronously
   per-quad by `executeHook()` / `executeHookChain()`. Used for validation and transformation
   pipelines at the point of data ingestion.

2. **KnowledgeHooks** — JSON-declarative policy objects evaluated by `KnowledgeHookEngine`
   against an entire RDF store. Used for whole-store governance, SHACL enforcement, and
   SPARQL-driven transformations with cryptographic audit receipts.

See [explanation/01-two-hook-systems.md](./explanation/01-two-hook-systems.md) for the full
rationale.

## Quick install

```bash
pnpm add @unrdf/hooks
```
