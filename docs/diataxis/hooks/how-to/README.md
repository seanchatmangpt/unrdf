# How-To Guides — @unrdf/hooks

How-to guides are goal-oriented. Each guide solves a specific task. They assume you already
understand the basics; if you are new to the package, start with the
[Tutorials](../tutorials/README.md).

## Index

| #   | Guide                                                                    | When to use                                                                   |
| --- | ------------------------------------------------------------------------ | ----------------------------------------------------------------------------- |
| 01  | [Define a custom validation hook](./01-define-custom-validation-hook.md) | You need per-quad validation or transformation logic not covered by built-ins |
| 02  | [Chain hooks with a registry](./02-chain-hooks-with-registry.md)         | You want trigger-based dispatch across a registered set of hooks              |
| 03  | [Use a SHACL condition](./03-use-shacl-condition.md)                     | You need whole-store shape validation with block/annotate/repair enforcement  |

## Key gotchas to keep in mind

**N3 quad spread is broken.** `{...quad}` does not copy `subject`/`predicate`/`object`/`graph`
from N3 DataFactory quads (stored as prototype getters, not own properties). Always copy
explicitly in transform functions. See Guide 01 for the canonical pattern.

**`executeHooksByTrigger` returns `ChainResult`, not an array.** Do not index the return value
with `result[0]`. Use `result.valid` and `result.quad`. See Guide 02.
