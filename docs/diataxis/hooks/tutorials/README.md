# Tutorials — @unrdf/hooks

Tutorials guide you through learning-oriented tasks. Follow them in order if you are new to
the hooks package. Each tutorial produces a working result by the end.

## Prerequisites

```bash
pnpm add @unrdf/hooks
```

Node.js >= 18 is required.

## Index

| #   | Tutorial                                                                        | What you will learn                                          |
| --- | ------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| 01  | [Validate quads with built-in hooks](./01-validate-quads-with-builtin-hooks.md) | `executeHookChain`, `validateIRIFormat`, `trimLiterals`      |
| 02  | [Your first KnowledgeHook](./02-first-knowledge-hook.md)                        | JSON hook schema, `evaluateCondition`, `KnowledgeHookEngine` |

## After the tutorials

- Read the [How-To Guides](../how-to/README.md) for specific tasks such as defining custom
  hooks or using SHACL conditions.
- Consult the [Reference](../reference/README.md) for exact API signatures.
- Read the [Explanation](../explanation/README.md) to understand the design decisions behind
  the two hook systems.
