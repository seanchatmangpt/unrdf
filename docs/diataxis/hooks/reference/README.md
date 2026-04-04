# Reference — @unrdf/hooks

Reference material is for looking things up. It is accurate and complete, not tutorial-style.

## Index

| Document                                               | Covers                                                                                                                                                            |
| ------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [low-level-api.md](./low-level-api.md)                 | `defineHook`, `executeHook`, `executeHookChain`, `executeBatch`, `validateBatch`, `transformBatch`, `createHookRegistry`, `registerHook`, `executeHooksByTrigger` |
| [knowledge-hook-schema.md](./knowledge-hook-schema.md) | `KnowledgeHookSchema` field reference: all condition kinds, effect kinds, metadata fields                                                                         |
| [builtin-hooks.md](./builtin-hooks.md)                 | Table of all built-in hooks: name, trigger, behaviour, pooled variant                                                                                             |

## Imports

All public exports are available from the package root:

```javascript
import {
  // Low-level hooks
  defineHook,
  executeHook,
  executeHookChain,
  executeBatch,
  validateBatch,
  transformBatch,
  createHookRegistry,
  registerHook,
  executeHooksByTrigger,

  // KnowledgeHooks
  KnowledgeHookEngine,
  createKnowledgeHook,
  validateKnowledgeHook,

  // Built-ins
  validateSubjectIRI,
  validatePredicateIRI,
  validateIRIFormat,
  validateObjectLiteral,
  validateLanguageTag,
  rejectBlankNodes,
  normalizeLanguageTag,
  trimLiterals,
  normalizeNamespace,
  normalizeLanguageTagPooled,
  trimLiteralsPooled,
  standardValidation,
} from '@unrdf/hooks';
```
