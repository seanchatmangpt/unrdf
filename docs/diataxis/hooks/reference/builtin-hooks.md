# Reference: Built-in Hooks

All built-in hooks are exported from `@unrdf/hooks` and are created with `defineHook()`,
so they carry pre-computed flags and are compatible with all executor functions.

---

## Validation hooks

These hooks set `validate` and return `false` to reject a quad. They do not modify the quad.

| Export name             | Internal name             | Trigger      | What it checks                                                                                               |
| ----------------------- | ------------------------- | ------------ | ------------------------------------------------------------------------------------------------------------ |
| `validateSubjectIRI`    | `validate-subject-iri`    | `before-add` | `quad.subject.termType === 'NamedNode'`                                                                      |
| `validatePredicateIRI`  | `validate-predicate-iri`  | `before-add` | `quad.predicate.termType === 'NamedNode'`                                                                    |
| `validateIRIFormat`     | `validate-iri-format`     | `before-add` | Subject and predicate IRIs contain no spaces and parse as a valid URL (RFC-3987)                             |
| `validateObjectLiteral` | `validate-object-literal` | `before-add` | `quad.object.termType === 'Literal'` and `value.length > 0`                                                  |
| `validateLanguageTag`   | `validate-language-tag`   | `before-add` | Language tag matches BCP 47: `/^[a-zA-Z]{2,3}(-[a-zA-Z]{2,4})?$/`. Quads without a language tag always pass. |
| `rejectBlankNodes`      | `reject-blank-nodes`      | `before-add` | Neither subject nor object is a `BlankNode`                                                                  |

---

## Transformation hooks

These hooks set `transform` and return a new quad object. They do not reject quads.

| Export name            | Internal name            | Trigger      | What it does                                                                                                             |
| ---------------------- | ------------------------ | ------------ | ------------------------------------------------------------------------------------------------------------------------ |
| `normalizeLanguageTag` | `normalize-language-tag` | `before-add` | Lowercases `quad.object.language`. Passes through quads with no language tag unchanged.                                  |
| `trimLiterals`         | `trim-literals`          | `before-add` | Trims leading/trailing whitespace from `quad.object.value` for `Literal` objects. Passes through non-literals unchanged. |
| `normalizeNamespace`   | `normalize-namespace`    | `before-add` | Pass-through in the current implementation. Reserved for namespace prefix expansion.                                     |

All transformation hooks copy quad properties explicitly (not via spread) to handle N3
prototype getters correctly.

---

## Pooled transformation hooks

Pooled variants use an internal `quadPool` for zero-allocation transforms. Use them in
high-throughput batch processing (10 000+ quads). The returned quad carries a `_pooled` flag;
clone it before storing to a persistent collection.

| Export name                  | Internal name                   | Trigger      | What it does                                                                                  |
| ---------------------------- | ------------------------------- | ------------ | --------------------------------------------------------------------------------------------- |
| `normalizeLanguageTagPooled` | `normalize-language-tag-pooled` | `before-add` | Same as `normalizeLanguageTag` but uses pool allocation                                       |
| `trimLiteralsPooled`         | `trim-literals-pooled`          | `before-add` | Same as `trimLiterals` but uses pool allocation; skips allocation if value is already trimmed |

---

## Composite hooks

| Export name          | Internal name         | Trigger      | What it does                                                                                                                                                     |
| -------------------- | --------------------- | ------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `standardValidation` | `standard-validation` | `before-add` | Combined check: predicate must be `NamedNode`; subject must be `NamedNode` or `BlankNode`; IRIs (subject and predicate) must contain no spaces and parse as URLs |

`standardValidation` is the recommended single-hook option for most ingestion pipelines when
you do not need to customise individual checks.

---

## Usage example

```javascript
import {
  validateIRIFormat,
  trimLiterals,
  standardValidation,
  normalizeLanguageTagPooled,
  trimLiteralsPooled,
  executeHookChain,
  transformBatch,
} from '@unrdf/hooks';

// Standard pipeline for small batches
const result = await executeHookChain([standardValidation, trimLiterals], quad);

// High-throughput batch with pooled variants
const normalized = await transformBatch(
  [normalizeLanguageTagPooled, trimLiteralsPooled],
  largeQuadArray
);
```

---

## `builtinHooks` named export

A convenience object containing all built-in hooks:

```javascript
import { builtinHooks } from '@unrdf/hooks';

// builtinHooks.validateSubjectIRI
// builtinHooks.validatePredicateIRI
// builtinHooks.validateObjectLiteral
// builtinHooks.validateIRIFormat
// builtinHooks.validateLanguageTag
// builtinHooks.rejectBlankNodes
// builtinHooks.normalizeNamespace
// builtinHooks.normalizeLanguageTag
// builtinHooks.trimLiterals
// builtinHooks.normalizeLanguageTagPooled
// builtinHooks.trimLiteralsPooled
// builtinHooks.standardValidation
```
