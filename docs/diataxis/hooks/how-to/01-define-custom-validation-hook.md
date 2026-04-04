# How To: Define a Custom Validation Hook

Use `defineHook()` when you need per-quad validation or transformation logic that the built-in
hooks do not cover.

## Validation-only hook

```javascript
import { defineHook, executeHook } from '@unrdf/hooks';

const rejectInternalSubjects = defineHook({
  name: 'reject-internal-subjects',
  trigger: 'before-add',
  validate: quad => {
    // Return true to accept, false to reject
    return !quad.subject.value.includes('/internal/');
  },
});

const quad = {
  subject: { termType: 'NamedNode', value: 'http://example.org/internal/secret' },
  predicate: { termType: 'NamedNode', value: 'http://schema.org/name' },
  object: { termType: 'Literal', value: 'secret', language: '', datatype: null },
  graph: { termType: 'DefaultGraph', value: '' },
};

const result = executeHook(rejectInternalSubjects, quad);
console.log(result.valid); // false
console.log(result.error); // 'Validation failed for hook: reject-internal-subjects'
```

`executeHook` is synchronous. The `validate` function must return a plain boolean. A non-boolean
return is coerced with a warning (POKA-YOKE guard).

## Transform hook — IMPORTANT: N3 quad spread is broken

When writing a `transform` function, you must copy quad properties explicitly. The spread
operator (`{...quad}`) does **not** copy `subject`, `predicate`, `object`, or `graph` from N3
DataFactory quads because those properties are stored on the prototype as getters, not as own
enumerable properties.

**Wrong — spread silently drops the RDF terms:**

```javascript
// DO NOT DO THIS with N3 quads
transform: quad => ({
  ...quad,
  object: { ...quad.object, value: quad.object.value.toUpperCase() },
});
```

**Correct — always use explicit property assignment:**

```javascript
import { defineHook, executeHook } from '@unrdf/hooks';

const uppercaseObjects = defineHook({
  name: 'uppercase-objects',
  trigger: 'before-add',
  transform: quad => ({
    subject: quad.subject,
    predicate: quad.predicate,
    object: {
      termType: quad.object.termType,
      value: quad.object.value.toUpperCase(),
      datatype: quad.object.datatype,
      language: quad.object.language,
    },
    graph: quad.graph,
  }),
});

const result = executeHook(uppercaseObjects, quad);
console.log(result.quad.object.value); // 'ALICE SMITH'
```

The transform function receives the current quad and must return a new object with all four
properties (`subject`, `predicate`, `object`, `graph`) present. Returning `null` or `undefined`
sets `result.valid = false`. Returning an object missing any of the three required properties
throws a `TypeError`.

## Hook with both validate and transform

A hook may define both `validate` and `transform`. Validation runs first; if it returns
`false`, the transform is never called.

```javascript
const validateAndNormalize = defineHook({
  name: 'validate-and-normalize-literals',
  trigger: 'before-add',
  validate: quad => quad.object.termType === 'Literal',
  transform: quad => ({
    subject: quad.subject,
    predicate: quad.predicate,
    object: {
      termType: quad.object.termType,
      value: quad.object.value.trim(),
      datatype: quad.object.datatype,
      language: quad.object.language ? quad.object.language.toLowerCase() : '',
    },
    graph: quad.graph,
  }),
});
```

## Available triggers

Common triggers for data-ingestion hooks:

| Trigger         | When                           |
| --------------- | ------------------------------ |
| `before-add`    | Before a quad enters the store |
| `after-add`     | After a quad enters the store  |
| `before-remove` | Before a quad is deleted       |
| `before-query`  | Before a query executes        |

See [reference/low-level-api.md](../reference/low-level-api.md) for the full trigger list.

## Notes

- `defineHook()` validates the config with Zod and pre-computes `_hasValidation` /
  `_hasTransformation` flags for sub-microsecond hot-path execution.
- A hook must define at least one of `validate`, `transform`, or `run`. Providing none throws
  a `ZodError`.
- Hook names must be unique within a registry. Duplicate names throw at `registerHook()` time.
