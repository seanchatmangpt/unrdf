# Tutorial 01: Validate Quads with Built-in Hooks

By the end of this tutorial you will have a working quad-validation pipeline that uses two
built-in hooks — `validateIRIFormat` and `trimLiterals` — chained together via
`executeHookChain`.

## What you will build

A small script that validates and normalises quads before they are added to an RDF store.

## Step 1 — Import the built-in hooks and executor

```javascript
// validate-pipeline.mjs
import { validateIRIFormat, trimLiterals, executeHookChain } from '@unrdf/hooks';
```

`validateIRIFormat` checks that the subject and predicate are well-formed IRIs (RFC-3987: no
spaces, parseable as a URL). `trimLiterals` strips leading/trailing whitespace from literal
object values.

## Step 2 — Create a sample quad

Built-in hooks work with any plain object that has `subject`, `predicate`, `object`, and
optionally `graph` properties. You do not need an N3 DataFactory quad for this tutorial.

```javascript
const quad = {
  subject: { termType: 'NamedNode', value: 'http://example.org/alice' },
  predicate: { termType: 'NamedNode', value: 'http://schema.org/name' },
  object: { termType: 'Literal', value: '  Alice Smith  ', language: '', datatype: null },
  graph: { termType: 'DefaultGraph', value: '' },
};
```

## Step 3 — Run the chain

```javascript
const hooks = [validateIRIFormat, trimLiterals];

const result = await executeHookChain(hooks, quad);
```

`executeHookChain` returns a `ChainResult` object — **not** an array:

```javascript
// result shape
{
  valid: true,
  quad: { ...quadWithTrimmedObject },
  results: [ /* per-hook HookResult */ ],
  error: undefined,
}
```

## Step 4 — Act on the result

```javascript
if (!result.valid) {
  console.error('Quad rejected:', result.error);
  // result.failedHook contains the name of the first hook that failed
} else {
  console.log('Trimmed value:', result.quad.object.value); // 'Alice Smith'
  // store.add(result.quad);  // use the (transformed) quad
}
```

## Step 5 — Try an invalid quad

```javascript
const badQuad = {
  subject: { termType: 'NamedNode', value: 'not a valid IRI' }, // space inside
  predicate: { termType: 'NamedNode', value: 'http://schema.org/name' },
  object: { termType: 'Literal', value: 'test', language: '', datatype: null },
  graph: { termType: 'DefaultGraph', value: '' },
};

const bad = await executeHookChain([validateIRIFormat, trimLiterals], badQuad);
console.log(bad.valid); // false
console.log(bad.error); // 'Validation failed for hook: validate-iri-format'
console.log(bad.failedHook); // 'validate-iri-format'
```

The chain stops at the first failure. `trimLiterals` is never reached for `badQuad`.

## Complete working script

```javascript
// validate-pipeline.mjs
import { validateIRIFormat, trimLiterals, executeHookChain } from '@unrdf/hooks';

const quads = [
  {
    subject: { termType: 'NamedNode', value: 'http://example.org/alice' },
    predicate: { termType: 'NamedNode', value: 'http://schema.org/name' },
    object: { termType: 'Literal', value: '  Alice Smith  ', language: '', datatype: null },
    graph: { termType: 'DefaultGraph', value: '' },
  },
  {
    subject: { termType: 'NamedNode', value: 'bad IRI' }, // invalid
    predicate: { termType: 'NamedNode', value: 'http://schema.org/name' },
    object: { termType: 'Literal', value: 'Bob', language: '', datatype: null },
    graph: { termType: 'DefaultGraph', value: '' },
  },
];

const hooks = [validateIRIFormat, trimLiterals];

for (const quad of quads) {
  const result = await executeHookChain(hooks, quad);
  if (result.valid) {
    console.log('OK:', result.quad.object.value);
  } else {
    console.log('REJECTED:', result.error);
  }
}
// OK: Alice Smith
// REJECTED: Validation failed for hook: validate-iri-format
```

## Next steps

- Tutorial 02: [Your first KnowledgeHook](./02-first-knowledge-hook.md) — whole-store
  policy evaluation.
- How-To: [Define a custom validation hook](../how-to/01-define-custom-validation-hook.md).
