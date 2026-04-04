# @unrdf/hooks - Quick Start Guide

Get policy enforcement and hook-based validation running in 5 minutes.

## Quick Start

### 1. Install

```bash
pnpm add @unrdf/hooks
```

### 2. Use Built-in Hooks

```javascript
import { executeHook, validateIRIFormat } from '@unrdf/hooks';

const quad = {
  subject: { value: 'http://example.org/subject' },
  predicate: { value: 'http://example.org/pred' },
  object: { value: 'test' },
};

// Validate quad using built-in hook
const result = await executeHook(validateIRIFormat, quad);
console.log('Valid:', result.valid); // true
```

### 3. Define Custom Hook

```javascript
import { defineHook, executeHook } from '@unrdf/hooks';

// Define a custom validation hook
const myHook = defineHook({
  name: 'custom-validator',
  trigger: 'before-add',
  validate: quad => {
    // Custom validation logic
    return quad.subject.value.startsWith('http://');
  },
});

// Execute it
const result = await executeHook(myHook, quad);
```

### 4. Define Hook with Transform

```javascript
import { defineHook, executeHook } from '@unrdf/hooks';

const normalizer = defineHook({
  name: 'uppercase-objects',
  trigger: 'before-add',
  transform: quad => ({
    ...quad,
    object: {
      ...quad.object,
      value: quad.object.value.toUpperCase(),
    },
  }),
});

const result = await executeHook(normalizer, quad);
console.log('Transformed:', result.quad.object.value); // 'TEST'
```

### 5. Chain Multiple Hooks

```javascript
import { defineHook, executeHookChain, validateIRIFormat, trimLiterals } from '@unrdf/hooks';

// Create a chain of hooks
const hooks = [
  validateIRIFormat, // Built-in: validate IRI
  defineHook({
    name: 'custom-check',
    trigger: 'before-add',
    validate: q => q.object.value.length > 0,
  }),
  trimLiterals, // Built-in: trim whitespace
];

// Execute entire chain
const result = await executeHookChain(hooks, quad);
console.log('Valid:', result.valid);
console.log('Transformed:', result.quad);
```

### 6. Batch Operations

```javascript
import {
  validateSubjectIRI,
  validateBatch,
  executeBatch,
  transformBatch
} from '@unrdf/hooks';

const quads = [
  { subject: { value: 'http://example.org/s1' }, ... },
  { subject: { value: 'http://example.org/s2' }, ... },
  { subject: { value: 'invalid' }, ... }
];

// Get boolean array of validity
const validArray = await validateBatch([validateSubjectIRI], quads);
console.log(validArray); // [true, true, false]

// Get detailed results
const results = await executeBatch([validateSubjectIRI], quads);
results.forEach((r, i) => {
  console.log(`Quad ${i}: ${r.valid ? '✓' : '✗'}`);
});

// Apply transformations
const normalized = await transformBatch([trimLiterals], quads);
```

## Built-in Hooks

### Validators

- `validateSubjectIRI` - Subject must be NamedNode
- `validatePredicateIRI` - Predicate must be NamedNode
- `validateIRIFormat` - RFC3987 compliance
- `validateObjectLiteral` - Non-empty literals
- `validateLanguageTag` - BCP47 language tags
- `rejectBlankNodes` - Reject blank nodes

### Normalizers

- `normalizeLanguageTag` - Lowercase language tags
- `trimLiterals` - Remove whitespace
- `normalizeNamespace` - Standardize URIs
- `normalizeLanguageTagPooled` - Fast with pooling
- `trimLiteralsPooled` - Fast with pooling

### Composite

- `standardValidation` - All-in-one validation

## Patterns

### Validate Before Store

```javascript
import { executeHookChain, standardValidation } from '@unrdf/hooks';
import { createStore } from '@unrdf/oxigraph';

const store = createStore();

async function addQuad(quad) {
  const result = await executeHookChain([standardValidation], quad);

  if (!result.valid) {
    throw new Error('Quad validation failed');
  }

  // Add the (potentially transformed) quad
  store.add(result.quad);
}
```

### Custom Validation Pipeline

```javascript
const pipeline = [
  defineHook({
    name: 'reject-internal',
    trigger: 'before-add',
    validate: q => !q.subject.value.includes('internal'),
  }),
  defineHook({
    name: 'normalize',
    trigger: 'before-add',
    transform: q => ({ ...q, object: { value: q.object.value.trim() } }),
  }),
  standardValidation,
];

const result = await executeHookChain(pipeline, quad);
```

### Performance-Optimized Batch

```javascript
import { normalizeLanguageTagPooled, trimLiteralsPooled, transformBatch } from '@unrdf/hooks';

const quads = largeBatch; // 10,000+ quads

// Pooled versions use object reuse (no allocation)
const normalized = await transformBatch([normalizeLanguageTagPooled, trimLiteralsPooled], quads);
```

## Testing

Run the comprehensive test suite:

```bash
pnpm test -- builtin-hooks-advanced
```

This runs 46 tests covering:

- All 12+ built-in hooks
- Edge cases (empty literals, invalid IRIs, etc.)
- Hook chaining and composition
- Batch operations and scaling
- Performance baselines

## Examples

See [examples/](./examples/) for:

- `hook-chains/` - Multi-hook validation pipelines
- `policy-hooks/` - Policy enforcement examples

## Support

- **API Reference**: See [API-REFERENCE.md](./API-REFERENCE.md)
- **Architecture**: See [ARCHITECTURE.md](./ARCHITECTURE.md)
- **Issues**: https://github.com/unrdf/unrdf/issues
