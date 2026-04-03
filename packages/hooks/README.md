# @unrdf/hooks

![Version](https://img.shields.io/badge/version-5.0.0--beta.1-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)


**Knowledge Hooks - Policy Definition and Execution Framework**

The policy enforcement layer for UNRDF. Define and execute hooks to enforce rules, validate data, and transform quads.

## Installation

```bash
pnpm add @unrdf/hooks
```

## 📚 Examples

See these examples that demonstrate @unrdf/hooks:

- **[basic-knowledge-hook.mjs](../../examples/basic-knowledge-hook.mjs)** - Hook basics: validation and transformation (15 min)
- **[define-hook-example.mjs](../../examples/define-hook-example.mjs)** - Advanced hook composition patterns (20 min)
- **[production-hook-test.mjs](../../examples/production-hook-test.mjs)** - Production-ready hook testing
- **[hook-lifecycle-test.mjs](../../examples/hook-lifecycle-test.mjs)** - Complete hook lifecycle
- **[knowledge-hooks-events.mjs](../../examples/knowledge-hooks-events.mjs)** - Event-driven hooks

**Want to learn hooks?** Follow the [hooks learning path](../../examples/README.md#-knowledge-hooks).

## Quick Start

```javascript
import { defineHook, executeHook } from '@unrdf/hooks'

// Define a validation hook
defineHook('validate-pii', {
  type: 'validate-before-write',
  async check(quad) {
    // Reject PII without consent
    if (isPII(quad) && !hasConsent(quad.subject)) {
      return false
    }
    return true
  }
})

// Execute hooks
const isValid = await executeHook('validate-pii', quad)
```

## Features

- ✅ Define custom hooks for any RDF operation
- ✅ Pre-write validation (block invalid data)
- ✅ Post-write transformation (modify data)
- ✅ Hook composition (chain multiple hooks)
- ✅ Type-safe hook definitions
- ✅ Async/await support

## Use Cases

- **Validation**: Check data before writing
- **Transformation**: Modify quads on the fly
- **Enforcement**: Implement security policies
- **Audit**: Track and log changes
- **Compliance**: Enforce regulatory requirements

## Governance Hook Examples

### Example 1: IRI Format Validation

```javascript
import { defineHook, executeHookChain } from '@unrdf/hooks';
import { namedNode } from '@unrdf/oxigraph';

// Define IRI validation hook
const iriValidationHook = defineHook('validate-iri-format', {
  type: 'validate-before-write',
  async check(quad) {
    // Only validate on triples (not quads with graph contexts)
    if (!quad.subject || !quad.predicate || !quad.object) return true;

    const { subject, predicate, object } = quad;

    // Check IRI format using RFC 3986
    const iriRegex = /^[a-zA-Z][a-zA-Z0-9+.-]*:[^\s<>"{}|^\\`]+$/;

    const subjectValid = subject.termType === 'NamedNode' && iriRegex.test(subject.value);
    const predicateValid = predicate.termType === 'NamedNode' && iriRegex.test(predicate.value);

    let objectValid = true;
    if (object.termType === 'NamedNode') {
      objectValid = iriRegex.test(object.value);
    }
    // Literals are always valid

    if (!subjectValid || !predicateValid || !objectValid) {
      return {
        valid: false,
        message: `Invalid IRI format: subject=${subject.value}, predicate=${predicate.value}, object=${object.value}`
      };
    }

    return { valid: true };
  }
});

// Execute validation chain
const validationResults = await executeHookChain(
  [iriValidationHook],
  quad
);

console.log(validationResults[0]); // { valid: true, message: '' }
```

### Example 2: Reject Blank Nodes (Referential Integrity)

```javascript
import { defineHook } from '@unrdf/hooks';

// Define blank node rejection hook
const blankNodeHook = defineHook('reject-blank-nodes', {
  type: 'validate-before-write',
  async check(quad) {
    const { subject, object } = quad;

    // Reject blank nodes as subjects (they must have named IRIs)
    if (subject.termType === 'BlankNode') {
      return {
        valid: false,
        message: `Blank nodes are not allowed as subjects (found: ${subject.value})`
      };
    }

    // Optionally reject blank nodes in certain positions
    // This prevents ambiguity in knowledge graphs
    if (object.termType === 'BlankNode') {
      return {
        valid: false,
        message: `Blank nodes are not allowed as objects (found: ${object.value})`
      };
    }

    return { valid: true };
  }
});
```

### Example 3: Namespace Normalization

```javascript
import { defineHook, executeBatch } from '@unrdf/hooks';
import { COMMON_PREFIXES } from '@unrdf/core';

// Define namespace normalization hook
const normalizeNamespaceHook = defineHook('normalize-namespace', {
  type: 'post-write-transform',
  async transform(quads) {
    const normalized = [];

    for (const quad of quads) {
      const { subject, predicate, object } = quad;

      // Normalize subject and predicate to canonical form
      const normalizedSubject = normalizeIri(subject.value);
      const normalizedPredicate = normalizeIri(predicate.value);

      // Create normalized quad
      const { namedNode, literal } = { namedNode, literal }; // Context for destructuring
      normalized.push(
        quad(
          namedNode(normalizedSubject),
          namedNode(normalizedPredicate),
          object.termType === 'Literal' ? object : namedNode(object.value)
        )
      );
    }

    return normalized;
  }
});

// Helper function for namespace normalization
function normalizeIri(iri) {
  // Normalize 'http://example.com/' to 'http://example.com/'
  // and 'http://example.com' to 'http://example.com/'

  // Check if ends with slash
  const hasSlash = iri.endsWith('/');
  const noSlash = iri.replace(/\/$/, '');

  // Check against common prefixes
  for (const [prefix, uri] of Object.entries(COMMON_PREFIXES)) {
    if (iri.startsWith(uri)) {
      const local = iri.slice(uri.length);
      return hasSlash ? uri : `${noSlash}/`;
    }
  }

  return iri;
}

// Batch normalization example
const quads = [
  quad(namedNode('http://example.com/alice'), namedNode('http://xmlns.com/foaf/0.1/name'), literal('Alice')),
  quad(namedNode('http://example.com/alice'), namedNode('http://example.com/'), literal('exists')),
];

const normalized = await executeBatch([normalizeNamespaceHook], quads);
console.log(normalized);
```

### Example 4: Custom Governance Tier Validation

```javascript
import { defineHook } from '@unrdf/hooks';

// Define tier-based validation hook
const tierValidationHook = defineHook('validate-tier-access', {
  type: 'validate-before-write',
  async check(quad) {
    const { subject, predicate, object } = quad;

    // Check if this is a governance-related triple
    const governancePredicates = [
      'http://example.com/policy/tier',
      'http://example.com/governance/admission',
      'http://example.com/governance/receipt',
    ];

    if (!governancePredicates.includes(predicate.value)) {
      return { valid: true }; // Not a governance triple, skip validation
    }

    // Validate that tier is properly set
    if (object.termType !== 'NamedNode') {
      return {
        valid: false,
        message: `Governance tier must be a NamedNode, got: ${object.termType}`
      };
    }

    const validTiers = ['gold', 'silver', 'bronze'];
    const tier = object.value.toLowerCase().split('/').pop();

    if (!validTiers.includes(tier)) {
      return {
        valid: false,
        message: `Invalid governance tier: ${tier}. Must be one of: ${validTiers.join(', ')}`
      };
    }

    return { valid: true };
  }
});
```

### Example 5: Hook Registry for Multiple Governance Rules

```javascript
import { createHookRegistry, executeHookChain } from '@unrdf/hooks';
import { defineHook } from './hooks/define-hook.mjs';

// Create hook registry for governance rules
const governanceRegistry = createHookRegistry();

// Register multiple validation hooks
governanceRegistry.register('iri-format', defineHook('validate-iri-format', {
  type: 'validate-before-write',
  async check(quad) {
    // IRI validation logic...
    return { valid: true };
  }
}));

governanceRegistry.register('blank-nodes', defineHook('reject-blank-nodes', {
  type: 'validate-before-write',
  async check(quad) {
    // Blank node rejection logic...
    return { valid: true };
  }
}));

governanceRegistry.register('tier-validation', defineHook('validate-tier-access', {
  type: 'validate-before-write',
  async check(quad) {
    // Tier validation logic...
    return { valid: true };
  }
}));

// Execute all registered hooks for a quad
async function validateQuad(governanceRegistry, quad) {
  const results = await executeHookChain(
    governanceRegistry.getAll(),
    quad
  );

  // Check if all hooks passed
  const allValid = results.every(r => r.valid === true);

  if (!allValid) {
    const errors = results
      .filter(r => !r.valid)
      .map(r => r.message);
    throw new Error(`Governance validation failed:\n${errors.join('\n')}`);
  }

  return quad;
}

// Usage
const validatedQuad = await validateQuad(governanceRegistry, quad);
```

These governance examples demonstrate:
- **IRI format validation** for data quality
- **Blank node rejection** for referential integrity
- **Namespace normalization** for consistent data representation
- **Tier-based access control** for governance rules
- **Hook registry** for managing multiple governance rules

See [API Reference](./docs/API.md) for complete hook API documentation.

## Documentation

- **[API Reference](./docs/API.md)** - Complete API documentation
- **[User Guide](./docs/GUIDE.md)** - How to use hooks
- **[Examples](./examples/)** - Code examples
- **[Contributing](./docs/CONTRIBUTING.md)** - How to contribute

## Depends On

- `@unrdf/core` - RDF substrate

## VOC Usage

- VOC-1: Autonomous Knowledge Agent (policy enforcement)
- VOC-3: ML Agent (apply learned patterns)
- VOC-4: Audit Agent (compliance monitoring)
- VOC-5: Data Engineer (validation during ingestion)

## License

MIT
