# Knowledge Hook Pattern (Base)

Reusable Knowledge Hook structure used throughout documentation.

## Base Pattern

```typescript
import { defineKnowledgeHook } from 'unrdf/hooks';

defineKnowledgeHook({
  // Unique identifier
  id: 'hook-name',

  // When to run: 'pre-transaction' or 'post-transaction'
  type: 'pre-transaction',

  // Predicate: WHEN to trigger (returns boolean)
  predicate: (delta) => {
    // Check if delta matches trigger conditions
    return delta.added.some(quad =>
      quad.predicate.value === 'http://schema.org/targetProperty'
    );
  },

  // Effect: WHAT to do (async function)
  effect: async (delta, context) => {
    // Access added/removed quads
    for (const quad of delta.added) {
      // Process each quad
      const subject = quad.subject.value;
      const object = quad.object.value;

      // Use context helpers
      await context.insert([/* new quads */]);
      await context.query(`SPARQL query`);
      await context.emit('event-name', data);
    }
  }
});
```

## Common Predicates

```typescript
// Match specific predicate
predicate: (delta) => {
  return delta.added.some(q =>
    q.predicate.value === 'http://schema.org/price'
  );
}

// Match subject pattern
predicate: (delta) => {
  return delta.added.some(q =>
    q.subject.value.startsWith('http://example.org/product/')
  );
}

// Threshold-based
predicate: (delta) => {
  return delta.added.length > 10;
}
```

## Common Effects

```typescript
// Validation
effect: async (delta, context) => {
  for (const quad of delta.added) {
    if (!isValid(quad.object.value)) {
      throw new Error('Validation failed');
    }
  }
}

// Enrichment
effect: async (delta, context) => {
  const enrichedQuads = delta.added.map(enrich);
  await context.insert(enrichedQuads);
}

// Notification
effect: async (delta, context) => {
  await context.emit('data-changed', {
    timestamp: new Date().toISOString(),
    changes: delta.added.length
  });
}
```

See specific chapters for advanced patterns and real-world examples.
