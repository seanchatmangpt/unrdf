# Knowledge Hooks Mastery

**Knowledge Hooks** are UNRDF's killer feature: reactive triggers that automatically execute when your RDF data changes. They enable patterns that were impossible before.

This chapter covers everything you need to master Knowledge Hooks in production.

---

## What are Knowledge Hooks?

Knowledge Hooks are **reactive triggers** that execute code when your RDF knowledge graph changes:

```typescript
defineKnowledgeHook({
  id: 'validate-product-price',
  type: 'pre-transaction',

  // ✅ Predicate: When to trigger
  predicate: (delta) => {
    return delta.added.some(quad =>
      quad.predicate.value === 'http://schema.org/price'
    );
  },

  // ✅ Effect: What to do
  effect: async (delta, context) => {
    for (const quad of delta.added) {
      const price = parseFloat(quad.object.value);
      if (price < 0) {
        throw new Error('Price must be positive');
      }
    }
  }
});
```

**When this runs:** Before any transaction that adds a `schema:price` triple.

---

## Hook Types

### 1. Pre-Transaction Hooks

Run **before** a transaction commits. Use for validation, enrichment, and blocking operations:

```typescript
defineKnowledgeHook({
  id: 'pre-transaction-example',
  type: 'pre-transaction',  // ✅ Runs before commit

  predicate: (delta) => true,  // Always trigger

  effect: async (delta, context) => {
    // ✅ Can throw to abort transaction
    if (invalidData(delta)) {
      throw new Error('Invalid data - transaction aborted');
    }

    // ✅ Can modify data before commit
    await context.insert([...enrichedQuads]);
  }
});
```

**Use cases:**
- Validation (block invalid data)
- Data enrichment (add computed values)
- Access control (check permissions)
- Normalization (fix data formats)

### 2. Post-Transaction Hooks

Run **after** a transaction commits. Use for notifications, caching, and side effects:

```typescript
defineKnowledgeHook({
  id: 'post-transaction-example',
  type: 'post-transaction',  // ✅ Runs after commit

  predicate: (delta) => true,

  effect: async (delta, context) => {
    // ✅ Transaction already committed - can't abort
    // ✅ Perfect for side effects and notifications

    await context.emit('data-changed', delta);
    await invalidateCache(delta);
    await notifySubscribers(delta);
  }
});
```

**Use cases:**
- Real-time notifications
- Cache invalidation
- Analytics/logging
- Triggering external APIs
- Updating derived data

---

## Predicates: When to Trigger

Predicates determine **when** a hook should execute:

### Pattern 1: Match Specific Predicate

```typescript
predicate: (delta) => {
  return delta.added.some(quad =>
    quad.predicate.value === 'http://schema.org/price'
  );
}
```

### Pattern 2: Match Subject Pattern

```typescript
predicate: (delta) => {
  return delta.added.some(quad =>
    quad.subject.value.startsWith('http://example.org/product/')
  );
}
```

### Pattern 3: Match Object Value

```typescript
predicate: (delta) => {
  return delta.added.some(quad =>
    quad.object.value === 'urgent' &&
    quad.predicate.value === 'http://example.org/priority'
  );
}
```

### Pattern 4: Threshold-Based

```typescript
predicate: (delta) => {
  // Trigger only if > 10 quads added
  return delta.added.length > 10;
}
```

### Pattern 5: SPARQL ASK Query

```typescript
import { askQuery } from 'unrdf/hooks/predicates';

predicate: askQuery(`
  ASK WHERE {
    ?product schema:price ?price .
    FILTER(?price < 0)
  }
`)
```

### Pattern 6: SHACL Validation

```typescript
import { matchSHACL } from 'unrdf/hooks/predicates';

predicate: matchSHACL(shaclShapesGraph)
```

---

## Effects: What to Do

Effects define **what** happens when a predicate matches:

### Pattern 1: Validation

```typescript
effect: async (delta, context) => {
  for (const quad of delta.added) {
    if (quad.predicate.value === 'http://schema.org/email') {
      const email = quad.object.value;
      if (!isValidEmail(email)) {
        throw new Error(`Invalid email: ${email}`);
      }
    }
  }
}
```

### Pattern 2: Data Enrichment

```typescript
effect: async (delta, context) => {
  for (const quad of delta.added) {
    if (quad.predicate.value === 'http://schema.org/name') {
      const subject = quad.subject;

      // Add computed slug
      const slug = slugify(quad.object.value);
      await context.insert([
        quad(subject, namedNode('http://example.org/slug'), literal(slug))
      ]);

      // Add timestamp
      await context.insert([
        quad(subject, namedNode('http://schema.org/dateModified'), literal(new Date().toISOString()))
      ]);
    }
  }
}
```

### Pattern 3: Real-time Notifications

```typescript
effect: async (delta, context) => {
  for (const quad of delta.added) {
    if (quad.predicate.value === 'http://schema.org/price') {
      const productId = quad.subject.value;
      const newPrice = parseFloat(quad.object.value);

      // Broadcast to all connected clients
      await context.emit('product-price-update', {
        productId,
        price: newPrice,
        timestamp: new Date().toISOString()
      });
    }
  }
}
```

### Pattern 4: External API Calls

```typescript
effect: async (delta, context) => {
  for (const quad of delta.added) {
    if (quad.predicate.value === 'http://example.org/orderPlaced') {
      const orderId = quad.subject.value;

      // Call external API
      await fetch('https://api.example.com/orders', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ orderId })
      });
    }
  }
}
```

### Pattern 5: Derived Data

```typescript
effect: async (delta, context) => {
  // Recompute aggregate when items change
  const productId = delta.added[0].subject.value;

  const result = await context.query(`
    SELECT (AVG(?rating) AS ?avgRating) (COUNT(?rating) AS ?count) WHERE {
      <${productId}> schema:review ?review .
      ?review schema:reviewRating ?rating .
    }
  `);

  const avgRating = result[0].avgRating.value;
  const count = result[0].count.value;

  await context.insert([
    quad(
      namedNode(productId),
      namedNode('http://schema.org/aggregateRating'),
      literal(avgRating)
    ),
    quad(
      namedNode(productId),
      namedNode('http://example.org/reviewCount'),
      literal(count)
    )
  ]);
}
```

---

## Hook Context API

The `context` parameter provides helper methods:

```typescript
interface HookContext {
  // ✅ Knowledge Engine instance
  engine: KnowledgeEngine;

  // ✅ Current transaction
  transaction: Transaction;

  // ✅ Timestamp of change
  timestamp: Date;

  // ✅ User ID (if authenticated)
  userId?: string;

  // ✅ Custom metadata
  metadata?: Record<string, any>;

  // === Helper Methods ===

  // Insert quads in current transaction
  insert(quads: Quad[]): Promise<void>;

  // Delete quads in current transaction
  delete(quads: Quad[]): Promise<void>;

  // Query knowledge graph
  query(sparql: string): Promise<Bindings[]>;

  // Emit event to subscribers
  emit(event: string, data: any): void;

  // Access logger
  log(message: string): void;
  error(message: string, error?: Error): void;
}
```

---

## Real-World Examples

### Example 1: E-commerce Order Processing

```typescript
// Hook 1: Validate order before creation
defineKnowledgeHook({
  id: 'validate-order',
  type: 'pre-transaction',

  predicate: (delta) => {
    return delta.added.some(q =>
      q.predicate.value === 'http://schema.org/orderStatus' &&
      q.object.value === 'pending'
    );
  },

  effect: async (delta, context) => {
    for (const quad of delta.added) {
      const orderId = quad.subject.value;

      // Check if all items are in stock
      const items = await context.query(`
        SELECT ?product ?quantity WHERE {
          <${orderId}> schema:orderedItem ?product .
          ?product schema:quantity ?quantity .
        }
      `);

      for (const item of items) {
        const stock = await checkStock(item.product.value);
        const requested = parseInt(item.quantity.value);

        if (stock < requested) {
          throw new Error(`Insufficient stock for ${item.product.value}`);
        }
      }
    }
  }
});

// Hook 2: Process payment after order creation
defineKnowledgeHook({
  id: 'process-payment',
  type: 'post-transaction',

  predicate: (delta) => {
    return delta.added.some(q =>
      q.predicate.value === 'http://schema.org/orderStatus' &&
      q.object.value === 'pending'
    );
  },

  effect: async (delta, context) => {
    for (const quad of delta.added) {
      const orderId = quad.subject.value;

      // Get payment info
      const payment = await context.query(`
        SELECT ?method ?amount WHERE {
          <${orderId}> schema:paymentMethod ?method ;
                       schema:totalPrice ?amount .
        }
      `);

      // Process payment
      const result = await processPayment({
        method: payment[0].method.value,
        amount: parseFloat(payment[0].amount.value)
      });

      if (result.success) {
        await context.insert([
          quad(
            namedNode(orderId),
            namedNode('http://schema.org/orderStatus'),
            literal('confirmed')
          )
        ]);
      }
    }
  }
});
```

### Example 2: Content Moderation

```typescript
defineKnowledgeHook({
  id: 'moderate-content',
  type: 'post-transaction',

  predicate: (delta) => {
    return delta.added.some(q =>
      q.predicate.value === 'http://schema.org/text'
    );
  },

  effect: async (delta, context) => {
    for (const quad of delta.added) {
      const contentId = quad.subject.value;
      const text = quad.object.value;

      // Call moderation API
      const moderation = await fetch('https://api.openai.com/v1/moderations', {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${process.env.OPENAI_API_KEY}`,
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ input: text })
      }).then(res => res.json());

      const flagged = moderation.results[0].flagged;

      if (flagged) {
        // Flag content for review
        await context.insert([
          quad(
            namedNode(contentId),
            namedNode('http://example.org/moderationStatus'),
            literal('flagged')
          ),
          quad(
            namedNode(contentId),
            namedNode('http://example.org/moderationReason'),
            literal(JSON.stringify(moderation.results[0].categories))
          )
        ]);

        // Notify moderators
        await context.emit('content-flagged', {
          contentId,
          categories: moderation.results[0].categories
        });
      }
    }
  }
});
```

### Example 3: Auto-tagging with AI

```typescript
defineKnowledgeHook({
  id: 'auto-tag-content',
  type: 'post-transaction',

  predicate: (delta) => {
    return delta.added.some(q =>
      q.predicate.value === 'http://schema.org/articleBody'
    );
  },

  effect: async (delta, context) => {
    for (const quad of delta.added) {
      const articleId = quad.subject.value;
      const body = quad.object.value;

      // Extract tags with AI
      const completion = await fetch('https://api.openai.com/v1/chat/completions', {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${process.env.OPENAI_API_KEY}`,
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          model: 'gpt-4',
          messages: [{
            role: 'user',
            content: `Extract 5-10 relevant tags for this article:\n\n${body}`
          }],
          temperature: 0.3
        })
      }).then(res => res.json());

      const tags = completion.choices[0].message.content.split(',').map(t => t.trim());

      // Add tags to article
      for (const tag of tags) {
        await context.insert([
          quad(
            namedNode(articleId),
            namedNode('http://schema.org/keywords'),
            literal(tag)
          )
        ]);
      }
    }
  }
});
```

---

## Performance Optimization

### 1. Batch Operations

```typescript
effect: async (delta, context) => {
  // ❌ Bad: Individual inserts (slow)
  for (const quad of delta.added) {
    await context.insert([enrichQuad(quad)]);
  }

  // ✅ Good: Batch insert (fast)
  const enriched = delta.added.map(enrichQuad);
  await context.insert(enriched);
}
```

### 2. Debounce Expensive Operations

```typescript
import { debounce } from 'lodash';

const debouncedEffect = debounce(async (delta, context) => {
  // Expensive operation only runs once every 5 seconds
  await recomputeAggregates(context);
}, 5000);

defineKnowledgeHook({
  id: 'debounced-hook',
  type: 'post-transaction',
  predicate: (delta) => true,
  effect: debouncedEffect
});
```

### 3. Conditional Execution

```typescript
effect: async (delta, context) => {
  // ✅ Exit early if nothing to do
  const relevantQuads = delta.added.filter(q =>
    q.predicate.value === 'http://schema.org/price'
  );

  if (relevantQuads.length === 0) return;

  // Process only relevant quads
  await processQuads(relevantQuads, context);
}
```

---

## Testing Knowledge Hooks

See [Standard Testing Pattern](../_includes/patterns/testing-pattern.md) for base approach.

**Hook-specific testing:**

```typescript
import { describe, it, expect, beforeEach } from 'vitest';
import { createKnowledgeEngine } from 'unrdf';

describe('Product Price Validation Hook', () => {
  let engine;

  beforeEach(async () => {
    engine = await createKnowledgeEngine();
  });

  it('blocks negative prices', async () => {
    await expect(
      engine.insert([
        quad(
          namedNode('http://example.org/product1'),
          namedNode('http://schema.org/price'),
          literal('-10')
        )
      ])
    ).rejects.toThrow('Price must be positive');
  });

  it('allows valid prices', async () => {
    await expect(
      engine.insert([
        quad(
          namedNode('http://example.org/product1'),
          namedNode('http://schema.org/price'),
          literal('99.99')
        )
      ])
    ).resolves.not.toThrow();
  });
});
```

---

## Debugging Knowledge Hooks

### Enable Debug Logging

```typescript
const engine = await createKnowledgeEngine({
  hooks: {
    debug: true  // ✅ Log all hook executions
  }
});

// Output:
// [Hook] pre-transaction/validate-price triggered (delta: 1 quads)
// [Hook] pre-transaction/validate-price completed (23ms)
```

### Inspect Delta

```typescript
effect: async (delta, context) => {
  context.log(`Delta: ${delta.added.length} added, ${delta.removed.length} removed`);

  for (const quad of delta.added) {
    context.log(`Added: ${quad.subject.value} ${quad.predicate.value} ${quad.object.value}`);
  }
}
```

---

## Next Steps

- **[Innovation 1: Reactive Knowledge Graphs](../innovations/reactive-kg.md)**
- **[Policy Packs & Validation](./policy-validation.md)**
- **[Streaming & Real-time](./streaming.md)**

---

> **🔥 Mastery Tip:** Knowledge Hooks turn UNRDF from a database into a reactive system. Use them liberally!
