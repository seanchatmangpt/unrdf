# DELTA Predicates

**DELTA predicates** detect specific changes in your knowledge graph by monitoring additions and removals. They're perfect for reactive behavior and change tracking.

## When to Use DELTA Predicates

Use DELTA predicates for:
- ✅ **Change tracking** - "A new person was added"
- ✅ **Reactive updates** - "When price changes, update cache"
- ✅ **Audit logging** - "Record all relationship changes"
- ✅ **Event sourcing** - "Trigger workflow on status change"

## Basic Syntax

```javascript
import { defineHook } from 'unrdf';
import { namedNode } from '@rdfjs/data-model';

defineHook({
  meta: {
    name: 'track-person-changes',
    description: 'Log when persons are added or modified'
  },
  when: {
    kind: 'delta',
    pattern: {
      predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: namedNode('http://xmlns.com/foaf/0.1/Person')
    }
  },
  run: async (event) => {
    console.log(`Person change detected:`);
    console.log(`  Additions: ${event.delta.additions.length}`);
    console.log(`  Removals: ${event.delta.removals.length}`);
  }
});
```

## Pattern Matching

DELTA predicates match quads using patterns. You can specify any combination of subject, predicate, object, or graph:

```javascript
// Match specific predicate
when: {
  kind: 'delta',
  pattern: {
    predicate: namedNode('foaf:name')
  }
}

// Match specific subject and predicate
when: {
  kind: 'delta',
  pattern: {
    subject: namedNode('http://example.org/alice'),
    predicate: namedNode('foaf:knows')
  }
}

// Match all changes (empty pattern)
when: {
  kind: 'delta',
  pattern: {}
}
```

## Event Structure

DELTA predicates provide this structure to the `run` function:

```javascript
{
  delta: {
    additions: [
      /* RDF quads that were added */
    ],
    removals: [
      /* RDF quads that were removed */
    ]
  },
  payload: {
    additions: [...],
    removals: [...],
    actor: 'user@example.org'
  },
  context: {
    graph: Store,
    env: {...}
  },
  name: 'hook-name'
}
```

## Complete Examples

### Example 1: Audit Logging

Log all person changes:

```javascript
defineHook({
  meta: {
    name: 'audit-person-changes',
    description: 'Log all person additions and modifications'
  },
  when: {
    kind: 'delta',
    pattern: {
      predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: namedNode('http://xmlns.com/foaf/0.1/Person')
    }
  },
  run: async (event) => {
    const auditEntry = {
      timestamp: new Date().toISOString(),
      actor: event.payload.actor,
      additions: event.delta.additions.map(q => ({
        subject: q.subject.value,
        predicate: q.predicate.value,
        object: q.object.value
      })),
      removals: event.delta.removals.map(q => ({
        subject: q.subject.value,
        predicate: q.predicate.value,
        object: q.object.value
      }))
    };

    await auditLog.write(auditEntry);
    console.log(`Audit entry created for ${event.payload.actor}`);
  }
});
```

### Example 2: Cache Invalidation

Clear cache when data changes:

```javascript
defineHook({
  meta: {
    name: 'invalidate-person-cache',
    description: 'Clear cache when person data changes'
  },
  when: {
    kind: 'delta',
    pattern: {
      predicate: namedNode('foaf:name')
    }
  },
  run: async (event) => {
    // Extract affected persons
    const affectedPersons = new Set();

    for (const quad of [...event.delta.additions, ...event.delta.removals]) {
      affectedPersons.add(quad.subject.value);
    }

    // Invalidate cache for each person
    for (const personIri of affectedPersons) {
      await cache.delete(`person:${personIri}`);
    }

    console.log(`Cache invalidated for ${affectedPersons.size} persons`);
  }
});
```

### Example 3: Auto-Indexing

Automatically update search index:

```javascript
defineHook({
  meta: {
    name: 'update-search-index',
    description: 'Update search index when person names change'
  },
  when: {
    kind: 'delta',
    pattern: {
      predicate: namedNode('foaf:name')
    }
  },
  run: async (event) => {
    // Index new names
    for (const quad of event.delta.additions) {
      await searchIndex.add({
        iri: quad.subject.value,
        name: quad.object.value,
        type: 'person'
      });
    }

    // Remove old names
    for (const quad of event.delta.removals) {
      await searchIndex.remove(quad.subject.value);
    }

    return { indexed: event.delta.additions.length };
  }
});
```

### Example 4: Relationship Validation

Ensure bidirectional relationships:

```javascript
defineHook({
  meta: {
    name: 'symmetric-friendship',
    description: 'Automatically create reciprocal friendships'
  },
  when: {
    kind: 'delta',
    pattern: {
      predicate: namedNode('foaf:knows')
    }
  },
  run: async (event) => {
    const reciprocalTriples = [];

    // For each new friendship, create reciprocal
    for (const quad of event.delta.additions) {
      const person1 = quad.subject;
      const person2 = quad.object;

      // Check if reciprocal exists
      const hasReciprocal = event.context.graph.getQuads(
        person2,
        quad.predicate,
        person1,
        null
      ).length > 0;

      if (!hasReciprocal) {
        reciprocalTriples.push(
          quad(person2, quad.predicate, person1)
        );
      }
    }

    return {
      result: 'reciprocals-added',
      assertions: reciprocalTriples
    };
  }
});
```

### Example 5: Status Change Notifications

Send notifications on status changes:

```javascript
defineHook({
  meta: {
    name: 'status-change-notification',
    description: 'Notify when user status changes'
  },
  when: {
    kind: 'delta',
    pattern: {
      predicate: namedNode('ex:status')
    }
  },
  run: async (event) => {
    for (const quad of event.delta.additions) {
      const userId = quad.subject.value;
      const newStatus = quad.object.value;

      // Find old status (in removals)
      const oldStatusQuad = event.delta.removals.find(
        q => q.subject.equals(quad.subject) &&
             q.predicate.equals(quad.predicate)
      );

      const oldStatus = oldStatusQuad?.object.value || 'unknown';

      await sendNotification({
        userId,
        message: `Status changed from ${oldStatus} to ${newStatus}`,
        timestamp: new Date().toISOString()
      });
    }
  }
});
```

### Example 6: Workflow Trigger

Trigger workflow on state transitions:

```javascript
defineHook({
  meta: {
    name: 'order-workflow-trigger',
    description: 'Trigger workflow when order status changes to "paid"'
  },
  when: {
    kind: 'delta',
    pattern: {
      predicate: namedNode('ex:orderStatus'),
      object: literal('paid')
    }
  },
  run: async (event) => {
    for (const quad of event.delta.additions) {
      const orderId = quad.subject.value;

      // Trigger fulfillment workflow
      await workflowEngine.start({
        workflow: 'order-fulfillment',
        input: { orderId },
        actor: event.payload.actor
      });

      console.log(`Fulfillment workflow started for order ${orderId}`);
    }
  }
});
```

## Advanced Patterns

### Pattern 1: Hash-Based Change Detection

Track content changes using hashes:

```javascript
import { createHash } from 'crypto';

const contentHashes = new Map();

defineHook({
  meta: { name: 'detect-content-changes' },
  when: {
    kind: 'delta',
    pattern: {
      predicate: namedNode('ex:description')
    }
  },
  run: async (event) => {
    for (const quad of event.delta.additions) {
      const content = quad.object.value;
      const hash = createHash('sha256').update(content).digest('hex');
      const previousHash = contentHashes.get(quad.subject.value);

      if (previousHash && previousHash !== hash) {
        console.log(`Content changed for ${quad.subject.value}`);
        await notifyContentChange(quad.subject.value);
      }

      contentHashes.set(quad.subject.value, hash);
    }
  }
});
```

### Pattern 2: Aggregated Change Notifications

Batch notifications for multiple changes:

```javascript
const changeBatch = new Map();
let batchTimer = null;

defineHook({
  meta: { name: 'batched-notifications' },
  when: {
    kind: 'delta',
    pattern: {
      predicate: namedNode('foaf:name')
    }
  },
  run: async (event) => {
    // Add changes to batch
    for (const quad of event.delta.additions) {
      const existing = changeBatch.get(quad.subject.value) || [];
      existing.push(quad);
      changeBatch.set(quad.subject.value, existing);
    }

    // Schedule batch notification
    if (batchTimer) clearTimeout(batchTimer);

    batchTimer = setTimeout(async () => {
      const changes = Array.from(changeBatch.entries());
      await sendBatchNotification(changes);
      changeBatch.clear();
    }, 5000); // 5 second delay
  }
});
```

### Pattern 3: Conditional Reactions

React only to specific change patterns:

```javascript
defineHook({
  meta: { name: 'price-increase-alert' },
  when: {
    kind: 'delta',
    pattern: {
      predicate: namedNode('ex:price')
    }
  },
  run: async (event) => {
    for (const addition of event.delta.additions) {
      // Find corresponding removal (old price)
      const removal = event.delta.removals.find(
        q => q.subject.equals(addition.subject) &&
             q.predicate.equals(addition.predicate)
      );

      if (removal) {
        const oldPrice = parseFloat(removal.object.value);
        const newPrice = parseFloat(addition.object.value);

        // Alert only on price increases > 10%
        const increase = ((newPrice - oldPrice) / oldPrice) * 100;

        if (increase > 10) {
          await sendAlert(
            `Price increased by ${increase.toFixed(1)}% for ${addition.subject.value}`
          );
        }
      }
    }
  }
});
```

## Monitoring Specific Properties

Track changes to specific predicates:

```javascript
// Monitor email changes
defineHook({
  meta: { name: 'email-change-audit' },
  when: {
    kind: 'delta',
    pattern: { predicate: namedNode('foaf:mbox') }
  },
  run: async (event) => {
    for (const quad of event.delta.additions) {
      await auditLog.write({
        type: 'email-change',
        subject: quad.subject.value,
        newEmail: quad.object.value,
        actor: event.payload.actor,
        timestamp: new Date().toISOString()
      });
    }
  }
});

// Monitor relationship changes
defineHook({
  meta: { name: 'relationship-change-tracker' },
  when: {
    kind: 'delta',
    pattern: { predicate: namedNode('foaf:knows') }
  },
  run: async (event) => {
    console.log(`New relationships: ${event.delta.additions.length}`);
    console.log(`Removed relationships: ${event.delta.removals.length}`);
  }
});
```

## Performance Optimization

### Filter Early

```javascript
before: async ({ payload }) => {
  // Skip if no relevant changes
  const hasPersonChanges = payload.additions.some(
    q => q.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
         q.object.value === 'http://xmlns.com/foaf/0.1/Person'
  );

  if (!hasPersonChanges) {
    return { cancel: true, reason: 'No person changes' };
  }

  return payload;
}
```

### Batch Processing

```javascript
run: async (event) => {
  // Process all changes in one batch
  const changes = event.delta.additions.map(quad => ({
    subject: quad.subject.value,
    predicate: quad.predicate.value,
    object: quad.object.value
  }));

  await database.batchInsert(changes);
}
```

## Testing DELTA Predicates

```javascript
import { createDarkMatterCore, defineHook, registerHook } from 'unrdf';
import { namedNode, quad, literal } from '@rdfjs/data-model';
import { describe, it, expect, vi } from 'vitest';

describe('DELTA Predicate: track-person-changes', () => {
  it('should detect person additions', async () => {
    const system = await createDarkMatterCore();
    const logSpy = vi.spyOn(console, 'log');

    const hook = defineHook({
      meta: { name: 'track-person-changes' },
      when: {
        kind: 'delta',
        pattern: {
          predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          object: namedNode('http://xmlns.com/foaf/0.1/Person')
        }
      },
      run: async (event) => {
        console.log(`Persons added: ${event.delta.additions.length}`);
      }
    });

    await registerHook(hook);

    await system.executeTransaction({
      additions: [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        )
      ],
      removals: [],
      actor: 'test'
    });

    expect(logSpy).toHaveBeenCalledWith('Persons added: 1');

    await system.cleanup();
  });
});
```

## Best Practices

### ✅ Do's

```javascript
// ✅ Be specific with patterns
pattern: {
  predicate: namedNode('foaf:name')
}

// ✅ Process additions and removals
for (const quad of event.delta.additions) { /* ... */ }
for (const quad of event.delta.removals) { /* ... */ }

// ✅ Log meaningful information
console.log(`Person ${quad.subject.value} name changed`);

// ✅ Use batch operations
await database.batchUpdate(changes);
```

### ❌ Don'ts

```javascript
// ❌ Don't use empty patterns unnecessarily
pattern: {} // Matches ALL changes!

// ❌ Don't ignore removals
// Only processing additions can lead to inconsistencies

// ❌ Don't make expensive API calls per change
for (const quad of event.delta.additions) {
  await expensiveAPI(quad); // Slow!
}

// ❌ Don't mutate event object
event.delta.additions.push(...); // Readonly!
```

## Next Steps

- **[SHACL Predicates](shacl.md)** - Shape-based validation
- **[Custom Predicates](custom.md)** - Build your own predicate types
- **[Effects](../effects.md)** - What hooks can do with detected changes
