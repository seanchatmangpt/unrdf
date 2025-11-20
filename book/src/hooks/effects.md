# Hook Effects

Effects are what Knowledge Hooks **do** when their predicates are satisfied. UNRDF provides a comprehensive effect system with security sandboxing and execution control.

## Effect Types

| Type | Purpose | Example |
|------|---------|---------|
| **Veto** | Reject transactions | Block invalid data |
| **Log** | Record events | Audit trails |
| **Notify** | Send alerts | Email, webhook, Slack |
| **Transform** | Modify data | Normalize, enrich |
| **Assert** | Add knowledge | Inferred triples |
| **External** | Call APIs | Integration |

## Basic Effect Patterns

### Veto Effect

Reject transactions by throwing errors:

```javascript
defineHook({
  meta: { name: 'veto-invalid-age' },
  when: {
    kind: 'sparql-ask',
    query: 'ASK { ?person ex:age ?age . FILTER (?age < 0) }'
  },
  run: async (event) => {
    if (event.result === true) {
      // Veto: throws error, transaction fails
      throw new Error('Age cannot be negative');
    }
  }
});
```

### Log Effect

Record events for audit trails:

```javascript
defineHook({
  meta: { name: 'audit-person-changes' },
  when: {
    kind: 'delta',
    pattern: { predicate: namedNode('rdf:type'), object: namedNode('foaf:Person') }
  },
  run: async (event) => {
    // Log effect: record to audit log
    await auditLog.write({
      timestamp: new Date().toISOString(),
      actor: event.payload.actor,
      action: 'person-change',
      additions: event.delta.additions.length,
      removals: event.delta.removals.length
    });

    console.log(`Audit: Person changed by ${event.payload.actor}`);

    return { result: 'logged' };
  }
});
```

### Notify Effect

Send notifications to external systems:

```javascript
defineHook({
  meta: { name: 'large-transaction-alert' },
  when: {
    kind: 'threshold',
    value: 10000,
    operator: 'gt'
  },
  run: async (event) => {
    if (event.result.satisfied) {
      // Notify effect: send alert
      await fetch('https://api.example.com/alerts', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          type: 'large-transaction',
          value: event.result.value,
          threshold: event.result.threshold,
          timestamp: new Date().toISOString()
        })
      });

      return { result: 'alert-sent' };
    }
  }
});
```

### Transform Effect

Modify data before it's committed:

```javascript
defineHook({
  meta: { name: 'normalize-email' },
  when: {
    kind: 'delta',
    pattern: { predicate: namedNode('foaf:mbox') }
  },
  run: async (event) => {
    const normalized = [];

    for (const quad of event.delta.additions) {
      const email = quad.object.value;
      const normalizedEmail = email.toLowerCase().trim();

      // Transform effect: replace with normalized email
      normalized.push(
        quad(
          quad.subject,
          quad.predicate,
          literal(normalizedEmail)
        )
      );
    }

    return {
      result: 'normalized',
      deltas: {
        removals: event.delta.additions,
        additions: normalized
      }
    };
  }
});
```

### Assert Effect

Add inferred knowledge:

```javascript
defineHook({
  meta: { name: 'infer-reciprocal-friendship' },
  when: {
    kind: 'delta',
    pattern: { predicate: namedNode('foaf:knows') }
  },
  run: async (event) => {
    const assertions = [];

    for (const quad of event.delta.additions) {
      // Assert effect: add reciprocal relationship
      assertions.push(
        quad(
          quad.object, // Reverse: friend knows person back
          quad.predicate,
          quad.subject
        )
      );
    }

    return {
      result: 'reciprocals-added',
      assertions
    };
  }
});
```

## Effect Sandboxing

All hook effects run in **isolated sandboxes** for security:

```javascript
// Sandbox automatically applied
defineHook({
  meta: { name: 'sandboxed-effect' },
  when: { kind: 'delta', pattern: {} },
  run: async (event) => {
    // ✅ Allowed: Graph access
    const graph = event.context.graph;

    // ✅ Allowed: Environment variables
    const apiKey = event.context.env.API_KEY;

    // ✅ Allowed: Safe APIs
    await fetch('https://api.example.com/webhook');

    // ❌ Blocked: Filesystem access
    // await fs.readFile('./secret.txt'); // Error!

    // ❌ Blocked: Process manipulation
    // process.exit(1); // Error!

    // ❌ Blocked: Arbitrary code execution
    // eval('malicious code'); // Error!
  }
});
```

### Sandbox Configuration

Control sandbox restrictions:

```javascript
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore({
  hooks: {
    sandbox: {
      // Allow network access
      allowNetwork: true,

      // Allow specific domains only
      allowedDomains: ['api.example.com', 'webhook.slack.com'],

      // Timeout for effects
      timeout: 30000, // 30 seconds

      // Memory limit
      maxMemory: '128MB',

      // CPU time limit
      maxCpuTime: 10000 // 10 seconds
    }
  }
});
```

## Effect Execution Order

Effects execute in this order:

```
1. Before phase (all hooks)
2. Condition evaluation (all hooks)
3. Run phase (all hooks, parallel by default)
4. After phase (all hooks)
```

### Sequential Execution

Force hooks to run sequentially:

```javascript
defineHook({
  meta: {
    name: 'sequential-effect',
    priority: 1, // Lower priority runs first
    sequential: true
  },
  when: { kind: 'delta', pattern: {} },
  run: async (event) => {
    // Runs sequentially with other sequential hooks
  }
});
```

### Dependency Management

Specify hook dependencies:

```javascript
defineHook({
  meta: {
    name: 'dependent-effect',
    dependencies: ['prerequisite-hook']
  },
  when: { kind: 'delta', pattern: {} },
  run: async (event) => {
    // Runs after 'prerequisite-hook' completes
  }
});
```

## Complete Effect Examples

### Example 1: Multi-Effect Hook

Combine multiple effects:

```javascript
defineHook({
  meta: { name: 'comprehensive-person-hook' },
  when: {
    kind: 'delta',
    pattern: { predicate: namedNode('rdf:type'), object: namedNode('foaf:Person') }
  },

  async before({ payload }) {
    // Effect 1: Validate
    if (!payload.actor) {
      return { cancel: true, reason: 'Actor required' };
    }
    return payload;
  },

  async run(event) {
    const additions = event.delta.additions;

    // Effect 2: Log
    await auditLog.write({
      action: 'person-added',
      count: additions.length,
      actor: event.payload.actor
    });

    // Effect 3: Transform (add timestamps)
    const timestamped = additions.map(quad =>
      quad(
        quad.subject,
        namedNode('ex:createdAt'),
        literal(new Date().toISOString())
      )
    );

    // Effect 4: Assert (infer role)
    const roleAssertions = additions.map(quad =>
      quad(
        quad.subject,
        namedNode('ex:role'),
        literal('user')
      )
    );

    // Effect 5: Notify
    await fetch('https://api.example.com/webhooks/new-person', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        count: additions.length,
        actor: event.payload.actor
      })
    });

    return {
      result: 'person-added',
      assertions: [...timestamped, ...roleAssertions]
    };
  },

  async after({ result, cancelled }) {
    // Effect 6: Metrics
    if (!cancelled) {
      await metrics.increment('person.added');
    }

    return { status: 'completed' };
  }
});
```

### Example 2: Conditional Effects

Apply effects based on conditions:

```javascript
defineHook({
  meta: { name: 'conditional-pricing' },
  when: {
    kind: 'delta',
    pattern: { predicate: namedNode('ex:price') }
  },
  run: async (event) => {
    const effects = [];

    for (const quad of event.delta.additions) {
      const price = parseFloat(quad.object.value);

      // Conditional effect: different actions for different price ranges
      if (price > 10000) {
        // High-value: require approval
        effects.push({
          type: 'assertion',
          quad: quad(
            quad.subject,
            namedNode('ex:requiresApproval'),
            literal('true')
          )
        });

        // High-value: notify management
        await sendNotification({
          to: 'management@example.com',
          subject: 'High-value product',
          price
        });
      } else if (price > 1000) {
        // Medium-value: flag for review
        effects.push({
          type: 'assertion',
          quad: quad(
            quad.subject,
            namedNode('ex:flagged'),
            literal('review')
          )
        });
      }
      // Low-value: no special handling
    }

    const assertions = effects
      .filter(e => e.type === 'assertion')
      .map(e => e.quad);

    return {
      result: 'pricing-processed',
      assertions
    };
  }
});
```

### Example 3: Error Recovery Effect

Graceful degradation:

```javascript
defineHook({
  meta: { name: 'resilient-notification' },
  when: {
    kind: 'delta',
    pattern: { predicate: namedNode('ex:status'), object: literal('published') }
  },
  run: async (event) => {
    const notifications = [];

    // Try primary notification channel
    try {
      await sendEmail(event.delta.additions);
      notifications.push({ channel: 'email', status: 'success' });
    } catch (error) {
      console.error('Email notification failed:', error);
      notifications.push({ channel: 'email', status: 'failed' });

      // Fallback to secondary channel
      try {
        await sendSlack(event.delta.additions);
        notifications.push({ channel: 'slack', status: 'success' });
      } catch (fallbackError) {
        console.error('Slack notification failed:', fallbackError);
        notifications.push({ channel: 'slack', status: 'failed' });

        // Last resort: queue for retry
        await retryQueue.add({
          type: 'notification',
          data: event.delta.additions
        });
        notifications.push({ channel: 'queue', status: 'queued' });
      }
    }

    return {
      result: 'notification-attempted',
      notifications
    };
  }
});
```

### Example 4: Batch Effect

Process changes in batches:

```javascript
const batchQueue = [];
let batchTimer = null;

defineHook({
  meta: { name: 'batched-index-update' },
  when: {
    kind: 'delta',
    pattern: { predicate: namedNode('foaf:name') }
  },
  run: async (event) => {
    // Add to batch queue
    batchQueue.push(...event.delta.additions);

    // Clear existing timer
    if (batchTimer) {
      clearTimeout(batchTimer);
    }

    // Schedule batch processing
    batchTimer = setTimeout(async () => {
      if (batchQueue.length === 0) return;

      const batch = [...batchQueue];
      batchQueue.length = 0;

      // Process entire batch at once
      await searchIndex.batchUpdate(batch);

      console.log(`Batch processed: ${batch.length} items`);
    }, 5000); // 5 second delay

    return { result: 'queued-for-batch' };
  }
});
```

## Effect Return Values

Effects can return structured results:

```javascript
return {
  result: any,              // Primary result
  assertions: [Quad],       // RDF quads to add
  deltas: {                 // Complex changes
    additions: [Quad],
    removals: [Quad]
  },
  cancelled: boolean,       // Cancel flag
  reason: string           // Cancellation reason
};
```

## Error Handling in Effects

### Graceful Errors

```javascript
run: async (event) => {
  try {
    await riskyOperation();
    return { result: 'success' };
  } catch (error) {
    console.error('Operation failed:', error);

    // Don't fail transaction, just log
    return {
      result: 'failed',
      error: error.message
    };
  }
}
```

### Critical Errors

```javascript
run: async (event) => {
  try {
    await criticalOperation();
    return { result: 'success' };
  } catch (error) {
    // Fail transaction on critical error
    throw new Error(`Critical failure: ${error.message}`);
  }
}
```

## Performance Optimization

### Async Fire-and-Forget

```javascript
run: async (event) => {
  // Don't await non-critical operations
  sendNotification(event).catch(console.error);

  // Continue immediately
  return { result: 'processing' };
}
```

### Parallel Effects

```javascript
run: async (event) => {
  // Run multiple effects in parallel
  const [logged, notified, indexed] = await Promise.all([
    auditLog.write(event),
    sendNotification(event),
    searchIndex.update(event)
  ]);

  return {
    result: 'all-effects-applied',
    logged,
    notified,
    indexed
  };
}
```

## Best Practices

### ✅ Do's

```javascript
// ✅ Use descriptive result objects
return {
  result: 'email-normalized',
  count: normalized.length,
  actor: event.payload.actor
};

// ✅ Handle errors gracefully
try {
  await effect();
} catch (error) {
  console.error('Effect failed:', error);
  return { result: 'failed', error: error.message };
}

// ✅ Return meaningful data
return {
  result: 'processed',
  assertions: newTriples,
  metadata: { timestamp: new Date().toISOString() }
};

// ✅ Use appropriate effect types
// Veto for validation
// Log for audit
// Notify for alerts
// Transform for data modification
```

### ❌ Don'ts

```javascript
// ❌ Don't return just strings
return 'ok';

// ❌ Don't swallow errors silently
try {
  await effect();
} catch (error) {
  // Silent failure!
}

// ❌ Don't block on slow operations
await slowExternalAPI(); // Consider async

// ❌ Don't mutate event object
event.payload.modified = true; // Readonly!
```

## Next Steps

- **[Policy Packs](policy-packs.md)** - Organize hooks and effects into packages
- **[Lifecycle](lifecycle.md)** - Understand hook execution phases
- **[Predicates](predicates.md)** - Trigger conditions for effects
