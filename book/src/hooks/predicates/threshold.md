# THRESHOLD Predicates

**THRESHOLD predicates** evaluate numeric conditions against values in your knowledge graph. They're perfect for enforcing limits, quotas, and business rules based on quantities.

## When to Use THRESHOLD Predicates

Use THRESHOLD predicates for:
- ✅ **Numeric limits** - "Transaction amount exceeds $10,000"
- ✅ **Quotas** - "Storage usage above 90%"
- ✅ **Business rules** - "Discount only if purchases > 100"
- ✅ **Monitoring** - "CPU usage above threshold"

## Basic Syntax

```javascript
import { defineHook } from 'unrdf';

defineHook({
  meta: {
    name: 'large-transaction-alert',
    description: 'Alert on transactions exceeding $10,000'
  },
  when: {
    kind: 'threshold',
    value: 10000,
    operator: 'gt', // greater than
    query: `
      SELECT (SUM(?amount) AS ?total)
      WHERE {
        ?transaction ex:amount ?amount
      }
    `
  },
  run: async (event) => {
    if (event.result.satisfied) {
      console.log(`Large transaction alert: $${event.result.value}`);
    }
  }
});
```

## Operators

THRESHOLD predicates support these comparison operators:

| Operator | Meaning | Example |
|----------|---------|---------|
| `gt` | Greater than (`>`) | `value > 1000` |
| `gte` | Greater than or equal (`>=`) | `value >= 1000` |
| `lt` | Less than (`<`) | `value < 100` |
| `lte` | Less than or equal (`<=`) | `value <= 100` |
| `eq` | Equal (`==`) | `value == 42` |
| `neq` | Not equal (`!=`) | `value != 0` |

## Event Structure

THRESHOLD predicates provide this structure to the `run` function:

```javascript
{
  result: {
    satisfied: true,     // Whether threshold condition is met
    value: 15000,        // Actual value
    threshold: 10000,    // Configured threshold
    operator: 'gt'       // Comparison operator
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

### Example 1: Transaction Limit

Reject transactions exceeding a limit:

```javascript
defineHook({
  meta: {
    name: 'transaction-limit',
    description: 'Reject transactions over $50,000'
  },
  when: {
    kind: 'threshold',
    value: 50000,
    operator: 'gt',
    query: `
      PREFIX ex: <http://example.org/>

      SELECT (SUM(?amount) AS ?total)
      WHERE {
        ?transaction ex:amount ?amount
      }
    `
  },
  run: async (event) => {
    if (event.result.satisfied) {
      throw new Error(
        `Transaction exceeds limit: $${event.result.value} > $${event.result.threshold}`
      );
    }
  }
});
```

### Example 2: Storage Quota

Alert when storage usage exceeds quota:

```javascript
defineHook({
  meta: {
    name: 'storage-quota-alert',
    description: 'Alert when storage exceeds 90% capacity'
  },
  when: {
    kind: 'threshold',
    value: 0.9, // 90%
    operator: 'gte',
    query: `
      PREFIX ex: <http://example.org/>

      SELECT (?used / ?total AS ?usage)
      WHERE {
        ex:system ex:storageUsed ?used ;
                  ex:storageTotal ?total
      }
    `
  },
  run: async (event) => {
    if (event.result.satisfied) {
      const percentage = (event.result.value * 100).toFixed(1);
      await sendAlert(`Storage at ${percentage}% capacity`);
    }
  }
});
```

### Example 3: Minimum Age Requirement

Ensure minimum age:

```javascript
defineHook({
  meta: {
    name: 'minimum-age',
    description: 'Ensure persons are at least 18 years old'
  },
  when: {
    kind: 'threshold',
    value: 18,
    operator: 'lt',
    query: `
      PREFIX ex: <http://example.org/>

      SELECT (MIN(?age) AS ?minAge)
      WHERE {
        ?person a foaf:Person ;
                ex:age ?age
      }
    `
  },
  run: async (event) => {
    if (event.result.satisfied) {
      throw new Error(
        `Minimum age not met: ${event.result.value} < ${event.result.threshold}`
      );
    }
  }
});
```

### Example 4: Team Size Limit

Enforce maximum team size:

```javascript
defineHook({
  meta: {
    name: 'max-team-size',
    description: 'Limit teams to 20 members'
  },
  when: {
    kind: 'threshold',
    value: 20,
    operator: 'gt',
    query: `
      PREFIX org: <http://www.w3.org/ns/org#>

      SELECT (COUNT(?member) AS ?count)
      WHERE {
        ?team a org:Team .
        ?member org:memberOf ?team
      }
    `
  },
  run: async (event) => {
    if (event.result.satisfied) {
      throw new Error(
        `Team size limit exceeded: ${event.result.value} members (max ${event.result.threshold})`
      );
    }
  }
});
```

### Example 5: Price Range Validation

Ensure prices are within acceptable range:

```javascript
defineHook({
  meta: {
    name: 'price-range-validation',
    description: 'Ensure all prices are between $0.01 and $999,999.99'
  },
  when: {
    kind: 'threshold',
    value: 0,
    operator: 'lte',
    query: `
      PREFIX ex: <http://example.org/>

      SELECT (MIN(?price) AS ?minPrice)
      WHERE {
        ?product ex:price ?price
      }
    `
  },
  run: async (event) => {
    // Check minimum price
    if (event.result.satisfied) {
      throw new Error('Price must be greater than $0');
    }
  }
});

// Companion hook for maximum price
defineHook({
  meta: {
    name: 'max-price-validation',
    description: 'Ensure prices do not exceed $999,999.99'
  },
  when: {
    kind: 'threshold',
    value: 999999.99,
    operator: 'gt',
    query: `
      PREFIX ex: <http://example.org/>

      SELECT (MAX(?price) AS ?maxPrice)
      WHERE {
        ?product ex:price ?price
      }
    `
  },
  run: async (event) => {
    if (event.result.satisfied) {
      throw new Error(
        `Price exceeds maximum: $${event.result.value} > $${event.result.threshold}`
      );
    }
  }
});
```

## Aggregation Functions

THRESHOLD predicates work with SPARQL aggregation functions:

### COUNT

```javascript
when: {
  kind: 'threshold',
  value: 1000,
  operator: 'gt',
  query: `
    SELECT (COUNT(?person) AS ?count)
    WHERE {
      ?person a foaf:Person
    }
  `
}
```

### SUM

```javascript
when: {
  kind: 'threshold',
  value: 100000,
  operator: 'gte',
  query: `
    SELECT (SUM(?amount) AS ?total)
    WHERE {
      ?transaction ex:amount ?amount
    }
  `
}
```

### AVG

```javascript
when: {
  kind: 'threshold',
  value: 3.5,
  operator: 'lt',
  query: `
    SELECT (AVG(?rating) AS ?avgRating)
    WHERE {
      ?product ex:rating ?rating
    }
  `
}
```

### MIN

```javascript
when: {
  kind: 'threshold',
  value: 18,
  operator: 'lt',
  query: `
    SELECT (MIN(?age) AS ?minAge)
    WHERE {
      ?person ex:age ?age
    }
  `
}
```

### MAX

```javascript
when: {
  kind: 'threshold',
  value: 65,
  operator: 'gt',
  query: `
    SELECT (MAX(?age) AS ?maxAge)
    WHERE {
      ?person ex:age ?age
    }
  `
}
```

## Advanced Patterns

### Pattern 1: Dynamic Thresholds

Use graph data for threshold values:

```javascript
defineHook({
  meta: { name: 'dynamic-quota' },
  when: {
    kind: 'sparql-select',
    query: `
      PREFIX ex: <http://example.org/>

      SELECT ?currentUsage ?limit
      WHERE {
        ex:system ex:currentUsage ?currentUsage ;
                  ex:usageLimit ?limit
      }
    `
  },
  run: async (event) => {
    const { currentUsage, limit } = event.result[0];

    if (currentUsage.value > limit.value) {
      throw new Error(
        `Usage ${currentUsage.value} exceeds limit ${limit.value}`
      );
    }
  }
});
```

### Pattern 2: Multi-Threshold Alerts

Different actions at different thresholds:

```javascript
defineHook({
  meta: { name: 'multi-level-alerts' },
  when: {
    kind: 'sparql-select',
    query: `
      SELECT (SUM(?amount) AS ?total)
      WHERE {
        ?transaction ex:amount ?amount
      }
    `
  },
  run: async (event) => {
    const total = event.result[0]?.total?.value || 0;

    if (total > 100000) {
      // Critical threshold
      await sendCriticalAlert(`Critical: Transaction total $${total}`);
      throw new Error('Transaction exceeds critical threshold');
    } else if (total > 50000) {
      // Warning threshold
      await sendWarning(`Warning: Transaction total $${total}`);
    } else if (total > 10000) {
      // Info threshold
      await log(`Info: Large transaction $${total}`);
    }
  }
});
```

### Pattern 3: Percentage-Based Thresholds

Calculate percentages for comparison:

```javascript
defineHook({
  meta: { name: 'percentage-threshold' },
  when: {
    kind: 'threshold',
    value: 0.8, // 80%
    operator: 'gte',
    query: `
      PREFIX ex: <http://example.org/>

      SELECT ((?active / ?total) AS ?percentage)
      WHERE {
        {
          SELECT (COUNT(?active) AS ?active)
          WHERE {
            ?person a foaf:Person ;
                    ex:status "active"
          }
        }
        {
          SELECT (COUNT(?person) AS ?total)
          WHERE {
            ?person a foaf:Person
          }
        }
      }
    `
  },
  run: async (event) => {
    if (event.result.satisfied) {
      const pct = (event.result.value * 100).toFixed(1);
      console.log(`Active users at ${pct}%`);
    }
  }
});
```

## Time-Series Monitoring

Monitor values over time:

```javascript
defineHook({
  meta: { name: 'cpu-usage-monitor' },
  when: {
    kind: 'threshold',
    value: 80, // 80% CPU
    operator: 'gt',
    query: `
      PREFIX ex: <http://example.org/>

      SELECT (AVG(?cpu) AS ?avgCpu)
      WHERE {
        ?measurement ex:timestamp ?time ;
                     ex:cpuUsage ?cpu .

        # Last 5 minutes
        FILTER (?time > NOW() - "PT5M"^^xsd:duration)
      }
    `
  },
  run: async (event) => {
    if (event.result.satisfied) {
      await sendAlert(
        `High CPU usage: ${event.result.value.toFixed(1)}% (5min avg)`
      );
    }
  }
});
```

## Performance Optimization

### Use Efficient Aggregations

```javascript
// ✅ Good: Single aggregation
SELECT (COUNT(?person) AS ?count)
WHERE {
  ?person a foaf:Person
}

// ❌ Slow: Multiple passes
SELECT ?person
WHERE {
  ?person a foaf:Person
}
# Then count in application code
```

### Cache Threshold Queries

```javascript
const thresholdCache = new Map();
const CACHE_TTL = 60000; // 60 seconds

before: async ({ payload }) => {
  const cacheKey = 'transaction-total';
  const cached = thresholdCache.get(cacheKey);

  if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
    // Use cached value, skip SPARQL query
    return {
      cancel: true,
      reason: 'Using cached threshold result'
    };
  }

  return payload;
}
```

### Use Incremental Calculations

```javascript
// Track running total instead of recalculating
let runningTotal = 0;

before: async ({ payload }) => {
  // Update running total
  for (const quad of payload.additions) {
    if (quad.predicate.value === 'ex:amount') {
      runningTotal += parseFloat(quad.object.value);
    }
  }

  return { ...payload, runningTotal };
},

run: async (event) => {
  if (event.payload.runningTotal > 10000) {
    throw new Error('Total exceeds threshold');
  }
}
```

## Error Handling

### Provide Context in Error Messages

```javascript
run: async (event) => {
  if (event.result.satisfied) {
    throw new Error(
      `Threshold exceeded: ${event.result.value} ${event.result.operator} ${event.result.threshold}\n` +
      `Transaction by: ${event.payload.actor}\n` +
      `Timestamp: ${new Date().toISOString()}`
    );
  }
}
```

### Handle Missing Values

```javascript
run: async (event) => {
  // SPARQL might return undefined for aggregations with no data
  const value = event.result[0]?.total?.value;

  if (value === undefined) {
    console.warn('No data available for threshold check');
    return { result: 'skipped' };
  }

  if (value > 10000) {
    throw new Error(`Threshold exceeded: ${value}`);
  }
}
```

## Testing THRESHOLD Predicates

```javascript
import { createDarkMatterCore, defineHook, registerHook } from 'unrdf';
import { namedNode, quad, literal } from '@rdfjs/data-model';
import { describe, it, expect } from 'vitest';

describe('THRESHOLD Predicate: transaction-limit', () => {
  it('should reject transactions over $10,000', async () => {
    const system = await createDarkMatterCore();

    const hook = defineHook({
      meta: { name: 'transaction-limit' },
      when: {
        kind: 'threshold',
        value: 10000,
        operator: 'gt',
        query: `
          SELECT (SUM(?amount) AS ?total)
          WHERE {
            ?transaction <http://example.org/amount> ?amount
          }
        `
      },
      run: async (event) => {
        if (event.result.satisfied) {
          throw new Error('Transaction exceeds limit');
        }
      }
    });

    await registerHook(hook);

    await expect(
      system.executeTransaction({
        additions: [
          quad(
            namedNode('http://example.org/tx1'),
            namedNode('http://example.org/amount'),
            literal('15000', namedNode('http://www.w3.org/2001/XMLSchema#decimal'))
          )
        ],
        removals: [],
        actor: 'test'
      })
    ).rejects.toThrow('Transaction exceeds limit');

    await system.cleanup();
  });

  it('should allow transactions under $10,000', async () => {
    const system = await createDarkMatterCore();
    // ... (register hook as above)

    await system.executeTransaction({
      additions: [
        quad(
          namedNode('http://example.org/tx2'),
          namedNode('http://example.org/amount'),
          literal('5000', namedNode('http://www.w3.org/2001/XMLSchema#decimal'))
        )
      ],
      removals: [],
      actor: 'test'
    });

    // Should not throw
    await system.cleanup();
  });
});
```

## Best Practices

### ✅ Do's

```javascript
// ✅ Use meaningful threshold values
value: 10000  // Clear business meaning

// ✅ Document why threshold exists
meta: {
  name: 'transaction-limit',
  description: 'Enforce regulatory limit of $10,000 per transaction'
}

// ✅ Provide detailed error messages
throw new Error(
  `Transaction $${event.result.value} exceeds limit $${event.result.threshold}`
);

// ✅ Use appropriate operators
operator: 'gt'  // Explicit and correct
```

### ❌ Don'ts

```javascript
// ❌ Don't use magic numbers
value: 42  // What does this mean?

// ❌ Don't use vague error messages
throw new Error('Limit exceeded');

// ❌ Don't forget to handle edge cases
// Missing check for undefined values

// ❌ Don't use wrong operators
operator: '>'  // Should be 'gt'
```

## Next Steps

- **[DELTA Predicates](delta.md)** - React to specific changes in the graph
- **[SHACL Predicates](shacl.md)** - Shape-based validation
- **[Custom Predicates](custom.md)** - Build your own predicate types
