# Custom Predicates

Build your own predicate types to extend UNRDF's knowledge hook system with domain-specific logic.

## When to Create Custom Predicates

Create custom predicates when:
- ✅ Built-in predicates don't fit your use case
- ✅ You need complex, reusable logic
- ✅ Domain-specific validation is required
- ✅ Integration with external systems is needed

## Predicate Interface

All predicates must implement this interface:

```javascript
{
  kind: string,           // Predicate type identifier
  evaluate: (graph, context) => Promise<any>, // Evaluation function
  isSatisfied: (result) => boolean            // Satisfaction check
}
```

## Creating a Basic Custom Predicate

### Example: Word Count Predicate

Check if text exceeds a word count:

```javascript
function createWordCountPredicate(minWords, maxWords) {
  return {
    kind: 'custom:word-count',

    async evaluate(graph, context) {
      const { select } = await import('unrdf');

      // Query text content
      const results = await select(graph, `
        SELECT ?text
        WHERE {
          ?subject ex:description ?text
        }
      `);

      const totalWords = results.reduce((sum, row) => {
        const text = row.text.value;
        const wordCount = text.trim().split(/\s+/).length;
        return sum + wordCount;
      }, 0);

      return {
        totalWords,
        minWords,
        maxWords,
        satisfied: totalWords >= minWords && totalWords <= maxWords
      };
    },

    isSatisfied(result) {
      return result.satisfied;
    }
  };
}

// Use the custom predicate
defineHook({
  meta: {
    name: 'word-count-validation',
    description: 'Ensure description has 100-500 words'
  },
  when: createWordCountPredicate(100, 500),
  run: async (event) => {
    if (!event.result.satisfied) {
      throw new Error(
        `Word count ${event.result.totalWords} outside range ${event.result.minWords}-${event.result.maxWords}`
      );
    }
  }
});
```

## Complete Examples

### Example 1: Geographic Distance Predicate

Check if locations are within a certain distance:

```javascript
import { distance } from 'geo-distance';

function createGeoDistancePredicate(maxDistanceKm) {
  return {
    kind: 'custom:geo-distance',

    async evaluate(graph, context) {
      const { select } = await import('unrdf');

      // Get all location pairs
      const results = await select(graph, `
        PREFIX geo: <http://www.w3.org/2003/01/geo/wgs84_pos#>

        SELECT ?loc1 ?lat1 ?lon1 ?loc2 ?lat2 ?lon2
        WHERE {
          ?loc1 geo:lat ?lat1 ; geo:long ?lon1 .
          ?loc2 geo:lat ?lat2 ; geo:long ?lon2 .
          FILTER (?loc1 != ?loc2)
        }
      `);

      const violations = [];

      for (const row of results) {
        const dist = distance(
          { lat: row.lat1.value, lon: row.lon1.value },
          { lat: row.lat2.value, lon: row.lon2.value }
        );

        if (dist > maxDistanceKm) {
          violations.push({
            loc1: row.loc1.value,
            loc2: row.loc2.value,
            distance: dist
          });
        }
      }

      return {
        violations,
        satisfied: violations.length === 0
      };
    },

    isSatisfied(result) {
      return result.satisfied;
    }
  };
}

defineHook({
  meta: {
    name: 'geo-proximity-check',
    description: 'Ensure related locations are within 100km'
  },
  when: createGeoDistancePredicate(100),
  run: async (event) => {
    if (!event.result.satisfied) {
      const details = event.result.violations
        .map(v => `${v.loc1} and ${v.loc2}: ${v.distance.toFixed(1)}km`)
        .join('\n');

      throw new Error(
        `Locations exceed 100km limit:\n${details}`
      );
    }
  }
});
```

### Example 2: Time-Based Predicate

Check if events occur within business hours:

```javascript
function createBusinessHoursPredicate() {
  return {
    kind: 'custom:business-hours',

    async evaluate(graph, context) {
      const { select } = await import('unrdf');

      const results = await select(graph, `
        PREFIX ex: <http://example.org/>

        SELECT ?event ?timestamp
        WHERE {
          ?event ex:scheduledAt ?timestamp
        }
      `);

      const violations = [];

      for (const row of results) {
        const timestamp = new Date(row.timestamp.value);
        const hour = timestamp.getHours();
        const day = timestamp.getDay();

        // Business hours: Mon-Fri, 9am-5pm
        if (day === 0 || day === 6 || hour < 9 || hour >= 17) {
          violations.push({
            event: row.event.value,
            timestamp: row.timestamp.value,
            reason: day === 0 || day === 6 ? 'weekend' : 'outside hours'
          });
        }
      }

      return {
        violations,
        satisfied: violations.length === 0
      };
    },

    isSatisfied(result) {
      return result.satisfied;
    }
  };
}

defineHook({
  meta: {
    name: 'business-hours-check',
    description: 'Ensure events are scheduled during business hours'
  },
  when: createBusinessHoursPredicate(),
  run: async (event) => {
    if (!event.result.satisfied) {
      throw new Error(
        `${event.result.violations.length} events scheduled outside business hours`
      );
    }
  }
});
```

### Example 3: External API Validation

Validate data against external service:

```javascript
function createEmailValidationPredicate(apiKey) {
  return {
    kind: 'custom:email-validation',

    async evaluate(graph, context) {
      const { select } = await import('unrdf');

      const results = await select(graph, `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?person ?email
        WHERE {
          ?person foaf:mbox ?email
        }
      `);

      const invalidEmails = [];

      for (const row of results) {
        const email = row.email.value.replace('mailto:', '');

        // Call external email validation API
        const response = await fetch(
          `https://api.emailvalidation.com/verify?email=${email}`,
          { headers: { 'X-API-Key': apiKey } }
        );

        const data = await response.json();

        if (!data.valid) {
          invalidEmails.push({
            person: row.person.value,
            email,
            reason: data.reason
          });
        }
      }

      return {
        invalidEmails,
        satisfied: invalidEmails.length === 0
      };
    },

    isSatisfied(result) {
      return result.satisfied;
    }
  };
}

defineHook({
  meta: {
    name: 'external-email-validation',
    description: 'Validate emails via external API'
  },
  when: createEmailValidationPredicate(process.env.EMAIL_API_KEY),
  run: async (event) => {
    if (!event.result.satisfied) {
      const details = event.result.invalidEmails
        .map(e => `${e.email}: ${e.reason}`)
        .join('\n');

      throw new Error(
        `Invalid emails detected:\n${details}`
      );
    }
  }
});
```

### Example 4: Graph Pattern Matching

Complex pattern matching beyond SPARQL:

```javascript
function createCyclicDependencyPredicate() {
  return {
    kind: 'custom:cyclic-dependency',

    async evaluate(graph, context) {
      const { select } = await import('unrdf');

      // Get all dependencies
      const results = await select(graph, `
        PREFIX ex: <http://example.org/>

        SELECT ?from ?to
        WHERE {
          ?from ex:dependsOn ?to
        }
      `);

      // Build dependency graph
      const dependencies = new Map();

      for (const row of results) {
        const from = row.from.value;
        const to = row.to.value;

        if (!dependencies.has(from)) {
          dependencies.set(from, new Set());
        }
        dependencies.get(from).add(to);
      }

      // Detect cycles using DFS
      const cycles = [];
      const visited = new Set();
      const recStack = new Set();

      function detectCycle(node, path = []) {
        visited.add(node);
        recStack.add(node);
        path.push(node);

        const deps = dependencies.get(node) || new Set();

        for (const dep of deps) {
          if (!visited.has(dep)) {
            if (detectCycle(dep, [...path])) {
              return true;
            }
          } else if (recStack.has(dep)) {
            // Cycle detected
            cycles.push([...path, dep]);
            return true;
          }
        }

        recStack.delete(node);
        return false;
      }

      for (const node of dependencies.keys()) {
        if (!visited.has(node)) {
          detectCycle(node);
        }
      }

      return {
        cycles,
        satisfied: cycles.length === 0
      };
    },

    isSatisfied(result) {
      return result.satisfied;
    }
  };
}

defineHook({
  meta: {
    name: 'prevent-cyclic-dependencies',
    description: 'Detect and prevent cyclic dependencies'
  },
  when: createCyclicDependencyPredicate(),
  run: async (event) => {
    if (!event.result.satisfied) {
      const cycleDetails = event.result.cycles
        .map(cycle => cycle.join(' → '))
        .join('\n');

      throw new Error(
        `Cyclic dependencies detected:\n${cycleDetails}`
      );
    }
  }
});
```

## Advanced Patterns

### Pattern 1: Caching Predicate Results

```javascript
const predicateCache = new Map();

function createCachedPredicate(basePredicate, ttl = 60000) {
  return {
    kind: `cached:${basePredicate.kind}`,

    async evaluate(graph, context) {
      const cacheKey = `${basePredicate.kind}:${graph.size}`;
      const cached = predicateCache.get(cacheKey);

      if (cached && Date.now() - cached.timestamp < ttl) {
        return cached.result;
      }

      const result = await basePredicate.evaluate(graph, context);

      predicateCache.set(cacheKey, {
        result,
        timestamp: Date.now()
      });

      return result;
    },

    isSatisfied(result) {
      return basePredicate.isSatisfied(result);
    }
  };
}

// Use cached predicate
const expensivePredicate = createExpensiveValidationPredicate();
const cached = createCachedPredicate(expensivePredicate, 30000);

defineHook({
  meta: { name: 'cached-validation' },
  when: cached,
  run: async (event) => {
    // Uses cached result if available
  }
});
```

### Pattern 2: Composable Predicates

```javascript
function createComposedPredicate(predicates, operator = 'AND') {
  return {
    kind: 'custom:composed',

    async evaluate(graph, context) {
      const results = await Promise.all(
        predicates.map(p => p.evaluate(graph, context))
      );

      let satisfied;
      if (operator === 'AND') {
        satisfied = results.every(r => predicates[results.indexOf(r)].isSatisfied(r));
      } else if (operator === 'OR') {
        satisfied = results.some(r => predicates[results.indexOf(r)].isSatisfied(r));
      }

      return {
        results,
        operator,
        satisfied
      };
    },

    isSatisfied(result) {
      return result.satisfied;
    }
  };
}

// Compose predicates
const ageCheck = createAgePredicate(18);
const emailCheck = createEmailPredicate();
const combined = createComposedPredicate([ageCheck, emailCheck], 'AND');

defineHook({
  meta: { name: 'combined-validation' },
  when: combined,
  run: async (event) => {
    if (!event.result.satisfied) {
      throw new Error('Validation failed');
    }
  }
});
```

### Pattern 3: Predicate Factory

```javascript
class PredicateFactory {
  static threshold(property, operator, value) {
    return {
      kind: 'custom:threshold',
      async evaluate(graph, context) {
        const { select } = await import('unrdf');

        const results = await select(graph, `
          SELECT (${operator.toUpperCase()}(?value) AS ?result)
          WHERE {
            ?s <${property}> ?value
          }
        `);

        const result = results[0]?.result?.value;
        const passes = this._compare(result, operator, value);

        return { result, operator, value, satisfied: passes };
      },
      isSatisfied(result) {
        return result.satisfied;
      },
      _compare(a, op, b) {
        switch (op) {
          case 'gt': return a > b;
          case 'gte': return a >= b;
          case 'lt': return a < b;
          case 'lte': return a <= b;
          case 'eq': return a === b;
          default: return false;
        }
      }
    };
  }

  static pattern(subject, predicate, object) {
    return {
      kind: 'custom:pattern',
      async evaluate(graph, context) {
        const quads = graph.getQuads(subject, predicate, object, null);
        return {
          matches: quads.length,
          satisfied: quads.length > 0
        };
      },
      isSatisfied(result) {
        return result.satisfied;
      }
    };
  }
}

// Use factory
defineHook({
  meta: { name: 'factory-predicate' },
  when: PredicateFactory.threshold('ex:price', 'gt', 100),
  run: async (event) => {
    // ...
  }
});
```

## Testing Custom Predicates

```javascript
import { describe, it, expect } from 'vitest';
import { Store } from 'n3';
import { namedNode, quad, literal } from '@rdfjs/data-model';

describe('Custom Predicate: word-count', () => {
  it('should detect word count violations', async () => {
    const predicate = createWordCountPredicate(10, 50);

    const graph = new Store([
      quad(
        namedNode('http://example.org/doc1'),
        namedNode('http://example.org/description'),
        literal('This is a very short text')
      )
    ]);

    const result = await predicate.evaluate(graph, {});

    expect(result.totalWords).toBe(6);
    expect(result.satisfied).toBe(false);
  });

  it('should pass for valid word count', async () => {
    const predicate = createWordCountPredicate(5, 50);

    const graph = new Store([
      quad(
        namedNode('http://example.org/doc1'),
        namedNode('http://example.org/description'),
        literal('This is a text with exactly twenty words that should pass the validation check successfully')
      )
    ]);

    const result = await predicate.evaluate(graph, {});

    expect(result.satisfied).toBe(true);
  });
});
```

## Best Practices

### ✅ Do's

```javascript
// ✅ Use descriptive kind identifiers
kind: 'custom:word-count'

// ✅ Handle errors gracefully
async evaluate(graph, context) {
  try {
    // Evaluation logic
  } catch (error) {
    return {
      error: error.message,
      satisfied: false
    };
  }
}

// ✅ Provide detailed results
return {
  totalWords: 42,
  minWords: 10,
  maxWords: 50,
  satisfied: true,
  details: { /* additional info */ }
};

// ✅ Document parameters
/**
 * Creates a word count predicate
 * @param {number} minWords - Minimum word count
 * @param {number} maxWords - Maximum word count
 * @returns {Predicate} Custom predicate
 */
function createWordCountPredicate(minWords, maxWords) {
  // ...
}
```

### ❌ Don'ts

```javascript
// ❌ Don't use generic kind names
kind: 'custom'

// ❌ Don't swallow errors silently
async evaluate(graph, context) {
  try {
    // ...
  } catch (error) {
    return { satisfied: true }; // Wrong!
  }
}

// ❌ Don't return only booleans
return true; // Should return detailed object

// ❌ Don't block on expensive operations
async evaluate(graph, context) {
  await slowExternalAPI(); // Consider caching
}
```

## Integration with Condition Evaluator

Custom predicates integrate seamlessly:

```javascript
// condition-evaluator.mjs will call your predicate
import { createConditionEvaluator } from 'unrdf';

const evaluator = createConditionEvaluator();

// Register custom predicate type
evaluator.registerPredicateType('custom:word-count', createWordCountPredicate);

// Now hooks can use it
defineHook({
  meta: { name: 'word-count-check' },
  when: createWordCountPredicate(100, 500),
  run: async (event) => {
    // ...
  }
});
```

## Performance Optimization

### Use Incremental Evaluation

```javascript
function createIncrementalPredicate() {
  let previousResult = null;

  return {
    kind: 'custom:incremental',

    async evaluate(graph, context) {
      // Only re-evaluate changed data
      if (previousResult && !context.delta) {
        return previousResult;
      }

      const result = await expensiveEvaluation(graph);
      previousResult = result;
      return result;
    },

    isSatisfied(result) {
      return result.satisfied;
    }
  };
}
```

### Parallel Processing

```javascript
async evaluate(graph, context) {
  const [result1, result2, result3] = await Promise.all([
    checkConstraint1(graph),
    checkConstraint2(graph),
    checkConstraint3(graph)
  ]);

  return {
    results: [result1, result2, result3],
    satisfied: result1 && result2 && result3
  };
}
```

## Next Steps

- **[Effects](../effects.md)** - What hooks can do with predicate results
- **[Policy Packs](../policy-packs.md)** - Package custom predicates for reuse
- **[Lifecycle](../lifecycle.md)** - Understand hook execution phases
