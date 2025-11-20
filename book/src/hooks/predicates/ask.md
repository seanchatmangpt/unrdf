# ASK Predicates

**ASK predicates** use SPARQL ASK queries to check for the existence of patterns in the knowledge graph. They return a boolean: `true` if the pattern exists, `false` otherwise.

## When to Use ASK Predicates

Use ASK predicates for:
- ✅ **Existence checks** - "Does a person without a name exist?"
- ✅ **Validation rules** - "Are there any invalid email addresses?"
- ✅ **Constraint checking** - "Is this invariant violated?"
- ✅ **Boolean conditions** - "Is the age below the threshold?"

## Basic Syntax

```javascript
import { defineHook } from 'unrdf';

defineHook({
  meta: {
    name: 'person-name-required',
    description: 'Ensures all persons have names'
  },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      ASK {
        ?person a foaf:Person .
        FILTER NOT EXISTS { ?person foaf:name ?name }
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('Person without name detected');
    }
  }
});
```

**Explanation:**
- Query checks if any `foaf:Person` exists **without** a `foaf:name`
- `event.result` is `true` if such a person exists
- Hook vetoes the transaction by throwing an error

## Event Structure

ASK predicates provide this structure to the `run` function:

```javascript
{
  result: true | false,  // ASK query result
  payload: {
    additions: [...],    // Transaction additions
    removals: [...],     // Transaction removals
    actor: 'user@example.org'
  },
  context: {
    graph: Store,        // RDF graph
    env: {...}           // Environment variables
  },
  name: 'hook-name'
}
```

## Complete Examples

### Example 1: Data Quality - Email Validation

Ensure all email addresses are valid:

```javascript
defineHook({
  meta: {
    name: 'email-validation',
    description: 'Ensure all email addresses are valid format'
  },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      ASK {
        ?person foaf:mbox ?email .
        # Check for invalid email format
        FILTER (!REGEX(STR(?email), "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"))
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('Invalid email address detected');
    }
    return { result: 'valid' };
  }
});
```

### Example 2: Business Rule - Age Restriction

Enforce minimum age requirement:

```javascript
defineHook({
  meta: {
    name: 'age-restriction',
    description: 'Ensure all persons are 18 or older'
  },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX ex: <http://example.org/>

      ASK {
        ?person a foaf:Person ;
                ex:age ?age .
        FILTER (?age < 18)
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('All persons must be 18 or older');
    }
    return { result: 'age-valid' };
  }
});
```

### Example 3: Relationship Validation

Ensure bidirectional friendships:

```javascript
defineHook({
  meta: {
    name: 'symmetric-friendship',
    description: 'Ensure friendships are bidirectional'
  },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      ASK {
        # Check for one-way friendship
        ?person1 foaf:knows ?person2 .
        FILTER NOT EXISTS {
          ?person2 foaf:knows ?person1
        }
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('Friendship must be bidirectional');
    }
    return { result: 'friendship-valid' };
  }
});
```

### Example 4: Data Completeness

Ensure required fields are present:

```javascript
defineHook({
  meta: {
    name: 'person-completeness',
    description: 'Ensure persons have all required fields'
  },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>

      ASK {
        ?person a foaf:Person .

        # Missing required fields
        FILTER NOT EXISTS {
          ?person foaf:name ?name ;
                  foaf:mbox ?email ;
                  ex:age ?age
        }
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('Person profile incomplete: name, email, and age required');
    }
    return { result: 'complete' };
  }
});
```

### Example 5: Cycle Detection

Prevent cycles in hierarchies:

```javascript
defineHook({
  meta: {
    name: 'prevent-cycles',
    description: 'Prevent cycles in organizational hierarchy'
  },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX org: <http://www.w3.org/ns/org#>

      ASK {
        # Check if person is their own ancestor
        ?person org:reportsTo+ ?person
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('Cycle detected in organizational hierarchy');
    }
    return { result: 'no-cycles' };
  }
});
```

### Example 6: Uniqueness Constraint

Ensure unique email addresses:

```javascript
defineHook({
  meta: {
    name: 'unique-emails',
    description: 'Ensure email addresses are unique'
  },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      ASK {
        # Find duplicate emails
        ?person1 foaf:mbox ?email .
        ?person2 foaf:mbox ?email .
        FILTER (?person1 != ?person2)
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('Duplicate email address detected');
    }
    return { result: 'unique' };
  }
});
```

### Example 7: Temporal Constraint

Ensure dates are logical:

```javascript
defineHook({
  meta: {
    name: 'valid-date-range',
    description: 'Ensure end date is after start date'
  },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX ex: <http://example.org/>

      ASK {
        ?event ex:startDate ?start ;
               ex:endDate ?end .
        FILTER (?end < ?start)
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('End date must be after start date');
    }
    return { result: 'valid-dates' };
  }
});
```

## Advanced Patterns

### Pattern 1: Conditional Validation

```javascript
defineHook({
  meta: { name: 'conditional-email-required' },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX ex: <http://example.org/>

      ASK {
        # Email required only for employees
        ?person ex:role "employee" .
        FILTER NOT EXISTS { ?person foaf:mbox ?email }
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('Employees must have email addresses');
    }
  }
});
```

### Pattern 2: Multi-Condition Check

```javascript
defineHook({
  meta: { name: 'multi-condition-validation' },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX ex: <http://example.org/>

      ASK {
        ?person a foaf:Person ;
                ex:age ?age ;
                ex:salary ?salary .

        # Complex validation: age and salary must be consistent
        FILTER (
          (?age < 18 && ?salary > 0) ||
          (?age >= 65 && ?salary > 100000)
        )
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('Age and salary combination is invalid');
    }
  }
});
```

### Pattern 3: Aggregation Check

```javascript
defineHook({
  meta: { name: 'team-size-limit' },
  when: {
    kind: 'sparql-ask',
    query: `
      PREFIX org: <http://www.w3.org/ns/org#>

      ASK {
        {
          SELECT ?team (COUNT(?member) AS ?count)
          WHERE {
            ?team a org:Team .
            ?member org:memberOf ?team .
          }
          GROUP BY ?team
        }
        FILTER (?count > 10)
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('Team size exceeds maximum of 10 members');
    }
  }
});
```

## Performance Optimization

### Use LIMIT for Early Termination

```javascript
// ❌ Slow: Scans entire graph
ASK {
  ?person a foaf:Person .
  FILTER NOT EXISTS { ?person foaf:name ?name }
}

// ✅ Fast: Stops at first match
ASK {
  {
    SELECT ?person
    WHERE {
      ?person a foaf:Person .
      FILTER NOT EXISTS { ?person foaf:name ?name }
    }
    LIMIT 1
  }
}
```

### Use Indexes Effectively

```javascript
// ✅ Good: Uses predicate index
ASK {
  ?person foaf:name ?name .  # Indexed predicate first
  ?person a foaf:Person .
}

// ❌ Less efficient: Type filter first
ASK {
  ?person a foaf:Person .    # Broader filter first
  ?person foaf:name ?name .
}
```

### Cache ASK Results

```javascript
const askCache = new Map();

before: async ({ payload }) => {
  const cacheKey = `person-${payload.additions[0]?.subject.value}`;
  if (askCache.has(cacheKey)) {
    return { cancel: true, reason: 'Cached result available' };
  }
  return payload;
}
```

## Error Handling

### Provide Detailed Error Messages

```javascript
run: async (event) => {
  if (event.result === true) {
    // Query to get details
    const details = await select(event.context.graph, `
      SELECT ?person
      WHERE {
        ?person a foaf:Person .
        FILTER NOT EXISTS { ?person foaf:name ?name }
      }
      LIMIT 5
    `);

    const personList = details.map(d => d.person.value).join(', ');
    throw new Error(`Persons without names detected: ${personList}`);
  }
}
```

## Testing ASK Predicates

```javascript
import { createDarkMatterCore, defineHook, registerHook } from 'unrdf';
import { namedNode, quad, literal } from '@rdfjs/data-model';
import { describe, it, expect } from 'vitest';

describe('ASK Predicate: person-name-required', () => {
  it('should reject person without name', async () => {
    const system = await createDarkMatterCore();

    const hook = defineHook({
      meta: { name: 'person-name-required' },
      when: {
        kind: 'sparql-ask',
        query: `
          ASK {
            ?person a <http://xmlns.com/foaf/0.1/Person> .
            FILTER NOT EXISTS { ?person <http://xmlns.com/foaf/0.1/name> ?name }
          }
        `
      },
      run: async (event) => {
        if (event.result === true) {
          throw new Error('Person without name detected');
        }
      }
    });

    await registerHook(hook);

    await expect(
      system.executeTransaction({
        additions: [
          quad(
            namedNode('http://example.org/alice'),
            namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
            namedNode('http://xmlns.com/foaf/0.1/Person')
          )
        ],
        removals: [],
        actor: 'test'
      })
    ).rejects.toThrow('Person without name detected');

    await system.cleanup();
  });

  it('should allow person with name', async () => {
    const system = await createDarkMatterCore();

    // Register hook (same as above)

    await system.executeTransaction({
      additions: [
        quad(
          namedNode('http://example.org/bob'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        ),
        quad(
          namedNode('http://example.org/bob'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Bob')
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
// ✅ Use descriptive hook names
meta: { name: 'person-name-required' }

// ✅ Add comments to complex queries
query: `
  # Check for persons without names
  ASK {
    ?person a foaf:Person .
    FILTER NOT EXISTS { ?person foaf:name ?name }
  }
`

// ✅ Provide helpful error messages
if (event.result === true) {
  throw new Error('Person without name detected. All persons must have foaf:name.');
}

// ✅ Use PREFIX declarations
query: `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  ASK { ... }
`
```

### ❌ Don'ts

```javascript
// ❌ Don't use vague hook names
meta: { name: 'hook1' }

// ❌ Don't write unreadable queries
query: `ASK{?s<http://xmlns.com/foaf/0.1/name>?o}`

// ❌ Don't use generic error messages
if (event.result === true) {
  throw new Error('Validation failed');
}

// ❌ Don't hardcode long URIs
query: `
  ASK {
    ?person a <http://xmlns.com/foaf/0.1/Person> .
  }
`
```

## Next Steps

- **[THRESHOLD Predicates](threshold.md)** - Numeric comparisons and limits
- **[DELTA Predicates](delta.md)** - React to specific changes
- **[SHACL Predicates](shacl.md)** - Shape-based validation
- **[Custom Predicates](custom.md)** - Build your own predicate types
