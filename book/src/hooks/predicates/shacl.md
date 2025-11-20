# SHACL Predicates

**SHACL predicates** use SHACL (Shapes Constraint Language) shapes to validate RDF data structure and constraints. They're perfect for schema validation and data quality enforcement.

## When to Use SHACL Predicates

Use SHACL predicates for:
- ✅ **Schema validation** - "Does data match the expected shape?"
- ✅ **Structural constraints** - "Required properties, cardinality, datatypes"
- ✅ **Data quality** - "Value ranges, patterns, uniqueness"
- ✅ **Complex validation** - "Multi-property constraints, dependencies"

## Basic Syntax

```javascript
import { defineHook } from 'unrdf';
import { parseTurtle } from 'unrdf';

// Define SHACL shape
const personShape = await parseTurtle(`
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix ex: <http://example.org/> .

  ex:PersonShape
    a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
      sh:path foaf:name ;
      sh:minCount 1 ;
      sh:maxCount 1 ;
      sh:datatype xsd:string ;
    ] ;
    sh:property [
      sh:path foaf:mbox ;
      sh:minCount 1 ;
      sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    ] .
`);

defineHook({
  meta: {
    name: 'person-shape-validation',
    description: 'Validate person data against SHACL shape'
  },
  when: {
    kind: 'shacl',
    shapes: personShape
  },
  run: async (event) => {
    if (!event.result.conforms) {
      const violations = event.result.results.map(r =>
        `${r.path}: ${r.message}`
      ).join(', ');

      throw new Error(`SHACL validation failed: ${violations}`);
    }
  }
});
```

## Event Structure

SHACL predicates provide this structure to the `run` function:

```javascript
{
  result: {
    conforms: false,      // Whether data conforms to shapes
    results: [            // Validation results (if !conforms)
      {
        path: 'foaf:name',
        message: 'Property needs to have at least 1 value',
        severity: 'Violation',
        focusNode: 'http://example.org/alice',
        sourceConstraintComponent: 'MinCountConstraintComponent'
      }
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

### Example 1: Person Profile Validation

Validate complete person profiles:

```javascript
const personProfileShape = await parseTurtle(`
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix ex: <http://example.org/> .
  @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

  ex:PersonProfileShape
    a sh:NodeShape ;
    sh:targetClass foaf:Person ;

    # Name (required, string, 1-100 chars)
    sh:property [
      sh:path foaf:name ;
      sh:minCount 1 ;
      sh:maxCount 1 ;
      sh:datatype xsd:string ;
      sh:minLength 1 ;
      sh:maxLength 100 ;
    ] ;

    # Email (required, valid format)
    sh:property [
      sh:path foaf:mbox ;
      sh:minCount 1 ;
      sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    ] ;

    # Age (optional, 0-150)
    sh:property [
      sh:path ex:age ;
      sh:maxCount 1 ;
      sh:datatype xsd:integer ;
      sh:minInclusive 0 ;
      sh:maxInclusive 150 ;
    ] ;

    # Phone (optional, E.164 format)
    sh:property [
      sh:path foaf:phone ;
      sh:maxCount 1 ;
      sh:pattern "^\\+[1-9]\\d{1,14}$" ;
    ] .
`);

defineHook({
  meta: {
    name: 'person-profile-validation',
    description: 'Validate person profile completeness and correctness'
  },
  when: {
    kind: 'shacl',
    shapes: personProfileShape
  },
  run: async (event) => {
    if (!event.result.conforms) {
      const violations = event.result.results.map(r =>
        `  - ${r.path || 'unknown'}: ${r.message} (focus: ${r.focusNode})`
      ).join('\n');

      throw new Error(
        `Person profile validation failed:\n${violations}`
      );
    }

    return { result: 'valid', violations: 0 };
  }
});
```

### Example 2: Organization Structure Validation

Validate organizational hierarchy:

```javascript
const orgStructureShape = await parseTurtle(`
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix org: <http://www.w3.org/ns/org#> .
  @prefix ex: <http://example.org/> .

  ex:OrganizationShape
    a sh:NodeShape ;
    sh:targetClass org:Organization ;

    # Every organization must have a name
    sh:property [
      sh:path org:identifier ;
      sh:minCount 1 ;
      sh:maxCount 1 ;
      sh:datatype xsd:string ;
    ] ;

    # Organizations can have members
    sh:property [
      sh:path org:hasMember ;
      sh:class foaf:Person ;
    ] ;

    # Organizations can have sub-organizations
    sh:property [
      sh:path org:hasSubOrganization ;
      sh:class org:Organization ;
    ] .

  ex:EmployeeShape
    a sh:NodeShape ;
    sh:targetClass ex:Employee ;

    # Every employee must belong to exactly one organization
    sh:property [
      sh:path org:memberOf ;
      sh:minCount 1 ;
      sh:maxCount 1 ;
      sh:class org:Organization ;
    ] ;

    # Every employee must have an employee ID
    sh:property [
      sh:path ex:employeeId ;
      sh:minCount 1 ;
      sh:maxCount 1 ;
      sh:datatype xsd:string ;
      sh:pattern "^EMP\\d{6}$" ;
    ] .
`);

defineHook({
  meta: {
    name: 'org-structure-validation',
    description: 'Validate organizational structure and employee data'
  },
  when: {
    kind: 'shacl',
    shapes: orgStructureShape
  },
  run: async (event) => {
    if (!event.result.conforms) {
      const violations = event.result.results;

      throw new Error(
        `Organization structure validation failed: ${violations.length} violations`
      );
    }
  }
});
```

### Example 3: Product Catalog Validation

Validate product data:

```javascript
const productShape = await parseTurtle(`
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .
  @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

  ex:ProductShape
    a sh:NodeShape ;
    sh:targetClass ex:Product ;

    # Product name
    sh:property [
      sh:path ex:name ;
      sh:minCount 1 ;
      sh:datatype xsd:string ;
      sh:minLength 1 ;
      sh:maxLength 200 ;
    ] ;

    # SKU (unique identifier)
    sh:property [
      sh:path ex:sku ;
      sh:minCount 1 ;
      sh:maxCount 1 ;
      sh:datatype xsd:string ;
      sh:pattern "^[A-Z]{3}-\\d{6}$" ;
    ] ;

    # Price (must be positive)
    sh:property [
      sh:path ex:price ;
      sh:minCount 1 ;
      sh:maxCount 1 ;
      sh:datatype xsd:decimal ;
      sh:minExclusive 0 ;
    ] ;

    # Stock quantity (non-negative integer)
    sh:property [
      sh:path ex:stock ;
      sh:minCount 1 ;
      sh:maxCount 1 ;
      sh:datatype xsd:integer ;
      sh:minInclusive 0 ;
    ] ;

    # Category (must be IRI)
    sh:property [
      sh:path ex:category ;
      sh:minCount 1 ;
      sh:nodeKind sh:IRI ;
    ] .
`);

defineHook({
  meta: {
    name: 'product-catalog-validation',
    description: 'Validate product data for catalog integrity'
  },
  when: {
    kind: 'shacl',
    shapes: productShape
  },
  run: async (event) => {
    if (!event.result.conforms) {
      const violationsByProduct = new Map();

      for (const violation of event.result.results) {
        const product = violation.focusNode;
        if (!violationsByProduct.has(product)) {
          violationsByProduct.set(product, []);
        }
        violationsByProduct.get(product).push(violation);
      }

      const details = Array.from(violationsByProduct.entries())
        .map(([product, violations]) =>
          `${product}:\n${violations.map(v => `  - ${v.path}: ${v.message}`).join('\n')}`
        )
        .join('\n\n');

      throw new Error(
        `Product catalog validation failed:\n${details}`
      );
    }
  }
});
```

## SHACL Constraint Types

### Property Constraints

```turtle
# Cardinality
sh:minCount 1 ;
sh:maxCount 1 ;

# Datatype
sh:datatype xsd:string ;

# Value range
sh:minInclusive 0 ;
sh:maxInclusive 100 ;
sh:minExclusive 0 ;
sh:maxExclusive 100 ;

# String length
sh:minLength 1 ;
sh:maxLength 100 ;

# Pattern matching
sh:pattern "^[A-Z]{3}-\\d{6}$" ;

# Value constraints
sh:in ( "active" "inactive" "pending" ) ;

# Node kind
sh:nodeKind sh:IRI ;
sh:nodeKind sh:Literal ;
sh:nodeKind sh:BlankNode ;

# Class constraint
sh:class foaf:Person ;
```

### Property Pair Constraints

```turtle
# Less than
sh:property [
  sh:path ex:startDate ;
  sh:lessThan ex:endDate ;
] ;

# Less than or equal
sh:property [
  sh:path ex:minAge ;
  sh:lessThanOrEquals ex:maxAge ;
] ;

# Equals
sh:property [
  sh:path ex:primaryEmail ;
  sh:equals ex:contactEmail ;
] ;

# Disjoint
sh:property [
  sh:path ex:firstName ;
  sh:disjoint ex:lastName ;
] ;
```

### Logical Constraints

```turtle
# AND (all must conform)
sh:and (
  ex:PersonShape
  ex:EmployeeShape
) ;

# OR (at least one must conform)
sh:or (
  ex:EmailContactShape
  ex:PhoneContactShape
) ;

# XOR (exactly one must conform)
sh:xone (
  ex:IndividualShape
  ex:OrganizationShape
) ;

# NOT (must not conform)
sh:not ex:BannedUserShape ;
```

### Qualified Value Shapes

```turtle
# At least 2 friends who are also Persons
sh:property [
  sh:path foaf:knows ;
  sh:qualifiedMinCount 2 ;
  sh:qualifiedValueShape [
    sh:class foaf:Person ;
  ] ;
] ;
```

## Advanced Patterns

### Pattern 1: Conditional Validation

```turtle
# Validate email only if person is an employee
ex:ConditionalEmailShape
  a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path ex:role ;
    sh:equals "employee" ;
    sh:property [
      sh:path foaf:mbox ;
      sh:minCount 1 ;
    ] ;
  ] .
```

### Pattern 2: Custom SPARQL Constraints

```turtle
ex:UniqueEmailShape
  a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:sparql [
    sh:message "Email must be unique" ;
    sh:select """
      SELECT $this
      WHERE {
        $this foaf:mbox ?email .
        ?other foaf:mbox ?email .
        FILTER ($this != ?other)
      }
    """ ;
  ] .
```

### Pattern 3: Multi-Language Support

```turtle
ex:MultilingualNameShape
  a sh:NodeShape ;
  sh:targetClass ex:Product ;
  sh:property [
    sh:path ex:name ;
    sh:minCount 1 ;
    sh:uniqueLang true ;
  ] .
```

## Performance Optimization

### Scope Shapes Narrowly

```turtle
# ✅ Good: Specific target
sh:targetClass foaf:Person ;

# ❌ Slow: Validates everything
sh:targetSubjectsOf foaf:name ;
```

### Use Simple Constraints First

```javascript
before: async ({ payload }) => {
  // Quick check before SHACL
  const hasPerson = payload.additions.some(
    q => q.predicate.value === 'rdf:type' &&
         q.object.value === 'foaf:Person'
  );

  if (!hasPerson) {
    return { cancel: true, reason: 'No persons to validate' };
  }

  return payload;
}
```

## Error Handling

### Provide Detailed Error Reports

```javascript
run: async (event) => {
  if (!event.result.conforms) {
    const report = {
      totalViolations: event.result.results.length,
      violations: event.result.results.map(v => ({
        focusNode: v.focusNode,
        path: v.path,
        message: v.message,
        severity: v.severity,
        value: v.value
      }))
    };

    console.error('SHACL Validation Report:', JSON.stringify(report, null, 2));

    throw new Error(
      `SHACL validation failed with ${report.totalViolations} violations`
    );
  }
}
```

## Testing SHACL Predicates

```javascript
import { createDarkMatterCore, defineHook, registerHook, parseTurtle } from 'unrdf';
import { namedNode, quad, literal } from '@rdfjs/data-model';
import { describe, it, expect } from 'vitest';

describe('SHACL Predicate: person-shape-validation', () => {
  it('should reject person without required name', async () => {
    const system = await createDarkMatterCore();

    const shapes = await parseTurtle(`
      @prefix sh: <http://www.w3.org/ns/shacl#> .
      @prefix foaf: <http://xmlns.com/foaf/0.1/> .
      @prefix ex: <http://example.org/> .

      ex:PersonShape
        a sh:NodeShape ;
        sh:targetClass foaf:Person ;
        sh:property [
          sh:path foaf:name ;
          sh:minCount 1 ;
        ] .
    `);

    const hook = defineHook({
      meta: { name: 'person-shape-validation' },
      when: {
        kind: 'shacl',
        shapes
      },
      run: async (event) => {
        if (!event.result.conforms) {
          throw new Error('SHACL validation failed');
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
    ).rejects.toThrow('SHACL validation failed');

    await system.cleanup();
  });
});
```

## Best Practices

### ✅ Do's

```turtle
# ✅ Use descriptive shape names
ex:PersonShape

# ✅ Add helpful messages
sh:message "Name must be between 1 and 100 characters" ;

# ✅ Use appropriate severity
sh:severity sh:Violation ;  # Blocks transaction
sh:severity sh:Warning ;    # Logs warning

# ✅ Scope shapes appropriately
sh:targetClass foaf:Person ;
```

### ❌ Don'ts

```turtle
# ❌ Don't use vague messages
sh:message "Invalid" ;

# ❌ Don't over-constrain
sh:minLength 1 ;
sh:maxLength 10000 ;  # Too restrictive

# ❌ Don't validate everything
# (Use specific target classes)
```

## Next Steps

- **[Custom Predicates](custom.md)** - Build your own predicate types
- **[Effects](../effects.md)** - What hooks can do with validation results
- **[Policy Packs](../policy-packs.md)** - Bundle SHACL shapes with hooks
