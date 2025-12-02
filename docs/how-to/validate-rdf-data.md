# How-To: Validate RDF Data

**Problem**: You need to validate RDF graphs against SHACL shapes or custom SPARQL constraints to ensure data quality and conformance.

## Solution

UNRDF provides comprehensive validation through SHACL and SPARQL. Use `validateShacl()` for shape-based validation or custom SPARQL queries for domain-specific checks.

### Basic SHACL Validation

```javascript
import { parseTurtle, validateShacl, formatValidationReport } from 'unrdf';

// Your RDF data
const dataTtl = `
@prefix ex: <http://example.org/> .
@prefix schema: <http://schema.org/> .

ex:alice a schema:Person ;
  schema:name "Alice" ;
  schema:email "alice@example.org" .
`;

// SHACL shapes defining constraints
const shapesTtl = `
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix schema: <http://schema.org/> .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass schema:Person ;
  sh:property [
    sh:path schema:name ;
    sh:minCount 1 ;
    sh:datatype xsd:string ;
  ] ;
  sh:property [
    sh:path schema:email ;
    sh:minCount 1 ;
    sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
  ] .
`;

// Validate
const store = parseTurtle(dataTtl);
const report = await validateShacl(store, shapesTtl);

// Check results
if (report.conforms) {
  console.log('✓ Data is valid');
} else {
  console.log('✗ Validation failed');
  console.log(formatValidationReport(report));
}
```

### Multiple Shape Graphs

For complex validation scenarios, validate against multiple shape graphs:

```javascript
import { validateShaclMultiple, getValidationErrors } from 'unrdf';

const shapes = [
  personShapesTtl,
  organizationShapesTtl,
  addressShapesTtl
];

const report = await validateShaclMultiple(store, shapes);

if (!report.conforms) {
  const errors = getValidationErrors(report);
  errors.forEach(err => {
    console.log(`Violation: ${err.message}`);
    console.log(`  Focus: ${err.focusNode}`);
    console.log(`  Path: ${err.path}`);
  });
}
```

### Custom SPARQL Validation

Use SPARQL queries for domain-specific validation rules:

```javascript
import { query } from 'unrdf';

// Find persons without email addresses
const missingEmails = `
  PREFIX schema: <http://schema.org/>

  SELECT ?person WHERE {
    ?person a schema:Person .
    FILTER NOT EXISTS { ?person schema:email ?email }
  }
`;

const results = query(store, missingEmails);
if (results.length > 0) {
  console.log(`Found ${results.length} persons without emails`);
}

// Validate email format with SPARQL FILTER
const invalidEmails = `
  PREFIX schema: <http://schema.org/>

  SELECT ?person ?email WHERE {
    ?person schema:email ?email .
    FILTER (!REGEX(str(?email), "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"))
  }
`;

const invalid = query(store, invalidEmails);
invalid.forEach(row => {
  console.log(`Invalid email: ${row.email} for ${row.person}`);
});
```

### Knowledge Hook Validation

Integrate validation into Knowledge Hooks for automatic enforcement:

```javascript
import { defineHook, registerHook } from 'unrdf';

const validationHook = defineHook({
  meta: {
    name: 'data-quality-enforcer',
    description: 'Validates all data changes against SHACL shapes'
  },
  channel: {
    graphs: ['*']  // Watch all graphs
  },
  when: { kind: 'transaction' },

  async before(event) {
    // Validate before transaction commits
    const report = await validateShacl(event.store, shapesTtl);

    if (!report.conforms) {
      const errors = getValidationErrors(report);
      throw new Error(`Validation failed: ${errors.length} violations`);
    }

    return { valid: true };
  }
});

registerHook(validationHook);
```

### Transaction-Based Validation

Use `TransactionManager` for atomic validation + updates:

```javascript
import { TransactionManager } from 'unrdf';

const txManager = new TransactionManager();

// Add validation hook
txManager.addHook({
  before: async (event) => {
    const report = await validateShacl(event.store, shapesTtl);
    if (!report.conforms) {
      throw new Error('SHACL validation failed');
    }
  }
});

// Apply transaction with automatic validation
try {
  await txManager.apply(store, {
    additions: [newQuad1, newQuad2],
    removals: [oldQuad]
  });
  console.log('✓ Transaction committed with valid data');
} catch (err) {
  console.error('✗ Transaction rejected:', err.message);
}
```

## Variations

### Severity Levels

Handle warnings vs errors differently:

```javascript
import { getValidationWarnings, hasValidationErrors } from 'unrdf';

const report = await validateShacl(store, shapesTtl);

if (hasValidationErrors(report)) {
  throw new Error('Critical validation errors - cannot proceed');
}

const warnings = getValidationWarnings(report);
if (warnings.length > 0) {
  console.warn(`Data has ${warnings.length} warnings but is acceptable`);
}
```

### Progressive Validation

Validate incrementally during data construction:

```javascript
import { initStore, useGraph } from 'unrdf';

await initStore();
const graph = useGraph();

// Add data
graph.add(quad1, quad2, quad3);

// Validate intermediate state
const report1 = await validateShacl(graph.store, shapesTtl);

// Continue if valid
if (report1.conforms) {
  graph.add(quad4, quad5);
}
```

### Quality Assessment

Use quality utilities for comprehensive checks:

```javascript
import { assessDataQuality, findBrokenLinks, suggestImprovements } from 'unrdf/utils';

// Overall quality score
const quality = assessDataQuality(store);
console.log(`Quality score: ${quality.score}/100`);
console.log(`Completeness: ${quality.completeness}%`);
console.log(`Consistency: ${quality.consistency}%`);

// Find specific issues
const brokenLinks = findBrokenLinks(store);
console.log(`Broken IRIs: ${brokenLinks.length}`);

// Get recommendations
const improvements = suggestImprovements(store);
improvements.forEach(suggestion => {
  console.log(`- ${suggestion.message}`);
});
```

## Related Guides

- [How-To: Query with SPARQL](./query-with-sparql.md) - SPARQL patterns for validation
- [How-To: Create Knowledge Hooks](./create-knowledge-hooks.md) - Automatic validation enforcement
- [How-To: Handle Transactions](./handle-transactions.md) - Atomic validation + updates
- [How-To: Assess Data Quality](./assess-data-quality.md) - Quality metrics and fixes
