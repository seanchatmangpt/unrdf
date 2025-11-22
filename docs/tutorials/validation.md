# Tutorial: SHACL Validation

Learn to define and apply SHACL (Shapes Constraint Language) validation rules with UNRDF.

## Learning Objectives

By the end of this tutorial, you will:

- Write SHACL shapes for data validation
- Apply validation to RDF graphs
- Handle and report validation errors
- Create reusable validation patterns

## Prerequisites

- Completed [Creating RDF Documents](./creating-rdf-documents.md) tutorial
- Basic understanding of RDF schema concepts
- Sample data setup (provided below)

## What is SHACL?

SHACL (Shapes Constraint Language) is a W3C standard for validating RDF graphs. It defines:

- **Shapes** - Templates describing valid data structures
- **Constraints** - Rules that data must satisfy
- **Targets** - Which nodes the shapes apply to

Think of SHACL as "JSON Schema for RDF."

## Step 1: Your First Shape

Create `shapes/person-shape.ttl`:

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <http://example.org/shapes/> .

ex:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
        sh:path foaf:name ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:message "Every person must have exactly one name (string)"
    ] .
```

Apply it:

```javascript
// src/validation-tutorial.mjs
import { readFileSync } from 'node:fs';
import { parseTurtle, validateShacl, formatValidationReport } from 'unrdf/knowledge-engine';

const data = `
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix ex: <http://example.org/> .

  ex:alice a foaf:Person ;
      foaf:name "Alice Smith" .

  ex:bob a foaf:Person .  # Missing name - will fail validation
`;

const shapes = readFileSync('./shapes/person-shape.ttl', 'utf-8');
const store = await parseTurtle(data);
const report = await validateShacl(store, shapes);

if (report.conforms) {
  console.log('Data is valid!');
} else {
  console.log('Validation errors:');
  console.log(formatValidationReport(report));
}
```

## Step 2: Common Constraint Types

### Cardinality Constraints

```turtle
ex:CardinalityExample a sh:PropertyShape ;
    sh:path foaf:name ;
    sh:minCount 1 ;      # At least 1
    sh:maxCount 1 ;      # At most 1
    # Combined: exactly 1
```

### Datatype Constraints

```turtle
ex:DatatypeExample a sh:PropertyShape ;
    sh:path foaf:age ;
    sh:datatype xsd:integer ;
    sh:minInclusive 0 ;
    sh:maxInclusive 150 ;
    sh:message "Age must be an integer between 0 and 150"
```

### String Constraints

```turtle
ex:StringExample a sh:PropertyShape ;
    sh:path foaf:email ;
    sh:datatype xsd:string ;
    sh:minLength 5 ;
    sh:maxLength 100 ;
    sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    sh:message "Must be a valid email address"
```

### Value Constraints

```turtle
ex:ValueExample a sh:PropertyShape ;
    sh:path ex:status ;
    sh:in ( "active" "inactive" "pending" ) ;
    sh:message "Status must be one of: active, inactive, pending"
```

### Class Constraints

```turtle
ex:ClassExample a sh:PropertyShape ;
    sh:path foaf:knows ;
    sh:class foaf:Person ;
    sh:message "Can only know other Person resources"
```

## Step 3: Complete Person Schema

Create `shapes/complete-person.ttl`:

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <http://example.org/> .
@prefix shapes: <http://example.org/shapes/> .

shapes:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;

    # Required: name
    sh:property [
        sh:path foaf:name ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:minLength 1 ;
        sh:maxLength 100 ;
        sh:message "Person must have exactly one name (1-100 characters)"
    ] ;

    # Optional: age (if present, must be valid)
    sh:property [
        sh:path foaf:age ;
        sh:maxCount 1 ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
        sh:message "Age must be an integer between 0 and 150"
    ] ;

    # Optional: email (if present, must be valid format)
    sh:property [
        sh:path foaf:mbox ;
        sh:nodeKind sh:IRI ;
        sh:pattern "^mailto:" ;
        sh:message "Email must be a mailto: IRI"
    ] ;

    # Optional: knows (must reference other People)
    sh:property [
        sh:path foaf:knows ;
        sh:class foaf:Person ;
        sh:message "Can only know other Person resources"
    ] ;

    # Optional: department (must reference a Department)
    sh:property [
        sh:path ex:department ;
        sh:maxCount 1 ;
        sh:class ex:Department ;
        sh:message "Department must be a valid Department resource"
    ] .

shapes:DepartmentShape a sh:NodeShape ;
    sh:targetClass ex:Department ;

    sh:property [
        sh:path ex:name ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:message "Department must have exactly one name"
    ] ;

    sh:property [
        sh:path ex:budget ;
        sh:maxCount 1 ;
        sh:datatype xsd:decimal ;
        sh:minExclusive 0 ;
        sh:message "Budget must be a positive decimal"
    ] .
```

## Step 4: Handling Validation Results

```javascript
import {
  validateShacl,
  hasValidationErrors,
  getValidationErrors,
  getValidationWarnings,
  formatValidationReport
} from 'unrdf/knowledge-engine';

async function validateData(store, shapes) {
  const report = await validateShacl(store, shapes);

  // Quick check
  if (report.conforms) {
    console.log('All data is valid');
    return { valid: true };
  }

  // Detailed analysis
  const errors = getValidationErrors(report);
  const warnings = getValidationWarnings(report);

  console.log(`Found ${errors.length} errors, ${warnings.length} warnings`);

  // Process each error
  errors.forEach((error, index) => {
    console.log(`\nError ${index + 1}:`);
    console.log(`  Focus Node: ${error.focusNode?.value}`);
    console.log(`  Property: ${error.path?.value}`);
    console.log(`  Message: ${error.message}`);
    console.log(`  Severity: ${error.severity?.value || 'Violation'}`);
  });

  return {
    valid: false,
    errorCount: errors.length,
    warningCount: warnings.length,
    details: formatValidationReport(report)
  };
}
```

## Step 5: Severity Levels

SHACL supports three severity levels:

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .

# Violation (default) - Critical errors
shapes:CriticalShape a sh:PropertyShape ;
    sh:path ex:required ;
    sh:minCount 1 ;
    sh:severity sh:Violation ;
    sh:message "CRITICAL: Required field missing"
.

# Warning - Non-critical issues
shapes:WarningShape a sh:PropertyShape ;
    sh:path ex:recommended ;
    sh:minCount 1 ;
    sh:severity sh:Warning ;
    sh:message "WARNING: Recommended field missing"
.

# Info - Suggestions
shapes:InfoShape a sh:PropertyShape ;
    sh:path ex:optional ;
    sh:minCount 1 ;
    sh:severity sh:Info ;
    sh:message "INFO: Consider adding this field"
.
```

Filter by severity:

```javascript
function filterBySeverity(report, severityLevel) {
  const results = report.results || [];
  return results.filter(r =>
    r.severity?.value?.endsWith(severityLevel) ||
    (severityLevel === 'Violation' && !r.severity)
  );
}

const violations = filterBySeverity(report, 'Violation');
const warnings = filterBySeverity(report, 'Warning');
const info = filterBySeverity(report, 'Info');
```

## Step 6: Conditional Constraints

### sh:or - Alternative Shapes

```turtle
# Person must have either email OR phone
shapes:ContactShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:or (
        [ sh:property [ sh:path foaf:mbox ; sh:minCount 1 ] ]
        [ sh:property [ sh:path foaf:phone ; sh:minCount 1 ] ]
    ) ;
    sh:message "Person must have either email or phone"
.
```

### sh:and - Combined Shapes

```turtle
# Must satisfy multiple conditions
shapes:EmployeeShape a sh:NodeShape ;
    sh:targetClass ex:Employee ;
    sh:and (
        shapes:PersonShape
        [ sh:property [ sh:path ex:employeeId ; sh:minCount 1 ] ]
    )
.
```

### sh:not - Negation

```turtle
# Cannot be both manager and intern
shapes:RoleShape a sh:NodeShape ;
    sh:targetClass ex:Employee ;
    sh:not [
        sh:and (
            [ sh:property [ sh:path ex:role ; sh:hasValue "manager" ] ]
            [ sh:property [ sh:path ex:role ; sh:hasValue "intern" ] ]
        )
    ]
.
```

### sh:xone - Exactly One

```turtle
# Exactly one of these roles
shapes:ExclusiveRoleShape a sh:NodeShape ;
    sh:xone (
        [ sh:property [ sh:path ex:role ; sh:hasValue "admin" ] ]
        [ sh:property [ sh:path ex:role ; sh:hasValue "user" ] ]
        [ sh:property [ sh:path ex:role ; sh:hasValue "guest" ] ]
    )
.
```

## Step 7: Qualified Constraints

Apply constraints to a subset of values:

```turtle
# At least 2 friends who are adults
shapes:AdultFriendsShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
        sh:path foaf:knows ;
        sh:qualifiedMinCount 2 ;
        sh:qualifiedValueShape [
            sh:property [
                sh:path foaf:age ;
                sh:minInclusive 18
            ]
        ] ;
        sh:message "Must know at least 2 adults"
    ]
.
```

## Step 8: SPARQL-Based Constraints

For complex validation logic:

```turtle
shapes:UniqueEmailShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:sparql [
        sh:message "Email must be unique across all persons" ;
        sh:prefixes [
            sh:declare [ sh:prefix "foaf" ; sh:namespace "http://xmlns.com/foaf/0.1/"^^xsd:anyURI ]
        ] ;
        sh:select """
            SELECT $this ?other
            WHERE {
                $this foaf:mbox ?email .
                ?other foaf:mbox ?email .
                FILTER ($this != ?other)
            }
        """
    ]
.
```

## Step 9: Validation in Workflows

### Validate Before Insert

```javascript
async function validateAndInsert(newData, existingStore, shapes) {
  // Parse new data
  const newStore = await parseTurtle(newData);

  // Validate new data alone
  const newDataReport = await validateShacl(newStore, shapes);
  if (!newDataReport.conforms) {
    throw new Error('New data is invalid: ' + formatValidationReport(newDataReport));
  }

  // Merge and validate combined
  const mergedStore = new Store([...existingStore, ...newStore]);
  const mergedReport = await validateShacl(mergedStore, shapes);

  if (!mergedReport.conforms) {
    throw new Error('Merged data violates constraints');
  }

  return mergedStore;
}
```

### Continuous Validation

```javascript
class ValidatedStore {
  constructor(shapes) {
    this.store = new Store();
    this.shapes = shapes;
  }

  async add(turtle) {
    const newQuads = await parseTurtle(turtle);
    const tempStore = new Store([...this.store, ...newQuads]);

    const report = await validateShacl(tempStore, this.shapes);
    if (!report.conforms) {
      throw new ValidationError(report);
    }

    // Only add if valid
    for (const quad of newQuads) {
      this.store.addQuad(quad);
    }
  }
}
```

## Step 10: Custom Error Messages

Create informative error messages:

```turtle
shapes:DetailedPersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;

    sh:property [
        sh:path foaf:name ;
        sh:minCount 1 ;
        sh:message "Person {$this} is missing required 'name' property"@en ;
        sh:message "La persona {$this} no tiene la propiedad 'nombre' requerida"@es
    ] ;

    sh:property [
        sh:path foaf:age ;
        sh:datatype xsd:integer ;
        sh:message "Age of {$this} must be an integer, found: {$value}"@en
    ] .
```

## Exercise: Build a Data Quality Dashboard

```javascript
// exercises/quality-dashboard.mjs
import { parseTurtle, validateShacl, select } from 'unrdf/knowledge-engine';

const shapes = `
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Critical: Must have
ex:CriticalShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:severity sh:Violation ;
    sh:property [
        sh:path foaf:name ;
        sh:minCount 1 ;
        sh:message "Missing required name"
    ] .

# Warning: Should have
ex:WarningShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:severity sh:Warning ;
    sh:property [
        sh:path foaf:mbox ;
        sh:minCount 1 ;
        sh:message "Missing recommended email"
    ] .

# Info: Nice to have
ex:InfoShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:severity sh:Info ;
    sh:property [
        sh:path foaf:homepage ;
        sh:minCount 1 ;
        sh:message "Consider adding homepage"
    ] .
`;

async function generateQualityReport(dataStore) {
  const report = await validateShacl(dataStore, shapes);

  // Count by severity
  const violations = report.results?.filter(r =>
    !r.severity || r.severity.value.endsWith('Violation')
  ) || [];
  const warnings = report.results?.filter(r =>
    r.severity?.value.endsWith('Warning')
  ) || [];
  const info = report.results?.filter(r =>
    r.severity?.value.endsWith('Info')
  ) || [];

  // Count total entities
  const entities = await select(dataStore, `
    SELECT (COUNT(DISTINCT ?s) AS ?count)
    WHERE { ?s a ?type }
  `);

  const totalEntities = parseInt(entities[0]?.count?.value || 0);
  const problemEntities = new Set(report.results?.map(r => r.focusNode?.value)).size;

  // Calculate quality score (0-100)
  const qualityScore = Math.max(0, 100 - (violations.length * 10) - (warnings.length * 2) - (info.length * 0.5));

  return {
    summary: {
      totalEntities,
      compliantEntities: totalEntities - problemEntities,
      qualityScore: Math.round(qualityScore),
      grade: qualityScore >= 90 ? 'A' : qualityScore >= 80 ? 'B' : qualityScore >= 70 ? 'C' : qualityScore >= 60 ? 'D' : 'F'
    },
    issues: {
      critical: violations.length,
      warnings: warnings.length,
      suggestions: info.length
    },
    details: {
      violations: violations.map(v => ({
        entity: v.focusNode?.value,
        property: v.path?.value,
        message: v.message
      })),
      warnings: warnings.map(w => ({
        entity: w.focusNode?.value,
        property: w.path?.value,
        message: w.message
      }))
    }
  };
}

// Test data
const testData = `
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .

ex:alice a foaf:Person ;
    foaf:name "Alice" ;
    foaf:mbox <mailto:alice@example.com> ;
    foaf:homepage <http://alice.example.com> .

ex:bob a foaf:Person ;
    foaf:name "Bob" .

ex:carol a foaf:Person .
`;

const store = await parseTurtle(testData);
const dashboard = await generateQualityReport(store);
console.log(JSON.stringify(dashboard, null, 2));
```

## Common Mistakes

### Mistake 1: Wrong Target

```turtle
# Wrong - targeting property instead of class
sh:targetNode foaf:name .

# Correct - target the class of nodes
sh:targetClass foaf:Person .
```

### Mistake 2: Missing sh:path

```turtle
# Wrong - constraint without path
shapes:BadShape a sh:PropertyShape ;
    sh:minCount 1 .

# Correct - always specify path
shapes:GoodShape a sh:PropertyShape ;
    sh:path foaf:name ;
    sh:minCount 1 .
```

### Mistake 3: Datatype Mismatch

```turtle
# Data uses untyped literal
ex:alice foaf:age "30" .

# Shape expects integer - will fail!
sh:datatype xsd:integer .

# Data should be:
ex:alice foaf:age "30"^^xsd:integer .
# Or:
ex:alice foaf:age 30 .
```

## Summary

You learned:

- Writing SHACL shapes with various constraint types
- Handling validation results and errors
- Using severity levels for different issue types
- Conditional constraints (or, and, not, xone)
- SPARQL-based custom constraints
- Building validation into data workflows

## Next Steps

- [Performance Guide](../guides/performance-optimization.md) - Optimize validation
- [API Reference](../reference/api-reference.md) - Full validation API
- [Architecture](../explanation/system-design.md) - How SHACL integrates
