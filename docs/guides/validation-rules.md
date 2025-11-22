# How-To: Create Validation Rules

Task-oriented guide for SHACL validation in UNRDF.

## Quick Reference

```javascript
import { validateShacl, hasValidationErrors, formatValidationReport } from 'unrdf/knowledge-engine';

const report = await validateShacl(dataStore, shapesString);

if (report.conforms) {
  console.log('Valid!');
} else {
  console.log(formatValidationReport(report));
}
```

## How to Validate Required Fields

```turtle
# shapes/required-fields.ttl
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/shapes/> .

ex:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
        sh:path foaf:name ;
        sh:minCount 1 ;
        sh:message "Name is required"
    ] ;
    sh:property [
        sh:path foaf:mbox ;
        sh:minCount 1 ;
        sh:message "Email is required"
    ] .
```

```javascript
import { readFileSync } from 'node:fs';
import { parseTurtle, validateShacl } from 'unrdf/knowledge-engine';

const shapes = readFileSync('./shapes/required-fields.ttl', 'utf-8');
const report = await validateShacl(store, shapes);
```

## How to Validate Data Types

```turtle
ex:TypedFieldsShape a sh:NodeShape ;
    sh:targetClass ex:Product ;
    sh:property [
        sh:path ex:name ;
        sh:datatype xsd:string ;
        sh:message "Name must be a string"
    ] ;
    sh:property [
        sh:path ex:price ;
        sh:datatype xsd:decimal ;
        sh:message "Price must be a decimal"
    ] ;
    sh:property [
        sh:path ex:quantity ;
        sh:datatype xsd:integer ;
        sh:message "Quantity must be an integer"
    ] ;
    sh:property [
        sh:path ex:createdAt ;
        sh:datatype xsd:dateTime ;
        sh:message "CreatedAt must be a dateTime"
    ] .
```

## How to Validate Value Ranges

```turtle
ex:RangeValidation a sh:NodeShape ;
    sh:targetClass ex:Employee ;

    # Age between 18 and 100
    sh:property [
        sh:path ex:age ;
        sh:minInclusive 18 ;
        sh:maxInclusive 100 ;
        sh:message "Age must be between 18 and 100"
    ] ;

    # Salary greater than 0
    sh:property [
        sh:path ex:salary ;
        sh:minExclusive 0 ;
        sh:message "Salary must be positive"
    ] ;

    # Rating 1-5
    sh:property [
        sh:path ex:rating ;
        sh:minInclusive 1 ;
        sh:maxInclusive 5 ;
        sh:message "Rating must be 1-5"
    ] .
```

## How to Validate String Patterns

```turtle
ex:PatternValidation a sh:NodeShape ;
    sh:targetClass ex:User ;

    # Email format
    sh:property [
        sh:path ex:email ;
        sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
        sh:message "Invalid email format"
    ] ;

    # Phone format (US)
    sh:property [
        sh:path ex:phone ;
        sh:pattern "^\\+1-\\d{3}-\\d{3}-\\d{4}$" ;
        sh:message "Phone must be +1-XXX-XXX-XXXX format"
    ] ;

    # Username (alphanumeric, 3-20 chars)
    sh:property [
        sh:path ex:username ;
        sh:pattern "^[a-zA-Z0-9_]{3,20}$" ;
        sh:minLength 3 ;
        sh:maxLength 20 ;
        sh:message "Username must be 3-20 alphanumeric characters"
    ] .
```

## How to Validate Enumerated Values

```turtle
ex:EnumValidation a sh:NodeShape ;
    sh:targetClass ex:Order ;

    sh:property [
        sh:path ex:status ;
        sh:in ( "pending" "processing" "shipped" "delivered" "cancelled" ) ;
        sh:message "Status must be one of: pending, processing, shipped, delivered, cancelled"
    ] ;

    sh:property [
        sh:path ex:priority ;
        sh:in ( "low" "medium" "high" "urgent" ) ;
        sh:message "Invalid priority level"
    ] .
```

## How to Validate Relationships

```turtle
ex:RelationshipValidation a sh:NodeShape ;
    sh:targetClass ex:Employee ;

    # Manager must be a Manager type
    sh:property [
        sh:path ex:reportsTo ;
        sh:class ex:Manager ;
        sh:message "Must report to a Manager"
    ] ;

    # Department must exist
    sh:property [
        sh:path ex:department ;
        sh:class ex:Department ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:message "Must belong to exactly one Department"
    ] ;

    # Team members must be Employees
    sh:property [
        sh:path ex:teamMember ;
        sh:class ex:Employee ;
        sh:message "Team members must be Employees"
    ] .
```

## How to Create Conditional Validation

### Either/Or (sh:or)

```turtle
ex:ContactRequired a sh:NodeShape ;
    sh:targetClass ex:Customer ;
    sh:or (
        [ sh:property [ sh:path ex:email ; sh:minCount 1 ] ]
        [ sh:property [ sh:path ex:phone ; sh:minCount 1 ] ]
    ) ;
    sh:message "Customer must have either email or phone"
.
```

### Both Required (sh:and)

```turtle
ex:VerifiedUser a sh:NodeShape ;
    sh:targetClass ex:VerifiedUser ;
    sh:and (
        [ sh:property [ sh:path ex:email ; sh:minCount 1 ] ]
        [ sh:property [ sh:path ex:emailVerified ; sh:hasValue true ] ]
    ) ;
    sh:message "Verified user must have email and emailVerified=true"
.
```

### Exactly One (sh:xone)

```turtle
ex:PaymentMethod a sh:NodeShape ;
    sh:targetClass ex:Payment ;
    sh:xone (
        [ sh:property [ sh:path ex:creditCard ; sh:minCount 1 ] ]
        [ sh:property [ sh:path ex:bankAccount ; sh:minCount 1 ] ]
        [ sh:property [ sh:path ex:paypal ; sh:minCount 1 ] ]
    ) ;
    sh:message "Must have exactly one payment method"
.
```

## How to Add Severity Levels

```turtle
# Critical (default)
ex:CriticalCheck a sh:PropertyShape ;
    sh:path ex:required ;
    sh:minCount 1 ;
    sh:severity sh:Violation ;
    sh:message "CRITICAL: Required field missing"
.

# Warning
ex:WarningCheck a sh:PropertyShape ;
    sh:path ex:recommended ;
    sh:minCount 1 ;
    sh:severity sh:Warning ;
    sh:message "WARNING: Consider adding this field"
.

# Info
ex:InfoCheck a sh:PropertyShape ;
    sh:path ex:optional ;
    sh:minCount 1 ;
    sh:severity sh:Info ;
    sh:message "INFO: Optional field not provided"
.
```

## How to Process Validation Results

```javascript
import { validateShacl, getValidationErrors, getValidationWarnings } from 'unrdf/knowledge-engine';

async function validateWithDetails(store, shapes) {
  const report = await validateShacl(store, shapes);

  const errors = getValidationErrors(report);
  const warnings = getValidationWarnings(report);

  return {
    isValid: report.conforms,
    errors: errors.map(e => ({
      node: e.focusNode?.value,
      path: e.path?.value,
      message: e.message,
      value: e.value?.value
    })),
    warnings: warnings.map(w => ({
      node: w.focusNode?.value,
      path: w.path?.value,
      message: w.message
    })),
    summary: {
      errorCount: errors.length,
      warningCount: warnings.length
    }
  };
}
```

## How to Validate Before Insert

```javascript
async function validateAndInsert(newData, existingStore, shapes) {
  const newStore = await parseTurtle(newData);

  // Validate new data alone
  const report = await validateShacl(newStore, shapes);

  if (!report.conforms) {
    const errors = getValidationErrors(report);
    throw new ValidationError('Invalid data', errors);
  }

  // Merge into existing store
  for (const quad of newStore) {
    existingStore.addQuad(quad);
  }

  return existingStore;
}

class ValidationError extends Error {
  constructor(message, errors) {
    super(message);
    this.name = 'ValidationError';
    this.errors = errors;
  }
}
```

## How to Create Reusable Shapes

```turtle
# shapes/common/string-types.ttl
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix shapes: <http://example.org/shapes/> .

shapes:EmailShape a sh:PropertyShape ;
    sh:datatype xsd:string ;
    sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    sh:message "Invalid email format"
.

shapes:URLShape a sh:PropertyShape ;
    sh:datatype xsd:anyURI ;
    sh:pattern "^https?://" ;
    sh:message "Must be a valid HTTP(S) URL"
.

shapes:PhoneUSShape a sh:PropertyShape ;
    sh:datatype xsd:string ;
    sh:pattern "^\\+1-\\d{3}-\\d{3}-\\d{4}$" ;
    sh:message "Must be US phone format: +1-XXX-XXX-XXXX"
.
```

Use them:

```turtle
# shapes/customer.ttl
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix shapes: <http://example.org/shapes/> .
@prefix ex: <http://example.org/> .

ex:CustomerShape a sh:NodeShape ;
    sh:targetClass ex:Customer ;
    sh:property [
        sh:path ex:email ;
        sh:node shapes:EmailShape ;
        sh:minCount 1
    ] ;
    sh:property [
        sh:path ex:phone ;
        sh:node shapes:PhoneUSShape
    ] .
```

## Troubleshooting

### Validation Always Passes

1. Check `sh:targetClass` matches your data's `rdf:type`
2. Verify shapes file is parsed correctly
3. Test with obviously invalid data

### Wrong Error Messages

1. Ensure `sh:message` is defined on the constraint
2. Check for multilingual messages (`sh:message "..."@en`)

### Performance Issues

1. Limit shapes to necessary constraints
2. Use specific `sh:targetClass` over `sh:targetNode`
3. Avoid complex SPARQL constraints when simpler work

## Related

- [SHACL Tutorial](../tutorials/validation.md) - In-depth learning
- [API Reference](../reference/api-reference.md) - Full validation API
- [Performance Guide](./performance-optimization.md) - Optimization tips
