# SHACL Validation

Knowd provides comprehensive support for SHACL (Shapes Constraint Language) validation, allowing you to define and enforce data quality rules for your knowledge graph.

## Overview

SHACL is a W3C standard for validating RDF data against a set of conditions called "shapes". Knowd supports:

- **Node Shapes** - Validate individual nodes
- **Property Shapes** - Validate properties of nodes
- **Constraint Components** - Built-in validation functions
- **Custom Functions** - User-defined validation logic
- **Advanced Features** - Property paths, logical operators, and more

## Basic Usage

### Simple Validation

**Define shapes:**
```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [
    sh:path ex:name ;
    sh:minCount 1 ;
    sh:datatype xsd:string ;
  ] ;
  sh:property [
    sh:path ex:age ;
    sh:minInclusive 0 ;
    sh:maxInclusive 150 ;
    sh:datatype xsd:integer ;
  ] .
```

**Validate data:**
```bash
curl -X POST http://localhost:8090/v1/validate \
  -H "Content-Type: application/json" \
  -d '{
    "data": "@prefix ex: <http://example.org/> . ex:alice a ex:Person ; ex:name \"Alice\" ; ex:age 25 .",
    "shapes": "@prefix sh: <http://www.w3.org/ns/shacl#> . ex:PersonShape a sh:NodeShape ; sh:targetClass ex:Person ; sh:property [ sh:path ex:name ; sh:minCount 1 ] ."
  }'
```

**Response:**
```json
{
  "conforms": true,
  "violations": []
}
```

## Constraint Components

Knowd supports all major SHACL constraint components:

### Cardinality Constraints

**sh:minCount, sh:maxCount:**
```turtle
ex:PersonShape sh:property [
  sh:path ex:email ;
  sh:minCount 1 ;
  sh:maxCount 3 ;
] .
```

**sh:minLength, sh:maxLength:**
```turtle
ex:PersonShape sh:property [
  sh:path ex:description ;
  sh:minLength 10 ;
  sh:maxLength 500 ;
] .
```

### Value Constraints

**sh:hasValue:**
```turtle
ex:AdminShape sh:property [
  sh:path ex:role ;
  sh:hasValue ex:admin ;
] .
```

**sh:in:**
```turtle
ex:StatusShape sh:property [
  sh:path ex:status ;
  sh:in (ex:active ex:inactive ex:pending) ;
] .
```

**sh:languageIn:**
```turtle
ex:DescriptionShape sh:property [
  sh:path ex:description ;
  sh:languageIn ("en" "fr" "de") ;
] .
```

### Datatype Constraints

**sh:datatype:**
```turtle
ex:AgeShape sh:property [
  sh:path ex:age ;
  sh:datatype xsd:integer ;
] .
```

**sh:class:**
```turtle
ex:EmployeeShape sh:property [
  sh:path ex:manager ;
  sh:class ex:Person ;
] .
```

### String Constraints

**sh:pattern:**
```turtle
ex:EmailShape sh:property [
  sh:path ex:email ;
  sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
] .
```

**sh:minLength, sh:maxLength:**
```turtle
ex:NameShape sh:property [
  sh:path ex:name ;
  sh:minLength 2 ;
  sh:maxLength 50 ;
] .
```

### Numeric Constraints

**sh:minInclusive, sh:maxInclusive:**
```turtle
ex:ScoreShape sh:property [
  sh:path ex:score ;
  sh:minInclusive 0 ;
  sh:maxInclusive 100 ;
  sh:datatype xsd:decimal ;
] .
```

**sh:minExclusive, sh:maxExclusive:**
```turtle
ex:TemperatureShape sh:property [
  sh:path ex:temperature ;
  sh:minExclusive -273.15 ;
  sh:maxExclusive 10000 ;
] .
```

## Advanced Features

### Property Paths

**Simple paths:**
```turtle
ex:ChainShape a sh:NodeShape ;
  sh:targetNode ex:start ;
  sh:property [
    sh:path (ex:link1 ex:link2 ex:link3) ;
    sh:minCount 1 ;
  ] .
```

**Complex paths:**
```turtle
ex:NetworkShape a sh:NodeShape ;
  sh:targetClass ex:Node ;
  sh:property [
    sh:path [ sh:alternativePath (ex:direct ex:indirect+) ] ;
    sh:minCount 1 ;
  ] .
```

### Logical Operators

**sh:and:**
```turtle
ex:StrictPersonShape sh:property [
  sh:path ex:age ;
  sh:and (
    [ sh:minInclusive 18 ]
    [ sh:maxInclusive 65 ]
    [ sh:datatype xsd:integer ]
  ) ;
] .
```

**sh:or:**
```turtle
ex:ContactShape sh:property [
  sh:path ex:contact ;
  sh:or (
    [ sh:datatype xsd:string ]
    [ sh:class ex:PhoneNumber ]
    [ sh:class ex:EmailAddress ]
  ) ;
] .
```

**sh:not:**
```turtle
ex:SafePersonShape sh:property [
  sh:path ex:riskLevel ;
  sh:not [ sh:in (ex:high ex:critical) ] ;
] .
```

### Custom Functions

**Function definition:**
```turtle
ex:validateEmail a sh:SPARQLFunction ;
  sh:parameter [
    sh:path ex:email ;
    sh:datatype xsd:string ;
  ] ;
  sh:returnType xsd:boolean ;
  sh:select """
    SELECT (REGEX($email, "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$") AS ?valid)
  """ .
```

**Function usage:**
```turtle
ex:EmailShape sh:property [
  sh:path ex:email ;
  ex:validateEmail true ;
] .
```

## Validation Reports

### Compact Report Format

**Default response format:**
```json
{
  "conforms": false,
  "violations": [
    {
      "focusNode": "<http://example.org/person1>",
      "property": "<http://example.org/age>",
      "constraint": "sh:minInclusive",
      "message": "Value must be >= 18",
      "severity": "Violation",
      "value": 15
    }
  ]
}
```

### Detailed Report Format

Enable detailed reports for debugging:

```bash
curl -X POST http://localhost:8090/v1/validate \
  -H "Content-Type: application/json" \
  -d '{
    "data": "...",
    "shapes": "...",
    "reportFormat": "detailed"
  }'
```

**Detailed response:**
```json
{
  "conforms": false,
  "results": [
    {
      "focusNode": "<http://example.org/person1>",
      "resultPath": "<http://example.org/age>",
      "sourceShape": "<http://example.org/PersonShape>",
      "constraintComponent": "<http://www.w3.org/ns/shacl#MinInclusiveConstraintComponent>",
      "value": 15,
      "message": "Value 15 is less than minimum 18",
      "severity": "sh:Violation"
    }
  ]
}
```

## Validation Targets

### Target Declaration

**Target class:**
```turtle
ex:PersonShape a sh:NodeShape ;
  sh:targetClass ex:Person .
```

**Target subjects of:**
```turtle
ex:EmployeeShape a sh:NodeShape ;
  sh:targetSubjectsOf ex:worksFor .
```

**Target objects of:**
```turtle
ex:ProjectShape a sh:NodeShape ;
  sh:targetObjectsOf ex:hasProject .
```

**Target nodes:**
```turtle
ex:SpecificShape a sh:NodeShape ;
  sh:targetNode ex:importantPerson .
```

### Implicit Targets

**Shapes without explicit targets** validate all nodes that match their criteria.

## Best Practices

### 1. Start Simple

Begin with basic constraints and gradually add complexity:

```turtle
# Start with basic cardinality
ex:BasicShape sh:property [
  sh:path ex:name ;
  sh:minCount 1 ;
] .

# Add datatype constraints
ex:BasicShape sh:property [
  sh:path ex:age ;
  sh:datatype xsd:integer ;
] .

# Add value constraints
ex:BasicShape sh:property [
  sh:path ex:status ;
  sh:in (ex:active ex:inactive) ;
] .
```

### 2. Use Descriptive Messages

Provide clear error messages for validation failures:

```turtle
ex:AgeShape sh:property [
  sh:path ex:age ;
  sh:minInclusive 0 ;
  sh:message "Age must be non-negative" ;
] .
```

### 3. Organize Shapes by Purpose

Group related constraints:

```turtle
# Data quality shapes
ex:DataQualityShape a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [
    sh:path ex:name ;
    sh:minLength 1 ;
    sh:pattern "^[A-Za-z ]+$" ;
  ] .

# Business rule shapes
ex:BusinessRuleShape a sh:NodeShape ;
  sh:targetClass ex:Employee ;
  sh:property [
    sh:path ex:salary ;
    sh:minInclusive 30000 ;
    sh:message "Salary must be at least $30,000" ;
  ] .
```

### 4. Performance Considerations

**Efficient patterns:**
```turtle
# Good - specific path reduces search space
ex:EfficientShape sh:property [
  sh:path ex:person/ex:name ;
  sh:minCount 1 ;
] .

# Avoid - broad patterns can be slow
ex:InefficientShape sh:property [
  sh:path [ sh:zeroOrMorePath ex:relatedTo ] ;
  sh:minCount 1 ;
] .
```

## Integration Examples

### Pre-validation Hooks

Use hooks to validate data before processing:

```turtle
# Hook that validates incoming data
ex:ValidationHook a ex:Hook ;
  ex:type "shacl-validate" ;
  ex:shapes ex:PersonShape ;
  ex:trigger "before-transaction" .
```

### Post-validation Actions

Trigger actions based on validation results:

```turtle
# Hook that sends alerts for validation failures
ex:AlertHook a ex:Hook ;
  ex:type "validation-alert" ;
  ex:condition "validation-failed" ;
  ex:action "send-notification" .
```

## Troubleshooting

### Common Validation Errors

**"Shape not found":**
- Ensure shape definitions are included in the validation request
- Check that shape URIs are correctly referenced

**"Target not matched":**
- Verify target declarations match your data structure
- Check that target nodes/subjects exist in the data

**"Performance issues":**
- Use specific property paths instead of broad searches
- Limit the scope of validation targets
- Enable detailed logging for performance analysis

### Debugging Validation

**Enable verbose logging:**
```bash
KNOWD_LOG_LEVEL=debug ./knowd
```

**Check validation details:**
```bash
curl -X POST http://localhost:8090/v1/validate \
  -H "Content-Type: application/json" \
  -d '{
    "data": "...",
    "shapes": "...",
    "debug": true
  }'
```

## Examples

### Complete Validation Workflow

**1. Define comprehensive shapes:**
```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Person validation
ex:PersonShape a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [
    sh:path ex:name ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:string ;
    sh:minLength 2 ;
  ] ;
  sh:property [
    sh:path ex:age ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
    sh:minInclusive 0 ;
    sh:maxInclusive 150 ;
  ] ;
  sh:property [
    sh:path ex:email ;
    sh:minCount 1 ;
    sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
  ] .

# Organization validation
ex:OrgShape a sh:NodeShape ;
  sh:targetClass ex:Organization ;
  sh:property [
    sh:path ex:name ;
    sh:minCount 1 ;
    sh:datatype xsd:string ;
  ] ;
  sh:property [
    sh:path ex:employee ;
    sh:class ex:Person ;
  ] .
```

**2. Validate sample data:**
```bash
curl -X POST http://localhost:8090/v1/validate \
  -H "Content-Type: application/json" \
  -d '{
    "data": "@prefix ex: <http://example.org/> . ex:alice a ex:Person ; ex:name \"Alice\" ; ex:age 25 ; ex:email \"alice@example.com\" . ex:acme a ex:Organization ; ex:name \"ACME Corp\" ; ex:employee ex:alice .",
    "shapes": "@prefix sh: <http://www.w3.org/ns/shacl#> . ex:PersonShape a sh:NodeShape ; sh:targetClass ex:Person ; sh:property [ sh:path ex:name ; sh:minCount 1 ] . ex:OrgShape a sh:NodeShape ; sh:targetClass ex:Organization ; sh:property [ sh:path ex:name ; sh:minCount 1 ] ."
  }'
```

**3. Handle validation results:**
```bash
# If validation passes
if [ "$(echo $RESPONSE | jq '.conforms')" = "true" ]; then
  echo "Data is valid"
else
  echo "Validation errors:"
  echo $RESPONSE | jq '.violations'
fi
```

This comprehensive SHACL validation system ensures data quality and integrity across your knowledge graph.
