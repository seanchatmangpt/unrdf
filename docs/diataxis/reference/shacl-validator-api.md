# SHACL Validator API Reference

Complete API reference for SHACL validation in RDF-KGN.

## Overview

SHACL (Shapes Constraint Language) provides data validation for RDF graphs. This API allows you to define validation rules and check RDF data conformance.

## KGenSHACLTemplates

Main class for SHACL shape generation and validation.

### Constructor

```javascript
new KGenSHACLTemplates(options)
```

**Parameters:**
- `options` (Object)
  - `deterministicMode` (boolean) - Enable deterministic rendering (default: `true`)
  - `staticBuildTime` (string) - Static timestamp for determinism (default: `'2024-01-01T00:00:00.000Z'`)
  - `namespace` (string) - SHACL shapes namespace (default: `'http://kgen.ai/shacl#'`)
  - `baseIRI` (string) - Base IRI for shapes (default: `'http://kgen.ai/'`)

**Returns:** `KGenSHACLTemplates` instance

**Example:**

```javascript
import { KGenSHACLTemplates } from '@unrdf/kgn/src/base/shacl-templates.js';

const validator = new KGenSHACLTemplates({
  namespace: 'http://example.org/shapes#',
  baseIRI: 'http://example.org/',
  deterministicMode: true
});
```

## Core Methods

### `generateShape(templateName, context)`

Generate a SHACL shape from a template.

**Parameters:**
- `templateName` (string) - Name of built-in template
- `context` (Object) - Template variables

**Available Templates:**
- `basic_node_shape` - Basic node shape with properties
- `property_shape` - Property-specific validation
- `template_shape` - Template validation
- `filter_shape` - Filter validation
- `knowledge_graph_shape` - Knowledge graph validation
- `component_shape` - Component with required/optional properties
- `validation_rule` - Custom SPARQL-based validation

**Returns:** `Object`
- `name` (string) - Generated shape name
- `description` (string) - Shape description
- `content` (string) - SHACL shape in Turtle format
- `metadata` (Object) - Generation metadata
  - `template` (string) - Template used
  - `generated` (string) - Generation timestamp
  - `namespace` (string) - Shape namespace
  - `context` (Array<string>) - Context keys used

**Example:**

```javascript
const personShape = validator.generateShape('basic_node_shape', {
  shapeName: 'Person',
  targetClass: 'Person',
  description: 'Validates person data',
  properties: [
    {
      path: 'name',
      datatype: 'xsd:string',
      minCount: 1,
      maxCount: 1,
      message: 'Person must have exactly one name'
    },
    {
      path: 'email',
      datatype: 'xsd:string',
      minCount: 1,
      pattern: '^[^@]+@[^@]+\\.[^@]+$',
      message: 'Person must have a valid email'
    }
  ]
});

console.log(personShape.content);
```

### `generateValidationFile(shapes, filename)`

Generate complete SHACL validation file with multiple shapes.

**Parameters:**
- `shapes` (Array<Object>) - Array of shape definitions
  - `template` (string) - Template name
  - `context` (Object) - Template context
- `filename` (string) - Output filename (default: `'validation.ttl'`)

**Returns:** `Object`
- `filename` (string) - Output filename
- `content` (string) - Complete Turtle document
- `shapes` (Array<Object>) - Generated shapes
- `metadata` (Object) - File metadata
  - `shapeCount` (number) - Number of shapes
  - `generated` (string) - Generation timestamp
  - `namespace` (string) - Shapes namespace

**Example:**

```javascript
const validationFile = validator.generateValidationFile([
  {
    template: 'basic_node_shape',
    context: {
      shapeName: 'Person',
      targetClass: 'Person',
      properties: [
        { path: 'name', datatype: 'xsd:string', minCount: 1 }
      ]
    }
  },
  {
    template: 'basic_node_shape',
    context: {
      shapeName: 'Organization',
      targetClass: 'Organization',
      properties: [
        { path: 'name', datatype: 'xsd:string', minCount: 1 }
      ]
    }
  }
], 'my-validation.ttl');

console.log(`Generated ${validationFile.shapes.length} shapes`);
console.log(validationFile.content);
```

### `registerShapeTemplate(name, template)`

Register a custom SHACL shape template.

**Parameters:**
- `name` (string) - Template name
- `template` (Object)
  - `name` (string) - Display name
  - `description` (string) - Template description
  - `template` (string) - Template content (Nunjucks syntax)

**Example:**

```javascript
validator.registerShapeTemplate('custom_email_shape', {
  name: 'EmailValidationShape',
  description: 'Validates email addresses',
  template: `
ex:{{ shapeName }}Shape
    a sh:NodeShape ;
    sh:targetClass ex:{{ targetClass }} ;
    sh:property [
        sh:path ex:email ;
        sh:datatype xsd:string ;
        sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
        sh:message "{{ message | default('Invalid email address') }}" ;
    ] .
  `
});

const shape = validator.generateShape('custom_email_shape', {
  shapeName: 'Contact',
  targetClass: 'Contact',
  message: 'Contact email is invalid'
});
```

### `getTemplateNames()`

Get list of available shape template names.

**Returns:** `Array<string>` - Sorted array of template names

**Example:**

```javascript
const templates = validator.getTemplateNames();
console.log('Available templates:', templates);
// ['basic_node_shape', 'component_shape', 'filter_shape', ...]
```

### `getTemplate(name)`

Get shape template by name.

**Parameters:**
- `name` (string) - Template name

**Returns:** `Object|undefined` - Template definition or undefined

**Example:**

```javascript
const template = validator.getTemplate('basic_node_shape');
if (template) {
  console.log(template.name);
  console.log(template.description);
}
```

### `getStats()`

Get statistics about registered templates.

**Returns:** `Object`
- `totalTemplates` (number) - Number of registered templates
- `namespace` (string) - Shapes namespace
- `baseIRI` (string) - Base IRI
- `templates` (Array<string>) - Template names

**Example:**

```javascript
const stats = validator.getStats();
console.log(`${stats.totalTemplates} templates registered`);
console.log(`Namespace: ${stats.namespace}`);
```

## Shape Property Constraints

### Cardinality Constraints

```javascript
{
  path: 'name',
  minCount: 1,        // Required (at least 1)
  maxCount: 1         // At most 1 (single value)
}

{
  path: 'tags',
  minCount: 0,        // Optional
  maxCount: 10        // At most 10 values
}

{
  path: 'author',
  minCount: 1         // At least 1 (required, can be multiple)
}
```

### Datatype Constraints

```javascript
{
  path: 'name',
  datatype: 'xsd:string'
}

{
  path: 'age',
  datatype: 'xsd:integer',
  minInclusive: 0,
  maxInclusive: 150
}

{
  path: 'price',
  datatype: 'xsd:decimal',
  minExclusive: 0
}

{
  path: 'createdAt',
  datatype: 'xsd:dateTime'
}
```

### String Constraints

```javascript
{
  path: 'email',
  datatype: 'xsd:string',
  pattern: '^[^@]+@[^@]+\\.[^@]+$'
}

{
  path: 'bio',
  datatype: 'xsd:string',
  minLength: 10,
  maxLength: 500
}

{
  path: 'languageTag',
  datatype: 'xsd:string',
  languageIn: ['en', 'fr', 'de']
}
```

### Value Constraints

```javascript
{
  path: 'status',
  datatype: 'xsd:string',
  in: ['"draft"', '"published"', '"archived"']
}

{
  path: 'category',
  class: 'ex:Category'  // Must be instance of Category
}

{
  path: 'homepage',
  nodeKind: 'sh:IRI'     // Must be an IRI
}
```

## Built-in Shape Templates

### basic_node_shape

Basic node shape with property constraints.

**Context:**
- `shapeName` (string) - Shape name
- `targetClass` (string) - Target class name
- `description` (string) - Shape description (optional)
- `properties` (Array<Object>) - Property constraints

**Example:**

```javascript
const shape = validator.generateShape('basic_node_shape', {
  shapeName: 'Person',
  targetClass: 'Person',
  properties: [
    { path: 'name', datatype: 'xsd:string', minCount: 1 },
    { path: 'age', datatype: 'xsd:integer', minCount: 0, maxCount: 1 }
  ]
});
```

### property_shape

Detailed property-level validation.

**Context:**
- `shapeName` (string) - Shape name
- `propertyPath` (string) - Property path
- `datatype` (string) - XSD datatype (optional)
- `nodeKind` (string) - Node kind (sh:IRI, sh:Literal, etc.)
- `minCount`, `maxCount` (number) - Cardinality
- `minLength`, `maxLength` (number) - String length
- `pattern` (string) - Regex pattern
- `in` (Array) - Allowed values
- `message` (string) - Validation message
- `severity` (string) - Severity level (optional)

**Example:**

```javascript
const shape = validator.generateShape('property_shape', {
  shapeName: 'EmailProperty',
  propertyPath: 'email',
  datatype: 'xsd:string',
  minCount: 1,
  maxCount: 1,
  pattern: '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$',
  message: 'Invalid email format',
  severity: 'sh:Violation'
});
```

### component_shape

Component with required and optional properties.

**Context:**
- `shapeName` (string) - Shape name
- `targetClass` (string) - Target class
- `description` (string) - Description
- `requiredProperties` (Array<Object>) - Required properties
- `optionalProperties` (Array<Object>) - Optional properties

**Example:**

```javascript
const shape = validator.generateShape('component_shape', {
  shapeName: 'Article',
  targetClass: 'Article',
  description: 'Validates article data',
  requiredProperties: [
    { name: 'title', datatype: 'xsd:string', message: 'Title required' },
    { name: 'content', datatype: 'xsd:string', message: 'Content required' }
  ],
  optionalProperties: [
    { name: 'summary', datatype: 'xsd:string' },
    { name: 'publishedAt', datatype: 'xsd:dateTime' }
  ]
});
```

### validation_rule

Custom SPARQL-based validation rule.

**Context:**
- `ruleName` (string) - Rule name
- `targetClass` (string) - Target class (optional)
- `targetNode` (string) - Specific target node (optional)
- `description` (string) - Rule description
- `sparqlConstraint` (string) - SPARQL SELECT query
- `sparqlMessage` (string) - Violation message

**Example:**

```javascript
const shape = validator.generateShape('validation_rule', {
  ruleName: 'ConsistentDates',
  targetClass: 'Event',
  description: 'Validates that end date is after start date',
  sparqlConstraint: `
    SELECT $this
    WHERE {
      $this ex:startDate ?start ;
            ex:endDate ?end .
      FILTER(?end < ?start)
    }
  `,
  sparqlMessage: 'End date must be after start date'
});
```

## Validation Severity Levels

### sh:Info

Informational messages that don't fail validation.

```javascript
{
  path: 'description',
  minLength: 100,
  severity: 'sh:Info',
  message: 'Consider adding a longer description'
}
```

### sh:Warning

Warnings that don't fail validation but indicate potential issues.

```javascript
{
  path: 'image',
  minCount: 1,
  severity: 'sh:Warning',
  message: 'Adding an image improves engagement'
}
```

### sh:Violation

Violations that fail validation.

```javascript
{
  path: 'email',
  minCount: 1,
  severity: 'sh:Violation',  // Default severity
  message: 'Email is required'
}
```

## Mock Validation

### shaclValidate(data, shape)

Mock SHACL validation for testing (from RDF filters).

**Parameters:**
- `data` (any) - Data to validate
- `shape` (any) - SHACL shape

**Returns:** `Object`
- `conforms` (boolean) - Whether data conforms
- `results` (Array) - Validation results (empty in mock)
- `message` (string) - Status message

**Example:**

```javascript
import { rdfFilters } from '@unrdf/kgn/src/filters/rdf.js';

const result = rdfFilters.shaclValidate(myData, myShape);

if (result.conforms) {
  console.log('✅ Validation passed');
} else {
  console.log('❌ Validation failed');
}
```

## Validation Result Format

### ValidationReport

```typescript
interface ValidationReport {
  conforms: boolean;
  violations: ValidationResult[];
  warnings: ValidationResult[];
  infos: ValidationResult[];
  timestamp: string;
}
```

### ValidationResult

```typescript
interface ValidationResult {
  focusNode: string;        // Node that failed validation
  resultPath: string;       // Property path
  value?: string;           // Actual value (if applicable)
  message: string;          // Validation message
  severity: string;         // sh:Violation, sh:Warning, sh:Info
  sourceConstraint: string; // Shape that was violated
  sourceShape: string;      // Shape containing the constraint
}
```

## Best Practices

### 1. Use Appropriate Severity

```javascript
// ✅ Good: Match severity to requirement
{
  path: 'email',
  minCount: 1,
  severity: 'sh:Violation',  // Critical: email required
  message: 'Email is required'
}

{
  path: 'bio',
  minLength: 50,
  severity: 'sh:Warning',    // Nice to have: longer bio
  message: 'Bio should be at least 50 characters'
}

{
  path: 'avatar',
  minCount: 1,
  severity: 'sh:Info',       // Optional: avatar recommended
  message: 'Consider adding an avatar'
}
```

### 2. Provide Clear Messages

```javascript
// ❌ Bad: Vague message
{
  path: 'email',
  pattern: '^[^@]+@[^@]+$',
  message: 'Invalid'
}

// ✅ Good: Specific, actionable message
{
  path: 'email',
  pattern: '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$',
  message: 'Email must be a valid address (e.g., user@example.com)'
}
```

### 3. Use Composition for Complex Validation

```javascript
// Define base shape
const basePersonShape = validator.generateShape('basic_node_shape', {
  shapeName: 'BasePerson',
  properties: [
    { path: 'name', datatype: 'xsd:string', minCount: 1 },
    { path: 'email', datatype: 'xsd:string', minCount: 1 }
  ]
});

// Extend with specific requirements
const employeeShape = validator.generateShape('basic_node_shape', {
  shapeName: 'Employee',
  targetClass: 'Employee',
  properties: [
    { path: 'employeeId', datatype: 'xsd:string', minCount: 1 },
    { path: 'department', datatype: 'xsd:string', minCount: 1 }
  ]
});
// Add: sh:node ex:BasePersonShape ;
```

### 4. Test Shapes with Known Data

```javascript
// Create test data
const validPerson = `
ex:alice
  a ex:Person ;
  ex:name "Alice Smith" ;
  ex:email "alice@example.org" .
`;

const invalidPerson = `
ex:bob
  a ex:Person ;
  ex:name "Bob" .
  # Missing required email
`;

// Validate both
const result1 = validate(validPerson, personShape);
const result2 = validate(invalidPerson, personShape);

console.assert(result1.conforms === true, 'Valid person should pass');
console.assert(result2.conforms === false, 'Invalid person should fail');
```

## See Also

- [RDF-KGN API](./rdf-kgn-api.md)
- [Validate RDF with SHACL How-To](../how-to/validate-rdf-with-shacl.md)
- [RDF Validation Workflow Tutorial](../tutorials/rdf-validation-workflow.md)
- [Template-Driven RDF Generation](../explanation/template-driven-rdf-generation.md)
