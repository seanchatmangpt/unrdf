# Validation Utils

Schema-driven validation utilities for RDF data using Zod.

## Overview

The `validation-utils` module provides comprehensive validation for RDF terms, quads, stores, and various RDF formats using Zod schemas and custom validation logic.

## Schemas

### `IRISchema`

Zod schema for validating RDF IRIs.

```javascript
import { IRISchema } from 'unrdf/utils';

const result = IRISchema.safeParse('http://example.org/resource');
if (result.success) {
  console.log('Valid IRI');
}
```

### `LiteralSchema`

Zod schema for validating RDF literals.

```javascript
import { LiteralSchema } from 'unrdf/utils';

const literal = {
  termType: 'Literal',
  value: 'Hello',
  datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' }
};

const result = LiteralSchema.safeParse(literal);
```

### `NamedNodeSchema`

Zod schema for validating RDF named nodes.

```javascript
import { NamedNodeSchema } from 'unrdf/utils';

const namedNode = {
  termType: 'NamedNode',
  value: 'http://example.org/resource'
};

const result = NamedNodeSchema.safeParse(namedNode);
```

### `BlankNodeSchema`

Zod schema for validating RDF blank nodes.

```javascript
import { BlankNodeSchema } from 'unrdf/utils';

const blankNode = {
  termType: 'BlankNode',
  value: 'bnode1'
};

const result = BlankNodeSchema.safeParse(blankNode);
```

### `TermSchema`

Zod schema for validating any RDF term.

```javascript
import { TermSchema } from 'unrdf/utils';

const result = TermSchema.safeParse(anyTerm);
```

### `QuadSchema`

Zod schema for validating RDF quads.

```javascript
import { QuadSchema } from 'unrdf/utils';

const result = QuadSchema.safeParse(quad);
```

## Validation Functions

### `validateIRI(iri)`

Validates an RDF IRI.

```javascript
import { validateIRI } from 'unrdf/utils';

const isValid = validateIRI('http://example.org/resource');
// Returns: boolean
```

**Parameters:**
- `iri` (string) - IRI to validate

**Returns:** boolean

### `validateLiteral(literal)`

Validates an RDF literal.

```javascript
import { validateLiteral } from 'unrdf/utils';

const isValid = validateLiteral(literalTerm);
// Returns: boolean
```

**Parameters:**
- `literal` (Literal) - Literal term to validate

**Returns:** boolean

### `validateNamedNode(namedNode)`

Validates an RDF named node.

```javascript
import { validateNamedNode } from 'unrdf/utils';

const isValid = validateNamedNode(namedNodeTerm);
// Returns: boolean
```

**Parameters:**
- `namedNode` (NamedNode) - Named node term to validate

**Returns:** boolean

### `validateBlankNode(blankNode)`

Validates an RDF blank node.

```javascript
import { validateBlankNode } from 'unrdf/utils';

const isValid = validateBlankNode(blankNodeTerm);
// Returns: boolean
```

**Parameters:**
- `blankNode` (BlankNode) - Blank node term to validate

**Returns:** boolean

### `validateTerm(term)`

Validates any RDF term.

```javascript
import { validateTerm } from 'unrdf/utils';

const isValid = validateTerm(anyTerm);
// Returns: boolean
```

**Parameters:**
- `term` (Term) - RDF term to validate

**Returns:** boolean

### `validateQuad(quad)`

Validates an RDF quad.

```javascript
import { validateQuad } from 'unrdf/utils';

const isValid = validateQuad(quad);
// Returns: boolean
```

**Parameters:**
- `quad` (Quad) - RDF quad to validate

**Returns:** boolean

### `validateTurtle(turtle)`

Validates Turtle syntax.

```javascript
import { validateTurtle } from 'unrdf/utils';

const isValid = await validateTurtle('@prefix ex: <http://example.org/> . ex:test a ex:Thing .');
// Returns: boolean
```

**Parameters:**
- `turtle` (string) - Turtle string to validate

**Returns:** Promise<boolean>

### `validateNQuads(nquads)`

Validates N-Quads syntax.

```javascript
import { validateNQuads } from 'unrdf/utils';

const isValid = await validateNQuads('<http://example.org/s> <http://example.org/p> <http://example.org/o> .');
// Returns: boolean
```

**Parameters:**
- `nquads` (string) - N-Quads string to validate

**Returns:** Promise<boolean>

### `validateJSONLD(jsonld)`

Validates JSON-LD syntax.

```javascript
import { validateJSONLD } from 'unrdf/utils';

const jsonld = {
  '@context': { 'ex': 'http://example.org/' },
  '@id': 'ex:test',
  'ex:name': 'Test'
};

const isValid = await validateJSONLD(jsonld);
// Returns: boolean
```

**Parameters:**
- `jsonld` (Object) - JSON-LD object to validate

**Returns:** Promise<boolean>

### `validateSPARQL(query)`

Validates SPARQL query syntax.

```javascript
import { validateSPARQL } from 'unrdf/utils';

const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
const isValid = await validateSPARQL(query);
// Returns: boolean
```

**Parameters:**
- `query` (string) - SPARQL query to validate

**Returns:** Promise<boolean>

### `validateSHACL(shapes)`

Validates SHACL shapes.

```javascript
import { validateSHACL } from 'unrdf/utils';

const shapes = { /* SHACL shapes */ };
const isValid = await validateSHACL(shapes);
// Returns: boolean
```

**Parameters:**
- `shapes` (Object) - SHACL shapes to validate

**Returns:** Promise<boolean>

### `validateStore(store)`

Validates an RDF store for common issues.

```javascript
import { validateStore } from 'unrdf/utils';

const result = validateStore(store);
// Returns: { valid: boolean, issues: Array, issueCount: number, errorCount: number, warningCount: number }
```

**Parameters:**
- `store` (Store) - RDF store to validate

**Returns:** Validation result object

### `validateRDFConstraints(store)`

Validates RDF constraints (predicate must be NamedNode, subject cannot be Literal, etc.).

```javascript
import { validateRDFConstraints } from 'unrdf/utils';

const result = validateRDFConstraints(store);
// Returns: { valid: boolean, violations: Array, violationCount: number, errorCount: number, warningCount: number }
```

**Parameters:**
- `store` (Store) - RDF store to validate

**Returns:** Validation result object

### `validateCommonPatterns(store)`

Validates common RDF patterns.

```javascript
import { validateCommonPatterns } from 'unrdf/utils';

const result = validateCommonPatterns(store);
// Returns: { valid: boolean, patterns: Array, patternCount: number }
```

**Parameters:**
- `store` (Store) - RDF store to validate

**Returns:** Validation result object

### `createValidationPipeline(validators)`

Creates a validation pipeline with multiple validators.

```javascript
import { createValidationPipeline } from 'unrdf/utils';

const pipeline = createValidationPipeline([
  validateStore,
  validateRDFConstraints,
  validateCommonPatterns
]);

const result = pipeline(store);
// Returns: Combined validation results
```

**Parameters:**
- `validators` (Function[]) - Array of validation functions

**Returns:** Validation pipeline function

## Validation Results

Validation functions return result objects with the following structure:

```javascript
{
  valid: boolean,           // Overall validation status
  issues: Array,           // Array of issues found
  issueCount: number,      // Total number of issues
  errorCount: number,      // Number of errors
  warningCount: number     // Number of warnings
}
```

### Issue Structure

```javascript
{
  type: 'error' | 'warning',  // Issue severity
  message: string,            // Description of the issue
  quad?: Quad,               // Associated quad (if applicable)
  subject?: string,          // Subject IRI (if applicable)
  predicate?: string,        // Predicate IRI (if applicable)
  object?: string            // Object value (if applicable)
}
```

## Examples

### Basic Validation

```javascript
import { validateIRI, validateTerm, validateQuad } from 'unrdf/utils';

// Validate IRI
const isValidIRI = validateIRI('http://example.org/resource');

// Validate term
const isValidTerm = validateTerm(namedNode);

// Validate quad
const isValidQuad = validateQuad(quad);
```

### Store Validation

```javascript
import { validateStore, validateRDFConstraints } from 'unrdf/utils';

// Validate store
const storeResult = validateStore(store);
if (!storeResult.valid) {
  console.log(`Found ${storeResult.issueCount} issues:`);
  storeResult.issues.forEach(issue => {
    console.log(`- ${issue.type}: ${issue.message}`);
  });
}

// Validate RDF constraints
const constraintResult = validateRDFConstraints(store);
if (!constraintResult.valid) {
  console.log(`Found ${constraintResult.violationCount} constraint violations`);
}
```

### Format Validation

```javascript
import { validateTurtle, validateJSONLD, validateSPARQL } from 'unrdf/utils';

// Validate Turtle
const turtle = '@prefix ex: <http://example.org/> . ex:test a ex:Thing .';
const isValidTurtle = await validateTurtle(turtle);

// Validate JSON-LD
const jsonld = {
  '@context': { 'ex': 'http://example.org/' },
  '@id': 'ex:test',
  'ex:name': 'Test'
};
const isValidJSONLD = await validateJSONLD(jsonld);

// Validate SPARQL
const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
const isValidSPARQL = await validateSPARQL(query);
```

### Validation Pipeline

```javascript
import { createValidationPipeline, validateStore, validateRDFConstraints } from 'unrdf/utils';

// Create validation pipeline
const pipeline = createValidationPipeline([
  validateStore,
  validateRDFConstraints
]);

// Run validation
const result = pipeline(store);
if (!result.valid) {
  console.log(`Validation failed with ${result.issueCount} issues`);
  result.issues.forEach(issue => {
    console.log(`- ${issue.type}: ${issue.message}`);
  });
}
```

### Custom Validation

```javascript
import { z } from 'zod';

// Create custom schema
const PersonSchema = z.object({
  '@id': z.string().url(),
  'http://example.org/name': z.string().min(1),
  'http://example.org/age': z.number().min(0).max(150)
});

// Validate person data
const personData = {
  '@id': 'http://example.org/person1',
  'http://example.org/name': 'Alice',
  'http://example.org/age': 30
};

const result = PersonSchema.safeParse(personData);
if (result.success) {
  console.log('Valid person data');
} else {
  console.log('Validation errors:', result.error.issues);
}
```

## Error Handling

- **Graceful Failures**: Validation functions return false instead of throwing
- **Detailed Errors**: Zod provides detailed error information
- **Issue Tracking**: Store validation tracks multiple issues
- **Type Safety**: TypeScript support with proper type definitions

## Performance Notes

- **Efficient Validation**: Uses optimized Zod schemas
- **Lazy Evaluation**: Validation stops at first error when possible
- **Memory Usage**: Minimal memory overhead for validation operations
- **Large Stores**: Optimized for validating large RDF stores

## Related Modules

- [Term Utils](./term-utils.md) - Term creation and manipulation
- [Quad Utils](./quad-utils.md) - Quad operations
- [Graph Utils](./graph-utils.md) - Store operations
