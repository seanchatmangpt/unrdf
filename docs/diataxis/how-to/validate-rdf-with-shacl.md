# How to Validate RDF with SHACL

**Goal:** Validate RDF data using SHACL shapes
**Time:** 10-15 minutes
**Difficulty:** Intermediate

## Problem

You need to ensure RDF data conforms to expected structure and constraints before using it in production.

## Solution

Use SHACL (Shapes Constraint Language) to define validation rules and check RDF data against them.

## Prerequisites

- `@unrdf/kgn` package installed
- RDF data to validate
- Understanding of RDF structure

## Quick Example

```javascript
import { KGenSHACLTemplates } from '@unrdf/kgn/src/base/shacl-templates.js';

// Generate validation shape
const shacl = new KGenSHACLTemplates();

const shape = shacl.generateShape('basic_node_shape', {
  shapeName: 'Person',
  targetClass: 'Person',
  properties: [
    { path: 'name', datatype: 'xsd:string', minCount: 1 },
    { path: 'email', datatype: 'xsd:string', minCount: 1 }
  ]
});

console.log(shape.content);
```

## Method 1: Generate SHACL Shapes

### Step 1: Define Shape Requirements

```javascript
import { KGenSHACLTemplates } from '@unrdf/kgn/src/base/shacl-templates.js';

const shacl = new KGenSHACLTemplates({
  namespace: 'http://example.org/shapes#',
  baseIRI: 'http://example.org/'
});

// Define shape with required properties
const personShape = shacl.generateShape('component_shape', {
  shapeName: 'Person',
  targetClass: 'Person',
  description: 'Validates person data with comprehensive rules',
  requiredProperties: [
    {
      name: 'name',
      datatype: 'xsd:string',
      maxCount: 1,
      pattern: '.+', // Non-empty
      message: 'Person must have a non-empty name'
    },
    {
      name: 'email',
      datatype: 'xsd:string',
      maxCount: 1,
      pattern: '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$',
      message: 'Person must have a valid email address'
    }
  ],
  optionalProperties: [
    {
      name: 'age',
      datatype: 'xsd:integer',
      maxCount: 1
    },
    {
      name: 'bio',
      datatype: 'xsd:string',
      maxCount: 1
    }
  ]
});
```

### Step 2: Save Shapes to File

```javascript
import { writeFileSync } from 'fs';

writeFileSync('person-shapes.ttl', personShape.content);
console.log('✅ SHACL shapes saved to person-shapes.ttl');
```

## Method 2: Create Custom Validation Rules

### Property Constraints

```javascript
// Number range validation
const ageShape = shacl.generateShape('property_shape', {
  shapeName: 'Age',
  propertyPath: 'age',
  datatype: 'xsd:integer',
  minCount: 0,
  maxCount: 1,
  minInclusive: 0,
  maxInclusive: 150,
  message: 'Age must be between 0 and 150'
});

// String length validation
const bioShape = shacl.generateShape('property_shape', {
  shapeName: 'Bio',
  propertyPath: 'bio',
  datatype: 'xsd:string',
  minLength: 10,
  maxLength: 500,
  message: 'Bio must be between 10 and 500 characters'
});

// Enum validation
const roleShape = shacl.generateShape('property_shape', {
  shapeName: 'Role',
  propertyPath: 'role',
  datatype: 'xsd:string',
  in: ['"admin"', '"user"', '"guest"'],
  message: 'Role must be one of: admin, user, guest'
});
```

### Relationship Constraints

```javascript
// Required relationship
const employeeShape = shacl.generateShape('basic_node_shape', {
  shapeName: 'Employee',
  targetClass: 'Employee',
  properties: [
    {
      path: 'worksFor',
      class: 'ex:Organization',
      minCount: 1,
      maxCount: 1,
      message: 'Employee must work for exactly one organization'
    }
  ]
});

// Optional relationships
const personRelationsShape = shacl.generateShape('basic_node_shape', {
  shapeName: 'PersonRelations',
  targetClass: 'Person',
  properties: [
    {
      path: 'knows',
      class: 'ex:Person',
      message: 'knows must reference a Person'
    },
    {
      path: 'memberOf',
      class: 'ex:Organization',
      message: 'memberOf must reference an Organization'
    }
  ]
});
```

## Method 3: SPARQL-Based Validation

For complex business rules, use SPARQL constraints.

```javascript
const complexRule = shacl.generateShape('validation_rule', {
  ruleName: 'EmployeeValidation',
  targetClass: 'Employee',
  description: 'Validates that employees have valid employment data',
  sparqlConstraint: `
    SELECT $this ?value
    WHERE {
      $this ex:hireDate ?hireDate .
      $this ex:birthDate ?birthDate .
      FILTER(?hireDate < ?birthDate)
    }
  `,
  sparqlMessage: 'Hire date cannot be before birth date'
});
```

## Method 4: Validate RDF Data

### Mock Validation (Development)

```javascript
import { rdfFilters } from '@unrdf/kgn/src/filters/rdf.js';

// Use the mock validator
const result = rdfFilters.shaclValidate(data, shapes);

console.log('Validation result:', result);
console.log('Conforms:', result.conforms);
console.log('Violations:', result.results);
```

### Production Validation

For production, integrate with a SHACL validator library:

```javascript
// Pseudo-code for production validator
import { SHACLValidator } from 'shacl-validator-library';

async function validateRDF(dataFile, shapesFile) {
  const validator = new SHACLValidator();

  const data = await loadRDF(dataFile);
  const shapes = await loadRDF(shapesFile);

  const report = await validator.validate(data, shapes);

  return {
    conforms: report.conforms,
    violations: report.results.map(r => ({
      focusNode: r.focusNode,
      path: r.resultPath,
      severity: r.severity,
      message: r.message
    }))
  };
}

const result = await validateRDF('data.ttl', 'shapes.ttl');

if (!result.conforms) {
  console.error('Validation failed:');
  result.violations.forEach(v => {
    console.error(`  - ${v.focusNode}: ${v.message}`);
  });
  process.exit(1);
}
```

## Common Validation Patterns

### Pattern 1: Required Properties

```turtle
sh:property [
    sh:path ex:name ;
    sh:minCount 1 ;
    sh:message "Name is required" ;
] ;
```

### Pattern 2: Unique Values

```turtle
sh:property [
    sh:path ex:email ;
    sh:maxCount 1 ;
    sh:uniqueLang true ;
    sh:message "Email must be unique" ;
] ;
```

### Pattern 3: Format Validation

```turtle
sh:property [
    sh:path ex:phone ;
    sh:pattern "^\\+?[1-9]\\d{1,14}$" ;
    sh:message "Phone must be in E.164 format" ;
] ;
```

### Pattern 4: Conditional Validation

```turtle
# If type is Employee, then employeeId is required
ex:ConditionalEmployeeIdShape
    a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:sparql [
        sh:select """
            SELECT $this
            WHERE {
                $this a ex:Employee .
                FILTER NOT EXISTS {
                    $this ex:employeeId ?id .
                }
            }
        """ ;
        sh:message "Employees must have an employeeId" ;
    ] .
```

### Pattern 5: Cross-Property Validation

```turtle
ex:DateRangeShape
    a sh:NodeShape ;
    sh:targetClass ex:Event ;
    sh:sparql [
        sh:select """
            SELECT $this
            WHERE {
                $this ex:startDate ?start .
                $this ex:endDate ?end .
                FILTER(?end < ?start)
            }
        """ ;
        sh:message "End date must be after start date" ;
    ] .
```

## Validation Severity Levels

```javascript
// Information (doesn't fail validation)
const infoShape = {
  path: 'ex:preferredName',
  severity: 'sh:Info',
  message: 'Consider adding a preferred name'
};

// Warning (logs but doesn't fail)
const warningShape = {
  path: 'ex:bio',
  minLength: 50,
  severity: 'sh:Warning',
  message: 'Bio should be at least 50 characters for better profile'
};

// Violation (fails validation)
const violationShape = {
  path: 'ex:email',
  minCount: 1,
  severity: 'sh:Violation',
  message: 'Email is required'
};
```

## Complete Validation Workflow

```javascript
import { KGenSHACLTemplates } from '@unrdf/kgn/src/base/shacl-templates.js';
import { writeFileSync, readFileSync } from 'fs';

// 1. Define shapes
const shacl = new KGenSHACLTemplates();

const shapes = [
  {
    template: 'basic_node_shape',
    context: {
      shapeName: 'Person',
      targetClass: 'Person',
      properties: [
        { path: 'name', datatype: 'xsd:string', minCount: 1 },
        { path: 'email', datatype: 'xsd:string', minCount: 1 }
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
];

// 2. Generate shapes file
const shapesFile = shacl.generateValidationFile(shapes, 'validation.ttl');
writeFileSync('validation-shapes.ttl', shapesFile.content);

console.log('✅ Generated shapes:', shapesFile.shapes.length);

// 3. Load data to validate
const dataToValidate = readFileSync('data.ttl', 'utf-8');

// 4. Validate (mock for this example)
console.log('📋 Validating data...');

// In production, use actual SHACL validator
const validationResult = {
  conforms: true,
  violations: []
};

if (validationResult.conforms) {
  console.log('✅ Validation passed!');
} else {
  console.error('❌ Validation failed');
  validationResult.violations.forEach(v => {
    console.error(`  - ${v.focusNode}: ${v.message}`);
  });
  process.exit(1);
}
```

## Best Practices

1. **Start with basic shapes:** Add complexity incrementally
2. **Test shapes with known data:** Verify correct pass/fail behavior
3. **Use appropriate severity:** Info < Warning < Violation
4. **Document constraints:** Use rdfs:comment for shape documentation
5. **Version shapes:** Track changes to validation rules
6. **Separate concerns:** One shape per logical entity type
7. **Reuse shapes:** Use sh:node for composition

## Troubleshooting

### Problem: Shape doesn't match any nodes

**Solution:** Check `sh:targetClass` matches RDF data:

```sparql
# Check if target class exists
SELECT ?instance
WHERE {
  ?instance a ex:Person .
}
```

### Problem: Pattern doesn't match valid data

**Solution:** Test regex separately:

```javascript
const pattern = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/;
const email = 'test@example.org';
console.log(pattern.test(email)); // Should be true
```

### Problem: Too many false positives

**Solution:** Adjust constraints:

```turtle
# Too strict
sh:minCount 1 ;
sh:maxCount 1 ;

# More flexible
sh:minCount 0 ;
sh:maxCount 10 ;
```

## Advanced Techniques

### Technique 1: Shape Inheritance

```javascript
// Base shape
const basePersonShape = shacl.generateShape('basic_node_shape', {
  shapeName: 'BasePerson',
  properties: [
    { path: 'name', datatype: 'xsd:string', minCount: 1 }
  ]
});

// Extended shape
const employeeShape = shacl.generateShape('basic_node_shape', {
  shapeName: 'Employee',
  targetClass: 'Employee',
  properties: [
    { path: 'employeeId', datatype: 'xsd:string', minCount: 1 }
  ]
});
// Add: sh:node ex:BasePersonShape ; to manually extend
```

### Technique 2: Dynamic Shape Generation

```javascript
function generateShapeForClass(className, requiredProps, optionalProps) {
  return shacl.generateShape('component_shape', {
    shapeName: className,
    targetClass: className,
    requiredProperties: requiredProps.map(p => ({
      name: p,
      datatype: 'xsd:string',
      minCount: 1
    })),
    optionalProperties: optionalProps.map(p => ({
      name: p,
      datatype: 'xsd:string'
    }))
  });
}

const userShape = generateShapeForClass(
  'User',
  ['username', 'email'],
  ['firstName', 'lastName', 'bio']
);
```

### Technique 3: Batch Validation

```javascript
async function validateMultipleFiles(files, shapesFile) {
  const results = [];

  for (const file of files) {
    const result = await validateRDF(file, shapesFile);
    results.push({
      file,
      conforms: result.conforms,
      violations: result.violations.length
    });
  }

  return results;
}

const results = await validateMultipleFiles(
  ['data1.ttl', 'data2.ttl', 'data3.ttl'],
  'shapes.ttl'
);

results.forEach(r => {
  console.log(`${r.file}: ${r.conforms ? '✅' : '❌'} (${r.violations} violations)`);
});
```

## Related Guides

- [Generate RDF from Templates](./generate-rdf-from-templates.md)
- [Build SPARQL Queries](./build-sparql-queries.md)
- [Optimize RDF Serialization](./optimize-rdf-serialization.md)

## Reference

- [SHACL Validator API](../reference/shacl-validator-api.md)
- [RDF-KGN API](../reference/rdf-kgn-api.md)
- [RDF-KGN Architecture](../explanation/rdf-kgn-architecture.md)
