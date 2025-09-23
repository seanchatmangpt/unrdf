# UNRDF Playground

This playground provides comprehensive examples and use cases for the UNRDF (Unified RDF) framework - an opinionated composable framework for RDF knowledge operations.

## 🚀 Quick Start

1. **Install dependencies:**
   ```bash
   cd playground
   pnpm install
   ```

2. **Run examples:**
   ```bash
   # Run all examples
   pnpm examples

   # Run specific examples
   pnpm sparql        # SPARQL queries
   pnpm validation    # RDF validation
   pnpm reasoning     # OWL reasoning
   pnpm turtle        # Turtle parsing
   pnpm zod           # Zod integration
   pnpm cli-test      # CLI testing examples
   pnpm cli-scenarios # CLI scenario testing
   ```

3. **Development mode:**
   ```bash
   pnpm dev           # Watch mode for basic-usage.mjs
   ```

4. **Run tests:**
   ```bash
   pnpm test          # Run all tests
   pnpm test:watch    # Watch mode
   pnpm test:ui       # Interactive UI
   ```

## 📚 Examples Overview

### 1. Basic Usage (`examples/basic-usage.mjs`)
Demonstrates the core functionality of UNRDF:
- Creating and managing RDF stores
- Working with knowledge graphs
- Parsing and serializing Turtle data
- Basic SPARQL queries
- Zod schema integration

**Key Features:**
- `useStore()` - RDF store management
- `useGraph()` - Knowledge graph operations
- `useTurtle()` - Turtle format handling
- `useZod()` - Schema validation

### 2. SPARQL Queries (`examples/sparql-queries.mjs`)
Comprehensive SPARQL query examples:
- **SELECT queries** - Data retrieval
- **CONSTRUCT queries** - Graph construction
- **ASK queries** - Boolean checks
- **DESCRIBE queries** - Resource descriptions
- **Filtered queries** - Conditional data

**Sample Query Types:**
```sparql
# SELECT - Get books and authors
SELECT ?title ?authorName WHERE {
  ?book dc:title ?title ;
        dc:creator ?author .
  ?author foaf:name ?authorName .
}

# CONSTRUCT - Create new relationships
CONSTRUCT {
  ?book ex:hasAuthor ?authorName .
} WHERE {
  ?book dc:creator ?author .
  ?author foaf:name ?authorName .
}
```

### 3. Validation (`examples/validation.mjs`)
RDF data validation using SHACL:
- **SHACL shapes** - Define validation rules
- **Property constraints** - Data type and range validation
- **Custom validators** - Application-specific rules
- **Error reporting** - Detailed validation results

**Example SHACL Shape:**
```turtle
ex:PersonShape a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path foaf:name ;
    sh:datatype xsd:string ;
    sh:minCount 1 ;
  ] .
```

### 4. Reasoning (`examples/reasoning.mjs`)
OWL reasoning and inference:
- **OWL ontologies** - Class and property definitions
- **Inference engines** - EyeReasoner integration
- **Transitive properties** - Hierarchical relationships
- **Inverse properties** - Bidirectional relationships
- **Custom rules** - Application-specific inference

**Reasoning Features:**
- Subclass hierarchies
- Property transitivity
- Inverse relationships
- Custom rule-based inference

### 5. Turtle Parsing (`examples/turtle-parsing.mjs`)
Advanced Turtle RDF format handling:
- **Prefix management** - Namespace handling
- **Collections** - RDF lists and sequences
- **Blank nodes** - Anonymous resources
- **Datatypes** - XML Schema datatypes
- **Language tags** - Multilingual content

**Turtle Features:**
```turtle
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:product ex:name "Laptop"@en ;
           ex:price "999.99"^^xsd:decimal ;
           ex:tags ( "electronics" "computers" ) .
```

### 6. Zod Integration (`examples/zod-integration.mjs`)
Type-safe RDF operations with Zod:
- **Schema definition** - Type-safe data structures
- **RDF conversion** - Bidirectional data transformation
- **Validation** - Runtime type checking
- **Schema evolution** - Backward compatibility
- **Custom transforms** - Data processing

**Example Schema:**
```javascript
const PersonSchema = z.object({
  id: z.string().url(),
  name: z.string().min(1),
  age: z.number().int().min(0).max(150).optional(),
  email: z.string().email().optional()
});
```

### 7. CLI Testing (`examples/cli-testing.mjs`)
CLI testing using [citty-test-utils](https://github.com/seanchatmangpt/citty-test-utils):
- **Basic CLI operations** - Help, version, error handling
- **Command testing** - Parse, query, validate, reason
- **Output validation** - JSON, table, turtle formats
- **Error handling** - Invalid commands, missing files
- **Performance testing** - Operation timing
- **Cleanroom testing** - Docker-based isolated testing

**Example CLI Test:**
```javascript
const result = await runLocalCitty(['--help'], {
  cwd: './playground',
  env: { DEBUG: 'true' }
});

result
  .expectSuccess()
  .expectOutput('UNRDF Command Line Interface')
  .expectNoStderr();
```

### 8. CLI Scenarios (`examples/cli-scenarios.mjs`)
Advanced scenario-based testing:
- **Multi-step workflows** - Complex command sequences
- **Scenario builders** - Fluent API for test scenarios
- **Pre-built scenarios** - Reusable test patterns
- **Custom actions** - Application-specific logic
- **Environment comparison** - Local vs cleanroom testing
- **Retry mechanisms** - Resilience testing

**Example Scenario:**
```javascript
const result = await scenario('RDF Processing Pipeline')
  .step('Parse RDF data')
  .run('parse', './test-data/sample.ttl')
  .expectSuccess()
  .step('Query the parsed data')
  .run('query', './test-data/sample.ttl', '--query', 'SELECT ?name WHERE { ?person foaf:name ?name }')
  .expectSuccess()
  .execute('local', { cwd: './playground' });
```

## 🏗️ Architecture

The playground demonstrates UNRDF's composable architecture:

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   useStore()    │    │   useGraph()    │    │  useTurtle()    │
│                 │    │                 │    │                 │
│ • RDF Storage   │    │ • SPARQL Queries│    │ • Parse/Serialize│
│ • Quad Management│    │ • Graph Ops     │    │ • Format Support│
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                 │
         ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
         │ useValidator()  │    │ useReasoner()   │    │   useZod()      │
         │                 │    │                 │    │                 │
         │ • SHACL Shapes  │    │ • OWL Reasoning │    │ • Type Safety   │
         │ • Validation    │    │ • Inference     │    │ • Schema Def    │
         └─────────────────┘    └─────────────────┘    └─────────────────┘
```

## 🔧 Configuration

The playground uses a local reference to the main UNRDF package and includes CLI testing utilities:

```json
{
  "dependencies": {
    "unrdf": "file:../",
    "citty-test-utils": "^1.0.0",
    "vitest": "^3.2.4"
  }
}
```

This allows for:
- **Live development** - Changes to UNRDF are immediately available
- **Version consistency** - Always uses the latest local version
- **Easy testing** - Test new features before publishing
- **CLI testing** - Comprehensive command-line testing with citty-test-utils
- **Test automation** - Vitest integration for automated testing

## 📖 Usage Patterns

### 1. Basic RDF Operations
```javascript
import { useStore, useGraph, useTurtle } from 'unrdf';

const store = useStore();
const graph = useGraph(store);
const turtle = useTurtle();

// Parse and add data
const quads = await turtle.parse(turtleData);
await graph.addQuads(quads);

// Query data
const results = await graph.query(sparqlQuery);
```

### 2. Validation Workflow
```javascript
import { useValidator } from 'unrdf';

const validator = useValidator();
const results = await validator.validate(dataQuads, shapeQuads);

if (!results.conforms) {
  console.log('Validation errors:', results.results);
}
```

### 3. Type-Safe Operations
```javascript
import { useZod } from 'unrdf';
import { z } from 'zod';

const zodHelper = useZod();
const schema = z.object({ name: z.string() });

// Convert to RDF
const quads = zodHelper.toRdf(data, schema);

// Convert from RDF
const structured = zodHelper.fromRdf(quads, schema);
```

### 4. CLI Testing
```javascript
import { runLocalCitty, scenario } from 'citty-test-utils';

// Basic CLI test
const result = await runLocalCitty(['--help'], {
  cwd: './playground',
  env: { DEBUG: 'true' }
});

result
  .expectSuccess()
  .expectOutput('UNRDF Command Line Interface')
  .expectNoStderr();

// Scenario-based testing
const scenarioResult = await scenario('RDF Workflow')
  .step('Parse data')
  .run('parse', './data.ttl')
  .expectSuccess()
  .step('Query data')
  .run('query', './data.ttl', '--query', 'SELECT ?name WHERE { ?person foaf:name ?name }')
  .expectSuccess()
  .execute('local', { cwd: './playground' });
```

## 🧪 Testing

The playground includes comprehensive testing capabilities:

### **Example Testing**
Each example can be run independently to test specific functionality:

```bash
# Test SPARQL functionality
pnpm sparql

# Test validation
pnpm validation

# Test reasoning
pnpm reasoning

# Test CLI functionality
pnpm cli-test

# Test CLI scenarios
pnpm cli-scenarios
```

### **Automated Testing**
The playground includes a full test suite using Vitest:

```bash
# Run all tests
pnpm test

# Run tests in watch mode
pnpm test:watch

# Run tests with interactive UI
pnpm test:ui
```

### **CLI Testing Features**
The CLI testing examples demonstrate:

- **Basic Operations** - Help, version, error handling
- **Command Testing** - Parse, query, validate, reason
- **Output Validation** - JSON, table, turtle formats
- **Error Handling** - Invalid commands, missing files
- **Performance Testing** - Operation timing
- **Cleanroom Testing** - Docker-based isolated testing
- **Scenario Testing** - Multi-step workflows
- **Retry Mechanisms** - Resilience testing

### **Test Structure**
```
test/
├── cli.test.mjs          # Comprehensive CLI tests
└── README.md             # Test documentation

examples/
├── cli-testing.mjs       # CLI testing examples
├── cli-scenarios.mjs     # Scenario-based testing
└── ...                   # Other examples
```

## 📝 Development

To add new examples:

1. Create a new `.mjs` file in the `examples/` directory
2. Add a script entry to `package.json`
3. Follow the established patterns:
   - Use JSDoc for documentation
   - Include error handling
   - Provide clear console output
   - Demonstrate key features

## 🔗 Related Resources

- [UNRDF Main Package](../README.md)
- [RDF 1.1 Specification](https://www.w3.org/TR/rdf11-concepts/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [SHACL Specification](https://www.w3.org/TR/shacl/)
- [Zod Documentation](https://zod.dev/)
- [citty-test-utils](https://github.com/seanchatmangpt/citty-test-utils) - CLI testing framework
- [Vitest Documentation](https://vitest.dev/) - Testing framework
- [Citty Documentation](https://citty.dev/) - CLI framework

## 📄 License

MIT License - see the main package for details.
