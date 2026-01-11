# RDF-KGN API Reference

Complete API reference for RDF-KGN integration.

## Core Modules

### `@unrdf/kgn/src/doc-generator/rdf-builder`

Converts parsed JSDoc data to RDF triples.

#### `buildRDFGraph(parsedFile)`

Build RDF graph from parsed file data.

**Parameters:**
- `parsedFile` (Object) - Output from `parser.parseFile()`
  - `relativePath` (string) - Relative path to file
  - `file` (string) - Absolute file path
  - `exports` (Array) - Exported functions, classes, variables
  - `imports` (Array) - Import statements
  - `comments` (number) - Number of comments

**Returns:** `string` - Turtle RDF representation

**Example:**

```javascript
import { parseFile } from '@unrdf/kgn/src/doc-generator/parser.mjs';
import { buildRDFGraph } from '@unrdf/kgn/src/doc-generator/rdf-builder.mjs';

const parsed = parseFile('/path/to/file.mjs', '/project/root');
const rdf = buildRDFGraph(parsed);

console.log(rdf);
// @prefix code: <http://unrdf.org/vocab/code#> .
// ...
```

#### `buildRDFGraphs(parsedFiles)`

Build RDF graph for multiple files.

**Parameters:**
- `parsedFiles` (Array<Object>) - Array of parsed file data

**Returns:** `string` - Combined Turtle RDF with shared prefixes

**Example:**

```javascript
const files = [
  parseFile('/path/to/file1.mjs', rootDir),
  parseFile('/path/to/file2.mjs', rootDir)
];

const combinedRDF = buildRDFGraphs(files);
```

#### `buildJSONLD(parsedFile)`

Build JSON-LD representation of parsed file.

**Parameters:**
- `parsedFile` (Object) - Parsed file data

**Returns:** `Object` - JSON-LD document

**Example:**

```javascript
const jsonld = buildJSONLD(parsed);

console.log(jsonld);
// {
//   "@context": { ... },
//   "@id": "http://unrdf.org/packages/...",
//   "@type": ["fs:File", "code:Module"],
//   "code:exports": [...]
// }
```

#### `PREFIXES`

Standard RDF vocabulary prefixes.

**Type:** `Object<string, string>`

**Prefixes:**
- `rdf`: `http://www.w3.org/1999/02/22-rdf-syntax-ns#`
- `rdfs`: `http://www.w3.org/2000/01/rdf-schema#`
- `xsd`: `http://www.w3.org/2001/XMLSchema#`
- `code`: `http://unrdf.org/vocab/code#`
- `fs`: `http://unrdf.org/vocab/fs#`
- `doc`: `http://unrdf.org/vocab/doc#`
- `ex`: `http://example.org/`

**Example:**

```javascript
import { PREFIXES } from '@unrdf/kgn/src/doc-generator/rdf-builder.mjs';

console.log(PREFIXES.code); // http://unrdf.org/vocab/code#
```

### `@unrdf/kgn/src/doc-generator/parser`

Extracts JSDoc from JavaScript source files.

#### `parseFile(filePath, rootDir)`

Parse JSDoc comments and AST from a source file.

**Parameters:**
- `filePath` (string) - Absolute path to source file
- `rootDir` (string) - Workspace root directory (default: `process.cwd()`)

**Returns:** `Object`
- `file` (string) - Absolute file path
- `relativePath` (string) - Relative path from root
- `exports` (Array) - Exported entities
- `imports` (Array) - Import statements
- `comments` (number) - Number of JSDoc comments

**Example:**

```javascript
import { parseFile } from '@unrdf/kgn/src/doc-generator/parser.mjs';

const result = parseFile(
  '/home/user/project/src/utils.mjs',
  '/home/user/project'
);

console.log(result.relativePath); // src/utils.mjs
console.log(result.exports.length); // 5
```

#### `parseFiles(filePaths, rootDir)`

Parse multiple files in batch.

**Parameters:**
- `filePaths` (Array<string>) - Array of file paths
- `rootDir` (string) - Workspace root directory

**Returns:** `Array<Object>` - Parsed data for all files

**Example:**

```javascript
const files = [
  '/project/src/file1.mjs',
  '/project/src/file2.mjs'
];

const results = parseFiles(files, '/project');
results.forEach(r => {
  if (!r.error) {
    console.log(`Parsed ${r.relativePath}: ${r.exports.length} exports`);
  }
});
```

### `@unrdf/kgn/src/base/shacl-templates`

SHACL shape template generation.

#### `KGenSHACLTemplates`

Class for generating SHACL validation shapes from templates.

##### Constructor

```javascript
new KGenSHACLTemplates(options)
```

**Parameters:**
- `options` (Object)
  - `deterministicMode` (boolean) - Use deterministic rendering (default: `true`)
  - `staticBuildTime` (string) - Static timestamp for deterministic mode (default: `'2024-01-01T00:00:00.000Z'`)
  - `namespace` (string) - SHACL shapes namespace (default: `'http://kgen.ai/shacl#'`)
  - `baseIRI` (string) - Base IRI for shapes (default: `'http://kgen.ai/'`)

**Example:**

```javascript
import { KGenSHACLTemplates } from '@unrdf/kgn/src/base/shacl-templates.js';

const shacl = new KGenSHACLTemplates({
  namespace: 'http://example.org/shapes#',
  baseIRI: 'http://example.org/'
});
```

##### Methods

###### `generateShape(templateName, context)`

Generate SHACL shape from template.

**Parameters:**
- `templateName` (string) - Name of template to use
- `context` (Object) - Template variables

**Returns:** `Object`
- `name` (string) - Generated shape name
- `description` (string) - Shape description
- `content` (string) - SHACL shape in Turtle format
- `metadata` (Object) - Generation metadata

**Available Templates:**
- `basic_node_shape` - Basic node shape with properties
- `property_shape` - Property shape with constraints
- `template_shape` - Template validation shape
- `filter_shape` - Filter validation shape
- `knowledge_graph_shape` - Knowledge graph shape
- `component_shape` - Component validation shape
- `validation_rule` - Custom validation rule

**Example:**

```javascript
const shape = shacl.generateShape('basic_node_shape', {
  shapeName: 'Person',
  targetClass: 'Person',
  properties: [
    {
      path: 'name',
      datatype: 'xsd:string',
      minCount: 1,
      message: 'Name is required'
    }
  ]
});

console.log(shape.content); // SHACL shape in Turtle
```

###### `generateValidationFile(shapes, filename)`

Generate complete SHACL validation file.

**Parameters:**
- `shapes` (Array<Object>) - Array of shape definitions
  - `template` (string) - Template name
  - `context` (Object) - Template context
- `filename` (string) - Output filename (default: `'validation.ttl'`)

**Returns:** `Object`
- `filename` (string) - Output filename
- `content` (string) - Complete Turtle document
- `shapes` (Array) - Generated shapes
- `metadata` (Object) - File metadata

**Example:**

```javascript
const result = shacl.generateValidationFile([
  {
    template: 'basic_node_shape',
    context: {
      shapeName: 'Person',
      targetClass: 'Person',
      properties: [...]
    }
  },
  {
    template: 'basic_node_shape',
    context: {
      shapeName: 'Organization',
      targetClass: 'Organization',
      properties: [...]
    }
  }
], 'my-shapes.ttl');

console.log(result.content); // Complete SHACL file
```

###### `getTemplateNames()`

Get available shape template names.

**Returns:** `Array<string>` - Sorted template names

**Example:**

```javascript
const templates = shacl.getTemplateNames();
console.log(templates);
// ['basic_node_shape', 'component_shape', 'filter_shape', ...]
```

###### `getTemplate(name)`

Get shape template by name.

**Parameters:**
- `name` (string) - Template name

**Returns:** `Object|undefined` - Template definition

**Example:**

```javascript
const template = shacl.getTemplate('basic_node_shape');
console.log(template.description);
```

###### `registerShapeTemplate(name, template)`

Register a custom SHACL shape template.

**Parameters:**
- `name` (string) - Template name
- `template` (Object)
  - `name` (string) - Template display name
  - `description` (string) - Template description
  - `template` (string) - Template content (Nunjucks syntax)

**Example:**

```javascript
shacl.registerShapeTemplate('custom_shape', {
  name: 'CustomShape',
  description: 'My custom validation shape',
  template: `
    ex:{{ shapeName }}Shape
      a sh:NodeShape ;
      sh:targetClass ex:{{ targetClass }} .
  `
});

const shape = shacl.generateShape('custom_shape', {
  shapeName: 'Widget',
  targetClass: 'Widget'
});
```

### `@unrdf/kgn` (Template Engine)

Nunjucks-based template engine for RDF generation.

#### `TemplateEngine`

##### Constructor

```javascript
new TemplateEngine(options)
```

**Parameters:**
- `options` (Object)
  - `templatesDir` (string) - Templates directory path
  - `deterministicMode` (boolean) - Enable deterministic rendering
  - `strictMode` (boolean) - Enable strict mode
  - `autoescape` (boolean) - Enable HTML autoescaping (default: `false`)

**Example:**

```javascript
import { TemplateEngine } from '@unrdf/kgn';

const engine = new TemplateEngine({
  templatesDir: './templates',
  deterministicMode: true,
  strictMode: true
});
```

##### Methods

###### `render(templateName, context)`

Render a template with context.

**Parameters:**
- `templateName` (string) - Template filename
- `context` (Object) - Template variables

**Returns:** `Promise<string>` - Rendered content

**Example:**

```javascript
const result = await engine.render('person.njk', {
  name: 'Alice',
  age: 30
});

console.log(result);
```

###### `renderString(templateString, context)`

Render a template string.

**Parameters:**
- `templateString` (string) - Template content
- `context` (Object) - Template variables

**Returns:** `Promise<string>` - Rendered content

**Example:**

```javascript
const result = await engine.renderString(
  'Hello {{ name }}!',
  { name: 'Alice' }
);

console.log(result); // Hello Alice!
```

#### `renderTemplate(templatePath, context, options)`

Convenience function for one-off rendering.

**Parameters:**
- `templatePath` (string) - Path to template file
- `context` (Object) - Template variables
- `options` (Object) - Rendering options
  - `templatesDir` (string) - Templates directory
  - `deterministicMode` (boolean) - Deterministic rendering

**Returns:** `Promise<Object>`
- `content` (string) - Rendered content
- `metadata` (Object) - Rendering metadata

**Example:**

```javascript
import { renderTemplate } from '@unrdf/kgn';

const result = await renderTemplate(
  'template.njk',
  { data: 'value' },
  { templatesDir: './templates', deterministicMode: true }
);

console.log(result.content);
```

## SPARQL Execution

### `@unrdf/core/sparql/executor-sync`

Synchronous SPARQL query execution (recommended for performance).

#### `executeQuerySync(store, sparql, options)`

Execute a SPARQL query on a store.

**Parameters:**
- `store` (Store) - The RDF store to query
- `sparql` (string) - SPARQL query string
- `options` (Object) - Query options
  - `limit` (number) - Result limit
  - `offset` (number) - Result offset
  - `signal` (AbortSignal) - Abort signal

**Returns:** `Object` - Query results
- `rows` (Array) - Result bindings (for SELECT)
- `quads` (Array) - Result quads (for CONSTRUCT)
- `boolean` (boolean) - Result boolean (for ASK)
- `type` (string) - Query type

**Throws:**
- `TypeError` - If store or sparql is invalid
- `Error` - If query execution fails

**Example:**

```javascript
import { createStore } from '@unrdf/oxigraph';
import { executeQuerySync } from '@unrdf/core/sparql/executor-sync.mjs';

const store = createStore();
// ... add quads to store

const results = executeQuerySync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE { ?s foaf:name ?name }
`);

console.log('Results:', results.rows);
```

#### `executeSelectSync(store, sparql, options)`

Execute a SPARQL SELECT query.

**Parameters:**
- `store` (Store) - The store to query
- `sparql` (string) - SPARQL SELECT query string
- `options` (Object) - Query options

**Returns:** `Array<Object>` - Array of result bindings

**Example:**

```javascript
const results = executeSelectSync(store, `
  SELECT ?name ?email WHERE {
    ?s foaf:name ?name ;
       foaf:mbox ?email .
  }
`);

results.forEach(row => {
  console.log(`Name: ${row.name.value}, Email: ${row.email.value}`);
});
```

#### `executeConstructSync(store, sparql, options)`

Execute a SPARQL CONSTRUCT query.

**Parameters:**
- `store` (Store) - The store to query
- `sparql` (string) - SPARQL CONSTRUCT query string
- `options` (Object) - Query options

**Returns:** `Array` - Array of constructed quads

**Example:**

```javascript
const quads = executeConstructSync(store, `
  CONSTRUCT { ?s foaf:name ?name }
  WHERE { ?s foaf:name ?name }
`);

console.log('Constructed quads:', quads.length);
```

#### `executeAskSync(store, sparql, options)`

Execute a SPARQL ASK query.

**Parameters:**
- `store` (Store) - The store to query
- `sparql` (string) - SPARQL ASK query string
- `options` (Object) - Query options

**Returns:** `boolean` - Boolean result

**Example:**

```javascript
const exists = executeAskSync(store, `
  ASK { ?s foaf:name "Alice" }
`);

if (exists) {
  console.log('Alice exists in the store');
}
```

#### `prepareQuerySync(sparql)`

Prepare a SPARQL query (parse and validate without executing).

**Parameters:**
- `sparql` (string) - SPARQL query string

**Returns:** `Object` - Query metadata
- `type` (string) - Query type ('SELECT', 'CONSTRUCT', 'ASK', 'DESCRIBE')
- `variables` (Array<string>) - Query variables
- `prefixes` (Object) - Declared prefixes

**Example:**

```javascript
const metadata = prepareQuerySync(`
  SELECT ?s ?p ?o WHERE { ?s ?p ?o }
`);

console.log('Query type:', metadata.type); // SELECT
console.log('Variables:', metadata.variables); // ['s', 'p', 'o']
```

## RDF Store

### `@unrdf/oxigraph`

Oxigraph-based RDF store (10-100x faster than N3).

#### `createStore()`

Create a new RDF store.

**Returns:** `Store` - Oxigraph store instance

**Example:**

```javascript
import { createStore } from '@unrdf/oxigraph';
import { Parser } from 'n3';

const store = createStore();

const parser = new Parser({ format: 'text/turtle' });
parser.parse(turtleData).forEach(quad => {
  store.add(quad);
});

console.log(`Store has ${store.size} triples`);
```

## Type Definitions

### ParsedFile

```typescript
interface ParsedFile {
  file: string;
  relativePath: string;
  exports: ExportDeclaration[];
  imports: ImportDeclaration[];
  comments: number;
  error?: string;
}
```

### ExportDeclaration

```typescript
interface ExportDeclaration {
  name: string;
  type: 'function' | 'class' | 'variable';
  description?: string;
  params?: Parameter[];
  returns?: ReturnValue;
  examples?: string[];
  async?: boolean;
  methods?: Method[];
  extends?: string;
  valueType?: string;
}
```

### Parameter

```typescript
interface Parameter {
  name: string;
  type: string;
  description?: string;
  optional?: boolean;
  default?: any;
}
```

### ImportDeclaration

```typescript
interface ImportDeclaration {
  source: string;
  specifiers: ImportSpecifier[];
}
```

### ImportSpecifier

```typescript
interface ImportSpecifier {
  imported: string;
  local: string;
  type: 'ImportSpecifier' | 'ImportDefaultSpecifier' | 'ImportNamespaceSpecifier';
}
```

## See Also

- [SPARQL Query Builder API](./sparql-query-builder-api.md)
- [SHACL Validator API](./shacl-validator-api.md)
- [RDF Filters Reference](./rdf-filters-reference.md)
