# Projection API Reference

API Reference for `@unrdf/projection`

---

## Table of Contents

### Classes
- [JsDocProjector](#jsdocprojector)
- [ArchitectureProjector](#architectureprojector)
- [ExampleProjector](#exampleprojector)
- [GuideProjector](#guideprojector)
- [ChangelogProjector](#changelogprojector)
- [DiataxisRenderer](#diataxisrenderer)
- [ProjectionPipeline](#projectionpipeline)

### Functions
- [createPipeline](#createpipeline)
- [projectApiDoc](#projectapidoc)
- [projectExamples](#projectexamples)
- [projectChangelog](#projectchangelog)

---

## JsDocProjector

`Class`

Transforms source code JSDoc comments to Markdown API documentation.

**Constructor:**

| Name | Type | Description |
|------|------|-------------|
| `options` (optional) | `Object` | Projector options |
| `options.includePrivate` (optional) | `boolean` | Include private members (default: false) |
| `options.includeExamples` (optional) | `boolean` | Include examples in output (default: true) |
| `options.headingPrefix` (optional) | `string` | Markdown heading prefix (default: '##') |

**Methods:**

### parseJsDoc

Parse JSDoc comments from source code.

```javascript
const entries = projector.parseJsDoc(sourceContent);
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `sourceContent` | `string` | JavaScript/MJS source content |

**Returns:** `Array<Object>` - Parsed JSDoc entries

### project

Project source content to Markdown documentation.

```javascript
const result = await projector.project(sourceContent, moduleName);
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `sourceContent` | `string` | JavaScript source content |
| `moduleName` | `string` | Module name |

**Returns:** `Promise<{markdown: string, hash: string, entries: Array}>` - Projection result

### projectMultiple

Project multiple source files.

```javascript
const result = await projector.projectMultiple(sources);
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `sources` | `Array<{content: string, name: string}>` | Source files |

**Returns:** `Promise<{modules: Array, combinedHash: string}>` - Projection results

### verifyDeterminism

Verify projection determinism.

```javascript
const isValid = await projector.verifyDeterminism(sourceContent, moduleName, expectedHash);
```

**Returns:** `Promise<boolean>` - True if hash matches

**Example:**

```javascript
import { JsDocProjector } from '@unrdf/projection';

const projector = new JsDocProjector();
const result = await projector.project(fs.readFileSync('module.mjs', 'utf-8'), 'my-module');

console.log(result.markdown);  // Markdown API documentation
console.log(result.hash);      // Deterministic content hash
console.log(result.entries);   // Parsed JSDoc entries
```

---

## ArchitectureProjector

`Class`

Transforms RDF partition structure to architecture documentation.

**Constructor:**

| Name | Type | Description |
|------|------|-------------|
| `options` (optional) | `Object` | Projector options |
| `options.includeMermaid` (optional) | `boolean` | Generate Mermaid diagrams (default: true) |
| `options.includeMetrics` (optional) | `boolean` | Include size metrics (default: true) |

**Methods:**

### project

Project architecture input to Markdown documentation.

```javascript
const result = await projector.project(input);
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `input.partitions` | `Array<Object>` | Partition metadata array |
| `input.dependencies` | `Array<Object>` | Dependency edges |
| `input.namespaces` | `Object` | Namespace to partition mapping |
| `input.metadata` | `Object` | Project metadata |

**Returns:** `Promise<{markdown: string, hash: string}>` - Projection result

### extractFromUniverse

Extract architecture input from Universe instance.

```javascript
const input = projector.extractFromUniverse(universe);
```

**Example:**

```javascript
import { ArchitectureProjector } from '@unrdf/projection';

const projector = new ArchitectureProjector();
const input = projector.extractFromUniverse(universe);
const result = await projector.project(input);

console.log(result.markdown);  // Architecture documentation with Mermaid diagrams
```

---

## ExampleProjector

`Class`

Transforms test files to executable example documentation.

**Methods:**

### parseTestFile

Parse test file and extract examples.

```javascript
const examples = projector.parseTestFile(testContent);
```

### project

Project test file to example documentation.

```javascript
const result = await projector.project(testContent, fileName);
```

**Returns:** `Promise<{markdown: string, hash: string, examples: Array}>` - Projection result

**Example:**

```javascript
import { ExampleProjector } from '@unrdf/projection';

const projector = new ExampleProjector();
const result = await projector.project(
  fs.readFileSync('universe.test.mjs', 'utf-8'),
  'universe.test.mjs'
);

console.log(result.exampleCount);  // Number of examples extracted
```

---

## GuideProjector

`Class`

Generates how-to guides from templates and patterns.

**Methods:**

### getAvailableTemplates

Get list of available guide templates.

```javascript
const templates = projector.getAvailableTemplates();
// ['create-partition', 'add-ontology', 'create-receipt', 'query-universe', 'project-documentation']
```

### generateFromTemplate

Generate guide from template.

```javascript
const guide = projector.generateFromTemplate('create-partition', {
  description: 'How to create a custom partition',
  code: ['// Step 1 code', '// Step 2 code'],
});
```

### project

Project guides to Markdown documentation.

```javascript
const result = await projector.project(guides);
```

**Example:**

```javascript
import { GuideProjector } from '@unrdf/projection';

const projector = new GuideProjector();
const guide = projector.generateFromTemplate('query-universe', {});
const result = await projector.project([guide]);

console.log(result.index.markdown);  // Index of all guides
console.log(result.guides[0].markdown);  // Individual guide content
```

---

## ChangelogProjector

`Class`

Transforms governance receipts to changelog documentation.

**Constructor:**

| Name | Type | Description |
|------|------|-------------|
| `options.format` (optional) | `string` | Output format: 'markdown' or 'json' |
| `options.groupByDate` (optional) | `boolean` | Group entries by date (default: true) |

**Methods:**

### project

Project receipts to changelog documentation.

```javascript
const result = await projector.project(receipts, metadata);
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `receipts` | `Array<Object>` | Receipt objects |
| `metadata.projectName` | `string` | Project name |
| `metadata.version` | `string` | Project version |

**Returns:** `Promise<{markdown: string, hash: string, entries: Array}>` - Projection result

### generateSummary

Generate summary statistics.

```javascript
const summary = projector.generateSummary(entries);
// { total: 10, byDecision: { allow: 8, deny: 2 }, approvalRate: '80.0%' }
```

**Example:**

```javascript
import { ChangelogProjector } from '@unrdf/projection';

const projector = new ChangelogProjector();
const result = await projector.project(receiptChain.getAll(), {
  projectName: 'MyProject',
  version: '1.0.0',
});

console.log(result.markdown);  // Changelog in Markdown format
console.log(result.summary);   // { totalEntries, allowCount, denyCount }
```

---

## DiataxisRenderer

`Class`

Routes documentation to Diataxis quadrants (Tutorial, How-to, Reference, Explanation).

**Constructor:**

| Name | Type | Description |
|------|------|-------------|
| `config.projectName` | `string` | Project name |
| `config.version` (optional) | `string` | Project version |
| `config.audiences` (optional) | `Array<string>` | Target audiences |

**Methods:**

### classifyDocument

Classify document to Diataxis quadrant.

```javascript
const quadrant = renderer.classifyDocument(doc);
// 'tutorial', 'how-to', 'reference', or 'explanation'
```

### render

Render all documentation with Diataxis structure.

```javascript
const result = await renderer.render(documents);
```

**Returns:**

```javascript
{
  mainIndex: string,      // Main documentation index
  navigation: Object,     // Navigation structure
  quadrants: {            // By quadrant
    tutorial: { index, documents },
    'how-to': { index, documents },
    reference: { index, documents },
    explanation: { index, documents },
  },
  audiences: {            // By audience
    user: string,
    contributor: string,
  },
  hash: string,           // Combined hash
}
```

**Example:**

```javascript
import { DiataxisRenderer } from '@unrdf/projection';

const renderer = new DiataxisRenderer({
  projectName: 'UNRDF',
  version: '1.0.0',
  audiences: ['user', 'contributor'],
});

const result = await renderer.render(documents);
console.log(result.mainIndex);  // Main documentation index
```

---

## ProjectionPipeline

`Class`

Unified pipeline orchestrating all projectors.

**Constructor:**

| Name | Type | Description |
|------|------|-------------|
| `config.projectName` | `string` | Project name |
| `config.version` (optional) | `string` | Project version |
| `config.audiences` (optional) | `Array<string>` | Target audiences |
| `config.projectors` (optional) | `Object` | Enable/disable projectors |

**Methods:**

### run

Run complete projection pipeline.

```javascript
const result = await pipeline.run({
  sources: [
    { name: 'module.mjs', content: '...', type: 'source' },
    { name: 'module.test.mjs', content: '...', type: 'test' },
  ],
  universe: universe,
  receipts: receiptChain.getAll(),
  guides: [],
  customDocs: [],
});
```

**Returns:**

```javascript
{
  inputHash: string,      // Hash of all inputs
  outputHash: string,     // Combined output hash
  deterministic: boolean, // Verification passed
  projections: {          // Individual projection results
    jsdoc, architecture, examples, guides, changelog
  },
  diataxis: Object,       // Diataxis-structured output
  receipts: Array,        // Projection receipts
  summary: {
    sourceCount, testCount, documentCount, guideCount, exampleCount
  },
}
```

### generateReport

Generate pipeline summary report.

```javascript
const report = pipeline.generateReport(result);
```

### verifyDeterminism

Verify pipeline determinism.

```javascript
const isValid = await pipeline.verifyDeterminism(input, expectedHash);
```

**Example:**

```javascript
import { ProjectionPipeline } from '@unrdf/projection';

const pipeline = new ProjectionPipeline({
  projectName: 'UNRDF',
  version: '1.0.0',
});

const result = await pipeline.run({
  sources: [
    { name: 'universe.mjs', content: fs.readFileSync('universe.mjs', 'utf-8'), type: 'source' },
  ],
});

console.log(result.outputHash);     // Deterministic combined hash
console.log(result.deterministic);  // true
console.log(pipeline.generateReport(result));  // Markdown report
```

---

## createPipeline

`Function`

Create a standard projection pipeline with default configuration.

```javascript
const pipeline = createPipeline(projectName, version);
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `projectName` | `string` | Project name |
| `version` (optional) | `string` | Project version |

**Returns:** `ProjectionPipeline` - Configured pipeline

---

## projectApiDoc

`Function`

Quick project a single source file to API documentation.

```javascript
const { markdown, hash } = await projectApiDoc(sourceContent, moduleName);
```

**Parameters:**

| Name | Type | Description |
|------|------|-------------|
| `sourceContent` | `string` | Source file content |
| `moduleName` | `string` | Module name |

**Returns:** `Promise<{markdown: string, hash: string}>` - API documentation

---

## projectExamples

`Function`

Quick project test file to examples documentation.

```javascript
const { markdown, hash } = await projectExamples(testContent, fileName);
```

---

## projectChangelog

`Function`

Quick project receipts to changelog.

```javascript
const { markdown, hash } = await projectChangelog(receipts, metadata);
```

---

*Generated by @unrdf/projection*
