# Projection Examples

Executable examples extracted from `projection.test.mjs`

---

## Table of Contents

- [JsDocProjector](#jsdocprojector) (5 examples)
- [ArchitectureProjector](#architectureprojector) (3 examples)
- [ExampleProjector](#exampleprojector) (4 examples)
- [GuideProjector](#guideprojector) (4 examples)
- [ChangelogProjector](#changelogprojector) (4 examples)
- [DiataxisRenderer](#diataxisrenderer) (5 examples)
- [ProjectionPipeline](#projectionpipeline) (5 examples)

---

## JsDocProjector

Examples for the JsDocProjector class.

### Parse JSDoc Comments

Extract JSDoc entries from source code.

*Tags: `async`*

```javascript
import { JsDocProjector } from '@unrdf/projection';

const projector = new JsDocProjector();
const source = `
/**
 * Add two numbers
 * @param {number} a - First number
 * @param {number} b - Second number
 * @returns {number} Sum
 */
export function add(a, b) {
  return a + b;
}
`;

const entries = projector.parseJsDoc(source);
console.log(entries.length);  // 1
console.log(entries[0].name); // 'add'
```

---

### Extract Parameters

Parse function parameters from JSDoc.

```javascript
const entries = projector.parseJsDoc(source);
const addFn = entries.find(e => e.name === 'add');

console.log(addFn.params.length);    // 2
console.log(addFn.params[0].name);   // 'a'
console.log(addFn.params[0].type);   // 'number'
```

---

### Generate Deterministic Output

Verify that same input produces same hash.

```javascript
const result1 = await projector.project(source, 'math');
const result2 = await projector.project(source, 'math');

console.log(result1.hash === result2.hash); // true
```

---

### Generate Markdown with TOC

Create API documentation with table of contents.

```javascript
const result = await projector.project(source, 'math');

console.log(result.markdown.includes('# math'));           // true
console.log(result.markdown.includes('Table of Contents')); // true
console.log(result.markdown.includes('```javascript'));    // true
```

---

### Project Multiple Files

Process multiple source files at once.

```javascript
const sources = [
  { name: 'math.mjs', content: mathSource },
  { name: 'string.mjs', content: stringSource },
];

const result = await projector.projectMultiple(sources);

console.log(result.modules.length);   // 2
console.log(result.combinedHash);     // Combined deterministic hash
console.log(result.totalEntries);     // Total JSDoc entries
```

---

## ArchitectureProjector

Examples for the ArchitectureProjector class.

### Generate Partition Documentation

Document RDF partition structure.

```javascript
import { ArchitectureProjector } from '@unrdf/projection';

const projector = new ArchitectureProjector();
const input = {
  partitions: [
    {
      name: 'IndustrialSubstrate',
      description: 'Foundational W3C ontologies',
      readOnly: true,
      namespaceIris: ['http://www.w3.org/ns/prov#'],
      protectedNamespaces: [],
      size: 1000,
    },
  ],
  dependencies: [],
  namespaces: {},
};

const result = await projector.project(input);
console.log(result.markdown.includes('IndustrialSubstrate')); // true
console.log(result.partitionCount); // 1
```

---

### Generate Mermaid Diagrams

Create visual dependency diagrams.

```javascript
const input = {
  partitions: [
    { name: 'P1', description: 'Part 1', readOnly: false, namespaceIris: [], protectedNamespaces: [], size: 10 },
    { name: 'P2', description: 'Part 2', readOnly: true, namespaceIris: [], protectedNamespaces: [], size: 20 },
  ],
  dependencies: [
    { from: 'P1', to: 'P2', type: 'depends-on' },
  ],
};

const result = await projector.project(input);

console.log(result.markdown.includes('```mermaid')); // true
console.log(result.markdown.includes('graph TD'));   // true
console.log(result.markdown.includes('P1 --> P2'));  // true
```

---

### Extract from Universe

Get architecture input from Universe instance.

```javascript
import { Universe } from '@unrdf/universe';

const universe = new Universe();
const input = projector.extractFromUniverse(universe);

console.log(input.partitions.length); // 6
console.log(input.metadata.name);     // 'UNRDF Universe'
```

---

## ExampleProjector

Examples for the ExampleProjector class.

### Extract Test Cases

Parse test file to extract examples.

```javascript
import { ExampleProjector } from '@unrdf/projection';

const projector = new ExampleProjector();
const testCode = `
describe('Calculator', () => {
  it('should add numbers', () => {
    expect(add(1, 2)).toBe(3);
  });
});
`;

const examples = projector.parseTestFile(testCode);
console.log(examples.length);           // 1
console.log(examples[0].name);          // 'should add numbers'
console.log(examples[0].category);      // 'Calculator'
```

---

### Group by Category

Organize examples by describe block.

```javascript
const examples = projector.parseTestFile(testCode);
const categories = projector.groupByCategory(examples);

console.log(categories.length);           // 1
console.log(categories[0].name);          // 'Calculator'
console.log(categories[0].examples.length); // 1
```

---

### Generate Example Documentation

Project test file to Markdown.

```javascript
const result = await projector.project(testCode, 'calc.test.mjs');

console.log(result.exampleCount);   // 1
console.log(result.categoryCount);  // 1
console.log(result.markdown.includes('```javascript')); // true
```

---

### Project Multiple Test Files

Process all test files at once.

```javascript
const testFiles = [
  { name: 'calc.test.mjs', content: calcTest },
  { name: 'string.test.mjs', content: stringTest },
];

const result = await projector.projectMultiple(testFiles);

console.log(result.files.length);     // 2
console.log(result.totalExamples);    // Total examples across files
console.log(result.combinedHash);     // Deterministic combined hash
```

---

## GuideProjector

Examples for the GuideProjector class.

### List Available Templates

Get built-in guide templates.

```javascript
import { GuideProjector } from '@unrdf/projection';

const projector = new GuideProjector();
const templates = projector.getAvailableTemplates();

console.log(templates);
// ['create-partition', 'add-ontology', 'create-receipt', 'query-universe', 'project-documentation']
```

---

### Generate from Template

Create guide from built-in template.

```javascript
const guide = projector.generateFromTemplate('create-partition', {
  description: 'Create a partition for customer data',
  prerequisites: ['Node.js 18+'],
});

console.log(guide.title);         // 'Creating a Custom Partition'
console.log(guide.steps.length);  // 4
console.log(guide.category);      // 'RDF Management'
```

---

### Customize Template Steps

Add code and notes to template.

```javascript
const guide = projector.generateFromTemplate('create-partition', {
  code: [
    'class CustomerPartition extends Partition { }',
    'super({ name: "CustomerData" })',
  ],
  notes: [
    ['Partitions can be read-only or writable'],
    ['Namespace IRIs define allowed data'],
  ],
});

console.log(guide.steps[0].code);   // First step code
console.log(guide.steps[0].notes);  // First step notes
```

---

### Project Guides to Markdown

Generate complete guide documentation.

```javascript
const guide = projector.generateFromTemplate('query-universe', {});
const result = await projector.project([guide]);

console.log(result.guides.length);        // 1
console.log(result.index.markdown);       // Index page
console.log(result.guides[0].markdown);   // Guide content
console.log(result.combinedHash);         // Deterministic hash
```

---

## ChangelogProjector

Examples for the ChangelogProjector class.

### Parse Receipt to Entry

Convert governance receipt to changelog entry.

```javascript
import { ChangelogProjector } from '@unrdf/projection';

const projector = new ChangelogProjector();
const receipt = {
  receiptHash: 'abc123...',
  decision: 'allow',
  epoch: 'τ_2025_12_26_1200_000',
  generatedAtTime: '2025-12-26T12:00:00.000Z',
  inputHashes: {
    ontologyReleases: ['hash1'],
    deltaCapsule: 'deltahash',
  },
  outputHash: 'output...',
  beforeHash: null,
  toolchainVersion: { node: 'v22.0.0', packages: {} },
};

const entry = projector.parseReceipt(receipt);

console.log(entry.decision); // 'allow'
console.log(entry.date);     // '2025-12-26'
console.log(entry.epoch);    // 'τ_2025_12_26_1200_000'
```

---

### Generate Markdown Changelog

Create changelog from receipts.

```javascript
const result = await projector.project([receipt], {
  projectName: 'MyProject',
  version: '1.0.0',
});

console.log(result.markdown.includes('# Changelog'));  // true
console.log(result.markdown.includes('[ALLOW]'));      // true
console.log(result.entries.length);                    // 1
```

---

### Generate Summary Statistics

Get changelog statistics.

```javascript
const entries = [
  projector.parseReceipt(allowReceipt),
  projector.parseReceipt(denyReceipt),
];

const summary = projector.generateSummary(entries);

console.log(summary.total);            // 2
console.log(summary.byDecision.allow); // 1
console.log(summary.byDecision.deny);  // 1
console.log(summary.approvalRate);     // '50.0%'
```

---

### Output as JSON

Generate changelog in JSON format.

```javascript
const jsonProjector = new ChangelogProjector({ format: 'json' });
const result = await jsonProjector.project([receipt], {
  projectName: 'MyProject',
});

const changelog = JSON.parse(result.json);
console.log(changelog.entries.length);   // 1
console.log(changelog.summary.totalEntries); // 1
```

---

## DiataxisRenderer

Examples for the DiataxisRenderer class.

### Classify Documents

Automatically classify documents to quadrants.

```javascript
import { DiataxisRenderer } from '@unrdf/projection';

const renderer = new DiataxisRenderer({
  projectName: 'UNRDF',
  version: '1.0.0',
});

const tutorialDoc = {
  title: 'Getting Started',
  content: 'In this tutorial you will learn...',
};
console.log(renderer.classifyDocument(tutorialDoc)); // 'tutorial'

const referenceDoc = {
  title: 'API Reference',
  content: '@param x - the value\n@returns result',
};
console.log(renderer.classifyDocument(referenceDoc)); // 'reference'
```

---

### Render with Navigation

Generate documentation with navigation structure.

```javascript
const docs = [
  { id: 'intro', title: 'Introduction', content: '...', quadrant: 'tutorial' },
  { id: 'api', title: 'API', content: '...', quadrant: 'reference' },
];

const result = await renderer.render(docs);

console.log(result.navigation.tutorial.items.length); // 1
console.log(result.navigation.reference.items.length); // 1
```

---

### Generate Main Index

Create documentation index page.

```javascript
const result = await renderer.render(docs);

console.log(result.mainIndex.includes('Documentation Structure')); // true
console.log(result.mainIndex.includes('Diataxis'));                // true
```

---

### Render for Audiences

Generate audience-specific views.

```javascript
const renderer = new DiataxisRenderer({
  projectName: 'UNRDF',
  audiences: ['user', 'contributor'],
});

const docs = [
  { id: 'intro', title: 'Intro', content: '...', quadrant: 'tutorial', audience: 'user' },
  { id: 'arch', title: 'Architecture', content: '...', quadrant: 'explanation', audience: 'contributor' },
];

const result = await renderer.render(docs);

console.log(result.audiences.user);        // User-specific index
console.log(result.audiences.contributor); // Contributor-specific index
```

---

### Verify Determinism

Check that rendering is deterministic.

```javascript
const result1 = await renderer.render(docs);
const result2 = await renderer.render(docs);

console.log(result1.hash === result2.hash); // true
```

---

## ProjectionPipeline

Examples for the ProjectionPipeline class.

### Run Complete Pipeline

Execute full documentation generation.

```javascript
import { ProjectionPipeline } from '@unrdf/projection';

const pipeline = new ProjectionPipeline({
  projectName: 'MyProject',
  version: '1.0.0',
});

const result = await pipeline.run({
  sources: [
    { name: 'module.mjs', content: sourceCode, type: 'source' },
    { name: 'module.test.mjs', content: testCode, type: 'test' },
  ],
  receipts: [receipt],
});

console.log(result.outputHash);              // Combined hash
console.log(result.summary.sourceCount);     // 1
console.log(result.summary.testCount);       // 1
console.log(result.summary.documentCount);   // Total documents
```

---

### Access Projection Results

Get individual projector outputs.

```javascript
const result = await pipeline.run(input);

// API documentation
console.log(result.projections.jsdoc.modules[0].markdown);

// Examples
console.log(result.projections.examples.files[0].markdown);

// Changelog
console.log(result.projections.changelog.markdown);
```

---

### Get Projection Receipts

Access audit trail of projections.

```javascript
const result = await pipeline.run(input);

for (const receipt of result.receipts) {
  console.log(receipt.projectorType);  // 'jsdoc', 'examples', etc.
  console.log(receipt.inputHash);      // Input hash
  console.log(receipt.outputHash);     // Output hash
  console.log(receipt.documentCount);  // Documents generated
}
```

---

### Generate Pipeline Report

Create summary report.

```javascript
const result = await pipeline.run(input);
const report = pipeline.generateReport(result);

console.log(report.includes('Documentation Projection Report')); // true
console.log(report.includes('Projection Receipts'));            // true
```

---

### Verify Pipeline Determinism

Confirm reproducible output.

```javascript
const result = await pipeline.run(input);
const verified = await pipeline.verifyDeterminism(input, result.outputHash);

console.log(verified); // true
```

---

*Generated by @unrdf/projection*
