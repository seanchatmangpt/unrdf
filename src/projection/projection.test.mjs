/**
 * @fileoverview Tests for Projection Module
 *
 * Tests verify:
 * - Deterministic output (same input -> same hash)
 * - Correct Markdown generation
 * - Receipt emission
 * - Diataxis classification
 */

import { describe, it, expect, beforeEach } from 'vitest';

import { JsDocProjector } from './jsdoc-projector.mjs';
import { ArchitectureProjector } from './architecture-projector.mjs';
import { ExampleProjector } from './example-projector.mjs';
import { GuideProjector } from './guide-projector.mjs';
import { ChangelogProjector } from './changelog-projector.mjs';
import { DiataxisRenderer } from './diataxis-renderer.mjs';
import { ProjectionPipeline } from './projection-pipeline.mjs';

// Sample source code for testing
const sampleSource = `
/**
 * @fileoverview Sample module for testing
 * @module sample
 */

/**
 * Add two numbers
 *
 * @param {number} a - First number
 * @param {number} b - Second number
 * @returns {number} Sum of a and b
 *
 * @example
 * const result = add(1, 2);
 * // result === 3
 */
export function add(a, b) {
  return a + b;
}

/**
 * Sample class for testing
 *
 * @class Calculator
 */
export class Calculator {
  /**
   * Create calculator
   */
  constructor() {
    this.value = 0;
  }
}
`;

// Sample test code for testing
const sampleTest = `
import { describe, it, expect } from 'vitest';
import { add, Calculator } from './sample.mjs';

describe('add function', () => {
  it('should add two positive numbers', () => {
    const result = add(1, 2);
    expect(result).toBe(3);
  });

  it('should add negative numbers', () => {
    const result = add(-1, -2);
    expect(result).toBe(-3);
  });
});

describe('Calculator class', () => {
  it('should create with default value', () => {
    const calc = new Calculator();
    expect(calc.value).toBe(0);
  });
});
`;

// Sample receipt for testing
const sampleReceipt = {
  receiptHash: 'abc123def456abc123def456abc123def456abc123def456abc123def456abcd',
  decision: 'allow',
  epoch: 'τ_2025_12_26_1200_000',
  generatedAtTime: '2025-12-26T12:00:00.000Z',
  inputHashes: {
    ontologyReleases: ['hash1', 'hash2'],
    deltaCapsule: 'deltahash123',
  },
  outputHash: 'outputhash456',
  beforeHash: null,
  toolchainVersion: {
    node: 'v22.0.0',
    packages: { zod: '^4.0.0' },
  },
};

describe('JsDocProjector', () => {
  let projector;

  beforeEach(() => {
    projector = new JsDocProjector();
  });

  it('should parse JSDoc comments', () => {
    const entries = projector.parseJsDoc(sampleSource);
    expect(entries.length).toBeGreaterThan(0);
  });

  it('should extract function parameters', () => {
    const entries = projector.parseJsDoc(sampleSource);
    const addFn = entries.find(e => e.name === 'add');
    expect(addFn).toBeDefined();
    expect(addFn.params.length).toBe(2);
    expect(addFn.params[0].name).toBe('a');
  });

  it('should extract return type', () => {
    const entries = projector.parseJsDoc(sampleSource);
    const addFn = entries.find(e => e.name === 'add');
    expect(addFn.returns).toBeDefined();
    expect(addFn.returns.type).toBe('number');
  });

  it('should generate deterministic output', async () => {
    const result1 = await projector.project(sampleSource, 'sample');
    const result2 = await projector.project(sampleSource, 'sample');
    expect(result1.hash).toBe(result2.hash);
  });

  it('should generate Markdown with table of contents', async () => {
    const result = await projector.project(sampleSource, 'sample');
    expect(result.markdown).toContain('# sample');
    expect(result.markdown).toContain('Table of Contents');
  });

  it('should include examples in output', async () => {
    const result = await projector.project(sampleSource, 'sample');
    expect(result.markdown).toContain('```javascript');
  });
});

describe('ArchitectureProjector', () => {
  let projector;

  beforeEach(() => {
    projector = new ArchitectureProjector();
  });

  it('should generate partition documentation', async () => {
    const input = {
      partitions: [
        {
          name: 'TestPartition',
          description: 'Test partition',
          readOnly: true,
          namespaceIris: ['http://example.org/'],
          protectedNamespaces: [],
          size: 100,
        },
      ],
      dependencies: [],
      namespaces: {},
    };

    const result = await projector.project(input);
    expect(result.markdown).toContain('TestPartition');
    expect(result.partitionCount).toBe(1);
  });

  it('should generate Mermaid diagrams', async () => {
    const input = {
      partitions: [
        { name: 'P1', description: 'Part 1', readOnly: false, namespaceIris: [], protectedNamespaces: [], size: 10 },
        { name: 'P2', description: 'Part 2', readOnly: true, namespaceIris: [], protectedNamespaces: [], size: 20 },
      ],
      dependencies: [{ from: 'P1', to: 'P2', type: 'depends-on' }],
    };

    const result = await projector.project(input);
    expect(result.markdown).toContain('```mermaid');
    expect(result.markdown).toContain('graph TD');
  });

  it('should be deterministic', async () => {
    const input = {
      partitions: [
        { name: 'P1', description: 'Part 1', readOnly: false, namespaceIris: [], protectedNamespaces: [], size: 10 },
      ],
    };

    const result1 = await projector.project(input);
    const result2 = await projector.project(input);
    expect(result1.hash).toBe(result2.hash);
  });
});

describe('ExampleProjector', () => {
  let projector;

  beforeEach(() => {
    projector = new ExampleProjector();
  });

  it('should extract test cases as examples', () => {
    const examples = projector.parseTestFile(sampleTest);
    expect(examples.length).toBeGreaterThan(0);
  });

  it('should group examples by category', () => {
    const examples = projector.parseTestFile(sampleTest);
    const categories = projector.groupByCategory(examples);
    expect(categories.length).toBeGreaterThan(0);
  });

  it('should generate deterministic output', async () => {
    const result1 = await projector.project(sampleTest, 'sample.test.mjs');
    const result2 = await projector.project(sampleTest, 'sample.test.mjs');
    expect(result1.hash).toBe(result2.hash);
  });

  it('should include code blocks', async () => {
    const result = await projector.project(sampleTest, 'sample.test.mjs');
    expect(result.markdown).toContain('```javascript');
  });
});

describe('GuideProjector', () => {
  let projector;

  beforeEach(() => {
    projector = new GuideProjector();
  });

  it('should generate guides from templates', async () => {
    const templates = projector.getAvailableTemplates();
    expect(templates.length).toBeGreaterThan(0);
  });

  it('should generate guide from template', () => {
    const guide = projector.generateFromTemplate('create-partition', {
      description: 'How to create a custom partition',
    });
    expect(guide.title).toBeDefined();
    expect(guide.steps.length).toBeGreaterThan(0);
  });

  it('should project guides to Markdown', async () => {
    const guide = projector.generateFromTemplate('create-partition', {});
    const result = await projector.project([guide]);
    expect(result.guides.length).toBe(1);
    expect(result.index.markdown).toContain('How-To Guides');
  });

  it('should be deterministic', async () => {
    const guide = projector.generateFromTemplate('query-universe', {});
    const result1 = await projector.project([guide]);
    const result2 = await projector.project([guide]);
    expect(result1.combinedHash).toBe(result2.combinedHash);
  });
});

describe('ChangelogProjector', () => {
  let projector;

  beforeEach(() => {
    projector = new ChangelogProjector();
  });

  it('should parse receipt to changelog entry', () => {
    const entry = projector.parseReceipt(sampleReceipt);
    expect(entry.decision).toBe('allow');
    expect(entry.date).toBe('2025-12-26');
  });

  it('should generate Markdown changelog', async () => {
    const result = await projector.project([sampleReceipt], { projectName: 'Test' });
    expect(result.markdown).toContain('Changelog');
    expect(result.markdown).toContain('ALLOW');
  });

  it('should generate summary statistics', () => {
    const entries = [projector.parseReceipt(sampleReceipt)];
    const summary = projector.generateSummary(entries);
    expect(summary.total).toBe(1);
    expect(summary.byDecision.allow).toBe(1);
  });

  it('should be deterministic', async () => {
    const result1 = await projector.project([sampleReceipt]);
    const result2 = await projector.project([sampleReceipt]);
    expect(result1.hash).toBe(result2.hash);
  });
});

describe('DiataxisRenderer', () => {
  let renderer;

  beforeEach(() => {
    renderer = new DiataxisRenderer({
      projectName: 'TestProject',
      version: '1.0.0',
      audiences: ['user', 'contributor'],
    });
  });

  it('should classify documents to quadrants', () => {
    const doc = { content: 'In this tutorial you will learn...', title: 'Getting Started' };
    const quadrant = renderer.classifyDocument(doc);
    expect(quadrant).toBe('tutorial');
  });

  it('should classify API docs as reference', () => {
    const doc = { content: '@param x - the value\n@returns the result', title: 'API Reference' };
    const quadrant = renderer.classifyDocument(doc);
    expect(quadrant).toBe('reference');
  });

  it('should generate navigation structure', async () => {
    const docs = [
      { id: 'doc1', title: 'Tutorial 1', content: 'Tutorial content', quadrant: 'tutorial' },
      { id: 'doc2', title: 'Guide 1', content: 'Guide content', quadrant: 'how-to' },
    ];
    const result = await renderer.render(docs);
    expect(result.navigation).toBeDefined();
    expect(result.navigation.tutorial.items.length).toBe(1);
  });

  it('should generate main index', async () => {
    const docs = [
      { id: 'doc1', title: 'Doc 1', content: 'Content', quadrant: 'tutorial' },
    ];
    const result = await renderer.render(docs);
    expect(result.mainIndex).toContain('Documentation Structure');
    expect(result.mainIndex).toContain('Diataxis');
  });

  it('should be deterministic', async () => {
    const docs = [
      { id: 'doc1', title: 'Doc 1', content: 'Content', quadrant: 'tutorial' },
    ];
    const result1 = await renderer.render(docs);
    const result2 = await renderer.render(docs);
    expect(result1.hash).toBe(result2.hash);
  });
});

describe('ProjectionPipeline', () => {
  let pipeline;

  beforeEach(() => {
    pipeline = new ProjectionPipeline({
      projectName: 'TestProject',
      version: '1.0.0',
    });
  });

  it('should run complete pipeline', async () => {
    const result = await pipeline.run({
      sources: [
        { name: 'sample.mjs', content: sampleSource, type: 'source' },
        { name: 'sample.test.mjs', content: sampleTest, type: 'test' },
      ],
      receipts: [sampleReceipt],
    });

    expect(result.outputHash).toBeDefined();
    expect(result.summary.sourceCount).toBe(1);
    expect(result.summary.testCount).toBe(1);
  });

  it('should emit projection receipts', async () => {
    const result = await pipeline.run({
      sources: [
        { name: 'sample.mjs', content: sampleSource, type: 'source' },
      ],
    });

    expect(result.receipts.length).toBeGreaterThan(0);
    expect(result.receipts[0].projectorType).toBeDefined();
  });

  it('should be deterministic', async () => {
    const input = {
      sources: [
        { name: 'sample.mjs', content: sampleSource, type: 'source' },
      ],
    };

    const result1 = await pipeline.run(input);
    const result2 = await pipeline.run(input);
    expect(result1.outputHash).toBe(result2.outputHash);
  });

  it('should generate pipeline report', async () => {
    const result = await pipeline.run({
      sources: [
        { name: 'sample.mjs', content: sampleSource, type: 'source' },
      ],
    });

    const report = pipeline.generateReport(result);
    expect(report).toContain('Documentation Projection Report');
    expect(report).toContain('TestProject');
  });

  it('should verify determinism', async () => {
    const input = {
      sources: [
        { name: 'sample.mjs', content: sampleSource, type: 'source' },
      ],
    };

    const result = await pipeline.run(input);
    const verified = await pipeline.verifyDeterminism(input, result.outputHash);
    expect(verified).toBe(true);
  });
});

describe('Determinism Guarantees', () => {
  it('should produce consistent hashes across multiple runs', async () => {
    const pipeline = new ProjectionPipeline({
      projectName: 'DeterminismTest',
    });

    const input = {
      sources: [
        { name: 'test.mjs', content: sampleSource, type: 'source' },
        { name: 'test.test.mjs', content: sampleTest, type: 'test' },
      ],
      receipts: [sampleReceipt],
    };

    // Run pipeline 5 times
    const hashes = [];
    for (let i = 0; i < 5; i++) {
      const result = await pipeline.run(input);
      hashes.push(result.outputHash);
    }

    // All hashes should be identical
    const uniqueHashes = new Set(hashes);
    expect(uniqueHashes.size).toBe(1);
  });

  it('should detect changes in input', async () => {
    // Test that different source content produces different API docs
    const projector = new JsDocProjector();

    const source1 = `
/**
 * Function one
 * @param {number} x - Input
 * @returns {number} Output
 */
export function funcOne(x) { return x; }
`;

    const source2 = `
/**
 * Function two - different
 * @param {string} y - String input
 * @returns {string} String output
 */
export function funcTwo(y) { return y; }
`;

    const result1 = await projector.project(source1, 'module1');
    const result2 = await projector.project(source2, 'module2');

    // Different content should produce different hashes
    expect(result1.hash).not.toBe(result2.hash);
  });
});
