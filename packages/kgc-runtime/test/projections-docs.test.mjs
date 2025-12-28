/**
 * Tests for Documentation Projections (Π_docs)
 */

import { describe, it, expect } from 'vitest';
import {
  projectReceiptToDocs,
  projectSchemaToDocs,
  projectFunctionToDocs,
  projectWorkflowToDocs,
  generateCrossRefs,
  generateTableOfContents,
  DocsProjectionSchema,
} from '../src/projections-docs.mjs';
import { z } from 'zod';

describe('Π_docs - Documentation Projections', () => {
  describe('projectReceiptToDocs', () => {
    it('should project receipt to markdown documentation', () => {
      const receipt = {
        id: 'receipt-001',
        timestamp: '2024-01-01T00:00:00Z',
        operation: 'test-operation',
        inputs: { x: 1, y: 2 },
        outputs: { result: 3 },
        hash: '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef',
      };

      const projection = projectReceiptToDocs(receipt);

      expect(projection.type).toBe('docs');
      expect(projection.format).toBe('markdown');
      expect(projection.content).toContain('# Receipt Documentation');
      expect(projection.content).toContain('receipt-001');
      expect(projection.content).toContain('test-operation');
      expect(projection.frontMatter.id).toBe('receipt-001');
      expect(projection.sections).toHaveLength(4);

      // Validate schema
      DocsProjectionSchema.parse(projection);
    });

    it('should include parent hash if present', () => {
      const receipt = {
        id: 'receipt-002',
        timestamp: '2024-01-01T00:00:00Z',
        operation: 'test',
        inputs: {},
        outputs: {},
        hash: 'a'.repeat(64),
        parentHash: 'b'.repeat(64),
      };

      const projection = projectReceiptToDocs(receipt);

      expect(projection.content).toContain('Parent Hash');
    });
  });

  describe('projectSchemaToDocs', () => {
    it('should project Zod schema to documentation', () => {
      const TestSchema = z.object({
        name: z.string(),
        age: z.number(),
      });

      const example = { name: 'Alice', age: 30 };

      const projection = projectSchemaToDocs('TestSchema', TestSchema, example);

      expect(projection.type).toBe('docs');
      expect(projection.content).toContain('# TestSchema Documentation');
      expect(projection.content).toContain('```json');
      expect(projection.content).toContain('Alice');
      expect(projection.content).toContain('## Example');
      expect(projection.content).toContain('## Usage');
      expect(projection.frontMatter.schema).toBe('TestSchema');
    });

    it('should generate docs without example', () => {
      const TestSchema = z.string();

      const projection = projectSchemaToDocs('SimpleSchema', TestSchema);

      expect(projection.content).toContain('# SimpleSchema Documentation');
      expect(projection.content).not.toContain('## Example');
    });
  });

  describe('projectFunctionToDocs', () => {
    it('should project function to API documentation', () => {
      const fnInfo = {
        name: 'calculateSum',
        description: 'Calculates the sum of two numbers',
        params: [
          { name: 'a', type: 'number', description: 'First number' },
          { name: 'b', type: 'number', description: 'Second number' },
        ],
        returns: { type: 'number', description: 'The sum of a and b' },
        example: 'const result = calculateSum(1, 2); // 3',
      };

      const projection = projectFunctionToDocs(null, fnInfo);

      expect(projection.type).toBe('docs');
      expect(projection.content).toContain('# calculateSum()');
      expect(projection.content).toContain('## Description');
      expect(projection.content).toContain('## Parameters');
      expect(projection.content).toContain('## Returns');
      expect(projection.content).toContain('## Example');
      expect(projection.content).toContain('First number');
      expect(projection.frontMatter.function).toBe('calculateSum');
    });
  });

  describe('projectWorkflowToDocs', () => {
    it('should project workflow to documentation', () => {
      const workflow = {
        id: 'wf-001',
        name: 'Data Processing Workflow',
        description: 'Processes incoming data through multiple stages',
        steps: [
          { id: 'step-1', type: 'validate', description: 'Validate input data' },
          { id: 'step-2', type: 'transform', description: 'Transform data format' },
          { id: 'step-3', type: 'store', description: 'Store in database' },
        ],
      };

      const projection = projectWorkflowToDocs(workflow);

      expect(projection.type).toBe('docs');
      expect(projection.content).toContain('# Data Processing Workflow');
      expect(projection.content).toContain('## Workflow Steps');
      expect(projection.content).toContain('1. **validate**');
      expect(projection.content).toContain('Validate input data');
      expect(projection.crossRefs).toHaveLength(3);
      expect(projection.frontMatter.workflowId).toBe('wf-001');
    });
  });

  describe('generateCrossRefs', () => {
    it('should generate markdown links for references', () => {
      const text = 'See Receipt and WorkItem for details';
      const refMap = new Map([
        ['Receipt', '/docs/receipt'],
        ['WorkItem', '/docs/work-item'],
      ]);

      const result = generateCrossRefs(text, refMap);

      expect(result).toContain('[Receipt](/docs/receipt)');
      expect(result).toContain('[WorkItem](/docs/work-item)');
    });
  });

  describe('generateTableOfContents', () => {
    it('should generate TOC from sections', () => {
      const sections = [
        { title: 'Introduction', level: 1 },
        { title: 'Getting Started', level: 2 },
        { title: 'Installation', level: 3 },
        { title: 'Usage', level: 2 },
      ];

      const toc = generateTableOfContents(sections);

      expect(toc).toContain('## Table of Contents');
      expect(toc).toContain('[Introduction](#introduction)');
      expect(toc).toContain('  [Getting Started](#getting-started)');
      expect(toc).toContain('    [Installation](#installation)');
    });
  });
});
