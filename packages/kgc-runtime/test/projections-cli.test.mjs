/**
 * Tests for CLI Projections (Π_cli)
 */

import { describe, it, expect } from 'vitest';
import {
  projectReceiptToCLI,
  projectWorkItemsToCLI,
  projectObjectToTree,
  projectArrayToList,
  flattenForCLI,
  CLIProjectionSchema,
} from '../src/projections-cli.mjs';

describe('Π_cli - CLI Projections', () => {
  describe('projectReceiptToCLI', () => {
    it('should project receipt to CLI format with colors', () => {
      const receipt = {
        id: 'receipt-001',
        timestamp: '2024-01-01T00:00:00Z',
        operation: 'test-operation',
        inputs: { x: 1 },
        outputs: { y: 2 },
        hash: '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef',
      };

      const projection = projectReceiptToCLI(receipt, true);

      expect(projection.type).toBe('cli');
      expect(projection.format).toBe('summary');
      expect(projection.colored).toBe(true);
      expect(projection.content).toContain('receipt-001');
      expect(projection.content).toContain('test-operation');
      expect(projection.metadata.receiptId).toBe('receipt-001');

      // Validate schema
      CLIProjectionSchema.parse(projection);
    });

    it('should project receipt without colors', () => {
      const receipt = {
        id: 'receipt-002',
        timestamp: '2024-01-01T00:00:00Z',
        operation: 'test-op',
        inputs: {},
        outputs: {},
        hash: 'a'.repeat(64),
      };

      const projection = projectReceiptToCLI(receipt, false);

      expect(projection.colored).toBe(false);
      expect(projection.content).not.toContain('\x1b['); // No ANSI codes
    });

    it('should include parent hash if present', () => {
      const receipt = {
        id: 'receipt-003',
        timestamp: '2024-01-01T00:00:00Z',
        operation: 'test',
        inputs: {},
        outputs: {},
        hash: 'a'.repeat(64),
        parentHash: 'b'.repeat(64),
      };

      const projection = projectReceiptToCLI(receipt, false);

      expect(projection.content).toContain('Parent:');
      expect(projection.content).toContain('bbbbbbbbbbbbbbbb');
    });
  });

  describe('projectWorkItemsToCLI', () => {
    it('should project work items to table format', () => {
      const workItems = [
        { id: 'item-001', goal: 'Process data', state: 'completed', priority: 1 },
        { id: 'item-002', goal: 'Generate report', state: 'running', priority: 2 },
        { id: 'item-003', goal: 'Send notification', state: 'failed', priority: 0 },
      ];

      const projection = projectWorkItemsToCLI(workItems, false);

      expect(projection.type).toBe('cli');
      expect(projection.format).toBe('table');
      expect(projection.content).toContain('ID');
      expect(projection.content).toContain('Goal');
      expect(projection.content).toContain('State');
      expect(projection.content).toContain('Priority');
      expect(projection.metadata.count).toBe(3);
    });

    it('should handle empty work items array', () => {
      const projection = projectWorkItemsToCLI([], false);

      expect(projection.content).toContain('No work items');
    });

    it('should use colors for different states', () => {
      const workItems = [
        { id: 'item-001', goal: 'Task', state: 'completed', priority: 1 },
      ];

      const projection = projectWorkItemsToCLI(workItems, true);

      expect(projection.colored).toBe(true);
      expect(projection.content).toContain('\x1b['); // Has ANSI codes
    });
  });

  describe('projectObjectToTree', () => {
    it('should project nested object to tree format', () => {
      const obj = {
        root: {
          child1: {
            grandchild: 'value',
          },
          child2: 'simple',
        },
        array: [1, 2, 3],
      };

      const projection = projectObjectToTree(obj, false);

      expect(projection.type).toBe('cli');
      expect(projection.format).toBe('tree');
      expect(projection.content).toContain('root');
      expect(projection.content).toContain('child1');
      expect(projection.content).toContain('grandchild');
      expect(projection.content).toContain('└──');
    });

    it('should handle arrays in tree', () => {
      const obj = {
        items: [1, 2, 3],
      };

      const projection = projectObjectToTree(obj, false);

      expect(projection.content).toContain('[3 items]');
    });
  });

  describe('projectArrayToList', () => {
    it('should project string array to numbered list', () => {
      const items = ['First item', 'Second item', 'Third item'];

      const projection = projectArrayToList(items, false);

      expect(projection.type).toBe('cli');
      expect(projection.format).toBe('list');
      expect(projection.content).toContain('1. First item');
      expect(projection.content).toContain('2. Second item');
      expect(projection.content).toContain('3. Third item');
      expect(projection.metadata.count).toBe(3);
    });

    it('should project object array with labels and descriptions', () => {
      const items = [
        { label: 'Task 1', description: 'Do something' },
        { label: 'Task 2', description: 'Do something else' },
      ];

      const projection = projectArrayToList(items, false);

      expect(projection.content).toContain('Task 1');
      expect(projection.content).toContain('Do something');
      expect(projection.content).toContain('Task 2');
    });
  });

  describe('flattenForCLI', () => {
    it('should flatten nested object with dot notation', () => {
      const obj = {
        user: {
          name: 'Alice',
          address: {
            city: 'NYC',
            zip: '10001',
          },
        },
        count: 42,
      };

      const flattened = flattenForCLI(obj);

      expect(flattened['user.name']).toBe('Alice');
      expect(flattened['user.address.city']).toBe('NYC');
      expect(flattened['user.address.zip']).toBe('10001');
      expect(flattened['count']).toBe(42);
    });

    it('should handle arrays without flattening', () => {
      const obj = {
        items: [1, 2, 3],
      };

      const flattened = flattenForCLI(obj);

      expect(Array.isArray(flattened.items)).toBe(true);
      expect(flattened.items).toEqual([1, 2, 3]);
    });
  });
});
