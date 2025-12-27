/**
 * @file Tool Wrapper Tests - TDD Test Suite
 * @description Comprehensive tests for tool contract validation and receipt generation
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { z } from 'zod';
import { Wrap } from '../src/tool-wrapper.mjs';

describe('ToolWrapper - Tool Contract Enforcement', () => {
  let mockTool;
  let manifest;

  beforeEach(() => {
    // Mock tool that simulates file read
    mockTool = async (inputs) => {
      if (!inputs.path) {
        throw new Error('Path required');
      }
      return {
        content: `Content of ${inputs.path}`,
        size: 42,
      };
    };

    // Tool manifest with schemas
    manifest = {
      name: 'Read',
      version: '1.0.0',
      schema_in: z.object({
        path: z.string(),
        encoding: z.string().optional(),
      }),
      schema_out: z.object({
        content: z.string(),
        size: z.number(),
      }),
      capabilities: ['file-read'],
    };
  });

  describe('Input Validation', () => {
    it('should validate inputs against schema_in', async () => {
      const wrapped = Wrap(mockTool, manifest);
      const result = await wrapped({ path: '/test.txt' });

      expect(result.receipt.inputs).toEqual({
        path: '/test.txt',
      });
      expect(result.receipt.status).toBe('success');
    });

    it('should reject invalid inputs', async () => {
      const wrapped = Wrap(mockTool, manifest);

      await expect(wrapped({ invalid: 'field' })).rejects.toThrow();
    });

    it('should handle optional fields in schema', async () => {
      const wrapped = Wrap(mockTool, manifest);
      const result = await wrapped({
        path: '/test.txt',
        encoding: 'utf-8',
      });

      expect(result.receipt.inputs).toEqual({
        path: '/test.txt',
        encoding: 'utf-8',
      });
    });
  });

  describe('Output Validation', () => {
    it('should validate outputs against schema_out', async () => {
      const wrapped = Wrap(mockTool, manifest);
      const result = await wrapped({ path: '/test.txt' });

      expect(result.receipt.outputs).toEqual({
        content: 'Content of /test.txt',
        size: 42,
      });
      expect(result.receipt.status).toBe('success');
    });

    it('should reject invalid tool outputs', async () => {
      const badTool = async () => ({ invalid: 'output' });
      const wrapped = Wrap(badTool, manifest);

      const result = await wrapped({ path: '/test.txt' });
      expect(result.receipt.status).toBe('error');
      expect(result.receipt.error).toBeDefined();
    });

    it('should handle tool execution errors', async () => {
      const errorTool = async () => {
        throw new Error('Tool execution failed');
      };
      const wrapped = Wrap(errorTool, manifest);

      const result = await wrapped({ path: '/test.txt' });

      expect(result.receipt.status).toBe('error');
      expect(result.receipt.error).toBe('Tool execution failed');
    });
  });

  describe('Receipt Generation', () => {
    it('should generate receipt with all required fields', async () => {
      const wrapped = Wrap(mockTool, manifest);
      const result = await wrapped({ path: '/test.txt' });

      expect(result.receipt).toMatchObject({
        tool_name: 'Read',
        version: '1.0.0',
        inputs: { path: '/test.txt' },
        outputs: { content: 'Content of /test.txt', size: 42 },
        status: 'success',
      });
      expect(result.receipt.timestamp).toBeDefined();
      expect(typeof result.receipt.timestamp).toBe('number');
    });

    it('should include execution time in receipt', async () => {
      const wrapped = Wrap(mockTool, manifest);
      const result = await wrapped({ path: '/test.txt' });

      expect(result.receipt.execution_time_ms).toBeDefined();
      expect(typeof result.receipt.execution_time_ms).toBe('number');
      expect(result.receipt.execution_time_ms).toBeGreaterThanOrEqual(0);
    });

    it('should include error details in failed receipts', async () => {
      const errorTool = async () => {
        throw new Error('File not found');
      };
      const wrapped = Wrap(errorTool, manifest);
      const result = await wrapped({ path: '/missing.txt' });

      expect(result.receipt.status).toBe('error');
      expect(result.receipt.error).toBe('File not found');
      expect(result.receipt.outputs).toBeNull();
    });
  });

  describe('Delta Computation', () => {
    it('should return delta from successful tool execution', async () => {
      const wrapped = Wrap(mockTool, manifest);
      const result = await wrapped({ path: '/test.txt' });

      expect(result.delta).toEqual({
        content: 'Content of /test.txt',
        size: 42,
      });
    });

    it('should return null delta on error', async () => {
      const errorTool = async () => {
        throw new Error('Failed');
      };
      const wrapped = Wrap(errorTool, manifest);
      const result = await wrapped({ path: '/test.txt' });

      expect(result.delta).toBeNull();
    });
  });

  describe('Immutability Enforcement', () => {
    it('should prevent tool mutation without receipts', async () => {
      const statefulTool = async (inputs) => {
        // Simulate state mutation
        return { mutated: true, state: 'changed' };
      };

      const stateManifest = {
        name: 'StatefulTool',
        version: '1.0.0',
        schema_in: z.object({ action: z.string() }),
        schema_out: z.object({
          mutated: z.boolean(),
          state: z.string(),
        }),
        capabilities: ['stateful'],
      };

      const wrapped = Wrap(statefulTool, stateManifest);
      const result = await wrapped({ action: 'mutate' });

      // Every execution MUST generate a receipt
      expect(result.receipt).toBeDefined();
      expect(result.receipt.tool_name).toBe('StatefulTool');

      // Multiple calls generate different receipts
      const result2 = await wrapped({ action: 'mutate' });
      expect(result2.receipt.timestamp).toBeGreaterThanOrEqual(
        result.receipt.timestamp,
      );
    });
  });

  describe('Tool Versioning', () => {
    it('should track tool version in receipts', async () => {
      const wrapped = Wrap(mockTool, manifest);
      const result = await wrapped({ path: '/test.txt' });

      expect(result.receipt.version).toBe('1.0.0');
    });

    it('should allow different versions of same tool', async () => {
      const manifestV2 = {
        ...manifest,
        version: '2.0.0',
        schema_out: z.object({
          content: z.string(),
          size: z.number(),
          hash: z.string().optional(),
        }),
      };

      const toolV2 = async (inputs) => ({
        content: `Content of ${inputs.path}`,
        size: 42,
        hash: 'abc123',
      });

      const wrappedV2 = Wrap(toolV2, manifestV2);
      const result = await wrappedV2({ path: '/test.txt' });

      expect(result.receipt.version).toBe('2.0.0');
      expect(result.receipt.outputs.hash).toBe('abc123');
    });
  });

  describe('Complex Schema Validation', () => {
    it('should validate nested schemas', async () => {
      const complexTool = async (inputs) => ({
        result: {
          data: inputs.query,
          metadata: {
            timestamp: Date.now(),
            count: 1,
          },
        },
      });

      const complexManifest = {
        name: 'ComplexTool',
        version: '1.0.0',
        schema_in: z.object({
          query: z.string(),
        }),
        schema_out: z.object({
          result: z.object({
            data: z.string(),
            metadata: z.object({
              timestamp: z.number(),
              count: z.number(),
            }),
          }),
        }),
        capabilities: ['query'],
      };

      const wrapped = Wrap(complexTool, complexManifest);
      const result = await wrapped({ query: 'test' });

      expect(result.receipt.status).toBe('success');
      expect(result.receipt.outputs.result.metadata.count).toBe(1);
    });

    it('should validate array schemas', async () => {
      const arrayTool = async () => ({
        items: [1, 2, 3],
      });

      const arrayManifest = {
        name: 'ArrayTool',
        version: '1.0.0',
        schema_in: z.object({}),
        schema_out: z.object({
          items: z.array(z.number()),
        }),
        capabilities: ['array-processing'],
      };

      const wrapped = Wrap(arrayTool, arrayManifest);
      const result = await wrapped({});

      expect(result.receipt.outputs.items).toEqual([1, 2, 3]);
    });
  });
});
