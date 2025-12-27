/**
 * @file Tool Registry Tests
 * @description Tests for ToolRegistry class functionality
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { ToolRegistry, createRegistry } from '../src/tool-registry.mjs';
import { z } from 'zod';
import { join } from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

describe('ToolRegistry', () => {
  let registry;

  beforeEach(() => {
    registry = new ToolRegistry();
  });

  describe('Tool Registration', () => {
    it('should register a tool with valid manifest', () => {
      const manifest = {
        name: 'TestTool',
        version: '1.0.0',
        description: 'Test tool',
        schema_in: z.object({ input: z.string() }),
        schema_out: z.object({ output: z.string() }),
        capabilities: ['test'],
      };

      registry.registerTool(manifest);
      const tool = registry.getTool('TestTool');

      expect(tool).toBeDefined();
      expect(tool.name).toBe('TestTool');
      expect(tool.version).toBe('1.0.0');
    });

    it('should reject invalid manifest', () => {
      const invalidManifest = {
        name: 'Invalid',
        // Missing required fields
      };

      expect(() => registry.registerTool(invalidManifest)).toThrow();
    });

    it('should store multiple versions of same tool', () => {
      const v1 = {
        name: 'Tool',
        version: '1.0.0',
        schema_in: z.object({}),
        schema_out: z.object({}),
        capabilities: [],
      };

      const v2 = {
        name: 'Tool',
        version: '2.0.0',
        schema_in: z.object({}),
        schema_out: z.object({}),
        capabilities: [],
      };

      registry.registerTool(v1);
      registry.registerTool(v2);

      expect(registry.getToolVersion('Tool', '1.0.0')).toBeDefined();
      expect(registry.getToolVersion('Tool', '2.0.0')).toBeDefined();
      expect(registry.getTool('Tool').version).toBe('2.0.0'); // Latest
    });
  });

  describe('Schema Conversion', () => {
    it('should convert object schema definition to Zod', () => {
      const manifest = {
        name: 'Read',
        version: '1.0.0',
        schema_in: {
          type: 'object',
          properties: {
            path: { type: 'string' },
            encoding: { type: 'string' },
          },
          required: ['path'],
        },
        schema_out: {
          type: 'object',
          properties: {
            content: { type: 'string' },
          },
          required: ['content'],
        },
        capabilities: ['file-read'],
      };

      registry.registerTool(manifest);
      const tool = registry.getTool('Read');

      // Test that schemas work
      expect(tool.schema_in.parse({ path: '/test' })).toBeDefined();
      expect(() => tool.schema_in.parse({})).toThrow(); // Missing required field
    });

    it('should handle nested object schemas', () => {
      const manifest = {
        name: 'Complex',
        version: '1.0.0',
        schema_in: {
          type: 'object',
          properties: {
            config: {
              type: 'object',
              properties: {
                timeout: { type: 'number' },
              },
              required: ['timeout'],
            },
          },
          required: ['config'],
        },
        schema_out: {
          type: 'object',
          properties: {
            result: { type: 'string' },
          },
          required: ['result'],
        },
        capabilities: [],
      };

      registry.registerTool(manifest);
      const tool = registry.getTool('Complex');

      expect(
        tool.schema_in.parse({ config: { timeout: 5000 } }),
      ).toBeDefined();
    });

    it('should handle array schemas', () => {
      const manifest = {
        name: 'ArrayTool',
        version: '1.0.0',
        schema_in: {
          type: 'object',
          properties: {},
          required: [],
        },
        schema_out: {
          type: 'object',
          properties: {
            items: {
              type: 'array',
              items: { type: 'string' },
            },
          },
          required: ['items'],
        },
        capabilities: [],
      };

      registry.registerTool(manifest);
      const tool = registry.getTool('ArrayTool');

      expect(tool.schema_out.parse({ items: ['a', 'b'] })).toBeDefined();
      expect(() => tool.schema_out.parse({ items: [1, 2] })).toThrow();
    });
  });

  describe('Tool Queries', () => {
    beforeEach(() => {
      registry.registerTool({
        name: 'Read',
        version: '1.0.0',
        schema_in: z.object({}),
        schema_out: z.object({}),
        capabilities: ['file-read', 'filesystem-access'],
      });

      registry.registerTool({
        name: 'Write',
        version: '1.0.0',
        schema_in: z.object({}),
        schema_out: z.object({}),
        capabilities: ['file-write', 'filesystem-access'],
      });

      registry.registerTool({
        name: 'Bash',
        version: '1.0.0',
        schema_in: z.object({}),
        schema_out: z.object({}),
        capabilities: ['command-execution'],
      });
    });

    it('should get all tools', () => {
      const tools = registry.getAllTools();
      expect(tools).toHaveLength(3);
      expect(tools.map((t) => t.name).sort()).toEqual([
        'Bash',
        'Read',
        'Write',
      ]);
    });

    it('should query tools by capability', () => {
      const fileTools = registry.getToolsByCapability('filesystem-access');
      expect(fileTools).toHaveLength(2);
      expect(fileTools.map((t) => t.name).sort()).toEqual([
        'Read',
        'Write',
      ]);
    });

    it('should check if tool has capability', () => {
      expect(registry.hasCapability('Read', 'file-read')).toBe(true);
      expect(registry.hasCapability('Read', 'file-write')).toBe(false);
      expect(registry.hasCapability('NonExistent', 'any')).toBe(false);
    });
  });

  describe('Output Validation', () => {
    beforeEach(() => {
      registry.registerTool({
        name: 'TestTool',
        version: '1.0.0',
        schema_in: z.object({}),
        schema_out: z.object({
          result: z.string(),
          count: z.number(),
        }),
        capabilities: [],
      });
    });

    it('should validate correct output', () => {
      const output = { result: 'success', count: 42 };
      expect(registry.validateOutput('TestTool', output)).toBe(true);
    });

    it('should reject invalid output', () => {
      const output = { result: 'success' }; // Missing count
      expect(registry.validateOutput('TestTool', output)).toBe(false);
    });

    it('should throw on non-existent tool', () => {
      expect(() =>
        registry.validateOutput('NonExistent', {}),
      ).toThrow();
    });
  });

  describe('Registry Statistics', () => {
    beforeEach(() => {
      registry.registerTool({
        name: 'Read',
        version: '1.0.0',
        schema_in: z.object({}),
        schema_out: z.object({}),
        capabilities: ['file-read', 'filesystem-access'],
      });

      registry.registerTool({
        name: 'Write',
        version: '1.0.0',
        schema_in: z.object({}),
        schema_out: z.object({}),
        capabilities: ['file-write', 'filesystem-access'],
      });
    });

    it('should return registry statistics', () => {
      const stats = registry.getStats();

      expect(stats.total_tools).toBe(2);
      expect(stats.unique_capabilities).toBe(3);
      expect(stats.capabilities.sort()).toEqual([
        'file-read',
        'file-write',
        'filesystem-access',
      ]);
    });
  });

  describe('Loading from File', () => {
    it('should load tools from registry file', () => {
      const registryPath = join(
        __dirname,
        '../../../var/kgc/tool-registry.json',
      );
      const fileRegistry = new ToolRegistry({ registryPath });

      const tools = fileRegistry.getAllTools();
      expect(tools.length).toBeGreaterThan(0);

      // Check built-in tools
      const bashTool = fileRegistry.getTool('Bash');
      expect(bashTool).toBeDefined();
      expect(bashTool.name).toBe('Bash');
      expect(bashTool.capabilities).toContain('command-execution');

      const readTool = fileRegistry.getTool('Read');
      expect(readTool).toBeDefined();
      expect(readTool.capabilities).toContain('file-read');
    });

    it('should validate loaded schemas work correctly', () => {
      const registryPath = join(
        __dirname,
        '../../../var/kgc/tool-registry.json',
      );
      const fileRegistry = new ToolRegistry({ registryPath });

      const readTool = fileRegistry.getTool('Read');

      // Should accept valid input
      const validInput = readTool.schema_in.parse({ path: '/test.txt' });
      expect(validInput.path).toBe('/test.txt');

      // Should reject invalid input
      expect(() => readTool.schema_in.parse({})).toThrow();
    });
  });

  describe('Factory Function', () => {
    it('should create registry with createRegistry', () => {
      const registryPath = join(
        __dirname,
        '../../../var/kgc/tool-registry.json',
      );
      const reg = createRegistry(registryPath);

      expect(reg).toBeInstanceOf(ToolRegistry);
      expect(reg.getAllTools().length).toBeGreaterThan(0);
    });
  });
});
