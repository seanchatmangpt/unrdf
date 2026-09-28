/**
 * @file MCP Server Tests
 * @module @unrdf/daemon/mcp/__tests__/mcp.test
 * @description Tests for the Model Context Protocol server
 */

import { describe, it, expect } from 'vitest';
import { createMCPServer } from '../index.mjs';
import { mcpGeneratedTools } from '../tools-generated.mjs';
import { mcpResources } from '../resources.mjs';
import { mcpPrompts } from '../prompts.mjs';

describe('MCP Server', () => {
  describe('createMCPServer', () => {
    it('should create a server with expected capabilities', async () => {
      const server = createMCPServer();
      expect(server).toBeDefined();
      expect(typeof server).toBe('object');
      expect(server).toBeTruthy();
    });
  });

  describe('Tool Registration (generated from cli-commands.ttl)', () => {
    const EXPECTED_TOOL_CATEGORIES = {
      graph: ['graph_create', 'graph_load', 'graph_query', 'graph_dump', 'graph_stats'],
      convert: ['convert', 'to_turtle', 'to_ntriples', 'to_json'],
      daemon: ['daemon_status', 'daemon_list', 'daemon_logs'],
      template: ['template_generate', 'template_list', 'template_query', 'template_extract'],
    };

    it('should have valid tool registry', () => {
      // Check registration and structure
      expect(mcpGeneratedTools).toBeDefined();
      expect(Array.isArray(mcpGeneratedTools)).toBe(true);
      expect(mcpGeneratedTools.length).toBeGreaterThan(0);

      // Check all tools have required fields
      const toolNames = mcpGeneratedTools.map(t => t.name);
      for (const tool of mcpGeneratedTools) {
        expect(tool.name).toBeTruthy();
        expect(tool.description).toBeTruthy();
        expect(tool.inputSchema).toBeDefined();
        expect(tool.inputSchema.type).toBe('object');
      }

      // Check all expected categories are present
      const expectedTools = Object.values(EXPECTED_TOOL_CATEGORIES).flat();
      expectedTools.forEach(toolName => {
        expect(toolNames).toContain(toolName);
      });
    });

    it('graph_query tool should have correct schema', () => {
      const tool = mcpGeneratedTools.find(t => t.name === 'graph_query');
      expect(tool).toBeDefined();
      expect(tool.inputSchema.required).toContain('file');
      expect(tool.inputSchema.required).toContain('query');

      const formatProp = tool.inputSchema.properties['format'];
      expect(formatProp).toBeDefined();
      expect(formatProp.type).toBe('string');
      expect(formatProp.default).toBe('table');
    });
  });

  describe('Resource Registration', () => {
    const EXPECTED_RESOURCES = [
      { uri: 'sparql://endpoints/config', name: 'SPARQL Endpoints Configuration', mimeType: 'application/json' },
    ];

    it('should have valid resource registry', () => {
      expect(mcpResources).toBeDefined();
      expect(Array.isArray(mcpResources)).toBe(true);
      expect(mcpResources.length).toBeGreaterThan(0);

      // Verify expected resources are present
      EXPECTED_RESOURCES.forEach(expectedResource => {
        const resource = mcpResources.find(r => r.uri === expectedResource.uri);
        expect(resource).toBeDefined();
        expect(resource.name).toBe(expectedResource.name);
        expect(resource.mimeType).toBe(expectedResource.mimeType);
      });
    });
  });

  describe('Prompt Registration', () => {
    const EXPECTED_PROMPTS = [
      { name: 'sparql_builder', minArguments: 1 },
    ];

    it('should have valid prompt registry', () => {
      expect(mcpPrompts).toBeDefined();
      expect(Array.isArray(mcpPrompts)).toBe(true);
      expect(mcpPrompts.length).toBeGreaterThan(0);

      // Verify expected prompts are present
      EXPECTED_PROMPTS.forEach(expectedPrompt => {
        const prompt = mcpPrompts.find(p => p.name === expectedPrompt.name);
        expect(prompt).toBeDefined();
        expect(prompt.arguments).toBeDefined();
        expect(prompt.arguments.length).toBeGreaterThanOrEqual(expectedPrompt.minArguments);
      });
    });
  });
});
