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
    // Define expected tool categories
    const EXPECTED_TOOL_CATEGORIES = {
      graph: ['graph_create', 'graph_load', 'graph_query', 'graph_dump', 'graph_stats'],
      convert: ['convert', 'to_turtle', 'to_ntriples', 'to_json'],
      daemon: ['daemon_status', 'daemon_list', 'daemon_logs'],
      template: ['template_generate', 'template_list', 'template_query', 'template_extract'],
    };

    it('should have tools registered', () => {
      // Verify tools are registered without hardcoding exact count
      expect(mcpGeneratedTools).toBeDefined();
      expect(Array.isArray(mcpGeneratedTools)).toBe(true);
      expect(mcpGeneratedTools.length).toBeGreaterThan(0);
    });

    it('should include all graph tools', () => {
      const names = mcpGeneratedTools.map(t => t.name);
      EXPECTED_TOOL_CATEGORIES.graph.forEach(toolName => {
        expect(names).toContain(toolName);
      });
    });

    it('should include convert tools', () => {
      const names = mcpGeneratedTools.map(t => t.name);
      EXPECTED_TOOL_CATEGORIES.convert.forEach(toolName => {
        expect(names).toContain(toolName);
      });
    });

    it('should include daemon tools', () => {
      const names = mcpGeneratedTools.map(t => t.name);
      EXPECTED_TOOL_CATEGORIES.daemon.forEach(toolName => {
        expect(names).toContain(toolName);
      });
    });

    it('should include template tools', () => {
      const names = mcpGeneratedTools.map(t => t.name);
      EXPECTED_TOOL_CATEGORIES.template.forEach(toolName => {
        expect(names).toContain(toolName);
      });
    });

    it('graph_query tool should have required args: file, query', () => {
      const tool = mcpGeneratedTools.find(t => t.name === 'graph_query');
      expect(tool).toBeDefined();
      expect(tool.inputSchema.required).toContain('file');
      expect(tool.inputSchema.required).toContain('query');
    });

    it('graph_query tool should have format property', () => {
      const tool = mcpGeneratedTools.find(t => t.name === 'graph_query');
      const formatProp = tool.inputSchema.properties['format'];
      expect(formatProp).toBeDefined();
      expect(formatProp.type).toBe('string');
      expect(formatProp.default).toBe('table');
    });

    it('all tools should have name and description', () => {
      for (const tool of mcpGeneratedTools) {
        expect(tool.name, `${tool.name} missing name`).toBeTruthy();
        expect(tool.description, `${tool.name} missing description`).toBeTruthy();
      }
    });

    it('all tools should have inputSchema', () => {
      for (const tool of mcpGeneratedTools) {
        expect(tool.inputSchema, `${tool.name} missing inputSchema`).toBeDefined();
        expect(tool.inputSchema.type).toBe('object');
      }
    });
  });

  describe('Resource Registration', () => {
    const EXPECTED_RESOURCES = [
      { uri: 'sparql://endpoints/config', name: 'SPARQL Endpoints Configuration', mimeType: 'application/json' },
    ];

    it('should register expected resources', () => {
      EXPECTED_RESOURCES.forEach(expectedResource => {
        const resource = mcpResources.find(r => r.uri === expectedResource.uri);
        expect(resource).toBeDefined();
        expect(resource.name).toBe(expectedResource.name);
        expect(resource.mimeType).toBe(expectedResource.mimeType);
      });
    });

    it('should have resources registered', () => {
      expect(mcpResources).toBeDefined();
      expect(Array.isArray(mcpResources)).toBe(true);
      expect(mcpResources.length).toBeGreaterThan(0);
    });
  });

  describe('Prompt Registration', () => {
    const EXPECTED_PROMPTS = [
      { name: 'sparql_builder', minArguments: 1 },
    ];

    it('should register expected prompts', () => {
      EXPECTED_PROMPTS.forEach(expectedPrompt => {
        const prompt = mcpPrompts.find(p => p.name === expectedPrompt.name);
        expect(prompt).toBeDefined();
        expect(prompt.arguments).toBeDefined();
        expect(prompt.arguments.length).toBeGreaterThanOrEqual(expectedPrompt.minArguments);
      });
    });

    it('should have prompts registered', () => {
      expect(mcpPrompts).toBeDefined();
      expect(Array.isArray(mcpPrompts)).toBe(true);
      expect(mcpPrompts.length).toBeGreaterThan(0);
    });
  });
});
