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
    it('should have 36 tools total', () => {
      expect(mcpGeneratedTools).toHaveLength(36);
    });

    it('should include all graph tools', () => {
      const names = mcpGeneratedTools.map(t => t.name);
      expect(names).toContain('graph_create');
      expect(names).toContain('graph_load');
      expect(names).toContain('graph_query');
      expect(names).toContain('graph_dump');
      expect(names).toContain('graph_stats');
    });

    it('should include convert tools', () => {
      const names = mcpGeneratedTools.map(t => t.name);
      expect(names).toContain('convert');
      expect(names).toContain('to_turtle');
      expect(names).toContain('to_ntriples');
      expect(names).toContain('to_json');
    });

    it('should include daemon tools', () => {
      const names = mcpGeneratedTools.map(t => t.name);
      expect(names).toContain('daemon_status');
      expect(names).toContain('daemon_list');
      expect(names).toContain('daemon_logs');
    });

    it('should include template tools', () => {
      const names = mcpGeneratedTools.map(t => t.name);
      expect(names).toContain('template_generate');
      expect(names).toContain('template_list');
      expect(names).toContain('template_query');
      expect(names).toContain('template_extract');
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
    it('should register endpoint config resource', () => {
      const resource = mcpResources.find(r => r.uri === 'sparql://endpoints/config');
      expect(resource).toBeDefined();
      expect(resource.name).toBe('SPARQL Endpoints Configuration');
      expect(resource.mimeType).toBe('application/json');
    });

    it('should have 4 resources total', () => {
      expect(mcpResources).toHaveLength(4);
    });
  });

  describe('Prompt Registration', () => {
    it('should register sparql_builder prompt', () => {
      const prompt = mcpPrompts.find(p => p.name === 'sparql_builder');
      expect(prompt).toBeDefined();
      expect(prompt.arguments).toHaveLength(2);
    });

    it('should have 4 prompts total', () => {
      expect(mcpPrompts).toHaveLength(4);
    });
  });
});
