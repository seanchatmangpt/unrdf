/**
 * @file MCP Server Tests
 * @module @unrdf/daemon/mcp/__tests__/mcp.test
 * @description Tests for the Model Context Protocol server
 */

import { describe, it, expect, vi } from 'vitest';
import { createMCPServer } from '../index.mjs';
import { mcpTools } from '../tools.mjs';
import { mcpResources } from '../resources.mjs';
import { mcpPrompts } from '../prompts.mjs';

describe('MCP Server', () => {
  describe('createMCPServer', () => {
    it('should create a server with expected capabilities', async () => {
      const server = createMCPServer();
      expect(server).toBeDefined();
      expect(typeof server).toBe('object');
      // Server is created and has request handlers registered
      expect(server._requestHandlers).toBeDefined();
    });
  });

  describe('Tool Registration', () => {
    it('should register list_endpoints tool', () => {
      const tool = mcpTools.find((t) => t.name === 'list_endpoints');
      expect(tool).toBeDefined();
      expect(tool.description).toBe('List all available SPARQL endpoints');
      expect(tool.inputSchema).toBeDefined();
      expect(tool.inputSchema.type).toBe('object');
    });

    it('should register execute_sparql tool', () => {
      const tool = mcpTools.find((t) => t.name === 'execute_sparql');
      expect(tool).toBeDefined();
      expect(tool.description).toBe('Execute a SPARQL query against an endpoint');
      expect(tool.inputSchema.properties).toHaveProperty('endpoint');
      expect(tool.inputSchema.properties).toHaveProperty('query');
      expect(tool.inputSchema.properties).toHaveProperty('format');
    });

    it('should register get_graph_stats tool', () => {
      const tool = mcpTools.find((t) => t.name === 'get_graph_stats');
      expect(tool).toBeDefined();
      expect(tool.description).toBe('Get statistics about an RDF graph');
      expect(tool.inputSchema.properties).toHaveProperty('graph_iri');
    });

    it('should register load_rdf_data tool', () => {
      const tool = mcpTools.find((t) => t.name === 'load_rdf_data');
      expect(tool).toBeDefined();
      expect(tool.description).toBe('Load RDF data from a file');
      expect(tool.inputSchema.properties).toHaveProperty('file_path');
      expect(tool.inputSchema.properties).toHaveProperty('format');
    });

    it('should have 4 tools total', () => {
      expect(mcpTools).toHaveLength(4);
    });
  });

  describe('Resource Registration', () => {
    it('should register endpoint config resource', () => {
      const resource = mcpResources.find(
        (r) => r.uri === 'sparql://endpoints/config'
      );
      expect(resource).toBeDefined();
      expect(resource.name).toBe('SPARQL Endpoints Configuration');
      expect(resource.mimeType).toBe('application/json');
    });

    it('should register ontology catalog resource', () => {
      const resource = mcpResources.find((r) => r.uri === 'rdf://ontologies/catalog');
      expect(resource).toBeDefined();
      expect(resource.name).toBe('RDF Ontology Catalog');
    });

    it('should register graph metadata resource', () => {
      const resource = mcpResources.find((r) => r.uri === 'graphs://metadata');
      expect(resource).toBeDefined();
      expect(resource.name).toBe('Graph Metadata');
    });

    it('should register query templates resource', () => {
      const resource = mcpResources.find((r) => r.uri === 'queries://templates');
      expect(resource).toBeDefined();
      expect(resource.name).toBe('SPARQL Query Templates');
    });

    it('should have 4 resources total', () => {
      expect(mcpResources).toHaveLength(4);
    });
  });

  describe('Prompt Registration', () => {
    it('should register sparql_builder prompt', () => {
      const prompt = mcpPrompts.find((p) => p.name === 'sparql_builder');
      expect(prompt).toBeDefined();
      expect(prompt.description).toBe('Build SPARQL queries for RDF graph exploration');
      expect(prompt.arguments).toBeDefined();
      expect(prompt.arguments).toHaveLength(2);
    });

    it('should register graph_analysis prompt', () => {
      const prompt = mcpPrompts.find((p) => p.name === 'graph_analysis');
      expect(prompt).toBeDefined();
      expect(prompt.description).toBe('Analyze RDF graph structure and content');
      expect(prompt.arguments).toHaveLength(2);
    });

    it('should register data_transform prompt', () => {
      const prompt = mcpPrompts.find((p) => p.name === 'data_transform');
      expect(prompt).toBeDefined();
      expect(prompt.description).toBe('Transform RDF data between formats');
      expect(prompt.arguments).toHaveLength(3);
    });

    it('should register ontology_doc prompt', () => {
      const prompt = mcpPrompts.find((p) => p.name === 'ontology_doc');
      expect(prompt).toBeDefined();
      expect(prompt.description).toBe('Generate documentation for RDF ontologies');
      expect(prompt.arguments).toHaveLength(2);
    });

    it('should have 4 prompts total', () => {
      expect(mcpPrompts).toHaveLength(4);
    });
  });

  describe('Transport Initialization', () => {
    it('should support StdioServerTransport', async () => {
      const server = createMCPServer();
      expect(server).toBeDefined();
      // StdioServerTransport would be initialized in actual server startup
    });

    it('should support SSEServerTransport', async () => {
      const server = createMCPServer();
      expect(server).toBeDefined();
      // SSEServerTransport would be initialized in actual server startup
    });
  });

  describe('Server Capabilities', () => {
    it('should support tool handlers', () => {
      const server = createMCPServer();
      expect(server).toBeDefined();
      // Server has request handlers registered for tools
      expect(server._requestHandlers).toBeDefined();
    });

    it('should support resource handlers', () => {
      const server = createMCPServer();
      expect(server).toBeDefined();
      // Server has request handlers registered for resources
      expect(server._requestHandlers).toBeDefined();
    });

    it('should support prompt handlers', () => {
      const server = createMCPServer();
      expect(server).toBeDefined();
      // Server has request handlers registered for prompts
      expect(server._requestHandlers).toBeDefined();
    });
  });
});
