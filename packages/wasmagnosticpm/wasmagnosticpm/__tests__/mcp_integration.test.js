/**
 * MCP Server Integration Tests
 *
 * Validates MCP tool registration, schema structure, and server setup.
 * Does NOT test WASM execution (that's covered by Rust integration tests).
 */
import { describe, it, expect, vi } from 'vitest';

// Mock the pictl WASM artifact so tests run without a compiled binary.
// The mcp_integration tests only validate tool registration and schema structure,
// not WASM execution, so an empty stub is sufficient.
vi.mock('../pkg/pictl.js', () => ({}));

import { PictlMCPServer } from '../src/mcp_server.js';
describe('PictlMCPServer', () => {
  it('constructs without error', () => {
    // Server construction sets up handlers and transport
    expect(() => new PictlMCPServer()).not.toThrow();
  });
  it('registers all expected discovery tools', async () => {
    // Access the private getAvailableTools method via reflection on the prototype
    const server = new PictlMCPServer();
    const tools = server.getAvailableTools();
    const toolNames = tools.map((t) => t.name);
    // Core discovery tools
    expect(toolNames).toContain('discover_dfg');
    expect(toolNames).toContain('discover_alpha_plus_plus');
    expect(toolNames).toContain('discover_ilp_optimization');
    expect(toolNames).toContain('discover_genetic_algorithm');
    expect(toolNames).toContain('discover_variants');
  });
  it('registers all expected analysis tools', async () => {
    const server = new PictlMCPServer();
    const tools = server.getAvailableTools();
    const toolNames = tools.map((t) => t.name);
    expect(toolNames).toContain('check_conformance');
    expect(toolNames).toContain('analyze_statistics');
    expect(toolNames).toContain('detect_bottlenecks');
    expect(toolNames).toContain('detect_concept_drift');
  });
  it('registers all OCEL tools', async () => {
    const server = new PictlMCPServer();
    const tools = server.getAvailableTools();
    const toolNames = tools.map((t) => t.name);
    expect(toolNames).toContain('load_ocel');
    expect(toolNames).toContain('flatten_ocel');
    expect(toolNames).toContain('discover_ocel_dfg_per_type');
    expect(toolNames).toContain('discover_oc_petri_net');
    expect(toolNames).toContain('encode_ocel_as_text');
  });
  it('registers all predictive tools', async () => {
    const server = new PictlMCPServer();
    const tools = server.getAvailableTools();
    const toolNames = tools.map((t) => t.name);
    expect(toolNames).toContain('predict_next_activity');
    expect(toolNames).toContain('predict_case_duration');
    expect(toolNames).toContain('score_trace_anomaly');
  });
  it('registers all ML tools', async () => {
    const server = new PictlMCPServer();
    const tools = server.getAvailableTools();
    const toolNames = tools.map((t) => t.name);
    expect(toolNames).toContain('ml_classify_traces');
    expect(toolNames).toContain('ml_cluster_traces');
    expect(toolNames).toContain('ml_forecast_throughput');
    expect(toolNames).toContain('ml_detect_anomalies');
    expect(toolNames).toContain('ml_regress_remaining_time');
    expect(toolNames).toContain('ml_pca_reduce');
  });
  it('registers utility tools', async () => {
    const server = new PictlMCPServer();
    const tools = server.getAvailableTools();
    const toolNames = tools.map((t) => t.name);
    expect(toolNames).toContain('compare_algorithms');
    expect(toolNames).toContain('get_capability_registry');
    expect(toolNames).toContain('clear_caches');
    expect(toolNames).toContain('cache_stats');
  });
  it('has at least 30 registered tools', async () => {
    const server = new PictlMCPServer();
    const tools = server.getAvailableTools();
    // MCP server exposes 30+ tools across all categories
    expect(tools.length).toBeGreaterThanOrEqual(30);
  });
  it('all tools have valid input schemas', async () => {
    const server = new PictlMCPServer();
    const tools = server.getAvailableTools();
    for (const tool of tools) {
      expect(tool.inputSchema.type).toBe('object');
      expect(tool.inputSchema.properties).toBeDefined();
      // required is optional (utility tools may omit it)
      if (tool.inputSchema.required) {
        expect(Array.isArray(tool.inputSchema.required)).toBe(true);
        // Every tool should have at least one required parameter
        // (except utility tools with no required params like get_capability_registry)
        if (!['get_capability_registry', 'clear_caches', 'cache_stats'].includes(tool.name)) {
          expect(tool.inputSchema.required.length).toBeGreaterThan(0);
        }
      }
    }
  });
  it('all required params have corresponding property definitions', async () => {
    const server = new PictlMCPServer();
    const tools = server.getAvailableTools();
    for (const tool of tools) {
      for (const req of tool.inputSchema.required ?? []) {
        expect(tool.inputSchema.properties[req]).toBeDefined();
      }
    }
  });
  it('all tools have non-empty descriptions', async () => {
    const server = new PictlMCPServer();
    const tools = server.getAvailableTools();
    for (const tool of tools) {
      expect(tool.description.length).toBeGreaterThan(0);
    }
  });
  it('discovery tools require xes_content parameter', async () => {
    const server = new PictlMCPServer();
    const tools = server.getAvailableTools();
    const discoveryTools = tools.filter(
      (t) => t.name.startsWith('discover_') && !t.name.includes('ocel') && !t.name.includes('oc_')
    );
    for (const tool of discoveryTools) {
      expect(tool.inputSchema.required).toContain('xes_content');
    }
  });
  it('OCEL tools require ocel_handle parameter', async () => {
    const server = new PictlMCPServer();
    const tools = server.getAvailableTools();
    const ocelTools = tools.filter((t) => t.name.includes('ocel') || t.name.includes('oc_'));
    for (const tool of ocelTools) {
      if (tool.name !== 'load_ocel') {
        expect(tool.inputSchema.required).toContain('ocel_handle');
      }
    }
  });
});
//# sourceMappingURL=mcp_integration.test.js.map
