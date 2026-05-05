/**
 * MCP Federation - Multi-server coordination and routing
 *
 * Σ_federation (distributed MCP):
 *   Σ ≔ {S₁,…,Sₙ}                   // server registry
 *   ρ: Tool → S                     // routing function
 *   τ: Σ → CapabilitySet           // tool discovery
 *   merge: Σ → UnifiedCapability    // capability aggregation
 *
 * Law (Routing Invariant):
 *   ∀t ∈ T: ρ(t) ∈ Σ ∧ t ∈ τ(ρ(t))
 *
 * Law (Federation Completeness):
 *   ⋃_{S∈Σ} τ(S) = T_total
 *
 * @module @unrdf/kgc-claude/mcp-federation
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now, VectorClock } from '@unrdf/kgc-4d';

/**
 * Server registration schema
 */
export const ServerRegistrationSchema = z.object({
  id: z.string(),
  name: z.string(),
  endpoint: z.string(),
  transport: z.enum(['stdio', 'http', 'websocket']),
  capabilities: z.object({
    tools: z.array(z.string()).default([]),
    resources: z.array(z.string()).default([]),
    prompts: z.array(z.string()).default([]),
  }),
  priority: z.number().int().default(0),
  status: z.enum(['active', 'inactive', 'error']).default('active'),
  metadata: z.record(z.any()).default({}),
});

/**
 * @typedef {z.infer<typeof ServerRegistrationSchema>} ServerRegistration
 */

/**
 * Routing rule schema
 */
export const RoutingRuleSchema = z.object({
  pattern: z.string(),
  server_id: z.string(),
  priority: z.number().int().default(0),
  metadata: z.record(z.any()).default({}),
});

/**
 * @typedef {z.infer<typeof RoutingRuleSchema>} RoutingRule
 */

/**
 * Tool invocation request schema
 */
export const ToolInvocationSchema = z.object({
  id: z.string(),
  tool_name: z.string(),
  parameters: z.any().optional(),
  context: z.any().optional(),
  t_ns: z.bigint(),
});

/**
 * @typedef {z.infer<typeof ToolInvocationSchema>} ToolInvocation
 */

/**
 * Invocation result schema
 */
export const InvocationResultSchema = z.object({
  invocation_id: z.string(),
  server_id: z.string(),
  tool_name: z.string(),
  success: z.boolean(),
  result: z.any().optional(),
  error: z.string().optional(),
  duration_ms: z.number(),
  hash: z.string(),
  t_ns: z.bigint(),
});

/**
 * @typedef {z.infer<typeof InvocationResultSchema>} InvocationResult
 */

/**
 * Federation receipt schema
 */
export const FederationReceiptSchema = z.object({
  id: z.string(),
  servers_registered: z.number().int(),
  tools_available: z.number().int(),
  invocations_total: z.number().int(),
  invocations_success: z.number().int(),
  invocations_failed: z.number().int(),
  hash: z.string(),
  t_ns: z.bigint(),
});

/**
 * @typedef {z.infer<typeof FederationReceiptSchema>} FederationReceipt
 */

/**
 * Generate UUID v4
 */
function generateUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * MCP Federation - Multi-server coordination
 */
export class MCPFederation {
  constructor() {
    /** @type {Map<string, ServerRegistration>} */
    this.servers = new Map();
    /** @type {RoutingRule[]} */
    this.routingRules = [];
    /** @type {Map<string, string>} */
    this.toolToServer = new Map();
    /** @type {InvocationResult[]} */
    this.invocationHistory = [];
    this.vectorClock = new VectorClock('mcp-federation');
  }

  /**
   * Register MCP server
   * @param {ServerRegistration} registration
   * @returns {string} server ID
   */
  registerServer(registration) {
    const validated = ServerRegistrationSchema.parse(registration);
    const serverId = validated.id || generateUUID();

    this.servers.set(serverId, { ...validated, id: serverId });

    // Update tool-to-server mapping
    for (const tool of validated.capabilities.tools) {
      this.toolToServer.set(tool, serverId);
    }

    return serverId;
  }

  /**
   * Unregister server
   * @param {string} serverId
   * @returns {boolean}
   */
  unregisterServer(serverId) {
    const server = this.servers.get(serverId);
    if (!server) {
      return false;
    }

    // Remove tool mappings
    for (const tool of server.capabilities.tools) {
      this.toolToServer.delete(tool);
    }

    this.servers.delete(serverId);
    return true;
  }

  /**
   * Add routing rule
   * @param {RoutingRule} rule
   */
  addRoutingRule(rule) {
    const validated = RoutingRuleSchema.parse(rule);
    this.routingRules.push(validated);
    // Sort by priority (higher first)
    this.routingRules.sort((a, b) => b.priority - a.priority);
  }

  /**
   * Route tool to server (ρ: Tool → S)
   * @param {string} toolName
   * @returns {string | null} server ID
   */
  route(toolName) {
    // Check routing rules first
    for (const rule of this.routingRules) {
      const regex = new RegExp(rule.pattern);
      if (regex.test(toolName)) {
        return rule.server_id;
      }
    }

    // Fallback to direct mapping
    return this.toolToServer.get(toolName) || null;
  }

  /**
   * Discover capabilities across all servers (τ: Σ → CapabilitySet)
   * @returns {object}
   */
  discoverCapabilities() {
    const capabilities = {
      tools: new Set(),
      resources: new Set(),
      prompts: new Set(),
      servers: [],
    };

    for (const [serverId, server] of this.servers) {
      if (server.status !== 'active') continue;

      server.capabilities.tools.forEach((t) => capabilities.tools.add(t));
      server.capabilities.resources.forEach((r) => capabilities.resources.add(r));
      server.capabilities.prompts.forEach((p) => capabilities.prompts.add(p));

      capabilities.servers.push({
        id: serverId,
        name: server.name,
        transport: server.transport,
      });
    }

    return {
      tools: Array.from(capabilities.tools),
      resources: Array.from(capabilities.resources),
      prompts: Array.from(capabilities.prompts),
      servers: capabilities.servers,
    };
  }

  /**
   * Invoke tool on appropriate server
   * @param {string} toolName
   * @param {object} parameters
   * @param {object} context
   * @returns {Promise<InvocationResult>}
   */
  async invokeTool(toolName, parameters = {}, context = {}) {
    const invocationId = generateUUID();
    const startTime = performance.now();
    const t_ns = now();

    const invocation = ToolInvocationSchema.parse({
      id: invocationId,
      tool_name: toolName,
      parameters: Object.keys(parameters).length > 0 ? parameters : undefined,
      context: Object.keys(context).length > 0 ? context : undefined,
      t_ns,
    });

    // Route to server
    const serverId = this.route(toolName);
    if (!serverId) {
      const error = `No server registered for tool: ${toolName}`;
      const hash = await blake3(JSON.stringify({ invocationId, error }));

      const result = InvocationResultSchema.parse({
        invocation_id: invocationId,
        server_id: 'unknown',
        tool_name: toolName,
        success: false,
        error,
        duration_ms: performance.now() - startTime,
        hash,
        t_ns: now(),
      });

      this.invocationHistory.push(result);
      return result;
    }

    const server = this.servers.get(serverId);
    if (!server || server.status !== 'active') {
      const error = `Server ${serverId} not active`;
      const hash = await blake3(JSON.stringify({ invocationId, error }));

      const result = InvocationResultSchema.parse({
        invocation_id: invocationId,
        server_id: serverId,
        tool_name: toolName,
        success: false,
        error,
        duration_ms: performance.now() - startTime,
        hash,
        t_ns: now(),
      });

      this.invocationHistory.push(result);
      return result;
    }

    // Mock invocation (actual transport implementation would go here)
    const resultData = { invocationId, toolName, parameters, server: server.name };
    const hash = await blake3(JSON.stringify(resultData));

    const result = InvocationResultSchema.parse({
      invocation_id: invocationId,
      server_id: serverId,
      tool_name: toolName,
      success: true,
      result: resultData,
      duration_ms: performance.now() - startTime,
      hash,
      t_ns: now(),
    });

    this.invocationHistory.push(result);
    return result;
  }

  /**
   * Generate federation receipt
   * @returns {Promise<FederationReceipt>}
   */
  async generateReceipt() {
    const capabilities = this.discoverCapabilities();
    const successCount = this.invocationHistory.filter((i) => i.success).length;
    const failedCount = this.invocationHistory.filter((i) => !i.success).length;

    const receiptData = {
      servers_registered: this.servers.size,
      tools_available: capabilities.tools.length,
      invocations_total: this.invocationHistory.length,
      invocations_success: successCount,
      invocations_failed: failedCount,
    };

    const hash = await blake3(JSON.stringify(receiptData));

    return FederationReceiptSchema.parse({
      id: generateUUID(),
      ...receiptData,
      hash,
      t_ns: now(),
    });
  }

  /**
   * Get server by ID
   * @param {string} serverId
   * @returns {ServerRegistration | undefined}
   */
  getServer(serverId) {
    return this.servers.get(serverId);
  }

  /**
   * List all servers
   * @returns {ServerRegistration[]}
   */
  listServers() {
    return Array.from(this.servers.values());
  }

  /**
   * Get invocation history
   * @param {number} limit
   * @returns {InvocationResult[]}
   */
  getHistory(limit = 100) {
    return this.invocationHistory.slice(-limit);
  }

  /**
   * Clear all state
   */
  clear() {
    this.servers.clear();
    this.routingRules = [];
    this.toolToServer.clear();
    this.invocationHistory = [];
  }
}

/**
 * Create MCP federation instance
 * @returns {MCPFederation}
 */
export function createFederation() {
  return new MCPFederation();
}

export default MCPFederation;
