/**
 * MCP Server Builder - Tool/Resource/Prompt DSL
 *
 * Σ_mcp (server construction):
 *   S ≔ (T, R, P, σ)
 *   T ≔ {t₁,…,tₙ}              // tool definitions
 *   R ≔ {r₁,…,rₘ}              // resource definitions
 *   P ≔ {p₁,…,pₖ}              // prompt templates
 *   σ: (T ∪ R ∪ P) → Schema   // schema mapping
 *
 * Law (Composability):
 *   S₁ ⊕ S₂ = S₃ where T₃ = T₁ ∪ T₂, R₃ = R₁ ∪ R₂, P₃ = P₁ ∪ P₂
 *
 * @module @unrdf/kgc-claude/mcp-server-builder
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';

/**
 * Tool parameter schema
 */
export const ToolParameterSchema = z.object({
  name: z.string(),
  description: z.string(),
  type: z.enum(['string', 'number', 'boolean', 'object', 'array']),
  required: z.boolean().default(false),
  schema: z.record(z.any()).optional(),
});

/**
 * @typedef {z.infer<typeof ToolParameterSchema>} ToolParameter
 */

/**
 * Tool definition schema
 */
export const ToolDefinitionSchema = z.object({
  name: z.string().regex(/^[a-zA-Z0-9_-]+$/),
  description: z.string(),
  parameters: z.array(ToolParameterSchema).default([]),
  handler: z.function(),
  metadata: z.record(z.any()).default({}),
});

/**
 * @typedef {z.infer<typeof ToolDefinitionSchema>} ToolDefinition
 */

/**
 * Resource definition schema
 */
export const ResourceDefinitionSchema = z.object({
  uri: z.string().url(),
  name: z.string(),
  description: z.string(),
  mimeType: z.string().default('text/plain'),
  handler: z.function(),
  metadata: z.record(z.any()).default({}),
});

/**
 * @typedef {z.infer<typeof ResourceDefinitionSchema>} ResourceDefinition
 */

/**
 * Prompt template schema
 */
export const PromptTemplateSchema = z.object({
  name: z.string().regex(/^[a-zA-Z0-9_-]+$/),
  description: z.string(),
  template: z.string(),
  arguments: z.array(ToolParameterSchema).default([]),
  metadata: z.record(z.any()).default({}),
});

/**
 * @typedef {z.infer<typeof PromptTemplateSchema>} PromptTemplate
 */

/**
 * MCP server configuration schema
 */
export const MCPServerConfigSchema = z.object({
  name: z.string().regex(/^[a-zA-Z0-9_-]+$/),
  version: z.string().default('1.0.0'),
  description: z.string(),
  tools: z.array(ToolDefinitionSchema).default([]),
  resources: z.array(ResourceDefinitionSchema).default([]),
  prompts: z.array(PromptTemplateSchema).default([]),
  transport: z.enum(['stdio', 'http', 'websocket']).default('stdio'),
  metadata: z.record(z.any()).default({}),
});

/**
 * @typedef {z.infer<typeof MCPServerConfigSchema>} MCPServerConfig
 */

/**
 * Server capability manifest
 */
export const CapabilityManifestSchema = z.object({
  server_name: z.string(),
  server_version: z.string(),
  tools_count: z.number().int(),
  resources_count: z.number().int(),
  prompts_count: z.number().int(),
  transport: z.string(),
  hash: z.string(),
  t_ns: z.bigint(),
});

/**
 * @typedef {z.infer<typeof CapabilityManifestSchema>} CapabilityManifest
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
 * MCP Server Builder - Composable server construction
 */
export class MCPServerBuilder {
  constructor() {
    /** @type {Map<string, ToolDefinition>} */
    this.tools = new Map();
    /** @type {Map<string, ResourceDefinition>} */
    this.resources = new Map();
    /** @type {Map<string, PromptTemplate>} */
    this.prompts = new Map();
    /** @type {MCPServerConfig | null} */
    this.config = null;
  }

  /**
   * Add tool definition
   * @param {ToolDefinition} tool
   * @returns {MCPServerBuilder}
   */
  addTool(tool) {
    const validated = ToolDefinitionSchema.parse(tool);
    this.tools.set(validated.name, validated);
    return this;
  }

  /**
   * Add resource definition
   * @param {ResourceDefinition} resource
   * @returns {MCPServerBuilder}
   */
  addResource(resource) {
    const validated = ResourceDefinitionSchema.parse(resource);
    this.resources.set(validated.uri, validated);
    return this;
  }

  /**
   * Add prompt template
   * @param {PromptTemplate} prompt
   * @returns {MCPServerBuilder}
   */
  addPrompt(prompt) {
    const validated = PromptTemplateSchema.parse(prompt);
    this.prompts.set(validated.name, validated);
    return this;
  }

  /**
   * Build server configuration
   * @param {string} name
   * @param {string} description
   * @param {object} options
   * @returns {Promise<MCPServerConfig>}
   */
  async build(name, description, options = {}) {
    const config = MCPServerConfigSchema.parse({
      name,
      description,
      tools: Array.from(this.tools.values()),
      resources: Array.from(this.resources.values()),
      prompts: Array.from(this.prompts.values()),
      transport: options.transport || 'stdio',
      version: options.version || '1.0.0',
      metadata: options.metadata || {},
    });

    this.config = config;
    return config;
  }

  /**
   * Generate capability manifest
   * @returns {Promise<CapabilityManifest>}
   */
  async generateManifest() {
    if (!this.config) {
      throw new Error('Server not built - call build() first');
    }

    const manifestData = {
      server_name: this.config.name,
      server_version: this.config.version,
      tools_count: this.config.tools.length,
      resources_count: this.config.resources.length,
      prompts_count: this.config.prompts.length,
      transport: this.config.transport,
    };

    const hash = await blake3(JSON.stringify(manifestData));

    return CapabilityManifestSchema.parse({
      ...manifestData,
      hash,
      t_ns: now(),
    });
  }

  /**
   * Compose with another builder (S₁ ⊕ S₂)
   * @param {MCPServerBuilder} other
   * @returns {MCPServerBuilder}
   */
  compose(other) {
    const composed = new MCPServerBuilder();

    // Merge tools
    for (const [name, tool] of this.tools) {
      composed.tools.set(name, tool);
    }
    for (const [name, tool] of other.tools) {
      composed.tools.set(name, tool);
    }

    // Merge resources
    for (const [uri, resource] of this.resources) {
      composed.resources.set(uri, resource);
    }
    for (const [uri, resource] of other.resources) {
      composed.resources.set(uri, resource);
    }

    // Merge prompts
    for (const [name, prompt] of this.prompts) {
      composed.prompts.set(name, prompt);
    }
    for (const [name, prompt] of other.prompts) {
      composed.prompts.set(name, prompt);
    }

    return composed;
  }

  /**
   * Export server configuration to JSON
   * @returns {string}
   */
  toJSON() {
    if (!this.config) {
      throw new Error('Server not built - call build() first');
    }
    return JSON.stringify(this.config, null, 2);
  }

  /**
   * Get tool by name
   * @param {string} name
   * @returns {ToolDefinition | undefined}
   */
  getTool(name) {
    return this.tools.get(name);
  }

  /**
   * Get resource by URI
   * @param {string} uri
   * @returns {ResourceDefinition | undefined}
   */
  getResource(uri) {
    return this.resources.get(uri);
  }

  /**
   * Get prompt by name
   * @param {string} name
   * @returns {PromptTemplate | undefined}
   */
  getPrompt(name) {
    return this.prompts.get(name);
  }

  /**
   * Clear all definitions
   */
  clear() {
    this.tools.clear();
    this.resources.clear();
    this.prompts.clear();
    this.config = null;
  }
}

/**
 * Create MCP server builder instance
 * @returns {MCPServerBuilder}
 */
export function createServerBuilder() {
  return new MCPServerBuilder();
}

/**
 * Tool DSL helper - fluent API for tool definition
 * @param {string} name
 * @param {string} description
 * @returns {object}
 */
export function tool(name, description) {
  const params = [];

  return {
    param(paramName, paramDesc, type = 'string', required = false) {
      params.push({ name: paramName, description: paramDesc, type, required });
      return this;
    },
    handler(fn) {
      return {
        name,
        description,
        parameters: params,
        handler: fn,
        metadata: {},
      };
    },
  };
}

/**
 * Resource DSL helper
 * @param {string} uri
 * @param {string} name
 * @param {string} description
 * @returns {object}
 */
export function resource(uri, name, description) {
  return {
    mimeType: 'text/plain',
    metadata: {},
    withMimeType(mime) {
      this.mimeType = mime;
      return this;
    },
    handler(fn) {
      return {
        uri,
        name,
        description,
        mimeType: this.mimeType,
        handler: fn,
        metadata: this.metadata,
      };
    },
  };
}

/**
 * Prompt DSL helper
 * @param {string} name
 * @param {string} description
 * @param {string} template
 * @returns {object}
 */
export function prompt(name, description, template) {
  const args = [];

  return {
    arg(argName, argDesc, type = 'string', required = false) {
      args.push({ name: argName, description: argDesc, type, required });
      return this;
    },
    build() {
      return {
        name,
        description,
        template,
        arguments: args,
        metadata: {},
      };
    },
  };
}

export default MCPServerBuilder;
