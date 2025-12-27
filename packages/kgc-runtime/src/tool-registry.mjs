/**
 * @file Tool Registry - Central repository for tool manifests
 * @module @unrdf/kgc-runtime/tool-registry
 * @description Manages tool manifests, versioning, and capability queries
 */

import { z } from 'zod';
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

/**
 * Tool manifest schema
 */
const ManifestSchema = z.object({
  name: z.string(),
  version: z.string(),
  description: z.string().optional(),
  schema_in: z.any(), // Will be converted to Zod schema
  schema_out: z.any(), // Will be converted to Zod schema
  capabilities: z.array(z.string()),
});

/**
 * Tool Registry class for managing tool manifests
 */
export class ToolRegistry {
  /**
   * Create a new ToolRegistry
   * @param {Object} options - Configuration options
   * @param {string} options.registryPath - Path to registry JSON file
   */
  constructor(options = {}) {
    /** @type {Map<string, Object>} */
    this.tools = new Map();

    /** @type {Map<string, Map<string, Object>>} */
    this.versionedTools = new Map();

    this.registryPath = options.registryPath;

    if (this.registryPath) {
      this.loadFromFile(this.registryPath);
    }
  }

  /**
   * Load tool manifests from JSON file
   * @param {string} filePath - Path to registry JSON
   */
  loadFromFile(filePath) {
    try {
      const data = readFileSync(filePath, 'utf-8');
      const registry = JSON.parse(data);

      if (registry.tools && Array.isArray(registry.tools)) {
        for (const toolData of registry.tools) {
          this.registerTool(toolData);
        }
      }
    } catch (error) {
      throw new Error(`Failed to load registry from ${filePath}: ${error.message}`);
    }
  }

  /**
   * Register a tool manifest
   * @param {Object} manifest - Tool manifest
   * @throws {Error} If manifest is invalid
   */
  registerTool(manifest) {
    // Validate manifest structure
    ManifestSchema.parse(manifest);

    const { name, version } = manifest;

    // Convert schema definitions to Zod schemas if they're objects
    const processedManifest = {
      ...manifest,
      schema_in:
        typeof manifest.schema_in === 'object' &&
        !manifest.schema_in._def
          ? this.convertToZodSchema(manifest.schema_in)
          : manifest.schema_in,
      schema_out:
        typeof manifest.schema_out === 'object' &&
        !manifest.schema_out._def
          ? this.convertToZodSchema(manifest.schema_out)
          : manifest.schema_out,
    };

    // Store latest version by name
    this.tools.set(name, processedManifest);

    // Store versioned tool
    if (!this.versionedTools.has(name)) {
      this.versionedTools.set(name, new Map());
    }
    this.versionedTools.get(name).set(version, processedManifest);
  }

  /**
   * Convert schema definition to Zod schema
   * @param {Object} schemaDef - Schema definition
   * @returns {z.ZodSchema} Zod schema
   * @private
   */
  convertToZodSchema(schemaDef) {
    if (!schemaDef.type) {
      return z.any();
    }

    switch (schemaDef.type) {
      case 'object': {
        const shape = {};
        if (schemaDef.properties) {
          for (const [key, propDef] of Object.entries(
            schemaDef.properties,
          )) {
            let fieldSchema = this.convertToZodSchema(propDef);

            // Handle optional fields
            if (
              schemaDef.required &&
              !schemaDef.required.includes(key)
            ) {
              fieldSchema = fieldSchema.optional();
            }

            shape[key] = fieldSchema;
          }
        }
        return z.object(shape);
      }

      case 'string':
        return z.string();

      case 'number':
        return z.number();

      case 'boolean':
        return z.boolean();

      case 'array':
        return z.array(
          schemaDef.items
            ? this.convertToZodSchema(schemaDef.items)
            : z.any(),
        );

      case 'null':
        return z.null();

      default:
        return z.any();
    }
  }

  /**
   * Get tool manifest by name (latest version)
   * @param {string} name - Tool name
   * @returns {Object|null} Tool manifest or null if not found
   */
  getTool(name) {
    return this.tools.get(name) || null;
  }

  /**
   * Get tool manifest by name and version
   * @param {string} name - Tool name
   * @param {string} version - Tool version
   * @returns {Object|null} Tool manifest or null if not found
   */
  getToolVersion(name, version) {
    const versions = this.versionedTools.get(name);
    if (!versions) return null;
    return versions.get(version) || null;
  }

  /**
   * Get all tool manifests
   * @returns {Array<Object>} Array of all tool manifests
   */
  getAllTools() {
    return Array.from(this.tools.values());
  }

  /**
   * Get all versions of a tool
   * @param {string} name - Tool name
   * @returns {Array<Object>} Array of tool manifests for all versions
   */
  getAllVersions(name) {
    const versions = this.versionedTools.get(name);
    if (!versions) return [];
    return Array.from(versions.values());
  }

  /**
   * Query tools by capability
   * @param {string} capability - Capability to search for
   * @returns {Array<Object>} Array of tools with the capability
   */
  getToolsByCapability(capability) {
    return this.getAllTools().filter((tool) =>
      tool.capabilities.includes(capability),
    );
  }

  /**
   * Check if tool supports capability
   * @param {string} toolName - Tool name
   * @param {string} capability - Capability to check
   * @returns {boolean} True if tool supports capability
   */
  hasCapability(toolName, capability) {
    const tool = this.getTool(toolName);
    if (!tool) return false;
    return tool.capabilities.includes(capability);
  }

  /**
   * Validate tool output against registered schema
   * @param {string} toolName - Tool name
   * @param {*} output - Output to validate
   * @returns {boolean} True if valid
   */
  validateOutput(toolName, output) {
    const tool = this.getTool(toolName);
    if (!tool) {
      throw new Error(`Tool ${toolName} not found in registry`);
    }

    try {
      tool.schema_out.parse(output);
      return true;
    } catch {
      return false;
    }
  }

  /**
   * Get registry statistics
   * @returns {Object} Statistics about the registry
   */
  getStats() {
    const tools = this.getAllTools();
    const capabilities = new Set();

    for (const tool of tools) {
      for (const cap of tool.capabilities) {
        capabilities.add(cap);
      }
    }

    return {
      total_tools: tools.length,
      unique_capabilities: capabilities.size,
      capabilities: Array.from(capabilities),
    };
  }
}

/**
 * Create a ToolRegistry instance with default registry
 * @param {string} registryPath - Optional path to registry file
 * @returns {ToolRegistry} New ToolRegistry instance
 */
export function createRegistry(registryPath) {
  return new ToolRegistry({ registryPath });
}
