/**
 * @file Policy Pack abstraction for versioned governance units
 * @module policy-pack
 *
 * @description
 * Policy packs bundle related knowledge hooks into versioned, portable
 * governance units that can be activated/deactivated as cohesive sets.
 */

import { readFileSync, writeFileSync, existsSync, mkdirSync, readdirSync } from 'fs';
import { join, dirname, basename, extname } from 'path';
import { createKnowledgeHook, validateKnowledgeHook } from './schemas.mjs';
import { z } from 'zod';
import { randomUUID } from 'crypto';

/**
 * Schema for policy pack metadata
 */
const PolicyPackMetaSchema = z.object({
  name: z
    .string()
    .min(1)
    .max(100)
    .regex(
      /^[a-zA-Z0-9:_-]+$/,
      'Name must contain only alphanumeric characters, colons, hyphens, and underscores'
    ),
  version: z.string().regex(/^\d+\.\d+\.\d+$/, 'Version must be semantic version format'),
  description: z.string().min(1).max(1000).optional(),
  author: z.string().min(1).max(100).optional(),
  license: z.string().min(1).max(100).optional(),
  tags: z.array(z.string().min(1).max(50)).max(20).optional(),
  ontology: z.array(z.string().min(1).max(100)).max(10).optional(),
  dependencies: z
    .array(
      z.object({
        name: z.string().min(1),
        version: z.string().min(1),
        required: z.boolean().default(true),
      })
    )
    .max(20)
    .optional(),
  createdAt: z.coerce.date().optional(),
  updatedAt: z.coerce.date().optional(),
});

/**
 * Schema for policy pack configuration
 */
const PolicyPackConfigSchema = z.object({
  enabled: z.boolean().default(true),
  priority: z.number().int().min(0).max(100).default(50),
  strictMode: z.boolean().default(false),
  timeout: z.number().int().positive().max(300000).default(30000),
  retries: z.number().int().nonnegative().max(5).default(1),
  conditions: z
    .object({
      environment: z.array(z.string()).optional(),
      version: z.string().optional(),
      features: z.array(z.string()).optional(),
    })
    .optional(),
});

/**
 * Schema for policy pack manifest
 */
const PolicyPackManifestSchema = z.object({
  id: z.string().uuid(),
  meta: PolicyPackMetaSchema,
  config: PolicyPackConfigSchema,
  hooks: z.array(
    z.object({
      name: z.string().min(1),
      file: z.string().min(1),
      enabled: z.boolean().default(true),
      priority: z.number().int().min(0).max(100).default(50),
    })
  ),
  conditions: z
    .array(
      z.object({
        name: z.string().min(1),
        file: z.string().min(1),
        type: z.enum(['sparql-ask', 'sparql-select', 'shacl']),
      })
    )
    .optional(),
  resources: z
    .array(
      z.object({
        name: z.string().min(1),
        file: z.string().min(1),
        type: z.enum(['ontology', 'vocabulary', 'data', 'other']),
      })
    )
    .optional(),
});

/**
 * Policy Pack class for managing versioned governance units
 */
export class PolicyPack {
  /**
   * Create a new policy pack
   * @param {Object} manifest - Policy pack manifest
   * @param {string} [basePath] - Base path for file resolution
   */
  constructor(manifest, basePath = process.cwd()) {
    this.basePath = basePath;
    this.manifest = PolicyPackManifestSchema.parse(manifest);
    this.hooks = new Map();
    this.conditions = new Map();
    this.resources = new Map();
    this.loaded = false;
  }

  /**
   * Load the policy pack from filesystem
   * @returns {Promise<void>}
   */
  async load() {
    if (this.loaded) return;

    const packPath = join(this.basePath, 'policy-packs', this.manifest.meta.name);

    // Load hooks
    for (const hookDef of this.manifest.hooks) {
      if (!hookDef.enabled) continue;

      const hookFile = join(packPath, hookDef.file);
      if (!existsSync(hookFile)) {
        throw new Error(`Hook file not found: ${hookFile}`);
      }

      const hookModule = await import(`file://${hookFile}`);
      const hook = hookModule.default || hookModule;

      // Validate hook
      const validation = validateKnowledgeHook(hook);
      if (!validation.success) {
        throw new Error(
          `Invalid hook ${hookDef.name}: ${validation.errors.map(e => e.message).join(', ')}`
        );
      }

      // Set priority from manifest
      hook.priority = hookDef.priority;

      this.hooks.set(hookDef.name, hook);
    }

    // Load conditions
    if (this.manifest.conditions) {
      for (const conditionDef of this.manifest.conditions) {
        const conditionFile = join(packPath, conditionDef.file);
        if (!existsSync(conditionFile)) {
          throw new Error(`Condition file not found: ${conditionFile}`);
        }

        const conditionContent = readFileSync(conditionFile, 'utf8');
        this.conditions.set(conditionDef.name, {
          content: conditionContent,
          type: conditionDef.type,
          file: conditionDef.file,
        });
      }
    }

    // Load resources
    if (this.manifest.resources) {
      for (const resourceDef of this.manifest.resources) {
        const resourceFile = join(packPath, resourceDef.file);
        if (!existsSync(resourceFile)) {
          throw new Error(`Resource file not found: ${resourceFile}`);
        }

        const resourceContent = readFileSync(resourceFile, 'utf8');
        this.resources.set(resourceDef.name, {
          content: resourceContent,
          type: resourceDef.type,
          file: resourceDef.file,
        });
      }
    }

    this.loaded = true;
  }

  /**
   * Get all hooks in this policy pack
   * @returns {Array} Array of hook definitions
   */
  getHooks() {
    if (!this.loaded) {
      throw new Error('Policy pack not loaded. Call load() first.');
    }

    return Array.from(this.hooks.values());
  }

  /**
   * Get a specific hook by name
   * @param {string} name - Hook name
   * @returns {Object} Hook definition or null
   */
  getHook(name) {
    if (!this.loaded) {
      throw new Error('Policy pack not loaded. Call load() first.');
    }

    return this.hooks.get(name) || null;
  }

  /**
   * Get all conditions in this policy pack
   * @returns {Array} Array of condition definitions
   */
  getConditions() {
    if (!this.loaded) {
      throw new Error('Policy pack not loaded. Call load() first.');
    }

    return Array.from(this.conditions.values());
  }

  /**
   * Get a specific condition by name
   * @param {string} name - Condition name
   * @returns {Object} Condition definition or null
   */
  getCondition(name) {
    if (!this.loaded) {
      throw new Error('Policy pack not loaded. Call load() first.');
    }

    return this.conditions.get(name) || null;
  }

  /**
   * Get all resources in this policy pack
   * @returns {Array} Array of resource definitions
   */
  getResources() {
    if (!this.loaded) {
      throw new Error('Policy pack not loaded. Call load() first.');
    }

    return Array.from(this.resources.values());
  }

  /**
   * Get a specific resource by name
   * @param {string} name - Resource name
   * @returns {Object} Resource definition or null
   */
  getResource(name) {
    if (!this.loaded) {
      throw new Error('Policy pack not loaded. Call load() first.');
    }

    return this.resources.get(name) || null;
  }

  /**
   * Check if this policy pack is compatible with the current environment
   * @param {Object} [environment] - Environment information
   * @returns {Object} Compatibility check result
   */
  checkCompatibility(environment = {}) {
    const result = {
      compatible: true,
      issues: [],
      warnings: [],
    };

    // Check version compatibility
    if (this.manifest.config.conditions?.version) {
      const requiredVersion = this.manifest.config.conditions.version;
      const currentVersion = environment.version || '1.0.0';

      if (!this._isVersionCompatible(currentVersion, requiredVersion)) {
        result.compatible = false;
        result.issues.push(
          `Version ${currentVersion} is not compatible with required ${requiredVersion}`
        );
      }
    }

    // Check environment compatibility
    if (this.manifest.config.conditions?.environment) {
      const requiredEnvs = this.manifest.config.conditions.environment;
      const currentEnv = environment.environment || 'development';

      if (!requiredEnvs.includes(currentEnv)) {
        result.warnings.push(
          `Environment ${currentEnv} not in required list: ${requiredEnvs.join(', ')}`
        );
      }
    }

    // Check feature compatibility
    if (this.manifest.config.conditions?.features) {
      const requiredFeatures = this.manifest.config.conditions.features;
      const availableFeatures = environment.features || [];

      for (const feature of requiredFeatures) {
        if (!availableFeatures.includes(feature)) {
          result.compatible = false;
          result.issues.push(`Required feature ${feature} is not available`);
        }
      }
    }

    return result;
  }

  /**
   * Get policy pack statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      id: this.manifest.id,
      name: this.manifest.meta.name,
      version: this.manifest.meta.version,
      loaded: this.loaded,
      hooks: {
        total: this.manifest.hooks.length,
        enabled: this.manifest.hooks.filter(h => h.enabled).length,
        loaded: this.hooks.size,
      },
      conditions: {
        total: this.manifest.conditions?.length || 0,
        loaded: this.conditions.size,
      },
      resources: {
        total: this.manifest.resources?.length || 0,
        loaded: this.resources.size,
      },
      config: this.manifest.config,
    };
  }

  /**
   * Check if version is compatible
   * @param {string} current - Current version
   * @param {string} required - Required version
   * @returns {boolean} Is compatible
   * @private
   */
  _isVersionCompatible(current, required) {
    // Simple version compatibility check
    // In production, this would use proper semver parsing
    const currentParts = current.split('.').map(Number);
    const requiredParts = required.split('.').map(Number);

    // Check major version compatibility
    return currentParts[0] >= requiredParts[0];
  }
}

/**
 * Policy Pack Manager for managing multiple policy packs
 */
export class PolicyPackManager {
  /**
   * Create a new policy pack manager
   * @param {string} [basePath] - Base path for policy packs
   */
  constructor(basePath = process.cwd()) {
    this.basePath = basePath;
    this.packs = new Map();
    this.activePacks = new Set();
  }

  /**
   * Load a policy pack from manifest file
   * @param {string} manifestPath - Path to manifest file
   * @returns {Promise<PolicyPack>} Loaded policy pack
   */
  async loadPolicyPack(manifestPath) {
    if (!existsSync(manifestPath)) {
      throw new Error(`Manifest file not found: ${manifestPath}`);
    }

    const manifestContent = readFileSync(manifestPath, 'utf8');
    const manifest = JSON.parse(manifestContent);

    const pack = new PolicyPack(manifest, this.basePath);
    await pack.load();

    this.packs.set(pack.manifest.meta.name, pack);
    return pack;
  }

  /**
   * Load all policy packs from a directory
   * @param {string} [packsDir] - Directory containing policy packs
   * @returns {Promise<Array<PolicyPack>>} Array of loaded policy packs
   */
  async loadAllPolicyPacks(packsDir = join(this.basePath, 'policy-packs')) {
    if (!existsSync(packsDir)) {
      return [];
    }

    const packs = [];
    const entries = readdirSync(packsDir, { withFileTypes: true });

    for (const entry of entries) {
      if (entry.isDirectory()) {
        const manifestPath = join(packsDir, entry.name, 'manifest.json');
        if (existsSync(manifestPath)) {
          try {
            const pack = await this.loadPolicyPack(manifestPath);
            packs.push(pack);
          } catch (error) {
            console.warn(`Failed to load policy pack ${entry.name}:`, error.message);
          }
        }
      }
    }

    return packs;
  }

  /**
   * Activate a policy pack
   * @param {string} packName - Policy pack name
   * @returns {boolean} Success
   */
  activatePolicyPack(packName) {
    const pack = this.packs.get(packName);
    if (!pack) {
      throw new Error(`Policy pack ${packName} not found`);
    }

    if (!pack.manifest.config.enabled) {
      throw new Error(`Policy pack ${packName} is disabled`);
    }

    this.activePacks.add(packName);
    return true;
  }

  /**
   * Deactivate a policy pack
   * @param {string} packName - Policy pack name
   * @returns {boolean} Success
   */
  deactivatePolicyPack(packName) {
    return this.activePacks.delete(packName);
  }

  /**
   * Get all active policy packs
   * @returns {Array<PolicyPack>} Array of active policy packs
   */
  getActivePolicyPacks() {
    return Array.from(this.activePacks)
      .map(name => this.packs.get(name))
      .filter(pack => pack !== undefined);
  }

  /**
   * Get all hooks from active policy packs
   * @returns {Array} Array of hook definitions
   */
  getActiveHooks() {
    const hooks = [];

    for (const packName of this.activePacks) {
      const pack = this.packs.get(packName);
      if (pack) {
        hooks.push(...pack.getHooks());
      }
    }

    // Sort by priority
    return hooks.sort((a, b) => (b.priority || 50) - (a.priority || 50));
  }

  /**
   * Get policy pack by name
   * @param {string} name - Policy pack name
   * @returns {PolicyPack} Policy pack or null
   */
  getPolicyPack(name) {
    return this.packs.get(name) || null;
  }

  /**
   * Get all policy packs
   * @returns {Array<PolicyPack>} Array of all policy packs
   */
  getAllPolicyPacks() {
    return Array.from(this.packs.values());
  }

  /**
   * Get manager statistics
   * @returns {Object} Statistics
   */
  getStats() {
    const _activePacks = this.getActivePolicyPacks();
    const allHooks = this.getActiveHooks();

    return {
      totalPacks: this.packs.size,
      activePacks: this.activePacks.size,
      totalHooks: allHooks.length,
      packs: Array.from(this.packs.values()).map(pack => pack.getStats()),
    };
  }
}

/**
 * Create a policy pack from a directory structure
 * @param {string} packDir - Directory containing policy pack files
 * @returns {Promise<PolicyPack>} Created policy pack
 */
export async function createPolicyPackFromDirectory(packDir) {
  const manifestPath = join(packDir, 'manifest.json');

  if (!existsSync(manifestPath)) {
    throw new Error(`Manifest file not found: ${manifestPath}`);
  }

  const manager = new PolicyPackManager(dirname(packDir));
  return manager.loadPolicyPack(manifestPath);
}

/**
 * Create a new policy pack manifest
 * @param {Object} options - Manifest options
 * @returns {Object} Policy pack manifest
 */
export function createPolicyPackManifest(name, hooks, options = {}) {
  const manifest = {
    id: randomUUID(),
    meta: {
      name: name,
      version: options.version || '1.0.0',
      description: options.description,
      author: options.author,
      license: options.license || 'MIT',
      tags: options.tags || [],
      ontology: options.ontology || [],
      dependencies: options.dependencies || [],
      createdAt: new Date().toISOString(),
    },
    config: {
      enabled: options.enabled !== false,
      priority: options.priority || 50,
      strictMode: options.strictMode || false,
      timeout: options.timeout || 30000,
      retries: options.retries || 1,
      conditions: options.conditions || {},
    },
    hooks: hooks.map(hook => ({
      name: hook.meta.name,
      file: `${hook.meta.name}.mjs`,
      enabled: true,
      priority: hook.priority || 50,
    })),
    conditions: options.conditions || [],
    resources: options.resources || [],
  };

  return PolicyPackManifestSchema.parse(manifest);
}
