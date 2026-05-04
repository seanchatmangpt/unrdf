/**
 * Command Registry - Dynamic command management and discovery
 *
 * Hyper-advanced capability for runtime command registration, aliasing,
 * context-aware commands, and plugin-based command extension.
 *
 * @module @unrdf/kgc-claude/capabilities/command-registry
 */

import { z } from 'zod';
import { readdir, readFile, stat } from 'node:fs/promises';
import { join, basename } from 'node:path';
import { blake3 } from 'hash-wasm';

/**
 * Simple YAML frontmatter parser (minimal implementation)
 * @param {string} yaml - YAML string
 * @returns {Object} Parsed object
 */
function parseYaml(yaml) {
  const obj = {};
  const lines = yaml.split('\n');
  let currentKey = null;
  let arrayBuffer = [];
  let inArray = false;

  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith('#')) continue;

    // Simple key-value
    const kvMatch = trimmed.match(/^([a-zA-Z_][a-zA-Z0-9_]*):(.*)$/);
    if (kvMatch) {
      if (inArray && currentKey) {
        obj[currentKey] = arrayBuffer;
        arrayBuffer = [];
        inArray = false;
      }

      const [, key, value] = kvMatch;
      currentKey = key;
      const val = value.trim();

      if (val === '') {
        // Potential array start
        inArray = true;
      } else {
        obj[key] = val.replace(/^['"]|['"]$/g, ''); // Remove quotes
      }
    } else if (trimmed.startsWith('- ')) {
      // Array item
      const item = trimmed.slice(2).trim().replace(/^['"]|['"]$/g, '');
      arrayBuffer.push(item);
      inArray = true;
    }
  }

  // Flush any remaining array
  if (inArray && currentKey) {
    obj[currentKey] = arrayBuffer;
  }

  return obj;
}

/**
 * Command metadata schema (from frontmatter)
 */
const CommandMetadataSchema = z.object({
  name: z.string().min(1),
  description: z.string().min(1),
  version: z.string().optional(),
  arguments: z.array(z.any()).optional(),
  agents: z.array(z.string()).optional(),
  requires: z.array(z.string()).optional(),
  hooks: z.record(z.string(), z.string()).optional(),
  tags: z.array(z.string()).optional(),
  category: z.string().optional(),
  priority: z.enum(['low', 'medium', 'high', 'critical']).optional(),
  type: z.enum(['command', 'macro', 'plugin']).default('command'),
});

/**
 * Registered command entry schema
 */
export const CommandEntrySchema = z.object({
  path: z.string(), // Slash command path (e.g., '/research:explore')
  filePath: z.string(), // Filesystem path
  metadata: CommandMetadataSchema,
  namespace: z.string().optional(),
  contentHash: z.string(),
  lastModified: z.number(),
  aliases: z.array(z.string()).default([]),
  enabled: z.boolean().default(true),
});

/**
 * @typedef {z.infer<typeof CommandEntrySchema>} CommandEntry
 * @typedef {z.infer<typeof CommandMetadataSchema>} CommandMetadata
 */

/**
 * Command Registry - Centralized command discovery and management
 *
 * @example
 * const registry = new CommandRegistry();
 * await registry.scan('.claude/commands');
 * const cmd = registry.get('/research:explore');
 * const allResearch = registry.findByNamespace('research');
 */
export class CommandRegistry {
  constructor() {
    /** @type {Map<string, CommandEntry>} */
    this.commands = new Map();

    /** @type {Map<string, string>} */
    this.aliases = new Map();

    /** @type {Map<string, Set<string>>} */
    this.namespaceIndex = new Map();

    /** @type {Map<string, Set<string>>} */
    this.tagIndex = new Map();

    /** @type {Map<string, Set<string>>} */
    this.categoryIndex = new Map();
  }

  /**
   * Scan a directory for command files
   *
   * @param {string} basePath - Base directory to scan
   * @param {Object} [options]
   * @param {boolean} [options.recursive] - Scan subdirectories
   * @param {string[]} [options.exclude] - Paths to exclude
   * @returns {Promise<{ scanned: number, registered: number, errors: string[] }>}
   */
  async scan(basePath, options = {}) {
    const { recursive = true, exclude = [] } = options;
    const errors = [];
    let scanned = 0;
    let registered = 0;

    const scanDir = async (dir, namespace = '') => {
      try {
        const entries = await readdir(dir, { withFileTypes: true });

        for (const entry of entries) {
          const fullPath = join(dir, entry.name);

          // Check exclusions
          if (exclude.some((excl) => fullPath.includes(excl))) {
            continue;
          }

          if (entry.isDirectory() && recursive) {
            const subNamespace = namespace ? `${namespace}:${entry.name}` : entry.name;
            await scanDir(fullPath, subNamespace);
          } else if (entry.isFile() && entry.name.endsWith('.md')) {
            scanned++;

            try {
              const commandEntry = await this.loadCommand(fullPath, namespace);
              if (commandEntry) {
                this.register(commandEntry);
                registered++;
              }
            } catch (err) {
              errors.push(`Failed to load ${fullPath}: ${err.message}`);
            }
          }
        }
      } catch (err) {
        errors.push(`Failed to scan ${dir}: ${err.message}`);
      }
    };

    await scanDir(basePath);

    return { scanned, registered, errors };
  }

  /**
   * Load a single command file
   *
   * @param {string} filePath - Path to command markdown file
   * @param {string} [namespace] - Namespace prefix
   * @returns {Promise<CommandEntry | null>}
   */
  async loadCommand(filePath, namespace = '') {
    const content = await readFile(filePath, 'utf-8');
    const stats = await stat(filePath);

    // Extract YAML frontmatter
    const frontmatterMatch = content.match(/^---\s*\n([\s\S]*?)\n---/);
    if (!frontmatterMatch) {
      return null; // No frontmatter, skip
    }

    const frontmatter = parseYaml(frontmatterMatch[1]);
    const metadata = CommandMetadataSchema.parse(frontmatter);

    // Compute command path
    const fileName = basename(filePath, '.md');
    const commandPath = namespace ? `/${namespace}:${fileName}` : `/${fileName}`;

    // Compute content hash
    const contentHash = await blake3(content);

    return {
      path: commandPath,
      filePath,
      metadata,
      namespace: namespace || undefined,
      contentHash,
      lastModified: stats.mtimeMs,
      aliases: [],
      enabled: true,
    };
  }

  /**
   * Register a command entry
   *
   * @param {CommandEntry} entry
   */
  register(entry) {
    this.commands.set(entry.path, entry);

    // Update namespace index
    if (entry.namespace) {
      if (!this.namespaceIndex.has(entry.namespace)) {
        this.namespaceIndex.set(entry.namespace, new Set());
      }
      this.namespaceIndex.get(entry.namespace).add(entry.path);
    }

    // Update tag index
    if (entry.metadata.tags) {
      entry.metadata.tags.forEach((tag) => {
        if (!this.tagIndex.has(tag)) {
          this.tagIndex.set(tag, new Set());
        }
        this.tagIndex.get(tag).add(entry.path);
      });
    }

    // Update category index
    if (entry.metadata.category) {
      if (!this.categoryIndex.has(entry.metadata.category)) {
        this.categoryIndex.set(entry.metadata.category, new Set());
      }
      this.categoryIndex.get(entry.metadata.category).add(entry.path);
    }
  }

  /**
   * Get a command by path or alias
   *
   * @param {string} pathOrAlias - Command path or alias
   * @returns {CommandEntry | undefined}
   */
  get(pathOrAlias) {
    // Try direct path
    if (this.commands.has(pathOrAlias)) {
      return this.commands.get(pathOrAlias);
    }

    // Try alias
    const resolvedPath = this.aliases.get(pathOrAlias);
    if (resolvedPath) {
      return this.commands.get(resolvedPath);
    }

    return undefined;
  }

  /**
   * Check if a command exists
   *
   * @param {string} pathOrAlias
   * @returns {boolean}
   */
  has(pathOrAlias) {
    return this.get(pathOrAlias) !== undefined;
  }

  /**
   * Create an alias for a command
   *
   * @param {string} alias - Alias name
   * @param {string} commandPath - Command path
   * @returns {boolean} True if alias created
   */
  alias(alias, commandPath) {
    if (!this.commands.has(commandPath)) {
      return false;
    }

    this.aliases.set(alias, commandPath);

    // Add to command entry
    const entry = this.commands.get(commandPath);
    if (entry && !entry.aliases.includes(alias)) {
      entry.aliases.push(alias);
    }

    return true;
  }

  /**
   * Find commands by namespace
   *
   * @param {string} namespace
   * @returns {CommandEntry[]}
   */
  findByNamespace(namespace) {
    const paths = this.namespaceIndex.get(namespace);
    if (!paths) return [];

    return Array.from(paths)
      .map((path) => this.commands.get(path))
      .filter(Boolean);
  }

  /**
   * Find commands by tag
   *
   * @param {string} tag
   * @returns {CommandEntry[]}
   */
  findByTag(tag) {
    const paths = this.tagIndex.get(tag);
    if (!paths) return [];

    return Array.from(paths)
      .map((path) => this.commands.get(path))
      .filter(Boolean);
  }

  /**
   * Find commands by category
   *
   * @param {string} category
   * @returns {CommandEntry[]}
   */
  findByCategory(category) {
    const paths = this.categoryIndex.get(category);
    if (!paths) return [];

    return Array.from(paths)
      .map((path) => this.commands.get(path))
      .filter(Boolean);
  }

  /**
   * Search commands by text query
   *
   * @param {string} query - Search query
   * @param {Object} [options]
   * @param {string[]} [options.fields] - Fields to search (name, description, tags)
   * @param {boolean} [options.caseSensitive] - Case sensitive search
   * @returns {CommandEntry[]}
   */
  search(query, options = {}) {
    const { fields = ['name', 'description', 'tags'], caseSensitive = false } = options;
    const searchQuery = caseSensitive ? query : query.toLowerCase();

    const results = [];

    for (const entry of this.commands.values()) {
      let match = false;

      if (fields.includes('name')) {
        const name = caseSensitive ? entry.metadata.name : entry.metadata.name.toLowerCase();
        if (name.includes(searchQuery)) match = true;
      }

      if (fields.includes('description')) {
        const desc = caseSensitive ? entry.metadata.description : entry.metadata.description.toLowerCase();
        if (desc.includes(searchQuery)) match = true;
      }

      if (fields.includes('tags') && entry.metadata.tags) {
        const tags = caseSensitive ? entry.metadata.tags : entry.metadata.tags.map((t) => t.toLowerCase());
        if (tags.some((t) => t.includes(searchQuery))) match = true;
      }

      if (match) {
        results.push(entry);
      }
    }

    return results;
  }

  /**
   * List all commands
   *
   * @param {Object} [options]
   * @param {boolean} [options.enabledOnly] - Only return enabled commands
   * @param {string} [options.sortBy] - Sort by field (name, priority, lastModified)
   * @returns {CommandEntry[]}
   */
  list(options = {}) {
    const { enabledOnly = false, sortBy = 'name' } = options;

    let commands = Array.from(this.commands.values());

    if (enabledOnly) {
      commands = commands.filter((cmd) => cmd.enabled);
    }

    // Sort
    commands.sort((a, b) => {
      if (sortBy === 'name') {
        return a.metadata.name.localeCompare(b.metadata.name);
      } else if (sortBy === 'priority') {
        const priorityOrder = { critical: 0, high: 1, medium: 2, low: 3 };
        const aPri = priorityOrder[a.metadata.priority || 'medium'];
        const bPri = priorityOrder[b.metadata.priority || 'medium'];
        return aPri - bPri;
      } else if (sortBy === 'lastModified') {
        return b.lastModified - a.lastModified;
      }
      return 0;
    });

    return commands;
  }

  /**
   * Enable or disable a command
   *
   * @param {string} commandPath
   * @param {boolean} enabled
   * @returns {boolean} True if command found
   */
  setEnabled(commandPath, enabled) {
    const entry = this.get(commandPath);
    if (entry) {
      entry.enabled = enabled;
      return true;
    }
    return false;
  }

  /**
   * Get all namespaces
   *
   * @returns {string[]}
   */
  getNamespaces() {
    return Array.from(this.namespaceIndex.keys()).sort();
  }

  /**
   * Get all tags
   *
   * @returns {string[]}
   */
  getTags() {
    return Array.from(this.tagIndex.keys()).sort();
  }

  /**
   * Get all categories
   *
   * @returns {string[]}
   */
  getCategories() {
    return Array.from(this.categoryIndex.keys()).sort();
  }

  /**
   * Get registry statistics
   *
   * @returns {{ total: number, enabled: number, namespaces: number, tags: number, categories: number, byType: Record<string, number> }}
   */
  getStats() {
    const byType = {};

    for (const entry of this.commands.values()) {
      const type = entry.metadata.type || 'command';
      byType[type] = (byType[type] || 0) + 1;
    }

    return {
      total: this.commands.size,
      enabled: Array.from(this.commands.values()).filter((c) => c.enabled).length,
      namespaces: this.namespaceIndex.size,
      tags: this.tagIndex.size,
      categories: this.categoryIndex.size,
      byType,
    };
  }

  /**
   * Export registry to JSON
   *
   * @returns {{ commands: CommandEntry[], aliases: Record<string, string>, metadata: { version: string, generated: string } }}
   */
  toJSON() {
    return {
      commands: Array.from(this.commands.values()),
      aliases: Object.fromEntries(this.aliases),
      metadata: {
        version: '1.0.0',
        generated: new Date().toISOString(),
      },
    };
  }

  /**
   * Import registry from JSON
   *
   * @param {string} json - JSON string
   * @returns {{ imported: number, skipped: number }}
   */
  fromJSON(json) {
    const data = JSON.parse(json);
    let imported = 0;
    let skipped = 0;

    if (data.commands) {
      data.commands.forEach((entry) => {
        try {
          const validated = CommandEntrySchema.parse(entry);
          this.register(validated);
          imported++;
        } catch (err) {
          skipped++;
        }
      });
    }

    if (data.aliases) {
      Object.entries(data.aliases).forEach(([alias, path]) => {
        this.aliases.set(alias, path);
      });
    }

    return { imported, skipped };
  }

  /**
   * Clear all commands
   */
  clear() {
    this.commands.clear();
    this.aliases.clear();
    this.namespaceIndex.clear();
    this.tagIndex.clear();
    this.categoryIndex.clear();
  }
}

/**
 * Context-aware command resolver
 *
 * Resolves commands based on current context (project, user state, etc.)
 */
export class ContextualCommandResolver {
  /**
   * @param {CommandRegistry} registry
   */
  constructor(registry) {
    this.registry = registry;
    /** @type {Map<string, any>} */
    this.context = new Map();
  }

  /**
   * Set context value
   *
   * @param {string} key - Context key
   * @param {any} value - Context value
   */
  setContext(key, value) {
    this.context.set(key, value);
  }

  /**
   * Get context value
   *
   * @param {string} key
   * @returns {any}
   */
  getContext(key) {
    return this.context.get(key);
  }

  /**
   * Resolve command with context awareness
   *
   * @param {string} commandPath - Command path
   * @returns {CommandEntry | undefined}
   */
  resolve(commandPath) {
    let entry = this.registry.get(commandPath);
    if (!entry) return undefined;

    // Check if command is enabled
    if (!entry.enabled) return undefined;

    // Check requirements against context
    if (entry.metadata.requires) {
      const missingRequirements = entry.metadata.requires.filter((req) => {
        // Simple check - could be more sophisticated
        return !this.context.has(`requirement:${req}`);
      });

      if (missingRequirements.length > 0) {
        // Return entry but mark as unavailable
        return {
          ...entry,
          enabled: false,
          unavailableReason: `Missing requirements: ${missingRequirements.join(', ')}`,
        };
      }
    }

    return entry;
  }

  /**
   * Get recommended commands based on context
   *
   * @param {Object} [options]
   * @param {number} [options.limit] - Maximum recommendations
   * @returns {Array<{ command: CommandEntry, score: number, reason: string }>}
   */
  getRecommendations(options = {}) {
    const { limit = 10 } = options;
    const recommendations = [];

    // Get recent commands from context
    const recentCommands = this.context.get('recentCommands') || [];

    // Score commands
    for (const entry of this.registry.list({ enabledOnly: true })) {
      let score = 0;
      const reasons = [];

      // Priority boost
      if (entry.metadata.priority === 'high') score += 10;
      if (entry.metadata.priority === 'critical') score += 20;

      // Recent usage boost
      if (recentCommands.includes(entry.path)) {
        score += 15;
        reasons.push('Recently used');
      }

      // Context match (tags, category)
      const contextTags = this.context.get('tags') || [];
      const matchingTags = entry.metadata.tags?.filter((t) => contextTags.includes(t)) || [];
      if (matchingTags.length > 0) {
        score += matchingTags.length * 5;
        reasons.push(`Matches tags: ${matchingTags.join(', ')}`);
      }

      if (score > 0) {
        recommendations.push({
          command: entry,
          score,
          reason: reasons.join('; '),
        });
      }
    }

    // Sort by score descending
    recommendations.sort((a, b) => b.score - a.score);

    return recommendations.slice(0, limit);
  }
}

/**
 * Plugin-based command extension
 *
 * Allows dynamic command registration from plugins
 */
export class CommandPlugin {
  /**
   * @param {string} name - Plugin name
   */
  constructor(name) {
    this.name = name;
    /** @type {CommandEntry[]} */
    this.commands = [];
  }

  /**
   * Add a command to this plugin
   *
   * @param {CommandEntry} entry
   */
  addCommand(entry) {
    this.commands.push(entry);
  }

  /**
   * Install plugin into registry
   *
   * @param {CommandRegistry} registry
   * @returns {{ installed: number }}
   */
  install(registry) {
    let installed = 0;

    this.commands.forEach((entry) => {
      registry.register(entry);
      installed++;
    });

    return { installed };
  }

  /**
   * Uninstall plugin from registry
   *
   * @param {CommandRegistry} registry
   * @returns {{ removed: number }}
   */
  uninstall(registry) {
    let removed = 0;

    this.commands.forEach((entry) => {
      if (registry.commands.has(entry.path)) {
        registry.commands.delete(entry.path);
        removed++;
      }
    });

    return { removed };
  }
}
