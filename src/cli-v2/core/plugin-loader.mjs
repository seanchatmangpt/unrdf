/**
 * @file CLI Plugin Loader
 * @module cli-v2/core/plugin-loader
 *
 * @description
 * Loads and manages CLI plugins for extending functionality.
 */

import { readdir, readFile, access } from 'node:fs/promises';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';

/**
 * Plugin Loader for CLI extensions
 */
export class PluginLoader {
  constructor(config = {}) {
    this.config = config;
    this.plugins = new Map();
    this.pluginDirs = config.pluginDirs || [
      join(process.cwd(), '.unrdf/plugins'),
      join(process.env.HOME || process.env.USERPROFILE, '.unrdf/plugins')
    ];
  }

  /**
   * Load all plugins from plugin directories
   */
  async loadPlugins() {
    for (const dir of this.pluginDirs) {
      try {
        await access(dir);
        await this.loadPluginsFromDir(dir);
      } catch {
        // Directory doesn't exist, skip
      }
    }
  }

  /**
   * Load plugins from a specific directory
   * @param {string} dir - Plugin directory
   */
  async loadPluginsFromDir(dir) {
    try {
      const entries = await readdir(dir, { withFileTypes: true });

      for (const entry of entries) {
        if (entry.isDirectory()) {
          const pluginPath = join(dir, entry.name, 'index.mjs');
          try {
            await access(pluginPath);
            await this.loadPlugin(pluginPath);
          } catch {
            // Plugin index not found, skip
          }
        }
      }
    } catch (error) {
      console.error(`Failed to load plugins from ${dir}:`, error.message);
    }
  }

  /**
   * Load a single plugin
   * @param {string} path - Plugin file path
   */
  async loadPlugin(path) {
    try {
      const pluginUrl = pathToFileURL(path).href;
      const pluginModule = await import(pluginUrl);

      const plugin = pluginModule.default || pluginModule;

      if (!plugin.name) {
        throw new Error('Plugin must have a name');
      }

      if (typeof plugin.init !== 'function') {
        throw new Error('Plugin must have an init function');
      }

      // Initialize plugin
      await plugin.init(this.config);

      this.plugins.set(plugin.name, {
        ...plugin,
        path
      });

      console.log(`Loaded plugin: ${plugin.name}`);
    } catch (error) {
      console.error(`Failed to load plugin ${path}:`, error.message);
    }
  }

  /**
   * Get all loaded plugins
   * @returns {Array} List of plugins
   */
  getPlugins() {
    return Array.from(this.plugins.values());
  }

  /**
   * Get a specific plugin
   * @param {string} name - Plugin name
   * @returns {Object|null} Plugin instance
   */
  getPlugin(name) {
    return this.plugins.get(name) || null;
  }

  /**
   * Unload a plugin
   * @param {string} name - Plugin name
   */
  async unloadPlugin(name) {
    const plugin = this.plugins.get(name);
    if (!plugin) {
      return;
    }

    if (typeof plugin.cleanup === 'function') {
      await plugin.cleanup();
    }

    this.plugins.delete(name);
  }

  /**
   * Install a plugin from npm
   * @param {string} packageName - NPM package name
   */
  async install(packageName) {
    // TODO: Implement npm install integration
    throw new Error('Plugin installation not yet implemented');
  }

  /**
   * Cleanup all plugins
   */
  async cleanup() {
    for (const [name, plugin] of this.plugins.entries()) {
      if (typeof plugin.cleanup === 'function') {
        try {
          await plugin.cleanup();
        } catch (error) {
          console.error(`Failed to cleanup plugin ${name}:`, error.message);
        }
      }
    }

    this.plugins.clear();
  }
}
