/**
 * @file template-engine.mjs
 * @module @unrdf/chatman-equation/template-engine
 * @description Tera-compatible template engine using Nunjucks for documentation generation
 */

import nunjucks from 'nunjucks';
import { fileURLToPath } from 'url';
import { dirname, join, resolve } from 'path';
import { z } from 'zod';
import { readFileSync, writeFileSync, mkdirSync, existsSync } from 'fs';
import { loadConfig } from './config-loader.mjs';
import { registerCustomFilters } from './filters.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Template engine options schema
 */
const TemplateEngineOptionsSchema = z.object({
  templatesDir: z.string().default(resolve(__dirname, '../templates')),
  outputDir: z.string().default(resolve(__dirname, '../generated')),
  autoescape: z.boolean().default(true),
  trimBlocks: z.boolean().default(true),
  lstripBlocks: z.boolean().default(true),
  throwOnUndefined: z.boolean().default(false),
});

/**
 * Validate render context is an object
 * @param {*} context - Context to validate
 * @returns {Object} Validated context
 */
function validateContext(context) {
  if (typeof context !== 'object' || context === null) {
    throw new Error('Context must be an object');
  }
  return context;
}

/**
 * Template engine for generating documentation from TOML configs
 */
export class TemplateEngine {
  /**
   * @param {Object} [options] - Engine options
   * @param {string} [options.templatesDir] - Templates directory path
   * @param {string} [options.outputDir] - Output directory path
   * @param {boolean} [options.autoescape] - Enable autoescaping
   * @param {boolean} [options.trimBlocks] - Trim blocks
   * @param {boolean} [options.lstripBlocks] - Left-strip blocks
   * @param {boolean} [options.throwOnUndefined] - Throw on undefined variables
   */
  constructor(options = {}) {
    this.options = TemplateEngineOptionsSchema.parse(options);
    this.env = this._createEnvironment();
    this._registerFilters();
  }

  /**
   * Create Nunjucks environment with Tera-compatible settings
   * @private
   * @returns {nunjucks.Environment} Configured environment
   */
  _createEnvironment() {
    const env = new nunjucks.Environment(
      new nunjucks.FileSystemLoader(this.options.templatesDir, {
        watch: false,
        noCache: true,
      }),
      {
        autoescape: this.options.autoescape,
        trimBlocks: this.options.trimBlocks,
        lstripBlocks: this.options.lstripBlocks,
        throwOnUndefined: this.options.throwOnUndefined,
      }
    );

    return env;
  }

  /**
   * Register custom filters for template processing
   * @private
   */
  _registerFilters() {
    registerCustomFilters(this.env);
  }

  /**
   * Render a template with context data
   * @param {string} templateName - Template file name (e.g., 'equation-reference.tera')
   * @param {Object} context - Template context data
   * @returns {string} Rendered content
   * @throws {Error} If template not found or rendering fails
   * @example
   * const engine = new TemplateEngine();
   * const output = engine.render('equation-reference.tera', {
   *   title: 'Chatman Equation',
   *   version: '1.0.0',
   *   equations: [...]
   * });
   */
  render(templateName, context) {
    try {
      const validatedContext = validateContext(context);
      return this.env.render(templateName, validatedContext);
    } catch (error) {
      throw new Error(`Failed to render template ${templateName}: ${error.message}`);
    }
  }

  /**
   * Render template from string content
   * @param {string} templateString - Template content as string
   * @param {Object} context - Template context data
   * @returns {string} Rendered content
   * @example
   * const output = engine.renderString('Hello {{ name }}!', { name: 'World' });
   */
  renderString(templateString, context) {
    try {
      const validatedContext = validateContext(context);
      return this.env.renderString(templateString, validatedContext);
    } catch (error) {
      throw new Error(`Failed to render template string: ${error.message}`);
    }
  }

  /**
   * Render template and write to file
   * @param {string} templateName - Template file name
   * @param {Object} context - Template context data
   * @param {string} outputPath - Output file path (relative to outputDir or absolute)
   * @returns {string} Absolute path to written file
   * @throws {Error} If rendering or writing fails
   * @example
   * const filePath = engine.renderToFile(
   *   'equation-reference.tera',
   *   context,
   *   'reference/chatman-equation.md'
   * );
   */
  renderToFile(templateName, context, outputPath) {
    try {
      const rendered = this.render(templateName, context);
      const fullPath = resolve(this.options.outputDir, outputPath);
      const outputDir = dirname(fullPath);

      // Ensure output directory exists
      if (!existsSync(outputDir)) {
        mkdirSync(outputDir, { recursive: true });
      }

      writeFileSync(fullPath, rendered, 'utf-8');
      return fullPath;
    } catch (error) {
      throw new Error(`Failed to render to file ${outputPath}: ${error.message}`);
    }
  }

  /**
   * Generate documentation from TOML configuration file
   * @param {string} configPath - Path to TOML config file
   * @param {string} [outputPath] - Optional output path override
   * @returns {Object} Generation result with file paths
   * @throws {Error} If config loading or generation fails
   * @example
   * const result = engine.generateFromConfig(
   *   './config/chatman-equation.toml',
   *   'docs/reference/chatman.md'
   * );
   */
  generateFromConfig(configPath, outputPath) {
    try {
      const config = loadConfig(configPath);
      const templateName = config.template || 'equation-reference.tera';
      const output = outputPath || config.output || 'generated.md';

      const filePath = this.renderToFile(templateName, config, output);

      return {
        success: true,
        configPath,
        templateName,
        outputPath: filePath,
        context: config,
      };
    } catch (error) {
      throw new Error(`Failed to generate from config ${configPath}: ${error.message}`);
    }
  }

  /**
   * Batch generate multiple documents from config directory
   * @param {string} configDir - Directory containing TOML config files
   * @param {Object} [options] - Generation options
   * @param {string} [options.pattern] - Glob pattern for config files
   * @returns {Array<Object>} Array of generation results
   * @example
   * const results = engine.batchGenerate('./configs', {
   *   pattern: '**\/*.toml'
   * });
   */
  async batchGenerate(configDir, options = {}) {
    const { glob } = await import('glob');
    const pattern = options.pattern || '**/*.toml';
    const configFiles = await glob(pattern, { cwd: configDir, absolute: true });

    const results = [];
    for (const configPath of configFiles) {
      try {
        const result = this.generateFromConfig(configPath);
        results.push(result);
      } catch (error) {
        results.push({
          success: false,
          configPath,
          error: error.message,
        });
      }
    }

    return results;
  }

  /**
   * Get available templates
   * @returns {Promise<Array<string>>} List of template file names
   */
  async getAvailableTemplates() {
    const { readdirSync } = await import('fs');
    const templates = readdirSync(this.options.templatesDir);
    return templates.filter((f) => f.endsWith('.tera'));
  }

  /**
   * Add custom filter to environment
   * @param {string} name - Filter name
   * @param {Function} fn - Filter function
   * @example
   * engine.addFilter('uppercase', (str) => str.toUpperCase());
   */
  addFilter(name, fn) {
    this.env.addFilter(name, fn);
  }

  /**
   * Add custom global to environment
   * @param {string} name - Global variable name
   * @param {*} value - Global variable value
   * @example
   * engine.addGlobal('version', '1.0.0');
   */
  addGlobal(name, value) {
    this.env.addGlobal(name, value);
  }
}

/**
 * Create a new template engine instance
 * @param {Object} [options] - Engine options
 * @returns {TemplateEngine} New template engine
 * @example
 * const engine = createTemplateEngine({
 *   outputDir: './docs'
 * });
 */
export function createTemplateEngine(options = {}) {
  return new TemplateEngine(options);
}

export default TemplateEngine;
