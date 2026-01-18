/**
 * @file Public API for Tera Engine
 * @module @unrdf/tera-engine/api
 * @description High-level API combining engine, loaders, and filters
 */

import { createTeraEngine } from './engine.mjs';
import { getAllFilters } from './filters.mjs';
import { loadToml, loadRdfContext, mergeContexts } from './loaders.mjs';
import { validateRenderOptions } from './schemas.mjs';

/**
 * Creates a fully-configured Tera engine with RDF support
 * @param {Object} [config] - Engine configuration
 * @param {boolean} [config.includeRdfFilters=true] - Include RDF filters
 * @param {Record<string, Function>} [config.customFilters] - Additional custom filters
 * @param {Object} [config.options] - Default template options
 * @returns {Object} Configured engine with loaders
 * @example
 * const engine = createRdfTeraEngine();
 * const output = await engine.renderFile('template.html', {
 *   toml: { path: 'config.toml' },
 *   rdf: { path: 'data.ttl' }
 * });
 */
export function createRdfTeraEngine(config = {}) {
  const filters = config.includeRdfFilters !== false
    ? { ...getAllFilters(), ...config.customFilters }
    : { ...config.customFilters };

  const engine = createTeraEngine({
    filters,
    options: config.options,
  });

  return {
    engine,

    /**
     * Renders template with merged TOML/RDF context
     * @param {string} template - Template string
     * @param {Object} [sources] - Data sources
     * @param {Object} [sources.toml] - TOML loader options
     * @param {Object} [sources.rdf] - RDF loader options
     * @param {Object} [sources.context] - Additional context data
     * @param {Object} [options] - Template options
     * @returns {Promise<string>} Rendered output
     */
    async render(template, sources = {}, options = {}) {
      const context = await mergeContexts({
        toml: sources.toml,
        rdf: sources.rdf,
      });

      // Merge additional context
      if (sources.context) {
        Object.assign(context, sources.context);
      }

      return engine.render(template, context, options);
    },

    /**
     * Renders template file with merged context
     * @param {string} templatePath - Path to template file
     * @param {Object} [sources] - Data sources
     * @param {Object} [options] - Template options
     * @returns {Promise<string>} Rendered output
     */
    async renderFile(templatePath, sources = {}, options = {}) {
      const { readFile } = await import('fs/promises');
      const template = await readFile(templatePath, 'utf-8');
      return this.render(template, sources, options);
    },

    /**
     * Registers custom filter
     * @param {string} name - Filter name
     * @param {Function} fn - Filter function
     */
    registerFilter(name, fn) {
      engine.registerFilter(name, fn);
    },

    /**
     * Loads TOML data
     * @param {Object} options - Loader options
     * @returns {Promise<Object>} Parsed TOML
     */
    async loadToml(options) {
      return loadToml(options);
    },

    /**
     * Loads RDF data
     * @param {Object} options - Loader options
     * @returns {Promise<Object>} RDF store and triples
     */
    async loadRdf(options) {
      return loadRdfContext(options);
    },
  };
}

/**
 * Quick render helper with automatic context loading
 * @param {Object} options - Render options
 * @param {string} options.template - Template string or path
 * @param {Object} [options.toml] - TOML loader options
 * @param {Object} [options.rdf] - RDF loader options
 * @param {Object} [options.context] - Additional context
 * @param {Object} [options.options] - Template options
 * @param {boolean} [options.isFile=false] - Template is file path
 * @returns {Promise<string>} Rendered output
 * @example
 * const output = await quickRender({
 *   template: 'Hello {{ name | upper }}!',
 *   context: { name: 'world' }
 * });
 *
 * const output2 = await quickRender({
 *   template: 'template.html',
 *   isFile: true,
 *   toml: { path: 'config.toml' },
 *   rdf: { path: 'data.ttl' }
 * });
 */
export async function quickRender(options) {
  const validOptions = validateRenderOptions({
    template: options.template,
    context: options.context || {},
    options: options.options,
  });

  const engine = createRdfTeraEngine();

  if (options.isFile) {
    return engine.renderFile(
      validOptions.template,
      {
        toml: options.toml,
        rdf: options.rdf,
        context: validOptions.context,
      },
      validOptions.options
    );
  }

  return engine.render(
    validOptions.template,
    {
      toml: options.toml,
      rdf: options.rdf,
      context: validOptions.context,
    },
    validOptions.options
  );
}

/**
 * Integration with @unrdf/knowledge-engine
 * @param {Object} knowledgeEngine - Knowledge engine instance
 * @param {string} template - Template string
 * @param {Object} [context] - Additional context
 * @param {Object} [options] - Template options
 * @returns {Promise<string>} Rendered output
 * @example
 * import { KnowledgeHookManager } from '@unrdf/knowledge-engine';
 * const km = new KnowledgeHookManager();
 * const output = await renderWithKnowledge(km, 'Hello {{ name }}!', { name: 'RDF' });
 */
export async function renderWithKnowledge(knowledgeEngine, template, context = {}, options = {}) {
  const engine = createRdfTeraEngine();

  // Extract knowledge from engine
  const knowledgeContext = {
    hooks: knowledgeEngine.hooks || [],
    store: knowledgeEngine.store,
  };

  const mergedContext = {
    ...knowledgeContext,
    ...context,
  };

  return engine.engine.render(template, mergedContext, options);
}
