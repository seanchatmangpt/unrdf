/**
 * @file UNRDF Tera Template Engine
 * @module @unrdf/tera-engine
 * @description Tera-compatible template engine with RDF/Turtle and TOML integration
 *
 * @example
 * import { createRdfTeraEngine, quickRender } from '@unrdf/tera-engine';
 *
 * // Basic usage
 * const output = await quickRender({
 *   template: 'Hello {{ name | upper }}!',
 *   context: { name: 'world' }
 * });
 *
 * // With RDF and TOML
 * const engine = createRdfTeraEngine();
 * const result = await engine.render('template', {
 *   toml: { path: 'config.toml' },
 *   rdf: { path: 'data.ttl' }
 * });
 */

// Core engine
export {
  TeraEngine,
  createTeraEngine,
  renderTemplate
} from './engine.mjs';

// Filters
export {
  // Standard filters
  upper,
  lower,
  capitalize,
  truncate,
  join,
  length,
  first,
  last,
  reverse,
  replace,
  // RDF filters
  localname,
  namespace,
  prefixedName,
  ntriples,
  turtle,
  subjects,
  predicates,
  objects,
  filterByPredicate,
  // Filter collections
  getStandardFilters,
  getRdfFilters,
  getAllFilters,
} from './filters.mjs';

// Loaders
export {
  loadToml,
  loadTomlSync,
  loadRdf,
  loadRdfContext,
  querySparql,
  mergeContexts,
} from './loaders.mjs';

// High-level API
export {
  createRdfTeraEngine,
  quickRender,
  renderWithKnowledge,
} from './api.mjs';

// Schemas
export {
  TemplateContextSchema,
  TemplateOptionsSchema,
  FilterFunctionSchema,
  FilterRegistrySchema,
  TomlLoaderOptionsSchema,
  RdfLoaderOptionsSchema,
  SparqlQueryOptionsSchema,
  RenderOptionsSchema,
  EngineConfigSchema,
  validateContext,
  validateOptions,
  validateEngineConfig,
  validateRenderOptions,
  validateTomlOptions,
  validateRdfOptions,
  validateSparqlOptions,
} from './schemas.mjs';
