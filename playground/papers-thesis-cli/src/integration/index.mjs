/**
 * @fileoverview Integration layer exports
 *
 * @description
 * Exports all integration adapters for external services:
 * - Nunjucks template engine with custom LaTeX filters
 * - UNRDF knowledge graph integration
 * - File I/O operations for paper management
 *
 * @module integration
 * @version 1.0.0
 * @license MIT
 */

// =============================================================================
// Imports for createIntegration factory (must be at top in ESM)
// =============================================================================

import { createTemplateEngine as createTemplateEngineImpl } from './templates.mjs';
import { createKnowledgeGraph as createKnowledgeGraphImpl } from './knowledge-graph.mjs';
import * as fileIO from './file-io.mjs';

// =============================================================================
// Template Engine Exports
// =============================================================================

export {
  // Core template functions
  initTemplateEngine,
  getTemplateEngine,
  createTemplateEngine,
  templateEngine,
  renderTemplate,
  renderTemplateString,
  renderPaper,
  renderThesis,
  previewTemplate,

  // Template discovery and validation
  getAvailableTemplates,
  templateExists,
  validateContext,
  validatePartialContext,
  getTemplateMetadata,
  createContextFactory,

  // Constants
  DEFAULT_TEMPLATES_DIR,
  TEMPLATE_FAMILIES,
  TEMPLATE_EXTENSION,

  // Schemas
  TemplateContextSchema,
  AuthorSchema,
  SectionSchema,
  BibliographyEntrySchema,
  FigureSchema,
  TableSchema,
} from './templates.mjs';

// =============================================================================
// Nunjucks Filter Exports
// =============================================================================

export {
  // LaTeX-specific filters
  texescape,
  bibtexkey,
  latexjoin,

  // Formatting filters
  formatdate,
  formatauthor,
  formatnumber,

  // String transformation filters
  uppercase,
  lowercase,
  capitalize,
  titlecase,
  slugify,
  wraptext,
  truncate,
  striptags,

  // Utility filters
  ordinal,
  pluralize,

  // Constants and utilities
  LATEX_ESCAPE_MAP,
  ALL_FILTERS,
  FILTER_METADATA,
  registerAllFilters,
  FilterRegistrationSchema,
} from './nunjucks-filters.mjs';

// Legacy aliases for backward compatibility
export { texescape as texEscape } from './nunjucks-filters.mjs';
export { bibtexkey as toBibtexKey } from './nunjucks-filters.mjs';
export { wraptext as wrapText } from './nunjucks-filters.mjs';

// =============================================================================
// File I/O Exports
// =============================================================================

export {
  // Core file operations
  writePaper,
  readPaper,
  deletePaper,
  copyPaper,
  movePaper,

  // Directory operations
  ensureOutputDir,
  listPapers,
  cleanOutputDir,
  getOutputStats,

  // Utilities
  paperExists,
  getPaperInfo,
  formatFileSize,

  // Constants
  DEFAULT_OUTPUT_DIR,
  SUPPORTED_EXTENSIONS,

  // Schemas
  FileInfoSchema,
  WriteOptionsSchema,
} from './file-io.mjs';

// =============================================================================
// Knowledge Graph Exports
// =============================================================================

export {
  createKnowledgeGraph,
  knowledgeGraph,
  NAMED_QUERIES,
  PREFIXES,
  createPrefixDeclarations,
} from './knowledge-graph.mjs';

// =============================================================================
// SPARQL Layer Exports
// =============================================================================

export {
  // Initialization
  initKnowledgeGraph,
  loadOntology,
  shutdown as shutdownKnowledgeGraph,
  isInitialized as isSparqlInitialized,

  // Query Execution
  executeSparqlSelect,
  executeSparqlAsk,
  executeSparqlConstruct,
  executeNamedQuery,

  // Data Operations
  insertTriples,
  exportAsTurtle,

  // Introspection
  getOntologyClasses,
  getOntologyProperties,
  listNamedQueries,

  // Metrics and Stats
  getQueryMetrics,
  resetQueryMetrics,
  getGraphStats,
  getLoadedOntologies,

  // Cache
  clearQueryCache,
  getCacheStats,

  // Constants
  ONTOLOGY_PATHS,
  NAMED_QUERIES as SPARQL_NAMED_QUERIES,

  // Error class
  SparqlQueryError,
} from './sparql.mjs';

// =============================================================================
// Configuration
// =============================================================================

/**
 * Integration configuration defaults
 * @type {Object}
 */
export const config = {
  templates: {
    dir: '/Users/sac/unrdf/playground/papers-thesis-cli/templates',
    extension: '.tex.njk',
    families: ['imrad', 'dsr', 'argument', 'contribution', 'monograph', 'narrative'],
  },
  output: {
    dir: '/Users/sac/unrdf/playground/papers-thesis-cli/output',
    extensions: ['.tex', '.bib', '.cls', '.sty', '.txt'],
  },
  knowledgeGraph: {
    ontologyPath: '/Users/sac/unrdf/playground/papers-thesis-cli/ontologies/papers-thesis.ttl',
    prefixes: {
      pt: 'http://papers-thesis.org/ontology#',
      ex: 'http://papers-thesis.org/examples#',
    },
  },
};

// =============================================================================
// Integration Factory
// =============================================================================

/**
 * Create a complete integration setup
 *
 * @param {Object} [options] - Configuration options
 * @param {string} [options.templatesDir] - Custom templates directory
 * @param {string} [options.outputDir] - Custom output directory
 * @param {string} [options.ontologyPath] - Custom ontology path
 * @returns {Object} Complete integration interface
 *
 * @example
 * const integration = createIntegration({
 *   templatesDir: './my-templates',
 *   outputDir: './my-output'
 * });
 *
 * const latex = await integration.templates.render('imrad', context);
 * await integration.files.writePaper('./output/paper.tex', latex);
 */
export function createIntegration(options = {}) {
  const templatesDir = options.templatesDir || config.templates.dir;
  const outputDir = options.outputDir || config.output.dir;
  const ontologyPath = options.ontologyPath || config.knowledgeGraph.ontologyPath;

  return {
    /**
     * Template engine interface
     */
    templates: createTemplateEngineImpl(templatesDir),

    /**
     * Knowledge graph interface
     */
    graph: createKnowledgeGraphImpl({ ontologyPath }),

    /**
     * File I/O operations
     */
    files: {
      write: (path, content, opts) => fileIO.writePaper(path, content, opts),
      read: path => fileIO.readPaper(path),
      delete: path => fileIO.deletePaper(path),
      list: (dir = outputDir, opts) => fileIO.listPapers(dir, opts),
      exists: path => fileIO.paperExists(path),
      info: path => fileIO.getPaperInfo(path),
      ensureDir: dir => fileIO.ensureOutputDir(dir),
      stats: (dir = outputDir) => fileIO.getOutputStats(dir),
    },

    /**
     * Configuration
     */
    config: {
      templatesDir,
      outputDir,
      ontologyPath,
    },
  };
}
