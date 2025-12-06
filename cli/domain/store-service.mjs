/**
 * @file Store Domain Service - Business logic layer for RDF store operations
 * @module cli/domain/store-service
 *
 * ARCHITECTURE:
 * - CLI Commands (presentation) → Domain Services (business logic) → Packages (data access)
 * - This service encapsulates ALL business logic for store operations
 * - Commands should be thin wrappers that validate args and call this service
 * - Packages (@unrdf/core, @unrdf/oxigraph) should only do data access
 */

import { getStore } from '../utils/store-instance.mjs';
import { z } from 'zod';

/* ========================================================================= */
/* Schemas - Domain-level validation                                        */
/* ========================================================================= */

const ExecuteQueryOptionsSchema = z.object({
  query: z.string().min(1, 'Query cannot be empty'),
  timeout: z.number().int().positive().optional().default(10000),
  format: z.enum(['json', 'table', 'csv', 'turtle']).optional().default('json'),
});

const ImportDataOptionsSchema = z.object({
  content: z.string().min(1, 'Content cannot be empty'),
  format: z.enum(['turtle', 'ntriples', 'nquads', 'jsonld', 'rdfxml']),
  graph: z.string().optional(),
  baseIRI: z.string().url().optional(),
});

const ExportDataOptionsSchema = z.object({
  format: z.enum(['turtle', 'ntriples', 'nquads', 'jsonld', 'rdfxml']),
  graph: z.string().optional(),
  includeMetadata: z.boolean().optional().default(false),
});

const UpdateDataOptionsSchema = z.object({
  updateQuery: z.string().min(1, 'Update query cannot be empty'),
  validateOnly: z.boolean().optional().default(false),
});

const StoreStatsOptionsSchema = z.object({
  includeGraphs: z.boolean().optional().default(true),
  includeTriples: z.boolean().optional().default(true),
});

/* ========================================================================= */
/* Domain Service Class                                                     */
/* ========================================================================= */

/**
 * StoreService - Domain logic for RDF store operations
 *
 * Responsibilities:
 * - Validate business rules
 * - Orchestrate package operations
 * - Transform data for presentation layer
 * - Handle errors with domain-specific messages
 *
 * Does NOT:
 * - Parse CLI arguments (that's the command's job)
 * - Format output for display (that's the command's job)
 * - Directly manipulate store (delegates to packages)
 */
export class StoreService {
  #store;

  constructor() {
    this.#store = getStore();
  }

  /**
   * Execute SPARQL query
   *
   * @param {Object} options - Query options
   * @param {string} options.query - SPARQL query string
   * @param {number} [options.timeout] - Query timeout in ms (default: 10000)
   * @param {string} [options.format] - Output format (default: 'json')
   * @returns {Object} Query results with metadata
   *
   * @example
   * const service = new StoreService();
   * const result = await service.executeQuery({
   *   query: 'SELECT * WHERE { ?s ?p ?o }',
   *   timeout: 5000,
   *   format: 'json'
   * });
   * // result = { data: [...], metadata: { rowCount, executionTime } }
   */
  async executeQuery(options) {
    // Validate at domain boundary
    const validated = ExecuteQueryOptionsSchema.parse(options);

    const startTime = Date.now();

    try {
      // Delegate to package layer
      const results = this.#store.query(validated.query, {
        timeout: validated.timeout
      });

      const executionTime = Date.now() - startTime;

      // Transform for presentation layer
      return {
        data: results,
        metadata: {
          rowCount: Array.isArray(results) ? results.length : 0,
          executionTime,
          queryType: this.#detectQueryType(validated.query),
          format: validated.format
        }
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Query execution failed');
    }
  }

  /**
   * Import RDF data into store
   *
   * @param {Object} options - Import options
   * @param {string} options.content - RDF content to import
   * @param {string} options.format - RDF format (turtle, nquads, etc.)
   * @param {string} [options.graph] - Target graph name
   * @param {string} [options.baseIRI] - Base IRI for relative URIs
   * @returns {Object} Import result with statistics
   *
   * @example
   * const service = new StoreService();
   * const result = await service.importData({
   *   content: '<s> <p> <o> .',
   *   format: 'turtle',
   *   graph: 'http://example.org/graph1'
   * });
   * // result = { quadsAdded: 1, totalQuads: 1, graph: '...' }
   */
  async importData(options) {
    const validated = ImportDataOptionsSchema.parse(options);

    const beforeSize = this.#store.size();

    try {
      // Map format names to Oxigraph MIME types
      const formatMap = {
        'turtle': 'text/turtle',
        'ntriples': 'application/n-triples',
        'nquads': 'application/n-quads',
        'jsonld': 'application/ld+json',
        'rdfxml': 'application/rdf+xml'
      };

      const mimeType = formatMap[validated.format] || validated.format;

      // Delegate to package layer
      this.#store.load(validated.content, {
        format: mimeType,
        to_graph: validated.graph
          ? { value: validated.graph, termType: 'NamedNode' }
          : undefined,
        base_iri: validated.baseIRI
      });

      const afterSize = this.#store.size();

      return {
        quadsAdded: afterSize - beforeSize,
        totalQuads: afterSize,
        graph: validated.graph || 'default',
        format: validated.format
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Import failed');
    }
  }

  /**
   * Export RDF data from store
   *
   * @param {Object} options - Export options
   * @param {string} options.format - Output format
   * @param {string} [options.graph] - Source graph (default: all graphs)
   * @param {boolean} [options.includeMetadata] - Include metadata quads
   * @returns {Object} Export result with serialized data
   *
   * @example
   * const service = new StoreService();
   * const result = await service.exportData({
   *   format: 'turtle',
   *   graph: 'http://example.org/graph1'
   * });
   * // result = { content: '<s> <p> <o> .', format: 'turtle', quadCount: 1 }
   */
  async exportData(options) {
    const validated = ExportDataOptionsSchema.parse(options);

    try {
      // Map format to MIME type
      const formatMap = {
        'turtle': 'text/turtle',
        'ntriples': 'application/n-triples',
        'nquads': 'application/n-quads',
        'jsonld': 'application/ld+json',
        'rdfxml': 'application/rdf+xml'
      };

      const mimeType = formatMap[validated.format] || validated.format;

      // Delegate to package layer
      const content = this.#store.dump({
        format: mimeType,
        from_graph: validated.graph
          ? { value: validated.graph, termType: 'NamedNode' }
          : undefined
      });

      // Count quads
      const quadCount = this.#store.size();

      return {
        content,
        format: validated.format,
        quadCount,
        graph: validated.graph || 'all'
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Export failed');
    }
  }

  /**
   * Execute SPARQL UPDATE query
   *
   * @param {Object} options - Update options
   * @param {string} options.updateQuery - SPARQL UPDATE query
   * @param {boolean} [options.validateOnly] - Only validate, don't execute
   * @returns {Object} Update result with statistics
   *
   * @example
   * const service = new StoreService();
   * const result = await service.updateData({
   *   updateQuery: 'DELETE WHERE { ?s ?p ?o }',
   *   validateOnly: false
   * });
   * // result = { success: true, quadsAffected: 10 }
   */
  async updateData(options) {
    const validated = UpdateDataOptionsSchema.parse(options);

    if (validated.validateOnly) {
      // TODO: Add SPARQL UPDATE syntax validation
      return {
        valid: true,
        query: validated.updateQuery
      };
    }

    const beforeSize = this.#store.size();

    try {
      // Delegate to package layer
      this.#store.update(validated.updateQuery);

      const afterSize = this.#store.size();

      return {
        success: true,
        quadsAffected: Math.abs(afterSize - beforeSize),
        totalQuads: afterSize
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Update failed');
    }
  }

  /**
   * Get store statistics
   *
   * @param {Object} [options] - Statistics options
   * @param {boolean} [options.includeGraphs] - Include graph list
   * @param {boolean} [options.includeTriples] - Include triple count
   * @returns {Object} Store statistics
   *
   * @example
   * const service = new StoreService();
   * const stats = await service.getStats({ includeGraphs: true });
   * // stats = { totalQuads: 100, graphs: [...], tripleCount: 95 }
   */
  async getStats(options = {}) {
    const validated = StoreStatsOptionsSchema.parse(options);

    try {
      const stats = {
        totalQuads: this.#store.size()
      };

      if (validated.includeGraphs) {
        // Query for all distinct graphs
        const graphQuery = `SELECT DISTINCT ?g WHERE { GRAPH ?g { ?s ?p ?o } }`;
        const graphResults = this.#store.query(graphQuery);
        stats.graphs = graphResults.map(row => row.g.value);
        stats.graphCount = stats.graphs.length;
      }

      if (validated.includeTriples) {
        // Default graph quads
        const defaultQuery = `SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }`;
        const defaultResults = this.#store.query(defaultQuery);
        stats.defaultGraphQuads = defaultResults[0]?.count?.value || 0;
      }

      return stats;
    } catch (error) {
      throw this.#enhanceError(error, 'Failed to get statistics');
    }
  }

  /**
   * Clear all data from store (or specific graph)
   *
   * @param {Object} [options] - Clear options
   * @param {string} [options.graph] - Graph to clear (default: all)
   * @returns {Object} Clear result
   *
   * @example
   * const service = new StoreService();
   * const result = await service.clear({ graph: 'http://example.org/graph1' });
   * // result = { success: true, quadsRemoved: 10 }
   */
  async clear(options = {}) {
    const beforeSize = this.#store.size();

    try {
      if (options.graph) {
        this.#store.update(`CLEAR GRAPH <${options.graph}>`);
      } else {
        this.#store.update('CLEAR ALL');
      }

      const afterSize = this.#store.size();

      return {
        success: true,
        quadsRemoved: beforeSize - afterSize,
        graph: options.graph || 'all'
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Clear failed');
    }
  }

  /* ========================================================================= */
  /* Private Helper Methods                                                    */
  /* ========================================================================= */

  /**
   * Detect SPARQL query type (SELECT, ASK, CONSTRUCT, DESCRIBE)
   * @private
   */
  #detectQueryType(query) {
    const upperQuery = query.trim().toUpperCase();
    if (upperQuery.startsWith('SELECT')) return 'SELECT';
    if (upperQuery.startsWith('ASK')) return 'ASK';
    if (upperQuery.startsWith('CONSTRUCT')) return 'CONSTRUCT';
    if (upperQuery.startsWith('DESCRIBE')) return 'DESCRIBE';
    return 'UNKNOWN';
  }

  /**
   * Enhance error with domain context
   * @private
   */
  #enhanceError(error, context) {
    const enhanced = new Error(`${context}: ${error.message}`);
    enhanced.cause = error;
    enhanced.domain = 'StoreService';
    return enhanced;
  }
}

/* ========================================================================= */
/* Singleton Instance                                                        */
/* ========================================================================= */

let storeServiceInstance = null;

/**
 * Get singleton StoreService instance
 *
 * @returns {StoreService}
 *
 * @example
 * import { getStoreService } from './domain/store-service.mjs';
 * const service = getStoreService();
 * const result = await service.executeQuery({ query: '...' });
 */
export function getStoreService() {
  if (!storeServiceInstance) {
    storeServiceInstance = new StoreService();
  }
  return storeServiceInstance;
}
