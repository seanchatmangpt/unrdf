/**
 * @file Graph Domain Service - Business logic layer for RDF graph operations
 * @module cli/domain/graph-service
 *
 * ARCHITECTURE:
 * - CLI Commands (presentation) → Domain Services (business logic) → Packages (data access)
 * - This service encapsulates ALL business logic for graph-specific operations
 * - Commands should be thin wrappers that validate args and call this service
 * - Packages (@unrdf/core, @unrdf/oxigraph) should only do data access
 */

import { getStore } from '../utils/store-instance.mjs';
import { z } from 'zod';

/* ========================================================================= */
/* Schemas - Domain-level validation                                        */
/* ========================================================================= */

const ListGraphsOptionsSchema = z.object({
  includeStats: z.boolean().optional().default(false),
  sortBy: z.enum(['name', 'size', 'created']).optional().default('name'),
});

const CreateGraphOptionsSchema = z.object({
  name: z.string().min(1, 'Graph name is required'),
  metadata: z.record(z.any()).optional(),
});

const DeleteGraphOptionsSchema = z.object({
  name: z.string().min(1, 'Graph name is required'),
  force: z.boolean().optional().default(false),
});

const CopyGraphOptionsSchema = z.object({
  source: z.string().min(1, 'Source graph is required'),
  target: z.string().min(1, 'Target graph is required'),
  overwrite: z.boolean().optional().default(false),
});

const MergeGraphsOptionsSchema = z.object({
  sources: z.array(z.string()).min(1, 'At least one source graph is required'),
  target: z.string().min(1, 'Target graph is required'),
  strategy: z.enum(['union', 'intersection']).optional().default('union'),
});

/* ========================================================================= */
/* Domain Service Class                                                     */
/* ========================================================================= */

/**
 * GraphService - Domain logic for RDF graph operations
 *
 * Responsibilities:
 * - Validate business rules for graph operations
 * - Orchestrate graph-level operations
 * - Transform graph data for presentation layer
 * - Handle errors with domain-specific messages
 *
 * Does NOT:
 * - Parse CLI arguments (that's the command's job)
 * - Format output for display (that's the command's job)
 * - Directly manipulate store (delegates to packages)
 */
export class GraphService {
  #store;

  constructor() {
    this.#store = getStore();
  }

  /**
   * List all graphs in store
   *
   * @param {Object} [options] - List options
   * @param {boolean} [options.includeStats] - Include statistics for each graph
   * @param {string} [options.sortBy] - Sort by 'name', 'size', or 'created'
   * @returns {Object} Graph list with metadata
   *
   * @example
   * const service = new GraphService();
   * const result = await service.listGraphs({ includeStats: true });
   * // result = { graphs: [...], metadata: { totalGraphs, totalQuads } }
   */
  async listGraphs(options = {}) {
    const validated = ListGraphsOptionsSchema.parse(options);

    try {
      // Query for all distinct graphs
      const query = `SELECT DISTINCT ?g WHERE { GRAPH ?g { ?s ?p ?o } }`;
      const results = this.#store.query(query);

      let graphs = results.map(row => ({
        name: row.g.value,
        type: row.g.termType
      }));

      // Add default graph if it has data
      const defaultQuery = `SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }`;
      const defaultResults = this.#store.query(defaultQuery);
      const defaultCount = parseInt(defaultResults[0]?.count?.value || '0', 10);

      if (defaultCount > 0) {
        graphs.unshift({
          name: 'default',
          type: 'DefaultGraph'
        });
      }

      // Add statistics if requested
      if (validated.includeStats) {
        graphs = await Promise.all(graphs.map(async (graph) => {
          const stats = await this.getGraphStats(graph.name);
          return {
            ...graph,
            stats
          };
        }));

        // Sort by size if requested
        if (validated.sortBy === 'size') {
          graphs.sort((a, b) => (b.stats?.quadCount || 0) - (a.stats?.quadCount || 0));
        }
      }

      // Sort by name (default)
      if (validated.sortBy === 'name') {
        graphs.sort((a, b) => a.name.localeCompare(b.name));
      }

      return {
        graphs,
        metadata: {
          totalGraphs: graphs.length,
          totalQuads: this.#store.size(),
          includeStats: validated.includeStats
        }
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Failed to list graphs');
    }
  }

  /**
   * Get statistics for a specific graph
   *
   * @param {string} graphName - Graph name
   * @returns {Object} Graph statistics
   *
   * @example
   * const service = new GraphService();
   * const stats = await service.getGraphStats('http://example.org/graph1');
   * // stats = { quadCount: 100, subjects: 10, predicates: 5, objects: 50 }
   */
  async getGraphStats(graphName) {
    try {
      let countQuery;
      if (graphName === 'default') {
        countQuery = `SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }`;
      } else {
        countQuery = `SELECT (COUNT(*) as ?count) WHERE { GRAPH <${graphName}> { ?s ?p ?o } }`;
      }

      const countResults = this.#store.query(countQuery);
      const quadCount = parseInt(countResults[0]?.count?.value || '0', 10);

      // Count distinct subjects
      let subjectQuery;
      if (graphName === 'default') {
        subjectQuery = `SELECT (COUNT(DISTINCT ?s) as ?count) WHERE { ?s ?p ?o }`;
      } else {
        subjectQuery = `SELECT (COUNT(DISTINCT ?s) as ?count) WHERE { GRAPH <${graphName}> { ?s ?p ?o } }`;
      }

      const subjectResults = this.#store.query(subjectQuery);
      const subjectCount = parseInt(subjectResults[0]?.count?.value || '0', 10);

      return {
        graphName,
        quadCount,
        subjectCount,
        predicateCount: null, // TODO: Add if needed
        objectCount: null     // TODO: Add if needed
      };
    } catch (error) {
      throw this.#enhanceError(error, `Failed to get stats for graph: ${graphName}`);
    }
  }

  /**
   * Create a new named graph
   *
   * @param {Object} options - Create options
   * @param {string} options.name - Graph name/URI
   * @param {Object} [options.metadata] - Graph metadata
   * @returns {Object} Creation result
   *
   * @example
   * const service = new GraphService();
   * const result = await service.createGraph({
   *   name: 'http://example.org/graph1',
   *   metadata: { created: new Date().toISOString() }
   * });
   * // result = { success: true, graph: '...' }
   */
  async createGraph(options) {
    const validated = CreateGraphOptionsSchema.parse(options);

    try {
      // Check if graph already exists
      const exists = await this.graphExists(validated.name);
      if (exists) {
        throw new Error(`Graph already exists: ${validated.name}`);
      }

      // Create graph by adding metadata triple
      // (in RDF, graphs are created implicitly when quads are added to them)
      // We'll add a minimal metadata triple to establish the graph
      if (validated.metadata) {
        const metadataTriples = Object.entries(validated.metadata)
          .map(([key, value]) => {
            const valueStr = typeof value === 'string' ? `"${value}"` : value;
            return `<${validated.name}> <http://purl.org/dc/terms/${key}> ${valueStr} .`;
          })
          .join('\n');

        this.#store.load(metadataTriples, {
          format: 'text/turtle',
          to_graph: { value: validated.name, termType: 'NamedNode' }
        });
      } else {
        // Add minimal triple to create graph
        this.#store.load(
          `<${validated.name}> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2004/03/tav#Graph> .`,
          {
            format: 'text/turtle',
            to_graph: { value: validated.name, termType: 'NamedNode' }
          }
        );
      }

      return {
        success: true,
        graph: validated.name,
        created: true
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Failed to create graph');
    }
  }

  /**
   * Delete a named graph
   *
   * @param {Object} options - Delete options
   * @param {string} options.name - Graph name to delete
   * @param {boolean} [options.force] - Force delete without confirmation
   * @returns {Object} Deletion result
   *
   * @example
   * const service = new GraphService();
   * const result = await service.deleteGraph({
   *   name: 'http://example.org/graph1',
   *   force: true
   * });
   * // result = { success: true, removed: 100 }
   */
  async deleteGraph(options) {
    const validated = DeleteGraphOptionsSchema.parse(options);

    try {
      // Get quad count before deletion
      const stats = await this.getGraphStats(validated.name);

      // Delete graph using SPARQL UPDATE
      if (validated.name === 'default') {
        this.#store.update('CLEAR DEFAULT');
      } else {
        this.#store.update(`CLEAR GRAPH <${validated.name}>`);
      }

      return {
        success: true,
        graph: validated.name,
        quadsRemoved: stats.quadCount
      };
    } catch (error) {
      throw this.#enhanceError(error, `Failed to delete graph: ${validated.name}`);
    }
  }

  /**
   * Copy graph from source to target
   *
   * @param {Object} options - Copy options
   * @param {string} options.source - Source graph name
   * @param {string} options.target - Target graph name
   * @param {boolean} [options.overwrite] - Overwrite target if exists
   * @returns {Object} Copy result
   *
   * @example
   * const service = new GraphService();
   * const result = await service.copyGraph({
   *   source: 'http://example.org/graph1',
   *   target: 'http://example.org/graph2',
   *   overwrite: false
   * });
   * // result = { success: true, quadsCopied: 100 }
   */
  async copyGraph(options) {
    const validated = CopyGraphsOptionsSchema.parse(options);

    try {
      // Check if target exists
      const targetExists = await this.graphExists(validated.target);
      if (targetExists && !validated.overwrite) {
        throw new Error(`Target graph already exists: ${validated.target}. Use --overwrite to replace.`);
      }

      // Clear target if overwriting
      if (targetExists && validated.overwrite) {
        await this.deleteGraph({ name: validated.target, force: true });
      }

      // Copy using SPARQL UPDATE
      let copyQuery;
      if (validated.source === 'default') {
        copyQuery = `INSERT { GRAPH <${validated.target}> { ?s ?p ?o } } WHERE { ?s ?p ?o }`;
      } else {
        copyQuery = `INSERT { GRAPH <${validated.target}> { ?s ?p ?o } } WHERE { GRAPH <${validated.source}> { ?s ?p ?o } }`;
      }

      this.#store.update(copyQuery);

      // Get copied quad count
      const stats = await this.getGraphStats(validated.target);

      return {
        success: true,
        source: validated.source,
        target: validated.target,
        quadsCopied: stats.quadCount
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Failed to copy graph');
    }
  }

  /**
   * Merge multiple graphs into target
   *
   * @param {Object} options - Merge options
   * @param {string[]} options.sources - Source graph names
   * @param {string} options.target - Target graph name
   * @param {string} [options.strategy] - 'union' or 'intersection'
   * @returns {Object} Merge result
   *
   * @example
   * const service = new GraphService();
   * const result = await service.mergeGraphs({
   *   sources: ['http://example.org/graph1', 'http://example.org/graph2'],
   *   target: 'http://example.org/merged',
   *   strategy: 'union'
   * });
   * // result = { success: true, quadsMerged: 200 }
   */
  async mergeGraphs(options) {
    const validated = MergeGraphsOptionsSchema.parse(options);

    try {
      if (validated.strategy === 'union') {
        // Union: copy all quads from all sources to target
        for (const source of validated.sources) {
          await this.copyGraph({
            source,
            target: validated.target,
            overwrite: false // Append to existing
          });
        }
      } else {
        // Intersection: only quads that exist in ALL sources
        throw new Error('Intersection merge strategy not yet implemented');
      }

      const stats = await this.getGraphStats(validated.target);

      return {
        success: true,
        sources: validated.sources,
        target: validated.target,
        strategy: validated.strategy,
        quadsMerged: stats.quadCount
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Failed to merge graphs');
    }
  }

  /**
   * Check if graph exists
   *
   * @param {string} graphName - Graph name to check
   * @returns {Promise<boolean>} True if graph exists
   *
   * @example
   * const service = new GraphService();
   * const exists = await service.graphExists('http://example.org/graph1');
   * // exists = true
   */
  async graphExists(graphName) {
    try {
      if (graphName === 'default') {
        const query = `ASK WHERE { ?s ?p ?o }`;
        return this.#store.query(query);
      }

      const query = `ASK WHERE { GRAPH <${graphName}> { ?s ?p ?o } }`;
      return this.#store.query(query);
    } catch (error) {
      throw this.#enhanceError(error, `Failed to check if graph exists: ${graphName}`);
    }
  }

  /* ========================================================================= */
  /* Private Helper Methods                                                    */
  /* ========================================================================= */

  /**
   * Enhance error with domain context
   * @private
   */
  #enhanceError(error, context) {
    const enhanced = new Error(`${context}: ${error.message}`);
    enhanced.cause = error;
    enhanced.domain = 'GraphService';
    return enhanced;
  }
}

/* ========================================================================= */
/* Singleton Instance                                                        */
/* ========================================================================= */

let graphServiceInstance = null;

/**
 * Get singleton GraphService instance
 *
 * @returns {GraphService}
 *
 * @example
 * import { getGraphService } from './domain/graph-service.mjs';
 * const service = getGraphService();
 * const result = await service.listGraphs({ includeStats: true });
 */
export function getGraphService() {
  if (!graphServiceInstance) {
    graphServiceInstance = new GraphService();
  }
  return graphServiceInstance;
}
