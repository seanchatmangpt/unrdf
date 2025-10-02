/**
 * @file Query Optimizer for performance improvements
 * @module query-optimizer
 * 
 * @description
 * Implements query plan caching, indexing, and delta-aware evaluation
 * to optimize SPARQL and SHACL query performance.
 */

import { Store } from 'n3';
import { sha3_256 } from '@noble/hashes/sha3.js';
import { utf8ToBytes, bytesToHex } from '@noble/hashes/utils.js';
import { randomUUID } from 'crypto';
import { z } from 'zod';

/**
 * Schema for query plan
 */
const QueryPlanSchema = z.object({
  id: z.string(),
  query: z.string(),
  type: z.enum(['sparql-ask', 'sparql-select', 'shacl']),
  hash: z.string(),
  plan: z.object({
    operations: z.array(z.object({
      type: z.string(),
      cost: z.number(),
      selectivity: z.number(),
      dependencies: z.array(z.string()).optional()
    })),
    estimatedCost: z.number(),
    estimatedRows: z.number(),
    indexes: z.array(z.string()).optional()
  }),
  createdAt: z.number(),
  lastUsed: z.number(),
  hitCount: z.number().default(0)
});

/**
 * Schema for index definition
 */
const IndexSchema = z.object({
  id: z.string(),
  name: z.string(),
  type: z.enum(['predicate', 'subject', 'object', 'graph', 'composite']),
  fields: z.array(z.string()),
  selectivity: z.number().min(0).max(1),
  size: z.number().nonnegative(),
  createdAt: z.number(),
  lastUpdated: z.number()
});

/**
 * Schema for delta-aware evaluation context
 */
const DeltaAwareContextSchema = z.object({
  delta: z.object({
    additions: z.array(z.any()),
    removals: z.array(z.any())
  }),
  affectedSubjects: z.set(z.string()).optional(),
  affectedPredicates: z.set(z.string()).optional(),
  affectedObjects: z.set(z.string()).optional(),
  affectedGraphs: z.set(z.string()).optional()
});

/**
 * Query Optimizer for performance improvements
 */
export class QueryOptimizer {
  /**
   * Create a new query optimizer
   * @param {Object} [config] - Configuration
   */
  constructor(config = {}) {
    this.config = {
      enableCaching: config.enableCaching !== false,
      enableIndexing: config.enableIndexing !== false,
      enableDeltaAware: config.enableDeltaAware !== false,
      maxCacheSize: config.maxCacheSize || 1000,
      cacheMaxAge: config.cacheMaxAge || 300000, // 5 minutes
      indexUpdateThreshold: config.indexUpdateThreshold || 100,
      ...config
    };
    
    this.queryPlans = new Map();
    this.indexes = new Map();
    this.cache = new Map();
    this.stats = {
      cacheHits: 0,
      cacheMisses: 0,
      indexHits: 0,
      indexMisses: 0,
      deltaOptimizations: 0,
      totalQueries: 0
    };
  }

  /**
   * Optimize a query
   * @param {string} query - Query string
   * @param {string} type - Query type
   * @param {Store} graph - RDF graph
   * @param {Object} [delta] - Delta for delta-aware optimization
   * @returns {Promise<Object>} Optimized query plan
   */
  async optimizeQuery(query, type, graph, delta = null) {
    this.stats.totalQueries++;
    
    // Generate query hash
    const queryHash = this._hashQuery(query, type);
    
    // Check cache first
    if (this.config.enableCaching) {
      const cached = this._getCachedPlan(queryHash);
      if (cached) {
        this.stats.cacheHits++;
        cached.lastUsed = Date.now();
        cached.hitCount++;
        return cached;
      }
      this.stats.cacheMisses++;
    }
    
    // Create new query plan
    const plan = await this._createQueryPlan(query, type, queryHash, graph, delta);
    
    // Cache the plan
    if (this.config.enableCaching) {
      this._cachePlan(plan);
    }
    
    return plan;
  }

  /**
   * Execute optimized query
   * @param {Object} plan - Query plan
   * @param {Store} graph - RDF graph
   * @param {Object} [delta] - Delta for delta-aware execution
   * @returns {Promise<any>} Query result
   */
  async executeOptimizedQuery(plan, graph, delta = null) {
    if (this.config.enableDeltaAware && delta) {
      return this._executeDeltaAware(plan, graph, delta);
    }
    
    return this._executeStandard(plan, graph);
  }

  /**
   * Create indexes for a graph
   * @param {Store} graph - RDF graph
   * @returns {Promise<Array>} Created indexes
   */
  async createIndexes(graph) {
    if (!this.config.enableIndexing) {
      return [];
    }
    
    const indexes = [];
    const quads = graph.getQuads();
    
    // Create predicate index
    const predicateIndex = this._createPredicateIndex(quads);
    indexes.push(predicateIndex);
    
    // Create subject index
    const subjectIndex = this._createSubjectIndex(quads);
    indexes.push(subjectIndex);
    
    // Create object index
    const objectIndex = this._createObjectIndex(quads);
    indexes.push(objectIndex);
    
    // Create composite indexes for common patterns
    const compositeIndexes = this._createCompositeIndexes(quads);
    indexes.push(...compositeIndexes);
    
    // Store indexes
    for (const index of indexes) {
      this.indexes.set(index.id, index);
    }
    
    return indexes;
  }

  /**
   * Update indexes with delta
   * @param {Object} delta - Delta to apply
   * @returns {Promise<void>}
   */
  async updateIndexes(delta) {
    if (!this.config.enableIndexing) {
      return;
    }
    
    // Update indexes with additions and removals
    for (const [indexId, index] of this.indexes) {
      // Remove quads
      for (const quad of delta.removals) {
        this._removeFromIndex(index, quad);
      }
      
      // Add quads
      for (const quad of delta.additions) {
        this._addToIndex(index, quad);
      }
      
      index.lastUpdated = Date.now();
    }
  }

  /**
   * Get optimizer statistics
   * @returns {Object} Statistics
   */
  getStats() {
    const cacheHitRate = this.stats.totalQueries > 0 
      ? this.stats.cacheHits / this.stats.totalQueries 
      : 0;
    
    const indexHitRate = this.stats.totalQueries > 0 
      ? this.stats.indexHits / this.stats.totalQueries 
      : 0;
    
    return {
      config: this.config,
      cache: {
        size: this.cache.size,
        maxSize: this.config.maxCacheSize,
        hitRate: cacheHitRate,
        hits: this.stats.cacheHits,
        misses: this.stats.cacheMisses
      },
      indexes: {
        count: this.indexes.size,
        hitRate: indexHitRate,
        hits: this.stats.indexHits,
        misses: this.stats.indexMisses
      },
      optimization: {
        deltaOptimizations: this.stats.deltaOptimizations,
        totalQueries: this.stats.totalQueries
      }
    };
  }

  /**
   * Clear all caches and indexes
   */
  clear() {
    // Clear query plans with deep cleanup
    for (const plan of this.queryPlans.values()) {
      if (plan.plan && plan.plan.operations) {
        plan.plan.operations.length = 0;
      }
    }
    this.queryPlans.clear();

    // Clear indexes with deep cleanup
    for (const index of this.indexes.values()) {
      if (index.data && typeof index.data.clear === 'function') {
        index.data.clear();
      }
    }
    this.indexes.clear();

    // Clear cache
    this.cache.clear();

    // Reset stats
    this.stats = {
      cacheHits: 0,
      cacheMisses: 0,
      indexHits: 0,
      indexMisses: 0,
      deltaOptimizations: 0,
      totalQueries: 0
    };
  }

  /**
   * Cleanup query optimizer resources
   */
  async cleanup() {
    this.clear();
  }

  /**
   * Hash a query
   * @param {string} query - Query string
   * @param {string} type - Query type
   * @returns {string} Query hash
   * @private
   */
  _hashQuery(query, type) {
    const content = `${type}:${query}`;
    return bytesToHex(sha3_256(utf8ToBytes(content)));
  }

  /**
   * Get cached query plan
   * @param {string} queryHash - Query hash
   * @returns {Object|null} Cached plan or null
   * @private
   */
  _getCachedPlan(queryHash) {
    const cached = this.cache.get(queryHash);
    if (!cached) return null;
    
    // Check if expired
    if (Date.now() - cached.createdAt > this.config.cacheMaxAge) {
      this.cache.delete(queryHash);
      return null;
    }
    
    return cached;
  }

  /**
   * Cache a query plan
   * @param {Object} plan - Query plan
   * @private
   */
  _cachePlan(plan) {
    // Remove oldest if cache is full
    if (this.cache.size >= this.config.maxCacheSize) {
      const oldest = Array.from(this.cache.values())
        .sort((a, b) => a.lastUsed - b.lastUsed)[0];
      this.cache.delete(oldest.hash);
    }
    
    this.cache.set(plan.hash, plan);
  }

  /**
   * Create a query plan
   * @param {string} query - Query string
   * @param {string} type - Query type
   * @param {string} queryHash - Query hash
   * @param {Store} graph - RDF graph
   * @param {Object} [delta] - Delta for optimization
   * @returns {Promise<Object>} Query plan
   * @private
   */
  async _createQueryPlan(query, type, queryHash, graph, delta) {
    const plan = {
      id: randomUUID(),
      query,
      type,
      hash: queryHash,
      plan: {
        operations: [],
        estimatedCost: 0,
        estimatedRows: 0,
        indexes: []
      },
      createdAt: Date.now(),
      lastUsed: Date.now(),
      hitCount: 0
    };
    
    // Analyze query and create execution plan
    switch (type) {
      case 'sparql-ask':
        plan.plan = await this._analyzeSparqlAsk(query, graph, delta);
        break;
      case 'sparql-select':
        plan.plan = await this._analyzeSparqlSelect(query, graph, delta);
        break;
      case 'shacl':
        plan.plan = await this._analyzeShacl(query, graph, delta);
        break;
    }
    
    return plan;
  }

  /**
   * Analyze SPARQL ASK query
   * @param {string} query - Query string
   * @param {Store} graph - RDF graph
   * @param {Object} [delta] - Delta for optimization
   * @returns {Promise<Object>} Execution plan
   * @private
   */
  async _analyzeSparqlAsk(query, graph, delta) {
    const plan = {
      operations: [],
      estimatedCost: 0,
      estimatedRows: 0,
      indexes: []
    };
    
    // Simple analysis - in production this would be more sophisticated
    const operations = this._parseSparqlOperations(query);
    
    for (const op of operations) {
      const cost = this._estimateOperationCost(op, graph);
      const selectivity = this._estimateSelectivity(op, graph);
      
      plan.operations.push({
        type: op.type,
        cost,
        selectivity,
        dependencies: op.dependencies
      });
      
      plan.estimatedCost += cost;
    }
    
    // Add delta-aware optimizations
    if (delta) {
      plan.deltaAware = this._createDeltaAwarePlan(operations, delta);
      this.stats.deltaOptimizations++;
    }
    
    return plan;
  }

  /**
   * Analyze SPARQL SELECT query
   * @param {string} query - Query string
   * @param {Store} graph - RDF graph
   * @param {Object} [delta] - Delta for optimization
   * @returns {Promise<Object>} Execution plan
   * @private
   */
  async _analyzeSparqlSelect(query, graph, delta) {
    // Similar to ASK but with result estimation
    const plan = await this._analyzeSparqlAsk(query, graph, delta);
    plan.estimatedRows = this._estimateResultRows(query, graph);
    return plan;
  }

  /**
   * Analyze SHACL validation
   * @param {string} query - SHACL shapes
   * @param {Store} graph - RDF graph
   * @param {Object} [delta] - Delta for optimization
   * @returns {Promise<Object>} Execution plan
   * @private
   */
  async _analyzeShacl(query, graph, delta) {
    const plan = {
      operations: [],
      estimatedCost: 0,
      estimatedRows: 0,
      indexes: []
    };
    
    // SHACL-specific analysis
    const shapes = this._parseShaclShapes(query);
    
    for (const shape of shapes) {
      const cost = this._estimateShaclCost(shape, graph);
      plan.operations.push({
        type: 'shacl-validation',
        cost,
        selectivity: 0.1, // SHACL typically has low selectivity
        dependencies: []
      });
      
      plan.estimatedCost += cost;
    }
    
    return plan;
  }

  /**
   * Execute delta-aware query
   * @param {Object} plan - Query plan
   * @param {Store} graph - RDF graph
   * @param {Object} delta - Delta
   * @returns {Promise<any>} Query result
   * @private
   */
  async _executeDeltaAware(plan, graph, delta) {
    // Use delta information to optimize execution
    const affectedEntities = this._extractAffectedEntities(delta);
    
    // Check if query can be optimized based on delta
    if (plan.plan.deltaAware) {
      return this._executeOptimizedDeltaAware(plan, graph, delta, affectedEntities);
    }
    
    // Fall back to standard execution
    return this._executeStandard(plan, graph);
  }

  /**
   * Execute standard query
   * @param {Object} plan - Query plan
   * @param {Store} graph - RDF graph
   * @returns {Promise<any>} Query result
   * @private
   */
  async _executeStandard(plan, graph) {
    // Standard query execution
    // This would integrate with the actual query engine
    return { result: 'standard execution' };
  }

  /**
   * Create predicate index
   * @param {Array} quads - RDF quads
   * @returns {Object} Index
   * @private
   */
  _createPredicateIndex(quads) {
    const index = new Map();
    
    for (const quad of quads) {
      const predicate = quad.predicate.value;
      if (!index.has(predicate)) {
        index.set(predicate, []);
      }
      index.get(predicate).push(quad);
    }
    
    return {
      id: randomUUID(),
      name: 'predicate_index',
      type: 'predicate',
      fields: ['predicate'],
      selectivity: this._calculateSelectivity(index),
      size: index.size,
      createdAt: Date.now(),
      lastUpdated: Date.now(),
      data: index
    };
  }

  /**
   * Create subject index
   * @param {Array} quads - RDF quads
   * @returns {Object} Index
   * @private
   */
  _createSubjectIndex(quads) {
    const index = new Map();
    
    for (const quad of quads) {
      const subject = quad.subject.value;
      if (!index.has(subject)) {
        index.set(subject, []);
      }
      index.get(subject).push(quad);
    }
    
    return {
      id: randomUUID(),
      name: 'subject_index',
      type: 'subject',
      fields: ['subject'],
      selectivity: this._calculateSelectivity(index),
      size: index.size,
      createdAt: Date.now(),
      lastUpdated: Date.now(),
      data: index
    };
  }

  /**
   * Create object index
   * @param {Array} quads - RDF quads
   * @returns {Object} Index
   * @private
   */
  _createObjectIndex(quads) {
    const index = new Map();
    
    for (const quad of quads) {
      const object = quad.object.value;
      if (!index.has(object)) {
        index.set(object, []);
      }
      index.get(object).push(quad);
    }
    
    return {
      id: randomUUID(),
      name: 'object_index',
      type: 'object',
      fields: ['object'],
      selectivity: this._calculateSelectivity(index),
      size: index.size,
      createdAt: Date.now(),
      lastUpdated: Date.now(),
      data: index
    };
  }

  /**
   * Create composite indexes
   * @param {Array} quads - RDF quads
   * @returns {Array} Indexes
   * @private
   */
  _createCompositeIndexes(quads) {
    const indexes = [];
    
    // SPO index (most common pattern)
    const spoIndex = new Map();
    for (const quad of quads) {
      const key = `${quad.subject.value}:${quad.predicate.value}:${quad.object.value}`;
      if (!spoIndex.has(key)) {
        spoIndex.set(key, []);
      }
      spoIndex.get(key).push(quad);
    }
    
    indexes.push({
      id: randomUUID(),
      name: 'spo_index',
      type: 'composite',
      fields: ['subject', 'predicate', 'object'],
      selectivity: this._calculateSelectivity(spoIndex),
      size: spoIndex.size,
      createdAt: Date.now(),
      lastUpdated: Date.now(),
      data: spoIndex
    });
    
    return indexes;
  }

  /**
   * Calculate index selectivity
   * @param {Map} index - Index data
   * @returns {number} Selectivity
   * @private
   */
  _calculateSelectivity(index) {
    const totalEntries = Array.from(index.values()).reduce((sum, entries) => sum + entries.length, 0);
    const uniqueKeys = index.size;
    return uniqueKeys / totalEntries;
  }

  /**
   * Parse SPARQL operations
   * @param {string} query - SPARQL query
   * @returns {Array} Operations
   * @private
   */
  _parseSparqlOperations(query) {
    // Simple parsing - in production this would use a proper SPARQL parser
    const operations = [];
    
    if (query.includes('WHERE')) {
      operations.push({
        type: 'where-clause',
        cost: 100,
        dependencies: []
      });
    }
    
    if (query.includes('FILTER')) {
      operations.push({
        type: 'filter',
        cost: 50,
        dependencies: ['where-clause']
      });
    }
    
    return operations;
  }

  /**
   * Estimate operation cost
   * @param {Object} operation - Operation
   * @param {Store} graph - RDF graph
   * @returns {number} Estimated cost
   * @private
   */
  _estimateOperationCost(operation, graph) {
    const baseCost = operation.cost || 100;
    const graphSize = graph.size;
    return baseCost * Math.log(graphSize + 1);
  }

  /**
   * Estimate operation selectivity
   * @param {Object} operation - Operation
   * @param {Store} graph - RDF graph
   * @returns {number} Estimated selectivity
   * @private
   */
  _estimateSelectivity(operation, graph) {
    // Simple estimation - in production this would be more sophisticated
    switch (operation.type) {
      case 'where-clause':
        return 0.1;
      case 'filter':
        return 0.5;
      default:
        return 0.3;
    }
  }

  /**
   * Create delta-aware plan
   * @param {Array} operations - Operations
   * @param {Object} delta - Delta
   * @returns {Object} Delta-aware plan
   * @private
   */
  _createDeltaAwarePlan(operations, delta) {
    return {
      affectedEntities: this._extractAffectedEntities(delta),
      optimizedOperations: operations.filter(op => 
        this._isOperationAffected(op, delta)
      ),
      skipFullScan: delta.additions.length + delta.removals.length < 100
    };
  }

  /**
   * Extract affected entities from delta
   * @param {Object} delta - Delta
   * @returns {Object} Affected entities
   * @private
   */
  _extractAffectedEntities(delta) {
    const affected = {
      subjects: new Set(),
      predicates: new Set(),
      objects: new Set(),
      graphs: new Set()
    };
    
    for (const quad of [...delta.additions, ...delta.removals]) {
      affected.subjects.add(quad.subject.value);
      affected.predicates.add(quad.predicate.value);
      affected.objects.add(quad.object.value);
      if (quad.graph) {
        affected.graphs.add(quad.graph.value);
      }
    }
    
    return affected;
  }

  /**
   * Check if operation is affected by delta
   * @param {Object} operation - Operation
   * @param {Object} delta - Delta
   * @returns {boolean} Is affected
   * @private
   */
  _isOperationAffected(operation, delta) {
    // Simple check - in production this would analyze the operation
    return true;
  }

  /**
   * Estimate result rows
   * @param {string} query - Query
   * @param {Store} graph - RDF graph
   * @returns {number} Estimated rows
   * @private
   */
  _estimateResultRows(query, graph) {
    // Simple estimation
    return Math.min(graph.size * 0.1, 1000);
  }

  /**
   * Estimate SHACL cost
   * @param {Object} shape - SHACL shape
   * @param {Store} graph - RDF graph
   * @returns {number} Estimated cost
   * @private
   */
  _estimateShaclCost(shape, graph) {
    return graph.size * 0.5; // SHACL validation is expensive
  }

  /**
   * Parse SHACL shapes
   * @param {string} query - SHACL shapes
   * @returns {Array} Shapes
   * @private
   */
  _parseShaclShapes(query) {
    // Simple parsing - in production this would use a proper SHACL parser
    return [{ type: 'shacl-shape', cost: 100 }];
  }

  /**
   * Add quad to index
   * @param {Object} index - Index
   * @param {Object} quad - RDF quad
   * @private
   */
  _addToIndex(index, quad) {
    // Implementation depends on index type
    // This is a placeholder
  }

  /**
   * Remove quad from index
   * @param {Object} index - Index
   * @param {Object} quad - RDF quad
   * @private
   */
  _removeFromIndex(index, quad) {
    // Implementation depends on index type
    // This is a placeholder
  }

  /**
   * Execute optimized delta-aware query
   * @param {Object} plan - Query plan
   * @param {Store} graph - RDF graph
   * @param {Object} delta - Delta
   * @param {Object} affectedEntities - Affected entities
   * @returns {Promise<any>} Query result
   * @private
   */
  async _executeOptimizedDeltaAware(plan, graph, delta, affectedEntities) {
    // Use delta information to optimize execution
    // This is a placeholder for the actual optimization logic
    return { result: 'delta-aware execution' };
  }
}

/**
 * Create a query optimizer instance
 * @param {Object} [config] - Configuration
 * @returns {QueryOptimizer} Query optimizer
 */
export function createQueryOptimizer(config = {}) {
  return new QueryOptimizer(config);
}

/**
 * Default query optimizer instance
 */
export const defaultQueryOptimizer = createQueryOptimizer();
