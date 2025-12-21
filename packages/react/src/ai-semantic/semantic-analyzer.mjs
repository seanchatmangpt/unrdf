/**
 * @file AI Semantic Analyzer for RDF Graph Analysis
 * @module ai-semantic/semantic-analyzer
 *
 * @description
 * Implements AI-powered semantic analysis for RDF knowledge graphs.
 * Analyzes semantic relationships, extracts key concepts, computes similarity,
 * and suggests ontology improvements based on data patterns.
 *
 * Integrates with UNRDF's Knowledge Hook system and provides OTEL observability.
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import LRUCache from 'lru-cache';
import { z } from 'zod';

const tracer = trace.getTracer('unrdf-ai-semantic');

/**
 * Semantic analysis result schema
 */
const SemanticAnalysisResultSchema = z.object({
  concepts: z.array(
    z.object({
      uri: z.string(),
      label: z.string().optional(),
      frequency: z.number(),
      centrality: z.number(),
      type: z.string().optional(),
    })
  ),
  relationships: z.array(
    z.object({
      subject: z.string(),
      predicate: z.string(),
      object: z.string(),
      strength: z.number(),
    })
  ),
  patterns: z.array(
    z.object({
      pattern: z.string(),
      count: z.number(),
      confidence: z.number(),
    })
  ),
  suggestions: z.array(
    z.object({
      type: z.enum([
        'missing_inverse',
        'missing_subclass',
        'inconsistent_domain',
        'redundant_property',
      ]),
      description: z.string(),
      priority: z.enum(['high', 'medium', 'low']),
    })
  ),
  statistics: z.object({
    totalTriples: z.number(),
    uniqueSubjects: z.number(),
    uniquePredicates: z.number(),
    uniqueObjects: z.number(),
    avgDegree: z.number(),
    density: z.number(),
  }),
  duration: z.number(),
});

/**
 * Semantic similarity result schema
 */
const SimilarityResultSchema = z.object({
  similarity: z.number().min(0).max(1),
  method: z.string(),
  commonProperties: z.array(z.string()),
  commonNeighbors: z.array(z.string()),
});

/**
 * Semantic Analyzer Configuration
 */
const SemanticAnalyzerConfigSchema = z.object({
  cacheSize: z.number().default(1000),
  enableCache: z.boolean().default(true),
  maxConcepts: z.number().default(100),
  minConceptFrequency: z.number().default(2),
  similarityThreshold: z.number().min(0).max(1).default(0.7),
});

/**
 * AI Semantic Analyzer for RDF graphs
 */
export class SemanticAnalyzer {
  /**
   * Create a new semantic analyzer
   * @param {Object} [config] - Analyzer configuration
   */
  constructor(config = {}) {
    this.config = SemanticAnalyzerConfigSchema.parse(config);

    // LRU cache for analysis results
    this.cache = this.config.enableCache ? new LRUCache({ max: this.config.cacheSize }) : null;

    // Statistics
    this.stats = {
      analyses: 0,
      cacheHits: 0,
      cacheMisses: 0,
      avgDuration: 0,
    };
  }

  /**
   * Analyze an RDF graph to understand semantic relationships
   * @param {Store} store - RDF store to analyze
   * @param {Object} [options] - Analysis options
   * @param {boolean} [options.useCache=true] - Use cached results
   * @param {number} [options.maxConcepts] - Maximum concepts to extract
   * @returns {Promise<Object>} Analysis results
   */
  async analyze(store, options = {}) {
    return tracer.startActiveSpan('semantic.analyze', async (span) => {
      const startTime = Date.now();

      try {
        span.setAttributes({
          'semantic.store_size': store.size,
          'semantic.cache_enabled': this.config.enableCache,
        });

        // Check cache
        const cacheKey = this._getCacheKey(store);
        if (options.useCache !== false && this.cache) {
          const cached = this.cache.get(cacheKey);
          if (cached) {
            this.stats.cacheHits++;
            span.setAttribute('semantic.cache_hit', true);
            span.setStatus({ code: SpanStatusCode.OK });
            return cached;
          }
          this.stats.cacheMisses++;
        }

        // Extract concepts and entities
        const concepts = await this._extractConcepts(store, options.maxConcepts);
        span.setAttribute('semantic.concepts_count', concepts.length);

        // Analyze relationships
        const relationships = await this._analyzeRelationships(store);
        span.setAttribute('semantic.relationships_count', relationships.length);

        // Detect patterns
        const patterns = await this._detectPatterns(store);
        span.setAttribute('semantic.patterns_count', patterns.length);

        // Generate suggestions
        const suggestions = await this._generateSuggestions(store, concepts, relationships);
        span.setAttribute('semantic.suggestions_count', suggestions.length);

        // Calculate statistics
        const statistics = await this._calculateStatistics(store);

        const duration = Date.now() - startTime;
        const result = SemanticAnalysisResultSchema.parse({
          concepts,
          relationships,
          patterns,
          suggestions,
          statistics,
          duration,
        });

        // Update cache
        if (this.cache) {
          this.cache.set(cacheKey, result);
        }

        // Update stats
        this.stats.analyses++;
        this.stats.avgDuration =
          (this.stats.avgDuration * (this.stats.analyses - 1) + duration) / this.stats.analyses;

        span.setAttributes({
          'semantic.duration_ms': duration,
          'semantic.analysis_complete': true,
        });
        span.setStatus({ code: SpanStatusCode.OK });

        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Extract key concepts and entities from the graph
   * @param {Store} store - RDF store
   * @param {number} [maxConcepts] - Maximum concepts to return
   * @returns {Promise<Array>} Key concepts
   * @private
   */
  async _extractConcepts(store, maxConcepts) {
    return tracer.startActiveSpan('semantic.extract_concepts', async (span) => {
      try {
        const conceptMap = new Map();

        // Count subject occurrences
        for (const quad of store) {
          const subject = quad.subject.value;
          if (!conceptMap.has(subject)) {
            conceptMap.set(subject, {
              uri: subject,
              frequency: 0,
              types: new Set(),
              labels: new Set(),
            });
          }
          const concept = conceptMap.get(subject);
          concept.frequency++;

          // Track types
          if (quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') {
            concept.types.add(quad.object.value);
          }

          // Track labels
          if (quad.predicate.value === 'http://www.w3.org/2000/01/rdf-schema#label') {
            concept.labels.add(quad.object.value);
          }
        }

        // Calculate centrality (simplified PageRank-like metric)
        const centrality = await this._calculateCentrality(store, conceptMap);

        // Convert to array and filter
        let concepts = Array.from(conceptMap.entries())
          .map(([uri, data]) => ({
            uri,
            label: data.labels.size > 0 ? Array.from(data.labels)[0] : undefined,
            frequency: data.frequency,
            centrality: centrality.get(uri) || 0,
            type: data.types.size > 0 ? Array.from(data.types)[0] : undefined,
          }))
          .filter((c) => c.frequency >= this.config.minConceptFrequency)
          .sort((a, b) => b.centrality - a.centrality);

        // Limit results
        const limit = maxConcepts || this.config.maxConcepts;
        concepts = concepts.slice(0, limit);

        span.setAttribute('semantic.concepts_extracted', concepts.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return concepts;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Calculate centrality scores for concepts (simplified PageRank)
   * @param {Store} store - RDF store
   * @param {Map} conceptMap - Map of concepts
   * @returns {Promise<Map>} Centrality scores
   * @private
   */
  async _calculateCentrality(store, conceptMap) {
    const centrality = new Map();
    const damping = 0.85;
    const iterations = 10;

    // Initialize
    for (const uri of conceptMap.keys()) {
      centrality.set(uri, 1.0);
    }

    // Build adjacency list
    const outLinks = new Map();
    const inLinks = new Map();

    for (const quad of store) {
      const subj = quad.subject.value;
      const obj = quad.object.value;

      if (conceptMap.has(subj) && conceptMap.has(obj)) {
        if (!outLinks.has(subj)) outLinks.set(subj, []);
        if (!inLinks.has(obj)) inLinks.set(obj, []);
        outLinks.get(subj).push(obj);
        inLinks.get(obj).push(subj);
      }
    }

    // PageRank iterations
    for (let i = 0; i < iterations; i++) {
      const newCentrality = new Map();

      for (const uri of conceptMap.keys()) {
        let rank = 1 - damping;
        const incoming = inLinks.get(uri) || [];

        for (const source of incoming) {
          const outDegree = (outLinks.get(source) || []).length;
          if (outDegree > 0) {
            rank += damping * (centrality.get(source) / outDegree);
          }
        }

        newCentrality.set(uri, rank);
      }

      centrality.clear();
      for (const [uri, rank] of newCentrality) {
        centrality.set(uri, rank);
      }
    }

    return centrality;
  }

  /**
   * Analyze relationships between concepts
   * @param {Store} store - RDF store
   * @returns {Promise<Array>} Relationships with strength scores
   * @private
   */
  async _analyzeRelationships(store) {
    return tracer.startActiveSpan('semantic.analyze_relationships', async (span) => {
      try {
        const relationshipMap = new Map();

        for (const quad of store) {
          const key = `${quad.subject.value}|${quad.predicate.value}|${quad.object.value}`;
          if (!relationshipMap.has(key)) {
            relationshipMap.set(key, {
              subject: quad.subject.value,
              predicate: quad.predicate.value,
              object: quad.object.value,
              count: 0,
            });
          }
          relationshipMap.get(key).count++;
        }

        // Calculate relationship strength (normalized by max count)
        const maxCount = Math.max(...Array.from(relationshipMap.values()).map((r) => r.count), 1);

        const relationships = Array.from(relationshipMap.values())
          .map((r) => ({
            subject: r.subject,
            predicate: r.predicate,
            object: r.object,
            strength: r.count / maxCount,
          }))
          .sort((a, b) => b.strength - a.strength)
          .slice(0, 100); // Top 100 relationships

        span.setAttribute('semantic.relationships_analyzed', relationships.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return relationships;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        throw error;
      }
    });
  }

  /**
   * Detect common patterns in the graph
   * @param {Store} store - RDF store
   * @returns {Promise<Array>} Detected patterns
   * @private
   */
  async _detectPatterns(store) {
    return tracer.startActiveSpan('semantic.detect_patterns', async (span) => {
      try {
        const patterns = [];
        const predicateCount = new Map();

        // Count predicate usage
        for (const quad of store) {
          const pred = quad.predicate.value;
          predicateCount.set(pred, (predicateCount.get(pred) || 0) + 1);
        }

        // Identify common predicates
        const totalTriples = store.size;
        for (const [predicate, count] of predicateCount.entries()) {
          const confidence = count / totalTriples;
          if (count >= 3) {
            patterns.push({
              pattern: `Common predicate: ${predicate}`,
              count,
              confidence,
            });
          }
        }

        // Detect type patterns
        const typePatterns = await this._detectTypePatterns(store);
        patterns.push(...typePatterns);

        // Sort by confidence
        patterns.sort((a, b) => b.confidence - a.confidence);

        span.setAttribute('semantic.patterns_detected', patterns.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return patterns.slice(0, 20); // Top 20 patterns
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Detect type-based patterns
   * @param {Store} store - RDF store
   * @returns {Promise<Array>} Type patterns
   * @private
   */
  async _detectTypePatterns(store) {
    const typeCount = new Map();
    const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';

    for (const quad of store) {
      if (quad.predicate.value === RDF_TYPE) {
        const type = quad.object.value;
        typeCount.set(type, (typeCount.get(type) || 0) + 1);
      }
    }

    const totalEntities = typeCount.size;
    return Array.from(typeCount.entries())
      .filter(([_, count]) => count >= 2)
      .map(([type, count]) => ({
        pattern: `Entity type: ${type}`,
        count,
        confidence: count / Math.max(totalEntities, 1),
      }));
  }

  /**
   * Generate ontology improvement suggestions
   * @param {Store} store - RDF store
   * @param {Array} concepts - Extracted concepts
   * @param {Array} relationships - Analyzed relationships
   * @returns {Promise<Array>} Suggestions
   * @private
   */
  async _generateSuggestions(store, concepts, relationships) {
    return tracer.startActiveSpan('semantic.generate_suggestions', async (span) => {
      try {
        const suggestions = [];

        // Check for missing inverse properties
        const inverseCheck = await this._checkMissingInverses(store, relationships);
        suggestions.push(...inverseCheck);

        // Check for inconsistent domains/ranges
        const domainCheck = await this._checkInconsistentDomains(store);
        suggestions.push(...domainCheck);

        // Check for potential subclass relationships
        const subclassCheck = await this._checkPotentialSubclasses(store, concepts);
        suggestions.push(...subclassCheck);

        span.setAttribute('semantic.suggestions_generated', suggestions.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return suggestions;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        throw error;
      }
    });
  }

  /**
   * Check for missing inverse properties
   * @param {Store} store - RDF store
   * @param {Array} relationships - Relationships
   * @returns {Promise<Array>} Suggestions
   * @private
   */
  async _checkMissingInverses(store, relationships) {
    const suggestions = [];
    const predicates = new Set(relationships.map((r) => r.predicate));

    for (const pred of predicates) {
      // Simple heuristic: check if there's a reverse relationship pattern
      const forward = relationships.filter((r) => r.predicate === pred);
      const reverse = relationships.filter((r) =>
        forward.some((f) => f.subject === r.object && f.object === r.subject)
      );

      if (forward.length > 5 && reverse.length === 0) {
        suggestions.push({
          type: 'missing_inverse',
          description: `Consider adding inverse property for ${pred}`,
          priority: 'medium',
        });
      }
    }

    return suggestions;
  }

  /**
   * Check for inconsistent domain/range definitions
   * @param {Store} store - RDF store
   * @returns {Promise<Array>} Suggestions
   * @private
   */
  async _checkInconsistentDomains(store) {
    const suggestions = [];
    const RDFS_DOMAIN = 'http://www.w3.org/2000/01/rdf-schema#domain';
    const _RDFS_RANGE = 'http://www.w3.org/2000/01/rdf-schema#range';

    // This is a simplified check - a full implementation would be more comprehensive
    const domainMap = new Map();

    for (const quad of store) {
      if (quad.predicate.value === RDFS_DOMAIN) {
        domainMap.set(quad.subject.value, quad.object.value);
      }
    }

    // Check for predicates used with entities outside their declared domain
    for (const quad of store) {
      const expectedDomain = domainMap.get(quad.predicate.value);
      if (expectedDomain) {
        // Would need to check actual types - simplified here
        // In practice, query for rdf:type of subjects
      }
    }

    return suggestions;
  }

  /**
   * Check for potential subclass relationships
   * @param {Store} store - RDF store
   * @param {Array} concepts - Concepts
   * @returns {Promise<Array>} Suggestions
   * @private
   */
  async _checkPotentialSubclasses(store, _concepts) {
    const suggestions = [];
    const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';

    // Find entities with multiple types that might indicate subclass relationships
    const entityTypes = new Map();

    for (const quad of store) {
      if (quad.predicate.value === RDF_TYPE) {
        if (!entityTypes.has(quad.subject.value)) {
          entityTypes.set(quad.subject.value, new Set());
        }
        entityTypes.get(quad.subject.value).add(quad.object.value);
      }
    }

    // Entities with multiple types might suggest missing subclass axioms
    for (const [entity, types] of entityTypes.entries()) {
      if (types.size > 1) {
        const typeArray = Array.from(types);
        suggestions.push({
          type: 'missing_subclass',
          description: `Entity ${entity} has multiple types ${typeArray.join(', ')} - consider adding subclass relationships`,
          priority: 'low',
        });
      }
    }

    return suggestions.slice(0, 5); // Limit suggestions
  }

  /**
   * Calculate graph statistics
   * @param {Store} store - RDF store
   * @returns {Promise<Object>} Statistics
   * @private
   */
  async _calculateStatistics(store) {
    const subjects = new Set();
    const predicates = new Set();
    const objects = new Set();

    for (const quad of store) {
      subjects.add(quad.subject.value);
      predicates.add(quad.predicate.value);
      objects.add(quad.object.value);
    }

    const uniqueSubjects = subjects.size;
    const uniquePredicates = predicates.size;
    const uniqueObjects = objects.size;
    const totalTriples = store.size;

    // Calculate average degree (average number of edges per node)
    const nodes = new Set([...subjects, ...objects]);
    const avgDegree = totalTriples / Math.max(nodes.size, 1);

    // Calculate density (actual edges / possible edges)
    const maxPossibleEdges = nodes.size * (nodes.size - 1);
    const density = maxPossibleEdges > 0 ? totalTriples / maxPossibleEdges : 0;

    return {
      totalTriples,
      uniqueSubjects,
      uniquePredicates,
      uniqueObjects,
      avgDegree,
      density,
    };
  }

  /**
   * Compute semantic similarity between two concepts
   * @param {Store} store - RDF store
   * @param {string} concept1 - First concept URI
   * @param {string} concept2 - Second concept URI
   * @param {Object} [options] - Similarity options
   * @returns {Promise<Object>} Similarity result
   */
  async computeSimilarity(store, concept1, concept2, _options = {}) {
    return tracer.startActiveSpan('semantic.compute_similarity', async (span) => {
      try {
        span.setAttributes({
          'semantic.concept1': concept1,
          'semantic.concept2': concept2,
        });

        // Get properties for both concepts
        const props1 = this._getConceptProperties(store, concept1);
        const props2 = this._getConceptProperties(store, concept2);

        // Get neighbors
        const neighbors1 = this._getConceptNeighbors(store, concept1);
        const neighbors2 = this._getConceptNeighbors(store, concept2);

        // Jaccard similarity on properties
        const commonProps = props1.filter((p) => props2.includes(p));
        const propSimilarity =
          commonProps.length / Math.max(new Set([...props1, ...props2]).size, 1);

        // Jaccard similarity on neighbors
        const commonNeighbors = neighbors1.filter((n) => neighbors2.includes(n));
        const neighborSimilarity =
          commonNeighbors.length / Math.max(new Set([...neighbors1, ...neighbors2]).size, 1);

        // Combined similarity (weighted average)
        const similarity = propSimilarity * 0.6 + neighborSimilarity * 0.4;

        const result = SimilarityResultSchema.parse({
          similarity,
          method: 'jaccard',
          commonProperties: commonProps,
          commonNeighbors: commonNeighbors,
        });

        span.setAttributes({
          'semantic.similarity_score': similarity,
          'semantic.common_properties': commonProps.length,
          'semantic.common_neighbors': commonNeighbors.length,
        });
        span.setStatus({ code: SpanStatusCode.OK });

        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        throw error;
      }
    });
  }

  /**
   * Get properties of a concept
   * @param {Store} store - RDF store
   * @param {string} conceptUri - Concept URI
   * @returns {Array<string>} Property URIs
   * @private
   */
  _getConceptProperties(store, conceptUri) {
    const properties = [];
    for (const quad of store) {
      if (quad.subject.value === conceptUri) {
        properties.push(quad.predicate.value);
      }
    }
    return properties;
  }

  /**
   * Get neighbors of a concept
   * @param {Store} store - RDF store
   * @param {string} conceptUri - Concept URI
   * @returns {Array<string>} Neighbor URIs
   * @private
   */
  _getConceptNeighbors(store, conceptUri) {
    const neighbors = [];
    for (const quad of store) {
      if (quad.subject.value === conceptUri) {
        neighbors.push(quad.object.value);
      }
      if (quad.object.value === conceptUri) {
        neighbors.push(quad.subject.value);
      }
    }
    return neighbors;
  }

  /**
   * Generate cache key for a store
   * @param {Store} store - RDF store
   * @returns {string} Cache key
   * @private
   */
  _getCacheKey(store) {
    // Simple hash based on size and a sample of quads
    const sample = Array.from(store)
      .slice(0, 10)
      .map((q) => `${q.subject.value}|${q.predicate.value}|${q.object.value}`)
      .join('::');
    return `${store.size}:${sample}`;
  }

  /**
   * Clear the analysis cache
   */
  clearCache() {
    if (this.cache) {
      this.cache.clear();
    }
  }

  /**
   * Get analyzer statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      cacheSize: this.cache ? this.cache.size : 0,
      cacheHitRate: this.stats.analyses > 0 ? this.stats.cacheHits / this.stats.analyses : 0,
    };
  }
}

/**
 * Create a semantic analyzer instance
 * @param {Object} [config] - Configuration
 * @returns {SemanticAnalyzer} Semantic analyzer
 */
export function createSemanticAnalyzer(config = {}) {
  return new SemanticAnalyzer(config);
}

/**
 * Default semantic analyzer instance
 */
export const defaultSemanticAnalyzer = createSemanticAnalyzer();
