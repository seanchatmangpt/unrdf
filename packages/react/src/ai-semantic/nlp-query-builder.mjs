/**
 * @file NLP Query Builder for Natural Language to SPARQL Translation
 * @module ai-semantic/nlp-query-builder
 *
 * @description
 * Implements natural language processing for converting English queries to SPARQL.
 * Uses pattern matching, entity recognition, and semantic analysis to generate
 * accurate SPARQL queries from user input in natural language.
 *
 * Integrates with UNRDF's Knowledge Hook system and provides OTEL observability.
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import LRUCache from 'lru-cache';
import { z } from 'zod';

const tracer = trace.getTracer('unrdf-ai-nlp');

/**
 * Query pattern schema
 */
const QueryPatternSchema = z.object({
  pattern: z.string(),
  template: z.string(),
  confidence: z.number().min(0).max(1),
  variables: z.array(z.string()),
});

/**
 * NLP query result schema
 */
const NLPQueryResultSchema = z.object({
  sparql: z.string(),
  confidence: z.number().min(0).max(1),
  patterns: z.array(QueryPatternSchema),
  entities: z.array(z.string()),
  intent: z.enum(['select', 'ask', 'construct', 'describe']),
  duration: z.number(),
});

/**
 * NLP Query Builder Configuration
 */
const NLPQueryBuilderConfigSchema = z.object({
  cacheSize: z.number().default(1000),
  enableCache: z.boolean().default(true),
  confidenceThreshold: z.number().min(0).max(1).default(0.6),
  maxPatterns: z.number().default(5),
});

/**
 * NLP Query Builder for converting natural language to SPARQL
 */
export class NLPQueryBuilder {
  /**
   * Create a new NLP query builder
   * @param {Object} [config] - Builder configuration
   * @param {number} [config.cacheSize=1000] - Cache size for queries
   * @param {boolean} [config.enableCache=true] - Enable query caching
   * @param {number} [config.confidenceThreshold=0.6] - Minimum confidence threshold
   * @param {number} [config.maxPatterns=5] - Maximum patterns to match
   */
  constructor(config = {}) {
    this.config = NLPQueryBuilderConfigSchema.parse(config);

    // Initialize pattern database
    this.patterns = this._initializePatterns();

    // Initialize cache if enabled
    if (this.config.enableCache) {
      this.cache = new LRUCache({
        max: this.config.cacheSize,
        ttl: 1000 * 60 * 30, // 30 minutes
      });
    }

    // Statistics
    this.stats = {
      queries: 0,
      cacheHits: 0,
      successful: 0,
      failed: 0,
    };
  }

  /**
   * Convert natural language query to SPARQL
   * @param {string} naturalLanguage - Natural language query
   * @param {Object} [options] - Query options
   * @param {Object} [options.namespaces] - Namespace prefixes
   * @param {string} [options.defaultGraph] - Default graph URI
   * @returns {Promise<Object>} Query result with SPARQL and metadata
   */
  async buildQuery(naturalLanguage, options = {}) {
    return tracer.startActiveSpan('nlp.build_query', async (span) => {
      try {
        const startTime = Date.now();

        span.setAttributes({
          'nlp.query_length': naturalLanguage.length,
        });

        // Check cache
        const cacheKey = this._getCacheKey(naturalLanguage, options);
        if (this.config.enableCache && this.cache.has(cacheKey)) {
          this.stats.cacheHits++;
          this.stats.queries++;
          span.setStatus({ code: SpanStatusCode.OK });
          span.end();
          return this.cache.get(cacheKey);
        }

        // Normalize query
        const normalized = this._normalize(naturalLanguage);

        // Detect intent
        const intent = this._detectIntent(normalized);

        // Extract entities
        const entities = this._extractEntities(normalized);

        // Match patterns
        const matchedPatterns = this._matchPatterns(normalized);

        if (matchedPatterns.length === 0) {
          throw new Error('No matching patterns found for query');
        }

        // Generate SPARQL
        const sparql = this._generateSPARQL(
          matchedPatterns,
          entities,
          intent,
          options
        );

        const duration = Date.now() - startTime;
        const confidence = this._calculateConfidence(matchedPatterns);

        const result = NLPQueryResultSchema.parse({
          sparql,
          confidence,
          patterns: matchedPatterns,
          entities,
          intent,
          duration,
        });

        // Cache result
        if (this.config.enableCache) {
          this.cache.set(cacheKey, result);
        }

        this.stats.queries++;
        this.stats.successful++;

        span.setStatus({ code: SpanStatusCode.OK });
        span.setAttributes({
          'nlp.confidence': confidence,
          'nlp.intent': intent,
          'nlp.entities_count': entities.length,
        });
        span.end();

        return result;
      } catch (error) {
        this.stats.queries++;
        this.stats.failed++;
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.recordException(error);
        span.end();
        throw error;
      }
    });
  }

  /**
   * Initialize query patterns
   * @returns {Array<Object>} Pattern database
   * @private
   */
  _initializePatterns() {
    return [
      {
        pattern: /(?:find|get|show|list)\s+(?:all\s+)?(\w+)/i,
        template: 'SELECT ?item WHERE { ?item a :$1 }',
        confidence: 0.8,
        intent: 'select',
      },
      {
        pattern: /(?:what|which)\s+(\w+)\s+(?:has|have|with)\s+(\w+)/i,
        template: 'SELECT ?item WHERE { ?item :$2 ?value }',
        confidence: 0.75,
        intent: 'select',
      },
      {
        pattern: /(?:does|is)\s+(\w+)\s+(?:a|an)\s+(\w+)/i,
        template: 'ASK WHERE { :$1 a :$2 }',
        confidence: 0.85,
        intent: 'ask',
      },
      {
        pattern: /(?:count|how many)\s+(\w+)/i,
        template: 'SELECT (COUNT(?item) AS ?count) WHERE { ?item a :$1 }',
        confidence: 0.9,
        intent: 'select',
      },
      {
        pattern: /(?:describe|tell me about)\s+(\w+)/i,
        template: 'DESCRIBE :$1',
        confidence: 0.95,
        intent: 'describe',
      },
      {
        pattern: /(\w+)\s+(?:related to|connected to)\s+(\w+)/i,
        template: 'CONSTRUCT { ?x ?p ?y } WHERE { ?x ?p :$2 . :$2 ?p2 ?y }',
        confidence: 0.7,
        intent: 'construct',
      },
    ];
  }

  /**
   * Normalize natural language query
   * @param {string} query - Raw query
   * @returns {string} Normalized query
   * @private
   */
  _normalize(query) {
    return query
      .toLowerCase()
      .trim()
      .replace(/\s+/g, ' ')
      .replace(/[?!.]+$/, '');
  }

  /**
   * Detect query intent
   * @param {string} query - Normalized query
   * @returns {string} Intent type
   * @private
   */
  _detectIntent(query) {
    if (/^(?:does|is|are|can)/.test(query)) {
      return 'ask';
    }
    if (/^(?:describe|tell me about)/.test(query)) {
      return 'describe';
    }
    if (/(?:related to|connected to|construct)/.test(query)) {
      return 'construct';
    }
    return 'select';
  }

  /**
   * Extract entities from query
   * @param {string} query - Normalized query
   * @returns {Array<string>} Extracted entities
   * @private
   */
  _extractEntities(query) {
    const entities = [];

    // Extract capitalized words (potential entities)
    const words = query.split(' ');
    for (let i = 0; i < words.length; i++) {
      const word = words[i];
      if (word.length > 2 && /^[A-Z]/.test(word[0])) {
        entities.push(word);
      }
    }

    // Extract quoted strings
    const quoted = query.match(/"([^"]+)"/g);
    if (quoted) {
      entities.push(...quoted.map((q) => q.replace(/"/g, '')));
    }

    return entities;
  }

  /**
   * Match patterns against query
   * @param {string} query - Normalized query
   * @returns {Array<Object>} Matched patterns
   * @private
   */
  _matchPatterns(query) {
    const matches = [];

    for (const patternDef of this.patterns) {
      const match = query.match(patternDef.pattern);
      if (match) {
        matches.push({
          pattern: patternDef.pattern.toString(),
          template: patternDef.template,
          confidence: patternDef.confidence,
          variables: match.slice(1),
          intent: patternDef.intent,
        });
      }
    }

    // Sort by confidence
    matches.sort((a, b) => b.confidence - a.confidence);

    // Return top N patterns
    return matches.slice(0, this.config.maxPatterns);
  }

  /**
   * Generate SPARQL from matched patterns
   * @param {Array<Object>} patterns - Matched patterns
   * @param {Array<string>} entities - Extracted entities
   * @param {string} intent - Query intent
   * @param {Object} options - Query options
   * @returns {string} SPARQL query
   * @private
   */
  _generateSPARQL(patterns, entities, intent, options) {
    if (patterns.length === 0) {
      throw new Error('No patterns to generate SPARQL from');
    }

    // Use highest confidence pattern
    const bestPattern = patterns[0];

    // Replace variables in template
    let sparql = bestPattern.template;
    for (let i = 0; i < bestPattern.variables.length; i++) {
      sparql = sparql.replace(`$${i + 1}`, bestPattern.variables[i]);
    }

    // Add namespace prefixes
    if (options.namespaces) {
      const prefixes = Object.entries(options.namespaces)
        .map(([prefix, uri]) => `PREFIX ${prefix}: <${uri}>`)
        .join('\n');
      sparql = `${prefixes}\n\n${sparql}`;
    }

    // Add default graph if specified
    if (options.defaultGraph) {
      sparql = sparql.replace('WHERE {', `FROM <${options.defaultGraph}>\nWHERE {`);
    }

    return sparql;
  }

  /**
   * Calculate overall confidence from patterns
   * @param {Array<Object>} patterns - Matched patterns
   * @returns {number} Confidence score
   * @private
   */
  _calculateConfidence(patterns) {
    if (patterns.length === 0) return 0;

    // Weighted average of pattern confidences
    const totalConfidence = patterns.reduce((sum, p) => sum + p.confidence, 0);
    return totalConfidence / patterns.length;
  }

  /**
   * Generate cache key
   * @param {string} query - Natural language query
   * @param {Object} options - Query options
   * @returns {string} Cache key
   * @private
   */
  _getCacheKey(query, options) {
    const optionsHash = JSON.stringify(options);
    return `nlp:${query}:${optionsHash}`;
  }

  /**
   * Clear the query cache
   */
  clearCache() {
    if (this.cache) {
      this.cache.clear();
    }
  }

  /**
   * Get builder statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      cacheSize: this.cache ? this.cache.size : 0,
      cacheHitRate: this.stats.queries > 0 ? this.stats.cacheHits / this.stats.queries : 0,
      successRate: this.stats.queries > 0 ? this.stats.successful / this.stats.queries : 0,
    };
  }
}

/**
 * Create an NLP query builder instance
 * @param {Object} [config] - Configuration
 * @returns {NLPQueryBuilder} NLP query builder
 */
export function createNLPQueryBuilder(config = {}) {
  return new NLPQueryBuilder(config);
}

/**
 * Default NLP query builder instance
 */
export const defaultNLPQueryBuilder = createNLPQueryBuilder();
