/**
 * @file Natural Language to SPARQL Query Builder
 * @module ai-semantic/nlp-query-builder
 *
 * @description
 * Implements NL→SPARQL translation for natural language queries over RDF graphs.
 * Parses natural language questions, maps entities to RDF terms, generates SPARQL,
 * and supports complex multi-part queries with caching and optional LLM integration.
 *
 * Performance target: <300ms for NL→SPARQL translation
 */

import { _Store } from '@unrdf/core/rdf/n3-justified-only';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { LRUCache } from 'lru-cache';
import { z } from 'zod';

const tracer = trace.getTracer('unrdf-ai-semantic');

/**
 * NLP Query Result Schema
 */
const NLPQueryResultSchema = z.object({
  sparql: z.string(),
  entities: z.array(
    z.object({
      text: z.string(),
      uri: z.string(),
      type: z.string().optional(),
    })
  ),
  intent: z.enum(['select', 'ask', 'describe', 'construct']),
  confidence: z.number().min(0).max(1),
  method: z.enum(['pattern', 'llm', 'hybrid']),
  duration: z.number(),
});

/**
 * NLP Query Builder Configuration
 */
const NLPQueryBuilderConfigSchema = z.object({
  cacheSize: z.number().default(500),
  enableCache: z.boolean().default(true),
  enableLLM: z.boolean().default(false),
  llmApiKey: z.string().optional(),
  llmModel: z.string().default('gpt-3.5-turbo'),
  timeout: z.number().default(300), // 300ms timeout
  minConfidence: z.number().min(0).max(1).default(0.6),
});

/**
 * Natural Language to SPARQL Query Builder
 */
export class NLPQueryBuilder {
  /**
   * Create a new NLP query builder
   * @param {Object} [config] - Builder configuration
   */
  constructor(config = {}) {
    this.config = NLPQueryBuilderConfigSchema.parse(config);

    // LRU cache for query patterns
    this.cache = this.config.enableCache ? new LRUCache({ max: this.config.cacheSize }) : null;

    // Common SPARQL patterns for NL queries
    this.patterns = this._initializePatterns();

    // Entity mapping cache
    this.entityCache = new Map();

    // Statistics
    this.stats = {
      queries: 0,
      cacheHits: 0,
      cacheMisses: 0,
      avgDuration: 0,
      llmCalls: 0,
    };
  }

  /**
   * Initialize common NL→SPARQL patterns
   * @returns {Array} Query patterns
   * @private
   */
  _initializePatterns() {
    return [
      {
        // Non-capturing groups for question word and verb - we only need subject and predicate
        pattern: /^(?:what|which)\s+(.+?)\s+(?:is|are|was|were)\s+(.+?)$/i,
        intent: 'select',
        generator: (match, entities) => {
          const [_, subject, predicate] = match;
          const subjectUri = entities.find(e => e.text === subject.trim())?.uri || '?subject';
          const predicateUri = entities.find(e => e.text === predicate.trim())?.uri || '?predicate';
          return `SELECT ?result WHERE { <${subjectUri}> <${predicateUri}> ?result . }`;
        },
      },
      {
        // Non-capturing groups - we only need the subject
        pattern: /^(?:who|what)\s+(?:is|are|was|were)\s+(.+?)$/i,
        intent: 'describe',
        generator: (match, entities) => {
          const [_, subject] = match;
          const subjectUri = entities.find(e => e.text === subject.trim())?.uri || subject.trim();
          return `DESCRIBE <${subjectUri}>`;
        },
      },
      {
        // Non-capturing groups for verb and optional "all" - we only need the object
        pattern: /^(?:list|show|find)\s+(?:all\s+)?(.+?)$/i,
        intent: 'select',
        generator: (match, entities) => {
          const [_, object] = match;
          const objectType = entities.find(e => e.text === object.trim())?.uri;
          if (objectType) {
            return `SELECT ?item WHERE { ?item a <${objectType}> . }`;
          }
          return `SELECT ?item WHERE { ?item ?p ?o . FILTER(regex(str(?o), "${object}", "i")) }`;
        },
      },
      {
        // Non-capturing group for "how many" or "count" - we only need the object
        pattern: /^(?:how\s+many|count)\s+(.+?)$/i,
        intent: 'select',
        generator: (match, entities) => {
          const [_, object] = match;
          const objectType = entities.find(e => e.text === object.trim())?.uri;
          if (objectType) {
            return `SELECT (COUNT(?item) AS ?count) WHERE { ?item a <${objectType}> . }`;
          }
          return `SELECT (COUNT(?item) AS ?count) WHERE { ?item ?p ?o . FILTER(regex(str(?o), "${object}", "i")) }`;
        },
      },
      {
        // Non-capturing group for first verb - we need subject, predicate verb, and object
        pattern: /^(?:does|is|are)\s+(.+?)\s+(have|has|contain|contains)\s+(.+?)$/i,
        intent: 'ask',
        generator: (match, entities) => {
          const [_, subject, predicateVerb, object] = match;
          const subjectUri = entities.find(e => e.text === subject.trim())?.uri || '?subject';
          // predicateVerb (have/has/contain/contains) helps identify the predicate
          const predicateUri =
            entities.find(e => e.text === predicateVerb.trim())?.uri || '?predicate';
          const objectUri = entities.find(e => e.text === object.trim())?.uri || '?object';
          // Use actual predicate URI if found, otherwise use variable
          const predicatePart = predicateUri !== '?predicate' ? `<${predicateUri}>` : '?predicate';
          return `ASK WHERE { <${subjectUri}> ${predicatePart} <${objectUri}> . }`;
        },
      },
      {
        // Non-capturing group for relation phrase - we need subject and object
        pattern: /^(.+?)\s+(?:related\s+to|connected\s+to|linked\s+to)\s+(.+?)$/i,
        intent: 'select',
        generator: (match, entities) => {
          const [_, subject, object] = match;
          const subjectUri = entities.find(e => e.text === subject.trim())?.uri || subject.trim();
          const objectUri = entities.find(e => e.text === object.trim())?.uri || object.trim();
          return `SELECT ?relation WHERE {
            { <${subjectUri}> ?relation <${objectUri}> . }
            UNION
            { <${objectUri}> ?relation <${subjectUri}> . }
          }`;
        },
      },
    ];
  }

  /**
   * Parse natural language query and generate SPARQL
   * @param {string} nlQuery - Natural language query
   * @param {Store} store - RDF store for entity resolution
   * @param {Object} [options] - Query options
   * @returns {Promise<Object>} Query result with SPARQL
   */
  async buildQuery(nlQuery, store, options = {}) {
    // Merge options with config for runtime overrides
    const config = { ...this.config, ...options };

    return tracer.startActiveSpan('nlp.build_query', async span => {
      const startTime = Date.now();

      try {
        span.setAttributes({
          'nlp.query_length': nlQuery.length,
          'nlp.store_size': store.size,
          'nlp.cache_enabled': config.enableCache,
        });

        // Check cache
        const cacheKey = `${nlQuery}:${store.size}`;
        if (this.cache) {
          const cached = this.cache.get(cacheKey);
          if (cached) {
            this.stats.cacheHits++;
            span.setAttribute('nlp.cache_hit', true);
            span.setStatus({ code: SpanStatusCode.OK });
            return cached;
          }
          this.stats.cacheMisses++;
        }

        // Normalize query
        const normalized = this._normalizeQuery(nlQuery);
        span.setAttribute('nlp.normalized_query', normalized);

        // Extract and map entities
        const entities = await this._extractAndMapEntities(normalized, store);
        span.setAttribute('nlp.entities_found', entities.length);

        // Try pattern-based translation
        let sparql = null;
        let intent = 'select';
        let confidence = 0;
        let method = 'pattern';

        const patternResult = this._matchPattern(normalized, entities);
        if (patternResult) {
          sparql = patternResult.sparql;
          intent = patternResult.intent;
          confidence = patternResult.confidence;
          method = 'pattern';
        }

        // Fallback to LLM if enabled and confidence is low
        if (config.enableLLM && confidence < config.minConfidence) {
          const llmResult = await this._useLLMTranslation(normalized, store, entities, config);
          if (llmResult && llmResult.confidence > confidence) {
            sparql = llmResult.sparql;
            intent = llmResult.intent;
            confidence = llmResult.confidence;
            method = 'llm';
            this.stats.llmCalls++;
          }
        }

        // If still no SPARQL, use generic fallback
        if (!sparql) {
          sparql = this._generateFallbackSPARQL(normalized, entities);
          confidence = 0.3;
          method = 'pattern';
        }

        const duration = Date.now() - startTime;

        // Check timeout
        if (duration > this.config.timeout) {
          console.warn(
            `[NLP] Query translation exceeded timeout: ${duration}ms > ${this.config.timeout}ms`
          );
        }

        const result = NLPQueryResultSchema.parse({
          sparql,
          entities,
          intent,
          confidence,
          method,
          duration,
        });

        // Cache result
        if (this.cache) {
          this.cache.set(cacheKey, result);
        }

        // Update stats
        this.stats.queries++;
        this.stats.avgDuration =
          (this.stats.avgDuration * (this.stats.queries - 1) + duration) / this.stats.queries;

        span.setAttributes({
          'nlp.duration_ms': duration,
          'nlp.intent': intent,
          'nlp.confidence': confidence,
          'nlp.method': method,
          'nlp.translation_complete': true,
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
   * Normalize natural language query
   * @param {string} query - Raw query
   * @returns {string} Normalized query
   * @private
   */
  _normalizeQuery(query) {
    return query
      .trim()
      .toLowerCase()
      .replace(/\s+/g, ' ')
      .replace(/[?!.]+$/, '');
  }

  /**
   * Extract and map entities from query to RDF terms
   * @param {string} query - Normalized query
   * @param {Store} store - RDF store
   * @returns {Promise<Array>} Mapped entities
   * @private
   */
  async _extractAndMapEntities(query, store) {
    return tracer.startActiveSpan('nlp.extract_entities', async span => {
      try {
        const entities = [];
        const words = query.split(' ');

        // Extract potential entities (simple keyword extraction)
        const keywords = words.filter(
          w =>
            w.length > 3 &&
            ![
              'what',
              'which',
              'where',
              'when',
              'how',
              'who',
              'does',
              'have',
              'list',
              'show',
              'find',
            ].includes(w)
        );

        // Map keywords to RDF URIs
        for (const keyword of keywords) {
          const uri = await this._findEntityURI(keyword, store);
          if (uri) {
            entities.push({
              text: keyword,
              uri,
              type: await this._getEntityType(uri, store),
            });
          }
        }

        span.setAttribute('nlp.entities_extracted', entities.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return entities;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        return [];
      }
    });
  }

  /**
   * Find RDF URI for an entity text
   * @param {string} text - Entity text
   * @param {Store} store - RDF store
   * @returns {Promise<string|null>} Entity URI
   * @private
   */
  async _findEntityURI(text, store) {
    // Check cache
    if (this.entityCache.has(text)) {
      return this.entityCache.get(text);
    }

    const RDFS_LABEL = 'http://www.w3.org/2000/01/rdf-schema#label';

    // Search for matching labels
    for (const quad of store) {
      if (quad.predicate.value === RDFS_LABEL && quad.object.value.toLowerCase().includes(text)) {
        const uri = quad.subject.value;
        this.entityCache.set(text, uri);
        return uri;
      }
    }

    // Search in URIs themselves (for cases where URI contains the text)
    for (const quad of store) {
      if (quad.subject.value.toLowerCase().includes(text)) {
        const uri = quad.subject.value;
        this.entityCache.set(text, uri);
        return uri;
      }
    }

    return null;
  }

  /**
   * Get entity type from store
   * @param {string} uri - Entity URI
   * @param {Store} store - RDF store
   * @returns {Promise<string|undefined>} Entity type
   * @private
   */
  async _getEntityType(uri, store) {
    const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';

    for (const quad of store) {
      if (quad.subject.value === uri && quad.predicate.value === RDF_TYPE) {
        return quad.object.value;
      }
    }

    return undefined;
  }

  /**
   * Match query against patterns
   * @param {string} query - Normalized query
   * @param {Array} entities - Extracted entities
   * @returns {Object|null} Pattern match result
   * @private
   */
  _matchPattern(query, entities) {
    for (const pattern of this.patterns) {
      const match = query.match(pattern.pattern);
      if (match) {
        try {
          const sparql = pattern.generator(match, entities);
          return {
            sparql,
            intent: pattern.intent,
            confidence: 0.8, // Pattern-based confidence
          };
        } catch (error) {
          console.warn(`[NLP] Pattern generation failed:`, error.message);
        }
      }
    }

    return null;
  }

  /**
   * Use LLM for query translation (optional, requires API key)
   * @param {string} query - Normalized query
   * @param {Store} store - RDF store
   * @param {Array} entities - Extracted entities
   * @returns {Promise<Object|null>} LLM translation result
   * @private
   */
  async _useLLMTranslation(query, store, entities, config = null) {
    const cfg = config || this.config;
    if (!cfg.llmApiKey) {
      return null;
    }

    const provider = cfg.llmProvider || 'openai';
    const model =
      cfg.llmModel || (provider === 'anthropic' ? 'claude-3-5-sonnet-20241022' : 'gpt-4');

    try {
      let sparql;

      if (provider === 'anthropic') {
        // Anthropic Claude API
        const response = await fetch('https://api.anthropic.com/v1/messages', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'x-api-key': cfg.llmApiKey,
            'anthropic-version': '2023-06-01',
          },
          body: JSON.stringify({
            model,
            max_tokens: 500,
            messages: [
              {
                role: 'user',
                content: `You are a SPARQL query generator. Convert this natural language query to SPARQL:\n\n"${query}"\n\nAvailable entities: ${JSON.stringify(entities)}\n\nReturn only the SPARQL query, no explanation.`,
              },
            ],
          }),
        });

        if (!response.ok) {
          throw new Error(`Anthropic API error: ${response.statusText}`);
        }

        const data = await response.json();
        sparql = data.content[0].text.trim();
      } else {
        // OpenAI API (default)
        const response = await fetch('https://api.openai.com/v1/chat/completions', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: `Bearer ${cfg.llmApiKey}`,
          },
          body: JSON.stringify({
            model,
            messages: [
              {
                role: 'system',
                content:
                  'You are a SPARQL query generator. Convert natural language to SPARQL. Return only the SPARQL query, no explanation.',
              },
              {
                role: 'user',
                content: `Convert this query to SPARQL: "${query}"\n\nAvailable entities: ${JSON.stringify(entities)}`,
              },
            ],
            temperature: 0.3,
            max_tokens: 500,
          }),
        });

        if (!response.ok) {
          throw new Error(`OpenAI API error: ${response.statusText}`);
        }

        const data = await response.json();
        sparql = data.choices[0].message.content.trim();
      }

      // Extract SPARQL from code blocks if present
      const codeBlockMatch = sparql.match(/```(?:sparql)?\n?(.*?)```/s);
      if (codeBlockMatch) {
        sparql = codeBlockMatch[1].trim();
      }

      return {
        sparql,
        intent: this._inferIntent(sparql),
        confidence: 0.9,
      };
    } catch (error) {
      console.warn('[NLP] LLM translation failed:', error.message);
      return null;
    }
  }

  /**
   * Generate fallback SPARQL query
   * @param {string} query - Normalized query
   * @param {Array} entities - Extracted entities
   * @returns {string} Fallback SPARQL
   * @private
   */
  _generateFallbackSPARQL(query, _entities) {
    // Generic SELECT query that searches for any triple matching the query terms
    const keywords = query.split(' ').filter(w => w.length > 3);
    const filterConditions = keywords
      .map(k => `regex(str(?s), "${k}", "i") || regex(str(?o), "${k}", "i")`)
      .join(' || ');

    return `SELECT ?s ?p ?o WHERE {
      ?s ?p ?o .
      FILTER(${filterConditions || 'true'})
    } LIMIT 10`;
  }

  /**
   * Infer query intent from SPARQL
   * @param {string} sparql - SPARQL query
   * @returns {string} Intent
   * @private
   */
  _inferIntent(sparql) {
    const upper = sparql.trim().toUpperCase();
    if (upper.startsWith('SELECT')) return 'select';
    if (upper.startsWith('ASK')) return 'ask';
    if (upper.startsWith('DESCRIBE')) return 'describe';
    if (upper.startsWith('CONSTRUCT')) return 'construct';
    return 'select';
  }

  /**
   * Clear caches
   */
  clearCache() {
    if (this.cache) {
      this.cache.clear();
    }
    this.entityCache.clear();
  }

  /**
   * Get builder statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      cacheSize: this.cache ? this.cache.size : 0,
      entityCacheSize: this.entityCache.size,
      cacheHitRate: this.stats.queries > 0 ? this.stats.cacheHits / this.stats.queries : 0,
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
