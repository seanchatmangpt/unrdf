/**
 * @file Neural-Symbolic Hybrid Reasoning for RDF knowledge graphs
 * @module ai-ml-innovations/neural-symbolic-reasoner
 *
 * @description
 * Combines SPARQL/SHACL symbolic reasoning with neural embeddings for
 * probabilistic inference. Learns rule embeddings from SHACL shapes and
 * fuses symbolic + neural predictions with confidence weighting.
 *
 * Performance targets:
 * - Inference latency: <10ms (P95)
 * - Precision: >90% for high-confidence predictions
 * - Recall: >80% compared to pure symbolic
 */

import { z } from 'zod';
import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/ai-ml-innovations');

/**
 * Rule schema
 */
const RuleSchema = z.object({
  id: z.string(),
  name: z.string(),
  description: z.string().optional(),
  conditions: z.array(z.object({
    subject: z.string(),
    predicate: z.string(),
    object: z.string(),
  })),
  conclusion: z.object({
    subject: z.string(),
    predicate: z.string(),
    object: z.string(),
  }),
  confidence: z.number().min(0).max(1).default(1.0),
});

/**
 * Inference result schema
 */
const InferenceResultSchema = z.object({
  triple: z.object({
    subject: z.string(),
    predicate: z.string(),
    object: z.string(),
  }),
  rule: RuleSchema,
  confidence: z.number().min(0).max(1),
  method: z.enum(['symbolic', 'neural', 'hybrid']),
  explanation: z.string().optional(),
});

/**
 * Neural-Symbolic Reasoner configuration
 */
const NeuralSymbolicConfigSchema = z.object({
  embeddingDim: z.number().min(32).max(512).default(128),
  symbolicWeight: z.number().min(0).max(1).default(0.6),
  neuralWeight: z.number().min(0).max(1).default(0.4),
  minConfidence: z.number().min(0).max(1).default(0.7),
  maxRules: z.number().min(1).max(10000).default(1000),
  enableExplanations: z.boolean().default(true),
});

/**
 * Neural-Symbolic Hybrid Reasoner
 *
 * Combines symbolic SPARQL/SHACL reasoning with neural graph embeddings
 * for probabilistic inference over RDF knowledge graphs.
 *
 * @class
 */
export class NeuralSymbolicReasoner {
  /**
   * Create a new neural-symbolic reasoner
   *
   * @param {Object} config - Configuration
   * @param {Object} config.embeddingManager - Embedding manager instance
   * @param {Object} [config.shaclValidator] - SHACL validator instance
   * @param {Object} [config.inferenceEngine] - Symbolic inference engine
   */
  constructor(config = {}) {
    const validated = NeuralSymbolicConfigSchema.parse(config);

    this.config = validated;
    this.embeddingManager = config.embeddingManager;
    this.shaclValidator = config.shaclValidator;
    this.inferenceEngine = config.inferenceEngine;

    // Rule embeddings: rule ID -> {symbolic, neural, confidence}
    this.ruleEmbeddings = new Map();

    // Learned rule patterns
    this.rulePatterns = new Map();

    // Statistics
    this.stats = {
      symbolicInferences: 0,
      neuralInferences: 0,
      hybridInferences: 0,
      avgInferenceTime: 0,
      cacheHits: 0,
    };

    // Inference cache
    this.inferenceCache = new Map();
  }

  /**
   * Learn rule embeddings from SHACL shapes
   *
   * @param {Array<Object>} shaclShapes - SHACL shape definitions
   * @returns {Promise<Object>} Learning results
   *
   * @example
   * const results = await reasoner.learnRuleEmbeddings(shaclShapes);
   */
  async learnRuleEmbeddings(shaclShapes) {
    return tracer.startActiveSpan('neural_symbolic.learn_rules', async (span) => {
      try {
        span.setAttribute('rules.count', shaclShapes.length);

        // Parse SHACL shapes to rules
        const rules = await this.parseSHACLToRules(shaclShapes);

        span.setAttribute('rules.parsed', rules.length);

        // Embed each rule
        for (const rule of rules) {
          const embedding = await this.embedRule(rule);

          this.ruleEmbeddings.set(rule.id, {
            symbolic: rule,
            neural: embedding,
            confidence: rule.confidence,
          });
        }

        span.setAttributes({
          'rules.embedded': this.ruleEmbeddings.size,
          'embedding_dim': this.config.embeddingDim,
        });

        span.setStatus({ code: SpanStatusCode.OK });

        return {
          rulesLearned: this.ruleEmbeddings.size,
          embeddingDim: this.config.embeddingDim,
        };
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Hybrid inference: symbolic + neural reasoning
   *
   * @param {Object} triple - Input triple to reason about
   * @param {Object} options - Inference options
   * @returns {Promise<Array<Object>>} Inferred triples with confidence
   *
   * @example
   * const results = await reasoner.infer({
   *   subject: 'http://example.org/Alice',
   *   predicate: 'http://example.org/worksAt',
   *   object: 'http://example.org/CompanyA'
   * });
   */
  async infer(triple, options = {}) {
    return tracer.startActiveSpan('neural_symbolic.infer', async (span) => {
      const startTime = Date.now();

      try {
        const useCache = options.cache !== false;

        span.setAttributes({
          'triple.subject': triple.subject,
          'triple.predicate': triple.predicate,
          'triple.object': triple.object,
        });

        // Check cache
        const cacheKey = this._getCacheKey(triple);
        if (useCache && this.inferenceCache.has(cacheKey)) {
          this.stats.cacheHits++;
          span.setAttribute('cache.hit', true);
          const cached = this.inferenceCache.get(cacheKey);
          span.end();
          return cached;
        }

        // Step 1: Symbolic inference (deterministic)
        const symbolicResults = await this._symbolicInference(triple);

        span.setAttribute('symbolic.results', symbolicResults.length);

        // Step 2: Neural inference (probabilistic)
        const tripleEmbedding = await this._embedTriple(triple);
        const neuralResults = await this._neuralInference(tripleEmbedding, triple);

        span.setAttribute('neural.results', neuralResults.length);

        // Step 3: Fusion with confidence weighting
        const fusedResults = this.fuseInferences(symbolicResults, neuralResults);

        // Filter by confidence threshold
        const filtered = fusedResults.filter((r) => r.confidence >= this.config.minConfidence);

        const duration = Date.now() - startTime;
        this.stats.hybridInferences++;
        this.stats.avgInferenceTime =
          (this.stats.avgInferenceTime * (this.stats.hybridInferences - 1) + duration) /
          this.stats.hybridInferences;

        span.setAttributes({
          'inference.duration_ms': duration,
          'inference.results': filtered.length,
          'inference.avg_confidence':
            filtered.reduce((sum, r) => sum + r.confidence, 0) / filtered.length,
        });

        span.setStatus({ code: SpanStatusCode.OK });

        // Cache results
        if (useCache) {
          this.inferenceCache.set(cacheKey, filtered);
        }

        return filtered;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Parse SHACL shapes to rules
   *
   * @param {Array<Object>} shaclShapes - SHACL shapes
   * @returns {Promise<Array<Object>>} Parsed rules
   */
  async parseSHACLToRules(shaclShapes) {
    const rules = [];

    for (const shape of shaclShapes) {
      // Simplified SHACL parsing
      const rule = {
        id: shape.id || `rule_${rules.length}`,
        name: shape.name || `Rule ${rules.length}`,
        description: shape.description,
        conditions: shape.conditions || [],
        conclusion: shape.conclusion || {},
        confidence: 1.0, // Symbolic rules have full confidence
      };

      const validated = RuleSchema.parse(rule);
      rules.push(validated);
    }

    return rules;
  }

  /**
   * Embed a rule into vector space
   *
   * @param {Object} rule - Rule to embed
   * @returns {Promise<Array<number>>} Rule embedding
   */
  async embedRule(rule) {
    return tracer.startActiveSpan('neural_symbolic.embed_rule', async (span) => {
      try {
        span.setAttribute('rule.id', rule.id);

        // Embed conditions
        const conditionEmbeddings = await Promise.all(
          rule.conditions.map((cond) => this._embedTriple(cond))
        );

        // Embed conclusion
        const conclusionEmbedding = await this._embedTriple(rule.conclusion);

        // Combine embeddings (simplified: average pooling)
        const allEmbeddings = [...conditionEmbeddings, conclusionEmbedding];
        const ruleEmbedding = this._averagePooling(allEmbeddings);

        span.setAttributes({
          'rule.conditions': rule.conditions.length,
          'embedding.dim': ruleEmbedding.length,
        });

        span.setStatus({ code: SpanStatusCode.OK });

        return ruleEmbedding;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Fuse symbolic and neural inferences
   *
   * @param {Array<Object>} symbolic - Symbolic inference results
   * @param {Array<Object>} neural - Neural inference results
   * @returns {Array<Object>} Fused results
   */
  fuseInferences(symbolic, neural) {
    const combined = [];

    // Add symbolic results with symbolic weight
    for (const s of symbolic) {
      combined.push({
        ...s,
        confidence: s.confidence * this.config.symbolicWeight,
        method: 'symbolic',
      });
    }

    // Add or boost neural results
    for (const n of neural) {
      const existing = combined.find((c) => this.rulesEquivalent(c.rule, n.rule));

      if (!existing) {
        // New neural inference
        combined.push({
          ...n,
          confidence: n.confidence * this.config.neuralWeight,
          method: 'neural',
        });
      } else {
        // Both symbolic and neural agree - boost confidence
        existing.confidence = Math.min(
          1.0,
          existing.confidence + n.confidence * this.config.neuralWeight
        );
        existing.method = 'hybrid';
      }
    }

    // Sort by confidence descending
    combined.sort((a, b) => b.confidence - a.confidence);

    return combined.map((r) => InferenceResultSchema.parse(r));
  }

  /**
   * Check if two rules are equivalent
   *
   * @param {Object} rule1 - First rule
   * @param {Object} rule2 - Second rule
   * @returns {boolean} True if equivalent
   */
  rulesEquivalent(rule1, rule2) {
    return (
      rule1.id === rule2.id ||
      (this._triplesEqual(rule1.conclusion, rule2.conclusion) &&
        rule1.conditions.length === rule2.conditions.length &&
        rule1.conditions.every((c1, i) => this._triplesEqual(c1, rule2.conditions[i])))
    );
  }

  /**
   * Get reasoner statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      ruleEmbeddings: this.ruleEmbeddings.size,
      cacheSize: this.inferenceCache.size,
      config: this.config,
    };
  }

  /**
   * Clear inference cache
   */
  clearCache() {
    this.inferenceCache.clear();
  }

  /**
   * Symbolic inference using rules
   * @private
   */
  async _symbolicInference(triple) {
    const results = [];

    // Apply each rule symbolically
    for (const [_ruleId, ruleData] of this.ruleEmbeddings) {
      const rule = ruleData.symbolic;

      // Check if all conditions are satisfied
      const conditionsSatisfied = this._checkConditions(rule.conditions, triple);

      if (conditionsSatisfied) {
        results.push({
          triple: rule.conclusion,
          rule,
          confidence: rule.confidence,
          method: 'symbolic',
          explanation: this.config.enableExplanations
            ? this._generateExplanation(rule, 'symbolic')
            : undefined,
        });
      }
    }

    this.stats.symbolicInferences++;
    return results;
  }

  /**
   * Neural inference using embeddings
   * @private
   */
  async _neuralInference(tripleEmbedding, triple) {
    const results = [];

    // Compare triple embedding with each rule embedding
    for (const [_ruleId, ruleData] of this.ruleEmbeddings) {
      const similarity = this._cosineSimilarity(tripleEmbedding, ruleData.neural);

      // Threshold for neural inference
      if (similarity > 0.7) {
        results.push({
          triple: ruleData.symbolic.conclusion,
          rule: ruleData.symbolic,
          confidence: similarity,
          method: 'neural',
          explanation: this.config.enableExplanations
            ? this._generateExplanation(ruleData.symbolic, 'neural', similarity)
            : undefined,
        });
      }
    }

    this.stats.neuralInferences++;
    return results;
  }

  /**
   * Embed triple using embedding manager
   * @private
   */
  async _embedTriple(triple) {
    if (!this.embeddingManager) {
      // Fallback: simple hash-based embedding
      return this._hashEmbedding(triple);
    }

    // Use embedding manager if available
    const text = `${triple.subject} ${triple.predicate} ${triple.object}`;
    return this.embeddingManager.embed ?
      await this.embeddingManager.embed(text) :
      this._hashEmbedding(triple);
  }

  /**
   * Hash-based embedding (fallback)
   * @private
   */
  _hashEmbedding(triple) {
    const text = `${triple.subject} ${triple.predicate} ${triple.object}`;
    const hash = this._simpleHash(text);

    // Generate pseudo-random embedding from hash
    const embedding = new Array(this.config.embeddingDim);
    for (let i = 0; i < this.config.embeddingDim; i++) {
      const seed = hash + i;
      embedding[i] = (Math.sin(seed) + 1) / 2; // [0, 1]
    }

    return this._normalize(embedding);
  }

  /**
   * Simple string hash
   * @private
   */
  _simpleHash(str) {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = (hash << 5) - hash + char;
      hash = hash & hash; // Convert to 32-bit integer
    }
    return Math.abs(hash);
  }

  /**
   * Check if rule conditions are satisfied
   * @private
   */
  _checkConditions(conditions, triple) {
    // Simplified: check if any condition matches the triple
    return conditions.some((cond) => this._triplesEqual(cond, triple));
  }

  /**
   * Check if triples are equal
   * @private
   */
  _triplesEqual(t1, t2) {
    return (
      t1.subject === t2.subject && t1.predicate === t2.predicate && t1.object === t2.object
    );
  }

  /**
   * Generate explanation for inference
   * @private
   */
  _generateExplanation(rule, method, confidence = null) {
    let explanation = `[${method.toUpperCase()}] Applied rule: ${rule.name || rule.id}`;

    if (rule.description) {
      explanation += ` - ${rule.description}`;
    }

    if (method === 'neural' && confidence !== null) {
      explanation += ` (similarity: ${(confidence * 100).toFixed(1)}%)`;
    }

    return explanation;
  }

  /**
   * Average pooling of embeddings
   * @private
   */
  _averagePooling(embeddings) {
    if (embeddings.length === 0) {
      throw new Error('Cannot pool empty embeddings');
    }

    const dim = embeddings[0].length;
    const pooled = new Array(dim).fill(0);

    for (const emb of embeddings) {
      for (let i = 0; i < dim; i++) {
        pooled[i] += emb[i] / embeddings.length;
      }
    }

    return this._normalize(pooled);
  }

  /**
   * Normalize vector
   * @private
   */
  _normalize(vec) {
    const norm = Math.sqrt(vec.reduce((sum, val) => sum + val * val, 0));
    return norm > 0 ? vec.map((val) => val / norm) : vec;
  }

  /**
   * Cosine similarity
   * @private
   */
  _cosineSimilarity(a, b) {
    const dot = a.reduce((sum, val, i) => sum + val * b[i], 0);
    const normA = Math.sqrt(a.reduce((sum, val) => sum + val * val, 0));
    const normB = Math.sqrt(b.reduce((sum, val) => sum + val * val, 0));
    return normA > 0 && normB > 0 ? dot / (normA * normB) : 0;
  }

  /**
   * Get cache key for triple
   * @private
   */
  _getCacheKey(triple) {
    return `${triple.subject}|${triple.predicate}|${triple.object}`;
  }
}

/**
 * Create neural-symbolic reasoner instance
 *
 * @param {Object} config - Configuration
 * @returns {NeuralSymbolicReasoner} Reasoner instance
 */
export function createNeuralSymbolicReasoner(config = {}) {
  return new NeuralSymbolicReasoner(config);
}

export default NeuralSymbolicReasoner;
