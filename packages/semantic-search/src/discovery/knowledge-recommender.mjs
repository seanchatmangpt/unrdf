import { RDFEmbedder } from '../embeddings/rdf-embedder.mjs';
import { z } from 'zod';

/**
 * Zod schemas for validation
 */
const RecommendationOptionsSchema = z.object({
  limit: z.number().int().positive().default(10),
  threshold: z.number().min(0).max(1).default(0.6),
  diversityWeight: z.number().min(0).max(1).default(0.3),
}).partial();

/**
 * KnowledgeRecommender - Discover and recommend related concepts
 * Uses embeddings for knowledge graph exploration
 */
export class KnowledgeRecommender {
  /**
   * @param {Object} store - Oxigraph RDF store
   * @param {Object} options - Recommender configuration
   */
  constructor(store, options = {}) {
    if (!store) {
      throw new Error('RDF store is required');
    }

    this.store = store;
    this.embedder = new RDFEmbedder(options.embedder || {});
    this.entityEmbeddings = new Map();
  }

  /**
   * Initialize the recommender
   * @returns {Promise<void>}
   */
  async initialize() {
    await this.embedder.initialize();
  }

  /**
   * Get all triples about an entity
   * @param {string} entityUri - Entity URI
   * @returns {Array<Object>} Triples about entity
   */
  getEntityTriples(entityUri) {
    // Get triples where entity is subject
    const asSubject = this.store.match(
      { value: entityUri },
      null,
      null
    );

    // Get triples where entity is object
    const asObject = this.store.match(
      null,
      null,
      { value: entityUri }
    );

    return [...asSubject, ...asObject];
  }

  /**
   * Generate embedding for an entity
   * @param {string} entityUri - Entity URI
   * @returns {Promise<number[]>} Entity embedding
   */
  async embedEntity(entityUri) {
    // Check cache
    if (this.entityEmbeddings.has(entityUri)) {
      return this.entityEmbeddings.get(entityUri);
    }

    const triples = this.getEntityTriples(entityUri);

    if (triples.length === 0) {
      throw new Error(`No triples found for entity: ${entityUri}`);
    }

    const embedding = await this.embedder.embedEntity(triples);

    // Cache result
    this.entityEmbeddings.set(entityUri, embedding);

    return embedding;
  }

  /**
   * Find similar entities to a given entity
   * @param {string} entityUri - Reference entity URI
   * @param {Object} options - Recommendation options
   * @returns {Promise<Array>} Similar entities with scores
   */
  async findSimilarEntities(entityUri, options = {}) {
    const config = RecommendationOptionsSchema.parse(options);

    if (!this.embedder.embedder) {
      await this.initialize();
    }

    // Get embedding for query entity
    const queryEmbedding = await this.embedEntity(entityUri);

    // Get all unique entities in the graph
    const entities = this.getAllEntities();

    // Calculate similarities
    const similarities = [];

    for (const entity of entities) {
      if (entity === entityUri) continue; // Skip self

      try {
        const entityEmbedding = await this.embedEntity(entity);
        const similarity = this.cosineSimilarity(queryEmbedding, entityEmbedding);

        if (similarity >= config.threshold) {
          similarities.push({
            entity,
            label: this.embedder.extractLabel(entity),
            score: similarity,
          });
        }
      } catch (error) {
        // Skip entities with no triples
        continue;
      }
    }

    // Sort by similarity
    similarities.sort((a, b) => b.score - a.score);

    // Apply diversity if requested
    if (config.diversityWeight > 0) {
      return this.diversifyResults(similarities, config.limit, config.diversityWeight);
    }

    return similarities.slice(0, config.limit);
  }

  /**
   * Calculate cosine similarity between two vectors
   * @param {number[]} a - First vector
   * @param {number[]} b - Second vector
   * @returns {number} Cosine similarity [-1, 1]
   */
  cosineSimilarity(a, b) {
    if (a.length !== b.length) {
      throw new Error('Vectors must have same dimension');
    }

    let dotProduct = 0;
    let normA = 0;
    let normB = 0;

    for (let i = 0; i < a.length; i++) {
      dotProduct += a[i] * b[i];
      normA += a[i] * a[i];
      normB += b[i] * b[i];
    }

    return dotProduct / (Math.sqrt(normA) * Math.sqrt(normB));
  }

  /**
   * Get all unique entities in the graph
   * @returns {Array<string>} Entity URIs
   */
  getAllEntities() {
    const entities = new Set();
    const triples = this.store.match(null, null, null);

    triples.forEach(triple => {
      // Add subjects (entities)
      if (triple.subject.value.startsWith('http')) {
        entities.add(triple.subject.value);
      }
      // Add objects that are URIs (entities)
      if (triple.object.value && triple.object.value.startsWith('http')) {
        entities.add(triple.object.value);
      }
    });

    return Array.from(entities);
  }

  /**
   * Diversify results to avoid redundancy
   * @param {Array} results - Sorted results
   * @param {number} limit - Result limit
   * @param {number} diversityWeight - Diversity importance [0, 1]
   * @returns {Array} Diversified results
   */
  diversifyResults(results, limit, diversityWeight) {
    if (results.length <= limit) {
      return results;
    }

    const selected = [results[0]]; // Always take top result
    const remaining = results.slice(1);

    while (selected.length < limit && remaining.length > 0) {
      let bestIdx = 0;
      let bestScore = -Infinity;

      for (let i = 0; i < remaining.length; i++) {
        // Calculate min similarity to already selected
        let minSimilarity = Infinity;

        for (const sel of selected) {
          const sim = this.approximateSimilarity(
            remaining[i].entity,
            sel.entity
          );
          minSimilarity = Math.min(minSimilarity, sim);
        }

        // Combine relevance and diversity
        const combinedScore =
          remaining[i].score * (1 - diversityWeight) +
          (1 - minSimilarity) * diversityWeight;

        if (combinedScore > bestScore) {
          bestScore = combinedScore;
          bestIdx = i;
        }
      }

      selected.push(remaining[bestIdx]);
      remaining.splice(bestIdx, 1);
    }

    return selected;
  }

  /**
   * Approximate similarity between two entities (fast)
   * @param {string} entityA - First entity URI
   * @param {string} entityB - Second entity URI
   * @returns {number} Approximate similarity
   */
  approximateSimilarity(entityA, entityB) {
    // Simple string-based similarity for diversity
    const labelA = this.embedder.extractLabel(entityA);
    const labelB = this.embedder.extractLabel(entityB);

    const common = new Set([...labelA].filter(c => labelB.includes(c)));
    const union = new Set([...labelA, ...labelB]);

    return common.size / union.size;
  }

  /**
   * Recommend concepts related to a query
   * @param {string} query - Natural language query
   * @param {Object} options - Recommendation options
   * @returns {Promise<Array>} Recommended concepts
   */
  async recommendConcepts(query, options = {}) {
    const config = RecommendationOptionsSchema.parse(options);

    if (!this.embedder.embedder) {
      await this.initialize();
    }

    // Embed query
    const queryEmbedding = await this.embedder.embedText(query);

    // Get all entities
    const entities = this.getAllEntities();

    // Calculate similarities
    const recommendations = [];

    for (const entity of entities) {
      try {
        const entityEmbedding = await this.embedEntity(entity);
        const similarity = this.cosineSimilarity(queryEmbedding, entityEmbedding);

        if (similarity >= config.threshold) {
          recommendations.push({
            entity,
            label: this.embedder.extractLabel(entity),
            score: similarity,
            tripleCount: this.getEntityTriples(entity).length,
          });
        }
      } catch (error) {
        continue;
      }
    }

    recommendations.sort((a, b) => b.score - a.score);
    return recommendations.slice(0, config.limit);
  }

  /**
   * Clear caches
   */
  clearCache() {
    this.entityEmbeddings.clear();
    this.embedder.clearCache();
  }

  /**
   * Get recommender statistics
   * @returns {Object} Stats
   */
  getStats() {
    return {
      cachedEntities: this.entityEmbeddings.size,
      totalEntities: this.getAllEntities().length,
      embedderStats: this.embedder.getCacheStats(),
    };
  }
}

export default KnowledgeRecommender;
