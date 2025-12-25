/**
 * AI-Enhanced Knowledge Graph Search
 * Semantic similarity search on RDF triples using transformer models
 *
 * @module @unrdf/knowledge-engine/ai-enhanced-search
 * @description
 * Integrates Xenova Transformers (WASM-based) for semantic embeddings
 * and similarity search over RDF knowledge graphs. Enables natural
 * language queries against structured RDF data with vector similarity.
 */

import { z } from 'zod';
import { pipeline, env } from '@xenova/transformers';
import { query as sparqlQuery } from './query.mjs';
import { KnowledgeSubstrateCore } from './knowledge-substrate-core.mjs';

// Configure Transformers.js to use local models (no external downloads in production)
env.allowRemoteModels = false;
env.allowLocalModels = true;

// =============================================================================
// Configuration Schemas
// =============================================================================

/**
 * AI search configuration schema
 */
export const AISearchConfigSchema = z.object({
  /** Embedding model name (default: Xenova/all-MiniLM-L6-v2) */
  model: z.string().default('Xenova/all-MiniLM-L6-v2'),
  /** Maximum number of results to return */
  topK: z.number().int().positive().default(10),
  /** Minimum similarity threshold (0-1) */
  threshold: z.number().min(0).max(1).default(0.5),
  /** Enable caching of embeddings */
  cache: z.boolean().default(true),
  /** Batch size for embedding generation */
  batchSize: z.number().int().positive().default(32),
});

/**
 * Search result schema
 */
export const SearchResultSchema = z.object({
  /** RDF triple (subject, predicate, object) */
  triple: z.object({
    subject: z.string(),
    predicate: z.string(),
    object: z.string(),
  }),
  /** Similarity score (0-1) */
  score: z.number().min(0).max(1),
  /** Embedding vector */
  embedding: z.array(z.number()).optional(),
  /** Additional metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// AI-Enhanced Search Engine
// =============================================================================

/**
 * Creates an AI-enhanced semantic search engine for RDF knowledge graphs
 *
 * @param {Object} store - UNRDF store instance
 * @param {Object} config - Search configuration
 * @returns {Promise<Object>} Search engine instance
 *
 * @example
 * const core = await createKnowledgeSubstrateCore();
 * const searchEngine = await createAISearchEngine(core.store, {
 *   model: 'Xenova/all-MiniLM-L6-v2',
 *   topK: 5,
 *   threshold: 0.7
 * });
 *
 * const results = await searchEngine.search('machine learning algorithms');
 */
export async function createAISearchEngine(store, config = {}) {
  const validated = AISearchConfigSchema.parse(config);

  // Initialize embedding pipeline
  let embeddingPipeline = null;
  const embeddingCache = new Map();

  /**
   * Initialize the transformer model
   * @private
   */
  async function initModel() {
    if (!embeddingPipeline) {
      try {
        embeddingPipeline = await pipeline('feature-extraction', validated.model);
      } catch (error) {
        throw new Error(`Failed to load embedding model: ${error.message}`);
      }
    }
    return embeddingPipeline;
  }

  /**
   * Generate embedding for text
   *
   * @param {string} text - Text to embed
   * @returns {Promise<Array<number>>} Embedding vector
   */
  async function embed(text) {
    // Check cache
    if (validated.cache && embeddingCache.has(text)) {
      return embeddingCache.get(text);
    }

    const model = await initModel();

    // Generate embedding
    const output = await model(text, { pooling: 'mean', normalize: true });
    const embedding = Array.from(output.data);

    // Cache result
    if (validated.cache) {
      embeddingCache.set(text, embedding);
    }

    return embedding;
  }

  /**
   * Calculate cosine similarity between two vectors
   *
   * @param {Array<number>} a - First vector
   * @param {Array<number>} b - Second vector
   * @returns {number} Similarity score (0-1)
   */
  function cosineSimilarity(a, b) {
    if (a.length !== b.length) {
      throw new Error('Vectors must have same length');
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
   * Search knowledge graph using natural language query
   *
   * @param {string} queryText - Natural language query
   * @param {Object} options - Search options
   * @returns {Promise<Array<Object>>} Search results ranked by similarity
   */
  async function search(queryText, options = {}) {
    const opts = { ...validated, ...options };

    // Generate query embedding
    const queryEmbedding = await embed(queryText);

    // Fetch all triples from knowledge graph
    const sparqlQueryText = `
      SELECT ?s ?p ?o
      WHERE {
        ?s ?p ?o .
        FILTER(isLiteral(?o))
      }
      LIMIT 1000
    `;

    const triples = await sparqlQuery(store, sparqlQueryText);

    // Generate embeddings for each triple and calculate similarity
    const results = [];

    for (const binding of triples.bindings || []) {
      const tripleText = `${binding.s?.value || ''} ${binding.p?.value || ''} ${binding.o?.value || ''}`;
      const tripleEmbedding = await embed(tripleText);
      const score = cosineSimilarity(queryEmbedding, tripleEmbedding);

      if (score >= opts.threshold) {
        results.push({
          triple: {
            subject: binding.s?.value || '',
            predicate: binding.p?.value || '',
            object: binding.o?.value || '',
          },
          score,
          embedding: opts.includeEmbeddings ? tripleEmbedding : undefined,
        });
      }
    }

    // Sort by score descending and take top K
    results.sort((a, b) => b.score - a.score);
    const topResults = results.slice(0, opts.topK);

    return topResults.map((r) => SearchResultSchema.parse(r));
  }

  /**
   * Find similar triples to a given triple
   *
   * @param {Object} triple - Reference triple
   * @param {Object} options - Search options
   * @returns {Promise<Array<Object>>} Similar triples
   */
  async function findSimilar(triple, options = {}) {
    const tripleText = `${triple.subject} ${triple.predicate} ${triple.object}`;
    return search(tripleText, options);
  }

  /**
   * Cluster triples by semantic similarity
   *
   * @param {number} numClusters - Number of clusters to create
   * @returns {Promise<Array<Array<Object>>>} Clustered triples
   */
  async function cluster(numClusters = 5) {
    // Fetch all triples
    const sparqlQueryText = `
      SELECT ?s ?p ?o
      WHERE {
        ?s ?p ?o .
        FILTER(isLiteral(?o))
      }
      LIMIT 500
    `;

    const triples = await sparqlQuery(store, sparqlQueryText);

    // Generate embeddings for all triples
    const embeddings = [];
    const tripleData = [];

    for (const binding of triples.bindings || []) {
      const tripleText = `${binding.s?.value || ''} ${binding.p?.value || ''} ${binding.o?.value || ''}`;
      const embedding = await embed(tripleText);

      embeddings.push(embedding);
      tripleData.push({
        subject: binding.s?.value || '',
        predicate: binding.p?.value || '',
        object: binding.o?.value || '',
      });
    }

    // Simple k-means clustering (simplified implementation)
    const clusters = Array.from({ length: numClusters }, () => []);

    // Initialize centroids randomly
    const centroids = [];
    for (let i = 0; i < numClusters; i++) {
      const randomIdx = Math.floor(Math.random() * embeddings.length);
      centroids.push([...embeddings[randomIdx]]);
    }

    // Assign each triple to nearest centroid
    for (let i = 0; i < embeddings.length; i++) {
      let maxSim = -1;
      let bestCluster = 0;

      for (let j = 0; j < numClusters; j++) {
        const sim = cosineSimilarity(embeddings[i], centroids[j]);
        if (sim > maxSim) {
          maxSim = sim;
          bestCluster = j;
        }
      }

      clusters[bestCluster].push({
        triple: tripleData[i],
        embedding: embeddings[i],
      });
    }

    return clusters;
  }

  /**
   * Get embedding statistics
   *
   * @returns {Object} Cache statistics
   */
  function getStats() {
    return {
      cacheSize: embeddingCache.size,
      modelLoaded: embeddingPipeline !== null,
      config: validated,
    };
  }

  /**
   * Clear embedding cache
   */
  function clearCache() {
    embeddingCache.clear();
  }

  return {
    search,
    findSimilar,
    cluster,
    embed,
    getStats,
    clearCache,
  };
}

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Perform a quick semantic search without creating an engine instance
 *
 * @param {Object} store - UNRDF store instance
 * @param {string} query - Natural language query
 * @param {Object} options - Search options
 * @returns {Promise<Array<Object>>} Search results
 *
 * @example
 * const results = await semanticSearch(store, 'artificial intelligence', {
 *   topK: 5,
 *   threshold: 0.7
 * });
 */
export async function semanticSearch(store, query, options = {}) {
  const engine = await createAISearchEngine(store, options);
  const results = await engine.search(query);
  engine.clearCache();
  return results;
}

/**
 * Generate embeddings for multiple texts in batch
 *
 * @param {Array<string>} texts - Array of texts to embed
 * @param {Object} config - Configuration options
 * @returns {Promise<Array<Array<number>>>} Array of embeddings
 *
 * @example
 * const embeddings = await batchEmbed([
 *   'machine learning',
 *   'artificial intelligence',
 *   'neural networks'
 * ]);
 */
export async function batchEmbed(texts, config = {}) {
  const validated = AISearchConfigSchema.parse(config);
  const model = await pipeline('feature-extraction', validated.model);

  const embeddings = [];

  // Process in batches
  for (let i = 0; i < texts.length; i += validated.batchSize) {
    const batch = texts.slice(i, i + validated.batchSize);

    for (const text of batch) {
      const output = await model(text, { pooling: 'mean', normalize: true });
      embeddings.push(Array.from(output.data));
    }
  }

  return embeddings;
}
