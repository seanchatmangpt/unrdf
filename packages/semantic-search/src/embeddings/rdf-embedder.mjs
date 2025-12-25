import { pipeline } from '@xenova/transformers';
import { z } from 'zod';

/**
 * Zod schemas for validation
 */
const TripleSchema = z.object({
  subject: z.object({ value: z.string() }),
  predicate: z.object({ value: z.string() }),
  object: z.object({ value: z.string() }),
});

const EmbeddingOptionsSchema = z.object({
  model: z.string().default('Xenova/all-MiniLM-L6-v2'),
  pooling: z.enum(['mean', 'cls']).default('mean'),
  normalize: z.boolean().default(true),
  cache: z.boolean().default(true),
}).partial();

/**
 * RDFEmbedder - Convert RDF triples to vector embeddings
 * Uses transformer models for semantic representation
 */
export class RDFEmbedder {
  /**
   * @param {Object} options - Embedder configuration
   * @param {string} [options.model='Xenova/all-MiniLM-L6-v2'] - HuggingFace model ID
   * @param {string} [options.pooling='mean'] - Pooling strategy
   * @param {boolean} [options.normalize=true] - Normalize embeddings
   * @param {boolean} [options.cache=true] - Cache embeddings
   */
  constructor(options = {}) {
    const config = EmbeddingOptionsSchema.parse(options);
    this.model = config.model;
    this.pooling = config.pooling;
    this.normalize = config.normalize;
    this.cache = config.cache ? new Map() : null;
    this.embedder = null;
    this.dimension = 384; // MiniLM-L6-v2 embedding dimension
  }

  /**
   * Initialize the transformer model
   * @returns {Promise<void>}
   */
  async initialize() {
    if (!this.embedder) {
      this.embedder = await pipeline('feature-extraction', this.model);
    }
  }

  /**
   * Convert RDF triple to text representation
   * @param {Object} triple - RDF triple (subject, predicate, object)
   * @returns {string} Text representation
   */
  tripleToText(triple) {
    TripleSchema.parse(triple);

    const subject = this.extractLabel(triple.subject.value);
    const predicate = this.extractLabel(triple.predicate.value);
    const object = this.extractLabel(triple.object.value);

    return `${subject} ${predicate} ${object}`;
  }

  /**
   * Extract human-readable label from URI or literal
   * @param {string} value - URI or literal value
   * @returns {string} Readable label
   */
  extractLabel(value) {
    // Handle URIs - extract local name
    if (value.startsWith('http://') || value.startsWith('https://')) {
      const parts = value.split(/[/#]/);
      const label = parts[parts.length - 1];
      // Convert camelCase/PascalCase to spaces
      return label.replace(/([a-z])([A-Z])/g, '$1 $2').toLowerCase();
    }
    return value;
  }

  /**
   * Generate embedding for a single text
   * @param {string} text - Text to embed
   * @returns {Promise<number[]>} Embedding vector
   */
  async embedText(text) {
    if (!this.embedder) {
      await this.initialize();
    }

    // Check cache
    if (this.cache && this.cache.has(text)) {
      return this.cache.get(text);
    }

    const output = await this.embedder(text, {
      pooling: this.pooling,
      normalize: this.normalize
    });

    const embedding = Array.from(output.data);

    // Cache result
    if (this.cache) {
      this.cache.set(text, embedding);
    }

    return embedding;
  }

  /**
   * Generate embedding for RDF triple
   * @param {Object} triple - RDF triple
   * @returns {Promise<number[]>} Embedding vector
   */
  async embedTriple(triple) {
    const text = this.tripleToText(triple);
    return this.embedText(text);
  }

  /**
   * Generate embeddings for multiple triples in batch
   * @param {Array<Object>} triples - Array of RDF triples
   * @returns {Promise<Array<number[]>>} Array of embedding vectors
   */
  async embedTriples(triples) {
    if (!Array.isArray(triples)) {
      throw new Error('Input must be an array of triples');
    }

    const texts = triples.map(t => this.tripleToText(t));
    return Promise.all(texts.map(text => this.embedText(text)));
  }

  /**
   * Generate embedding for entity (all triples about entity)
   * @param {Array<Object>} triples - Triples describing entity
   * @returns {Promise<number[]>} Aggregated embedding
   */
  async embedEntity(triples) {
    const embeddings = await this.embedTriples(triples);

    // Average pooling over all triple embeddings
    const aggregated = new Array(this.dimension).fill(0);

    for (const embedding of embeddings) {
      for (let i = 0; i < this.dimension; i++) {
        aggregated[i] += embedding[i];
      }
    }

    // Normalize
    const norm = Math.sqrt(aggregated.reduce((sum, val) => sum + val * val, 0));
    return aggregated.map(val => val / norm);
  }

  /**
   * Clear embedding cache
   */
  clearCache() {
    if (this.cache) {
      this.cache.clear();
    }
  }

  /**
   * Get cache statistics
   * @returns {Object} Cache stats
   */
  getCacheStats() {
    return {
      enabled: !!this.cache,
      size: this.cache ? this.cache.size : 0,
      dimension: this.dimension,
      model: this.model,
    };
  }
}

export default RDFEmbedder;
