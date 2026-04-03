/**
 * Vector Database Integration Layer
 * High-performance semantic search + RAG support
 */

export class VectorDB {
  constructor() {
    this.collections = new Map();
    this.indexes = new Map();
  }

  /**
   * Create vector collection
   */
  createCollection(name, options = {}) {
    const collection = {
      name,
      dimension: options.dimension || 384,
      distance: options.distance || 'cosine',
      vectors: [],
      metadata: new Map(),
      created: new Date()
    };
    this.collections.set(name, collection);
    return collection;
  }

  /**
   * Insert vector with metadata
   */
  async insert(collectionName, vector, metadata = {}) {
    const collection = this.collections.get(collectionName);
    if (!collection) throw new Error(`Collection not found: ${collectionName}`);

    const id = `vec_${collection.vectors.length}_${Date.now()}`;
    const entry = {
      id,
      vector,
      metadata,
      inserted: new Date()
    };

    collection.vectors.push(entry);
    collection.metadata.set(id, metadata);

    return { id, ...entry };
  }

  /**
   * Semantic search (vector similarity)
   */
  async search(collectionName, queryVector, limit = 10, threshold = 0.7) {
    const collection = this.collections.get(collectionName);
    if (!collection) throw new Error(`Collection not found: ${collectionName}`);

    const results = collection.vectors
      .map(entry => ({
        ...entry,
        similarity: this._cosineSimilarity(queryVector, entry.vector)
      }))
      .filter(r => r.similarity >= threshold)
      .sort((a, b) => b.similarity - a.similarity)
      .slice(0, limit);

    return {
      query_dimension: queryVector.length,
      results_count: results.length,
      results,
      collectionName,
      timestamp: new Date()
    };
  }

  _cosineSimilarity(a, b) {
    const dotProduct = a.reduce((sum, val, i) => sum + val * b[i], 0);
    const normA = Math.sqrt(a.reduce((sum, val) => sum + val * val, 0));
    const normB = Math.sqrt(b.reduce((sum, val) => sum + val * val, 0));
    return dotProduct / (normA * normB);
  }

  /**
   * Batch insert vectors
   */
  async batchInsert(collectionName, vectors) {
    const results = [];
    for (const { vector, metadata } of vectors) {
      results.push(await this.insert(collectionName, vector, metadata));
    }
    return results;
  }

  /**
   * Get collection statistics
   */
  getStats(collectionName) {
    const collection = this.collections.get(collectionName);
    if (!collection) return null;

    return {
      name: collection.name,
      dimension: collection.dimension,
      count: collection.vectors.length,
      distance_metric: collection.distance,
      created: collection.created
    };
  }
}

export default VectorDB;
