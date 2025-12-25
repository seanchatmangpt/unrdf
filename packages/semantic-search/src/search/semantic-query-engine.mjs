import { RDFEmbedder } from '../embeddings/rdf-embedder.mjs';
import { LocalIndex } from 'vectra';
import { z } from 'zod';

/**
 * Zod schemas for validation
 */
const QueryOptionsSchema = z.object({
  limit: z.number().int().positive().default(10),
  threshold: z.number().min(0).max(1).default(0.5),
  hybridWeight: z.number().min(0).max(1).default(0.7),
}).partial();

/**
 * SemanticQueryEngine - Natural language queries over RDF knowledge graphs
 * Combines vector similarity search with SPARQL queries
 */
export class SemanticQueryEngine {
  /**
   * @param {Object} store - Oxigraph RDF store
   * @param {Object} options - Engine configuration
   */
  constructor(store, options = {}) {
    if (!store) {
      throw new Error('RDF store is required');
    }

    this.store = store;
    this.embedder = new RDFEmbedder(options.embedder || {});
    this.index = null;
    this.indexed = false;
    this.tripleCache = [];
  }

  /**
   * Initialize the query engine and build vector index
   * @returns {Promise<void>}
   */
  async initialize() {
    await this.embedder.initialize();

    // Create in-memory vector index
    this.index = new LocalIndex();
    await this.index.createIndex();

    this.indexed = false;
  }

  /**
   * Index all triples in the RDF store
   * @returns {Promise<number>} Number of indexed triples
   */
  async indexStore() {
    if (!this.index) {
      await this.initialize();
    }

    // Get all triples from store
    const triples = this.store.match(null, null, null);
    this.tripleCache = triples;

    // Generate embeddings and add to index
    for (let i = 0; i < triples.length; i++) {
      const triple = triples[i];
      const embedding = await this.embedder.embedTriple(triple);

      await this.index.insertItem({
        id: `triple-${i}`,
        vector: embedding,
        metadata: {
          subject: triple.subject.value,
          predicate: triple.predicate.value,
          object: triple.object.value,
        },
      });
    }

    this.indexed = true;
    return triples.length;
  }

  /**
   * Perform semantic search using natural language query
   * @param {string} query - Natural language query
   * @param {Object} options - Search options
   * @param {number} [options.limit=10] - Maximum results
   * @param {number} [options.threshold=0.5] - Similarity threshold
   * @returns {Promise<Array>} Search results with scores
   */
  async search(query, options = {}) {
    const config = QueryOptionsSchema.parse(options);

    if (!this.indexed) {
      await this.indexStore();
    }

    // Generate query embedding
    const queryEmbedding = await this.embedder.embedText(query);

    // Search vector index
    const results = await this.index.queryItems(queryEmbedding, config.limit);

    // Filter by threshold and format results
    return results
      .filter(result => result.score >= config.threshold)
      .map(result => ({
        triple: result.item.metadata,
        score: result.score,
        text: this.embedder.tripleToText({
          subject: { value: result.item.metadata.subject },
          predicate: { value: result.item.metadata.predicate },
          object: { value: result.item.metadata.object },
        }),
      }));
  }

  /**
   * Hybrid search combining semantic and SPARQL
   * @param {string} nlQuery - Natural language query
   * @param {string} sparqlPattern - Optional SPARQL WHERE pattern
   * @param {Object} options - Search options
   * @returns {Promise<Array>} Combined results
   */
  async hybridSearch(nlQuery, sparqlPattern = null, options = {}) {
    const config = QueryOptionsSchema.parse(options);

    // Semantic search
    const semanticResults = await this.search(nlQuery, {
      limit: config.limit * 2,
      threshold: config.threshold * 0.8, // Lower threshold for hybrid
    });

    if (!sparqlPattern) {
      return semanticResults.slice(0, config.limit);
    }

    // SPARQL filtering
    const sparqlQuery = `SELECT ?s ?p ?o WHERE { ${sparqlPattern} }`;
    const sparqlResults = this.store.query(sparqlQuery);

    // Create lookup set for SPARQL results
    const sparqlSet = new Set(
      sparqlResults.map(r =>
        `${r.s.value}|${r.p.value}|${r.o.value}`
      )
    );

    // Combine results with hybrid weighting
    const hybrid = semanticResults.map(result => {
      const key = `${result.triple.subject}|${result.triple.predicate}|${result.triple.object}`;
      const inSparql = sparqlSet.has(key);

      // Hybrid score: weighted combination
      const hybridScore = inSparql
        ? result.score * config.hybridWeight + (1 - config.hybridWeight)
        : result.score * config.hybridWeight;

      return {
        ...result,
        hybridScore,
        matchesSparql: inSparql,
      };
    });

    // Sort by hybrid score and return top results
    return hybrid
      .sort((a, b) => b.hybridScore - a.hybridScore)
      .slice(0, config.limit);
  }

  /**
   * Find similar triples to a given triple
   * @param {Object} triple - Reference triple
   * @param {number} limit - Maximum results
   * @returns {Promise<Array>} Similar triples
   */
  async findSimilar(triple, limit = 10) {
    if (!this.indexed) {
      await this.indexStore();
    }

    const embedding = await this.embedder.embedTriple(triple);
    const results = await this.index.queryItems(embedding, limit + 1);

    // Filter out the query triple itself
    return results
      .filter(r => {
        const meta = r.item.metadata;
        return !(
          meta.subject === triple.subject.value &&
          meta.predicate === triple.predicate.value &&
          meta.object === triple.object.value
        );
      })
      .slice(0, limit)
      .map(result => ({
        triple: result.item.metadata,
        score: result.score,
      }));
  }

  /**
   * Auto-complete query suggestions
   * @param {string} partialQuery - Partial query text
   * @param {number} limit - Maximum suggestions
   * @returns {Promise<Array<string>>} Query suggestions
   */
  async autocomplete(partialQuery, limit = 5) {
    if (!this.indexed) {
      await this.indexStore();
    }

    const results = await this.search(partialQuery, { limit, threshold: 0.3 });

    // Extract unique subjects and objects as suggestions
    const suggestions = new Set();

    results.forEach(result => {
      const subjectLabel = this.embedder.extractLabel(result.triple.subject);
      const objectLabel = this.embedder.extractLabel(result.triple.object);

      suggestions.add(subjectLabel);
      suggestions.add(objectLabel);
    });

    return Array.from(suggestions).slice(0, limit);
  }

  /**
   * Get engine statistics
   * @returns {Object} Engine stats
   */
  getStats() {
    return {
      indexed: this.indexed,
      tripleCount: this.tripleCache.length,
      cacheStats: this.embedder.getCacheStats(),
      storeSize: this.store.size,
    };
  }
}

export default SemanticQueryEngine;
