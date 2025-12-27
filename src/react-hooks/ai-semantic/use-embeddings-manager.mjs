/**
 * @file use-embeddings-manager.mjs
 * @description React hook for managing vector embeddings for semantic search
 */

import { useState, useCallback, _useEffect } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for generating and managing vector embeddings for entities
 *
 * @param {Object} config - Embeddings manager configuration
 * @param {string} [config.model='text-embedding-ada-002'] - Embedding model
 * @param {number} [config.dimensions=1536] - Embedding dimensions
 * @returns {Object} Embeddings manager state and operations
 *
 * @example
 * const {
 *   generateEmbedding,
 *   searchSimilar,
 *   embeddings,
 *   indexEntity
 * } = useEmbeddingsManager({
 *   model: 'text-embedding-ada-002',
 *   dimensions: 1536
 * });
 */
export function useEmbeddingsManager(config = {}) {
  const { _engine } = useKnowledgeEngineContext();
  const [embeddings, setEmbeddings] = useState(new Map());
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const generateEmbedding = useCallback(
    async _text => {
      try {
        setLoading(true);
        // Mock embedding generation (would use actual model in production)
        const embedding = Array.from({ length: config.dimensions || 1536 }, () => Math.random());
        setLoading(false);
        return embedding;
      } catch (err) {
        setError(err);
        setLoading(false);
        throw err;
      }
    },
    [config.dimensions]
  );

  const indexEntity = useCallback(
    async (entityUri, properties) => {
      try {
        setLoading(true);
        const text = Object.values(properties).join(' ');
        const embedding = await generateEmbedding(text);

        setEmbeddings(prev =>
          new Map(prev).set(entityUri, {
            embedding,
            properties,
            timestamp: new Date().toISOString(),
          })
        );

        setLoading(false);
        return { entityUri, embedding };
      } catch (err) {
        setError(err);
        setLoading(false);
        throw err;
      }
    },
    [generateEmbedding]
  );

  const searchSimilar = useCallback(
    async (queryText, topK = 10) => {
      try {
        setLoading(true);
        const queryEmbedding = await generateEmbedding(queryText);

        const similarities = Array.from(embeddings.entries()).map(([uri, data]) => ({
          uri,
          similarity: cosineSimilarity(queryEmbedding, data.embedding),
          properties: data.properties,
        }));

        const results = similarities.sort((a, b) => b.similarity - a.similarity).slice(0, topK);

        setLoading(false);
        return results;
      } catch (err) {
        setError(err);
        setLoading(false);
        throw err;
      }
    },
    [embeddings, generateEmbedding]
  );

  function cosineSimilarity(a, b) {
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

  return {
    generateEmbedding,
    indexEntity,
    searchSimilar,
    embeddings,
    loading,
    error,
  };
}
