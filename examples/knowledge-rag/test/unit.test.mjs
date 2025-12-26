/**
 * @file Unit Tests
 * @description Unit tests for knowledge RAG components
 */

import { describe, it, expect } from 'vitest';
import { SemanticSearch } from '../src/semantic-search.mjs';
import { QueryExpander } from '../src/query-expansion.mjs';

describe('SemanticSearch', () => {
  it('should create search engine', () => {
    const search = new SemanticSearch({ dimensions: 768 });
    expect(search).toBeDefined();
    expect(search.config.dimensions).toBe(768);
  });

  it('should generate embeddings', async () => {
    const search = new SemanticSearch();
    const embedding = await search.generateEmbedding('test query');

    expect(Array.isArray(embedding)).toBe(true);
    expect(embedding.length).toBe(1536);
  });

  it('should cache embeddings', async () => {
    const search = new SemanticSearch();

    await search.generateEmbedding('test');
    await search.generateEmbedding('test');

    const stats = search.getCacheStats();
    expect(stats.hits).toBe(1);
    expect(stats.misses).toBe(1);
  });
});

describe('QueryExpander', () => {
  it('should expand query with synonyms', async () => {
    const expander = new QueryExpander();
    const result = await expander.expand('person works at company');

    expect(result.tokens).toContain('person');
    expect(result.tokens).toContain('works');
    expect(result.expandedTerms.length).toBeGreaterThanOrEqual(result.tokens.length);
  });

  it('should build SPARQL query', async () => {
    const expander = new QueryExpander();
    const result = await expander.expand('test query');

    expect(result.sparql).toContain('SELECT');
    expect(result.sparql).toContain('WHERE');
  });
});
