import { describe, it, expect, beforeEach } from 'vitest';
import { RDFEmbedder } from '../src/embeddings/rdf-embedder.mjs';

describe('RDFEmbedder', () => {
  let embedder;

  beforeEach(() => {
    embedder = new RDFEmbedder({ cache: true });
  });

  describe('Initialization', () => {
    it('should create embedder with default options', () => {
      expect(embedder).toBeDefined();
      expect(embedder.model).toBe('Xenova/all-MiniLM-L6-v2');
      expect(embedder.dimension).toBe(384);
    });

    it('should initialize transformer model', async () => {
      await embedder.initialize();
      expect(embedder.embedder).toBeDefined();
    }, 30000);
  });

  describe('Triple to Text Conversion', () => {
    it('should convert triple to readable text', () => {
      const triple = {
        subject: { value: 'http://example.org/Person' },
        predicate: { value: 'http://schema.org/name' },
        object: { value: 'John Doe' },
      };

      const text = embedder.tripleToText(triple);
      expect(text).toBe('person name John Doe');
    });

    it('should handle camelCase URIs', () => {
      const triple = {
        subject: { value: 'http://example.org/PersonEntity' },
        predicate: { value: 'http://example.org/hasProperty' },
        object: { value: 'http://example.org/PropertyValue' },
      };

      const text = embedder.tripleToText(triple);
      expect(text).toContain('person entity');
      expect(text).toContain('has property');
    });
  });

  describe('Label Extraction', () => {
    it('should extract label from HTTP URI', () => {
      const label = embedder.extractLabel('http://example.org/SomeThing');
      expect(label).toBe('some thing');
    });

    it('should extract label from HTTPS URI', () => {
      const label = embedder.extractLabel('https://schema.org/Person');
      expect(label).toBe('person');
    });

    it('should handle literal values', () => {
      const label = embedder.extractLabel('John Doe');
      expect(label).toBe('John Doe');
    });

    it('should handle fragment identifiers', () => {
      const label = embedder.extractLabel('http://example.org#PersonName');
      expect(label).toBe('person name');
    });
  });

  describe('Text Embedding', () => {
    it('should generate embedding for text', async () => {
      const text = 'programming language';
      const embedding = await embedder.embedText(text);

      expect(Array.isArray(embedding)).toBe(true);
      expect(embedding.length).toBe(384);
      expect(embedding.every(v => typeof v === 'number')).toBe(true);
    }, 30000);

    it('should cache embeddings', async () => {
      const text = 'test text';

      const embedding1 = await embedder.embedText(text);
      const embedding2 = await embedder.embedText(text);

      // Should return same cached instance
      expect(embedding1).toBe(embedding2);

      const stats = embedder.getCacheStats();
      expect(stats.size).toBeGreaterThan(0);
    }, 30000);
  });

  describe('Triple Embedding', () => {
    it('should generate embedding for triple', async () => {
      const triple = {
        subject: { value: 'http://example.org/JavaScript' },
        predicate: { value: 'http://schema.org/description' },
        object: { value: 'programming language' },
      };

      const embedding = await embedder.embedTriple(triple);

      expect(Array.isArray(embedding)).toBe(true);
      expect(embedding.length).toBe(384);
    }, 30000);
  });

  describe('Batch Embedding', () => {
    it('should embed multiple triples', async () => {
      const triples = [
        {
          subject: { value: 'http://example.org/A' },
          predicate: { value: 'http://example.org/type' },
          object: { value: 'Thing' },
        },
        {
          subject: { value: 'http://example.org/B' },
          predicate: { value: 'http://example.org/type' },
          object: { value: 'Entity' },
        },
      ];

      const embeddings = await embedder.embedTriples(triples);

      expect(embeddings.length).toBe(2);
      expect(embeddings[0].length).toBe(384);
      expect(embeddings[1].length).toBe(384);
    }, 30000);

    it('should throw on invalid input', async () => {
      await expect(embedder.embedTriples('not an array')).rejects.toThrow();
    });
  });

  describe('Entity Embedding', () => {
    it('should aggregate embeddings for entity', async () => {
      const triples = [
        {
          subject: { value: 'http://example.org/Entity' },
          predicate: { value: 'http://schema.org/name' },
          object: { value: 'Test' },
        },
        {
          subject: { value: 'http://example.org/Entity' },
          predicate: { value: 'http://schema.org/description' },
          object: { value: 'Description' },
        },
      ];

      const embedding = await embedder.embedEntity(triples);

      expect(Array.isArray(embedding)).toBe(true);
      expect(embedding.length).toBe(384);

      // Should be normalized
      const norm = Math.sqrt(embedding.reduce((sum, v) => sum + v * v, 0));
      expect(norm).toBeCloseTo(1, 5);
    }, 30000);
  });

  describe('Cache Management', () => {
    it('should clear cache', async () => {
      await embedder.embedText('test');

      expect(embedder.getCacheStats().size).toBeGreaterThan(0);

      embedder.clearCache();
      expect(embedder.getCacheStats().size).toBe(0);
    }, 30000);

    it('should provide cache statistics', () => {
      const stats = embedder.getCacheStats();

      expect(stats).toHaveProperty('enabled');
      expect(stats).toHaveProperty('size');
      expect(stats).toHaveProperty('dimension');
      expect(stats).toHaveProperty('model');
    });
  });

  describe('Error Handling', () => {
    it('should validate triple schema', () => {
      const invalidTriple = {
        subject: { value: 123 }, // Invalid: number instead of string
        predicate: { value: 'test' },
        object: { value: 'test' },
      };

      expect(() => embedder.tripleToText(invalidTriple)).toThrow();
    });

    it('should handle missing triple fields', () => {
      const invalidTriple = {
        subject: { value: 'test' },
        // Missing predicate and object
      };

      expect(() => embedder.tripleToText(invalidTriple)).toThrow();
    });
  });
});
