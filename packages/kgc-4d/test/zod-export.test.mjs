/**
 * @file Test Zod schema exports from @unrdf/kgc-4d
 * @description Verifies that all Zod schemas are properly exported and usable
 */

import { describe, it, expect } from 'vitest';
import {
  DeltaSchema,
  SerializedDeltaSchema,
  guardDeltaValid,
  guardSerializedDeltaValid,
  TemporalQuerySchema,
  TemporalCacheConfigSchema,
  TemporalResultMetadataSchema,
  TemporalResultSchema,
  TimeRangeResultSchema,
  TemporalEngineOptionsSchema,
  guardTemporalQueryValid,
  guardTemporalResultValid,
  guardCacheConfigValid,
} from '../src/index.mjs';

describe('Zod Schema Exports', () => {
  describe('Delta Schemas', () => {
    it('should export DeltaSchema', () => {
      expect(DeltaSchema).toBeDefined();
      expect(typeof DeltaSchema).toBe('object');
      expect(DeltaSchema.parse).toBeDefined();
    });

    it('should export SerializedDeltaSchema', () => {
      expect(SerializedDeltaSchema).toBeDefined();
      expect(typeof SerializedDeltaSchema).toBe('object');
      expect(SerializedDeltaSchema.parse).toBeDefined();
    });

    it('should export guardDeltaValid', () => {
      expect(guardDeltaValid).toBeDefined();
      expect(typeof guardDeltaValid).toBe('function');
    });

    it('should export guardSerializedDeltaValid', () => {
      expect(guardSerializedDeltaValid).toBeDefined();
      expect(typeof guardSerializedDeltaValid).toBe('function');
    });

    it('should validate a valid delta', () => {
      const delta = {
        type: 'add',
        subject: 'http://example.org/subject',
        subjectType: 'NamedNode',
        predicate: 'http://example.org/predicate',
        object: {
          value: 'test value',
          type: 'Literal',
        },
      };

      const result = guardDeltaValid(delta);
      expect(result).toBeDefined();
      expect(result.type).toBe('add');
    });

    it('should validate a serialized delta', () => {
      const delta = {
        type: 'delete',
        subject: 'http://example.org/subject',
        subjectType: 'NamedNode',
        predicate: 'http://example.org/predicate',
        object: {
          value: 'test value',
          type: 'Literal',
        },
      };

      const result = guardSerializedDeltaValid(delta);
      expect(result).toBeDefined();
      expect(result.type).toBe('delete');
    });
  });

  describe('Temporal SPARQL Schemas', () => {
    it('should export TemporalQuerySchema', () => {
      expect(TemporalQuerySchema).toBeDefined();
      expect(typeof TemporalQuerySchema).toBe('object');
    });

    it('should export TemporalCacheConfigSchema', () => {
      expect(TemporalCacheConfigSchema).toBeDefined();
      expect(typeof TemporalCacheConfigSchema).toBe('object');
    });

    it('should export TemporalResultMetadataSchema', () => {
      expect(TemporalResultMetadataSchema).toBeDefined();
      expect(typeof TemporalResultMetadataSchema).toBe('object');
    });

    it('should export TemporalResultSchema', () => {
      expect(TemporalResultSchema).toBeDefined();
      expect(typeof TemporalResultSchema).toBe('object');
    });

    it('should export TimeRangeResultSchema', () => {
      expect(TimeRangeResultSchema).toBeDefined();
      expect(typeof TimeRangeResultSchema).toBe('object');
    });

    it('should export TemporalEngineOptionsSchema', () => {
      expect(TemporalEngineOptionsSchema).toBeDefined();
      expect(typeof TemporalEngineOptionsSchema).toBe('object');
    });

    it('should export guardTemporalQueryValid', () => {
      expect(guardTemporalQueryValid).toBeDefined();
      expect(typeof guardTemporalQueryValid).toBe('function');
    });

    it('should export guardTemporalResultValid', () => {
      expect(guardTemporalResultValid).toBeDefined();
      expect(typeof guardTemporalResultValid).toBe('function');
    });

    it('should export guardCacheConfigValid', () => {
      expect(guardCacheConfigValid).toBeDefined();
      expect(typeof guardCacheConfigValid).toBe('function');
    });

    it('should validate a current query', () => {
      const query = {
        mode: 'current',
        baseSparql: 'SELECT * WHERE { ?s ?p ?o }',
        originalQuery: 'SELECT * WHERE { ?s ?p ?o }',
      };

      const result = guardTemporalQueryValid(query);
      expect(result).toBeDefined();
      expect(result.mode).toBe('current');
    });

    it('should validate cache config', () => {
      const config = {
        maxSize: 500,
        ttl: 600000,
        enabled: true,
      };

      const result = guardCacheConfigValid(config);
      expect(result).toBeDefined();
      expect(result.maxSize).toBe(500);
    });
  });
});
