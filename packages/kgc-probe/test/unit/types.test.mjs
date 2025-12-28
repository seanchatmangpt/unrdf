/**
 * @fileoverview Unit Tests for Types Module
 *
 * Tests Zod schemas and type validation functions.
 *
 * @module @unrdf/kgc-probe/test/unit/types
 */

import { describe, it, expect } from 'vitest';
import {
  ObservationSchema,
  ArtifactSchema,
  ProbeConfigSchema,
  GuardConfigSchema,
  StorageConfigSchema,
  GuardViolationSchema,
  DiffResultSchema,
  validateObservation,
  validateArtifact,
  validateProbeConfig,
  tryValidateObservation
} from '../../src/types.mjs';
import { FROZEN_TIMESTAMP } from '../fixtures/frozen-environment.mjs';

describe('Types Module', () => {
  describe('ObservationSchema', () => {
    it('should validate correct observation', () => {
      const observation = {
        id: '550e8400-e29b-41d4-a716-446655440000',
        agent: 'completion',
        timestamp: FROZEN_TIMESTAMP,
        kind: 'completeness',
        severity: 'warning',
        subject: 'example:entity1',
        predicate: 'rdf:type',
        object: 'example:Person',
        evidence: {
          query: 'SELECT ?s WHERE { ?s ?p ?o }',
          result: { count: 10 },
          witnesses: ['example:entity1']
        },
        metrics: {
          confidence: 0.95,
          coverage: 0.85,
          latency_ms: 100
        },
        tags: ['test']
      };

      const result = ObservationSchema.safeParse(observation);
      expect(result.success).toBe(true);
    });

    it('should reject observation without required fields', () => {
      const incomplete = {
        id: '550e8400-e29b-41d4-a716-446655440000',
        agent: 'test'
        // Missing required fields
      };

      const result = ObservationSchema.safeParse(incomplete);
      expect(result.success).toBe(false);
    });

    it('should reject invalid UUID', () => {
      const invalidId = {
        id: 'not-a-uuid',
        agent: 'test',
        timestamp: FROZEN_TIMESTAMP,
        kind: 'completeness',
        severity: 'info',
        subject: 'test',
        evidence: { query: 'test', result: {}, witnesses: [] },
        metrics: { confidence: 0.5, coverage: 0.5, latency_ms: 10 }
      };

      const result = ObservationSchema.safeParse(invalidId);
      expect(result.success).toBe(false);
    });

    it('should reject invalid kind', () => {
      const invalidKind = {
        id: '550e8400-e29b-41d4-a716-446655440000',
        agent: 'test',
        timestamp: FROZEN_TIMESTAMP,
        kind: 'invalid_kind',  // Not in enum
        severity: 'info',
        subject: 'test',
        evidence: { query: 'test', result: {}, witnesses: [] },
        metrics: { confidence: 0.5, coverage: 0.5, latency_ms: 10 }
      };

      const result = ObservationSchema.safeParse(invalidKind);
      expect(result.success).toBe(false);
    });

    it('should reject confidence outside [0,1] range', () => {
      const invalidConfidence = {
        id: '550e8400-e29b-41d4-a716-446655440000',
        agent: 'test',
        timestamp: FROZEN_TIMESTAMP,
        kind: 'completeness',
        severity: 'info',
        subject: 'test',
        evidence: { query: 'test', result: {}, witnesses: [] },
        metrics: { confidence: 1.5, coverage: 0.5, latency_ms: 10 }
      };

      const result = ObservationSchema.safeParse(invalidConfidence);
      expect(result.success).toBe(false);
    });

    it('should accept all valid severity levels', () => {
      for (const severity of ['critical', 'warning', 'info']) {
        const obs = {
          id: '550e8400-e29b-41d4-a716-446655440000',
          agent: 'test',
          timestamp: FROZEN_TIMESTAMP,
          kind: 'completeness',
          severity,
          subject: 'test',
          evidence: { query: 'test', result: {}, witnesses: [] },
          metrics: { confidence: 0.5, coverage: 0.5, latency_ms: 10 }
        };

        const result = ObservationSchema.safeParse(obs);
        expect(result.success).toBe(true);
      }
    });

    it('should accept all valid kinds', () => {
      const validKinds = [
        'completeness', 'consistency', 'conformance', 'coverage',
        'caching', 'completeness_level', 'coherence', 'clustering',
        'classification', 'collaboration'
      ];

      for (const kind of validKinds) {
        const obs = {
          id: '550e8400-e29b-41d4-a716-446655440000',
          agent: 'test',
          timestamp: FROZEN_TIMESTAMP,
          kind,
          severity: 'info',
          subject: 'test',
          evidence: { query: 'test', result: {}, witnesses: [] },
          metrics: { confidence: 0.5, coverage: 0.5, latency_ms: 10 }
        };

        const result = ObservationSchema.safeParse(obs);
        expect(result.success).toBe(true);
      }
    });
  });

  describe('ProbeConfigSchema', () => {
    it('should validate minimal config', () => {
      const config = {
        universe_id: 'test-universe'
      };

      const result = ProbeConfigSchema.safeParse(config);
      expect(result.success).toBe(true);
    });

    it('should validate full config', () => {
      const config = {
        universe_id: 'test-universe',
        snapshot_id: 'snap-001',
        agents: ['completion', 'consistency'],
        guards: ['quality_check'],
        distributed: true,
        persist: false,
        timeout_ms: 60000,
        batch_size: 50
      };

      const result = ProbeConfigSchema.safeParse(config);
      expect(result.success).toBe(true);
    });

    it('should apply defaults', () => {
      const config = {
        universe_id: 'test-universe'
      };

      const result = ProbeConfigSchema.parse(config);
      expect(result.distributed).toBe(false);
      expect(result.persist).toBe(true);
      expect(result.timeout_ms).toBe(300000);
      expect(result.batch_size).toBe(100);
    });
  });

  describe('StorageConfigSchema', () => {
    it('should validate memory storage config', () => {
      const config = {
        type: 'memory'
      };

      const result = StorageConfigSchema.safeParse(config);
      expect(result.success).toBe(true);
    });

    it('should validate file storage config', () => {
      const config = {
        type: 'file',
        path: './artifacts'
      };

      const result = StorageConfigSchema.safeParse(config);
      expect(result.success).toBe(true);
    });

    it('should validate database storage config', () => {
      const config = {
        type: 'database',
        connectionString: 'postgresql://localhost/db'
      };

      const result = StorageConfigSchema.safeParse(config);
      expect(result.success).toBe(true);
    });

    it('should reject invalid type', () => {
      const config = {
        type: 'invalid'
      };

      const result = StorageConfigSchema.safeParse(config);
      expect(result.success).toBe(false);
    });
  });

  describe('Validation Functions', () => {
    it('should validate observation and return parsed data', () => {
      const obs = {
        id: '550e8400-e29b-41d4-a716-446655440000',
        agent: 'test',
        timestamp: FROZEN_TIMESTAMP,
        kind: 'completeness',
        severity: 'info',
        subject: 'test',
        evidence: { query: 'test', result: {}, witnesses: [] },
        metrics: { confidence: 0.5, coverage: 0.5, latency_ms: 10 }
      };

      const result = validateObservation(obs);
      expect(result).toBeDefined();
      expect(result.id).toBe(obs.id);
    });

    it('should throw on invalid observation', () => {
      expect(() => validateObservation({})).toThrow();
    });

    it('should validate probe config', () => {
      const config = { universe_id: 'test' };
      const result = validateProbeConfig(config);
      expect(result.universe_id).toBe('test');
    });

    it('should return null for invalid observation with tryValidate', () => {
      const result = tryValidateObservation({});
      expect(result).toBeNull();
    });

    it('should return observation for valid data with tryValidate', () => {
      const obs = {
        id: '550e8400-e29b-41d4-a716-446655440000',
        agent: 'test',
        timestamp: FROZEN_TIMESTAMP,
        kind: 'completeness',
        severity: 'info',
        subject: 'test',
        evidence: { query: 'test', result: {}, witnesses: [] },
        metrics: { confidence: 0.5, coverage: 0.5, latency_ms: 10 }
      };

      const result = tryValidateObservation(obs);
      expect(result).not.toBeNull();
      expect(result.id).toBe(obs.id);
    });
  });
});
