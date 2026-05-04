/**
 * @fileoverview Unit Tests for Artifact Module
 *
 * Tests artifact operations: hashing, merging, diffing, verification.
 *
 * @module @unrdf/kgc-probe/test/unit/artifact
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  hashObservations,
  mergeShards,
  diffArtifacts,
  verifyArtifact,
  computeArtifactSummary,
  serializeArtifact,
  deserializeArtifact,
  createObservationValidator,
  ObservationValidator
} from '../../src/artifact.mjs';
import { FROZEN_TIMESTAMP } from '../fixtures/frozen-environment.mjs';
import { ALL_SHARDS } from '../fixtures/precalculated-shards.mjs';

/**
 * Helper to create observation for tests
 */
function createTestObservation(agentId, kind, index) {
  return {
    id: `test-${agentId}-${index}`,
    agent: agentId,
    timestamp: FROZEN_TIMESTAMP,
    kind,
    severity: 'info',
    subject: `test:entity${index}`,
    predicate: 'test:prop',
    object: `test:value${index}`,
    evidence: {
      query: 'test query',
      result: { count: index },
      witnesses: []
    },
    metrics: {
      confidence: 0.9,
      coverage: 0.8,
      latency_ms: 10
    },
    tags: []
  };
}

describe('Artifact Module', () => {
  describe('hashObservations', () => {
    it('should hash empty array', async () => {
      const hash = await hashObservations([]);

      expect(hash).toBeDefined();
      expect(hash.length).toBe(64);
    });

    it('should produce 64-char hex hash', async () => {
      const observations = ALL_SHARDS[0].observations;
      const hash = await hashObservations(observations);

      expect(hash.length).toBe(64);
      expect(hash).toMatch(/^[0-9a-f]+$/);
    });

    it('should be deterministic', async () => {
      const observations = ALL_SHARDS[0].observations;

      const hash1 = await hashObservations(observations);
      const hash2 = await hashObservations(observations);

      expect(hash1).toBe(hash2);
    });

    it('should produce same hash regardless of order', async () => {
      const observations = [...ALL_SHARDS[0].observations];
      const reversed = [...observations].reverse();

      const hash1 = await hashObservations(observations);
      const hash2 = await hashObservations(reversed);

      expect(hash1).toBe(hash2);
    });

    it('should produce different hashes for different data', async () => {
      const hash1 = await hashObservations(ALL_SHARDS[0].observations);
      const hash2 = await hashObservations(ALL_SHARDS[1].observations);

      expect(hash1).not.toBe(hash2);
    });
  });

  describe('mergeShards', () => {
    it('should merge empty array', async () => {
      const merged = await mergeShards([]);

      expect(Array.isArray(merged)).toBe(true);
      expect(merged.length).toBe(0);
    });

    it('should merge single shard', async () => {
      const merged = await mergeShards([
        { observations: ALL_SHARDS[0].observations }
      ]);

      expect(merged.length).toBe(ALL_SHARDS[0].observations.length);
    });

    it('should merge multiple shards', async () => {
      const merged = await mergeShards([
        { observations: ALL_SHARDS[0].observations },
        { observations: ALL_SHARDS[1].observations }
      ]);

      const expectedLength = ALL_SHARDS[0].observations.length + ALL_SHARDS[1].observations.length;
      expect(merged.length).toBe(expectedLength);
    });

    it('should deduplicate identical observations', async () => {
      const merged = await mergeShards([
        { observations: ALL_SHARDS[0].observations },
        { observations: ALL_SHARDS[0].observations }
      ]);

      expect(merged.length).toBe(ALL_SHARDS[0].observations.length);
    });

    it('should merge with new observations', async () => {
      const newObs = [createTestObservation('new', 'new_kind', 0)];

      const merged = await mergeShards([
        { observations: ALL_SHARDS[0].observations }
      ], newObs);

      expect(merged.length).toBe(ALL_SHARDS[0].observations.length + 1);
    });

    it('should sort by timestamp', async () => {
      const merged = await mergeShards([
        { observations: ALL_SHARDS[0].observations },
        { observations: ALL_SHARDS[1].observations }
      ]);

      for (let i = 1; i < merged.length; i++) {
        const prevTs = new Date(merged[i - 1].timestamp).getTime();
        const currTs = new Date(merged[i].timestamp).getTime();
        expect(currTs).toBeGreaterThanOrEqual(prevTs);
      }
    });
  });

  describe('diffArtifacts', () => {
    it('should diff identical artifacts', () => {
      const artifact = { observations: ALL_SHARDS[0].observations };

      const diff = diffArtifacts(artifact, artifact);

      expect(diff.added.length).toBe(0);
      expect(diff.removed.length).toBe(0);
      expect(diff.summary.similarity_ratio).toBeGreaterThanOrEqual(0.5);
    });

    it('should detect added observations', () => {
      const artifact1 = { observations: ALL_SHARDS[0].observations };
      const artifact2 = {
        observations: [...ALL_SHARDS[0].observations, ...ALL_SHARDS[1].observations]
      };

      const diff = diffArtifacts(artifact1, artifact2);

      expect(diff.added.length).toBe(ALL_SHARDS[1].observations.length);
    });

    it('should detect removed observations', () => {
      const artifact1 = {
        observations: [...ALL_SHARDS[0].observations, ...ALL_SHARDS[1].observations]
      };
      const artifact2 = { observations: ALL_SHARDS[0].observations };

      const diff = diffArtifacts(artifact1, artifact2);

      expect(diff.removed.length).toBe(ALL_SHARDS[1].observations.length);
    });

    it('should calculate total changes', () => {
      const artifact1 = { observations: ALL_SHARDS[0].observations };
      const artifact2 = { observations: ALL_SHARDS[1].observations };

      const diff = diffArtifacts(artifact1, artifact2);

      expect(diff.summary.total_changes).toBeGreaterThan(0);
    });

    it('should handle empty artifacts', () => {
      const diff = diffArtifacts({ observations: [] }, { observations: [] });

      expect(diff.added.length).toBe(0);
      expect(diff.removed.length).toBe(0);
      expect(diff.summary.similarity_ratio).toBe(1.0);
    });
  });

  describe('computeArtifactSummary', () => {
    it('should compute summary for empty observations', () => {
      const summary = computeArtifactSummary([]);

      expect(summary.total).toBe(0);
      expect(summary.confidence_mean).toBe(0);
      expect(summary.coverage_mean).toBe(0);
    });

    it('should count total observations', () => {
      const observations = ALL_SHARDS[0].observations;
      const summary = computeArtifactSummary(observations);

      expect(summary.total).toBe(observations.length);
    });

    it('should count by kind', () => {
      const observations = ALL_SHARDS[0].observations;
      const summary = computeArtifactSummary(observations);

      let totalByKind = 0;
      for (const count of Object.values(summary.by_kind)) {
        totalByKind += count;
      }

      expect(totalByKind).toBe(observations.length);
    });

    it('should count by severity', () => {
      const observations = ALL_SHARDS.flatMap(s => s.observations);
      const summary = computeArtifactSummary(observations);

      const totalBySeverity =
        summary.by_severity.critical +
        summary.by_severity.warning +
        summary.by_severity.info;

      expect(totalBySeverity).toBe(observations.length);
    });

    it('should calculate mean confidence', () => {
      const observations = ALL_SHARDS[0].observations;
      const summary = computeArtifactSummary(observations);

      expect(summary.confidence_mean).toBeGreaterThan(0);
      expect(summary.confidence_mean).toBeLessThanOrEqual(1);
    });

    it('should calculate mean coverage', () => {
      const observations = ALL_SHARDS[0].observations;
      const summary = computeArtifactSummary(observations);

      expect(summary.coverage_mean).toBeGreaterThan(0);
      expect(summary.coverage_mean).toBeLessThanOrEqual(1);
    });
  });

  describe('serializeArtifact / deserializeArtifact', () => {
    it('should serialize artifact to JSON string', () => {
      const artifact = {
        version: '1.0',
        universe_id: 'test',
        observations: []
      };

      const json = serializeArtifact(artifact);

      expect(typeof json).toBe('string');
      expect(json).toContain('test');
    });

    it('should format JSON with indentation', () => {
      const artifact = { version: '1.0', universe_id: 'test', observations: [] };
      const json = serializeArtifact(artifact);

      expect(json).toContain('\n');
    });
  });

  describe('ObservationValidator', () => {
    let validator;

    beforeEach(() => {
      validator = createObservationValidator();
    });

    it('should validate correct observation', () => {
      const obs = {
        id: 'test-id',
        agent: 'test',
        timestamp: FROZEN_TIMESTAMP,
        kind: 'test',
        severity: 'info',
        subject: 'test:entity'
      };

      const result = validator.validate(obs);
      expect(result).toEqual(obs);
    });

    it('should throw on missing required field', () => {
      const incomplete = { id: 'test' };

      expect(() => validator.validate(incomplete)).toThrow();
    });

    it('should throw on non-object', () => {
      expect(() => validator.validate(null)).toThrow();
      expect(() => validator.validate('string')).toThrow();
    });

    it('should validate batch of observations', () => {
      const observations = [
        { id: '1', agent: 'a', timestamp: FROZEN_TIMESTAMP, kind: 'k', severity: 's', subject: 's' },
        { id: '2', agent: 'a', timestamp: FROZEN_TIMESTAMP, kind: 'k', severity: 's', subject: 's' }
      ];

      const result = validator.validateBatch(observations);

      expect(result.length).toBe(2);
    });
  });
});
