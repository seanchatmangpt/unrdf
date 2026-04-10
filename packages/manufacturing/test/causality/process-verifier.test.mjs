/**
 * @file Process Verifier Tests (Van der Aalst Conformance)
 */

import { describe, it, expect } from 'vitest';
import { verifyProcessConformance, DEVIATION_TYPES } from '../../src/causality/process-verifier.mjs';

describe('Process Verifier — Van der Aalst Conformance', () => {
  const intendedModel = {
    stages: [
      'seeded', 'bred', 'validated', 'projected', 'compiled',
      'benchmarked', 'released', 'observed', 'receipted'
    ],
    expectedVariants: {
      'source-code': 2, // Allow 2 variants (e.g., with/without benchmark)
    },
  };

  it('passes conformant process log', () => {
    const ocelLog = {
      objects: {
        'artifact-1': {
          id: 'artifact-1',
          type: 'artifact',
          kind: 'source-code',
        },
        'receipt-1': {
          id: 'receipt-1',
          type: 'receipt',
          artifact: 'artifact-1',
          hash: 'abc123',
          previousHash: '0000',
        },
      },
      events: [
        {
          id: 'e1',
          type: 'ArtifactCreated',
          object: 'artifact-1',
          activity: 'seeded',
          timestamp: '2026-04-10T12:00:00Z',
        },
        {
          id: 'e2',
          type: 'ArtifactValidated',
          object: 'artifact-1',
          activity: 'validated',
          timestamp: '2026-04-10T12:00:01Z',
        },
        {
          id: 'e3',
          type: 'ReceiptGenerated',
          object: 'receipt-1',
          activity: 'emit-receipt',
          timestamp: '2026-04-10T12:00:02Z',
          attributes: {
            artifact: 'artifact-1',
            stage: 'validated',
            previousHash: '0000',
            receiptHash: 'abc123',
          },
        },
      ],
    };

    const result = verifyProcessConformance(ocelLog, intendedModel);
    expect(result.conformant).toBe(true);
    expect(result.fitness).toBe(1.0);
    expect(result.precision).toBe(1.0);
    expect(result.deviations).toHaveLength(0);
  });

  it('detects skipped stage', () => {
    const ocelLog = {
      objects: {
        'artifact-1': {
          id: 'artifact-1',
          type: 'artifact',
          kind: 'source-code',
        },
      },
      events: [
        { id: 'e1', type: 'ArtifactCreated', object: 'artifact-1', activity: 'seeded', timestamp: '2026-04-10T12:00:00Z' },
        { id: 'e2', type: 'ArtifactReleased', object: 'artifact-1', activity: 'released', timestamp: '2026-04-10T12:00:01Z' },
      ],
    };

    const result = verifyProcessConformance(ocelLog, intendedModel);
    expect(result.conformant).toBe(false);
    expect(result.deviations.some(d => d.type === DEVIATION_TYPES.SKIPPED_STAGE)).toBe(true);
    // Note: "release before validate" is flagged as SKIPPED_STAGE, not IMPOSSIBLE_ORDERING
    // IMPOSSIBLE_ORDERING is for backward transitions like "validated before seeded"
  });

  it('detects impossible ordering (backward transition)', () => {
    const ocelLog = {
      objects: {
        'artifact-1': { id: 'artifact-1', type: 'artifact', kind: 'source-code' },
      },
      events: [
        { id: 'e1', type: 'ArtifactCreated', object: 'artifact-1', activity: 'validated', timestamp: '2026-04-10T12:00:00Z' },
        { id: 'e2', type: 'ArtifactCreated', object: 'artifact-1', activity: 'bred', timestamp: '2026-04-10T12:00:01Z' },
      ],
    };

    const result = verifyProcessConformance(ocelLog, intendedModel);
    expect(result.conformant).toBe(false);
    expect(result.deviations.some(d => d.message.includes('Backward stage transition'))).toBe(true);
  });

  it('detects release without validation', () => {
    const ocelLog = {
      objects: {
        'artifact-1': { id: 'artifact-1', type: 'artifact', kind: 'source-code' },
      },
      events: [
        { id: 'e1', type: 'ArtifactCreated', object: 'artifact-1', activity: 'seeded', timestamp: '2026-04-10T12:00:00Z' },
        { id: 'e2', type: 'ArtifactReleased', object: 'artifact-1', activity: 'released', timestamp: '2026-04-10T12:00:01Z' },
      ],
    };

    const result = verifyProcessConformance(ocelLog, intendedModel);
    expect(result.conformant).toBe(false);
    expect(result.deviations.some(d => d.message.includes('released without validation'))).toBe(true);
  });

  it('detects duplicate terminal states', () => {
    const ocelLog = {
      objects: {
        'artifact-1': { id: 'artifact-1', type: 'artifact', kind: 'source-code' },
      },
      events: [
        { id: 'e1', type: 'ArtifactCreated', object: 'artifact-1', activity: 'seeded', timestamp: '2026-04-10T12:00:00Z' },
        { id: 'e2', type: 'ArtifactReleased', object: 'artifact-1', activity: 'released', timestamp: '2026-04-10T12:00:02Z' },
        { id: 'e3', type: 'ArtifactReceipted', object: 'artifact-1', activity: 'receipted', timestamp: '2026-04-10T12:00:03Z' },
      ],
    };

    const result = verifyProcessConformance(ocelLog, intendedModel);
    expect(result.conformant).toBe(false);
    expect(result.deviations.some(d => d.type === DEVIATION_TYPES.DUPLICATE_TERMINAL)).toBe(true);
  });

  it('detects orphan object (no events)', () => {
    const ocelLog = {
      objects: {
        'artifact-1': { id: 'artifact-1', type: 'artifact', kind: 'source-code' },
      },
      events: [],
    };

    const result = verifyProcessConformance(ocelLog, intendedModel);
    expect(result.conformant).toBe(false);
    expect(result.deviations.some(d => d.type === DEVIATION_TYPES.ORPHAN_OBJECT)).toBe(true);
  });

  it('detects temporal violation (non-monotonic timestamps)', () => {
    const ocelLog = {
      objects: {
        'artifact-1': { id: 'artifact-1', type: 'artifact', kind: 'source-code' },
      },
      events: [
        { id: 'e1', type: 'ArtifactCreated', object: 'artifact-1', activity: 'validated', timestamp: '2026-04-10T12:00:02Z' },
        { id: 'e2', type: 'ArtifactCreated', object: 'artifact-1', activity: 'bred', timestamp: '2026-04-10T12:00:01Z' },
      ],
    };

    const result = verifyProcessConformance(ocelLog, intendedModel);
    expect(result.conformant).toBe(false);
    expect(result.deviations.some(d => d.type === DEVIATION_TYPES.TEMPORAL_VIOLATION)).toBe(true);
  });

  it('detects variant explosion', () => {
    const ocelLog = {
      objects: {
        'artifact-1': { id: 'artifact-1', type: 'artifact', kind: 'source-code' },
        'artifact-2': { id: 'artifact-2', type: 'artifact', kind: 'source-code' },
        'artifact-3': { id: 'artifact-3', type: 'artifact', kind: 'source-code' },
      },
      events: [
        { id: 'e1', type: 'ArtifactCreated', object: 'artifact-1', activity: 'seeded', timestamp: '2026-04-10T12:00:00Z' },
        { id: 'e2', type: 'ArtifactCreated', object: 'artifact-2', activity: 'seeded', timestamp: '2026-04-10T12:00:01Z' },
        { id: 'e3', type: 'ArtifactCreated', object: 'artifact-3', activity: 'seeded', timestamp: '2026-04-10T12:00:02Z' },
        { id: 'e4', type: 'ArtifactValidated', object: 'artifact-1', activity: 'validated', timestamp: '2026-04-10T12:00:10Z' },
        { id: 'e5', type: 'ArtifactReleased', object: 'artifact-1', activity: 'released', timestamp: '2026-04-10T12:01:00Z' }, // path 1
        { id: 'e6', type: 'ArtifactValidated', object: 'artifact-2', activity: 'validated', timestamp: '2026-04-10T12:00:10Z' },
        { id: 'e7', type: 'ArtifactCompiled', object: 'artifact-2', activity: 'compiled', timestamp: '2026-04-10T12:00:11Z' }, // path 2
        { id: 'e8', type: 'ArtifactValidated', object: 'artifact-3', activity: 'validated', timestamp: '2026-04-10T12:00:10Z' },
        { id: 'e9', type: 'ArtifactProjected', object: 'artifact-3', activity: 'projected', timestamp: '2026-04-10T12:00:11Z' }, // path 3
      ],
    };

    const result = verifyProcessConformance(ocelLog, intendedModel);
    expect(result.conformant).toBe(false);
    expect(result.deviations.some(d => d.type === DEVIATION_TYPES.VARIANT_EXPLOSION)).toBe(true);
  });

  it('returns diagnostics with event counts', () => {
    const ocelLog = {
      objects: {
        'artifact-1': { id: 'artifact-1', type: 'artifact', kind: 'source-code' },
      },
      events: [
        { id: 'e1', type: 'ArtifactCreated', object: 'artifact-1', activity: 'seeded', timestamp: '2026-04-10T12:00:00Z' },
        { id: 'e2', type: 'ArtifactValidated', object: 'artifact-1', activity: 'validated', timestamp: '2026-04-10T12:00:01Z' },
      ],
    };

    const result = verifyProcessConformance(ocelLog, intendedModel);
    expect(result.diagnostics.totalEvents).toBe(2);
    expect(result.diagnostics.uniqueArtifacts).toBe(1);
    expect(result.diagnostics.stageDistribution).toEqual({ seeded: 1, validated: 1 });
  });

  it('calculates fitness penalty for each deviation', () => {
    const ocelLog = {
      objects: {
        'artifact-1': { id: 'artifact-1', type: 'artifact', kind: 'source-code' },
      },
      events: [
        { id: 'e1', type: 'ArtifactCreated', object: 'artifact-1', activity: 'seeded', timestamp: '2026-04-10T12:00:00Z' },
        { id: 'e2', type: 'ArtifactReleased', object: 'artifact-1', activity: 'released', timestamp: '2026-04-10T12:00:01Z' },
      ],
    };

    const result = verifyProcessConformance(ocelLog, intendedModel);
    expect(result.fitness).toBeLessThan(1.0);
    expect(result.deviations.length).toBeGreaterThan(0);
  });
});
