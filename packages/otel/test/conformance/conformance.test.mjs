/**
 * @file Process Conformance Pipeline Tests
 * @description Chicago TDD — Van der Aalst conformance checking from OTel spans
 *
 * These tests enforce the doctrine:
 *   "If the code says it worked but the event log cannot prove a lawful
 *    process happened, then it did not work."
 */

import { describe, it, expect } from 'vitest';
import { checkProcessConformance, DEVIATION_TYPES } from '../../src/conformance/index.mjs';
import { generateImpossibleLog } from '../../src/ocel/index.mjs';
import { verifyProcessConformance } from '@unrdf/manufacturing/causality';

describe('checkProcessConformance — lawful process', () => {
  it('returns conformant:true for empty span list', async () => {
    const result = await checkProcessConformance([]);
    expect(result.conformant).toBe(true);
    expect(result.fitness).toBe(1.0);
  });

  it('returns conformant:true for a lawful manufacturing trace', async () => {
    const t = Date.now();
    const spans = [
      span('s1', 't1', 'seed', t, 'art-1'),
      span('s2', 't1', 'validate', t + 1, 'art-1'),
      span('s3', 't1', 'compile', t + 2, 'art-1'),
      span('s4', 't1', 'release', t + 3, 'art-1'),
    ];
    const result = await checkProcessConformance(spans);
    expect(result.conformant).toBe(true);
    expect(result.fitness).toBe(1.0);
    expect(result.deviations).toHaveLength(0);
  });

  it('result includes diagnostics', async () => {
    const t = Date.now();
    const spans = [span('s1', 't1', 'seed', t, 'art-1')];
    const result = await checkProcessConformance(spans);
    expect(result.diagnostics).toBeDefined();
    expect(result.diagnostics.totalEvents).toBeGreaterThanOrEqual(0);
  });
});

describe('checkProcessConformance — impossible logs (negative tests)', () => {
  it('rejects release_before_validate impossible log', async () => {
    const log = generateImpossibleLog('release_before_validate');
    const result = verifyProcessConformance(log, { stages: [] });
    expect(result.conformant).toBe(false);
    expect(result.deviations.length).toBeGreaterThan(0);
    const types = result.deviations.map(d => d.type);
    expect(types).toContain(DEVIATION_TYPES.SKIPPED_STAGE);
  });

  it('rejects concurrent_terminal_states impossible log', async () => {
    const log = generateImpossibleLog('concurrent_terminal_states');
    const result = verifyProcessConformance(log, { stages: [] });
    expect(result.conformant).toBe(false);
    const types = result.deviations.map(d => d.type);
    expect(types).toContain(DEVIATION_TYPES.DUPLICATE_TERMINAL);
  });

  it('rejects validate_before_breed impossible log', async () => {
    const log = generateImpossibleLog('validate_before_breed');
    const result = verifyProcessConformance(log, { stages: [] });
    expect(result.conformant).toBe(false);
    const types = result.deviations.map(d => d.type);
    expect(types).toContain(DEVIATION_TYPES.IMPOSSIBLE_ORDERING);
  });
});

describe('DEVIATION_TYPES re-export', () => {
  it('exports all 7 deviation types', () => {
    expect(DEVIATION_TYPES.SKIPPED_STAGE).toBeDefined();
    expect(DEVIATION_TYPES.IMPOSSIBLE_ORDERING).toBeDefined();
    expect(DEVIATION_TYPES.DUPLICATE_TERMINAL).toBeDefined();
    expect(DEVIATION_TYPES.ORPHAN_OBJECT).toBeDefined();
    expect(DEVIATION_TYPES.MISSING_PREDECESSOR).toBeDefined();
    expect(DEVIATION_TYPES.VARIANT_EXPLOSION).toBeDefined();
    expect(DEVIATION_TYPES.TEMPORAL_VIOLATION).toBeDefined();
  });
});

// Test helper: build a minimal OTel span
function span(spanId, traceId, name, startMs, artifactId) {
  return {
    spanId,
    traceId,
    name,
    startTimeUnixNano: String(startMs * 1_000_000),
    endTimeUnixNano: String((startMs + 1) * 1_000_000),
    attributes: { 'artifact.id': artifactId },
    status: { code: 'OK' },
  };
}
