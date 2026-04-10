/**
 * @file OTel → OCEL Conversion Tests
 * @description Chicago TDD — behavior verification for OTel span to OCEL event log conversion
 */

import { describe, it, expect } from 'vitest';
import {
  otelSpansToOcel,
  generateImpossibleLog,
  MANUFACTURING_STAGES,
  IMPOSSIBLE_LOG_SCENARIOS,
} from '../../src/ocel/index.mjs';

describe('MANUFACTURING_STAGES', () => {
  it('defines at least 7 stages', () => {
    expect(MANUFACTURING_STAGES.length).toBeGreaterThanOrEqual(7);
  });

  it('starts with seeded and includes released', () => {
    expect(MANUFACTURING_STAGES[0]).toBe('seeded');
    expect(MANUFACTURING_STAGES).toContain('released');
  });

  it('validated comes before released', () => {
    expect(MANUFACTURING_STAGES.indexOf('validated')).toBeLessThan(
      MANUFACTURING_STAGES.indexOf('released'),
    );
  });
});

describe('IMPOSSIBLE_LOG_SCENARIOS', () => {
  it('defines at least 5 scenarios', () => {
    expect(IMPOSSIBLE_LOG_SCENARIOS.length).toBeGreaterThanOrEqual(5);
  });

  it('includes release_before_validate', () => {
    expect(IMPOSSIBLE_LOG_SCENARIOS).toContain('release_before_validate');
  });

  it('includes concurrent_terminal_states', () => {
    expect(IMPOSSIBLE_LOG_SCENARIOS).toContain('concurrent_terminal_states');
  });
});

describe('otelSpansToOcel', () => {
  it('returns empty OCEL log for empty spans array', () => {
    const result = otelSpansToOcel([]);
    expect(result.objects).toBeDefined();
    expect(result.events).toBeDefined();
    expect(Object.keys(result.objects)).toHaveLength(0);
    expect(result.events).toHaveLength(0);
  });

  it('converts a validate span to validated activity', () => {
    const spans = [
      {
        spanId: 's1',
        traceId: 't1',
        name: 'validate',
        startTimeUnixNano: '1000000000',
        endTimeUnixNano: '2000000000',
        attributes: { 'artifact.id': 'artifact-001' },
        status: { code: 'OK' },
      },
    ];
    const result = otelSpansToOcel(spans);
    expect(result.events).toHaveLength(1);
    expect(result.events[0].activity).toBe('validated');
  });

  it('infers artifact objectType for artifact-related spans', () => {
    const spans = [
      {
        spanId: 's1',
        traceId: 't1',
        name: 'seed',
        startTimeUnixNano: '1000000000',
        endTimeUnixNano: '2000000000',
        attributes: { 'artifact.id': 'artifact-001' },
        status: { code: 'OK' },
      },
    ];
    const result = otelSpansToOcel(spans);
    const artifactObj = Object.values(result.objects).find(o => o.type === 'artifact');
    expect(artifactObj).toBeDefined();
  });

  it('uses artifact.id attribute as objectId when present', () => {
    const spans = [
      {
        spanId: 's1',
        traceId: 't1',
        name: 'compile',
        startTimeUnixNano: '1000000000',
        endTimeUnixNano: '2000000000',
        attributes: { 'artifact.id': 'my-artifact-42' },
        status: { code: 'OK' },
      },
    ];
    const result = otelSpansToOcel(spans);
    expect(result.objects['my-artifact-42']).toBeDefined();
  });

  it('groups spans from same traceId as events for same case', () => {
    const spans = [
      {
        spanId: 's1', traceId: 't1', name: 'seed',
        startTimeUnixNano: '1000000000', endTimeUnixNano: '2000000000',
        attributes: { 'artifact.id': 'art-1' }, status: { code: 'OK' },
      },
      {
        spanId: 's2', traceId: 't1', name: 'validate',
        startTimeUnixNano: '3000000000', endTimeUnixNano: '4000000000',
        attributes: { 'artifact.id': 'art-1' }, status: { code: 'OK' },
      },
    ];
    const result = otelSpansToOcel(spans);
    expect(result.events).toHaveLength(2);
    expect(result.events[0].object).toBe(result.events[1].object);
  });

  it('converts release span to released activity', () => {
    const spans = [
      {
        spanId: 's1', traceId: 't1', name: 'release',
        startTimeUnixNano: '5000000000', endTimeUnixNano: '6000000000',
        attributes: { 'artifact.id': 'art-x' }, status: { code: 'OK' },
      },
    ];
    const result = otelSpansToOcel(spans);
    expect(result.events[0].activity).toBe('released');
  });

  it('converts receipt span to receipted activity', () => {
    const spans = [
      {
        spanId: 's1', traceId: 't1', name: 'receipt',
        startTimeUnixNano: '7000000000', endTimeUnixNano: '8000000000',
        attributes: { 'artifact.id': 'art-y' }, status: { code: 'OK' },
      },
    ];
    const result = otelSpansToOcel(spans);
    expect(result.events[0].activity).toBe('receipted');
  });

  it('converts nanosecond timestamps to ISO strings', () => {
    const spans = [
      {
        spanId: 's1', traceId: 't1', name: 'validate',
        startTimeUnixNano: '1000000000000', endTimeUnixNano: '2000000000000',
        attributes: { 'artifact.id': 'art-z' }, status: { code: 'OK' },
      },
    ];
    const result = otelSpansToOcel(spans);
    expect(typeof result.events[0].timestamp).toBe('string');
    expect(result.events[0].timestamp).toMatch(/^\d{4}-\d{2}-\d{2}T/);
  });
});

describe('generateImpossibleLog', () => {
  it('returns an OCEL log object with objects and events', () => {
    const log = generateImpossibleLog('release_before_validate');
    expect(log.objects).toBeDefined();
    expect(log.events).toBeDefined();
  });

  it('release_before_validate scenario includes released event without validated', () => {
    const log = generateImpossibleLog('release_before_validate');
    const activities = log.events.map(e => e.activity);
    expect(activities).toContain('released');
    expect(activities).not.toContain('validated');
  });

  it('concurrent_terminal_states scenario has duplicate terminal events for same object', () => {
    const log = generateImpossibleLog('concurrent_terminal_states');
    const objectIds = log.events.map(e => e.object);
    // Find an object with two terminal events
    const releaseEvents = log.events.filter(e => e.activity === 'released');
    const duplicates = releaseEvents.filter(
      e => releaseEvents.filter(e2 => e2.object === e.object).length > 1,
    );
    expect(duplicates.length).toBeGreaterThan(0);
  });

  it('throws for unknown scenario', () => {
    expect(() => generateImpossibleLog('not_a_scenario')).toThrow();
  });

  it('validate_before_breed scenario has validate before breed in events', () => {
    const log = generateImpossibleLog('validate_before_breed');
    const activities = log.events.map(e => e.activity);
    const validateIdx = activities.indexOf('validated');
    const breedIdx = activities.indexOf('bred');
    // validate should appear before breed (impossible ordering)
    expect(validateIdx).toBeLessThan(breedIdx);
  });
});
