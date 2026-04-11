/**
 * @file OTel to OCEL Converter Tests — Chicago TDD
 * @description OTel trace conversion to OCEL 2.0 event logs
 *
 * Core Chicago TDD tests focused on event log derivation:
 * - Converts OTel spans to OCEL objects
 * - Converts OTel spans to OCEL events
 * - Preserves temporal ordering
 * - Validates OCEL structure
 * - Handles artifact lifecycle tracking
 * - Handles receipt chain tracking
 * - Handles proof gate evaluation
 */

import { describe, it, expect } from 'vitest';
import {
  convertOtelToOCEL,
  validateOCELLog,
  generateSampleOtelTraces,
} from '../../src/otel/index.mjs';

describe('OTel to OCEL Converter — Event Log Derivation', () => {

  it('converts sample OTel traces to valid OCEL structure', () => {
    const otelTraces = generateSampleOtelTraces();
    const ocelLog = convertOtelToOCEL(otelTraces);

    // Validate OCEL structure
    expect(ocelLog).toHaveProperty('@context');
    expect(ocelLog).toHaveProperty('objects');
    expect(ocelLog).toHaveProperty('events');
    expect(Array.isArray(ocelLog.events)).toBe(true);
  });

  it('creates artifact objects with lifecycle', () => {
    const otelTraces = generateSampleOtelTraces();
    const ocelLog = convertOtelToOCEL(otelTraces);

    const artifact = Object.values(ocelLog.objects).find(obj => obj.type === 'artifact');
    expect(artifact).toBeDefined();
    expect(artifact.id).toBe('artifact-1');
    expect(artifact.kind).toBe('source-code');
    expect(Array.isArray(artifact.lifecycle)).toBe(true);
  });

  it('creates receipt objects with hash and artifact reference', () => {
    const otelTraces = generateSampleOtelTraces();
    const ocelLog = convertOtelToOCEL(otelTraces);

    const receipt = Object.values(ocelLog.objects).find(obj => obj.type === 'receipt');
    expect(receipt).toBeDefined();
    expect(receipt.id).toBe('receipt-1');
    expect(receipt.hash).toBe('abc123def456');
    expect(receipt.artifact).toBe('artifact-1');
  });

  it('creates events with required fields (id, type, timestamp, object, activity)', () => {
    const otelTraces = generateSampleOtelTraces();
    const ocelLog = convertOtelToOCEL(otelTraces);

    expect(ocelLog.events.length).toBeGreaterThan(0);
    const firstEvent = ocelLog.events[0];

    expect(firstEvent.id).toBeDefined();
    expect(firstEvent.type).toBeDefined();
    expect(firstEvent.timestamp).toBeDefined();
    expect(firstEvent.object).toBeDefined();
    expect(firstEvent.activity).toBeDefined();
  });

  it('preserves temporal ordering from OTel spans', () => {
    const otelTraces = generateSampleOtelTraces();
    const ocelLog = convertOtelToOCEL(otelTraces);

    // Events should be ordered by timestamp
    for (let i = 1; i < ocelLog.events.length; i++) {
      const prevTime = new Date(ocelLog.events[i - 1].timestamp).getTime();
      const currTime = new Date(ocelLog.events[i].timestamp).getTime();
      expect(currTime).toBeGreaterThanOrEqual(prevTime);
    }
  });

  it('includes OTel provenance in event attributes (trace_id, span_id, duration_us)', () => {
    const otelTraces = generateSampleOtelTraces();
    const ocelLog = convertOtelToOCEL(otelTraces);

    const firstEvent = ocelLog.events[0];
    expect(firstEvent.attributes).toHaveProperty('trace_id');
    expect(firstEvent.attributes).toHaveProperty('span_id');
    expect(firstEvent.attributes).toHaveProperty('duration_us');
  });

  it('validates OCEL log and returns errors for invalid structure', () => {
    const invalidLog = {}; // Missing required fields
    const { valid, errors } = validateOCELLog(invalidLog);

    expect(valid).toBe(false);
    expect(errors.length).toBeGreaterThan(0);
    expect(errors).toContain('Missing @context');
  });

  it('validates well-formed OCEL log successfully', () => {
    const otelTraces = generateSampleOtelTraces();
    const ocelLog = convertOtelToOCEL(otelTraces);
    const { valid, errors } = validateOCELLog(ocelLog);

    expect(valid).toBe(true);
    expect(errors).toHaveLength(0);
  });

  it('converts operator spans to OperatorExecuted events', () => {
    const otelTraces = {
      data: [{
        traceID: 'trace-1',
        spans: [{
          traceID: 'trace-1',
          spanID: 'span-1',
          operationName: 'operator.validate',
          startTime: Date.now() * 1000,
          duration: 5000,
          process: { serviceName: 'manufacturing', tags: [] },
          tags: [
            { key: 'artifact.id', value: 'artifact-1', type: 'string' },
            { key: 'operator', value: 'validate', type: 'string' },
            { key: 'status', value: 'success', type: 'string' },
          ],
        }],
      }],
    };

    const ocelLog = convertOtelToOCEL(otelTraces);
    const operatorEvent = ocelLog.events.find(e => e.type === 'OperatorExecuted');

    expect(operatorEvent).toBeDefined();
    expect(operatorEvent.activity).toBe('validate');
    expect(operatorEvent.object).toBe('artifact-1');
  });

  it('converts gate spans to ProofGateEvaluated events', () => {
    const otelTraces = {
      data: [{
        traceID: 'trace-1',
        spans: [{
          traceID: 'trace-1',
          spanID: 'span-1',
          operationName: 'gate.schema-valid',
          startTime: Date.now() * 1000,
          duration: 2000,
          process: { serviceName: 'manufacturing', tags: [] },
          tags: [
            { key: 'gate', value: 'schema-valid', type: 'string' },
            { key: 'result', value: 'pass', type: 'string' },
            { key: 'severity', value: 'critical', type: 'string' },
          ],
        }],
      }],
    };

    const ocelLog = convertOtelToOCEL(otelTraces);
    const gateEvent = ocelLog.events.find(e => e.type === 'ProofGateEvaluated');

    expect(gateEvent).toBeDefined();
    expect(gateEvent.activity).toBe('schema-valid');
    expect(gateEvent.attributes.result).toBe('pass');
  });

  it('respects maxEvents limit and stops processing', () => {
    const otelTraces = {
      data: [{
        traceID: 'trace-1',
        spans: Array.from({ length: 100 }, (_, i) => ({
          traceID: 'trace-1',
          spanID: `span-${i}`,
          operationName: 'operator.test',
          startTime: i * 1000,
          duration: 1000,
          process: { serviceName: 'manufacturing', tags: [] },
          tags: [
            { key: 'artifact.id', value: `artifact-${i}`, type: 'string' },
          ],
        })),
      }],
    };

    const ocelLog = convertOtelToOCEL(otelTraces, { maxEvents: 10 });
    expect(ocelLog.events.length).toBe(10);
  });

  it('filters spans by service name', () => {
    const otelTraces = {
      data: [{
        traceID: 'trace-1',
        spans: [
          {
            traceID: 'trace-1',
            spanID: 'span-1',
            operationName: 'operator.test',
            startTime: Date.now() * 1000,
            duration: 1000,
            process: { serviceName: 'manufacturing', tags: [] },
            tags: [{ key: 'artifact.id', value: 'artifact-1', type: 'string' }],
          },
          {
            traceID: 'trace-1',
            spanID: 'span-2',
            operationName: 'operator.test',
            startTime: Date.now() * 1000 + 1000,
            duration: 1000,
            process: { serviceName: 'other-service', tags: [] },
            tags: [{ key: 'artifact.id', value: 'artifact-2', type: 'string' }],
          },
        ],
      }],
    };

    const ocelLog = convertOtelToOCEL(otelTraces, { serviceName: 'manufacturing' });
    expect(ocelLog.events.length).toBe(1);
    expect(ocelLog.events[0].object).toBe('artifact-1');
  });

  it('handles empty OTel traces gracefully', () => {
    const ocelLog = convertOtelToOCEL({ data: [] });
    expect(ocelLog.events).toHaveLength(0);
    expect(ocelLog.objects).toEqual({});
  });

  it('handles OTel traces without spans gracefully', () => {
    const ocelLog = convertOtelToOCEL({ data: [{ traceID: 'trace-1', spans: [] }] });
    expect(ocelLog.events).toHaveLength(0);
  });
});
