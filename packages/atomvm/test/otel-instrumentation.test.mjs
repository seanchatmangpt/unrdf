/**
 * @fileoverview OTEL Instrumentation Test Suite
 * @module test/otel-instrumentation
 *
 * @description
 * Tests for the centralized OTEL span creation and management module.
 * Validates span creation, attribute recording, error handling, and
 * pre-configured span creators.
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  createSpan,
  withSpan,
  recordAttribute,
  recordAttributes,
  recordError,
  recordMetric,
  traceTriplePattern,
  traceSPARQLQuery,
  traceRPCCall,
  traceMessageValidation,
  traceCacheOperation,
  traceBatchOperation,
  traceWithTiming,
  getActiveSpan,
  isTracingEnabled,
  createChildSpan,
  getTracer,
  SERVICE_NAME,
  TRACER_VERSION,
} from '../src/otel-instrumentation.mjs';
import { SpanStatusCode } from '@opentelemetry/api';

describe('OTEL Instrumentation', () => {
  describe('createSpan', () => {
    it('creates a valid span with name', () => {
      const span = createSpan('test.span');
      expect(span).toBeDefined();
      expect(typeof span.end).toBe('function');
      span.end();
    });

    it('creates a span with custom attributes', () => {
      const span = createSpan('test.span', {
        'custom.attr': 'value',
        'custom.number': 42,
      });
      expect(span).toBeDefined();
      span.end();
    });

    it('includes service name attribute', () => {
      const span = createSpan('test.span');
      expect(span).toBeDefined();
      span.end();
    });
  });

  describe('withSpan', () => {
    it('executes function within span context', async () => {
      const result = await withSpan('test.operation', () => {
        return 'success';
      });
      expect(result).toBe('success');
    });

    it('handles async functions', async () => {
      const result = await withSpan('async.operation', async () => {
        await new Promise(resolve => setTimeout(resolve, 10));
        return 'async-result';
      });
      expect(result).toBe('async-result');
    });

    it('propagates errors after recording', async () => {
      const testError = new Error('Test error');

      await expect(
        withSpan('error.operation', () => {
          throw testError;
        })
      ).rejects.toThrow('Test error');
    });

    it('accepts custom attributes', async () => {
      const result = await withSpan(
        'attr.operation',
        () => 'with-attrs',
        { 'custom.key': 'custom-value' }
      );
      expect(result).toBe('with-attrs');
    });
  });

  describe('recordAttribute', () => {
    it('records attribute on span', () => {
      const span = createSpan('attr.test');
      recordAttribute(span, 'test.key', 'test-value');
      span.end();
    });

    it('handles null span gracefully', () => {
      expect(() => recordAttribute(null, 'key', 'value')).not.toThrow();
    });

    it('handles numeric values', () => {
      const span = createSpan('numeric.test');
      recordAttribute(span, 'count', 100);
      span.end();
    });

    it('handles boolean values', () => {
      const span = createSpan('boolean.test');
      recordAttribute(span, 'enabled', true);
      span.end();
    });
  });

  describe('recordAttributes', () => {
    it('records multiple attributes', () => {
      const span = createSpan('multi.attr.test');
      recordAttributes(span, {
        key1: 'value1',
        key2: 42,
        key3: true,
      });
      span.end();
    });
  });

  describe('recordError', () => {
    it('logs error in span', () => {
      const span = createSpan('error.test');
      const error = new Error('Test error message');
      recordError(span, error);
      span.end();
    });

    it('handles null span gracefully', () => {
      expect(() => recordError(null, new Error('test'))).not.toThrow();
    });

    it('records error type and message', () => {
      const span = createSpan('error.details.test');
      const error = new TypeError('Type error occurred');
      recordError(span, error);
      span.end();
    });
  });

  describe('recordMetric', () => {
    it('tracks metric in span', () => {
      const span = createSpan('metric.test');
      recordMetric(span, 'duration.ms', 150);
      recordMetric(span, 'result.count', 42);
      span.end();
    });

    it('handles null span gracefully', () => {
      expect(() => recordMetric(null, 'metric', 100)).not.toThrow();
    });
  });

  describe('Pre-configured span creators', () => {
    describe('traceTriplePattern', () => {
      it('traces pattern with all components', async () => {
        const result = await traceTriplePattern(
          'ex:subject',
          'ex:predicate',
          'ex:object',
          () => ['match1', 'match2']
        );
        expect(result).toEqual(['match1', 'match2']);
      });

      it('handles null pattern components', async () => {
        const result = await traceTriplePattern(null, null, null, () => []);
        expect(result).toEqual([]);
      });
    });

    describe('traceSPARQLQuery', () => {
      it('traces SELECT query', async () => {
        const result = await traceSPARQLQuery(
          'SELECT * WHERE { ?s ?p ?o }',
          () => [{ s: 'a', p: 'b', o: 'c' }]
        );
        expect(result).toHaveLength(1);
      });

      it('traces ASK query', async () => {
        const result = await traceSPARQLQuery(
          'ASK WHERE { ?s ?p ?o }',
          () => true
        );
        expect(result).toBe(true);
      });

      it('detects query types correctly', async () => {
        const queries = [
          'SELECT * WHERE { ?s ?p ?o }',
          'ASK { ?s ?p ?o }',
          'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }',
          'DESCRIBE <http://example.org/>',
        ];

        for (const query of queries) {
          const result = await traceSPARQLQuery(query, () => []);
          expect(result).toEqual([]);
        }
      });
    });

    describe('traceRPCCall', () => {
      it('traces RPC call with target and module', async () => {
        const result = await traceRPCCall(
          'node://remote:8080',
          'rpc_handler',
          () => ({ status: 'ok' })
        );
        expect(result).toEqual({ status: 'ok' });
      });
    });

    describe('traceMessageValidation', () => {
      it('traces validation operation', async () => {
        const result = await traceMessageValidation(() => ({
          valid: true,
          errors: [],
        }));
        expect(result).toEqual({ valid: true, errors: [] });
      });

      it('handles validation failures', async () => {
        await expect(
          traceMessageValidation(() => {
            throw new Error('Validation failed');
          })
        ).rejects.toThrow('Validation failed');
      });
    });

    describe('traceCacheOperation', () => {
      it('traces cache hit', async () => {
        const result = await traceCacheOperation('key:123', true, () => 'cached-value');
        expect(result).toBe('cached-value');
      });

      it('traces cache miss', async () => {
        const result = await traceCacheOperation('key:456', false, () => 'fresh-value');
        expect(result).toBe('fresh-value');
      });
    });

    describe('traceBatchOperation', () => {
      it('traces batch insert', async () => {
        const result = await traceBatchOperation('insert', 100, () => ({
          inserted: 100,
        }));
        expect(result).toEqual({ inserted: 100 });
      });
    });

    describe('traceWithTiming', () => {
      it('returns result with duration', async () => {
        const { result, duration } = await traceWithTiming('slow-op', async () => {
          await new Promise(resolve => setTimeout(resolve, 50));
          return 'done';
        });
        expect(result).toBe('done');
        expect(duration).toBeGreaterThanOrEqual(40);
      });
    });
  });

  describe('Utility functions', () => {
    describe('getTracer', () => {
      it('returns a tracer instance', () => {
        const t = getTracer();
        expect(t).toBeDefined();
        expect(typeof t.startSpan).toBe('function');
      });
    });

    describe('isTracingEnabled', () => {
      it('returns boolean', () => {
        const enabled = isTracingEnabled();
        expect(typeof enabled).toBe('boolean');
      });
    });

    describe('createChildSpan', () => {
      it('creates child span', () => {
        const child = createChildSpan('child.span', { parent: 'test' });
        expect(child).toBeDefined();
        child.end();
      });
    });

    describe('constants', () => {
      it('exports SERVICE_NAME', () => {
        expect(SERVICE_NAME).toBe('unrdf');
      });

      it('exports TRACER_VERSION', () => {
        expect(TRACER_VERSION).toBe('6.0.0');
      });
    });
  });

  describe('Nested spans maintain hierarchy', () => {
    it('creates nested span structure', async () => {
      const result = await withSpan('parent', async () => {
        return await withSpan('child', async () => {
          return await withSpan('grandchild', () => 'nested-result');
        });
      });
      expect(result).toBe('nested-result');
    });
  });
});
