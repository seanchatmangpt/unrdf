/**
 * @fileoverview OTEL Instrumentation Test Suite
 * @module test/otel-instrumentation
 *
 * @description
 * Tests for the centralized OTEL span creation and management module.
 * Validates span creation, attribute recording, error handling, and
 * pre-configured span creators.
 */

import { describe, it, expect, vi } from 'vitest';
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

describe('OTEL Instrumentation', () => {
  it('creates a valid span', () => {
    const span = createSpan('test.span');
    expect(span).toBeDefined();
    span.end();
  });

  it('executes function within span context', async () => {
    const result = await withSpan('test.operation', () => 'success');
    expect(result).toBe('success');
  });

  it('propagates errors', async () => {
    const error = new Error('Test error');
    await expect(withSpan('error', () => { throw error })).rejects.toThrow('Test error');
  });

  it('records attributes', () => {
    const span = createSpan('attr.test');
    recordAttribute(span, 'key', 'value');
    span.end();
  });

  it('traces triple patterns', async () => {
    const result = await traceTriplePattern('s', 'p', 'o', () => ['match']);
    expect(result).toEqual(['match']);
  });

  it('traces SPARQL queries', async () => {
    const result = await traceSPARQLQuery('SELECT * WHERE { ?s ?p ?o }', () => ['res']);
    expect(result).toEqual(['res']);
  });
});

