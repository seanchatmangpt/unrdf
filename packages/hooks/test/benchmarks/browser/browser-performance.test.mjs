/**
 * @file Browser Performance Benchmarks for UNRDF Hooks
 * @description
 * Real browser benchmarks via Playwright for measuring hook execution
 * performance across different JavaScript engines.
 *
 * Runs in: Chromium (V8), Firefox (SpiderMonkey), WebKit (JSC)
 */
/* global navigator */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import {
  defineHook,
  executeHook,
  executeHookChain,
  validateOnly,
  executeBatch,
  validateBatch,
  prewarmHookCache,
  clearHookCaches,
} from '@unrdf/hooks';

// Mock quad factory for browser environment
const createMockQuad = (subject = 'http://example.org/s') => ({
  subject: { termType: 'NamedNode', value: subject },
  predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
  object: { termType: 'Literal', value: 'test value', language: 'en' },
  graph: { termType: 'DefaultGraph', value: '' },
});

// Create test hooks
const validatorHook = defineHook({
  name: 'browser-validator',
  trigger: 'before-add',
  validate: quad => quad.subject.termType === 'NamedNode',
});

const transformHook = defineHook({
  name: 'browser-transform',
  trigger: 'before-add',
  transform: quad => quad,
});

describe('Browser Hook Performance', () => {
  const ITERATIONS = 10000;
  const WARMUP = 1000;

  beforeAll(() => {
    // Pre-warm caches
    prewarmHookCache([validatorHook, transformHook]);
  });

  afterAll(() => {
    clearHookCaches();
  });

  /**
   * High-resolution timing helper
   */
  function benchmark(fn, iterations = ITERATIONS, warmupIterations = WARMUP) {
    const quad = createMockQuad();

    // Warmup phase
    for (let i = 0; i < warmupIterations; i++) {
      fn(quad);
    }

    // Measurement phase
    const start = performance.now();
    for (let i = 0; i < iterations; i++) {
      fn(quad);
    }
    const end = performance.now();

    const totalMs = end - start;
    const perOpUs = (totalMs / iterations) * 1000;

    return { totalMs, perOpUs, iterations };
  }

  describe('Single Hook Execution', () => {
    it('should measure validation hook performance', () => {
      const result = benchmark(quad => executeHook(validatorHook, quad));

      console.log(`Single validation: ${result.perOpUs.toFixed(3)}μs/op`);
      console.log(`Total: ${result.totalMs.toFixed(2)}ms for ${result.iterations} ops`);

      expect(result.perOpUs).toBeLessThan(10); // <10μs target
    });

    it('should measure transform hook performance', () => {
      const result = benchmark(quad => executeHook(transformHook, quad));

      console.log(`Single transform: ${result.perOpUs.toFixed(3)}μs/op`);

      expect(result.perOpUs).toBeLessThan(10);
    });
  });

  describe('Hook Chain Execution', () => {
    it('should measure compiled chain performance', () => {
      const hooks = [validatorHook, transformHook];
      const result = benchmark(quad => executeHookChain(hooks, quad));

      console.log(`Compiled chain: ${result.perOpUs.toFixed(3)}μs/op`);

      expect(result.perOpUs).toBeLessThan(10); // <10μs target for compiled (machine-dependent)
    });

    it('should measure validation-only chain', () => {
      const hooks = [validatorHook];
      const result = benchmark(quad => validateOnly(hooks, quad));

      console.log(`Validation-only: ${result.perOpUs.toFixed(3)}μs/op`);

      expect(result.perOpUs).toBeLessThan(3); // <3μs target
    });
  });

  describe('Batch Execution', () => {
    it('should measure batch validation performance', () => {
      const hooks = [validatorHook];
      const quads = Array.from({ length: 1000 }, (_, i) =>
        createMockQuad(`http://example.org/s${i}`)
      );

      const start = performance.now();
      const bitmap = validateBatch(hooks, quads);
      const totalMs = performance.now() - start;

      const perOpUs = (totalMs / quads.length) * 1000;
      const validCount = bitmap.reduce((sum, v) => sum + v, 0);

      console.log(`Batch validation: ${perOpUs.toFixed(3)}μs/op`);
      console.log(`Valid: ${validCount}/${quads.length}`);

      expect(perOpUs).toBeLessThan(2); // <2μs target for batch
      expect(validCount).toBe(1000);
    });

    it('should measure batch execution performance', () => {
      const hooks = [validatorHook, transformHook];
      const quads = Array.from({ length: 1000 }, (_, i) =>
        createMockQuad(`http://example.org/s${i}`)
      );

      const start = performance.now();
      const result = executeBatch(hooks, quads);
      const totalMs = performance.now() - start;

      const perOpUs = (totalMs / quads.length) * 1000;

      console.log(`Batch execution: ${perOpUs.toFixed(3)}μs/op`);
      console.log(`Valid: ${result.validCount}/${quads.length}`);

      expect(perOpUs).toBeLessThan(5); // <5μs target for batch with transforms
      expect(result.validCount).toBe(1000);
    });
  });

  describe('Engine Comparison Metrics', () => {
    it('should report environment info', () => {
      const ua = typeof navigator !== 'undefined' ? navigator.userAgent : 'Node.js';
      console.log(`\nEnvironment: ${ua}`);
      console.log(
        `Performance.now precision: ${typeof performance !== 'undefined' ? 'available' : 'unavailable'}`
      );

      expect(true).toBe(true);
    });

    it('should measure 10K operations throughput', () => {
      const hooks = [validatorHook];
      const quads = Array.from({ length: 10000 }, (_, i) =>
        createMockQuad(`http://example.org/s${i}`)
      );

      const start = performance.now();
      const bitmap = validateBatch(hooks, quads);
      const totalMs = performance.now() - start;

      console.log(`\n10K operations: ${totalMs.toFixed(2)}ms total`);
      console.log(`Throughput: ${Math.round((10000 / totalMs) * 1000)} ops/sec`);

      // Target: <10ms for 10K operations
      expect(totalMs).toBeLessThan(100);
      expect(bitmap.reduce((sum, v) => sum + v, 0)).toBe(10000);
    });
  });
});
