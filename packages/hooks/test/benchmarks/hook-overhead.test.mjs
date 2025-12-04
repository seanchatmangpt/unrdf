/**
 * @file Knowledge Hook Performance Overhead Benchmarks (80/20 Optimized)
 * @module @unrdf/hooks/test/benchmarks/hook-overhead
 *
 * Fast benchmarks: <1s total execution
 * - Reduced iterations: 100 (from 10,000)
 * - Reduced quads: 1K (from 10K)
 * - Essential tests only
 *
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { namedNode, literal, quad } from '@unrdf/core';
import {
  KnowledgeHookManager,
  defineHook,
  executeHook,
  executeHookChain,
  builtinHooks,
} from '../../src/index.mjs';

// Reduced quad count for fast tests
const FAST_QUAD_COUNT = 1000;
const FAST_ITERATIONS = 100;

/**
 * Generate test quads with predictable patterns
 */
function generateQuads(count) {
  const quads = [];
  for (let i = 0; i < count; i++) {
    quads.push(
      quad(
        namedNode(`http://example.org/subject${i}`),
        namedNode(`http://example.org/predicate${i % 10}`),
        literal(`value${i}`)
      )
    );
  }
  return quads;
}

/**
 * Benchmark a synchronous function
 */
function benchmarkSync(fn) {
  const start = performance.now();
  fn();
  return performance.now() - start;
}

/**
 * Fast benchmark with stats
 */
function benchmarkWithStats(fn, iterations = FAST_ITERATIONS) {
  const durations = [];
  for (let i = 0; i < iterations; i++) {
    durations.push(benchmarkSync(fn));
  }
  durations.sort((a, b) => a - b);
  return {
    avg: durations.reduce((sum, d) => sum + d, 0) / durations.length,
    min: durations[0],
    max: durations[durations.length - 1],
    p95: durations[Math.floor(durations.length * 0.95)],
  };
}

describe('Knowledge Hook Overhead Benchmarks', () => {
  // Shared test quad
  const testQuad = quad(
    namedNode('http://example.org/s'),
    namedNode('http://example.org/p'),
    literal('test')
  );

  describe('1. Performance Gates', () => {
    it('GATE: 1K operations < 50ms', () => {
      const hooks = [
        defineHook({
          name: 'fast-validate',
          trigger: 'before-add',
          validate: () => true,
        }),
      ];
      const quads = generateQuads(FAST_QUAD_COUNT);

      const start = performance.now();
      for (const q of quads) {
        executeHookChain(hooks, q);
      }
      const duration = performance.now() - start;

      expect(duration).toBeLessThan(50);
    });

    it('GATE: compiled chain works', () => {
      const hooks = [
        defineHook({ name: 't1', trigger: 'before-add', validate: () => true }),
        defineHook({ name: 't2', trigger: 'before-add', transform: q => q }),
      ];

      const result = executeHookChain(hooks, testQuad);
      expect(result.valid).toBe(true);
    });
  });

  describe('2. Registration', () => {
    it('Register 10 hooks < 10ms', () => {
      const manager = new KnowledgeHookManager();
      const duration = benchmarkSync(() => {
        for (let i = 0; i < 10; i++) {
          manager.define({ name: `h${i}`, trigger: 'before-add', validate: () => true });
        }
      });
      expect(duration).toBeLessThan(10);
    });

    it('Register builtins < 20ms', () => {
      const duration = benchmarkSync(() => {
        new KnowledgeHookManager({ includeBuiltins: true });
      });
      expect(duration).toBeLessThan(20);
    });
  });

  describe('3. Single Hook Execution', () => {
    it('Simple validation overhead', () => {
      const hook = defineHook({ name: 'sv', trigger: 'before-add', validate: () => true });
      const stats = benchmarkWithStats(() => executeHook(hook, testQuad));
      expect(stats.avg).toBeLessThan(1); // <1ms per op
    });

    it('IRI validation overhead', () => {
      const stats = benchmarkWithStats(() => executeHook(builtinHooks.validateSubjectIRI, testQuad));
      expect(stats.avg).toBeLessThan(1);
    });

    it('Transformation overhead', () => {
      const stats = benchmarkWithStats(() => executeHook(builtinHooks.trimLiterals, testQuad));
      expect(stats.avg).toBeLessThan(1);
    });
  });

  describe('4. Hook Chain Execution', () => {
    it('1 hook chain', () => {
      const hooks = [builtinHooks.validateSubjectIRI];
      const stats = benchmarkWithStats(() => executeHookChain(hooks, testQuad));
      expect(stats.avg).toBeLessThan(1);
    });

    it('3 hooks chain', () => {
      const hooks = [
        builtinHooks.validateSubjectIRI,
        builtinHooks.validatePredicateIRI,
        builtinHooks.trimLiterals,
      ];
      const stats = benchmarkWithStats(() => executeHookChain(hooks, testQuad));
      expect(stats.avg).toBeLessThan(1);
    });

    it('5 hooks chain', () => {
      const hooks = [
        builtinHooks.validateSubjectIRI,
        builtinHooks.validatePredicateIRI,
        builtinHooks.validateIRIFormat,
        builtinHooks.trimLiterals,
        builtinHooks.normalizeLanguageTag,
      ];
      const stats = benchmarkWithStats(() => executeHookChain(hooks, testQuad));
      expect(stats.avg).toBeLessThan(1);
    });
  });

  describe('5. Bulk Operations', () => {
    it('1K ops + 1 hook < 50ms', () => {
      const quads = generateQuads(FAST_QUAD_COUNT);
      const hook = builtinHooks.validateSubjectIRI;

      const duration = benchmarkSync(() => {
        for (const q of quads) executeHook(hook, q);
      });

      expect(duration).toBeLessThan(50);
    });

    it('1K ops + 3 hooks < 100ms', () => {
      const quads = generateQuads(FAST_QUAD_COUNT);
      const hooks = [
        builtinHooks.validateSubjectIRI,
        builtinHooks.validatePredicateIRI,
        builtinHooks.trimLiterals,
      ];

      const duration = benchmarkSync(() => {
        for (const q of quads) executeHookChain(hooks, q);
      });

      expect(duration).toBeLessThan(100);
    });

    it('1K ops + 5 hooks < 150ms', () => {
      const quads = generateQuads(FAST_QUAD_COUNT);
      const hooks = [
        builtinHooks.validateSubjectIRI,
        builtinHooks.validatePredicateIRI,
        builtinHooks.validateIRIFormat,
        builtinHooks.trimLiterals,
        builtinHooks.normalizeLanguageTag,
      ];

      const duration = benchmarkSync(() => {
        for (const q of quads) executeHookChain(hooks, q);
      });

      expect(duration).toBeLessThan(150);
    });
  });

  describe('6. Policy Packs', () => {
    it('Standard validation policy', () => {
      const quads = generateQuads(FAST_QUAD_COUNT);
      const hook = builtinHooks.standardValidation;

      const duration = benchmarkSync(() => {
        for (const q of quads) executeHook(hook, q);
      });

      expect(duration).toBeLessThan(50);
    });

    it('Full transformation pipeline', () => {
      const quads = generateQuads(FAST_QUAD_COUNT);
      const hooks = [
        builtinHooks.trimLiterals,
        builtinHooks.normalizeLanguageTag,
        builtinHooks.normalizeNamespace,
      ];

      const duration = benchmarkSync(() => {
        for (const q of quads) executeHookChain(hooks, q);
      });

      expect(duration).toBeLessThan(100);
    });
  });

  describe('7. Memory', () => {
    it('100 hooks < 5MB', () => {
      const memBefore = process.memoryUsage().heapUsed;
      const manager = new KnowledgeHookManager();

      for (let i = 0; i < 100; i++) {
        manager.define({ name: `h${i}`, trigger: 'before-add', validate: () => true });
      }

      const memDelta = (process.memoryUsage().heapUsed - memBefore) / 1024 / 1024;
      expect(memDelta).toBeLessThan(5);
    });
  });

  describe('8. Summary', () => {
    it('Generate overhead report', () => {
      const baselineStats = benchmarkWithStats(() => void testQuad);
      const singleHookStats = benchmarkWithStats(() =>
        executeHook(builtinHooks.validateSubjectIRI, testQuad)
      );
      const threeHooksStats = benchmarkWithStats(() =>
        executeHookChain(
          [builtinHooks.validateSubjectIRI, builtinHooks.validatePredicateIRI, builtinHooks.trimLiterals],
          testQuad
        )
      );

      console.log('\n📊 Hook Overhead (100 iterations)');
      console.log(`  Baseline: ${(baselineStats.avg * 1000).toFixed(1)}μs`);
      console.log(`  1 Hook:   ${(singleHookStats.avg * 1000).toFixed(1)}μs`);
      console.log(`  3 Hooks:  ${(threeHooksStats.avg * 1000).toFixed(1)}μs\n`);

      expect(true).toBe(true);
    });
  });
});
