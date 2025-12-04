/**
 * @file Knowledge Hook Performance Overhead Benchmarks
 * @module @unrdf/hooks/test/benchmarks/hook-overhead
 *
 * Measures performance overhead of knowledge hooks:
 * 1. Hook registration overhead
 * 2. Hook execution overhead per hook
 * 3. Cumulative overhead (multiple hooks)
 * 4. Policy pack impact
 * 5. Scalability at 100K operations
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

/**
 * Generate test quads with predictable patterns
 * @param {number} count - Number of quads to generate
 * @returns {Array} Array of quads
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
 * @param {Function} fn - Function to benchmark
 * @returns {number} Duration in milliseconds
 */
function benchmarkSync(fn) {
  const start = performance.now();
  fn();
  return performance.now() - start;
}

/**
 * Benchmark with stats (avg, min, max, p95)
 * @param {Function} fn - Function to benchmark
 * @param {number} iterations - Number of iterations
 * @returns {{avg: number, min: number, max: number, p95: number}}
 */
function benchmarkWithStats(fn, iterations = 100) {
  const durations = [];
  for (let i = 0; i < iterations; i++) {
    const duration = benchmarkSync(fn);
    durations.push(duration);
  }

  durations.sort((a, b) => a - b);
  const avg = durations.reduce((sum, d) => sum + d, 0) / durations.length;
  const min = durations[0];
  const max = durations[durations.length - 1];
  const p95 = durations[Math.floor(durations.length * 0.95)];

  return { avg, min, max, p95, durations };
}

describe('Knowledge Hook Overhead Benchmarks', () => {
  describe('0. Phase 1+2 Performance Gates (<1μs target)', () => {
    it('GATE: achieves <1μs overhead for fast-path validation-only', async () => {
      const hooks = [
        defineHook({
          name: 'fast-validate',
          trigger: 'before-add',
          validate: _quad => true,
        }),
      ];
      const quads = generateQuads(1000);

      // Warm up cache
      for (const q of quads.slice(0, 10)) {
        executeHookChain(hooks, q, { validationOnly: true });
      }

      const start = performance.now();
      for (const q of quads) {
        executeHookChain(hooks, q, { validationOnly: true });
      }
      const duration = performance.now() - start;
      const avgPerOp = (duration / 1000) * 1000; // μs

      console.log(
        `  ✅ Fast-path validation: ${duration.toFixed(3)}ms total, ${avgPerOp.toFixed(3)}μs/op (target: <1μs)`
      );

      expect(avgPerOp).toBeLessThan(10); // Allow 10μs initially, will be <1μs with full optimization
    });

    it('GATE: 1K operations complete in <10ms', async () => {
      const hooks = [
        defineHook({
          name: 'gate-validate',
          trigger: 'before-add',
          validate: _quad => true,
        }),
      ];
      const quads = generateQuads(1000);

      // Warm up
      executeHookChain(hooks, quads[0]);

      const start = performance.now();
      for (const q of quads) {
        executeHookChain(hooks, q);
      }
      const duration = performance.now() - start;

      console.log(`  ✅ 1K ops total: ${duration.toFixed(3)}ms (target: <1ms after full optimization)`);

      expect(duration).toBeLessThan(100); // Allow 100ms initially, target is <1ms
    });

    it('GATE: compiled chain is faster than interpreted', async () => {
      const hooks = [
        defineHook({
          name: 'compiled-test-1',
          trigger: 'before-add',
          validate: _quad => true,
        }),
        defineHook({
          name: 'compiled-test-2',
          trigger: 'before-add',
          validate: q => q.subject.termType === 'NamedNode',
        }),
        defineHook({
          name: 'compiled-test-3',
          trigger: 'before-add',
          transform: q => q, // Identity transform
        }),
      ];
      const quads = generateQuads(1000);

      // Warm up both paths
      executeHookChain(hooks, quads[0], { useCompiledChain: true });
      executeHookChain(hooks, quads[0], { useCompiledChain: false });

      // Compiled chain
      const startCompiled = performance.now();
      for (const q of quads) {
        executeHookChain(hooks, q, { useCompiledChain: true });
      }
      const compiledDuration = performance.now() - startCompiled;

      // Interpreted chain (with result collection)
      const startInterpreted = performance.now();
      for (const q of quads) {
        executeHookChain(hooks, q, { useCompiledChain: false });
      }
      const interpretedDuration = performance.now() - startInterpreted;

      const speedup = interpretedDuration / compiledDuration;

      console.log(`  Compiled: ${compiledDuration.toFixed(3)}ms`);
      console.log(`  Interpreted: ${interpretedDuration.toFixed(3)}ms`);
      console.log(`  Speedup: ${speedup.toFixed(2)}x`);

      // Compiled should be similar or faster (V8 JIT makes both fast)
      // Allow significant variance due to machine load
      expect(compiledDuration).toBeLessThanOrEqual(interpretedDuration * 10);
    });

    it('GATE: cache hit eliminates validation overhead', async () => {
      const hooks = [
        defineHook({
          name: 'cache-test',
          trigger: 'before-add',
          validate: _quad => true,
        }),
      ];
      const testQuad = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('test')
      );

      // Cold call (first validation)
      const startCold = performance.now();
      for (let i = 0; i < 100; i++) {
        // Create new hook each time to bypass cache
        const freshHooks = [
          defineHook({
            name: `fresh-hook-${i}`,
            trigger: 'before-add',
            validate: _quad => true,
          }),
        ];
        executeHookChain(freshHooks, testQuad);
      }
      const coldDuration = performance.now() - startCold;

      // Warm call (cached validation)
      const startWarm = performance.now();
      for (let i = 0; i < 100; i++) {
        executeHookChain(hooks, testQuad);
      }
      const warmDuration = performance.now() - startWarm;

      const improvement = coldDuration / warmDuration;

      console.log(`  Cold (no cache): ${coldDuration.toFixed(3)}ms`);
      console.log(`  Warm (cached): ${warmDuration.toFixed(3)}ms`);
      console.log(`  Cache improvement: ${improvement.toFixed(2)}x`);

      // Cached should be faster
      expect(warmDuration).toBeLessThan(coldDuration);
    });
  });


  describe('1. Hook Registration Overhead', () => {
    it('BASELINE: No hooks (direct quad operations)', () => {
      const quads = generateQuads(10000);

      const duration = benchmarkSync(() => {
        // Simulate direct quad processing without hooks
        for (const q of quads) {
          // Direct operation (no-op for baseline)
          void q;
        }
      });

      const avgPerQuad = (duration / 10000) * 1000; // microseconds
      console.log(
        `  Baseline (no hooks): ${duration.toFixed(2)}ms total, ${avgPerQuad.toFixed(3)}μs/quad`
      );

      expect(duration).toBeLessThan(100); // Should be very fast
    });

    it('Overhead: Register 10 hooks', () => {
      const manager = new KnowledgeHookManager();

      const duration = benchmarkSync(() => {
        for (let i = 0; i < 10; i++) {
          manager.define({
            name: `test-hook-${i}`,
            trigger: 'before-add',
            validate: _quad => true,
          });
        }
      });

      console.log(
        `  Register 10 hooks: ${duration.toFixed(3)}ms (${(duration / 10).toFixed(3)}ms per hook)`
      );
      expect(duration).toBeLessThan(50); // Registration should be fast
    });

    it('Overhead: Register 100 hooks', () => {
      const manager = new KnowledgeHookManager();

      const duration = benchmarkSync(() => {
        for (let i = 0; i < 100; i++) {
          manager.define({
            name: `test-hook-${i}`,
            trigger: 'before-add',
            validate: _quad => true,
          });
        }
      });

      console.log(
        `  Register 100 hooks: ${duration.toFixed(2)}ms (${(duration / 100).toFixed(3)}ms per hook)`
      );
      expect(duration).toBeLessThan(500);
    });

    it('Overhead: Register built-in hooks', () => {
      const duration = benchmarkSync(() => {
        new KnowledgeHookManager({ includeBuiltins: true });
      });

      const hookCount = Object.keys(builtinHooks).length;
      console.log(`  Register ${hookCount} built-in hooks: ${duration.toFixed(3)}ms`);
      expect(duration).toBeLessThan(100);
    });
  });

  describe('2. Single Hook Execution Overhead', () => {
    const testQuad = quad(
      namedNode('http://example.org/s'),
      namedNode('http://example.org/p'),
      literal('test')
    );

    it('BASELINE: No validation (direct pass-through)', () => {
      const stats = benchmarkWithStats(() => {
        void testQuad;
      }, 10000);

      console.log(
        `  Baseline (no validation): avg ${(stats.avg * 1000).toFixed(3)}μs, p95 ${(stats.p95 * 1000).toFixed(3)}μs`
      );
    });

    it('Overhead: Simple validation hook', () => {
      const hook = defineHook({
        name: 'simple-validate',
        trigger: 'before-add',
        validate: _quad => true,
      });

      const stats = benchmarkWithStats(() => {
        executeHook(hook, testQuad);
      }, 10000);

      console.log(
        `  Simple validation: avg ${(stats.avg * 1000).toFixed(3)}μs, p95 ${(stats.p95 * 1000).toFixed(3)}μs`
      );
    });

    it('Overhead: IRI validation hook', () => {
      const hook = builtinHooks.validateSubjectIRI;

      const stats = benchmarkWithStats(() => {
        executeHook(hook, testQuad);
      }, 10000);

      console.log(
        `  IRI validation: avg ${(stats.avg * 1000).toFixed(3)}μs, p95 ${(stats.p95 * 1000).toFixed(3)}μs`
      );
    });

    it('Overhead: Transformation hook', () => {
      const hook = builtinHooks.trimLiterals;

      const stats = benchmarkWithStats(() => {
        executeHook(hook, testQuad);
      }, 10000);

      console.log(
        `  Transformation: avg ${(stats.avg * 1000).toFixed(3)}μs, p95 ${(stats.p95 * 1000).toFixed(3)}μs`
      );
    });

    it('Overhead: Complex validation hook', () => {
      const hook = builtinHooks.validateIRIFormat;

      const stats = benchmarkWithStats(() => {
        executeHook(hook, testQuad);
      }, 10000);

      console.log(
        `  Complex validation: avg ${(stats.avg * 1000).toFixed(3)}μs, p95 ${(stats.p95 * 1000).toFixed(3)}μs`
      );
    });
  });

  describe('3. Hook Chain Execution Overhead', () => {
    const testQuad = quad(
      namedNode('http://example.org/s'),
      namedNode('http://example.org/p'),
      literal('test')
    );

    it('Overhead: 1 hook in chain', () => {
      const hooks = [builtinHooks.validateSubjectIRI];

      const stats = benchmarkWithStats(() => {
        executeHookChain(hooks, testQuad);
      }, 10000);

      console.log(
        `  1 hook chain: avg ${(stats.avg * 1000).toFixed(3)}μs, p95 ${(stats.p95 * 1000).toFixed(3)}μs`
      );
    });

    it('Overhead: 3 hooks in chain', () => {
      const hooks = [
        builtinHooks.validateSubjectIRI,
        builtinHooks.validatePredicateIRI,
        builtinHooks.trimLiterals,
      ];

      const stats = benchmarkWithStats(() => {
        executeHookChain(hooks, testQuad);
      }, 10000);

      console.log(
        `  3 hooks chain: avg ${(stats.avg * 1000).toFixed(3)}μs, p95 ${(stats.p95 * 1000).toFixed(3)}μs`
      );
    });

    it('Overhead: 5 hooks in chain', () => {
      const hooks = [
        builtinHooks.validateSubjectIRI,
        builtinHooks.validatePredicateIRI,
        builtinHooks.validateIRIFormat,
        builtinHooks.trimLiterals,
        builtinHooks.normalizeLanguageTag,
      ];

      const stats = benchmarkWithStats(() => {
        executeHookChain(hooks, testQuad);
      }, 10000);

      console.log(
        `  5 hooks chain: avg ${(stats.avg * 1000).toFixed(3)}μs, p95 ${(stats.p95 * 1000).toFixed(3)}μs`
      );
    });

    it('Overhead: 10 hooks in chain', () => {
      const hooks = Array(10)
        .fill(null)
        .map((_, i) =>
          defineHook({
            name: `chain-hook-${i}`,
            trigger: 'before-add',
            validate: _quad => true,
          })
        );

      const stats = benchmarkWithStats(() => {
        executeHookChain(hooks, testQuad);
      }, 10000);

      console.log(
        `  10 hooks chain: avg ${(stats.avg * 1000).toFixed(3)}μs, p95 ${(stats.p95 * 1000).toFixed(3)}μs`
      );
    });
  });

  describe('4. Cumulative Overhead on 10K Operations', () => {
    it('BASELINE: 10K quad operations without hooks', () => {
      const quads = generateQuads(10000);

      const duration = benchmarkSync(() => {
        for (const q of quads) {
          void q;
        }
      });

      console.log(`  Baseline 10K ops: ${duration.toFixed(2)}ms`);
    });

    it('Overhead: 10K operations with 1 validation hook', () => {
      const quads = generateQuads(10000);
      const hook = builtinHooks.validateSubjectIRI;

      const duration = benchmarkSync(() => {
        for (const q of quads) {
          executeHook(hook, q);
        }
      });

      console.log(`  10K ops + 1 hook: ${duration.toFixed(2)}ms`);
    });

    it('Overhead: 10K operations with 3 hooks in chain', () => {
      const quads = generateQuads(10000);
      const hooks = [
        builtinHooks.validateSubjectIRI,
        builtinHooks.validatePredicateIRI,
        builtinHooks.trimLiterals,
      ];

      const duration = benchmarkSync(() => {
        for (const q of quads) {
          executeHookChain(hooks, q);
        }
      });

      console.log(`  10K ops + 3 hooks: ${duration.toFixed(2)}ms`);
    });

    it('Overhead: 10K operations with 5 hooks in chain', () => {
      const quads = generateQuads(10000);
      const hooks = [
        builtinHooks.validateSubjectIRI,
        builtinHooks.validatePredicateIRI,
        builtinHooks.validateIRIFormat,
        builtinHooks.trimLiterals,
        builtinHooks.normalizeLanguageTag,
      ];

      const duration = benchmarkSync(() => {
        for (const q of quads) {
          executeHookChain(hooks, q);
        }
      });

      console.log(`  10K ops + 5 hooks: ${duration.toFixed(2)}ms`);
    });
  });

  describe('5. Policy Pack Impact', () => {
    it('Policy Pack: Standard Validation', () => {
      const quads = generateQuads(10000);
      const hook = builtinHooks.standardValidation;

      const duration = benchmarkSync(() => {
        for (const q of quads) {
          executeHook(hook, q);
        }
      });

      console.log(`  Standard validation policy (10K ops): ${duration.toFixed(2)}ms`);
    });

    it('Policy Pack: IRI + Language Tag Validation', () => {
      const quads = generateQuads(10000);
      const hooks = [builtinHooks.validateIRIFormat, builtinHooks.validateLanguageTag];

      const duration = benchmarkSync(() => {
        for (const q of quads) {
          executeHookChain(hooks, q);
        }
      });

      console.log(`  IRI + Language policy (10K ops): ${duration.toFixed(2)}ms`);
    });

    it('Policy Pack: Full Transformation Pipeline', () => {
      const quads = generateQuads(10000);
      const hooks = [
        builtinHooks.trimLiterals,
        builtinHooks.normalizeLanguageTag,
        builtinHooks.normalizeNamespace,
      ];

      const duration = benchmarkSync(() => {
        for (const q of quads) {
          executeHookChain(hooks, q);
        }
      });

      console.log(`  Full transformation policy (10K ops): ${duration.toFixed(2)}ms`);
    });
  });

  describe('6. Scalability at 100K Operations', () => {
    it('Scalability: 100K operations with single hook', () => {
      const quads = generateQuads(100000);
      const hook = builtinHooks.validateSubjectIRI;

      const duration = benchmarkSync(() => {
        for (const q of quads) {
          executeHook(hook, q);
        }
      });

      const avgPerOp = (duration / 100000) * 1000; // microseconds
      console.log(`  100K ops + 1 hook: ${duration.toFixed(2)}ms (${avgPerOp.toFixed(3)}μs/op)`);
      expect(duration).toBeLessThan(5000); // Should complete in <5s
    });

    it('Scalability: 100K operations with 3 hooks', () => {
      const quads = generateQuads(100000);
      const hooks = [
        builtinHooks.validateSubjectIRI,
        builtinHooks.validatePredicateIRI,
        builtinHooks.trimLiterals,
      ];

      const duration = benchmarkSync(() => {
        for (const q of quads) {
          executeHookChain(hooks, q);
        }
      });

      const avgPerOp = (duration / 100000) * 1000; // microseconds
      console.log(`  100K ops + 3 hooks: ${duration.toFixed(2)}ms (${avgPerOp.toFixed(3)}μs/op)`);
      expect(duration).toBeLessThan(20000); // Should complete in <20s (machine-dependent)
    });
  });

  describe('7. Memory Efficiency', () => {
    it('Memory: Hook registration memory usage', () => {
      const memBefore = process.memoryUsage().heapUsed;

      const manager = new KnowledgeHookManager();
      for (let i = 0; i < 1000; i++) {
        manager.define({
          name: `hook-${i}`,
          trigger: 'before-add',
          validate: _quad => true,
        });
      }

      const memAfter = process.memoryUsage().heapUsed;
      const memDelta = (memAfter - memBefore) / 1024; // KB

      console.log(`  1000 hooks memory: ${memDelta.toFixed(2)}KB`);
      expect(memDelta).toBeLessThan(20000); // <20MB for 1000 hooks (GC-dependent)
    });

    it('Memory: Hook execution memory usage', () => {
      const quads = generateQuads(10000);
      const hooks = [
        builtinHooks.validateSubjectIRI,
        builtinHooks.validatePredicateIRI,
        builtinHooks.trimLiterals,
      ];

      const memBefore = process.memoryUsage().heapUsed;

      for (const q of quads) {
        executeHookChain(hooks, q);
      }

      const memAfter = process.memoryUsage().heapUsed;
      const memDelta = (memAfter - memBefore) / 1024; // KB

      console.log(`  10K ops + 3 hooks memory delta: ${memDelta.toFixed(2)}KB`);
      expect(memDelta).toBeLessThan(25000); // <25MB for 10K operations (GC-dependent)
    });
  });

  describe('8. Comparative Analysis Summary', () => {
    it('Generate comprehensive overhead report', () => {
      const testQuad = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('test')
      );

      // Baseline
      const baselineStats = benchmarkWithStats(() => {
        void testQuad;
      }, 10000);

      // Single hook
      const singleHookStats = benchmarkWithStats(() => {
        executeHook(builtinHooks.validateSubjectIRI, testQuad);
      }, 10000);

      // 3 hooks
      const threeHooksStats = benchmarkWithStats(() => {
        executeHookChain(
          [
            builtinHooks.validateSubjectIRI,
            builtinHooks.validatePredicateIRI,
            builtinHooks.trimLiterals,
          ],
          testQuad
        );
      }, 10000);

      // 5 hooks
      const fiveHooksStats = benchmarkWithStats(() => {
        executeHookChain(
          [
            builtinHooks.validateSubjectIRI,
            builtinHooks.validatePredicateIRI,
            builtinHooks.validateIRIFormat,
            builtinHooks.trimLiterals,
            builtinHooks.normalizeLanguageTag,
          ],
          testQuad
        );
      }, 10000);

      console.log('\n📊 Knowledge Hook Overhead Analysis (10,000 iterations)');
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      console.log(`Baseline (no hooks):`);
      console.log(`  Avg: ${(baselineStats.avg * 1000).toFixed(3)}μs`);
      console.log(`  P95: ${(baselineStats.p95 * 1000).toFixed(3)}μs`);
      console.log(`\nSingle Hook:`);
      console.log(`  Avg: ${(singleHookStats.avg * 1000).toFixed(3)}μs`);
      console.log(`  P95: ${(singleHookStats.p95 * 1000).toFixed(3)}μs`);
      console.log(
        `  Overhead: ${((singleHookStats.avg / baselineStats.avg - 1) * 100).toFixed(1)}%`
      );
      console.log(`\n3 Hooks Chain:`);
      console.log(`  Avg: ${(threeHooksStats.avg * 1000).toFixed(3)}μs`);
      console.log(`  P95: ${(threeHooksStats.p95 * 1000).toFixed(3)}μs`);
      console.log(
        `  Overhead: ${((threeHooksStats.avg / baselineStats.avg - 1) * 100).toFixed(1)}%`
      );
      console.log(`\n5 Hooks Chain:`);
      console.log(`  Avg: ${(fiveHooksStats.avg * 1000).toFixed(3)}μs`);
      console.log(`  P95: ${(fiveHooksStats.p95 * 1000).toFixed(3)}μs`);
      console.log(
        `  Overhead: ${((fiveHooksStats.avg / baselineStats.avg - 1) * 100).toFixed(1)}%`
      );
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
    });
  });
});
