/**
 * @fileoverview Effect Sandbox Browser Unit Tests (CRITICAL - P1)
 * Tests secure hook execution in Web Workers
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  EffectSandbox,
  createEffectSandbox
} from '../../src/knowledge-engine/effect-sandbox-browser.mjs';

describe('effect-sandbox-browser.mjs (CRITICAL)', () => {
  let sandbox;

  beforeEach(() => {
    sandbox = new EffectSandbox({
      type: 'worker',
      timeout: 5000,
      memoryLimit: 10 * 1024 * 1024,
      allowedGlobals: ['console', 'Date', 'Math', 'JSON']
    });
  });

  afterEach(() => {
    sandbox?.clear();
  });

  describe('Sandbox Creation', () => {
    it('should create sandbox with default config', () => {
      const defaultSandbox = new EffectSandbox();
      expect(defaultSandbox).toBeInstanceOf(EffectSandbox);
      expect(defaultSandbox.config.type).toBe('worker');
      expect(defaultSandbox.config.timeout).toBe(5000);
      defaultSandbox.clear();
    });

    it('should create sandbox with custom config', () => {
      const customSandbox = new EffectSandbox({
        timeout: 10000,
        memoryLimit: 20 * 1024 * 1024,
        strictMode: false
      });

      expect(customSandbox.config.timeout).toBe(10000);
      expect(customSandbox.config.memoryLimit).toBe(20 * 1024 * 1024);
      expect(customSandbox.config.strictMode).toBe(false);
      customSandbox.clear();
    });

    it('should create sandbox via factory function', () => {
      const factorySandbox = createEffectSandbox({ timeout: 3000 });
      expect(factorySandbox).toBeInstanceOf(EffectSandbox);
      expect(factorySandbox.config.timeout).toBe(3000);
      factorySandbox.clear();
    });
  });

  describe('Effect Execution in Worker', () => {
    it('should execute simple effect successfully', async () => {
      const effect = (context) => {
        return context.value * 2;
      };

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {},
        value: 21
      });

      expect(result.success).toBe(true);
      expect(result.result).toBe(42);
      expect(result.duration).toBeGreaterThanOrEqual(0);
    });

    it('should execute async effect', async () => {
      const effect = async (context) => {
        await new Promise(resolve => setTimeout(resolve, 10));
        return 'async result';
      };

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(true);
      expect(result.result).toBe('async result');
    });

    it('should pass context to effect', async () => {
      const effect = (context) => {
        return {
          event: context.event.type,
          storeSize: context.store.size,
          deltaOp: context.delta.operation
        };
      };

      const result = await sandbox.executeEffect(effect, {
        event: { type: 'test-event' },
        store: { size: 100 },
        delta: { operation: 'add' }
      });

      expect(result.success).toBe(true);
      expect(result.result.event).toBe('test-event');
      expect(result.result.storeSize).toBe(100);
      expect(result.result.deltaOp).toBe('add');
    });

    it('should provide sandbox APIs to effect', async () => {
      const effect = (context, apis) => {
        apis.log('Test log message');
        apis.assert(true, 'Should not fail');
        apis.emitEvent({ type: 'custom-event' });
        return 'success';
      };

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(true);
      expect(result.result).toBe('success');
    });
  });

  describe('Timeout Enforcement', () => {
    it('should timeout long-running effect', async () => {
      const shortTimeoutSandbox = new EffectSandbox({ timeout: 100 });

      const effect = () => {
        // Infinite loop - should timeout
        while (true) {
          // Busy wait
        }
      };

      const result = await shortTimeoutSandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain('timeout');
      shortTimeoutSandbox.clear();
    });

    it('should complete effect within timeout', async () => {
      const effect = async () => {
        await new Promise(resolve => setTimeout(resolve, 50));
        return 'completed';
      };

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(true);
      expect(result.result).toBe('completed');
    });
  });

  describe('Security and Isolation', () => {
    it('should restrict access to disallowed globals', async () => {
      const effect = () => {
        // Try to access window (should be undefined in worker)
        return typeof window === 'undefined';
      };

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(true);
      expect(result.result).toBe(true);
    });

    it('should allow access to permitted globals', async () => {
      const effect = () => {
        return {
          hasMath: typeof Math !== 'undefined',
          hasJSON: typeof JSON !== 'undefined',
          hasDate: typeof Date !== 'undefined',
          hasConsole: typeof console !== 'undefined'
        };
      };

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(true);
      expect(result.result.hasMath).toBe(true);
      expect(result.result.hasJSON).toBe(true);
      expect(result.result.hasDate).toBe(true);
      expect(result.result.hasConsole).toBe(true);
    });

    it('should isolate worker from main thread state', async () => {
      // Set global state in main thread
      globalThis.testValue = 'main-thread';

      const effect = () => {
        // Should not have access to main thread globals
        return typeof globalThis.testValue === 'undefined';
      };

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(true);
      expect(result.result).toBe(true);

      delete globalThis.testValue;
    });

    it('should prevent filesystem access in worker', async () => {
      const effect = () => {
        // Workers should not have file system access
        return typeof require === 'undefined';
      };

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(true);
      expect(result.result).toBe(true);
    });
  });

  describe('Error Handling', () => {
    it('should catch and report effect errors', async () => {
      const effect = () => {
        throw new Error('Effect execution error');
      };

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain('Effect execution error');
    });

    it('should handle assertion failures', async () => {
      const effect = (context, apis) => {
        apis.assert(false, 'Assertion should fail');
      };

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain('Assertion');
    });

    it('should validate context schema', async () => {
      const effect = () => 'test';

      const result = await sandbox.executeEffect(effect, {
        // Missing required fields
        event: null,
        store: null,
        delta: null
      });

      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });

    it('should handle worker initialization errors', async () => {
      const effect = () => {
        // Cause an error during worker setup
        undefined.property;
      };

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });
  });

  describe('Statistics and Monitoring', () => {
    it('should track execution statistics', async () => {
      const effect = () => 'test';

      await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      const stats = sandbox.getStats();
      expect(stats.totalExecutions).toBe(1);
      expect(stats.averageDuration).toBeGreaterThanOrEqual(0);
      expect(stats.type).toBe('worker');
    });

    it('should track multiple executions', async () => {
      const effect = () => 'test';

      for (let i = 0; i < 5; i++) {
        await sandbox.executeEffect(effect, {
          event: {},
          store: {},
          delta: {}
        });
      }

      const stats = sandbox.getStats();
      expect(stats.totalExecutions).toBe(5);
      expect(stats.averageDuration).toBeGreaterThanOrEqual(0);
    });

    it('should report active worker count', async () => {
      const stats = sandbox.getStats();
      expect(stats.activeWorkers).toBeGreaterThanOrEqual(0);
    });
  });

  describe('Cleanup and Resource Management', () => {
    it('should clean up workers after execution', async () => {
      const effect = () => 'test';

      await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      // Give time for cleanup
      await new Promise(resolve => setTimeout(resolve, 100));

      const stats = sandbox.getStats();
      expect(stats.activeWorkers).toBe(0);
    });

    it('should clear all sandbox state', async () => {
      const effect = () => 'test';

      await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      sandbox.clear();

      const stats = sandbox.getStats();
      expect(stats.totalExecutions).toBe(0);
      expect(stats.activeWorkers).toBe(0);
    });
  });

  describe('Performance Benchmarks', () => {
    it('should have minimal worker overhead', async () => {
      const effect = () => 42;

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.duration).toBeLessThan(50); // <50ms overhead
    });

    it('should handle concurrent executions', async () => {
      const effect = (context) => context.value;

      const promises = [];
      for (let i = 0; i < 10; i++) {
        promises.push(
          sandbox.executeEffect(effect, {
            event: {},
            store: {},
            delta: {},
            value: i
          })
        );
      }

      const results = await Promise.all(promises);

      expect(results).toHaveLength(10);
      results.forEach((result, i) => {
        expect(result.success).toBe(true);
        expect(result.result).toBe(i);
      });
    });

    it('should execute many effects performantly', async () => {
      const effect = () => 'test';
      const iterations = 100;

      const start = performance.now();

      for (let i = 0; i < iterations; i++) {
        await sandbox.executeEffect(effect, {
          event: {},
          store: {},
          delta: {}
        });
      }

      const duration = performance.now() - start;
      const avgPerExecution = duration / iterations;

      expect(avgPerExecution).toBeLessThan(100); // <100ms per execution
    });
  });

  describe('Global Mode (Fallback)', () => {
    it('should execute in global mode when configured', async () => {
      const globalSandbox = new EffectSandbox({ type: 'global' });

      const effect = (context) => context.value * 2;

      const result = await globalSandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {},
        value: 10
      });

      expect(result.success).toBe(true);
      expect(result.result).toBe(20);
      globalSandbox.clear();
    });

    it('should handle async effects in global mode', async () => {
      const globalSandbox = new EffectSandbox({ type: 'global' });

      const effect = async (context) => {
        await new Promise(resolve => setTimeout(resolve, 10));
        return 'async-global';
      };

      const result = await globalSandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(true);
      expect(result.result).toBe('async-global');
      globalSandbox.clear();
    });
  });

  describe('Edge Cases', () => {
    it('should handle effects returning null/undefined', async () => {
      const effect = () => null;

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(true);
      expect(result.result).toBeNull();
    });

    it('should handle effects returning complex objects', async () => {
      const effect = () => ({
        nested: {
          value: 42,
          array: [1, 2, 3],
          map: { key: 'value' }
        }
      });

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(true);
      expect(result.result.nested.value).toBe(42);
      expect(result.result.nested.array).toEqual([1, 2, 3]);
    });

    it('should handle empty context', async () => {
      const effect = (context) => {
        return Object.keys(context).length;
      };

      const result = await sandbox.executeEffect(effect, {
        event: {},
        store: {},
        delta: {}
      });

      expect(result.success).toBe(true);
      expect(result.result).toBeGreaterThanOrEqual(3);
    });
  });
});
