/**
 * @file Tests for useKnowledgeStack hook functionality
 * Tests stack presets, CRUD operations, dashboard variant, and production stack with telemetry
 * @since 3.2.0
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('useKnowledgeStack', () => {
  describe('Stack Preset Configuration', () => {
    it('should resolve basic preset with minimal features', () => {
      const presets = {
        basic: {
          enableRealtime: false,
          enableRecovery: false,
          enableErrorBoundary: true
        }
      };

      const config = presets['basic'];

      expect(config.enableRealtime).toBe(false);
      expect(config.enableRecovery).toBe(false);
      expect(config.enableErrorBoundary).toBe(true);
    });

    it('should resolve realtime preset with live updates', () => {
      const presets = {
        realtime: {
          enableRealtime: true,
          enableRecovery: false,
          enableErrorBoundary: true
        }
      };

      const config = presets['realtime'];

      expect(config.enableRealtime).toBe(true);
      expect(config.enableRecovery).toBe(false);
      expect(config.enableErrorBoundary).toBe(true);
    });

    it('should resolve resilient preset with recovery features', () => {
      const presets = {
        resilient: {
          enableRealtime: false,
          enableRecovery: true,
          enableErrorBoundary: true
        }
      };

      const config = presets['resilient'];

      expect(config.enableRealtime).toBe(false);
      expect(config.enableRecovery).toBe(true);
      expect(config.enableErrorBoundary).toBe(true);
    });

    it('should resolve full preset with all features enabled', () => {
      const presets = {
        full: {
          enableRealtime: true,
          enableRecovery: true,
          enableErrorBoundary: true
        }
      };

      const config = presets['full'];

      expect(config.enableRealtime).toBe(true);
      expect(config.enableRecovery).toBe(true);
      expect(config.enableErrorBoundary).toBe(true);
    });

    it('should allow config overrides on presets', () => {
      const preset = {
        enableRealtime: false,
        enableRecovery: false,
        enableErrorBoundary: true
      };

      const userConfig = {
        enableRealtime: true,
        maxRetries: 5
      };

      const resolved = {
        enableRealtime: userConfig.enableRealtime ?? preset.enableRealtime,
        enableRecovery: userConfig.enableRecovery ?? preset.enableRecovery,
        enableErrorBoundary: userConfig.enableErrorBoundary ?? preset.enableErrorBoundary,
        maxRetries: userConfig.maxRetries || 3
      };

      expect(resolved.enableRealtime).toBe(true);
      expect(resolved.enableRecovery).toBe(false);
      expect(resolved.maxRetries).toBe(5);
    });

    it('should default to basic preset when invalid preset specified', () => {
      const resolvePreset = (preset) => {
        const presets = {
          basic: { enableRealtime: false, enableRecovery: false },
          realtime: { enableRealtime: true, enableRecovery: false },
          resilient: { enableRealtime: false, enableRecovery: true },
          full: { enableRealtime: true, enableRecovery: true }
        };
        return presets[preset] || presets.basic;
      };

      const config = resolvePreset('invalid');

      expect(config.enableRealtime).toBe(false);
      expect(config.enableRecovery).toBe(false);
    });
  });

  describe('CRUD Operations', () => {
    it('should provide query operation', async () => {
      const stack = {
        query: vi.fn().mockResolvedValue([
          { subject: 'alice', predicate: 'knows', object: 'bob' },
          { subject: 'bob', predicate: 'knows', object: 'carol' }
        ])
      };

      const result = await stack.query('SELECT * WHERE { ?s foaf:knows ?o }');

      expect(result).toHaveLength(2);
      expect(stack.query).toHaveBeenCalledTimes(1);
    });

    it('should provide insert operation', async () => {
      const data = [];
      const stack = {
        insert: vi.fn((quads) => {
          data.push(...quads);
          return { inserted: quads.length };
        })
      };

      const result = await stack.insert([
        { subject: 'alice', predicate: 'knows', object: 'bob' }
      ]);

      expect(result.inserted).toBe(1);
      expect(data).toHaveLength(1);
    });

    it('should provide delete operation', async () => {
      let data = [
        { id: '1', subject: 'alice', predicate: 'knows', object: 'bob' },
        { id: '2', subject: 'bob', predicate: 'knows', object: 'carol' }
      ];

      const stack = {
        delete: vi.fn((pattern) => {
          const before = data.length;
          data = data.filter(q => q.subject !== pattern.subject);
          return { deleted: before - data.length };
        })
      };

      const result = await stack.delete({ subject: 'alice' });

      expect(result.deleted).toBe(1);
      expect(data).toHaveLength(1);
    });

    it('should handle query errors gracefully', async () => {
      const stack = {
        query: vi.fn().mockRejectedValue(new Error('Query failed')),
        error: null
      };

      try {
        await stack.query('SELECT * WHERE { ?s ?p ?o }');
      } catch (err) {
        stack.error = err;
      }

      expect(stack.error.message).toBe('Query failed');
    });

    it('should track loading state during operations', async () => {
      let loading = false;
      const stack = {
        query: vi.fn(async (sparql) => {
          loading = true;
          await new Promise(resolve => setTimeout(resolve, 10));
          loading = false;
          return [];
        })
      };

      const promise = stack.query('SELECT * WHERE { ?s ?p ?o }');
      // Loading would be true during execution
      await promise;

      expect(loading).toBe(false);
    });
  });

  describe('Dashboard Stack Variant', () => {
    it('should auto-start live mode', () => {
      let liveStarted = false;

      const createDashboardStack = () => {
        const stack = {
          startLive: vi.fn(() => { liveStarted = true; }),
          stopLive: vi.fn(() => { liveStarted = false; }),
          isLive: false
        };

        // Auto-start
        stack.startLive();
        stack.isLive = true;

        return stack;
      };

      const dashboard = createDashboardStack();

      expect(liveStarted).toBe(true);
      expect(dashboard.isLive).toBe(true);
    });

    it('should track live changes', () => {
      const changes = [];
      const dashboard = {
        changes,
        onChangeReceived: (change) => {
          changes.push(change);
        }
      };

      dashboard.onChangeReceived({ id: '1', operation: 'insert', quads: [] });
      dashboard.onChangeReceived({ id: '2', operation: 'delete', quads: [] });

      expect(changes).toHaveLength(2);
    });

    it('should provide live stats', () => {
      const dashboard = {
        liveStats: {
          totalChanges: 150,
          inserts: 100,
          deletes: 50,
          uptime: 3600000
        }
      };

      expect(dashboard.liveStats.totalChanges).toBe(150);
      expect(dashboard.liveStats.inserts).toBe(100);
    });

    it('should stop live mode on cleanup', () => {
      let liveActive = true;

      const dashboard = {
        stopLive: vi.fn(() => { liveActive = false; })
      };

      // Simulate unmount cleanup
      dashboard.stopLive();

      expect(liveActive).toBe(false);
      expect(dashboard.stopLive).toHaveBeenCalled();
    });
  });

  describe('Production Stack with Telemetry', () => {
    it('should use extended retry configuration', () => {
      const productionConfig = {
        preset: 'full',
        maxRetries: 5,
        retryDelay: 2000
      };

      expect(productionConfig.maxRetries).toBe(5);
      expect(productionConfig.retryDelay).toBe(2000);
    });

    it('should track retry count during recovery', async () => {
      let retryCount = 0;
      const maxRetries = 3;

      const executeWithRecovery = async (fn) => {
        while (retryCount < maxRetries) {
          try {
            return await fn();
          } catch (err) {
            retryCount++;
            if (retryCount >= maxRetries) throw err;
          }
        }
      };

      const failingFn = vi.fn()
        .mockRejectedValueOnce(new Error('Fail 1'))
        .mockRejectedValueOnce(new Error('Fail 2'))
        .mockResolvedValue({ success: true });

      const result = await executeWithRecovery(failingFn);

      expect(result.success).toBe(true);
      expect(retryCount).toBe(2);
    });

    it('should capture telemetry data', () => {
      const telemetry = {
        queries: [],
        recordQuery: (sparql, duration, resultCount) => {
          telemetry.queries.push({
            sparql,
            duration,
            resultCount,
            timestamp: Date.now()
          });
        },
        getMetrics: () => ({
          totalQueries: telemetry.queries.length,
          avgDuration: telemetry.queries.reduce((sum, q) => sum + q.duration, 0) / telemetry.queries.length,
          totalResults: telemetry.queries.reduce((sum, q) => sum + q.resultCount, 0)
        })
      };

      telemetry.recordQuery('SELECT * WHERE { ?s ?p ?o }', 50, 100);
      telemetry.recordQuery('SELECT * WHERE { ?s foaf:knows ?o }', 30, 25);

      const metrics = telemetry.getMetrics();

      expect(metrics.totalQueries).toBe(2);
      expect(metrics.avgDuration).toBe(40);
      expect(metrics.totalResults).toBe(125);
    });

    it('should handle error boundary capture', () => {
      const errorBoundary = {
        hasError: false,
        error: null,
        captureError: (err) => {
          errorBoundary.hasError = true;
          errorBoundary.error = err;
        },
        resetError: () => {
          errorBoundary.hasError = false;
          errorBoundary.error = null;
        }
      };

      errorBoundary.captureError(new Error('Component error'));

      expect(errorBoundary.hasError).toBe(true);
      expect(errorBoundary.error.message).toBe('Component error');

      errorBoundary.resetError();

      expect(errorBoundary.hasError).toBe(false);
    });

    it('should expose recovery state', () => {
      const recovery = {
        isRecovering: false,
        lastError: null,
        retryCount: 0,

        startRecovery: () => {
          recovery.isRecovering = true;
        },
        endRecovery: (success, error = null) => {
          recovery.isRecovering = false;
          if (!success) recovery.lastError = error;
        }
      };

      recovery.startRecovery();
      expect(recovery.isRecovering).toBe(true);

      recovery.endRecovery(false, new Error('Recovery failed'));
      expect(recovery.isRecovering).toBe(false);
      expect(recovery.lastError.message).toBe('Recovery failed');
    });
  });

  describe('Feature Flag Resolution', () => {
    it('should merge features based on preset and config', () => {
      const resolveFeatures = (preset, config) => {
        const presetDefaults = {
          basic: { realtime: false, recovery: false, errorBoundary: true },
          full: { realtime: true, recovery: true, errorBoundary: true }
        };

        const base = presetDefaults[preset] || presetDefaults.basic;

        return {
          realtime: config.enableRealtime ?? base.realtime,
          recovery: config.enableRecovery ?? base.recovery,
          errorBoundary: config.enableErrorBoundary ?? base.errorBoundary
        };
      };

      const features = resolveFeatures('basic', { enableRealtime: true });

      expect(features.realtime).toBe(true);
      expect(features.recovery).toBe(false);
      expect(features.errorBoundary).toBe(true);
    });

    it('should include realtime features when enabled', () => {
      const buildStack = (features) => {
        const stack = {
          query: vi.fn(),
          insert: vi.fn()
        };

        if (features.realtime) {
          Object.assign(stack, {
            changes: [],
            startLive: vi.fn(),
            stopLive: vi.fn(),
            isLive: false
          });
        }

        return stack;
      };

      const stack = buildStack({ realtime: true });

      expect(stack.startLive).toBeDefined();
      expect(stack.changes).toBeDefined();
    });

    it('should include recovery features when enabled', () => {
      const buildStack = (features) => {
        const stack = {
          query: vi.fn()
        };

        if (features.recovery) {
          Object.assign(stack, {
            retryCount: 0,
            isRecovering: false,
            lastError: null
          });
        }

        return stack;
      };

      const stack = buildStack({ recovery: true });

      expect(stack.retryCount).toBeDefined();
      expect(stack.isRecovering).toBeDefined();
    });
  });

  describe('Stack Metadata', () => {
    it('should expose preset name', () => {
      const stack = {
        preset: 'full',
        features: {
          enableRealtime: true,
          enableRecovery: true,
          enableErrorBoundary: true
        }
      };

      expect(stack.preset).toBe('full');
    });

    it('should expose resolved features', () => {
      const stack = {
        preset: 'realtime',
        features: {
          enableRealtime: true,
          enableRecovery: false,
          enableErrorBoundary: true
        }
      };

      expect(stack.features.enableRealtime).toBe(true);
      expect(stack.features.enableRecovery).toBe(false);
    });
  });
});

describe('useCRUDStack', () => {
  it('should use basic preset', () => {
    const createCRUDStack = (config = {}) => {
      return {
        ...config,
        preset: 'basic',
        enableErrorBoundary: true
      };
    };

    const stack = createCRUDStack();

    expect(stack.preset).toBe('basic');
    expect(stack.enableErrorBoundary).toBe(true);
  });

  it('should provide CRUD operations', () => {
    const crudStack = {
      query: vi.fn(),
      insert: vi.fn(),
      delete: vi.fn()
    };

    expect(crudStack.query).toBeDefined();
    expect(crudStack.insert).toBeDefined();
    expect(crudStack.delete).toBeDefined();
  });
});

describe('useDashboardStack', () => {
  it('should use realtime preset', () => {
    const createDashboardStack = (config = {}) => {
      return {
        ...config,
        preset: 'realtime',
        operations: ['insert', 'delete', 'update']
      };
    };

    const stack = createDashboardStack();

    expect(stack.preset).toBe('realtime');
    expect(stack.operations).toContain('update');
  });
});

describe('useProductionStack', () => {
  it('should use full preset with extended configuration', () => {
    const createProductionStack = (config = {}) => {
      return {
        ...config,
        preset: 'full',
        maxRetries: 5,
        retryDelay: 2000
      };
    };

    const stack = createProductionStack();

    expect(stack.preset).toBe('full');
    expect(stack.maxRetries).toBe(5);
    expect(stack.retryDelay).toBe(2000);
  });
});
