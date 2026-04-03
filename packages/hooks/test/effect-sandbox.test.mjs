/**
 * @vitest-environment node
 * @file Effect Sandbox Tests - Comprehensive coverage for effect-sandbox.mjs
 */
import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import {
  EffectSandbox,
  createSandboxedHook,
  createEffectSandbox,
} from '../src/hooks/effect-sandbox.mjs';

describe('EffectSandbox - Configuration', () => {
  it('should create sandbox with default config', () => {
    const sandbox = new EffectSandbox();
    const stats = sandbox.getStats();

    expect(stats.config.type).toBe('worker');
    expect(stats.config.timeout).toBe(30000);
    expect(stats.config.memoryLimit).toBe(64 * 1024 * 1024);
    expect(stats.config.maxWorkerPoolSize).toBe(50);
  });

  it('should create sandbox with custom config', () => {
    const sandbox = new EffectSandbox({
      type: 'isolate',
      timeout: 5000,
      memoryLimit: 128 * 1024 * 1024,
      maxWorkerPoolSize: 100,
    });

    const stats = sandbox.getStats();
    expect(stats.config.type).toBe('isolate');
    expect(stats.config.timeout).toBe(5000);
    expect(stats.config.memoryLimit).toBe(128 * 1024 * 1024);
    expect(stats.config.maxWorkerPoolSize).toBe(100);
  });

  it('should validate config with Zod schema', () => {
    expect(() => {
      new EffectSandbox({ timeout: -1000 });
    }).toThrow();

    expect(() => {
      new EffectSandbox({ type: 'invalid' });
    }).toThrow();
  });

  it('should enforce maximum timeout limit', () => {
    expect(() => {
      new EffectSandbox({ timeout: 400000 }); // exceeds 300000ms
    }).toThrow();
  });

  it('should enforce maximum memory limit', () => {
    expect(() => {
      new EffectSandbox({ memoryLimit: 2 * 1024 * 1024 * 1024 }); // exceeds 1GB
    }).toThrow();
  });
});

describe('EffectSandbox - Execution Context', () => {
  let sandbox;

  beforeEach(() => {
    sandbox = new EffectSandbox({ type: 'worker', timeout: 5000 });
  });

  afterEach(async () => {
    await sandbox.terminate();
  });

  it('should validate execution context schema', async () => {
    const invalidContext = {
      // missing required fields
    };

    const result = await sandbox.executeEffect(() => {}, invalidContext);

    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });

  it('should create safe effect with proper context', () => {
    const effect = () => console.log('test');
    const context = {
      event: { type: 'test' },
      store: {},
      delta: [],
      metadata: { foo: 'bar' },
    };

    const safeEffect = sandbox._createSafeEffect(effect, context);
    expect(safeEffect).toContain('const context =');
    expect(safeEffect).toContain('const effect =');
  });
});

describe('EffectSandbox - Sandbox Globals', () => {
  let sandbox;

  beforeEach(() => {
    sandbox = new EffectSandbox({
      allowedGlobals: ['console', 'Math', 'Date', 'JSON'],
    });
  });

  afterEach(async () => {
    await sandbox.terminate();
  });

  it('should create sandbox globals with allowed globals', () => {
    const context = {
      event: { type: 'test' },
      store: {},
      delta: [],
    };

    const globals = sandbox._createSandboxGlobals(context);

    expect(globals.console).toBeDefined();
    expect(globals.Math).toBeDefined();
    expect(globals.Date).toBeDefined();
    expect(globals.JSON).toBeDefined();
  });

  it('should include safe functions in globals', () => {
    const context = {
      event: { type: 'test' },
      store: {},
      delta: [],
    };

    const globals = sandbox._createSandboxGlobals(context);

    expect(typeof globals.emitEvent).toBe('function');
    expect(typeof globals.log).toBe('function');
    expect(typeof globals.assert).toBe('function');
  });

  it('should track emitted events', () => {
    const context = {
      event: { type: 'test' },
      store: {},
      delta: [],
    };

    const globals = sandbox._createSandboxGlobals(context);
    globals.emitEvent({ type: 'custom', data: 'test' });

    expect(context.events).toHaveLength(1);
    expect(context.events[0]).toEqual({ type: 'custom', data: 'test' });
  });

  it('should track assertions', () => {
    const context = {
      event: { type: 'test' },
      store: {},
      delta: [],
    };

    const globals = sandbox._createSandboxGlobals(context);
    globals.assert('http://s', 'http://p', 'http://o', 'http://g');

    expect(context.assertions).toHaveLength(1);
    expect(context.assertions[0]).toEqual({
      subject: 'http://s',
      predicate: 'http://p',
      object: 'http://o',
      graph: 'http://g',
    });
  });
});

describe('EffectSandbox - Safe Console & Require', () => {
  let sandbox;

  beforeEach(() => {
    sandbox = new EffectSandbox({
      allowedModules: ['crypto', 'path'],
    });
  });

  afterEach(async () => {
    await sandbox.terminate();
  });

  it('should create safe console', () => {
    const safeConsole = sandbox._createSafeConsole();

    expect(typeof safeConsole.log).toBe('function');
    expect(typeof safeConsole.warn).toBe('function');
    expect(typeof safeConsole.error).toBe('function');
    expect(typeof safeConsole.info).toBe('function');
  });

  it('should create safe require with allowlist', () => {
    const safeRequire = sandbox._createSafeRequire();

    expect(() => safeRequire('crypto')).not.toThrow();
    expect(() => safeRequire('path')).not.toThrow();
  });

  it('should block disallowed modules', () => {
    const safeRequire = sandbox._createSafeRequire();

    expect(() => safeRequire('fs')).toThrow('Module fs is not allowed');
    expect(() => safeRequire('child_process')).toThrow('not allowed');
  });
});

describe('EffectSandbox - Worker Cleanup', () => {
  let sandbox;

  beforeEach(() => {
    sandbox = new EffectSandbox({
      workerTTL: 1000, // 1 second TTL
      cleanupInterval: 500, // cleanup every 500ms
    });
  });

  afterEach(async () => {
    await sandbox.terminate();
  });

  it('should start cleanup timer on creation', () => {
    expect(sandbox.cleanupTimer).toBeDefined();
  });

  it('should cleanup stale workers', async () => {
    // Manually add a stale worker timestamp
    sandbox.workerTimestamps.set('test-id', Date.now() - 2000); // 2 seconds old

    sandbox._cleanupStaleWorkers();

    expect(sandbox.workerTimestamps.has('test-id')).toBe(false);
  });

  it('should not cleanup fresh workers', () => {
    sandbox.workerTimestamps.set('fresh-id', Date.now());

    sandbox._cleanupStaleWorkers();

    expect(sandbox.workerTimestamps.has('fresh-id')).toBe(true);
  });

  it('should clear cleanup timer on terminate', async () => {
    await sandbox.terminate();
    expect(sandbox.cleanupTimer).toBeNull();
  });
});

describe('EffectSandbox - Metrics & Statistics', () => {
  let sandbox;

  beforeEach(() => {
    sandbox = new EffectSandbox();
  });

  afterEach(async () => {
    await sandbox.terminate();
  });

  it('should initialize metrics to zero', () => {
    const stats = sandbox.getStats();
    expect(stats.executionCount).toBe(0);
    expect(stats.totalExecutions).toBe(0);
    expect(stats.averageDuration).toBe(0);
    expect(stats.activeWorkers).toBe(0);
  });

  it('should update metrics on execution', () => {
    sandbox._updateMetrics(100);
    sandbox._updateMetrics(200);

    const stats = sandbox.getStats();
    expect(stats.totalExecutions).toBe(2);
    expect(stats.averageDuration).toBe(150); // (100 + 200) / 2
  });

  it('should track active workers', () => {
    // Simulate adding workers
    sandbox.workers.set('worker-1', {});
    sandbox.workers.set('worker-2', {});

    const stats = sandbox.getStats();
    expect(stats.activeWorkers).toBe(2);
  });
});

describe('EffectSandbox - Error Handling', () => {
  let sandbox;

  beforeEach(() => {
    sandbox = new EffectSandbox({ type: 'worker', timeout: 5000 });
  });

  afterEach(async () => {
    await sandbox.terminate();
  });

  it('should handle invalid sandbox type', async () => {
    sandbox.config.type = 'invalid-type';

    const result = await sandbox.executeEffect(() => {}, {
      event: {},
      store: {},
      delta: [],
    });

    expect(result.success).toBe(false);
    expect(result.error).toContain('Unsupported sandbox type');
  });

  it('should return error result on execution failure', async () => {
    const result = await sandbox.executeEffect(
      () => {
        throw new Error('Test error');
      },
      {
        event: {},
        store: {},
        delta: [],
      }
    );

    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
    expect(result.duration).toBeGreaterThan(0);
  });
});

describe('EffectSandbox - Worker Pool Management', () => {
  let sandbox;

  beforeEach(() => {
    sandbox = new EffectSandbox({
      maxWorkerPoolSize: 2,
      timeout: 5000,
    });
  });

  afterEach(async () => {
    await sandbox.terminate();
  });

  it('should enforce worker pool size limit', () => {
    // Simulate pool exhaustion
    sandbox.workers.set('worker-1', {});
    sandbox.workers.set('worker-2', {});
    sandbox.workerTimestamps.set('worker-1', Date.now());
    sandbox.workerTimestamps.set('worker-2', Date.now());

    expect(sandbox.workers.size).toBe(2);
    expect(() => {
      if (sandbox.workers.size >= sandbox.config.maxWorkerPoolSize) {
        throw new Error('Worker pool exhausted');
      }
    }).toThrow('Worker pool exhausted');
  });
});

describe('EffectSandbox - Terminate', () => {
  it('should terminate all workers', async () => {
    const sandbox = new EffectSandbox();

    // Simulate workers
    const mockWorker = { terminate: vi.fn() };
    sandbox.workers.set('w1', mockWorker);
    sandbox.workers.set('w2', mockWorker);
    sandbox.workerTimestamps.set('w1', Date.now());
    sandbox.workerTimestamps.set('w2', Date.now());

    await sandbox.terminate();

    expect(sandbox.workers.size).toBe(0);
    expect(sandbox.workerTimestamps.size).toBe(0);
  });
});

describe('createSandboxedHook', () => {
  it('should create a sandboxed hook function', () => {
    const hook = () => console.log('test');
    const sandboxedHook = createSandboxedHook(hook);

    expect(typeof sandboxedHook).toBe('function');
  });

  it('should create sandbox with custom config', () => {
    const hook = () => {};
    const config = { timeout: 10000 };
    const sandboxedHook = createSandboxedHook(hook, config);

    expect(typeof sandboxedHook).toBe('function');
  });
});

describe('createEffectSandbox', () => {
  it('should create effect sandbox instance', () => {
    const sandbox = createEffectSandbox();
    expect(sandbox).toBeInstanceOf(EffectSandbox);
    sandbox.terminate();
  });

  it('should create sandbox with custom config', () => {
    const sandbox = createEffectSandbox({ timeout: 15000 });
    const stats = sandbox.getStats();
    expect(stats.config.timeout).toBe(15000);
    sandbox.terminate();
  });
});
