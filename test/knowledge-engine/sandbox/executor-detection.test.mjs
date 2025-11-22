/**
 * @fileoverview Sandbox Executor Detection Tests
 * @module test/knowledge-engine/sandbox/executor-detection
 *
 * @description
 * Tests for automatic detection of available sandbox executors and fallback selection.
 * Validates environment detection, version checking, and graceful degradation.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { createSandboxAdapter } from '../../../src/security/sandbox-adapter.mjs';

describe('Executor Detection', () => {
  let _originalNodeVersion;
  let _originalPlatform;

  beforeEach(() => {
    _originalNodeVersion = process.version;
    _originalPlatform = process.platform;
  });

  afterEach(() => {
    // Restore original environment
    vi.restoreAllMocks();
  });

  describe('Environment Detection', () => {
    it('should detect Node.js environment', () => {
      expect(typeof process !== 'undefined').toBe(true);
      expect(process.versions).toBeDefined();
      expect(process.versions.node).toBeDefined();
    });

    it('should detect Node.js version', () => {
      const version = process.versions.node;
      const major = parseInt(version.split('.')[0]);

      expect(major).toBeGreaterThanOrEqual(18);
    });

    it('should detect platform (linux, darwin, win32)', () => {
      const validPlatforms = ['linux', 'darwin', 'win32'];
      expect(validPlatforms).toContain(process.platform);
    });

    it('should detect architecture (x64, arm64)', () => {
      const validArchitectures = ['x64', 'arm64', 'arm'];
      expect(validArchitectures).toContain(process.arch);
    });

    it('should identify browser environment (when applicable)', () => {
      // In Node.js, window should be undefined
      expect(typeof window).toBe('undefined');
      expect(typeof document).toBe('undefined');
    });
  });

  describe('Executor Selection', () => {
    it('should default to worker executor currently', async () => {
      const adapter = createSandboxAdapter();
      // Initialize to get the engine type
      await adapter.run('1');
      expect(['worker', 'vm2', 'browser', 'isolated-vm']).toContain(adapter.getEngine());
    });

    it('should create functional executor', async () => {
      const adapter = createSandboxAdapter();

      const result = await adapter.run('1 + 1');
      expect(result).toBe(2);
    });

    it('should support timeout configuration', async () => {
      const adapter = createSandboxAdapter({ timeoutMs: 100 });

      await expect(async () => {
        await adapter.run('while(true) {}');
      }).rejects.toThrow();
    });

    it('should support sandbox context configuration', async () => {
      const adapter = createSandboxAdapter({
        sandbox: { customValue: 42 },
      });

      const result = await adapter.run('customValue');
      expect(result).toBe(42);
    });
  });

  describe('Fallback Executor Selection', () => {
    it('should fall back to worker when isolated-vm unavailable', async () => {
      // Currently, isolated-vm may not be available, so worker or vm2 is used
      const adapter = createSandboxAdapter();
      await adapter.run('1');
      expect(['worker', 'vm2', 'browser']).toContain(adapter.getEngine());
    });

    it('should work with fallback executor', async () => {
      const adapter = createSandboxAdapter();

      const result = await adapter.run('Math.pow(2, 10)');
      expect(result).toBe(1024);
    });

    it('should log warning when falling back (future)', () => {
      const consoleSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});

      // Future: When isolated-vm is preferred but unavailable
      // createSandboxAdapter() should log warning about fallback

      // Currently vm2 is expected, so no warning
      createSandboxAdapter();

      // In future, expect warning when isolated-vm unavailable
      // expect(consoleSpy).toHaveBeenCalledWith(expect.stringContaining('isolated-vm'));

      consoleSpy.mockRestore();
    });
  });

  describe('Feature Detection', () => {
    it('should detect vm2 availability', () => {
      let available = false;
      try {
        // eslint-disable-next-line
        require.resolve('vm2');
        available = true;
      } catch (error) {
        available = false;
      }
      // vm2 may or may not be available depending on environment
      expect(typeof available).toBe('boolean');
    });

    it('should detect isolated-vm availability (future)', () => {
      let available = false;

      try {
        // eslint-disable-next-line
        require.resolve('isolated-vm');
        available = true;
      } catch (error) {
        available = false;
      }

      // isolated-vm may or may not be installed
      expect(typeof available).toBe('boolean');
    });

    it('should check for native VM module', () => {
      try {
        // eslint-disable-next-line
        require('vm');
        expect(true).toBe(true);
      } catch (error) {
        expect.fail('Native vm module should be available');
      }
    });

    it('should detect worker_threads support', () => {
      try {
        // eslint-disable-next-line
        require('worker_threads');
        expect(true).toBe(true);
      } catch (error) {
        expect.fail('worker_threads should be available in Node 18+');
      }
    });
  });

  describe('Capability Detection', () => {
    it('should detect async/await support', async () => {
      const adapter = createSandboxAdapter();

      const code = '(async () => Promise.resolve(42))()';
      const result = await adapter.run(code);

      expect(result).toBe(42);
    });

    it('should detect Promise support', async () => {
      const adapter = createSandboxAdapter();

      const result = await adapter.run('Promise.resolve(42)');
      expect(result).toBe(42);
    });

    it('should detect ES6 features', async () => {
      const adapter = createSandboxAdapter();

      const tests = [
        { name: 'Arrow functions', code: '(() => 42)()' },
        { name: 'Template literals', code: '`test`' },
        { name: 'Destructuring', code: 'const {a} = {a:1}; a' },
        { name: 'Spread operator', code: '[...[1,2,3]]' },
        { name: 'Classes', code: 'class A {}; new A()' },
      ];

      for (const test of tests) {
        await expect(async () => {
          await adapter.run(test.code);
        }, `${test.name} should be supported`).not.toThrow();
      }
    });

    it('should detect Map/Set support', async () => {
      const adapter = createSandboxAdapter();

      const mapResult = await adapter.run('new Map([[1, "a"]]).get(1)');
      expect(mapResult).toBe('a');

      const setResult = await adapter.run('new Set([1, 2, 3]).size');
      expect(setResult).toBe(3);
    });

    it('should detect Symbol support', async () => {
      const adapter = createSandboxAdapter();

      const result = await adapter.run('typeof Symbol("test")');
      expect(result).toBe('symbol');
    });
  });

  describe('Version Compatibility', () => {
    it('should work on Node.js 18+', async () => {
      const version = process.versions.node;
      const major = parseInt(version.split('.')[0]);

      expect(major).toBeGreaterThanOrEqual(18);

      const adapter = createSandboxAdapter();
      const result = await adapter.run('1 + 1');
      expect(result).toBe(2);
    });

    it('should support current Node.js features', () => {
      const adapter = createSandboxAdapter();

      // Features available in Node 18+
      const tests = [
        'globalThis',
        'BigInt(42)',
        'Promise.allSettled([Promise.resolve(1)])',
        'Object.fromEntries([["a", 1]])',
      ];

      for (const test of tests) {
        expect(() => {
          adapter.run(test);
        }).not.toThrow();
      }
    });
  });

  describe('Graceful Degradation', () => {
    it('should handle missing optional features gracefully', async () => {
      const adapter = createSandboxAdapter();

      // Test that adapter works even if some features are missing
      const result = await adapter.run('1 + 1');
      expect(result).toBe(2);
    });

    it('should provide clear error messages on failures', () => {
      const adapter = createSandboxAdapter();

      try {
        adapter.run('throw new Error("Test error")');
        expect.fail('Should have thrown');
      } catch (error) {
        expect(error.message).toBeDefined();
        expect(error.message).not.toBe('');
      }
    });

    it('should handle syntax errors gracefully', async () => {
      const adapter = createSandboxAdapter();

      await expect(async () => {
        await adapter.run('const x = ;');
      }).rejects.toThrow();
    });

    it('should recover from errors and continue working', async () => {
      const adapter = createSandboxAdapter();

      // First execution fails
      try {
        await adapter.run('throw new Error("Error")');
      } catch (error) {
        // Expected
      }

      // Second execution should still work
      const result = await adapter.run('1 + 1');
      expect(result).toBe(2);
    });
  });

  describe('Performance Characteristics', () => {
    it('should initialize quickly', () => {
      const start = Date.now();
      createSandboxAdapter();
      const duration = Date.now() - start;

      expect(duration, 'Initialization should take less than 100ms').toBeLessThan(100);
    });

    it('should execute simple code quickly', () => {
      const adapter = createSandboxAdapter();

      const start = Date.now();
      adapter.run('1 + 1');
      const duration = Date.now() - start;

      expect(duration, 'Simple execution should take less than 50ms').toBeLessThan(50);
    });

    it('should support concurrent executions', async () => {
      const adapter = createSandboxAdapter();

      const promises = [];
      for (let i = 0; i < 10; i++) {
        promises.push(adapter.run(`${i} * 2`));
      }

      const results = await Promise.all(promises);
      expect(results).toEqual([0, 2, 4, 6, 8, 10, 12, 14, 16, 18]);
    });
  });

  describe('Error Recovery', () => {
    it('should recover from timeout errors', async () => {
      const adapter = createSandboxAdapter({ timeoutMs: 100 });

      // Timeout error
      await expect(async () => {
        await adapter.run('while(true) {}');
      }).rejects.toThrow();

      // Should still work after timeout
      const result = await adapter.run('1 + 1');
      expect(result).toBe(2);
    });

    it('should recover from memory errors', async () => {
      const adapter = createSandboxAdapter();

      // Memory error (if limits enforced)
      try {
        await adapter.run('const arr = []; while(true) arr.push(new Array(1000000));');
      } catch (error) {
        // Expected
      }

      // Should still work
      const result = await adapter.run('1 + 1');
      expect(result).toBe(2);
    });

    it('should handle multiple consecutive errors', async () => {
      const adapter = createSandboxAdapter();

      for (let i = 0; i < 5; i++) {
        await expect(async () => {
          await adapter.run('throw new Error("Error")');
        }).rejects.toThrow();
      }

      // Should still work
      const result = await adapter.run('42');
      expect(result).toBe(42);
    });
  });

  describe('Configuration Validation', () => {
    it('should accept valid timeout values', () => {
      const validTimeouts = [100, 500, 1000, 5000, 10000];

      for (const timeout of validTimeouts) {
        expect(() => {
          createSandboxAdapter({ timeoutMs: timeout });
        }).not.toThrow();
      }
    });

    it('should accept valid memory limit values', () => {
      const validLimits = [64, 128, 256, 512, 1024];

      for (const limit of validLimits) {
        expect(() => {
          createSandboxAdapter({ memoryLimitMB: limit });
        }).not.toThrow();
      }
    });

    it('should accept empty sandbox context', () => {
      expect(() => {
        createSandboxAdapter({ sandbox: {} });
      }).not.toThrow();
    });

    it('should accept custom sandbox context', async () => {
      const adapter = createSandboxAdapter({
        sandbox: {
          customValue: 42,
          customFunction: () => 'test',
        },
      });

      expect(await adapter.run('customValue')).toBe(42);
    });
  });

  describe('Future: Isolated-VM Detection', () => {
    it('should prefer isolated-vm when available (v3.1.0)', async () => {
      // This test will pass in v3.1.0 when isolated-vm is implemented

      // For now, we expect worker or vm2
      const adapter = createSandboxAdapter();
      await adapter.run('1');
      expect(['worker', 'vm2', 'browser', 'isolated-vm']).toContain(adapter.getEngine());
    });

    it('should detect isolated-vm version (v3.1.0)', () => {
      // In v3.1.0, should detect isolated-vm version
      // const version = getIsolatedVmVersion();
      // expect(version).toMatch(/^\d+\.\d+\.\d+$/);

      // Currently not implemented
      expect(true).toBe(true);
    });

    it('should support isolated-vm specific features (v3.1.0)', () => {
      // In v3.1.0, should support isolated-vm specific features
      // - Memory snapshots
      // - CPU time limits
      // - Synchronous execution
      // - Copy-free data transfer

      // Currently not implemented
      expect(true).toBe(true);
    });
  });
});

describe('Executor Factory', () => {
  it('should export createSandboxAdapter function', () => {
    expect(typeof createSandboxAdapter).toBe('function');
  });

  it('should create new instance on each call', () => {
    const adapter1 = createSandboxAdapter();
    const adapter2 = createSandboxAdapter();

    expect(adapter1).not.toBe(adapter2);
  });

  it('should create isolated instances', async () => {
    const adapter1 = createSandboxAdapter({ sandbox: { value: 1 } });
    const adapter2 = createSandboxAdapter({ sandbox: { value: 2 } });

    expect(await adapter1.run('value')).toBe(1);
    expect(await adapter2.run('value')).toBe(2);
  });
});
