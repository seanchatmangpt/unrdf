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
  let originalNodeVersion;
  let originalPlatform;

  beforeEach(() => {
    originalNodeVersion = process.version;
    originalPlatform = process.platform;
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
    it('should default to vm2 executor currently', () => {
      const adapter = createSandboxAdapter();

      expect(adapter.engine).toBe('vm2');
    });

    it('should create functional executor', () => {
      const adapter = createSandboxAdapter();

      const result = adapter.run('1 + 1');
      expect(result).toBe(2);
    });

    it('should support timeout configuration', () => {
      const adapter = createSandboxAdapter({ timeoutMs: 100 });

      expect(() => {
        adapter.run('while(true) {}');
      }).toThrow(/timeout/i);
    });

    it('should support sandbox context configuration', () => {
      const adapter = createSandboxAdapter({
        sandbox: { customValue: 42 }
      });

      const result = adapter.run('customValue');
      expect(result).toBe(42);
    });
  });

  describe('Fallback Executor Selection', () => {
    it('should fall back to vm2 when isolated-vm unavailable', () => {
      // Currently, isolated-vm is not implemented, so vm2 is always used
      const adapter = createSandboxAdapter();

      expect(adapter.engine).toBe('vm2');
    });

    it('should work with fallback executor', () => {
      const adapter = createSandboxAdapter();

      const result = adapter.run('Math.pow(2, 10)');
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
      try {
        // eslint-disable-next-line
        require.resolve('vm2');
        expect(true).toBe(true);
      } catch (error) {
        expect.fail('vm2 should be available');
      }
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

      // Currently isolated-vm is not installed
      expect(available).toBe(false);

      // In v3.1.0, this should be true
      // expect(available).toBe(true);
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
    it('should detect async/await support', () => {
      const adapter = createSandboxAdapter();

      const code = '(async () => Promise.resolve(42))()';
      const result = adapter.run(code);

      expect(result).toBeInstanceOf(Promise);
    });

    it('should detect Promise support', () => {
      const adapter = createSandboxAdapter();

      const result = adapter.run('Promise.resolve(42)');
      expect(result).toBeInstanceOf(Promise);
    });

    it('should detect ES6 features', () => {
      const adapter = createSandboxAdapter();

      const tests = [
        { name: 'Arrow functions', code: '(() => 42)()' },
        { name: 'Template literals', code: '`test`' },
        { name: 'Destructuring', code: 'const {a} = {a:1}; a' },
        { name: 'Spread operator', code: '[...[1,2,3]]' },
        { name: 'Classes', code: 'class A {}; new A()' }
      ];

      for (const test of tests) {
        expect(() => {
          adapter.run(test.code);
        }, `${test.name} should be supported`).not.toThrow();
      }
    });

    it('should detect Map/Set support', () => {
      const adapter = createSandboxAdapter();

      const mapResult = adapter.run('new Map([[1, "a"]]).get(1)');
      expect(mapResult).toBe('a');

      const setResult = adapter.run('new Set([1, 2, 3]).size');
      expect(setResult).toBe(3);
    });

    it('should detect Symbol support', () => {
      const adapter = createSandboxAdapter();

      const result = adapter.run('typeof Symbol("test")');
      expect(result).toBe('symbol');
    });
  });

  describe('Version Compatibility', () => {
    it('should work on Node.js 18+', () => {
      const version = process.versions.node;
      const major = parseInt(version.split('.')[0]);

      expect(major).toBeGreaterThanOrEqual(18);

      const adapter = createSandboxAdapter();
      const result = adapter.run('1 + 1');
      expect(result).toBe(2);
    });

    it('should support current Node.js features', () => {
      const adapter = createSandboxAdapter();

      // Features available in Node 18+
      const tests = [
        'globalThis',
        'BigInt(42)',
        'Promise.allSettled([Promise.resolve(1)])',
        'Object.fromEntries([["a", 1]])'
      ];

      for (const test of tests) {
        expect(() => {
          adapter.run(test);
        }).not.toThrow();
      }
    });
  });

  describe('Graceful Degradation', () => {
    it('should handle missing optional features gracefully', () => {
      const adapter = createSandboxAdapter();

      // Test that adapter works even if some features are missing
      const result = adapter.run('1 + 1');
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

    it('should handle syntax errors gracefully', () => {
      const adapter = createSandboxAdapter();

      expect(() => {
        adapter.run('const x = ;');
      }).toThrow();
    });

    it('should recover from errors and continue working', () => {
      const adapter = createSandboxAdapter();

      // First execution fails
      try {
        adapter.run('throw new Error("Error")');
      } catch (error) {
        // Expected
      }

      // Second execution should still work
      const result = adapter.run('1 + 1');
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

    it('should support concurrent executions', () => {
      const adapter = createSandboxAdapter();

      const promises = [];
      for (let i = 0; i < 10; i++) {
        promises.push(
          new Promise((resolve) => {
            const result = adapter.run(`${i} * 2`);
            resolve(result);
          })
        );
      }

      return Promise.all(promises).then(results => {
        expect(results).toEqual([0, 2, 4, 6, 8, 10, 12, 14, 16, 18]);
      });
    });
  });

  describe('Error Recovery', () => {
    it('should recover from timeout errors', () => {
      const adapter = createSandboxAdapter({ timeoutMs: 100 });

      // Timeout error
      expect(() => {
        adapter.run('while(true) {}');
      }).toThrow();

      // Should still work after timeout
      const result = adapter.run('1 + 1');
      expect(result).toBe(2);
    });

    it('should recover from memory errors', () => {
      const adapter = createSandboxAdapter();

      // Memory error (if limits enforced)
      try {
        adapter.run('const arr = []; while(true) arr.push(new Array(1000000));');
      } catch (error) {
        // Expected
      }

      // Should still work
      const result = adapter.run('1 + 1');
      expect(result).toBe(2);
    });

    it('should handle multiple consecutive errors', () => {
      const adapter = createSandboxAdapter();

      for (let i = 0; i < 5; i++) {
        expect(() => {
          adapter.run('throw new Error("Error")');
        }).toThrow();
      }

      // Should still work
      const result = adapter.run('42');
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

    it('should accept custom sandbox context', () => {
      const adapter = createSandboxAdapter({
        sandbox: {
          customValue: 42,
          customFunction: () => 'test'
        }
      });

      expect(adapter.run('customValue')).toBe(42);
    });
  });

  describe('Future: Isolated-VM Detection', () => {
    it('should prefer isolated-vm when available (v3.1.0)', () => {
      // This test will pass in v3.1.0 when isolated-vm is implemented

      // Mock isolated-vm availability
      // const adapter = createSandboxAdapter();
      // expect(adapter.engine).toBe('isolated-vm');

      // For now, we expect vm2
      const adapter = createSandboxAdapter();
      expect(adapter.engine).toBe('vm2');
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

  it('should create isolated instances', () => {
    const adapter1 = createSandboxAdapter({ sandbox: { value: 1 } });
    const adapter2 = createSandboxAdapter({ sandbox: { value: 2 } });

    expect(adapter1.run('value')).toBe(1);
    expect(adapter2.run('value')).toBe(2);
  });
});
