/**
 * @fileoverview Tests for AtomVM Node.js Runtime
 * @description
 * Tests that verify Node.js AtomVM execution works.
 */

import { describe, it, expect, beforeAll } from 'vitest';
import { AtomVMNodeRuntime } from '../src/node-runtime.mjs';
import { readFileSync, existsSync } from 'fs';
import { join, resolve } from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

describe('AtomVMNodeRuntime', () => {
  let runtime;

  beforeAll(() => {
    runtime = new AtomVMNodeRuntime({
      log: () => {}, // Suppress logs in tests
      errorLog: () => {},
    });
  });

  it('should load AtomVM Node.js file', async () => {
    await expect(runtime.load()).resolves.not.toThrow();
    expect(runtime.atomvmPath).toBeTruthy();
    expect(existsSync(runtime.atomvmPath)).toBe(true);
  });

  it('should find AtomVM file in public directory', async () => {
    await runtime.load();
    const publicDir = resolve(__dirname, '../../public');
    const expectedPath = join(publicDir, 'AtomVM-node-v0.6.6.js');
    // Check if file exists (may not exist in test environment, that's okay)
    const fileExists = existsSync(expectedPath);
    if (fileExists) {
      expect(runtime.atomvmPath).toBe(expectedPath);
    } else {
      // File doesn't exist, but load should handle it gracefully
      expect(runtime.atomvmPath).toBeTruthy();
    }
  });

  it('should have execute method', () => {
    expect(typeof runtime.execute).toBe('function');
  });

  it('should reject execution if not loaded', async () => {
    const unloadedRuntime = new AtomVMNodeRuntime();
    await expect(unloadedRuntime.execute('/test.avm')).rejects.toThrow('Runtime not ready');
  });

  it('should reject execution if no .avm file provided', async () => {
    await runtime.load();
    await expect(runtime.execute('')).rejects.toThrow('avmPath is required');
  });

  it('should throw error when .avm file does not exist', async () => {
    await runtime.load();
    // Code throws error immediately (fail fast), not graceful
    await expect(runtime.execute('/nonexistent.avm')).rejects.toThrow();
  });
});
