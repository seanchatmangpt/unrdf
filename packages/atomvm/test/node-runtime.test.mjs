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
      log: () => {},
      errorLog: () => {},
    });
  });

  it('loads AtomVM Node.js file', async () => {
    await expect(runtime.load()).resolves.not.toThrow();
    expect(runtime.atomvmPath).toBeTruthy();
    expect(existsSync(runtime.atomvmPath)).toBe(true);
  });

  it('exposes execute method', () => {
    expect(typeof runtime.execute).toBe('function');
  });

  it('rejects execution if not loaded', async () => {
    const unloadedRuntime = new AtomVMNodeRuntime();
    await expect(unloadedRuntime.execute('/test.avm')).rejects.toThrow('Runtime not ready');
  });

  it('rejects execution if no .avm file provided', async () => {
    await runtime.load();
    await expect(runtime.execute('')).rejects.toThrow('avmPath is required');
  });

  it('throws error when .avm file does not exist', async () => {
    await runtime.load();
    await expect(runtime.execute('/nonexistent.avm')).rejects.toThrow();
  });
});
