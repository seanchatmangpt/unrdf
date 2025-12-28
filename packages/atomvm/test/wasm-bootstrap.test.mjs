/**
 * @fileoverview WASM Bootstrap Test Suite
 * @module test/wasm-bootstrap
 *
 * @description
 * Tests for WASM asset checking and loading functionality.
 * Validates environment detection, asset path resolution, and load operations.
 */

import { describe, it, expect } from 'vitest';
import {
  ATOMVM_VERSION,
  detectEnvironment,
  getAssetPaths,
  checkWASMAssets,
  loadWASM,
  validateWASMModule,
  getWASMInfo,
} from '../src/wasm-bootstrap.mjs';

describe('WASM Bootstrap', () => {
  describe('detectEnvironment', () => {
    it('detects node environment', () => {
      const env = detectEnvironment();
      // Running in Node.js via vitest
      expect(env).toBe('node');
    });

    it('returns a valid environment type', () => {
      const env = detectEnvironment();
      expect(['browser', 'node', 'unknown']).toContain(env);
    });
  });

  describe('getAssetPaths', () => {
    it('returns correct paths for browser environment', () => {
      const paths = getAssetPaths('browser');
      expect(paths.jsPath).toContain('AtomVM-web-');
      expect(paths.jsPath).toContain(ATOMVM_VERSION);
      expect(paths.wasmPath).toContain('.wasm');
    });

    it('returns correct paths for node environment', () => {
      const paths = getAssetPaths('node');
      expect(paths.jsPath).toContain('AtomVM-node-');
      expect(paths.jsPath).toContain(ATOMVM_VERSION);
      expect(paths.wasmPath).toContain('.wasm');
    });

    it('includes version in paths', () => {
      const paths = getAssetPaths('node');
      expect(paths.jsPath).toContain(ATOMVM_VERSION);
      expect(paths.wasmPath).toContain(ATOMVM_VERSION);
    });
  });

  describe('checkWASMAssets', () => {
    it('returns asset status object', async () => {
      const status = await checkWASMAssets();
      
      expect(status).toHaveProperty('available');
      expect(status).toHaveProperty('environment');
      expect(status).toHaveProperty('expectedJsPath');
      expect(status).toHaveProperty('expectedWasmPath');
      expect(status).toHaveProperty('error');
    });

    it('detects environment in status', async () => {
      const status = await checkWASMAssets();
      expect(['browser', 'node', 'unknown']).toContain(status.environment);
    });

    it('provides expected paths in status', async () => {
      const status = await checkWASMAssets();
      expect(status.expectedJsPath).toBeTruthy();
      expect(status.expectedWasmPath).toBeTruthy();
      expect(status.expectedJsPath).toContain(ATOMVM_VERSION);
      expect(status.expectedWasmPath).toContain(ATOMVM_VERSION);
    });
  });

  describe('loadWASM', () => {
    it('returns load result object', async () => {
      const result = await loadWASM();
      
      expect(result).toHaveProperty('success');
      expect(result).toHaveProperty('module');
      expect(result).toHaveProperty('loadTimeMs');
      expect(result).toHaveProperty('error');
    });

    it('includes timing information', async () => {
      const result = await loadWASM();
      expect(typeof result.loadTimeMs).toBe('number');
      expect(result.loadTimeMs).toBeGreaterThanOrEqual(0);
    });

    it('accepts environment override', async () => {
      const result = await loadWASM({ environment: 'node' });
      expect(result).toBeDefined();
    });
  });

  describe('validateWASMModule', () => {
    it('returns invalid for null module', () => {
      const result = validateWASMModule(null);
      expect(result.valid).toBe(false);
      expect(result.missing.length).toBeGreaterThan(0);
    });

    it('returns invalid for undefined module', () => {
      const result = validateWASMModule(undefined);
      expect(result.valid).toBe(false);
      expect(result.missing).toContain('module is null or undefined');
    });

    it('returns validation result structure', () => {
      const result = validateWASMModule({});
      expect(result).toHaveProperty('valid');
      expect(result).toHaveProperty('missing');
      expect(Array.isArray(result.missing)).toBe(true);
    });
  });

  describe('getWASMInfo', () => {
    it('returns version and environment', () => {
      const info = getWASMInfo();
      expect(info.version).toBe(ATOMVM_VERSION);
      expect(['browser', 'node', 'unknown']).toContain(info.environment);
    });

    it('matches detected environment', () => {
      const info = getWASMInfo();
      const env = detectEnvironment();
      expect(info.environment).toBe(env);
    });
  });

  describe('ATOMVM_VERSION', () => {
    it('is defined and non-empty', () => {
      expect(ATOMVM_VERSION).toBeTruthy();
      expect(typeof ATOMVM_VERSION).toBe('string');
      expect(ATOMVM_VERSION.length).toBeGreaterThan(0);
    });

    it('follows version format', () => {
      expect(ATOMVM_VERSION).toMatch(/^v\d+\.\d+\.\d+$/);
    });
  });
});
