/**
 * @file Tests for configuration module (80/20 fast suite)
 */

import { describe, it, expect, afterEach } from 'vitest';
import {
  createConfig,
  mergeConfig,
  validateConfig,
  getGlobalConfig,
  setGlobalConfig,
  resetGlobalConfig,
} from '../src/config.mjs';

describe('Config Creation & Merging', () => {
  it('should create config with defaults', () => {
    const config = createConfig({}, {});
    expect(config.store.capacity).toBe(10000);
    expect(config.query.timeout).toBe(5000);
  });

  it('should merge configs correctly', () => {
    const merged = mergeConfig(
      { store: { capacity: 100 } },
      { query: { timeout: 5000 } }
    );
    expect(merged.store.capacity).toBe(100);
    expect(merged.query.timeout).toBe(5000);
  });
});

describe('Config Validation', () => {
  it('should validate valid config', () => {
    expect(validateConfig({ store: { capacity: 5000 } })).toBe(true);
  });

  it('should reject invalid config', () => {
    expect(validateConfig({ store: { capacity: -1 } })).toBe(false);
  });
});

describe('Global Config Management', () => {
  afterEach(() => {
    resetGlobalConfig();
  });

  it('should manage global config state', () => {
    setGlobalConfig({ query: { timeout: 10000 } });
    const config = getGlobalConfig();
    expect(config.query.timeout).toBe(10000);

    resetGlobalConfig();
    const reset = getGlobalConfig();
    expect(reset.query.timeout).toBe(5000);
  });
});
