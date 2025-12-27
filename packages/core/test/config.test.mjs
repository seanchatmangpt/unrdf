/**
 * @file Tests for configuration module
 */

import { describe, it, expect, afterEach } from 'vitest';
import {
  createConfig,
  parseEnv,
  mergeConfig,
  getConfigValue,
  validateConfig,
  getGlobalConfig,
  setGlobalConfig,
  resetGlobalConfig,
  DEFAULT_CONFIG,
} from '../src/config.mjs';

describe('parseEnv', () => {
  it('parses store capacity from env', () => {
    const config = parseEnv({ UNRDF_STORE_CAPACITY: '5000' });
    expect(config.store.capacity).toBe(5000);
  });

  it('parses query timeout from env', () => {
    const config = parseEnv({ UNRDF_QUERY_TIMEOUT: '10000' });
    expect(config.query.timeout).toBe(10000);
  });

  it('parses environment from UNRDF_ENV', () => {
    const config = parseEnv({ UNRDF_ENV: 'development' });
    expect(config.env).toBe('development');
  });

  it('falls back to NODE_ENV for environment', () => {
    const config = parseEnv({ NODE_ENV: 'test' });
    expect(config.env).toBe('test');
  });

  it('parses log level from env', () => {
    const config = parseEnv({ UNRDF_LOG_LEVEL: 'debug' });
    expect(config.logging.level).toBe('debug');
  });

  it('returns empty config for empty env', () => {
    const config = parseEnv({});
    expect(Object.keys(config)).toHaveLength(0);
  });
});

describe('mergeConfig', () => {
  it('merges multiple configs', () => {
    const merged = mergeConfig(
      { store: { capacity: 100 } },
      { store: { validate: false } },
      { query: { timeout: 5000 } }
    );

    expect(merged.store.capacity).toBe(100);
    expect(merged.store.validate).toBe(false);
    expect(merged.query.timeout).toBe(5000);
  });

  it('later configs override earlier', () => {
    const merged = mergeConfig(
      { store: { capacity: 100 } },
      { store: { capacity: 200 } }
    );

    expect(merged.store.capacity).toBe(200);
  });

  it('handles nested objects', () => {
    const merged = mergeConfig(
      { store: { capacity: 100, validate: true } },
      { store: { capacity: 200 } }
    );

    expect(merged.store.capacity).toBe(200);
    expect(merged.store.validate).toBe(true);
  });
});

describe('createConfig', () => {
  it('creates config with defaults', () => {
    const config = createConfig({}, {});  // Empty env to use defaults

    expect(config.store.capacity).toBe(10000);
    expect(config.store.validate).toBe(true);
    expect(config.query.timeout).toBe(5000);
    expect(config.env).toBe('production');
  });

  it('merges explicit config over defaults', () => {
    const config = createConfig({
      query: { timeout: 10000 },
    });

    expect(config.query.timeout).toBe(10000);
    expect(config.store.capacity).toBe(10000); // Still default
  });

  it('parses environment variables', () => {
    const config = createConfig({}, { UNRDF_QUERY_TIMEOUT: '15000' });

    expect(config.query.timeout).toBe(15000);
  });

  it('explicit config overrides env', () => {
    const config = createConfig(
      { query: { timeout: 20000 } },
      { UNRDF_QUERY_TIMEOUT: '15000' }
    );

    expect(config.query.timeout).toBe(20000);
  });

  it('throws on invalid config', () => {
    expect(() =>
      createConfig({ store: { capacity: -1 } })
    ).toThrow('Invalid configuration');
  });

  it('fills in missing nested objects', () => {
    const config = createConfig({ env: 'development' });

    expect(config.store).toBeDefined();
    expect(config.query).toBeDefined();
    expect(config.logging).toBeDefined();
  });
});

describe('getConfigValue', () => {
  const config = createConfig({}, {});  // Empty env to use defaults

  it('gets top-level value', () => {
    expect(getConfigValue(config, 'env')).toBe('production');
  });

  it('gets nested value', () => {
    expect(getConfigValue(config, 'query.timeout')).toBe(5000);
  });

  it('gets deeply nested value', () => {
    expect(getConfigValue(config, 'logging.level')).toBe('info');
  });

  it('returns undefined for missing path', () => {
    expect(getConfigValue(config, 'missing.path')).toBeUndefined();
  });
});

describe('validateConfig', () => {
  it('validates valid config', () => {
    expect(validateConfig({ store: { capacity: 5000 } })).toBe(true);
  });

  it('rejects invalid config', () => {
    expect(validateConfig({ store: { capacity: -1 } })).toBe(false);
  });

  it('accepts partial config', () => {
    expect(validateConfig({ query: { timeout: 1000 } })).toBe(true);
  });

  it('rejects config with wrong types', () => {
    expect(validateConfig({ env: 'invalid' })).toBe(false);
  });
});

describe('global config', () => {
  afterEach(() => {
    resetGlobalConfig();
  });

  it('gets default global config', () => {
    const config = getGlobalConfig();
    expect(config.store.capacity).toBe(10000);
  });

  it('sets global config', () => {
    setGlobalConfig({ query: { timeout: 10000 } });
    const config = getGlobalConfig();
    expect(config.query.timeout).toBe(10000);
  });

  it('resets global config', () => {
    setGlobalConfig({ query: { timeout: 10000 } });
    resetGlobalConfig();
    const config = getGlobalConfig();
    expect(config.query.timeout).toBe(5000);
  });
});

describe('DEFAULT_CONFIG', () => {
  it('has all required fields', () => {
    expect(DEFAULT_CONFIG.store).toBeDefined();
    expect(DEFAULT_CONFIG.query).toBeDefined();
    expect(DEFAULT_CONFIG.env).toBeDefined();
    expect(DEFAULT_CONFIG.logging).toBeDefined();
  });

  it('has sensible defaults', () => {
    expect(DEFAULT_CONFIG.store.capacity).toBe(10000);
    expect(DEFAULT_CONFIG.query.timeout).toBe(5000);
    expect(DEFAULT_CONFIG.env).toBe('production');
    expect(DEFAULT_CONFIG.logging.enabled).toBe(true);
  });
});
