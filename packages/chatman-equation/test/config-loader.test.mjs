/**
 * @file config-loader.test.mjs
 * @description Tests for TOML config loader
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { loadConfig, loadConfigs, mergeConfigs, validateConfig } from '../src/config-loader.mjs';
import { writeFileSync, mkdirSync, rmSync, existsSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import { z } from 'zod';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const TEST_DIR = resolve(__dirname, '.test-configs');

describe('Config Loader', () => {
  beforeEach(() => {
    if (!existsSync(TEST_DIR)) {
      mkdirSync(TEST_DIR, { recursive: true });
    }
  });

  afterEach(() => {
    if (existsSync(TEST_DIR)) {
      rmSync(TEST_DIR, { recursive: true, force: true });
    }
  });

  describe('loadConfig', () => {
    it('should load valid TOML config', () => {
      const configPath = resolve(TEST_DIR, 'test.toml');
      const configContent = `
title = "Test Config"
version = "1.0.0"
description = "Test description"
`;
      writeFileSync(configPath, configContent);

      const config = loadConfig(configPath);
      expect(config.title).toBe('Test Config');
      expect(config.version).toBe('1.0.0');
      expect(config.description).toBe('Test description');
      expect(config._configPath).toBe(configPath);
      expect(config._loadedAt).toBeDefined();
    });

    it('should load config with nested objects', () => {
      const configPath = resolve(TEST_DIR, 'nested.toml');
      const configContent = `
title = "Nested Config"

[metadata]
author = "Test"
date = "2026-01-18"

[[items]]
name = "Item 1"
value = 100

[[items]]
name = "Item 2"
value = 200
`;
      writeFileSync(configPath, configContent);

      const config = loadConfig(configPath);
      expect(config.title).toBe('Nested Config');
      expect(config.metadata.author).toBe('Test');
      expect(config.items).toHaveLength(2);
      expect(config.items[0].name).toBe('Item 1');
      expect(config.items[1].value).toBe(200);
    });

    it('should throw on nonexistent file', () => {
      expect(() => {
        loadConfig('/nonexistent/path/config.toml');
      }).toThrow('Config file not found');
    });

    it('should throw on invalid TOML', () => {
      const configPath = resolve(TEST_DIR, 'invalid.toml');
      writeFileSync(configPath, 'invalid toml content ][');

      expect(() => {
        loadConfig(configPath);
      }).toThrow();
    });
  });

  describe('loadConfigs', () => {
    it('should load multiple configs', () => {
      const config1Path = resolve(TEST_DIR, 'config1.toml');
      const config2Path = resolve(TEST_DIR, 'config2.toml');

      writeFileSync(config1Path, 'title = "Config 1"');
      writeFileSync(config2Path, 'title = "Config 2"');

      const configs = loadConfigs([config1Path, config2Path]);
      expect(configs).toHaveLength(2);
      expect(configs[0].title).toBe('Config 1');
      expect(configs[1].title).toBe('Config 2');
    });

    it('should handle errors gracefully', () => {
      const validPath = resolve(TEST_DIR, 'valid.toml');
      const invalidPath = resolve(TEST_DIR, 'nonexistent.toml');

      writeFileSync(validPath, 'title = "Valid"');

      const configs = loadConfigs([validPath, invalidPath]);
      expect(configs).toHaveLength(2);
      expect(configs[0].title).toBe('Valid');
      expect(configs[1]._error).toBeDefined();
    });
  });

  describe('validateConfig', () => {
    it('should validate config against schema', () => {
      const config = {
        title: 'Test',
        version: '1.0.0',
        count: 42,
      };

      const schema = z.object({
        title: z.string(),
        version: z.string(),
        count: z.number(),
      });

      const validated = validateConfig(config, schema);
      expect(validated.title).toBe('Test');
      expect(validated.count).toBe(42);
    });

    it('should throw on invalid config', () => {
      const config = {
        title: 'Test',
        count: 'not a number',
      };

      const schema = z.object({
        title: z.string(),
        count: z.number(),
      });

      expect(() => {
        validateConfig(config, schema);
      }).toThrow();
    });
  });

  describe('mergeConfigs', () => {
    it('should merge two configs', () => {
      const base = {
        title: 'Base',
        version: '1.0.0',
        metadata: {
          author: 'Base Author',
        },
      };

      const override = {
        version: '2.0.0',
        metadata: {
          date: '2026-01-18',
        },
      };

      const merged = mergeConfigs(base, override);
      expect(merged.title).toBe('Base');
      expect(merged.version).toBe('2.0.0');
      expect(merged.metadata.author).toBe('Base Author');
      expect(merged.metadata.date).toBe('2026-01-18');
    });

    it('should merge multiple configs', () => {
      const c1 = { a: 1, b: 2 };
      const c2 = { b: 3, c: 4 };
      const c3 = { c: 5, d: 6 };

      const merged = mergeConfigs(c1, c2, c3);
      expect(merged.a).toBe(1);
      expect(merged.b).toBe(3);
      expect(merged.c).toBe(5);
      expect(merged.d).toBe(6);
    });

    it('should handle arrays (override, not merge)', () => {
      const base = { items: [1, 2, 3] };
      const override = { items: [4, 5] };

      const merged = mergeConfigs(base, override);
      expect(merged.items).toEqual([4, 5]);
    });
  });

  describe('example configs', () => {
    it('should load example-equation.toml', () => {
      const configPath = resolve(__dirname, '../configs/example-equation.toml');
      const config = loadConfig(configPath);

      expect(config.title).toBe('Chatman Equation API Reference');
      expect(config.module).toBe('@unrdf/chatman-equation');
      expect(config.equations).toBeDefined();
      expect(config.equations.length).toBeGreaterThan(0);
    });

    it('should load example-tutorial.toml', () => {
      const configPath = resolve(__dirname, '../configs/example-tutorial.toml');
      const config = loadConfig(configPath);

      expect(config.title).toBe('Getting Started with Chatman Equation');
      expect(config.difficulty).toBe('Beginner');
      expect(config.steps).toBeDefined();
      expect(config.steps.length).toBeGreaterThan(0);
    });
  });
});
