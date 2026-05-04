/**
 * @file YAWL Nitro Configuration Tests
 * @module @unrdf/yawl/test/integrations/nitro-config
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  NitroExecutorConfigSchema,
  DEFAULT_CONFIG,
  loadConfigFromEnv,
  validateConfig,
  safeValidateConfig,
  ConfigManager,
  createConfigManager,
} from '../../src/integrations/nitro-config.mjs';

describe('NitroExecutorConfig', () => {
  describe('Schema Validation', () => {
    it('should validate default configuration', () => {
      const result = NitroExecutorConfigSchema.safeParse(DEFAULT_CONFIG);
      expect(result.success).toBe(true);
    });

    it('should apply default values when not provided', () => {
      const config = validateConfig({});
      expect(config.executionMode).toBe('adaptive');
      expect(config.timeouts.default).toBe(5000);
      expect(config.retry.enabled).toBe(true);
      expect(config.parallelism.maxConcurrentTasks).toBe(10);
    });

    it('should validate custom configuration', () => {
      const customConfig = {
        executorId: 'custom-executor',
        executionMode: 'parallel',
        timeouts: {
          default: 3000,
          task: 15000,
        },
      };

      const result = validateConfig(customConfig);
      expect(result.executorId).toBe('custom-executor');
      expect(result.executionMode).toBe('parallel');
      expect(result.timeouts.default).toBe(3000);
      expect(result.timeouts.task).toBe(15000);
    });

    it('should reject invalid execution mode', () => {
      const invalidConfig = {
        executionMode: 'invalid-mode',
      };

      expect(() => validateConfig(invalidConfig)).toThrow();
    });

    it('should reject negative timeout values', () => {
      const invalidConfig = {
        timeouts: {
          default: -1000,
        },
      };

      expect(() => validateConfig(invalidConfig)).toThrow();
    });

    it('should use safeValidateConfig for non-throwing validation', () => {
      const invalidConfig = {
        executionMode: 'invalid',
      };

      const result = safeValidateConfig(invalidConfig);
      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });
  });

  describe('Environment Variable Loading', () => {
    let originalEnv;

    beforeEach(() => {
      originalEnv = { ...process.env };
    });

    afterEach(() => {
      process.env = originalEnv;
    });

    it('should load configuration from environment variables', () => {
      process.env.YAWL_NITRO_EXECUTOR_ID = 'env-executor';
      process.env.YAWL_NITRO_EXECUTION_MODE = 'sequential';
      process.env.YAWL_NITRO_MAX_CONCURRENT_TASKS = '20';
      process.env.YAWL_NITRO_ENABLE_RECEIPTS = 'true';

      const config = loadConfigFromEnv();

      expect(config.executorId).toBe('env-executor');
      expect(config.executionMode).toBe('sequential');
      expect(config.parallelism.maxConcurrentTasks).toBe(20);
      expect(config.receipts.enabled).toBe(true);
    });

    it('should parse boolean environment variables correctly', () => {
      process.env.YAWL_NITRO_ENABLE_RECEIPTS = 'true';
      process.env.YAWL_NITRO_STRICT_VALIDATION = 'false';
      process.env.YAWL_NITRO_DEBUG = '1';

      const config = loadConfigFromEnv();

      expect(config.receipts.enabled).toBe(true);
      expect(config.strictValidation).toBe(false);
      expect(config.debug).toBe(true);
    });

    it('should parse integer environment variables correctly', () => {
      process.env.YAWL_NITRO_TIMEOUT_DEFAULT = '8000';
      process.env.YAWL_NITRO_RETRY_MAX_ATTEMPTS = '5';

      const config = loadConfigFromEnv();

      expect(config.timeouts.default).toBe(8000);
      expect(config.retry.maxAttempts).toBe(5);
    });

    it('should use default values when environment variables not set', () => {
      const config = loadConfigFromEnv();

      expect(config.executionMode).toBe(DEFAULT_CONFIG.executionMode);
      expect(config.timeouts.default).toBe(DEFAULT_CONFIG.timeouts.default);
    });

    it('should merge with base configuration', () => {
      process.env.YAWL_NITRO_TIMEOUT_DEFAULT = '7000';

      const baseConfig = {
        executorId: 'base-executor',
        executionMode: 'parallel',
      };

      const config = loadConfigFromEnv(baseConfig);

      expect(config.executorId).toBe('base-executor');
      expect(config.executionMode).toBe('parallel');
      expect(config.timeouts.default).toBe(7000);
    });
  });

  describe('ConfigManager', () => {
    let manager;

    beforeEach(() => {
      manager = createConfigManager();
    });

    it('should initialize with default configuration', () => {
      const config = manager.get();
      expect(config.executionMode).toBe('adaptive');
      expect(config.timeouts.default).toBe(5000);
    });

    it('should initialize with custom configuration', () => {
      const customManager = createConfigManager({
        executorId: 'custom',
        executionMode: 'sequential',
      });

      const config = customManager.get();
      expect(config.executorId).toBe('custom');
      expect(config.executionMode).toBe('sequential');
    });

    it('should update configuration', () => {
      manager.update({
        executionMode: 'parallel',
        timeouts: { default: 10000 },
      });

      const config = manager.get();
      expect(config.executionMode).toBe('parallel');
      expect(config.timeouts.default).toBe(10000);
    });

    it('should throw on invalid update', () => {
      expect(() => {
        manager.update({ executionMode: 'invalid' });
      }).toThrow();
    });

    it('should set nested configuration value', () => {
      manager.set('timeouts.task', 25000);
      expect(manager.getValue('timeouts.task')).toBe(25000);
    });

    it('should get nested configuration value', () => {
      const value = manager.getValue('retry.maxAttempts');
      expect(value).toBe(DEFAULT_CONFIG.retry.maxAttempts);
    });

    it('should notify listeners on configuration change', () => {
      let notified = false;
      let newConfig;
      let oldConfig;

      manager.onChange((newCfg, oldCfg) => {
        notified = true;
        newConfig = newCfg;
        oldConfig = oldCfg;
      });

      manager.update({ executionMode: 'parallel' });

      expect(notified).toBe(true);
      expect(newConfig.executionMode).toBe('parallel');
      expect(oldConfig.executionMode).toBe('adaptive');
    });

    it('should allow unsubscribing from changes', () => {
      let callCount = 0;

      const unsubscribe = manager.onChange(() => {
        callCount++;
      });

      manager.update({ executionMode: 'sequential' });
      expect(callCount).toBe(1);

      unsubscribe();

      manager.update({ executionMode: 'parallel' });
      expect(callCount).toBe(1);
    });

    it('should reset to default configuration', () => {
      manager.update({ executionMode: 'sequential', timeouts: { default: 10000 } });

      manager.reset();

      const config = manager.get();
      expect(config.executionMode).toBe(DEFAULT_CONFIG.executionMode);
      expect(config.timeouts.default).toBe(DEFAULT_CONFIG.timeouts.default);
    });

    it('should load configuration from environment', () => {
      const originalEnv = { ...process.env };
      process.env.YAWL_NITRO_EXECUTION_MODE = 'sequential';
      process.env.YAWL_NITRO_MAX_CONCURRENT_TASKS = '15';

      manager.loadFromEnv();

      const config = manager.get();
      expect(config.executionMode).toBe('sequential');
      expect(config.parallelism.maxConcurrentTasks).toBe(15);

      process.env = originalEnv;
    });
  });

  describe('Configuration Schemas', () => {
    it('should enforce timeout constraints', () => {
      const validTimeouts = {
        default: 5000,
        task: 30000,
        workflow: 300000,
        cancellation: 10000,
      };

      const result = validateConfig({ timeouts: validTimeouts });
      expect(result.timeouts).toEqual(validTimeouts);
    });

    it('should enforce retry constraints', () => {
      const validRetry = {
        enabled: true,
        maxAttempts: 5,
        backoffMs: 2000,
        backoffMultiplier: 1.5,
        maxBackoffMs: 60000,
      };

      const result = validateConfig({ retry: validRetry });
      expect(result.retry.maxAttempts).toBe(5);
    });

    it('should enforce parallelism constraints', () => {
      const validParallelism = {
        maxConcurrentTasks: 20,
        maxConcurrentWorkflows: 10,
        enableBatching: true,
        batchSize: 10,
      };

      const result = validateConfig({ parallelism: validParallelism });
      expect(result.parallelism).toEqual(validParallelism);
    });

    it('should enforce receipt configuration', () => {
      const validReceipts = {
        enabled: true,
        includePayload: true,
        includePreviousHash: true,
        enableChaining: true,
        enableMerkleProofs: true,
      };

      const result = validateConfig({ receipts: validReceipts });
      expect(result.receipts).toEqual(validReceipts);
    });

    it('should enforce event configuration', () => {
      const validEvents = {
        enabled: true,
        emitTaskEvents: true,
        emitWorkflowEvents: true,
        emitExecutionEvents: true,
        eventBufferSize: 2000,
      };

      const result = validateConfig({ events: validEvents });
      expect(result.events).toEqual(validEvents);
    });

    it('should enforce metrics configuration', () => {
      const validMetrics = {
        enabled: true,
        collectTaskMetrics: true,
        collectWorkflowMetrics: true,
        collectPerformanceMetrics: true,
        metricsRetentionMs: 7200000,
      };

      const result = validateConfig({ metrics: validMetrics });
      expect(result.metrics).toEqual(validMetrics);
    });

    it('should enforce pattern support configuration', () => {
      const validPatterns = {
        enableAllPatterns: false,
        supportedPatterns: ['WP1', 'WP2', 'WP3'],
        enableCancellation: true,
        enableMultiInstance: true,
        enableDeferred: false,
      };

      const result = validateConfig({ patterns: validPatterns });
      expect(result.patterns.supportedPatterns).toEqual(['WP1', 'WP2', 'WP3']);
    });
  });

  describe('Configuration Immutability', () => {
    it('should return copy of configuration to prevent mutation', () => {
      const manager = createConfigManager();
      const config1 = manager.get();
      const config2 = manager.get();

      expect(config1).not.toBe(config2);
      expect(config1).toEqual(config2);
    });
  });
});
