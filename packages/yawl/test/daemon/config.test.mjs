/**
 * @file YAWL Daemon Configuration Tests
 * @module @unrdf/yawl/test/daemon/config
 * @description Comprehensive tests for daemon configuration loading and validation
 * Tests cover schema validation, defaults, environment overrides, and error handling
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { Daemon } from '@unrdf/daemon';
import {
  YawlDaemonBridge,
  YawlDaemonBridgeConfigSchema,
  YawlRetryPolicySchema,
  YawlTimeoutConfigSchema,
  DistributionStrategySchema,
} from '@unrdf/daemon/integrations/yawl';
import { EventEmitter } from 'events';

/**
 * Generate UUID v4 for testing
 * @returns {string} Valid UUID v4
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Mock YAWL engine
 */
class MockYawlEngine extends EventEmitter {
  on(eventName, handler) {
    super.on(eventName, handler);
    return () => this.off(eventName, handler);
  }

  async createCase(options) {
    return { caseId: options.caseId || `case-${Date.now()}`, status: 'RUNNING' };
  }

  async enableTask(options) {
    return { ...options, status: 'ENABLED' };
  }

  async cancelTask(options) {
    return { ...options, status: 'CANCELLED' };
  }
}

describe('Daemon Configuration', () => {
  describe('schema validation', () => {
    it('should validate minimal valid configuration', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
      };

      // Act
      const result = () => new Daemon(config);

      // Assert
      expect(result).not.toThrow();
    });

    it('should require daemonId field', () => {
      // Arrange
      const config = {
        name: 'test-daemon',
      };

      // Act & Assert
      expect(() => new Daemon(config)).toThrow();
    });

    it('should require name field', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
      };

      // Act & Assert
      expect(() => new Daemon(config)).toThrow();
    });

    it('should validate daemonId as UUID', () => {
      // Arrange
      const config = {
        daemonId: 'not-a-uuid',
        name: 'test-daemon',
      };

      // Act & Assert
      expect(() => new Daemon(config)).toThrow();
    });

    it('should validate concurrency as positive integer', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
        concurrency: -5,
      };

      // Act & Assert
      expect(() => new Daemon(config)).toThrow();
    });

    it('should validate concurrency maximum limit', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
        concurrency: 101,
      };

      // Act & Assert
      expect(() => new Daemon(config)).toThrow();
    });

    it('should validate port range', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
        port: 70000, // Out of valid range
      };

      // Act & Assert
      expect(() => new Daemon(config)).toThrow();
    });

    it('should validate logLevel enum', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
        logLevel: 'invalid-level',
      };

      // Act & Assert
      expect(() => new Daemon(config)).toThrow();
    });

    it('should accept valid logLevel values', () => {
      // Arrange
      const validLevels = ['debug', 'info', 'warn', 'error'];

      // Act & Assert
      validLevels.forEach((level) => {
        const config = {
          daemonId: generateUUID(),
          name: 'test-daemon',
          logLevel: level,
        };
        expect(() => new Daemon(config)).not.toThrow();
      });
    });

    it('should validate healthCheckIntervalMs range', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
        healthCheckIntervalMs: 500, // Too low
      };

      // Act & Assert
      expect(() => new Daemon(config)).toThrow();
    });

    it('should validate YawlDaemonBridge config schema', () => {
      // Arrange
      const config = {
        daemonNodeId: 'node-1',
        maxConcurrentCases: 100,
      };

      // Act
      const result = YawlDaemonBridgeConfigSchema.parse(config);

      // Assert
      expect(result.daemonNodeId).toBe('node-1');
      expect(result.maxConcurrentCases).toBe(100);
    });

    it('should validate retry policy schema', () => {
      // Arrange
      const policy = {
        maxAttempts: 5,
        backoffMs: 2000,
        backoffMultiplier: 2.5,
      };

      // Act
      const result = YawlRetryPolicySchema.parse(policy);

      // Assert
      expect(result.maxAttempts).toBe(5);
      expect(result.backoffMs).toBe(2000);
      expect(result.backoffMultiplier).toBe(2.5);
    });

    it('should validate timeout config schema', () => {
      // Arrange
      const timeouts = {
        taskTimeoutMs: 60000,
        caseTimeoutMs: 300000,
        checkIntervalMs: 10000,
      };

      // Act
      const result = YawlTimeoutConfigSchema.parse(timeouts);

      // Assert
      expect(result.taskTimeoutMs).toBe(60000);
      expect(result.caseTimeoutMs).toBe(300000);
    });

    it('should validate distribution strategy enum', () => {
      // Arrange
      const validStrategies = ['round-robin', 'least-loaded', 'random', 'affinity'];

      // Act & Assert
      validStrategies.forEach((strategy) => {
        expect(() => DistributionStrategySchema.parse(strategy)).not.toThrow();
      });
    });

    it('should reject invalid distribution strategy', () => {
      // Arrange
      const invalid = 'invalid-strategy';

      // Act & Assert
      expect(() => DistributionStrategySchema.parse(invalid)).toThrow();
    });
  });

  describe('default values', () => {
    it('should apply default port', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.config.port).toBe(8080);
    });

    it('should apply default logLevel', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.config.logLevel).toBe('info');
    });

    it('should apply default concurrency', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.config.concurrency).toBe(10);
    });

    it('should apply default healthCheckIntervalMs', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.config.healthCheckIntervalMs).toBe(30000);
    });

    it('should apply default retry policy', () => {
      // Arrange
      const policy = YawlRetryPolicySchema.parse({});

      // Act & Assert
      expect(policy.maxAttempts).toBe(3);
      expect(policy.backoffMs).toBe(1000);
      expect(policy.backoffMultiplier).toBe(2);
      expect(policy.maxBackoffMs).toBe(30000);
    });

    it('should apply default timeout config', () => {
      // Arrange
      const timeouts = YawlTimeoutConfigSchema.parse({});

      // Act & Assert
      expect(timeouts.taskTimeoutMs).toBe(30000);
      expect(timeouts.caseTimeoutMs).toBe(3600000);
      expect(timeouts.checkIntervalMs).toBe(5000);
    });

    it('should apply default distribution strategy', () => {
      // Arrange
      const strategy = DistributionStrategySchema.parse(undefined);

      // Act & Assert
      expect(strategy).toBe('round-robin');
    });

    it('should apply default maxConcurrentCases', () => {
      // Arrange
      const config = {
        daemonNodeId: 'node-1',
      };

      // Act
      const result = YawlDaemonBridgeConfigSchema.parse(config);

      // Assert
      expect(result.maxConcurrentCases).toBe(100);
    });

    it('should apply default enableAutoRetry', () => {
      // Arrange
      const config = {
        daemonNodeId: 'node-1',
      };

      // Act
      const result = YawlDaemonBridgeConfigSchema.parse(config);

      // Assert
      expect(result.enableAutoRetry).toBe(true);
    });

    it('should apply default enableTimeoutTracking', () => {
      // Arrange
      const config = {
        daemonNodeId: 'node-1',
      };

      // Act
      const result = YawlDaemonBridgeConfigSchema.parse(config);

      // Assert
      expect(result.enableTimeoutTracking).toBe(true);
    });

    it('should generate default bridgeId', () => {
      // Arrange
      const config = {
        daemonNodeId: 'node-1',
      };

      // Act
      const result = YawlDaemonBridgeConfigSchema.parse(config);

      // Assert
      expect(result.bridgeId).toBeDefined();
      expect(result.bridgeId).toContain('yawl-bridge-');
    });

    it('should preserve user-provided values over defaults', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'custom-daemon',
        port: 9000,
        logLevel: 'debug',
        concurrency: 5,
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.config.port).toBe(9000);
      expect(daemon.config.logLevel).toBe('debug');
      expect(daemon.config.concurrency).toBe(5);
    });

    it('should support partial configuration override', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'partial-daemon',
        port: 8888, // Override only port
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.config.port).toBe(8888);
      expect(daemon.config.logLevel).toBe('info'); // Default
      expect(daemon.config.concurrency).toBe(10); // Default
    });
  });

  describe('configuration loading', () => {
    it('should load configuration from object', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'object-daemon',
        port: 8181,
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.config.daemonId).toBe(config.daemonId);
      expect(daemon.config.port).toBe(8181);
    });

    it('should deep copy configuration', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'copy-daemon',
        port: 8282,
      };

      // Act
      const daemon = new Daemon(config);
      config.port = 9999; // Modify original

      // Assert
      expect(daemon.config.port).toBe(8282); // Should not change
    });

    it('should validate during loading', () => {
      // Arrange
      const invalid = {
        daemonId: 'bad-id',
        name: 'invalid-daemon',
      };

      // Act & Assert
      expect(() => new Daemon(invalid)).toThrow();
    });

    it('should normalize configuration values', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: '  trimmed-daemon  ',
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.config.name).toBe('trimmed-daemon');
    });

    it('should load bridge configuration', () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'bridge-daemon',
      });
      const engine = new MockYawlEngine();
      const bridgeConfig = {
        daemonNodeId: 'node-bridge',
        maxConcurrentCases: 50,
      };

      // Act
      const bridge = new YawlDaemonBridge(daemon, engine, bridgeConfig);

      // Assert
      expect(bridge.config.daemonNodeId).toBe('node-bridge');
      expect(bridge.config.maxConcurrentCases).toBe(50);
    });

    it('should merge nested configuration objects', () => {
      // Arrange
      const config = {
        daemonNodeId: 'node-merge',
        retryPolicy: {
          maxAttempts: 5,
        },
      };

      // Act
      const result = YawlDaemonBridgeConfigSchema.parse(config);

      // Assert
      expect(result.retryPolicy.maxAttempts).toBe(5);
      expect(result.retryPolicy.backoffMs).toBe(1000); // Default merged
    });

    it('should handle empty configuration object', () => {
      // Arrange
      const config = {
        daemonNodeId: 'node-empty',
      };

      // Act
      const result = YawlDaemonBridgeConfigSchema.parse(config);

      // Assert
      expect(result).toBeDefined();
      expect(result.daemonNodeId).toBe('node-empty');
    });

    it('should load from factory function', () => {
      // Arrange
      const createConfig = () => ({
        daemonId: generateUUID(),
        name: 'factory-daemon',
      });

      // Act
      const daemon = new Daemon(createConfig());

      // Assert
      expect(daemon.config.name).toBe('factory-daemon');
    });

    it('should support configuration builder pattern conceptually', () => {
      // Arrange
      const baseConfig = {
        daemonId: generateUUID(),
        name: 'builder-daemon',
      };
      const extended = {
        ...baseConfig,
        port: 8383,
        logLevel: 'debug',
      };

      // Act
      const daemon = new Daemon(extended);

      // Assert
      expect(daemon.config.port).toBe(8383);
      expect(daemon.config.logLevel).toBe('debug');
    });

    it('should preserve type correctness during load', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'type-daemon',
        port: 8484,
        concurrency: 15,
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(typeof daemon.config.port).toBe('number');
      expect(typeof daemon.config.concurrency).toBe('number');
      expect(typeof daemon.config.name).toBe('string');
    });
  });

  describe('validation rules', () => {
    it('should enforce maxConcurrentCases > 0', () => {
      // Arrange
      const config = {
        daemonNodeId: 'node-validate',
        maxConcurrentCases: 0,
      };

      // Act & Assert
      expect(() => YawlDaemonBridgeConfigSchema.parse(config)).toThrow();
    });

    it('should enforce maxAttempts range', () => {
      // Arrange
      const invalidLow = { maxAttempts: 0 };
      const invalidHigh = { maxAttempts: 11 };

      // Act & Assert
      expect(() => YawlRetryPolicySchema.parse(invalidLow)).toThrow();
      expect(() => YawlRetryPolicySchema.parse(invalidHigh)).toThrow();
    });

    it('should enforce backoffMs minimum', () => {
      // Arrange
      const config = { backoffMs: 50 }; // Too low

      // Act & Assert
      expect(() => YawlRetryPolicySchema.parse(config)).toThrow();
    });

    it('should enforce taskTimeoutMs minimum', () => {
      // Arrange
      const config = { taskTimeoutMs: 500 }; // Too low

      // Act & Assert
      expect(() => YawlTimeoutConfigSchema.parse(config)).toThrow();
    });

    it('should enforce backoffMultiplier minimum', () => {
      // Arrange
      const config = { backoffMultiplier: 1.0 }; // Too low

      // Act & Assert
      expect(() => YawlRetryPolicySchema.parse(config)).toThrow();
    });

    it('should enforce jitterFactor range', () => {
      // Arrange
      const invalidLow = { jitterFactor: -0.1 };
      const invalidHigh = { jitterFactor: 1.1 };

      // Act & Assert
      expect(() => YawlRetryPolicySchema.parse(invalidLow)).toThrow();
      expect(() => YawlRetryPolicySchema.parse(invalidHigh)).toThrow();
    });

    it('should enforce daemonNodeId as required', () => {
      // Arrange
      const config = {
        maxConcurrentCases: 100,
      };

      // Act & Assert
      expect(() => YawlDaemonBridgeConfigSchema.parse(config)).toThrow();
    });

    it('should enforce name as non-empty string', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: '',
      };

      // Act & Assert
      expect(() => new Daemon(config)).toThrow();
    });

    it('should validate boolean flags', () => {
      // Arrange
      const config = {
        daemonNodeId: 'node-bool',
        enableAutoRetry: 'yes', // Invalid type
      };

      // Act & Assert
      expect(() => YawlDaemonBridgeConfigSchema.parse(config)).toThrow();
    });

    it('should accept valid boolean values', () => {
      // Arrange
      const config = {
        daemonNodeId: 'node-bool-valid',
        enableAutoRetry: true,
        enableTimeoutTracking: false,
      };

      // Act
      const result = YawlDaemonBridgeConfigSchema.parse(config);

      // Assert
      expect(result.enableAutoRetry).toBe(true);
      expect(result.enableTimeoutTracking).toBe(false);
    });

    it('should validate nested schema constraints', () => {
      // Arrange
      const config = {
        daemonNodeId: 'node-nested',
        retryPolicy: {
          maxAttempts: 3,
          backoffMs: 2000,
        },
        timeoutDefaults: {
          taskTimeoutMs: 60000,
        },
      };

      // Act
      const result = YawlDaemonBridgeConfigSchema.parse(config);

      // Assert
      expect(result.retryPolicy.maxAttempts).toBe(3);
      expect(result.timeoutDefaults.taskTimeoutMs).toBe(60000);
    });

    it('should reject extra unknown fields conceptually', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'strict-daemon',
        unknownField: 'should-be-ignored',
      };

      // Act
      const daemon = new Daemon(config);

      // Assert - Zod by default strips unknown fields
      expect(daemon.config).not.toHaveProperty('unknownField');
    });
  });

  describe('error handling', () => {
    it('should provide clear error message for invalid daemonId', () => {
      // Arrange
      const config = {
        daemonId: 'not-uuid',
        name: 'error-daemon',
      };

      // Act & Assert
      expect(() => new Daemon(config)).toThrow(/invalid|uuid/i);
    });

    it('should provide clear error for missing required field', () => {
      // Arrange
      const config = {
        name: 'no-id-daemon',
      };

      // Act & Assert
      expect(() => new Daemon(config)).toThrow(/required/i);
    });

    it('should handle null configuration gracefully', () => {
      // Arrange
      const config = null;

      // Act & Assert
      expect(() => new Daemon(config)).toThrow();
    });

    it('should handle undefined configuration gracefully', () => {
      // Arrange
      const config = undefined;

      // Act & Assert
      expect(() => new Daemon(config)).toThrow();
    });

    it('should report validation errors for bridge config', () => {
      // Arrange
      const config = {
        daemonNodeId: '', // Empty string
      };

      // Act & Assert
      expect(() => YawlDaemonBridgeConfigSchema.parse(config)).toThrow();
    });

    it('should aggregate multiple validation errors', () => {
      // Arrange
      const config = {
        // Missing daemonId and name
        port: 99999, // Invalid port
      };

      // Act
      let errorMessage = '';
      try {
        new Daemon(config);
      } catch (error) {
        errorMessage = error.message;
      }

      // Assert
      expect(errorMessage).toBeTruthy();
    });

    it('should handle type coercion errors', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'type-error-daemon',
        port: 'not-a-number',
      };

      // Act & Assert
      expect(() => new Daemon(config)).toThrow();
    });

    it('should validate logger object structure', () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'logger-daemon',
      });
      const engine = new MockYawlEngine();
      const config = {
        daemonNodeId: 'node-logger',
        logger: { invalid: 'logger' }, // Missing required methods
      };

      // Act
      const bridge = new YawlDaemonBridge(daemon, engine, config);

      // Assert
      expect(bridge.logger).toBeDefined();
    });

    it('should handle circular references in config', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'circular-daemon',
      };
      config.self = config; // Circular reference

      // Act
      const daemon = new Daemon(config);

      // Assert - Should handle without error
      expect(daemon.config.name).toBe('circular-daemon');
    });

    it('should provide helpful error for out-of-range values', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'range-error-daemon',
        concurrency: 150, // Exceeds max
      };

      // Act
      let errorMessage = '';
      try {
        new Daemon(config);
      } catch (error) {
        errorMessage = error.message;
      }

      // Assert
      expect(errorMessage).toContain('100');
    });

    it('should handle malformed retry policy', () => {
      // Arrange
      const config = {
        maxAttempts: 'invalid',
      };

      // Act & Assert
      expect(() => YawlRetryPolicySchema.parse(config)).toThrow();
    });

    it('should fail fast on critical validation errors', () => {
      // Arrange
      const config = {
        daemonId: null,
        name: null,
      };

      // Act
      const startTime = Date.now();
      try {
        new Daemon(config);
      } catch (e) {
        // Expected
      }
      const duration = Date.now() - startTime;

      // Assert
      expect(duration).toBeLessThan(100); // Should fail quickly
    });
  });
});
