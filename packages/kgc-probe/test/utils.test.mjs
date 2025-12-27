/**
 * @fileoverview Utility Tests
 *
 * Tests for:
 * - Logger
 * - Error classes
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import {
  Logger,
  createLogger,
  defaultLogger,
  LOG_LEVELS
} from '../src/utils/logger.mjs';
import {
  ProbeError,
  GuardViolationError,
  ValidationError,
  MergeConflictError,
  ReceiptError,
  ArtifactNotFoundError,
  TimeoutError,
  AgentError,
  StorageError,
  ConfigurationError,
  ErrorCodes,
  isProbeError,
  wrapError
} from '../src/utils/errors.mjs';

// ============================================================================
// Logger Tests
// ============================================================================

describe('Logger', () => {
  let outputs;
  let errors;
  let logger;

  beforeEach(() => {
    outputs = [];
    errors = [];
    logger = createLogger({
      prefix: 'test',
      output: (msg) => outputs.push(msg),
      errorOutput: (msg) => errors.push(msg)
    });
  });

  describe('createLogger', () => {
    it('should create logger instance', () => {
      expect(logger).toBeInstanceOf(Logger);
    });

    it('should create default logger', () => {
      expect(defaultLogger).toBeInstanceOf(Logger);
    });

    it('should accept configuration', () => {
      const customLogger = createLogger({
        level: 'debug',
        json: false,
        prefix: 'custom'
      });
      expect(customLogger.level).toBe('debug');
      expect(customLogger.prefix).toBe('custom');
      expect(customLogger.json).toBe(false);
    });
  });

  describe('log levels', () => {
    it('should have correct level ordering', () => {
      expect(LOG_LEVELS.debug).toBeLessThan(LOG_LEVELS.info);
      expect(LOG_LEVELS.info).toBeLessThan(LOG_LEVELS.warn);
      expect(LOG_LEVELS.warn).toBeLessThan(LOG_LEVELS.error);
      expect(LOG_LEVELS.error).toBeLessThan(LOG_LEVELS.silent);
    });

    it('should respect log level', () => {
      const debugLogger = createLogger({
        level: 'warn',
        output: (msg) => outputs.push(msg),
        errorOutput: (msg) => errors.push(msg)
      });

      debugLogger.debug('debug');
      debugLogger.info('info');
      debugLogger.warn('warn');
      debugLogger.error('error');

      expect(outputs.length).toBe(0);
      expect(errors.length).toBe(2);
    });

    it('should allow all levels when debug', () => {
      const debugLogger = createLogger({
        level: 'debug',
        output: (msg) => outputs.push(msg),
        errorOutput: (msg) => errors.push(msg)
      });

      debugLogger.debug('debug');
      debugLogger.info('info');
      debugLogger.warn('warn');
      debugLogger.error('error');

      expect(outputs.length).toBe(2);
      expect(errors.length).toBe(2);
    });
  });

  describe('log methods', () => {
    it('should log debug messages', () => {
      logger.setLevel('debug');
      logger.debug('debug message', { key: 'value' });
      expect(outputs[0]).toContain('debug');
    });

    it('should log info messages', () => {
      logger.info('info message');
      expect(outputs[0]).toContain('info');
    });

    it('should log warning messages', () => {
      logger.warn('warning message');
      expect(errors[0]).toContain('warn');
    });

    it('should log error messages', () => {
      logger.error('error message');
      expect(errors[0]).toContain('error');
    });

    it('should include context in logs', () => {
      logger.info('message', { key: 'value', num: 42 });
      expect(outputs[0]).toContain('key');
      expect(outputs[0]).toContain('value');
    });

    it('should include prefix in messages', () => {
      logger.info('message');
      expect(outputs[0]).toContain('[test]');
    });
  });

  describe('JSON format', () => {
    it('should output valid JSON', () => {
      logger.info('message', { key: 'value' });
      const parsed = JSON.parse(outputs[0]);
      expect(parsed.level).toBe('info');
      expect(parsed.message).toContain('message');
    });

    it('should include timestamp', () => {
      logger.info('message');
      const parsed = JSON.parse(outputs[0]);
      expect(parsed.timestamp).toBeDefined();
    });

    it('should include context', () => {
      logger.info('message', { foo: 'bar' });
      const parsed = JSON.parse(outputs[0]);
      expect(parsed.context.foo).toBe('bar');
    });
  });

  describe('human-readable format', () => {
    it('should format without JSON', () => {
      const humanLogger = createLogger({
        json: false,
        output: (msg) => outputs.push(msg)
      });

      humanLogger.info('Hello World');
      expect(outputs[0]).toContain('INFO:');
      expect(outputs[0]).toContain('Hello World');
    });
  });

  describe('child logger', () => {
    it('should create child with extended prefix', () => {
      const child = logger.child('sub');
      child.info('message');
      expect(outputs[0]).toContain('[test:sub]');
    });

    it('should inherit configuration', () => {
      const parent = createLogger({ level: 'warn', json: true });
      const child = parent.child('child');
      expect(child.level).toBe('warn');
      expect(child.json).toBe(true);
    });
  });

  describe('timed', () => {
    it('should time async operations', async () => {
      const result = await logger.timed('operation', async () => {
        await new Promise(r => setTimeout(r, 10));
        return 42;
      });

      expect(result).toBe(42);
      expect(outputs[0]).toContain('duration_ms');
      expect(outputs[0]).toContain('success');
    });

    it('should log errors from timed operations', async () => {
      await expect(
        logger.timed('failing', async () => {
          throw new Error('failed');
        })
      ).rejects.toThrow('failed');

      expect(errors[0]).toContain('error');
      expect(errors[0]).toContain('failed');
    });
  });

  describe('setLevel', () => {
    it('should change log level', () => {
      logger.setLevel('error');
      expect(logger.level).toBe('error');
    });

    it('should reject invalid levels', () => {
      expect(() => logger.setLevel('invalid')).toThrow();
    });
  });
});

// ============================================================================
// Error Tests
// ============================================================================

describe('ProbeError', () => {
  it('should create error with message', () => {
    const error = new ProbeError('Something failed');
    expect(error.message).toBe('Something failed');
    expect(error.name).toBe('ProbeError');
  });

  it('should include error code', () => {
    const error = new ProbeError('Failed', 'MY_CODE');
    expect(error.code).toBe('MY_CODE');
  });

  it('should include context', () => {
    const error = new ProbeError('Failed', 'CODE', { key: 'value' });
    expect(error.context.key).toBe('value');
  });

  it('should include recovery suggestion', () => {
    const error = new ProbeError('Failed', 'CODE', {}, 'Try this');
    expect(error.recovery).toBe('Try this');
  });

  it('should include timestamp', () => {
    const error = new ProbeError('Failed');
    expect(error.timestamp).toBeDefined();
  });

  it('should convert to JSON', () => {
    const error = new ProbeError('Failed', 'CODE', { key: 'value' });
    const json = error.toJSON();
    expect(json.name).toBe('ProbeError');
    expect(json.code).toBe('CODE');
    expect(json.message).toBe('Failed');
    expect(json.context.key).toBe('value');
  });

  it('should create from unknown error', () => {
    const error = ProbeError.from(new Error('Original'));
    expect(error).toBeInstanceOf(ProbeError);
    expect(error.message).toBe('Original');
  });

  it('should pass through ProbeError', () => {
    const original = new ProbeError('Original', 'CODE');
    const wrapped = ProbeError.from(original);
    expect(wrapped).toBe(original);
  });

  it('should handle string errors', () => {
    const error = ProbeError.from('string error');
    expect(error.message).toBe('string error');
  });
});

describe('GuardViolationError', () => {
  it('should create guard violation error', () => {
    const error = new GuardViolationError(
      'Access denied',
      'G-H1-ENV-TOKEN',
      { variable: 'AWS_SECRET' }
    );

    expect(error).toBeInstanceOf(ProbeError);
    expect(error.name).toBe('GuardViolationError');
    expect(error.code).toBe('GUARD_VIOLATION');
    expect(error.guardId).toBe('G-H1-ENV-TOKEN');
  });

  it('should include receipt ID', () => {
    const error = new GuardViolationError(
      'Access denied',
      'G-H1-ENV-TOKEN',
      {},
      'receipt-123'
    );

    expect(error.receiptId).toBe('receipt-123');
    expect(error.context.receiptId).toBe('receipt-123');
  });
});

describe('ValidationError', () => {
  it('should create validation error', () => {
    const error = new ValidationError('Invalid input', { field: 'name' });

    expect(error).toBeInstanceOf(ProbeError);
    expect(error.name).toBe('ValidationError');
    expect(error.code).toBe('VALIDATION_ERROR');
  });

  it('should include field name', () => {
    const error = new ValidationError('Invalid input', {}, 'email');
    expect(error.field).toBe('email');
  });

  it('should create from Zod error', () => {
    // Mock Zod error structure
    const zodError = {
      issues: [
        { path: ['user', 'email'], message: 'Invalid email', code: 'invalid_string' }
      ]
    };

    const error = ValidationError.fromZod(zodError);
    expect(error.field).toBe('user.email');
    expect(error.issues.length).toBe(1);
  });
});

describe('MergeConflictError', () => {
  it('should create merge conflict error', () => {
    const conflicts = [
      { claimId: 'cap-1', agents: ['agent-1', 'agent-2'] }
    ];
    const error = new MergeConflictError('Conflicts detected', conflicts);

    expect(error).toBeInstanceOf(ProbeError);
    expect(error.name).toBe('MergeConflictError');
    expect(error.code).toBe('MERGE_CONFLICT');
    expect(error.conflicts).toBe(conflicts);
  });
});

describe('ReceiptError', () => {
  it('should create receipt error', () => {
    const error = new ReceiptError('Hash mismatch', { expected: '0xabc' }, 'hash_chain');

    expect(error).toBeInstanceOf(ProbeError);
    expect(error.name).toBe('ReceiptError');
    expect(error.code).toBe('RECEIPT_ERROR');
    expect(error.verificationStep).toBe('hash_chain');
  });
});

describe('ArtifactNotFoundError', () => {
  it('should create artifact not found error', () => {
    const error = new ArtifactNotFoundError('run-123');

    expect(error).toBeInstanceOf(ProbeError);
    expect(error.name).toBe('ArtifactNotFoundError');
    expect(error.code).toBe('ARTIFACT_NOT_FOUND');
    expect(error.artifactId).toBe('run-123');
  });

  it('should include path', () => {
    const error = new ArtifactNotFoundError('run-123', './artifacts/');
    expect(error.context.path).toBe('./artifacts/');
  });
});

describe('TimeoutError', () => {
  it('should create timeout error', () => {
    const error = new TimeoutError('Scan timed out', 30000, 'scan');

    expect(error).toBeInstanceOf(ProbeError);
    expect(error.name).toBe('TimeoutError');
    expect(error.code).toBe('TIMEOUT');
    expect(error.timeoutMs).toBe(30000);
  });
});

describe('AgentError', () => {
  it('should create agent error', () => {
    const cause = new Error('Query failed');
    const error = new AgentError('Agent failed', 'completeness-agent', cause);

    expect(error).toBeInstanceOf(ProbeError);
    expect(error.name).toBe('AgentError');
    expect(error.code).toBe('AGENT_ERROR');
    expect(error.agentId).toBe('completeness-agent');
    expect(error.cause).toBe(cause);
  });
});

describe('StorageError', () => {
  it('should create storage error', () => {
    const error = new StorageError('Write failed', 'write', { path: '/tmp/file' });

    expect(error).toBeInstanceOf(ProbeError);
    expect(error.name).toBe('StorageError');
    expect(error.code).toBe('STORAGE_ERROR');
    expect(error.operation).toBe('write');
  });
});

describe('ConfigurationError', () => {
  it('should create configuration error', () => {
    const error = new ConfigurationError('Invalid config', { path: '.kgc-probe.json' });

    expect(error).toBeInstanceOf(ProbeError);
    expect(error.name).toBe('ConfigurationError');
    expect(error.code).toBe('CONFIG_ERROR');
  });
});

describe('ErrorCodes', () => {
  it('should export all error codes', () => {
    expect(ErrorCodes.PROBE_ERROR).toBe('PROBE_ERROR');
    expect(ErrorCodes.GUARD_VIOLATION).toBe('GUARD_VIOLATION');
    expect(ErrorCodes.VALIDATION_ERROR).toBe('VALIDATION_ERROR');
    expect(ErrorCodes.MERGE_CONFLICT).toBe('MERGE_CONFLICT');
    expect(ErrorCodes.RECEIPT_ERROR).toBe('RECEIPT_ERROR');
    expect(ErrorCodes.ARTIFACT_NOT_FOUND).toBe('ARTIFACT_NOT_FOUND');
    expect(ErrorCodes.TIMEOUT).toBe('TIMEOUT');
    expect(ErrorCodes.AGENT_ERROR).toBe('AGENT_ERROR');
    expect(ErrorCodes.STORAGE_ERROR).toBe('STORAGE_ERROR');
    expect(ErrorCodes.CONFIG_ERROR).toBe('CONFIG_ERROR');
  });
});

describe('isProbeError', () => {
  it('should return true for ProbeError', () => {
    expect(isProbeError(new ProbeError('test'))).toBe(true);
  });

  it('should return true for subclasses', () => {
    expect(isProbeError(new GuardViolationError('test', 'G-1'))).toBe(true);
    expect(isProbeError(new ValidationError('test'))).toBe(true);
  });

  it('should return false for regular Error', () => {
    expect(isProbeError(new Error('test'))).toBe(false);
  });

  it('should return false for non-errors', () => {
    expect(isProbeError('string')).toBe(false);
    expect(isProbeError(null)).toBe(false);
    expect(isProbeError(undefined)).toBe(false);
  });
});

describe('wrapError', () => {
  it('should wrap regular error', () => {
    const wrapped = wrapError(new Error('original'));
    expect(wrapped).toBeInstanceOf(ProbeError);
    expect(wrapped.message).toBe('original');
  });

  it('should pass through ProbeError', () => {
    const original = new ProbeError('original');
    const wrapped = wrapError(original);
    expect(wrapped).toBe(original);
  });

  it('should use custom code', () => {
    const wrapped = wrapError(new Error('test'), 'CUSTOM_CODE');
    expect(wrapped.code).toBe('CUSTOM_CODE');
  });
});
