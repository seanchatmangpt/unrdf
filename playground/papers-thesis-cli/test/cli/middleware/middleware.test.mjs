/**
 * @fileoverview Middleware Integration Tests
 *
 * @description
 * Tests for CLI middleware including logging, profiling,
 * validation, and config middleware.
 *
 * @module test/cli/middleware
 * @version 1.0.0
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  applyMiddleware,
  createContext,
  registerMiddleware,
  unregisterMiddleware,
  listMiddleware,
  compose,
  runWithMiddleware,
} from '../../../src/cli/middleware/index.mjs';

import {
  loggingMiddleware,
  maskSensitiveArgs,
  createLogEntry,
  setLogLevel,
} from '../../../src/cli/middleware/logging.mjs';

import {
  profilingMiddleware,
  createSpan,
  createProfilingContext,
  exportSpans,
} from '../../../src/cli/middleware/profiling.mjs';

import {
  validationMiddleware,
  CommonSchemas,
  Preconditions,
  registerValidator,
  createArgsSchema,
} from '../../../src/cli/middleware/validation.mjs';

import {
  configMiddleware,
  DEFAULT_CONFIG,
  ConfigSchema,
  mergeConfigs,
  applyCliOverrides,
  clearConfigCache,
} from '../../../src/cli/middleware/config.mjs';

describe('Middleware Orchestrator', () => {
  beforeEach(() => {
    // Clear any custom middleware between tests
    clearConfigCache();
  });

  describe('createContext', () => {
    it('should create a context with default values', () => {
      const ctx = createContext();

      expect(ctx.command).toBe('');
      expect(ctx.args).toEqual({});
      expect(ctx.options).toEqual({});
      expect(ctx.errors).toEqual([]);
      expect(ctx.warnings).toEqual([]);
      expect(ctx.meta.phase).toBe('init');
      expect(ctx.timing.checkpoints).toEqual([]);
    });

    it('should create a context with provided options', () => {
      const ctx = createContext({
        command: 'papers',
        args: { title: 'Test' },
        quiet: true,
      });

      expect(ctx.command).toBe('papers');
      expect(ctx.args.title).toBe('Test');
      expect(ctx.quiet).toBe(true);
    });
  });

  describe('applyMiddleware', () => {
    it('should apply middleware in sequence', async () => {
      const ctx = createContext({
        command: 'test',
        args: { format: 'json' },
        quiet: true,
      });

      const result = await applyMiddleware(ctx);

      expect(result.meta.middlewareExecuted).toContain('config');
      expect(result.meta.middlewareExecuted).toContain('validation');
      expect(result.meta.middlewareExecuted).toContain('logging');
      expect(result.meta.middlewareExecuted).toContain('profiling');
    });

    it('should skip specified middleware', async () => {
      const ctx = createContext({ command: 'test', quiet: true });

      const result = await applyMiddleware(ctx, {
        skip: ['profiling', 'validation'],
      });

      expect(result.meta.middlewareExecuted).not.toContain('profiling');
      expect(result.meta.middlewareExecuted).not.toContain('validation');
    });

    it('should run only specified middleware', async () => {
      const ctx = createContext({ command: 'test', quiet: true });

      const result = await applyMiddleware(ctx, {
        only: ['config'],
      });

      expect(result.meta.middlewareExecuted).toEqual(['config']);
    });
  });

  describe('registerMiddleware', () => {
    it('should register custom middleware', async () => {
      const customHandler = vi.fn(ctx => ({ ...ctx, custom: true }));

      registerMiddleware('custom-test', customHandler);

      const middleware = listMiddleware();
      const custom = middleware.find(m => m.name === 'custom-test');

      expect(custom).toBeDefined();
      expect(custom.builtin).toBe(false);

      // Cleanup
      unregisterMiddleware('custom-test');
    });

    it('should throw on invalid middleware', () => {
      expect(() => registerMiddleware('', () => {})).toThrow();
      expect(() => registerMiddleware('test', 'not-a-function')).toThrow();
    });
  });

  describe('compose', () => {
    it('should compose middleware functions', async () => {
      const addA = ctx => ({ ...ctx, a: true });
      const addB = ctx => ({ ...ctx, b: true });

      const composed = compose([addA, addB]);
      const result = await composed({ command: 'test' });

      expect(result.a).toBe(true);
      expect(result.b).toBe(true);
    });
  });
});

describe('Logging Middleware', () => {
  describe('maskSensitiveArgs', () => {
    it('should mask sensitive arguments', () => {
      const args = {
        username: 'alice',
        password: 'secret123',
        apiKey: 'key-abc',
        title: 'My Paper',
      };

      const masked = maskSensitiveArgs(args);

      expect(masked.username).toBe('alice');
      expect(masked.password).toBe('***MASKED***');
      expect(masked.apiKey).toBe('***MASKED***');
      expect(masked.title).toBe('My Paper');
    });
  });

  describe('createLogEntry', () => {
    it('should create JSON-LD formatted log entry', () => {
      const ctx = { command: 'papers', meta: { phase: 'test' } };
      const entry = createLogEntry('info', 'Test message', { foo: 'bar' }, ctx);

      expect(entry['@context']).toBeDefined();
      expect(entry['@type']).toBe('log:LogEntry');
      expect(entry.level).toBe('info');
      expect(entry.message).toBe('Test message');
      expect(entry.command).toBe('papers');
      expect(entry.data.foo).toBe('bar');
    });
  });

  describe('loggingMiddleware', () => {
    it('should add logging utilities to context', async () => {
      const ctx = createContext({ command: 'test', quiet: true });

      const result = await loggingMiddleware(ctx);

      expect(result.log).toBeDefined();
      expect(typeof result.log.info).toBe('function');
      expect(typeof result.log.error).toBe('function');
      expect(typeof result.log.warn).toBe('function');
      expect(typeof result.log.debug).toBe('function');
    });
  });
});

describe('Profiling Middleware', () => {
  describe('createSpan', () => {
    it('should create OTEL-format span', () => {
      const span = createSpan('test-span', 'trace-id-123');

      expect(span.traceId).toBe('trace-id-123');
      expect(span.spanId).toBeDefined();
      expect(span.name).toBe('test-span');
      expect(span.kind).toBe('SPAN_KIND_INTERNAL');
      expect(span.startTimeUnixNano).toBeDefined();
    });

    it('should support attributes and events', () => {
      const span = createSpan('test-span', 'trace-123');

      span.setAttribute('test.key', 'test-value');
      span.addEvent('test-event', { foo: 'bar' });
      span.end();

      expect(span.attributes).toHaveLength(1);
      expect(span.events).toHaveLength(1);
      expect(span.endTimeUnixNano).toBeDefined();
    });

    it('should export to OTEL format', () => {
      const span = createSpan('test-span', 'trace-123');
      span.end();

      const otel = span.toOTEL();

      expect(otel.traceId).toBe('trace-123');
      expect(otel.status.code).toBe('STATUS_CODE_OK');
    });
  });

  describe('profilingMiddleware', () => {
    it('should add profiling utilities to context', async () => {
      const ctx = createContext({ command: 'test', quiet: true });

      const result = await profilingMiddleware(ctx);

      expect(result.profiling).toBeDefined();
      expect(typeof result.profiling.startSpan).toBe('function');
      expect(typeof result.profiling.recordMetric).toBe('function');
      expect(typeof result.profiling.checkpoint).toBe('function');
      expect(typeof result.profiling.getMetrics).toBe('function');
    });
  });
});

describe('Validation Middleware', () => {
  describe('CommonSchemas', () => {
    it('should validate output format', () => {
      const result = CommonSchemas.outputFormat.safeParse('json');
      expect(result.success).toBe(true);

      const invalid = CommonSchemas.outputFormat.safeParse('invalid');
      expect(invalid.success).toBe(false);
    });

    it('should validate paper family', () => {
      const result = CommonSchemas.paperFamily.safeParse('IMRaD');
      expect(result.success).toBe(true);
      expect(result.data).toBe('imrad'); // Transformed to lowercase

      // Also test lowercase
      const lower = CommonSchemas.paperFamily.safeParse('imrad');
      expect(lower.success).toBe(true);
    });

    it('should validate non-empty string', () => {
      const valid = CommonSchemas.nonEmptyString.safeParse('hello');
      expect(valid.success).toBe(true);

      const invalid = CommonSchemas.nonEmptyString.safeParse('');
      expect(invalid.success).toBe(false);
    });
  });

  describe('Preconditions', () => {
    it('should check value in allowed set', () => {
      const result = Preconditions.oneOf('json', ['json', 'yaml', 'table']);
      expect(result.valid).toBe(true);

      const invalid = Preconditions.oneOf('xml', ['json', 'yaml', 'table']);
      expect(invalid.valid).toBe(false);
      expect(invalid.error).toContain('must be one of');
    });

    it('should check environment variable', () => {
      process.env.TEST_VAR = 'test';
      const result = Preconditions.envVar('TEST_VAR');
      expect(result.valid).toBe(true);
      delete process.env.TEST_VAR;

      const invalid = Preconditions.envVar('NONEXISTENT_VAR_12345');
      expect(invalid.valid).toBe(false);
    });
  });

  describe('registerValidator', () => {
    it('should register and use custom validator', async () => {
      registerValidator('custom-validator', (value, ctx) => {
        return { valid: value === 'expected', error: 'Value mismatch' };
      });

      const validator = await import('../../../src/cli/middleware/validation.mjs').then(m =>
        m.getValidator('custom-validator')
      );

      expect(validator).toBeDefined();
      expect(validator('expected', {}).valid).toBe(true);
      expect(validator('other', {}).valid).toBe(false);
    });
  });
});

describe('Config Middleware', () => {
  beforeEach(() => {
    clearConfigCache();
  });

  describe('DEFAULT_CONFIG', () => {
    it('should have expected structure', () => {
      expect(DEFAULT_CONFIG.logging).toBeDefined();
      expect(DEFAULT_CONFIG.output).toBeDefined();
      expect(DEFAULT_CONFIG.validation).toBeDefined();
      expect(DEFAULT_CONFIG.middleware).toBeDefined();
      expect(DEFAULT_CONFIG.templates).toBeDefined();
      expect(DEFAULT_CONFIG.knowledgeGraph).toBeDefined();
    });
  });

  describe('mergeConfigs', () => {
    it('should deep merge configurations', () => {
      const base = {
        logging: { level: 'info', format: 'pretty' },
        output: { format: 'table' },
      };

      const override = {
        logging: { level: 'debug' },
        custom: 'value',
      };

      const result = mergeConfigs(base, override);

      expect(result.logging.level).toBe('debug');
      expect(result.logging.format).toBe('pretty');
      expect(result.output.format).toBe('table');
      expect(result.custom).toBe('value');
    });
  });

  describe('applyCliOverrides', () => {
    it('should apply CLI argument overrides', () => {
      const config = { ...DEFAULT_CONFIG };
      const args = { format: 'json', verbose: true };

      const result = applyCliOverrides(config, args);

      expect(result.output.format).toBe('json');
      expect(result.logging.level).toBe('debug');
    });

    it('should apply dot-notation overrides', () => {
      const config = { ...DEFAULT_CONFIG };
      const args = { 'config.logging.format': 'jsonld' };

      const result = applyCliOverrides(config, args);

      expect(result.logging.format).toBe('jsonld');
    });
  });

  describe('ConfigSchema', () => {
    it('should validate default config', () => {
      const result = ConfigSchema.safeParse(DEFAULT_CONFIG);
      expect(result.success).toBe(true);
    });

    it('should apply defaults for missing values', () => {
      const result = ConfigSchema.safeParse({});
      expect(result.success).toBe(true);
      expect(result.data.logging.level).toBe('info');
    });
  });

  describe('configMiddleware', () => {
    it('should add config utilities to context', async () => {
      const ctx = createContext({ command: 'test', quiet: true });

      const result = await configMiddleware(ctx);

      expect(result.config).toBeDefined();
      expect(typeof result.getConfig).toBe('function');
      expect(typeof result.setConfig).toBe('function');
    });

    it('should support getConfig with path', async () => {
      const ctx = createContext({ command: 'test', quiet: true });

      const result = await configMiddleware(ctx);

      expect(result.getConfig('logging.level')).toBe('info');
      expect(result.getConfig('nonexistent', 'default')).toBe('default');
    });
  });
});
