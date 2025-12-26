/**
 * @file Error handling test suite
 */

import { describe, it, expect } from 'vitest';
import {
  UnrdfError,
  ValidationError,
  ConfigError,
  QueryError,
  StoreError,
  NetworkError,
  TimeoutError,
  ParserError,
  ERROR_CODES,
  createError,
  wrapError,
  assertError,
} from '../src/errors.mjs';

describe('Error Handling', () => {
  describe('UnrdfError', () => {
    it('should create basic error', () => {
      const error = new UnrdfError('Test error');

      expect(error).toBeInstanceOf(Error);
      expect(error.name).toBe('UnrdfError');
      expect(error.message).toBe('Test error');
      expect(error.code).toBe('ERR_UNRDF');
      expect(error.context).toEqual({});
      expect(error.timestamp).toBeDefined();
    });

    it('should create error with context', () => {
      const context = { foo: 'bar', count: 42 };
      const error = new UnrdfError('Test error', context);

      expect(error.context).toEqual(context);
    });

    it('should create error with custom code', () => {
      const error = new UnrdfError('Test error', {}, 'ERR_CUSTOM');

      expect(error.code).toBe('ERR_CUSTOM');
    });

    it('should generate documentation URL', () => {
      const error = new UnrdfError('Test error', {}, 'ERR_CUSTOM');

      expect(error.docsUrl).toBe('https://unrdf.dev/errors/ERR_CUSTOM');
    });

    it('should convert to string with context', () => {
      const error = new UnrdfError('Test error', { foo: 'bar' }, 'ERR_TEST');
      const str = error.toString();

      expect(str).toContain('UnrdfError [ERR_TEST]: Test error');
      expect(str).toContain('Context:');
      expect(str).toContain('"foo": "bar"');
      expect(str).toContain('Documentation: https://unrdf.dev/errors/ERR_TEST');
    });

    it('should convert to JSON', () => {
      const error = new UnrdfError('Test error', { foo: 'bar' }, 'ERR_TEST');
      const json = error.toJSON();

      expect(json.name).toBe('UnrdfError');
      expect(json.code).toBe('ERR_TEST');
      expect(json.message).toBe('Test error');
      expect(json.context).toEqual({ foo: 'bar' });
      expect(json.docsUrl).toBeDefined();
      expect(json.timestamp).toBeDefined();
      expect(json.stack).toBeDefined();
    });

    it('should filter stack trace', () => {
      const error = new UnrdfError('Test error');

      expect(error.stack).toBeDefined();
      expect(error.stack).not.toContain('node_modules');
      expect(error.stack).not.toContain('node:internal');
    });
  });

  describe('Specific Error Classes', () => {
    it('should create ValidationError', () => {
      const error = new ValidationError('Invalid data', { field: 'name' });

      expect(error).toBeInstanceOf(UnrdfError);
      expect(error.name).toBe('ValidationError');
      expect(error.code).toBe('ERR_VALIDATION');
      expect(error.context).toEqual({ field: 'name' });
    });

    it('should create ConfigError', () => {
      const error = new ConfigError('Invalid config', { key: 'timeout' });

      expect(error).toBeInstanceOf(UnrdfError);
      expect(error.name).toBe('ConfigError');
      expect(error.code).toBe('ERR_CONFIG');
    });

    it('should create QueryError', () => {
      const error = new QueryError('Query failed', { query: 'SELECT * WHERE { ?s ?p ?o }' });

      expect(error).toBeInstanceOf(UnrdfError);
      expect(error.name).toBe('QueryError');
      expect(error.code).toBe('ERR_QUERY');
    });

    it('should create StoreError', () => {
      const error = new StoreError('Store operation failed');

      expect(error).toBeInstanceOf(UnrdfError);
      expect(error.name).toBe('StoreError');
      expect(error.code).toBe('ERR_STORE');
    });

    it('should create NetworkError', () => {
      const error = new NetworkError('Network request failed', { url: 'http://example.org' });

      expect(error).toBeInstanceOf(UnrdfError);
      expect(error.name).toBe('NetworkError');
      expect(error.code).toBe('ERR_NETWORK');
    });

    it('should create TimeoutError', () => {
      const error = new TimeoutError('Operation timeout', { timeoutMs: 5000 });

      expect(error).toBeInstanceOf(UnrdfError);
      expect(error.name).toBe('TimeoutError');
      expect(error.code).toBe('ERR_TIMEOUT');
    });

    it('should create ParserError', () => {
      const error = new ParserError('Parse failed', { format: 'turtle' });

      expect(error).toBeInstanceOf(UnrdfError);
      expect(error.name).toBe('ParserError');
      expect(error.code).toBe('ERR_PARSER');
    });
  });

  describe('ERROR_CODES', () => {
    it('should define validation error codes', () => {
      expect(ERROR_CODES.ERR_INVALID_QUAD_SUBJECT).toBeDefined();
      expect(ERROR_CODES.ERR_INVALID_QUAD_SUBJECT.message).toBeTruthy();
      expect(ERROR_CODES.ERR_INVALID_QUAD_SUBJECT.solution).toBeTruthy();
      expect(ERROR_CODES.ERR_INVALID_QUAD_SUBJECT.example).toBeTruthy();
    });

    it('should define query error codes', () => {
      expect(ERROR_CODES.ERR_INVALID_SPARQL).toBeDefined();
      expect(ERROR_CODES.ERR_QUERY_EXECUTION).toBeDefined();
      expect(ERROR_CODES.ERR_QUERY_TIMEOUT).toBeDefined();
    });

    it('should define store error codes', () => {
      expect(ERROR_CODES.ERR_STORE_NOT_FOUND).toBeDefined();
      expect(ERROR_CODES.ERR_STORE_READ_ONLY).toBeDefined();
      expect(ERROR_CODES.ERR_QUAD_NOT_FOUND).toBeDefined();
    });

    it('should define network error codes', () => {
      expect(ERROR_CODES.ERR_NETWORK_REQUEST).toBeDefined();
      expect(ERROR_CODES.ERR_FEDERATION_TIMEOUT).toBeDefined();
    });
  });

  describe('createError', () => {
    it('should create error from code', () => {
      const error = createError('ERR_INVALID_QUAD_SUBJECT', {
        received: 'Literal',
        expected: ['NamedNode', 'BlankNode'],
      });

      expect(error).toBeInstanceOf(ValidationError);
      expect(error.code).toBe('ERR_INVALID_QUAD_SUBJECT');
      expect(error.message).toBe(ERROR_CODES.ERR_INVALID_QUAD_SUBJECT.message);
      expect(error.context.received).toBe('Literal');
      expect(error.context.solution).toBe(ERROR_CODES.ERR_INVALID_QUAD_SUBJECT.solution);
      expect(error.context.example).toBe(ERROR_CODES.ERR_INVALID_QUAD_SUBJECT.example);
    });

    it('should create QueryError for query codes', () => {
      const error = createError('ERR_INVALID_SPARQL');

      expect(error).toBeInstanceOf(QueryError);
      expect(error.code).toBe('ERR_INVALID_SPARQL');
    });

    it('should create StoreError for store codes', () => {
      const error = createError('ERR_STORE_NOT_FOUND');

      expect(error).toBeInstanceOf(StoreError);
      expect(error.code).toBe('ERR_STORE_NOT_FOUND');
    });

    it('should create NetworkError for network codes', () => {
      const error = createError('ERR_NETWORK_REQUEST');

      expect(error).toBeInstanceOf(NetworkError);
      expect(error.code).toBe('ERR_NETWORK_REQUEST');
    });

    it('should create TimeoutError for timeout codes', () => {
      const error = createError('ERR_QUERY_TIMEOUT');

      expect(error).toBeInstanceOf(TimeoutError);
      expect(error.code).toBe('ERR_QUERY_TIMEOUT');
    });

    it('should handle unknown error codes', () => {
      const error = createError('ERR_UNKNOWN_CODE', { foo: 'bar' });

      expect(error).toBeInstanceOf(UnrdfError);
      expect(error.code).toBe('ERR_UNKNOWN_CODE');
      expect(error.message).toContain('Unknown error code');
    });
  });

  describe('wrapError', () => {
    it('should wrap UnrdfError with additional context', () => {
      const original = new ValidationError('Invalid data', { field: 'name' });
      const wrapped = wrapError(original, { operation: 'addQuad' });

      expect(wrapped).toBe(original);
      expect(wrapped.context.field).toBe('name');
      expect(wrapped.context.operation).toBe('addQuad');
    });

    it('should wrap standard Error', () => {
      const original = new Error('Standard error');
      const wrapped = wrapError(original, { operation: 'test' });

      expect(wrapped).toBeInstanceOf(UnrdfError);
      expect(wrapped.code).toBe('ERR_WRAPPED');
      expect(wrapped.message).toBe('Standard error');
      expect(wrapped.context.operation).toBe('test');
      expect(wrapped.context.originalError).toBe('Error');
    });

    it('should handle empty context', () => {
      const original = new Error('Test');
      const wrapped = wrapError(original);

      expect(wrapped).toBeInstanceOf(UnrdfError);
      expect(wrapped.context).toBeDefined();
    });
  });

  describe('assertError', () => {
    it('should not throw when condition is true', () => {
      expect(() => {
        assertError(true, 'ERR_INVALID_QUAD_SUBJECT');
      }).not.toThrow();
    });

    it('should throw when condition is false', () => {
      expect(() => {
        assertError(false, 'ERR_INVALID_QUAD_SUBJECT', {
          received: 'Literal',
        });
      }).toThrow(ValidationError);
    });

    it('should throw with correct error details', () => {
      try {
        assertError(false, 'ERR_STORE_NOT_FOUND', { storeName: 'test' });
        throw new Error('Should have thrown');
      } catch (error) {
        expect(error).toBeInstanceOf(StoreError);
        expect(error.code).toBe('ERR_STORE_NOT_FOUND');
        expect(error.context.storeName).toBe('test');
      }
    });
  });

  describe('Error Message Quality', () => {
    it('should provide actionable error messages', () => {
      const error = createError('ERR_INVALID_QUAD_SUBJECT');

      expect(error.message).toContain('must be');
      expect(error.context.solution).toBeTruthy();
      expect(error.context.example).toBeTruthy();
    });

    it('should include code examples in context', () => {
      const error = createError('ERR_INVALID_SPARQL');

      expect(error.context.example).toContain('SELECT');
    });

    it('should suggest specific solutions', () => {
      const error = createError('ERR_QUERY_TIMEOUT');

      expect(error.context.solution).toContain('LIMIT');
    });
  });
});
