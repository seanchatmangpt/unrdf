/**
 * @file Message Validator Tests
 * @module test/message-validator.test
 *
 * @description
 * Comprehensive tests for distributed message validation.
 * Tests valid and invalid cases for each schema type.
 */

import { describe, it, expect } from 'vitest';
import {
  messageSchemas,
  validateTriplePattern,
  validateRPCCall,
  validateRPCResult,
  validateSPARQLQuery,
  validateBatchOperation,
  validateHealthCheck,
  validateMessage,
  parseTriplePattern,
  parseRPCCall,
  parseRPCResult,
  parseSPARQLQuery,
  createValidationMiddleware,
  withValidation,
} from '../src/message-validator.mjs';

// ============================================================================
// Triple Pattern Schema Tests
// ============================================================================

describe('validateTriplePattern', () => {
  describe('valid patterns', () => {
    it('should accept empty pattern (wildcard)', () => {
      const result = validateTriplePattern({});
      expect(result.success).toBe(true);
      expect(result.data).toEqual({});
    });

    it('should accept pattern with subject only', () => {
      const result = validateTriplePattern({ s: 'http://example.org/alice' });
      expect(result.success).toBe(true);
      expect(result.data.s).toBe('http://example.org/alice');
    });

    it('should accept pattern with all fields', () => {
      const result = validateTriplePattern({
        s: 'http://example.org/alice',
        p: 'http://xmlns.com/foaf/0.1/name',
        o: 'Alice',
      });
      expect(result.success).toBe(true);
      expect(result.data).toEqual({
        s: 'http://example.org/alice',
        p: 'http://xmlns.com/foaf/0.1/name',
        o: 'Alice',
      });
    });

    it('should accept null values for wildcard matching', () => {
      const result = validateTriplePattern({ s: null, p: null, o: null });
      expect(result.success).toBe(true);
    });

    it('should accept undefined values', () => {
      const result = validateTriplePattern({ s: undefined, p: 'http://example.org/pred' });
      expect(result.success).toBe(true);
    });
  });

  describe('invalid patterns', () => {
    it('should reject non-string subject', () => {
      const result = validateTriplePattern({ s: 123 });
      expect(result.success).toBe(false);
      expect(result.error).toContain('s');
    });

    it('should reject extra fields (strict mode)', () => {
      const result = validateTriplePattern({ s: 'test', extraField: 'not allowed' });
      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });

    it('should reject array values', () => {
      const result = validateTriplePattern({ s: ['a', 'b'] });
      expect(result.success).toBe(false);
    });
  });
});

// ============================================================================
// RPC Call Schema Tests
// ============================================================================

describe('validateRPCCall', () => {
  describe('valid calls', () => {
    it('should accept valid RPC call with all fields', () => {
      const result = validateRPCCall({
        target: 'node-1',
        module: 'graph',
        function: 'query',
        args: ['SELECT * WHERE { ?s ?p ?o }'],
      });
      expect(result.success).toBe(true);
      expect(result.data).toEqual({
        target: 'node-1',
        module: 'graph',
        function: 'query',
        args: ['SELECT * WHERE { ?s ?p ?o }'],
      });
    });

    it('should accept RPC call without args (defaults to empty array)', () => {
      const result = validateRPCCall({
        target: 'node-1',
        module: 'system',
        function: 'ping',
      });
      expect(result.success).toBe(true);
      expect(result.data.args).toEqual([]);
    });

    it('should accept RPC call with complex args', () => {
      const result = validateRPCCall({
        target: 'node-replica-2',
        module: 'knowledge_graph',
        function: 'batch_insert',
        args: [{ triples: [{ s: 'a', p: 'b', o: 'c' }] }, { timeout: 5000 }],
      });
      expect(result.success).toBe(true);
    });
  });

  describe('invalid calls', () => {
    it('should reject missing target', () => {
      const result = validateRPCCall({
        module: 'graph',
        function: 'query',
        args: [],
      });
      expect(result.success).toBe(false);
      expect(result.error).toContain('target');
    });

    it('should reject empty target string', () => {
      const result = validateRPCCall({
        target: '',
        module: 'graph',
        function: 'query',
        args: [],
      });
      expect(result.success).toBe(false);
      expect(result.error).toContain('Target');
    });

    it('should reject missing module', () => {
      const result = validateRPCCall({
        target: 'node-1',
        function: 'query',
        args: [],
      });
      expect(result.success).toBe(false);
      expect(result.error).toContain('module');
    });

    it('should reject missing function', () => {
      const result = validateRPCCall({
        target: 'node-1',
        module: 'graph',
        args: [],
      });
      expect(result.success).toBe(false);
      expect(result.error).toContain('function');
    });

    it('should reject args that is not an array', () => {
      const result = validateRPCCall({
        target: 'node-1',
        module: 'graph',
        function: 'query',
        args: 'not-an-array',
      });
      expect(result.success).toBe(false);
    });

    it('should reject extra fields', () => {
      const result = validateRPCCall({
        target: 'node-1',
        module: 'graph',
        function: 'query',
        args: [],
        unauthorized: 'field',
      });
      expect(result.success).toBe(false);
    });
  });
});

// ============================================================================
// RPC Result Schema Tests
// ============================================================================

describe('validateRPCResult', () => {
  describe('valid results', () => {
    it('should accept successful result', () => {
      const result = validateRPCResult({
        ok: true,
        result: { count: 42 },
      });
      expect(result.success).toBe(true);
      expect(result.data.ok).toBe(true);
      expect(result.data.result).toEqual({ count: 42 });
    });

    it('should accept error result with message', () => {
      const result = validateRPCResult({
        ok: false,
        result: null,
        error: 'Connection timeout',
      });
      expect(result.success).toBe(true);
      expect(result.data.ok).toBe(false);
      expect(result.data.error).toBe('Connection timeout');
    });

    it('should accept result with null result value', () => {
      const result = validateRPCResult({
        ok: true,
        result: null,
      });
      expect(result.success).toBe(true);
    });

    it('should accept result with undefined error on success', () => {
      const result = validateRPCResult({
        ok: true,
        result: 'data',
        error: undefined,
      });
      expect(result.success).toBe(true);
    });
  });

  describe('invalid results', () => {
    it('should reject error result without error message', () => {
      const result = validateRPCResult({
        ok: false,
        result: null,
      });
      expect(result.success).toBe(false);
      expect(result.error).toContain('error');
    });

    it('should reject missing ok field', () => {
      const result = validateRPCResult({
        result: 'some data',
      });
      expect(result.success).toBe(false);
    });

    it('should reject non-boolean ok field', () => {
      const result = validateRPCResult({
        ok: 'true',
        result: null,
      });
      expect(result.success).toBe(false);
    });

    it('should reject extra fields', () => {
      const result = validateRPCResult({
        ok: true,
        result: 'data',
        metadata: { shouldNotExist: true },
      });
      expect(result.success).toBe(false);
    });
  });
});

// ============================================================================
// SPARQL Query Schema Tests
// ============================================================================

describe('validateSPARQLQuery', () => {
  describe('valid queries', () => {
    it('should accept query without params', () => {
      const result = validateSPARQLQuery({
        query: 'SELECT * WHERE { ?s ?p ?o }',
      });
      expect(result.success).toBe(true);
      expect(result.data.query).toBe('SELECT * WHERE { ?s ?p ?o }');
    });

    it('should accept query with params', () => {
      const result = validateSPARQLQuery({
        query: 'SELECT * WHERE { ?s ?p ?o } LIMIT $limit',
        params: { limit: 100 },
      });
      expect(result.success).toBe(true);
      expect(result.data.params).toEqual({ limit: 100 });
    });

    it('should accept query with complex params', () => {
      const result = validateSPARQLQuery({
        query: 'SELECT ?name WHERE { $person foaf:name ?name }',
        params: {
          person: 'http://example.org/alice',
          limit: 10,
          offset: 0,
        },
      });
      expect(result.success).toBe(true);
    });

    it('should accept null params', () => {
      const result = validateSPARQLQuery({
        query: 'SELECT * WHERE { ?s ?p ?o }',
        params: null,
      });
      expect(result.success).toBe(true);
    });
  });

  describe('invalid queries', () => {
    it('should reject empty query string', () => {
      const result = validateSPARQLQuery({
        query: '',
      });
      expect(result.success).toBe(false);
      expect(result.error).toContain('query');
    });

    it('should reject missing query', () => {
      const result = validateSPARQLQuery({
        params: { limit: 100 },
      });
      expect(result.success).toBe(false);
    });

    it('should reject non-string query', () => {
      const result = validateSPARQLQuery({
        query: { sparql: 'SELECT * WHERE { ?s ?p ?o }' },
      });
      expect(result.success).toBe(false);
    });

    it('should reject extra fields', () => {
      const result = validateSPARQLQuery({
        query: 'SELECT * WHERE { ?s ?p ?o }',
        format: 'json',
      });
      expect(result.success).toBe(false);
    });
  });
});

// ============================================================================
// Batch Operation Schema Tests
// ============================================================================

describe('validateBatchOperation', () => {
  describe('valid operations', () => {
    it('should accept insert operation', () => {
      const result = validateBatchOperation({
        operation: 'insert',
        triples: [{ s: 'http://example.org/a', p: 'http://example.org/b', o: 'c' }],
      });
      expect(result.success).toBe(true);
    });

    it('should accept delete operation', () => {
      const result = validateBatchOperation({
        operation: 'delete',
        triples: [{ s: 'http://example.org/a' }],
      });
      expect(result.success).toBe(true);
    });

    it('should accept operation with transactionId', () => {
      const result = validateBatchOperation({
        operation: 'update',
        triples: [{ s: 'a', p: 'b', o: 'c' }],
        transactionId: 'tx-12345',
      });
      expect(result.success).toBe(true);
      expect(result.data.transactionId).toBe('tx-12345');
    });
  });

  describe('invalid operations', () => {
    it('should reject empty triples array', () => {
      const result = validateBatchOperation({
        operation: 'insert',
        triples: [],
      });
      expect(result.success).toBe(false);
      expect(result.error).toContain('triple');
    });

    it('should reject invalid operation type', () => {
      const result = validateBatchOperation({
        operation: 'merge',
        triples: [{ s: 'a' }],
      });
      expect(result.success).toBe(false);
    });
  });
});

// ============================================================================
// Health Check Schema Tests
// ============================================================================

describe('validateHealthCheck', () => {
  describe('valid health checks', () => {
    it('should accept minimal health check', () => {
      const result = validateHealthCheck({
        nodeId: 'node-1',
        timestamp: Date.now(),
        status: 'healthy',
      });
      expect(result.success).toBe(true);
    });

    it('should accept health check with metrics', () => {
      const result = validateHealthCheck({
        nodeId: 'node-1',
        timestamp: Date.now(),
        status: 'degraded',
        metrics: {
          latency: 150.5,
          errorRate: 0.05,
          queueDepth: 42,
        },
      });
      expect(result.success).toBe(true);
    });
  });

  describe('invalid health checks', () => {
    it('should reject invalid status', () => {
      const result = validateHealthCheck({
        nodeId: 'node-1',
        timestamp: Date.now(),
        status: 'unknown',
      });
      expect(result.success).toBe(false);
    });

    it('should reject negative timestamp', () => {
      const result = validateHealthCheck({
        nodeId: 'node-1',
        timestamp: -1,
        status: 'healthy',
      });
      expect(result.success).toBe(false);
    });

    it('should reject error rate > 1', () => {
      const result = validateHealthCheck({
        nodeId: 'node-1',
        timestamp: Date.now(),
        status: 'healthy',
        metrics: { errorRate: 1.5 },
      });
      expect(result.success).toBe(false);
    });
  });
});

// ============================================================================
// Generic Validation Tests
// ============================================================================

describe('validateMessage', () => {
  it('should validate known schema types', () => {
    const result = validateMessage('triplePattern', { s: 'test' });
    expect(result.success).toBe(true);
  });

  it('should return error for unknown schema type', () => {
    const result = validateMessage('unknownType', { data: 'test' });
    expect(result.success).toBe(false);
    expect(result.error).toContain('Unknown schema type');
    expect(result.error).toContain('Available');
  });
});

// ============================================================================
// Parse Functions (Strict Mode) Tests
// ============================================================================

describe('parse functions (strict mode)', () => {
  describe('parseTriplePattern', () => {
    it('should return data for valid input', () => {
      const data = parseTriplePattern({ s: 'test' });
      expect(data.s).toBe('test');
    });

    it('should throw for invalid input', () => {
      expect(() => parseTriplePattern({ s: 123 })).toThrow('Invalid triple pattern');
    });
  });

  describe('parseRPCCall', () => {
    it('should return data for valid input', () => {
      const data = parseRPCCall({
        target: 'node1',
        module: 'test',
        function: 'ping',
        args: [],
      });
      expect(data.target).toBe('node1');
    });

    it('should throw for invalid input', () => {
      expect(() => parseRPCCall({ target: '' })).toThrow('Invalid RPC call');
    });
  });

  describe('parseRPCResult', () => {
    it('should return data for valid input', () => {
      const data = parseRPCResult({ ok: true, result: 'success' });
      expect(data.ok).toBe(true);
    });

    it('should throw for invalid input', () => {
      expect(() => parseRPCResult({ ok: false })).toThrow('Invalid RPC result');
    });
  });

  describe('parseSPARQLQuery', () => {
    it('should return data for valid input', () => {
      const data = parseSPARQLQuery({ query: 'SELECT *' });
      expect(data.query).toBe('SELECT *');
    });

    it('should throw for invalid input', () => {
      expect(() => parseSPARQLQuery({ query: '' })).toThrow('Invalid SPARQL query');
    });
  });
});

// ============================================================================
// Integration Helpers Tests
// ============================================================================

describe('createValidationMiddleware', () => {
  it('should create middleware for known schema', () => {
    const validate = createValidationMiddleware('rpcCall');
    const result = validate({
      target: 'node1',
      module: 'test',
      function: 'fn',
      args: [],
    });
    expect(result.target).toBe('node1');
  });

  it('should throw for unknown schema type', () => {
    expect(() => createValidationMiddleware('invalid')).toThrow('Unknown schema type');
  });

  it('should throw on invalid data', () => {
    const validate = createValidationMiddleware('rpcCall');
    expect(() => validate({ target: '' })).toThrow('Validation failed');
  });
});

describe('withValidation', () => {
  it('should wrap function with validation', async () => {
    const fn = async call => {
      return `Called ${call.function}`;
    };

    const validated = withValidation(fn, 'rpcCall');
    const result = await validated({
      target: 'node1',
      module: 'test',
      function: 'myFunc',
      args: [],
    });

    expect(result).toBe('Called myFunc');
  });

  it('should throw before calling function for invalid data', async () => {
    const fn = async () => 'should not be called';
    const validated = withValidation(fn, 'rpcCall');

    await expect(validated({ target: '' })).rejects.toThrow('Validation failed');
  });

  it('should pass additional arguments', async () => {
    const fn = async (call, options) => {
      return { call: call.function, options };
    };

    const validated = withValidation(fn, 'rpcCall');
    const result = await validated(
      { target: 'n1', module: 'm', function: 'f', args: [] },
      { timeout: 1000 }
    );

    expect(result.options).toEqual({ timeout: 1000 });
  });
});

// ============================================================================
// Error Message Quality Tests
// ============================================================================

describe('error message quality', () => {
  it('should provide descriptive error for missing field', () => {
    const result = validateRPCCall({ target: 'node1' });
    expect(result.error).toMatch(/module/i);
    // Zod v4 uses "expected string, received undefined" instead of "required"
    expect(result.error).toMatch(/expected|required/i);
  });

  it('should provide path in error message', () => {
    const result = validateTriplePattern({ s: 123 });
    expect(result.error).toContain('[s]');
  });

  it('should include multiple errors when present', () => {
    const result = validateRPCCall({});
    // Should mention multiple missing fields
    expect(result.error).toContain('target');
    expect(result.error).toContain('module');
    expect(result.error).toContain('function');
  });
});

// ============================================================================
// Schema Export Tests
// ============================================================================

describe('messageSchemas export', () => {
  it('should export all schema types', () => {
    expect(messageSchemas.triplePattern).toBeDefined();
    expect(messageSchemas.rpcCall).toBeDefined();
    expect(messageSchemas.rpcResult).toBeDefined();
    expect(messageSchemas.sparqlQuery).toBeDefined();
    expect(messageSchemas.batchOperation).toBeDefined();
    expect(messageSchemas.healthCheck).toBeDefined();
  });

  it('should be frozen (immutable)', () => {
    expect(Object.isFrozen(messageSchemas)).toBe(true);
  });
});
