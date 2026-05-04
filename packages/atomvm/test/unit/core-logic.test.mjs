/**
 * @fileoverview Core Logic Unit Tests (No WASM Required)
 * @module test/unit/core-logic
 *
 * @description
 * Pure unit tests for core validation and caching logic.
 * These tests do NOT require:
 * - AtomVM WASM
 * - Oxigraph WASM
 * - Browser environment
 * - Service workers
 *
 * Tests included:
 * 1. Message Validator (Zod-based validation)
 * 2. RDF Validator (IRI/literal/triple validation)
 * 3. Query Cache (LRU cache with TTL)
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

// ============================================================================
// Message Validator Tests (Pure Zod Validation)
// ============================================================================

import {
  validateTriplePattern,
  validateRPCCall,
  validateRPCResult,
  validateSPARQLQuery,
  validateBatchOperation,
  validateHealthCheck,
  validateMessage,
  parseTriplePattern,
  parseRPCCall,
  createValidationMiddleware,
  messageSchemas,
} from '../../src/message-validator.mjs';

describe('Message Validator (No WASM)', () => {
  describe('validateTriplePattern', () => {
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

    it('should reject non-string subject', () => {
      const result = validateTriplePattern({ s: 123 });
      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });

    it('should reject extra fields (strict mode)', () => {
      const result = validateTriplePattern({ s: 'test', extraField: 'not allowed' });
      expect(result.success).toBe(false);
    });
  });

  describe('validateRPCCall', () => {
    it('should accept valid RPC call', () => {
      const result = validateRPCCall({
        target: 'node-1',
        module: 'graph',
        function: 'query',
        args: ['SELECT * WHERE { ?s ?p ?o }'],
      });
      expect(result.success).toBe(true);
      expect(result.data.target).toBe('node-1');
    });

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
    });

    it('should default args to empty array', () => {
      const result = validateRPCCall({
        target: 'node-1',
        module: 'system',
        function: 'ping',
      });
      expect(result.success).toBe(true);
      expect(result.data.args).toEqual([]);
    });
  });

  describe('validateRPCResult', () => {
    it('should accept successful result', () => {
      const result = validateRPCResult({
        ok: true,
        result: { count: 42 },
      });
      expect(result.success).toBe(true);
    });

    it('should accept error result with message', () => {
      const result = validateRPCResult({
        ok: false,
        result: null,
        error: 'Connection timeout',
      });
      expect(result.success).toBe(true);
    });

    it('should reject error result without error message', () => {
      const result = validateRPCResult({
        ok: false,
        result: null,
      });
      expect(result.success).toBe(false);
      expect(result.error).toContain('error');
    });
  });

  describe('validateSPARQLQuery', () => {
    it('should accept query without params', () => {
      const result = validateSPARQLQuery({
        query: 'SELECT * WHERE { ?s ?p ?o }',
      });
      expect(result.success).toBe(true);
    });

    it('should reject empty query string', () => {
      const result = validateSPARQLQuery({
        query: '',
      });
      expect(result.success).toBe(false);
    });
  });

  describe('validateBatchOperation', () => {
    it('should accept insert operation', () => {
      const result = validateBatchOperation({
        operation: 'insert',
        triples: [{ s: 'http://example.org/a', p: 'http://example.org/b', o: 'c' }],
      });
      expect(result.success).toBe(true);
    });

    it('should reject empty triples array', () => {
      const result = validateBatchOperation({
        operation: 'insert',
        triples: [],
      });
      expect(result.success).toBe(false);
    });
  });

  describe('validateHealthCheck', () => {
    it('should accept valid health check', () => {
      const result = validateHealthCheck({
        nodeId: 'node-1',
        timestamp: Date.now(),
        status: 'healthy',
      });
      expect(result.success).toBe(true);
    });

    it('should reject invalid status', () => {
      const result = validateHealthCheck({
        nodeId: 'node-1',
        timestamp: Date.now(),
        status: 'unknown',
      });
      expect(result.success).toBe(false);
    });
  });

  describe('validateMessage (generic)', () => {
    it('should validate known schema types', () => {
      const result = validateMessage('triplePattern', { s: 'test' });
      expect(result.success).toBe(true);
    });

    it('should return error for unknown schema type', () => {
      const result = validateMessage('unknownType', { data: 'test' });
      expect(result.success).toBe(false);
      expect(result.error).toContain('Unknown schema type');
    });
  });

  describe('parseTriplePattern (strict)', () => {
    it('should return data for valid input', () => {
      const data = parseTriplePattern({ s: 'test' });
      expect(data.s).toBe('test');
    });

    it('should throw for invalid input', () => {
      expect(() => parseTriplePattern({ s: 123 })).toThrow('Invalid triple pattern');
    });
  });

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
  });

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
});

// ============================================================================
// RDF Validator Tests (Pure Validation Logic)
// ============================================================================

import {
  RDFValidator,
  NAMESPACES,
  createPreInsertionValidator,
} from '../../src/rdf-validator.mjs';

describe('RDF Validator (No WASM)', () => {
  let validator;

  beforeEach(() => {
    validator = new RDFValidator();
  });

  describe('constructor', () => {
    it('should create validator instance', () => {
      expect(validator).toBeDefined();
      expect(validator.shapes).toBeDefined();
      expect(validator.errors).toEqual([]);
    });

    it('should register built-in shapes', () => {
      const shapes = validator.getRegisteredShapes();
      expect(shapes).toContain('foaf:Person');
      expect(shapes).toContain('schema:Person');
    });
  });

  describe('validateIRI', () => {
    it('should accept valid HTTP IRI', () => {
      const result = validator.validateIRI('http://example.org/resource');
      expect(result.valid).toBe(true);
    });

    it('should accept valid HTTPS IRI', () => {
      const result = validator.validateIRI('https://schema.org/Person');
      expect(result.valid).toBe(true);
    });

    it('should reject empty string', () => {
      const result = validator.validateIRI('');
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('IRI_INVALID');
    });

    it('should reject null', () => {
      const result = validator.validateIRI(null);
      expect(result.valid).toBe(false);
    });
  });

  describe('validateLiteral', () => {
    it('should accept valid xsd:string', () => {
      const result = validator.validateLiteral('Hello World', 'xsd:string');
      expect(result.valid).toBe(true);
    });

    it('should accept valid xsd:integer', () => {
      const result = validator.validateLiteral('42', 'xsd:integer');
      expect(result.valid).toBe(true);
    });

    it('should reject non-integer for xsd:integer', () => {
      const result = validator.validateLiteral('3.14', 'xsd:integer');
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('DATATYPE_MISMATCH');
    });

    it('should accept valid xsd:boolean', () => {
      const result = validator.validateLiteral('true', 'xsd:boolean');
      expect(result.valid).toBe(true);
    });
  });

  describe('validateTriple', () => {
    it('should accept valid triple', () => {
      const result = validator.validateTriple({
        subject: 'http://example.org/s',
        predicate: 'http://example.org/p',
        value: 'object value',
      });
      expect(result.valid).toBe(true);
      expect(result.errors).toHaveLength(0);
    });

    it('should accept triple with blank node subject', () => {
      const result = validator.validateTriple({
        subject: '_:b1',
        predicate: 'http://example.org/p',
        value: 'object',
      });
      expect(result.valid).toBe(true);
    });

    it('should reject triple without subject', () => {
      const result = validator.validateTriple({
        predicate: 'http://example.org/p',
        value: 'object',
      });
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'SUBJECT_MISSING')).toBe(true);
    });
  });

  describe('registerShape', () => {
    it('should register custom shape', () => {
      validator.registerShape('ex:CustomShape', [
        { property: 'ex:field', required: true },
      ]);
      expect(validator.getRegisteredShapes()).toContain('ex:CustomShape');
    });

    it('should throw on empty shape name', () => {
      expect(() => validator.registerShape('', [])).toThrow('Shape name must be');
    });
  });

  describe('prefix management', () => {
    it('should expand known prefixes', () => {
      expect(validator.expandPrefix('foaf:name')).toBe('http://xmlns.com/foaf/0.1/name');
      expect(validator.expandPrefix('xsd:integer')).toBe('http://www.w3.org/2001/XMLSchema#integer');
    });

    it('should return full IRIs unchanged', () => {
      const iri = 'http://example.org/resource';
      expect(validator.expandPrefix(iri)).toBe(iri);
    });

    it('should add custom prefix', () => {
      validator.addPrefix('ex', 'http://example.org/');
      expect(validator.expandPrefix('ex:resource')).toBe('http://example.org/resource');
    });
  });

  describe('NAMESPACES', () => {
    it('should export common namespace prefixes', () => {
      expect(NAMESPACES.xsd).toBe('http://www.w3.org/2001/XMLSchema#');
      expect(NAMESPACES.rdf).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
      expect(NAMESPACES.foaf).toBe('http://xmlns.com/foaf/0.1/');
    });
  });
});

// ============================================================================
// Query Cache Tests (Pure LRU Cache Logic)
// ============================================================================

import { QueryCache, createQueryCache } from '../../src/query-cache.mjs';

describe('Query Cache (No WASM)', () => {
  let cache;

  beforeEach(() => {
    cache = new QueryCache({ maxSize: 5, ttl: 60000 });
  });

  describe('constructor', () => {
    it('should create cache with default options', () => {
      const defaultCache = new QueryCache();
      expect(defaultCache.maxSize).toBe(100);
      expect(defaultCache.ttl).toBe(60000);
    });

    it('should throw on invalid maxSize', () => {
      expect(() => new QueryCache({ maxSize: 0 })).toThrow('maxSize must be a positive integer');
      expect(() => new QueryCache({ maxSize: -1 })).toThrow('maxSize must be a positive integer');
    });

    it('should throw on invalid ttl', () => {
      expect(() => new QueryCache({ ttl: -1 })).toThrow('ttl must be a non-negative number');
    });
  });

  describe('cache hit', () => {
    it('should return cached result on repeated query', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const result = { rows: [{ s: 'http://example.org/alice' }] };

      cache.set(query, {}, result);
      const cached = cache.get(query, {});

      expect(cached).toEqual(result);
      expect(cache.stats().hits).toBe(1);
    });

    it('should return cached result with same bindings', () => {
      const query = 'SELECT ?name WHERE { ?s foaf:name ?name }';
      const bindings = { s: 'http://example.org/alice' };
      const result = { rows: [{ name: 'Alice' }] };

      cache.set(query, bindings, result);
      const cached = cache.get(query, bindings);

      expect(cached).toEqual(result);
    });
  });

  describe('cache miss', () => {
    it('should return undefined on first query', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const result = cache.get(query, {});

      expect(result).toBeUndefined();
      expect(cache.stats().misses).toBe(1);
    });

    it('should throw on empty query', () => {
      expect(() => cache.get('')).toThrow('query must be a non-empty string');
      expect(() => cache.set('', {}, {})).toThrow('query must be a non-empty string');
    });
  });

  describe('LRU eviction', () => {
    it('should evict least recently used when maxSize exceeded', () => {
      // Fill cache to capacity
      for (let i = 0; i < 5; i++) {
        cache.set(`SELECT * WHERE { ?s ?p ${i} }`, {}, { result: i });
      }

      expect(cache.size).toBe(5);

      // Add one more - should evict first
      cache.set('SELECT * WHERE { ?s ?p 5 }', {}, { result: 5 });

      expect(cache.size).toBe(5);
      // First entry should be evicted
      expect(cache.get('SELECT * WHERE { ?s ?p 0 }', {})).toBeUndefined();
      // Last entry should exist
      expect(cache.get('SELECT * WHERE { ?s ?p 5 }', {})).toEqual({ result: 5 });
    });

    it('should track eviction count', () => {
      // Fill cache
      for (let i = 0; i < 5; i++) {
        cache.set(`SELECT * WHERE { ?s ?p ${i} }`, {}, { result: i });
      }

      // Add 3 more entries - should cause 3 evictions
      for (let i = 5; i < 8; i++) {
        cache.set(`SELECT * WHERE { ?s ?p ${i} }`, {}, { result: i });
      }

      expect(cache.stats().evictions).toBe(3);
    });
  });

  describe('TTL expiration', () => {
    it('should return undefined for expired entry', async () => {
      const shortTTLCache = new QueryCache({ maxSize: 10, ttl: 50 });
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const result = { rows: [] };

      shortTTLCache.set(query, {}, result);

      // Wait for expiration
      await new Promise(resolve => setTimeout(resolve, 100));

      const cached = shortTTLCache.get(query, {});
      expect(cached).toBeUndefined();
      expect(shortTTLCache.stats().expirations).toBe(1);
    });

    it('should return result before TTL expires', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const result = { rows: [] };

      cache.set(query, {}, result);

      // Immediate access - should not be expired
      const cached = cache.get(query, {});
      expect(cached).toEqual(result);
    });
  });

  describe('invalidateAll', () => {
    it('should clear entire cache', () => {
      cache.set('query1', {}, { result: 1 });
      cache.set('query2', {}, { result: 2 });
      cache.set('query3', {}, { result: 3 });

      expect(cache.size).toBe(3);

      const cleared = cache.invalidateAll();

      expect(cleared).toBe(3);
      expect(cache.size).toBe(0);
    });
  });

  describe('stats', () => {
    it('should track hits and misses', () => {
      cache.set('query1', {}, { result: 1 });

      // 2 hits
      cache.get('query1', {});
      cache.get('query1', {});

      // 3 misses
      cache.get('query2', {});
      cache.get('query3', {});
      cache.get('query4', {});

      const stats = cache.stats();
      expect(stats.hits).toBe(2);
      expect(stats.misses).toBe(3);
      expect(stats.hitRate).toBeCloseTo(0.4, 2); // 2/5 = 0.4
    });

    it('should report correct size', () => {
      expect(cache.stats().size).toBe(0);

      cache.set('query1', {}, { result: 1 });
      expect(cache.stats().size).toBe(1);

      cache.set('query2', {}, { result: 2 });
      expect(cache.stats().size).toBe(2);
    });
  });

  describe('has method', () => {
    it('should return true for existing entry', () => {
      cache.set('query1', {}, { result: 1 });
      expect(cache.has('query1', {})).toBe(true);
    });

    it('should return false for non-existing entry', () => {
      expect(cache.has('query1', {})).toBe(false);
    });

    it('should not affect hit/miss stats', () => {
      cache.set('query1', {}, { result: 1 });

      cache.has('query1', {});
      cache.has('query2', {});

      const stats = cache.stats();
      expect(stats.hits).toBe(0);
      expect(stats.misses).toBe(0);
    });
  });

  describe('createQueryCache factory', () => {
    it('should create cache with createQueryCache factory', () => {
      const factoryCache = createQueryCache({ maxSize: 10 });
      expect(factoryCache).toBeInstanceOf(QueryCache);
      expect(factoryCache.maxSize).toBe(10);
    });
  });
});
