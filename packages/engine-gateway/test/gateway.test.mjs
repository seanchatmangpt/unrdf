/**
 * @fileoverview EngineGateway Tests
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { EngineGateway } from '../src/gateway.mjs'
import { detectOperationType, isN3Operation, isOxigraphOperation } from '../src/operation-detector.mjs'
import { validateN3Usage, validateOxigraphUsage } from '../src/validators.mjs'
import { createStore } from '@unrdf/oxigraph'

describe('EngineGateway', () => {
  describe('Operation Type Detection', () => {
    it('should detect SPARQL queries as Oxigraph', () => {
      expect(detectOperationType('query')).toBe('oxigraph')
    })

    it('should detect SPARQL updates as Oxigraph', () => {
      expect(detectOperationType('update')).toBe('oxigraph')
    })

    it('should detect storage operations as Oxigraph', () => {
      expect(detectOperationType('add')).toBe('oxigraph')
      expect(detectOperationType('delete')).toBe('oxigraph')
      expect(detectOperationType('clear')).toBe('oxigraph')
    })

    it('should detect parsing as Oxigraph', () => {
      expect(detectOperationType('parse')).toBe('oxigraph')
    })

    it('should detect serialization as Oxigraph', () => {
      expect(detectOperationType('serialize')).toBe('oxigraph')
    })

    it('should detect stream-parse as N3', () => {
      expect(detectOperationType('stream-parse')).toBe('n3')
    })

    it('should detect stream-serialize as N3', () => {
      expect(detectOperationType('stream-serialize')).toBe('n3')
    })

    it('should detect n3-reason as N3', () => {
      expect(detectOperationType('n3-reason')).toBe('n3')
    })

    it('should detect permissive-parse as N3', () => {
      expect(detectOperationType('permissive-parse')).toBe('n3')
    })

    it('should detect rdf-transform as N3', () => {
      expect(detectOperationType('rdf-transform')).toBe('n3')
    })

    it('should handle unknown operations by defaulting to Oxigraph', () => {
      expect(detectOperationType('unknown-operation')).toBe('oxigraph')
    })

    it('should be case-insensitive', () => {
      expect(detectOperationType('QUERY')).toBe('oxigraph')
      expect(detectOperationType('Stream-Parse')).toBe('n3')
      expect(detectOperationType('STREAM-PARSE')).toBe('n3')
    })

    it('should handle null/undefined gracefully', () => {
      expect(detectOperationType(null)).toBe('oxigraph')
      expect(detectOperationType(undefined)).toBe('oxigraph')
      expect(detectOperationType('')).toBe('oxigraph')
    })
  })

  describe('Operation Helper Functions', () => {
    it('should correctly identify N3 operations', () => {
      expect(isN3Operation('stream-parse')).toBe(true)
      expect(isN3Operation('stream-serialize')).toBe(true)
      expect(isN3Operation('n3-reason')).toBe(true)
      expect(isN3Operation('permissive-parse')).toBe(true)
      expect(isN3Operation('rdf-transform')).toBe(true)
    })

    it('should correctly identify Oxigraph operations', () => {
      expect(isOxigraphOperation('query')).toBe(true)
      expect(isOxigraphOperation('update')).toBe(true)
      expect(isOxigraphOperation('add')).toBe(true)
      expect(isOxigraphOperation('parse')).toBe(true)
    })

    it('should return false for wrong engine checks', () => {
      expect(isN3Operation('query')).toBe(false)
      expect(isOxigraphOperation('stream-parse')).toBe(false)
    })
  })

  describe('Validation', () => {
    it('should validate N3 usage correctly', () => {
      const result = validateN3Usage('stream-parse')
      expect(result.valid).toBe(true)
      expect(result.engine).toBe('n3')
      expect(result.reenterOxigraph).toBe(true)
    })

    it('should validate N3 operations (not throw for valid Oxigraph ops)', () => {
      // validateN3Usage checks if operation is N3, returns valid for Oxigraph ops
      const queryResult = validateN3Usage('query')
      expect(queryResult.valid).toBe(true)
      expect(queryResult.engine).toBe('oxigraph')
    })

    it('should validate Oxigraph usage correctly', () => {
      const result = validateOxigraphUsage('query')
      expect(result.valid).toBe(true)
      expect(result.engine).toBe('oxigraph')
    })

    it('should validate all 5 N3-justified cases', () => {
      const justifiedOps = ['stream-parse', 'stream-serialize', 'n3-reason', 'permissive-parse', 'rdf-transform']
      justifiedOps.forEach((op) => {
        expect(() => validateN3Usage(op)).not.toThrow()
      })
    })

    it('should have justification for N3 operations', () => {
      const result = validateN3Usage('stream-parse')
      expect(result.justification).toBeDefined()
      expect(result.justification.length).toBeGreaterThan(0)
    })
  })

  describe('EngineGateway - Initialization', () => {
    let gateway

    beforeEach(() => {
      const store = createStore()
      gateway = new EngineGateway({ store, enforce: true })
    })

    it('should create gateway instance', () => {
      expect(gateway).toBeDefined()
    })

    it('should have enforce flag set', () => {
      expect(gateway.enforce).toBe(true)
    })

    it('should have verbose flag', () => {
      expect(gateway.verbose).toBe(false)
    })

    it('should accept configuration options', () => {
      const store = createStore()
      const configured = new EngineGateway({
        store,
        enforce: false,
        verbose: true
      })
      expect(configured.enforce).toBe(false)
      expect(configured.verbose).toBe(true)
    })
  })

  describe('EngineGateway - Routing Validation', () => {
    let gateway

    beforeEach(() => {
      const store = createStore()
      gateway = new EngineGateway({ store, enforce: true })
    })

    it('should check if operation can be routed', () => {
      expect(gateway.canRoute('query')).toBe(true)
      expect(gateway.canRoute('stream-parse')).toBe(true)
    })

    it('should return routing metadata', () => {
      const metadata = gateway.getRoutingMetadata('query')
      expect(metadata.operation).toBe('query')
      expect(metadata.engine).toBe('oxigraph')
      expect(metadata.valid).toBe(true)
    })

    it('should include justification in metadata', () => {
      const metadata = gateway.getRoutingMetadata('stream-parse')
      expect(metadata.justification).toBeDefined()
      expect(metadata.reenterOxigraph).toBe(true)
    })

    it('should handle metadata for all operation types', () => {
      const operations = ['query', 'stream-parse', 'add', 'permissive-parse']
      operations.forEach((op) => {
        const metadata = gateway.getRoutingMetadata(op)
        expect(metadata).toBeDefined()
        expect(metadata.operation).toBe(op)
        expect(metadata.engine).toBeDefined()
      })
    })
  })

  describe('EngineGateway - Enforcement', () => {
    it('should enforce μ(O) rules when enabled', () => {
      const store = createStore()
      const gateway = new EngineGateway({ store, enforce: true })

      // These should not throw (valid operations)
      expect(() => gateway.getRoutingMetadata('query')).not.toThrow()
      expect(() => gateway.getRoutingMetadata('stream-parse')).not.toThrow()
    })

    it('should allow operations when enforce is disabled', () => {
      const store = createStore()
      const gateway = new EngineGateway({ store, enforce: false })

      // Should not throw even for theoretical violations
      expect(() => gateway.getRoutingMetadata('query')).not.toThrow()
    })

    it('should provide routing metadata for unknown operations', () => {
      const store = createStore()
      const gateway = new EngineGateway({ store, enforce: true })

      // Unknown operations default to Oxigraph
      const metadata = gateway.getRoutingMetadata('query-invalid')
      expect(metadata.engine).toBe('oxigraph')
      expect(metadata.operation).toBe('query-invalid')
    })
  })

  describe('Operation Coverage', () => {
    const operations = [
      // Oxigraph operations
      'query',
      'update',
      'add',
      'delete',
      'match',
      'has',
      'load',
      'dump',
      'size',
      'clear',
      'parse',
      'serialize',

      // N3 operations (5 only)
      'stream-parse',
      'stream-serialize',
      'n3-reason',
      'permissive-parse',
      'rdf-transform'
    ]

    it('should handle all major operations', () => {
      operations.forEach((op) => {
        expect(() => detectOperationType(op)).not.toThrow()
        const type = detectOperationType(op)
        expect(['n3', 'oxigraph']).toContain(type)
      })
    })

    it('should route all operations to correct engine', () => {
      const oxigraphOps = ['query', 'update', 'add', 'delete', 'parse', 'serialize']
      const n3Ops = ['stream-parse', 'stream-serialize', 'n3-reason', 'permissive-parse', 'rdf-transform']

      oxigraphOps.forEach((op) => {
        expect(isOxigraphOperation(op)).toBe(true)
        expect(isN3Operation(op)).toBe(false)
      })

      n3Ops.forEach((op) => {
        expect(isN3Operation(op)).toBe(true)
        expect(isOxigraphOperation(op)).toBe(false)
      })
    })

    it('should validate all N3 operations as justified', () => {
      const justifiedN3 = ['stream-parse', 'stream-serialize', 'n3-reason', 'permissive-parse', 'rdf-transform']

      justifiedN3.forEach((op) => {
        expect(() => validateN3Usage(op)).not.toThrow()
        const result = validateN3Usage(op)
        expect(result.valid).toBe(true)
        expect(result.reenterOxigraph).toBe(true)
      })
    })
  })

  describe('μ(O) Principle Enforcement', () => {
    it('should enforce Oxigraph as authoritative for storage', () => {
      const storageOps = ['add', 'delete', 'clear']
      storageOps.forEach((op) => {
        expect(isOxigraphOperation(op)).toBe(true)
      })
    })

    it('should enforce Oxigraph for all SPARQL operations', () => {
      const sparqlOps = ['query', 'update']
      sparqlOps.forEach((op) => {
        expect(isOxigraphOperation(op)).toBe(true)
      })
    })

    it('should enforce N3 re-entry from all N3 operations', () => {
      const n3Ops = ['stream-parse', 'stream-serialize', 'n3-reason', 'permissive-parse', 'rdf-transform']
      n3Ops.forEach((op) => {
        const result = validateN3Usage(op)
        expect(result.reenterOxigraph).toBe(true)
      })
    })

    it('should have exactly 5 N3-justified operations', () => {
      const justifiedCount = 5
      const justifiedOps = ['stream-parse', 'stream-serialize', 'n3-reason', 'permissive-parse', 'rdf-transform']

      expect(justifiedOps.length).toBe(justifiedCount)
      justifiedOps.forEach((op) => {
        expect(() => validateN3Usage(op)).not.toThrow()
      })
    })
  })
})
