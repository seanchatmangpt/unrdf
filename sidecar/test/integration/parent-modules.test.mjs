/**
 * @file Parent Module Integration Tests (TDD London School)
 * @description Validates that sidecar can import and use parent KGC modules
 *
 * 80/20 VALIDATION STRATEGY:
 * - 20% of tests validate 80% of integration functionality
 * - Focus on: module loading, API contracts, basic operations
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest'

// Import parent modules using the same paths as the sidecar
import { KnowledgeHookManager } from '../../../src/knowledge-engine/knowledge-hook-manager.mjs'
import { TransactionManager } from '../../../src/knowledge-engine/transaction.mjs'
import { PolicyPack } from '../../../src/knowledge-engine/policy-pack.mjs'
import { EffectSandbox } from '../../../src/knowledge-engine/effect-sandbox.mjs'
import { LockchainWriter } from '../../../src/knowledge-engine/lockchain-writer.mjs'
import { ResolutionLayer } from '../../../src/knowledge-engine/resolution-layer.mjs'
import { ObservabilityManager } from '../../../src/knowledge-engine/observability.mjs'

describe('Parent Module Integration (TDD London School)', () => {
  describe('Module Import Validation', () => {
    it('should import KnowledgeHookManager class', () => {
      expect(KnowledgeHookManager).toBeDefined()
      expect(typeof KnowledgeHookManager).toBe('function')
      expect(KnowledgeHookManager.name).toBe('KnowledgeHookManager')
    })

    it('should import TransactionManager class', () => {
      expect(TransactionManager).toBeDefined()
      expect(typeof TransactionManager).toBe('function')
      expect(TransactionManager.name).toBe('TransactionManager')
    })

    it('should import PolicyPack class', () => {
      expect(PolicyPack).toBeDefined()
      expect(typeof PolicyPack).toBe('function')
      expect(PolicyPack.name).toBe('PolicyPack')
    })

    it('should import EffectSandbox class', () => {
      expect(EffectSandbox).toBeDefined()
      expect(typeof EffectSandbox).toBe('function')
      expect(EffectSandbox.name).toBe('EffectSandbox')
    })

    it('should import LockchainWriter class', () => {
      expect(LockchainWriter).toBeDefined()
      expect(typeof LockchainWriter).toBe('function')
      expect(LockchainWriter.name).toBe('LockchainWriter')
    })

    it('should import ResolutionLayer class', () => {
      expect(ResolutionLayer).toBeDefined()
      expect(typeof ResolutionLayer).toBe('function')
      expect(ResolutionLayer.name).toBe('ResolutionLayer')
    })

    it('should import ObservabilityManager class', () => {
      expect(ObservabilityManager).toBeDefined()
      expect(typeof ObservabilityManager).toBe('function')
      expect(ObservabilityManager.name).toBe('ObservabilityManager')
    })
  })

  describe('Manager Initialization (80% of Integration Value)', () => {
    let hookManager
    let transactionManager
    let policyPack
    let effectSandbox
    let lockchainWriter
    let resolutionLayer
    let observability

    beforeAll(() => {
      // Initialize managers with minimal config
      observability = new ObservabilityManager({
        enableTelemetry: false,
        serviceName: 'test-sidecar'
      })

      hookManager = new KnowledgeHookManager({
        apiKey: 'test-api-key',
        encryptionKey: 'test-encryption-key'
      })

      transactionManager = new TransactionManager({
        databaseUrl: 'memory://test'
      })

      policyPack = new PolicyPack({
        id: '00000000-0000-0000-0000-000000000000',
        meta: {
          id: 'test-policy',
          name: 'test-policy-pack',
          version: '1.0.0',
          description: 'Test policy pack'
        },
        config: {
          apiKey: 'test-api-key',
          encryptionKey: 'test-encryption-key'
        },
        hooks: []
      })

      effectSandbox = new EffectSandbox({
        timeout: 5000,
        memoryLimit: 50 * 1024 * 1024 // 50MB
      })

      resolutionLayer = new ResolutionLayer()

      // LockchainWriter requires git repo, skip in test
      lockchainWriter = null
    })

    it('should initialize ObservabilityManager without errors', () => {
      expect(observability).toBeDefined()
      expect(observability).toBeInstanceOf(ObservabilityManager)
    })

    it('should initialize KnowledgeHookManager without errors', () => {
      expect(hookManager).toBeDefined()
      expect(hookManager).toBeInstanceOf(KnowledgeHookManager)
    })

    it('should initialize TransactionManager without errors', () => {
      expect(transactionManager).toBeDefined()
      expect(transactionManager).toBeInstanceOf(TransactionManager)
    })

    it('should initialize PolicyPack without errors', () => {
      expect(policyPack).toBeDefined()
      expect(policyPack).toBeInstanceOf(PolicyPack)
    })

    it('should initialize EffectSandbox without errors', () => {
      expect(effectSandbox).toBeDefined()
      expect(effectSandbox).toBeInstanceOf(EffectSandbox)
    })

    it('should initialize ResolutionLayer without errors', () => {
      expect(resolutionLayer).toBeDefined()
      expect(resolutionLayer).toBeInstanceOf(ResolutionLayer)
    })

    afterAll(() => {
      // Cleanup
      if (observability && typeof observability.shutdown === 'function') {
        observability.shutdown()
      }
    })
  })

  describe('API Contract Validation (Critical for Integration)', () => {
    let hookManager
    let transactionManager
    let policyPack
    let effectSandbox
    let resolutionLayer
    let observability

    beforeAll(() => {
      observability = new ObservabilityManager({ enableTelemetry: false })
      hookManager = new KnowledgeHookManager({
        apiKey: 'test-key',
        encryptionKey: 'test-key'
      })
      transactionManager = new TransactionManager({ databaseUrl: 'memory://test' })
      policyPack = new PolicyPack({
        id: '00000000-0000-0000-0000-000000000000',
        meta: { id: 'test', name: 'test', version: '1.0.0' },
        config: {},
        hooks: []
      })
      effectSandbox = new EffectSandbox({ timeout: 5000 })
      resolutionLayer = new ResolutionLayer()
    })

    describe('KnowledgeHookManager API', () => {
      it('should have registerHook method', () => {
        expect(typeof hookManager.registerHook).toBe('function')
      })

      it('should have evaluateHooks method', () => {
        expect(typeof hookManager.evaluateHooks).toBe('function')
      })

      it('should have removeHook method', () => {
        expect(typeof hookManager.removeHook).toBe('function')
      })
    })

    describe('TransactionManager API', () => {
      it('should have applyTransaction method', () => {
        expect(typeof transactionManager.applyTransaction).toBe('function')
      })

      it('should have rollback method', () => {
        expect(typeof transactionManager.rollback).toBe('function')
      })
    })

    describe('PolicyPack API', () => {
      it('should have validate method', () => {
        expect(typeof policyPack.validate).toBe('function')
      })

      it('should have registerPolicy method', () => {
        expect(typeof policyPack.registerPolicy).toBe('function')
      })
    })

    describe('EffectSandbox API', () => {
      it('should have execute method', () => {
        expect(typeof effectSandbox.execute).toBe('function')
      })
    })

    describe('ResolutionLayer API', () => {
      it('should have resolve method', () => {
        expect(typeof resolutionLayer.resolve).toBe('function')
      })
    })

    describe('ObservabilityManager API', () => {
      it('should have recordMetric method', () => {
        expect(typeof observability.recordMetric).toBe('function')
      })

      it('should have startSpan method', () => {
        expect(typeof observability.startSpan).toBe('function')
      })

      it('should have recordError method', () => {
        expect(typeof observability.recordError).toBe('function')
      })
    })

    afterAll(() => {
      if (observability && typeof observability.shutdown === 'function') {
        observability.shutdown()
      }
    })
  })

  describe('Basic Operations (Smoke Tests - 20% That Validate 80%)', () => {
    let hookManager
    let policyPack
    let effectSandbox
    let observability

    beforeAll(() => {
      observability = new ObservabilityManager({ enableTelemetry: false })
      hookManager = new KnowledgeHookManager({
        apiKey: 'test-key',
        encryptionKey: 'test-key'
      })
      policyPack = new PolicyPack({
        id: '00000000-0000-0000-0000-000000000000',
        meta: {
          id: 'test',
          name: 'test-policy',
          version: '1.0.0',
          description: 'Test'
        },
        config: {},
        hooks: []
      })
      effectSandbox = new EffectSandbox({ timeout: 5000 })
    })

    it('should register a simple hook', async () => {
      const hook = {
        id: 'test-hook-001',
        predicate: {
          type: 'ASK',
          query: 'ASK { ?s ?p ?o }'
        },
        action: {
          type: 'SPARQL_UPDATE',
          value: 'INSERT DATA { <test:s> <test:p> <test:o> }'
        }
      }

      const result = await hookManager.registerHook(hook)
      expect(result).toBeDefined()
    })

    it('should evaluate hooks', async () => {
      const result = await hookManager.evaluateHooks({
        type: 'transaction',
        data: { triples: [] }
      })
      expect(result).toBeDefined()
    })

    it('should record observability metrics', () => {
      const result = observability.recordMetric('test.metric', 1)
      expect(result).toBeDefined()
    })

    it('should execute effects in sandbox', async () => {
      const effect = {
        code: 'export default function() { return { success: true } }',
        timeout: 1000
      }

      const result = await effectSandbox.execute(effect)
      expect(result).toBeDefined()
    })

    afterAll(() => {
      if (observability && typeof observability.shutdown === 'function') {
        observability.shutdown()
      }
    })
  })

  describe('Sidecar Plugin Integration Path', () => {
    it('should successfully import from plugin path', async () => {
      // This validates the exact import paths used in server/plugins/00.managers.mjs
      const imports = {
        KnowledgeHookManager: await import('../../../src/knowledge-engine/knowledge-hook-manager.mjs'),
        TransactionManager: await import('../../../src/knowledge-engine/transaction.mjs'),
        PolicyPack: await import('../../../src/knowledge-engine/policy-pack.mjs'),
        EffectSandbox: await import('../../../src/knowledge-engine/effect-sandbox.mjs'),
        LockchainWriter: await import('../../../src/knowledge-engine/lockchain-writer.mjs'),
        ResolutionLayer: await import('../../../src/knowledge-engine/resolution-layer.mjs'),
        ObservabilityManager: await import('../../../src/knowledge-engine/observability.mjs')
      }

      expect(imports.KnowledgeHookManager.KnowledgeHookManager).toBeDefined()
      expect(imports.TransactionManager.TransactionManager).toBeDefined()
      expect(imports.PolicyPack.PolicyPack).toBeDefined()
      expect(imports.EffectSandbox.EffectSandbox).toBeDefined()
      expect(imports.LockchainWriter.LockchainWriter).toBeDefined()
      expect(imports.ResolutionLayer.ResolutionLayer).toBeDefined()
      expect(imports.ObservabilityManager.ObservabilityManager).toBeDefined()
    })
  })
})
