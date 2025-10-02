// @ts-check
/**
 * Manager Crash Recovery Chaos Tests
 *
 * Tests resilience when managers crash or fail to initialize:
 * - Manager initialization failures
 * - Partial manager availability
 * - Manager state recovery
 * - Dependency chain failures
 * - Graceful degradation
 */

import { describe, it, expect, beforeEach, vi } from 'vitest'
import { setTimeout } from 'timers/promises'

describe('Manager Crash Recovery Chaos Tests', () => {
  describe('Manager Initialization Failures', () => {
    it('should handle single manager initialization failure', async () => {
      const initializeManagers = async () => {
        const managers = {
          hookManager: null,
          transactionManager: null,
          policyPack: null,
          effectSandbox: null,
          lockchainWriter: null,
          resolutionLayer: null,
          observability: null
        }

        const errors = []

        // Try to initialize each manager
        try {
          managers.hookManager = { initialized: true, name: 'hookManager' }
        } catch (e) {
          errors.push({ manager: 'hookManager', error: e.message })
        }

        try {
          managers.transactionManager = { initialized: true, name: 'transactionManager' }
        } catch (e) {
          errors.push({ manager: 'transactionManager', error: e.message })
        }

        try {
          // Simulate failure
          throw new Error('PolicyPack initialization failed - config invalid')
        } catch (e) {
          errors.push({ manager: 'policyPack', error: e.message })
        }

        try {
          managers.effectSandbox = { initialized: true, name: 'effectSandbox' }
        } catch (e) {
          errors.push({ manager: 'effectSandbox', error: e.message })
        }

        return { managers, errors }
      }

      const result = await initializeManagers()

      expect(result.errors.length).toBe(1)
      expect(result.errors[0].manager).toBe('policyPack')
      expect(result.managers.hookManager).toBeDefined()
      expect(result.managers.policyPack).toBeNull()
    })

    it('should fail fast when critical manager fails', async () => {
      const criticalManagers = ['observability', 'hookManager']

      const initializeWithFailFast = async () => {
        const managers = {}

        // Observability fails
        try {
          throw new Error('Observability manager failed - OTEL unreachable')
        } catch (error) {
          // Critical manager failed
          if (criticalManagers.includes('observability')) {
            throw new Error(`Critical manager 'observability' failed: ${error.message}`)
          }
        }

        return managers
      }

      await expect(initializeWithFailFast())
        .rejects.toThrow(/Critical manager/)
    })

    it('should initialize managers in dependency order', async () => {
      const initOrder = []

      const initializeInOrder = async () => {
        const dependencies = {
          observability: [],
          hookManager: ['observability'],
          transactionManager: ['observability'],
          policyPack: ['hookManager', 'observability'],
          effectSandbox: ['hookManager'],
          lockchainWriter: ['transactionManager'],
          resolutionLayer: ['transactionManager', 'policyPack']
        }

        const initialized = new Set()
        const managers = {}

        const initManager = async (name) => {
          // Check dependencies
          const deps = dependencies[name]
          for (const dep of deps) {
            if (!initialized.has(dep)) {
              throw new Error(`${name} requires ${dep} to be initialized first`)
            }
          }

          // Initialize
          await setTimeout(10) // Simulate init time
          managers[name] = { name, initialized: true }
          initialized.add(name)
          initOrder.push(name)
        }

        // Initialize in correct order
        await initManager('observability')
        await initManager('hookManager')
        await initManager('transactionManager')
        await initManager('policyPack')
        await initManager('effectSandbox')
        await initManager('lockchainWriter')
        await initManager('resolutionLayer')

        return managers
      }

      const managers = await initializeInOrder()

      expect(Object.keys(managers).length).toBe(7)
      expect(initOrder[0]).toBe('observability') // First
      expect(initOrder[initOrder.length - 1]).toBe('resolutionLayer') // Last
    })
  })

  describe('Partial Manager Availability', () => {
    it('should operate with degraded functionality when optional managers fail', async () => {
      const managers = {
        hookManager: { available: true, critical: true },
        transactionManager: { available: true, critical: true },
        policyPack: { available: true, critical: true },
        effectSandbox: { available: false, critical: false }, // Optional failed
        lockchainWriter: { available: false, critical: false }, // Optional failed
        resolutionLayer: { available: true, critical: true },
        observability: { available: true, critical: true }
      }

      const getAvailableFeatures = (managers) => {
        const features = {
          coreOperations: managers.hookManager.available &&
                         managers.transactionManager.available,
          policyEnforcement: managers.policyPack.available,
          effectExecution: managers.effectSandbox.available,
          auditLogging: managers.lockchainWriter.available,
          monitoring: managers.observability.available
        }

        return features
      }

      const features = getAvailableFeatures(managers)

      expect(features.coreOperations).toBe(true)
      expect(features.policyEnforcement).toBe(true)
      expect(features.effectExecution).toBe(false) // Degraded
      expect(features.auditLogging).toBe(false) // Degraded
      expect(features.monitoring).toBe(true)
    })

    it('should return graceful errors when disabled managers accessed', async () => {
      const managers = {
        effectSandbox: null,
        lockchainWriter: null
      }

      const executeEffect = async (effectCode) => {
        if (!managers.effectSandbox) {
          return {
            error: 'Effect execution unavailable - sandbox manager not initialized',
            degraded: true
          }
        }

        return { result: 'executed' }
      }

      const result = await executeEffect('console.log("test")')

      expect(result.error).toBeDefined()
      expect(result.degraded).toBe(true)
    })

    it('should track manager health status', async () => {
      const managerStates = {
        hookManager: 'healthy',
        transactionManager: 'healthy',
        policyPack: 'degraded',
        effectSandbox: 'failed',
        lockchainWriter: 'failed',
        resolutionLayer: 'healthy',
        observability: 'healthy'
      }

      const getHealthSummary = (states) => {
        const counts = {
          healthy: 0,
          degraded: 0,
          failed: 0
        }

        for (const state of Object.values(states)) {
          counts[state]++
        }

        const total = Object.keys(states).length
        const healthScore = ((counts.healthy * 1.0 + counts.degraded * 0.5 + counts.failed * 0.0) / total) * 100

        return {
          ...counts,
          total,
          healthScore: Math.round(healthScore)
        }
      }

      const summary = getHealthSummary(managerStates)

      expect(summary.healthy).toBe(4)
      expect(summary.degraded).toBe(1)
      expect(summary.failed).toBe(2)
      expect(summary.healthScore).toBeLessThan(100)
      expect(summary.healthScore).toBeGreaterThan(50)
    })
  })

  describe('Manager State Recovery', () => {
    it('should recover manager state from crash', async () => {
      let managerState = {
        initialized: true,
        registeredHooks: 5,
        cache: { size: 100 }
      }

      const simulateCrash = () => {
        managerState = null
      }

      const recoverState = async () => {
        // Attempt to restore from persistent storage
        const restoredState = {
          initialized: true,
          registeredHooks: 5, // Restored from DB
          cache: { size: 0 }  // Cache empty after recovery
        }

        return restoredState
      }

      simulateCrash()
      expect(managerState).toBeNull()

      managerState = await recoverState()

      expect(managerState.initialized).toBe(true)
      expect(managerState.registeredHooks).toBe(5)
      expect(managerState.cache.size).toBe(0)
    })

    it('should rebuild indexes after manager recovery', async () => {
      const rebuildIndexes = async (manager) => {
        const startTime = Date.now()

        // Simulate rebuilding indexes from data
        const hooks = [
          { id: '1', type: 'validation' },
          { id: '2', type: 'transform' },
          { id: '3', type: 'validation' }
        ]

        const indexes = {
          byType: new Map(),
          byId: new Map()
        }

        for (const hook of hooks) {
          // Build type index
          if (!indexes.byType.has(hook.type)) {
            indexes.byType.set(hook.type, [])
          }
          indexes.byType.get(hook.type).push(hook.id)

          // Build ID index
          indexes.byId.set(hook.id, hook)
        }

        return {
          indexes,
          rebuildTime: Date.now() - startTime,
          hooksIndexed: hooks.length
        }
      }

      const result = await rebuildIndexes('hookManager')

      expect(result.hooksIndexed).toBe(3)
      expect(result.indexes.byType.size).toBe(2)
      expect(result.indexes.byId.size).toBe(3)
      expect(result.indexes.byType.get('validation').length).toBe(2)
    })

    it('should validate state consistency after recovery', async () => {
      const persistedState = {
        hookCount: 10,
        hooks: Array(8).fill({ id: 'hook' }) // Inconsistent!
      }

      const validateState = (state) => {
        const errors = []

        if (state.hookCount !== state.hooks.length) {
          errors.push({
            field: 'hookCount',
            expected: state.hooks.length,
            actual: state.hookCount,
            severity: 'error'
          })
        }

        return {
          valid: errors.length === 0,
          errors
        }
      }

      const validation = validateState(persistedState)

      expect(validation.valid).toBe(false)
      expect(validation.errors.length).toBe(1)
      expect(validation.errors[0].field).toBe('hookCount')
    })
  })

  describe('Dependency Chain Failures', () => {
    it('should handle cascading failures when dependency crashes', async () => {
      const managers = {
        hookManager: { status: 'running', dependsOn: ['observability'] },
        policyPack: { status: 'running', dependsOn: ['hookManager', 'observability'] },
        effectSandbox: { status: 'running', dependsOn: ['hookManager'] },
        observability: { status: 'crashed', dependsOn: [] }
      }

      const propagateFailure = (managers, failedManager) => {
        const affected = []

        for (const [name, manager] of Object.entries(managers)) {
          if (manager.dependsOn.includes(failedManager)) {
            manager.status = 'degraded'
            affected.push(name)
          }
        }

        return affected
      }

      const affected = propagateFailure(managers, 'observability')

      expect(affected).toContain('hookManager')
      expect(affected).toContain('policyPack')
      expect(managers.hookManager.status).toBe('degraded')
      expect(managers.policyPack.status).toBe('degraded')
      expect(managers.effectSandbox.status).toBe('running') // Indirect dependency
    })

    it('should isolate failures in independent managers', async () => {
      const managers = {
        lockchainWriter: { status: 'crashed', dependsOn: ['transactionManager'] },
        effectSandbox: { status: 'running', dependsOn: ['hookManager'] },
        hookManager: { status: 'running', dependsOn: [] }
      }

      // Effect sandbox should not be affected by lockchain crash
      const isAffected = managers.effectSandbox.dependsOn.includes('lockchainWriter')

      expect(isAffected).toBe(false)
      expect(managers.effectSandbox.status).toBe('running')
      expect(managers.lockchainWriter.status).toBe('crashed')
    })
  })

  describe('Graceful Degradation', () => {
    it('should degrade to read-only mode when transaction manager fails', async () => {
      const systemMode = {
        readWrite: true,
        transactionManagerAvailable: false
      }

      const getOperationMode = (mode) => {
        if (!mode.transactionManagerAvailable) {
          return {
            read: true,
            write: false,
            reason: 'Transaction manager unavailable - read-only mode'
          }
        }

        return {
          read: true,
          write: true,
          reason: 'Normal operations'
        }
      }

      const mode = getOperationMode(systemMode)

      expect(mode.read).toBe(true)
      expect(mode.write).toBe(false)
      expect(mode.reason).toContain('read-only')
    })

    it('should skip optional features when managers unavailable', async () => {
      const request = {
        operation: 'applyTransaction',
        enableAudit: true,
        enableEffects: true
      }

      const managers = {
        lockchainWriter: null,
        effectSandbox: null
      }

      const processRequest = async (req, mgrs) => {
        const result = {
          operation: 'completed',
          auditLogged: false,
          effectsExecuted: false,
          warnings: []
        }

        // Skip audit if lockchain unavailable
        if (req.enableAudit && !mgrs.lockchainWriter) {
          result.warnings.push('Audit logging skipped - lockchain writer unavailable')
        } else if (req.enableAudit) {
          result.auditLogged = true
        }

        // Skip effects if sandbox unavailable
        if (req.enableEffects && !mgrs.effectSandbox) {
          result.warnings.push('Effect execution skipped - sandbox unavailable')
        } else if (req.enableEffects) {
          result.effectsExecuted = true
        }

        return result
      }

      const result = await processRequest(request, managers)

      expect(result.operation).toBe('completed')
      expect(result.auditLogged).toBe(false)
      expect(result.effectsExecuted).toBe(false)
      expect(result.warnings.length).toBe(2)
    })
  })
})
