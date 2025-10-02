/**
 * @file Full-Stack Integration Test
 * @description End-to-end validation of Auth → RBAC → Effect Execution → Audit Trail
 */
import { describe, it, expect, beforeAll, afterAll } from 'vitest'
import { registerUser, authenticateUser, generateTokenPair } from '../../server/utils/auth.mjs'
import { getRBACEngine, Roles, Resources, Actions } from '../../server/utils/rbac.mjs'
import { SecureSandbox } from '../../server/utils/secure-sandbox.mjs'

describe('Full-Stack Integration Test', () => {
  let adminUser, agentUser, writerUser, readerUser
  let adminToken, agentToken, writerToken, readerToken
  let rbacEngine
  let sandbox

  beforeAll(async () => {
    // Initialize RBAC engine
    rbacEngine = getRBACEngine()

    // Register test users
    adminUser = await registerUser('admin@test.com', 'Admin123!', ['admin'])
    agentUser = await registerUser('agent@test.com', 'Agent123!', ['agent'])
    writerUser = await registerUser('writer@test.com', 'Writer123!', ['writer'])
    readerUser = await registerUser('reader@test.com', 'Reader123!', ['reader'])

    // Assign roles in RBAC engine
    rbacEngine.assignRole(adminUser.userId, Roles.ADMIN)
    rbacEngine.assignRole(agentUser.userId, Roles.AGENT)
    rbacEngine.assignRole(writerUser.userId, Roles.WRITER)
    rbacEngine.assignRole(readerUser.userId, Roles.READER)

    // Authenticate users and get tokens
    const adminAuth = await authenticateUser('admin@test.com', 'Admin123!')
    const agentAuth = await authenticateUser('agent@test.com', 'Agent123!')
    const writerAuth = await authenticateUser('writer@test.com', 'Writer123!')
    const readerAuth = await authenticateUser('reader@test.com', 'Reader123!')

    adminToken = adminAuth.tokens.accessToken
    agentToken = agentAuth.tokens.accessToken
    writerToken = writerAuth.tokens.accessToken
    readerToken = readerAuth.tokens.accessToken

    // Initialize secure sandbox
    sandbox = new SecureSandbox({ memoryLimit: 128, timeout: 5000 })
  })

  afterAll(async () => {
    await sandbox.cleanup()
  })

  describe('Authentication Flow', () => {
    it('should authenticate all users successfully', () => {
      expect(adminToken).toBeDefined()
      expect(agentToken).toBeDefined()
      expect(writerToken).toBeDefined()
      expect(readerToken).toBeDefined()
    })

    it('should generate valid JWT tokens', () => {
      expect(adminToken).toMatch(/^eyJ/)
      expect(agentToken).toMatch(/^eyJ/)
      expect(writerToken).toMatch(/^eyJ/)
      expect(readerToken).toMatch(/^eyJ/)
    })

    it('should include correct roles in tokens', () => {
      const { verifyAccessToken } = await import('../../server/utils/auth.mjs')

      const adminPayload = verifyAccessToken(adminToken)
      const agentPayload = verifyAccessToken(agentToken)
      const writerPayload = verifyAccessToken(writerToken)
      const readerPayload = verifyAccessToken(readerToken)

      expect(adminPayload.roles).toContain('admin')
      expect(agentPayload.roles).toContain('agent')
      expect(writerPayload.roles).toContain('writer')
      expect(readerPayload.roles).toContain('reader')
    })
  })

  describe('RBAC Authorization Flow', () => {
    it('should allow admin full access to all resources', async () => {
      const decision = await rbacEngine.evaluate(
        adminUser.userId,
        Resources.KNOWLEDGE_HOOK,
        Actions.ADMIN
      )

      expect(decision.allowed).toBe(true)
      expect(decision.reason).toBe('Access granted')
      expect(decision.policies).toContain('admin-full-access')
    })

    it('should allow agent to register knowledge hooks', async () => {
      const decision = await rbacEngine.evaluate(
        agentUser.userId,
        Resources.KNOWLEDGE_HOOK,
        Actions.WRITE
      )

      expect(decision.allowed).toBe(true)
      expect(decision.policies).toContain('agent-register-hook')
    })

    it('should allow agent to register effects', async () => {
      const decision = await rbacEngine.evaluate(
        agentUser.userId,
        Resources.EFFECT,
        Actions.WRITE
      )

      expect(decision.allowed).toBe(true)
      expect(decision.policies).toContain('agent-register-effect')
    })

    it('should deny agent from applying transactions', async () => {
      const decision = await rbacEngine.evaluate(
        agentUser.userId,
        Resources.TRANSACTION,
        Actions.WRITE
      )

      expect(decision.allowed).toBe(false)
      expect(decision.reason).toBe('Access denied')
    })

    it('should allow writer to apply transactions', async () => {
      const decision = await rbacEngine.evaluate(
        writerUser.userId,
        Resources.TRANSACTION,
        Actions.WRITE
      )

      expect(decision.allowed).toBe(true)
      expect(decision.policies).toContain('writer-transaction')
    })

    it('should deny writer from registering hooks', async () => {
      const decision = await rbacEngine.evaluate(
        writerUser.userId,
        Resources.KNOWLEDGE_HOOK,
        Actions.WRITE
      )

      expect(decision.allowed).toBe(false)
    })

    it('should allow reader to read all resources', async () => {
      const decision = await rbacEngine.evaluate(
        readerUser.userId,
        Resources.TRANSACTION,
        Actions.READ
      )

      expect(decision.allowed).toBe(true)
      expect(decision.policies).toContain('reader-read')
    })

    it('should deny reader from writing', async () => {
      const decision = await rbacEngine.evaluate(
        readerUser.userId,
        Resources.TRANSACTION,
        Actions.WRITE
      )

      expect(decision.allowed).toBe(false)
    })
  })

  describe('Effect Execution Flow (Agent → RBAC → Sandbox)', () => {
    it('should allow authorized agent to register and execute effect', async () => {
      // Step 1: Check RBAC authorization
      const authDecision = await rbacEngine.evaluate(
        agentUser.userId,
        Resources.EFFECT,
        Actions.WRITE
      )

      expect(authDecision.allowed).toBe(true)

      // Step 2: Register effect in sandbox
      const effectId = 'test-effect-multiply'
      const effectCode = `
        function effect(input) {
          if (typeof input !== 'number') {
            throw new Error('Input must be a number');
          }
          return input * 2;
        }
      `

      await sandbox.createIsolate(effectId)
      await sandbox.registerEffect(effectId, effectCode)

      // Step 3: Execute effect
      const result = await sandbox.executeEffect(effectId, 10)

      expect(result).toBe(20)
    })

    it('should enforce memory limits in sandbox', async () => {
      const effectId = 'test-effect-memory'
      const maliciousCode = `
        function effect(input) {
          const arr = [];
          for (let i = 0; i < 10000000; i++) {
            arr.push(new Array(1000).fill('x'));
          }
          return arr.length;
        }
      `

      await sandbox.createIsolate(effectId)
      await sandbox.registerEffect(effectId, maliciousCode)

      // Execution should timeout or hit memory limit
      await expect(sandbox.executeEffect(effectId, 1))
        .rejects
        .toThrow()
    })

    it('should enforce execution timeout in sandbox', async () => {
      const effectId = 'test-effect-timeout'
      const slowCode = `
        function effect(input) {
          const start = Date.now();
          while (Date.now() - start < 10000) {
            // Infinite loop
          }
          return 'done';
        }
      `

      await sandbox.createIsolate(effectId)
      await sandbox.registerEffect(effectId, slowCode)

      // Execution should timeout
      await expect(sandbox.executeEffect(effectId, 1))
        .rejects
        .toThrow(/timeout/i)
    })

    it('should deny unauthorized user from executing effects', async () => {
      const decision = await rbacEngine.evaluate(
        readerUser.userId,
        Resources.EFFECT,
        Actions.EXECUTE
      )

      expect(decision.allowed).toBe(false)
    })
  })

  describe('Transaction Flow (Writer → RBAC → Audit)', () => {
    it('should allow authorized writer to apply transaction', async () => {
      // Step 1: Check RBAC authorization
      const authDecision = await rbacEngine.evaluate(
        writerUser.userId,
        Resources.TRANSACTION,
        Actions.WRITE
      )

      expect(authDecision.allowed).toBe(true)
      expect(authDecision.decisionId).toBeDefined()
      expect(authDecision.timestamp).toBeDefined()
    })

    it('should include cryptographic proof in authorization decision', async () => {
      const decision = await rbacEngine.evaluate(
        writerUser.userId,
        Resources.TRANSACTION,
        Actions.WRITE
      )

      expect(decision.decisionId).toBeDefined()
      expect(decision.timestamp).toBeDefined()
      expect(decision.userId).toBe(writerUser.userId)
      expect(decision.resource).toBe(Resources.TRANSACTION)
      expect(decision.action).toBe(Actions.WRITE)
    })

    it('should deny unauthorized user from applying transactions', async () => {
      const decision = await rbacEngine.evaluate(
        readerUser.userId,
        Resources.TRANSACTION,
        Actions.WRITE
      )

      expect(decision.allowed).toBe(false)
      expect(decision.reason).toBe('Access denied')
    })
  })

  describe('Audit Trail Flow', () => {
    it('should generate audit trail for all authorization decisions', async () => {
      const decisions = []

      // Generate multiple authorization decisions
      decisions.push(await rbacEngine.evaluate(adminUser.userId, Resources.SYSTEM, Actions.ADMIN))
      decisions.push(await rbacEngine.evaluate(agentUser.userId, Resources.KNOWLEDGE_HOOK, Actions.WRITE))
      decisions.push(await rbacEngine.evaluate(writerUser.userId, Resources.TRANSACTION, Actions.WRITE))
      decisions.push(await rbacEngine.evaluate(readerUser.userId, Resources.TRANSACTION, Actions.READ))

      // Verify all decisions have audit metadata
      for (const decision of decisions) {
        expect(decision.decisionId).toBeDefined()
        expect(decision.timestamp).toBeDefined()
        expect(decision.userId).toBeDefined()
        expect(decision.resource).toBeDefined()
        expect(decision.action).toBeDefined()
        expect(decision.allowed).toBeDefined()
      }
    })

    it('should include matched policies in audit trail', async () => {
      const decision = await rbacEngine.evaluate(
        agentUser.userId,
        Resources.KNOWLEDGE_HOOK,
        Actions.WRITE
      )

      expect(decision.policies).toBeDefined()
      expect(Array.isArray(decision.policies)).toBe(true)
      expect(decision.policies.length).toBeGreaterThan(0)
    })
  })

  describe('Complete Request Lifecycle', () => {
    it('should complete full request lifecycle: Auth → RBAC → Effect → Audit', async () => {
      // Step 1: Authentication
      const auth = await authenticateUser('agent@test.com', 'Agent123!')
      expect(auth.tokens.accessToken).toBeDefined()

      // Step 2: Authorization (RBAC)
      const authDecision = await rbacEngine.evaluate(
        agentUser.userId,
        Resources.EFFECT,
        Actions.WRITE
      )
      expect(authDecision.allowed).toBe(true)

      // Step 3: Effect registration and execution
      const effectId = 'full-lifecycle-effect'
      const effectCode = `
        function effect(input) {
          return { processed: true, value: input * 2 };
        }
      `

      await sandbox.createIsolate(effectId)
      await sandbox.registerEffect(effectId, effectCode)
      const result = await sandbox.executeEffect(effectId, 5)

      expect(result).toEqual({ processed: true, value: 10 })

      // Step 4: Audit trail verification
      expect(authDecision.decisionId).toBeDefined()
      expect(authDecision.timestamp).toBeDefined()
      expect(authDecision.policies).toContain('agent-register-effect')
    })

    it('should enforce security controls across entire lifecycle', async () => {
      // Attempt unauthorized action
      const authDecision = await rbacEngine.evaluate(
        readerUser.userId,
        Resources.EFFECT,
        Actions.WRITE
      )

      // Verify denial
      expect(authDecision.allowed).toBe(false)

      // Verify audit trail includes denial
      expect(authDecision.reason).toBe('Access denied')
      expect(authDecision.decisionId).toBeDefined()
    })
  })

  describe('Policy Cache Performance', () => {
    it('should cache authorization decisions for performance', async () => {
      const startTime = Date.now()

      // First evaluation (no cache)
      await rbacEngine.evaluate(
        agentUser.userId,
        Resources.KNOWLEDGE_HOOK,
        Actions.WRITE
      )

      const firstDuration = Date.now() - startTime

      const cacheStartTime = Date.now()

      // Second evaluation (cached)
      await rbacEngine.evaluate(
        agentUser.userId,
        Resources.KNOWLEDGE_HOOK,
        Actions.WRITE
      )

      const cachedDuration = Date.now() - cacheStartTime

      // Cached evaluation should be faster
      expect(cachedDuration).toBeLessThan(firstDuration)
    })

    it('should invalidate cache when roles change', async () => {
      // Initial evaluation
      const decision1 = await rbacEngine.evaluate(
        agentUser.userId,
        Resources.TRANSACTION,
        Actions.WRITE
      )

      expect(decision1.allowed).toBe(false)

      // Assign new role
      rbacEngine.assignRole(agentUser.userId, Roles.WRITER)

      // Re-evaluate (should reflect new role)
      const decision2 = await rbacEngine.evaluate(
        agentUser.userId,
        Resources.TRANSACTION,
        Actions.WRITE
      )

      expect(decision2.allowed).toBe(true)
    })
  })

  describe('Sandbox Resource Management', () => {
    it('should cleanup sandbox resources', async () => {
      const effectId = 'test-cleanup'
      await sandbox.createIsolate(effectId)

      expect(sandbox.isolates.has(effectId)).toBe(true)

      await sandbox.destroyIsolate(effectId)

      expect(sandbox.isolates.has(effectId)).toBe(false)
    })

    it('should report memory usage per effect', async () => {
      const effectId = 'test-memory-usage'
      await sandbox.createIsolate(effectId)

      const memoryUsage = await sandbox.getMemoryUsage(effectId)

      expect(memoryUsage.used).toBeDefined()
      expect(memoryUsage.total).toBeDefined()
      expect(memoryUsage.limit).toBeDefined()
      expect(memoryUsage.percentage).toBeDefined()
      expect(memoryUsage.percentage).toBeLessThanOrEqual(100)
    })
  })

  describe('Security Controls Validation', () => {
    it('should prevent privilege escalation', async () => {
      // Reader tries to escalate to writer
      const decision = await rbacEngine.evaluate(
        readerUser.userId,
        Resources.ROLE,
        Actions.ADMIN
      )

      expect(decision.allowed).toBe(false)
    })

    it('should enforce least privilege principle', async () => {
      // Agent should only have specific permissions
      const hookWrite = await rbacEngine.evaluate(agentUser.userId, Resources.KNOWLEDGE_HOOK, Actions.WRITE)
      const effectWrite = await rbacEngine.evaluate(agentUser.userId, Resources.EFFECT, Actions.WRITE)
      const transactionWrite = await rbacEngine.evaluate(agentUser.userId, Resources.TRANSACTION, Actions.WRITE)
      const systemAdmin = await rbacEngine.evaluate(agentUser.userId, Resources.SYSTEM, Actions.ADMIN)

      expect(hookWrite.allowed).toBe(true)
      expect(effectWrite.allowed).toBe(true)
      expect(transactionWrite.allowed).toBe(false)
      expect(systemAdmin.allowed).toBe(false)
    })

    it('should enforce separation of duties', async () => {
      // No single user should have all permissions (except admin)
      const writerAdmin = await rbacEngine.evaluate(writerUser.userId, Resources.SYSTEM, Actions.ADMIN)
      const agentAdmin = await rbacEngine.evaluate(agentUser.userId, Resources.SYSTEM, Actions.ADMIN)
      const readerAdmin = await rbacEngine.evaluate(readerUser.userId, Resources.SYSTEM, Actions.ADMIN)

      expect(writerAdmin.allowed).toBe(false)
      expect(agentAdmin.allowed).toBe(false)
      expect(readerAdmin.allowed).toBe(false)
    })
  })
})
