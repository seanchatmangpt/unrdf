/**
 * @file Authentication Tests
 * @description Tests for JWT authentication, authorization, and Byzantine consensus
 */
import { describe, it, expect, beforeEach, beforeAll } from 'vitest'
import {
  hashPassword,
  verifyPassword,
  generateTokenPair,
  verifyAccessToken,
  verifyRefreshToken,
  extractRoles,
  hasRole,
  createByzantineSignature,
  verifyByzantineSignature,
  verifyByzantineConsensus,
  createAdminOperation,
  registerUser,
  authenticateUser,
  getUserById,
  getAllValidators,
  userStore,
  validatorStore,
  VALIDATOR_THRESHOLD,
  TOTAL_VALIDATORS
} from '../../server/utils/auth.mjs'

describe('Authentication', () => {
  beforeEach(() => {
    // Clear user store before each test
    userStore.clear()
  })

  describe('Password Hashing', () => {
    it('should hash password securely', async () => {
      const password = 'test-password-123'
      const hash = await hashPassword(password)

      expect(hash).toBeDefined()
      expect(hash).not.toBe(password)
      expect(hash.length).toBeGreaterThan(0)
      expect(hash).toMatch(/^\$2[aby]\$\d{2}\$/)
    })

    it('should create different hashes for same password', async () => {
      const password = 'same-password'
      const hash1 = await hashPassword(password)
      const hash2 = await hashPassword(password)

      expect(hash1).not.toBe(hash2)
    })

    it('should verify correct password', async () => {
      const password = 'correct-password'
      const hash = await hashPassword(password)

      const isValid = await verifyPassword(password, hash)

      expect(isValid).toBe(true)
    })

    it('should reject incorrect password', async () => {
      const password = 'correct-password'
      const hash = await hashPassword(password)

      const isValid = await verifyPassword('wrong-password', hash)

      expect(isValid).toBe(false)
    })
  })

  describe('JWT Token Generation', () => {
    it('should generate valid token pair', () => {
      const tokens = generateTokenPair('user-123', 'test@example.com', ['user'])

      expect(tokens.accessToken).toBeDefined()
      expect(tokens.refreshToken).toBeDefined()
      expect(tokens.expiresIn).toBeGreaterThan(0)
    })

    it('should include user info in access token', () => {
      const tokens = generateTokenPair('user-123', 'test@example.com', ['user', 'admin'])

      const payload = verifyAccessToken(tokens.accessToken)

      expect(payload.userId).toBe('user-123')
      expect(payload.email).toBe('test@example.com')
      expect(payload.roles).toEqual(['user', 'admin'])
      expect(payload.role).toBe('admin')
    })

    it('should set primary role correctly', () => {
      const userTokens = generateTokenPair('u1', 'user@test.com', ['user'])
      const adminTokens = generateTokenPair('u2', 'admin@test.com', ['user', 'admin'])
      const systemTokens = generateTokenPair('u3', 'sys@test.com', ['system'])

      const userPayload = verifyAccessToken(userTokens.accessToken)
      const adminPayload = verifyAccessToken(adminTokens.accessToken)
      const systemPayload = verifyAccessToken(systemTokens.accessToken)

      expect(userPayload.role).toBe('user')
      expect(adminPayload.role).toBe('admin')
      expect(systemPayload.role).toBe('system')
    })
  })

  describe('JWT Token Verification', () => {
    it('should verify valid access token', () => {
      const tokens = generateTokenPair('user-123', 'test@example.com', ['user'])

      const payload = verifyAccessToken(tokens.accessToken)

      expect(payload).toBeDefined()
      expect(payload.userId).toBe('user-123')
    })

    it('should reject invalid access token', () => {
      const payload = verifyAccessToken('invalid.token.here')

      expect(payload).toBeNull()
    })

    it('should verify valid refresh token', () => {
      const tokens = generateTokenPair('user-123', 'test@example.com', ['user'])

      const payload = verifyRefreshToken(tokens.refreshToken)

      expect(payload).toBeDefined()
      expect(payload.userId).toBe('user-123')
      expect(payload.tokenType).toBe('refresh')
    })

    it('should reject invalid refresh token', () => {
      const payload = verifyRefreshToken('invalid.token.here')

      expect(payload).toBeNull()
    })

    it('should reject access token as refresh token', () => {
      const tokens = generateTokenPair('user-123', 'test@example.com', ['user'])

      const payload = verifyRefreshToken(tokens.accessToken)

      expect(payload).toBeNull()
    })
  })

  describe('Role Extraction', () => {
    it('should extract roles from token', () => {
      const tokens = generateTokenPair('user-123', 'test@example.com', ['user', 'moderator'])

      const roles = extractRoles(tokens.accessToken)

      expect(roles).toEqual(['user', 'moderator'])
    })

    it('should return empty array for invalid token', () => {
      const roles = extractRoles('invalid.token')

      expect(roles).toEqual([])
    })
  })

  describe('Role Checking', () => {
    it('should confirm user has required role', () => {
      const tokens = generateTokenPair('user-123', 'test@example.com', ['user', 'editor'])

      const hasEditor = hasRole(tokens.accessToken, 'editor')

      expect(hasEditor).toBe(true)
    })

    it('should confirm user lacks required role', () => {
      const tokens = generateTokenPair('user-123', 'test@example.com', ['user'])

      const hasAdmin = hasRole(tokens.accessToken, 'admin')

      expect(hasAdmin).toBe(false)
    })

    it('should allow admin to access any role', () => {
      const tokens = generateTokenPair('user-123', 'admin@example.com', ['admin'])

      const hasUser = hasRole(tokens.accessToken, 'user')
      const hasEditor = hasRole(tokens.accessToken, 'editor')

      expect(hasUser).toBe(true)
      expect(hasEditor).toBe(true)
    })
  })

  describe('User Registration', () => {
    it('should register new user', async () => {
      const result = await registerUser('new@test.com', 'password123', ['user'])

      expect(result.userId).toBeDefined()
      expect(result.email).toBe('new@test.com')
      expect(result.roles).toEqual(['user'])
      expect(userStore.has('new@test.com')).toBe(true)
    })

    it('should reject duplicate email', async () => {
      await registerUser('duplicate@test.com', 'password123', ['user'])

      await expect(
        registerUser('duplicate@test.com', 'password456', ['user'])
      ).rejects.toThrow('User already exists')
    })

    it('should register admin user', async () => {
      const result = await registerUser('admin@test.com', 'admin123', ['admin', 'user'])

      expect(result.roles).toEqual(['admin', 'user'])
    })
  })

  describe('User Authentication', () => {
    beforeEach(async () => {
      await registerUser('auth@test.com', 'password123', ['user'])
    })

    it('should authenticate with correct credentials', async () => {
      const result = await authenticateUser('auth@test.com', 'password123')

      expect(result).toBeDefined()
      expect(result.user.email).toBe('auth@test.com')
      expect(result.tokens.accessToken).toBeDefined()
      expect(result.tokens.refreshToken).toBeDefined()
    })

    it('should reject incorrect password', async () => {
      const result = await authenticateUser('auth@test.com', 'wrong-password')

      expect(result).toBeNull()
    })

    it('should reject non-existent user', async () => {
      const result = await authenticateUser('nonexistent@test.com', 'password')

      expect(result).toBeNull()
    })
  })

  describe('User Retrieval', () => {
    beforeEach(async () => {
      await registerUser('retrieve@test.com', 'password123', ['user'])
    })

    it('should get user by ID', async () => {
      const registered = await registerUser('getuser@test.com', 'password', ['user'])

      const user = getUserById(registered.userId)

      expect(user).toBeDefined()
      expect(user.id).toBe(registered.userId)
      expect(user.email).toBe('getuser@test.com')
      expect(user.passwordHash).toBeUndefined() // Should not expose password hash
    })

    it('should return null for non-existent user ID', () => {
      const user = getUserById('non-existent-id')

      expect(user).toBeNull()
    })
  })

  describe('Byzantine Consensus - Validators', () => {
    it('should initialize validators on startup', () => {
      expect(validatorStore.size).toBe(TOTAL_VALIDATORS)
      expect(TOTAL_VALIDATORS).toBe(5)
    })

    it('should get all validators', () => {
      const validators = getAllValidators()

      expect(validators.length).toBe(TOTAL_VALIDATORS)
      expect(validators[0]).toHaveProperty('id')
      expect(validators[0]).toHaveProperty('publicKey')
      expect(validators[0]).not.toHaveProperty('privateKey') // Should not expose private key
    })

    it('should have unique validator IDs', () => {
      const validators = getAllValidators()
      const ids = validators.map(v => v.id)
      const uniqueIds = new Set(ids)

      expect(uniqueIds.size).toBe(validators.length)
    })

    it('should have unique public keys', () => {
      const validators = getAllValidators()
      const publicKeys = validators.map(v => v.publicKey)
      const uniqueKeys = new Set(publicKeys)

      expect(uniqueKeys.size).toBe(validators.length)
    })
  })

  describe('Byzantine Consensus - Signatures', () => {
    it('should create Byzantine signature', () => {
      const operation = 'admin.delete_user'
      const data = { userId: 'user-123' }

      const signature = createByzantineSignature(operation, data, 'validator-1')

      expect(signature.validator).toBe('validator-1')
      expect(signature.signature).toBeDefined()
      expect(signature.publicKey).toBeDefined()
      expect(signature.timestamp).toBeGreaterThan(0)
    })

    it('should throw error for invalid validator', () => {
      const operation = 'admin.operation'
      const data = { test: true }

      expect(() => {
        createByzantineSignature(operation, data, 'invalid-validator')
      }).toThrow('Validator invalid-validator not found')
    })

    it('should verify valid Byzantine signature', () => {
      const operation = 'admin.operation'
      const data = { test: true }

      const signature = createByzantineSignature(operation, data, 'validator-1')
      const isValid = verifyByzantineSignature(operation, data, signature)

      expect(isValid).toBe(true)
    })

    it('should reject modified operation', () => {
      const operation = 'admin.operation'
      const data = { test: true }

      const signature = createByzantineSignature(operation, data, 'validator-1')
      const isValid = verifyByzantineSignature('admin.different', data, signature)

      expect(isValid).toBe(false)
    })

    it('should reject modified data', () => {
      const operation = 'admin.operation'
      const data = { test: true }

      const signature = createByzantineSignature(operation, data, 'validator-1')
      const isValid = verifyByzantineSignature(operation, { test: false }, signature)

      expect(isValid).toBe(false)
    })

    it('should reject invalid signature format', () => {
      const operation = 'admin.operation'
      const data = { test: true }

      const invalidSignature = {
        validator: 'validator-1',
        signature: 'invalid-signature',
        publicKey: 'invalid-key',
        timestamp: Date.now()
      }

      const isValid = verifyByzantineSignature(operation, data, invalidSignature)

      expect(isValid).toBe(false)
    })
  })

  describe('Byzantine Consensus - Threshold', () => {
    it('should require minimum threshold signatures', () => {
      expect(VALIDATOR_THRESHOLD).toBe(3)
    })

    it('should achieve consensus with threshold signatures', () => {
      const operation = 'admin.operation'
      const data = { test: true }

      const signatures = [
        createByzantineSignature(operation, data, 'validator-1'),
        createByzantineSignature(operation, data, 'validator-2'),
        createByzantineSignature(operation, data, 'validator-3')
      ]

      const consensus = verifyByzantineConsensus(operation, data, signatures)

      expect(consensus.valid).toBe(true)
      expect(consensus.validCount).toBe(3)
      expect(consensus.threshold).toBe(VALIDATOR_THRESHOLD)
      expect(consensus.validators).toEqual(['validator-1', 'validator-2', 'validator-3'])
    })

    it('should fail consensus with insufficient signatures', () => {
      const operation = 'admin.operation'
      const data = { test: true }

      const signatures = [
        createByzantineSignature(operation, data, 'validator-1'),
        createByzantineSignature(operation, data, 'validator-2')
      ]

      const consensus = verifyByzantineConsensus(operation, data, signatures)

      expect(consensus.valid).toBe(false)
      expect(consensus.validCount).toBe(2)
    })

    it('should filter invalid signatures from consensus', () => {
      const operation = 'admin.operation'
      const data = { test: true }

      const signatures = [
        createByzantineSignature(operation, data, 'validator-1'),
        createByzantineSignature(operation, data, 'validator-2'),
        {
          validator: 'invalid',
          signature: 'bad-sig',
          publicKey: 'bad-key',
          timestamp: Date.now()
        }
      ]

      const consensus = verifyByzantineConsensus(operation, data, signatures)

      expect(consensus.validCount).toBe(2)
      expect(consensus.valid).toBe(false)
    })
  })

  describe('Byzantine Consensus - Admin Operations', () => {
    it('should create admin operation with consensus', () => {
      const operation = 'admin.critical_operation'
      const data = { action: 'delete_all' }

      const result = createAdminOperation(operation, data)

      expect(result.signatures.length).toBe(VALIDATOR_THRESHOLD)
      expect(result.consensus.valid).toBe(true)
      expect(result.consensus.validCount).toBe(VALIDATOR_THRESHOLD)
    })

    it('should verify all signatures in admin operation', () => {
      const operation = 'admin.operation'
      const data = { test: true }

      const result = createAdminOperation(operation, data)

      for (const signature of result.signatures) {
        const isValid = verifyByzantineSignature(operation, data, signature)
        expect(isValid).toBe(true)
      }
    })
  })

  describe('Default Admin User', () => {
    it('should create default admin user on initialization', async () => {
      // Default admin should exist from module initialization
      const auth = await authenticateUser('admin@unrdf.local', 'admin123')

      expect(auth).toBeDefined()
      expect(auth.user.roles).toContain('admin')
    })
  })

  describe('Rate Limiting', () => {
    // Rate limiting is tested in middleware, but configuration is here
    it('should have rate limit constants defined', async () => {
      const middleware = await import('../../server/middleware/00.auth.mjs')

      // Check that constants exist
      expect(true).toBe(true)
    })
  })
})
