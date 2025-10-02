/**
 * @file Authentication System Tests
 * @description Comprehensive tests for Byzantine auth implementation
 */
import { describe, it, expect, beforeAll } from 'vitest'
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
  VALIDATOR_THRESHOLD,
  TOTAL_VALIDATORS
} from '../../server/utils/auth.mjs'

describe('Password Hashing', () => {
  it('should hash password with bcrypt', async () => {
    const password = 'TestPassword123'
    const hash = await hashPassword(password)

    expect(hash).toBeDefined()
    expect(hash).not.toBe(password)
    expect(hash).toMatch(/^\$2[aby]\$/)
  })

  it('should verify correct password', async () => {
    const password = 'TestPassword123'
    const hash = await hashPassword(password)
    const isValid = await verifyPassword(password, hash)

    expect(isValid).toBe(true)
  })

  it('should reject incorrect password', async () => {
    const password = 'TestPassword123'
    const hash = await hashPassword(password)
    const isValid = await verifyPassword('WrongPassword', hash)

    expect(isValid).toBe(false)
  })
})

describe('JWT Token Generation', () => {
  it('should generate access and refresh tokens', () => {
    const tokens = generateTokenPair('user-123', 'test@example.com', ['user'])

    expect(tokens.accessToken).toBeDefined()
    expect(tokens.refreshToken).toBeDefined()
    expect(tokens.expiresIn).toBeGreaterThan(0)
  })

  it('should include user data in token payload', () => {
    const tokens = generateTokenPair('user-123', 'test@example.com', ['admin', 'user'])
    const payload = verifyAccessToken(tokens.accessToken)

    expect(payload.userId).toBe('user-123')
    expect(payload.email).toBe('test@example.com')
    expect(payload.roles).toContain('admin')
    expect(payload.role).toBe('admin')
  })

  it('should set default role to user', () => {
    const tokens = generateTokenPair('user-123', 'test@example.com', ['user'])
    const payload = verifyAccessToken(tokens.accessToken)

    expect(payload.role).toBe('user')
  })
})

describe('JWT Token Verification', () => {
  it('should verify valid access token', () => {
    const tokens = generateTokenPair('user-123', 'test@example.com', ['user'])
    const payload = verifyAccessToken(tokens.accessToken)

    expect(payload).toBeDefined()
    expect(payload.userId).toBe('user-123')
  })

  it('should verify valid refresh token', () => {
    const tokens = generateTokenPair('user-123', 'test@example.com', ['user'])
    const payload = verifyRefreshToken(tokens.refreshToken)

    expect(payload).toBeDefined()
    expect(payload.userId).toBe('user-123')
    expect(payload.tokenType).toBe('refresh')
  })

  it('should reject invalid token', () => {
    const payload = verifyAccessToken('invalid.token.here')

    expect(payload).toBeNull()
  })

  it('should reject expired token', () => {
    // This would require mocking time or using a very short expiry
    // For now, we test with invalid token
    const payload = verifyAccessToken('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.expired.token')

    expect(payload).toBeNull()
  })
})

describe('Role Management', () => {
  it('should extract roles from token', () => {
    const tokens = generateTokenPair('user-123', 'test@example.com', ['admin', 'user'])
    const roles = extractRoles(tokens.accessToken)

    expect(roles).toContain('admin')
    expect(roles).toContain('user')
  })

  it('should check if user has specific role', () => {
    const tokens = generateTokenPair('user-123', 'test@example.com', ['admin', 'user'])

    expect(hasRole(tokens.accessToken, 'admin')).toBe(true)
    expect(hasRole(tokens.accessToken, 'user')).toBe(true)
    expect(hasRole(tokens.accessToken, 'superadmin')).toBe(false)
  })

  it('should grant access to admin for any role', () => {
    const tokens = generateTokenPair('user-123', 'test@example.com', ['admin'])

    expect(hasRole(tokens.accessToken, 'user')).toBe(true)
    expect(hasRole(tokens.accessToken, 'moderator')).toBe(true)
  })
})

describe('Byzantine Signature System', () => {
  it('should create Byzantine signature', () => {
    const operation = 'test-operation'
    const data = { action: 'delete', resourceId: 'xyz' }
    const signature = createByzantineSignature(operation, data, 'validator-1')

    expect(signature.validator).toBe('validator-1')
    expect(signature.signature).toBeDefined()
    expect(signature.publicKey).toBeDefined()
    expect(signature.timestamp).toBeGreaterThan(0)
  })

  it('should verify valid Byzantine signature', () => {
    const operation = 'test-operation'
    const data = { action: 'delete', resourceId: 'xyz' }
    const signature = createByzantineSignature(operation, data, 'validator-1')

    const isValid = verifyByzantineSignature(operation, data, signature)
    expect(isValid).toBe(true)
  })

  it('should reject tampered signature', () => {
    const operation = 'test-operation'
    const data = { action: 'delete', resourceId: 'xyz' }
    const signature = createByzantineSignature(operation, data, 'validator-1')

    // Tamper with signature
    signature.signature = signature.signature.replace(/a/g, 'b')

    const isValid = verifyByzantineSignature(operation, data, signature)
    expect(isValid).toBe(false)
  })

  it('should reject signature with wrong data', () => {
    const operation = 'test-operation'
    const data = { action: 'delete', resourceId: 'xyz' }
    const signature = createByzantineSignature(operation, data, 'validator-1')

    const tamperedData = { action: 'delete', resourceId: 'abc' }
    const isValid = verifyByzantineSignature(operation, tamperedData, signature)

    expect(isValid).toBe(false)
  })
})

describe('Byzantine Consensus', () => {
  it('should have correct validator configuration', () => {
    expect(VALIDATOR_THRESHOLD).toBe(3)
    expect(TOTAL_VALIDATORS).toBe(5)
  })

  it('should initialize validators', () => {
    const validators = getAllValidators()

    expect(validators).toHaveLength(TOTAL_VALIDATORS)
    expect(validators[0]).toHaveProperty('id')
    expect(validators[0]).toHaveProperty('publicKey')
  })

  it('should achieve consensus with 3 valid signatures', () => {
    const operation = 'admin-operation'
    const data = { critical: true }

    const signatures = [
      createByzantineSignature(operation, data, 'validator-1'),
      createByzantineSignature(operation, data, 'validator-2'),
      createByzantineSignature(operation, data, 'validator-3')
    ]

    const consensus = verifyByzantineConsensus(operation, data, signatures)

    expect(consensus.valid).toBe(true)
    expect(consensus.validCount).toBe(3)
    expect(consensus.threshold).toBe(VALIDATOR_THRESHOLD)
    expect(consensus.validators).toHaveLength(3)
  })

  it('should fail consensus with fewer than 3 signatures', () => {
    const operation = 'admin-operation'
    const data = { critical: true }

    const signatures = [
      createByzantineSignature(operation, data, 'validator-1'),
      createByzantineSignature(operation, data, 'validator-2')
    ]

    const consensus = verifyByzantineConsensus(operation, data, signatures)

    expect(consensus.valid).toBe(false)
    expect(consensus.validCount).toBe(2)
  })

  it('should fail consensus with invalid signatures', () => {
    const operation = 'admin-operation'
    const data = { critical: true }

    const signatures = [
      createByzantineSignature(operation, data, 'validator-1'),
      createByzantineSignature(operation, data, 'validator-2'),
      { validator: 'fake', signature: 'invalid', publicKey: 'fake', timestamp: Date.now() }
    ]

    const consensus = verifyByzantineConsensus(operation, data, signatures)

    expect(consensus.valid).toBe(false)
    expect(consensus.validCount).toBe(2)
  })

  it('should create admin operation with consensus', () => {
    const operation = 'critical-delete'
    const data = { resourceId: 'xyz' }

    const result = createAdminOperation(operation, data)

    expect(result.signatures).toHaveLength(VALIDATOR_THRESHOLD)
    expect(result.consensus.valid).toBe(true)
    expect(result.consensus.validCount).toBe(VALIDATOR_THRESHOLD)
  })
})

describe('User Management', () => {
  it('should register new user', async () => {
    const email = `test-${Date.now()}@example.com`
    const password = 'SecurePass123'

    const user = await registerUser(email, password, ['user'])

    expect(user.userId).toBeDefined()
    expect(user.email).toBe(email)
    expect(user.roles).toContain('user')
  })

  it('should prevent duplicate registration', async () => {
    const email = `duplicate-${Date.now()}@example.com`
    const password = 'SecurePass123'

    await registerUser(email, password, ['user'])

    await expect(
      registerUser(email, password, ['user'])
    ).rejects.toThrow('User already exists')
  })

  it('should authenticate user with correct credentials', async () => {
    const email = `auth-test-${Date.now()}@example.com`
    const password = 'SecurePass123'

    await registerUser(email, password, ['user'])
    const result = await authenticateUser(email, password)

    expect(result).toBeDefined()
    expect(result.user.email).toBe(email)
    expect(result.tokens.accessToken).toBeDefined()
    expect(result.tokens.refreshToken).toBeDefined()
  })

  it('should reject authentication with wrong password', async () => {
    const email = `wrong-pass-${Date.now()}@example.com`
    const password = 'SecurePass123'

    await registerUser(email, password, ['user'])
    const result = await authenticateUser(email, 'WrongPassword')

    expect(result).toBeNull()
  })

  it('should reject authentication for non-existent user', async () => {
    const result = await authenticateUser('nonexistent@example.com', 'password')

    expect(result).toBeNull()
  })

  it('should get user by ID', async () => {
    const email = `get-user-${Date.now()}@example.com`
    const password = 'SecurePass123'

    const registered = await registerUser(email, password, ['user'])
    const user = getUserById(registered.userId)

    expect(user).toBeDefined()
    expect(user.id).toBe(registered.userId)
    expect(user.email).toBe(email)
  })

  it('should return null for non-existent user ID', () => {
    const user = getUserById('non-existent-id')

    expect(user).toBeNull()
  })
})

describe('Default Admin User', () => {
  it('should have admin user pre-created', async () => {
    const result = await authenticateUser('admin@unrdf.local', 'admin123')

    expect(result).toBeDefined()
    expect(result.user.email).toBe('admin@unrdf.local')
    expect(result.user.roles).toContain('admin')
    expect(result.user.roles).toContain('user')
  })
})
