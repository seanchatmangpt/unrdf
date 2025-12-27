/**
 * @file Authentication Utilities
 * @description JWT generation/validation, cryptographic signature verification, Byzantine consensus
 */
import jwt from 'jsonwebtoken'
import bcrypt from 'bcrypt'
import crypto from 'crypto'
import { ec as EC } from 'elliptic'
import { z } from 'zod'

const ec = new EC('secp256k1')

/**
 * @typedef {Object} JWTPayload
 * @property {string} userId - User identifier
 * @property {string} email - User email
 * @property {string[]} roles - User roles
 * @property {'user'|'admin'|'system'} role - Primary role
 * @property {number} iat - Issued at
 * @property {number} exp - Expiration
 */

/**
 * @typedef {Object} TokenPair
 * @property {string} accessToken - JWT access token
 * @property {string} refreshToken - JWT refresh token
 * @property {number} expiresIn - Token expiration in seconds
 */

/**
 * @typedef {Object} ByzantineSignature
 * @property {string} validator - Validator ID
 * @property {string} signature - ECDSA signature
 * @property {string} publicKey - Validator public key
 * @property {number} timestamp - Signature timestamp
 */

// Environment-based configuration
const JWT_SECRET = process.env.JWT_SECRET || crypto.randomBytes(64).toString('hex')
const JWT_REFRESH_SECRET = process.env.JWT_REFRESH_SECRET || crypto.randomBytes(64).toString('hex')
const JWT_EXPIRY = process.env.JWT_EXPIRY || '15m'
const JWT_REFRESH_EXPIRY = process.env.JWT_REFRESH_EXPIRY || '7d'
const BCRYPT_ROUNDS = parseInt(process.env.BCRYPT_ROUNDS || '12', 10)

// Byzantine consensus configuration
const VALIDATOR_THRESHOLD = 3 // Minimum signatures required (3-of-5)
const TOTAL_VALIDATORS = 5

/**
 * In-memory user store (replace with database in production)
 * @type {Map<string, {id: string, email: string, passwordHash: string, roles: string[], publicKey?: string}>}
 */
const userStore = new Map()

/**
 * In-memory validator store (Byzantine consensus validators)
 * @type {Map<string, {id: string, publicKey: string, privateKey: string}>}
 */
const validatorStore = new Map()

/**
 * Initialize Byzantine validators
 */
function initializeValidators() {
  if (validatorStore.size > 0) return

  for (let i = 0; i < TOTAL_VALIDATORS; i++) {
    const keyPair = ec.genKeyPair()
    const validatorId = `validator-${i + 1}`

    validatorStore.set(validatorId, {
      id: validatorId,
      publicKey: keyPair.getPublic('hex'),
      privateKey: keyPair.getPrivate('hex')
    })
  }
}

// Initialize validators on module load
initializeValidators()

/**
 * Hash password using bcrypt
 * @param {string} password - Plain text password
 * @returns {Promise<string>} Hashed password
 */
export async function hashPassword(password) {
  return bcrypt.hash(password, BCRYPT_ROUNDS)
}

/**
 * Verify password against hash
 * @param {string} password - Plain text password
 * @param {string} hash - Hashed password
 * @returns {Promise<boolean>} True if password matches
 */
export async function verifyPassword(password, hash) {
  return bcrypt.compare(password, hash)
}

/**
 * Generate JWT token pair (access + refresh)
 * @param {string} userId - User identifier
 * @param {string} email - User email
 * @param {string[]} roles - User roles
 * @returns {TokenPair} Access and refresh tokens
 */
export function generateTokenPair(userId, email, roles = ['user']) {
  const primaryRole = roles.includes('admin') ? 'admin' : roles.includes('system') ? 'system' : 'user'

  const payload = {
    userId,
    email,
    roles,
    role: primaryRole
  }

  const accessToken = jwt.sign(payload, JWT_SECRET, {
    expiresIn: JWT_EXPIRY,
    issuer: 'unrdf-sidecar',
    audience: 'unrdf-api'
  })

  const refreshToken = jwt.sign(
    { userId, tokenType: 'refresh' },
    JWT_REFRESH_SECRET,
    {
      expiresIn: JWT_REFRESH_EXPIRY,
      issuer: 'unrdf-sidecar',
      audience: 'unrdf-api'
    }
  )

  // Parse expiry to seconds
  const expiresIn = JWT_EXPIRY.endsWith('m')
    ? parseInt(JWT_EXPIRY) * 60
    : JWT_EXPIRY.endsWith('h')
    ? parseInt(JWT_EXPIRY) * 3600
    : 900

  return {
    accessToken,
    refreshToken,
    expiresIn
  }
}

/**
 * Verify JWT access token
 * @param {string} token - JWT token
 * @returns {JWTPayload|null} Decoded payload or null if invalid
 */
export function verifyAccessToken(token) {
  try {
    return jwt.verify(token, JWT_SECRET, {
      issuer: 'unrdf-sidecar',
      audience: 'unrdf-api'
    })
  } catch (error) {
    return null
  }
}

/**
 * Verify JWT refresh token
 * @param {string} token - JWT refresh token
 * @returns {Object|null} Decoded payload or null if invalid
 */
export function verifyRefreshToken(token) {
  try {
    return jwt.verify(token, JWT_REFRESH_SECRET, {
      issuer: 'unrdf-sidecar',
      audience: 'unrdf-api'
    })
  } catch (error) {
    return null
  }
}

/**
 * Extract roles from JWT token
 * @param {string} token - JWT token
 * @returns {string[]} Array of roles
 */
export function extractRoles(token) {
  const payload = verifyAccessToken(token)
  return payload?.roles || []
}

/**
 * Check if token has required role
 * @param {string} token - JWT token
 * @param {string} requiredRole - Required role
 * @returns {boolean} True if user has role
 */
export function hasRole(token, requiredRole) {
  const roles = extractRoles(token)
  return roles.includes(requiredRole) || roles.includes('admin')
}

/**
 * Create Byzantine signature for admin operation
 * @param {string} operation - Operation identifier
 * @param {Object} data - Operation data
 * @param {string} validatorId - Validator ID
 * @returns {ByzantineSignature} Signature object
 */
export function createByzantineSignature(operation, data, validatorId) {
  const validator = validatorStore.get(validatorId)
  if (!validator) {
    throw new Error(`Validator ${validatorId} not found`)
  }

  const message = JSON.stringify({ operation, data, timestamp: Date.now() })
  const messageHash = crypto.createHash('sha256').update(message).digest()

  const keyPair = ec.keyFromPrivate(validator.privateKey, 'hex')
  const signature = keyPair.sign(messageHash)

  return {
    validator: validatorId,
    signature: signature.toDER('hex'),
    publicKey: validator.publicKey,
    timestamp: Date.now()
  }
}

/**
 * Verify Byzantine signature
 * @param {string} operation - Operation identifier
 * @param {Object} data - Operation data
 * @param {ByzantineSignature} signature - Signature to verify
 * @returns {boolean} True if signature is valid
 */
export function verifyByzantineSignature(operation, data, signature) {
  try {
    const message = JSON.stringify({ operation, data, timestamp: signature.timestamp })
    const messageHash = crypto.createHash('sha256').update(message).digest()

    const key = ec.keyFromPublic(signature.publicKey, 'hex')
    return key.verify(messageHash, signature.signature)
  } catch (error) {
    return false
  }
}

/**
 * Verify Byzantine consensus (3-of-5 threshold)
 * @param {string} operation - Operation identifier
 * @param {Object} data - Operation data
 * @param {ByzantineSignature[]} signatures - Array of signatures
 * @returns {{valid: boolean, validCount: number, threshold: number, validators: string[]}}
 */
export function verifyByzantineConsensus(operation, data, signatures) {
  if (!Array.isArray(signatures) || signatures.length < VALIDATOR_THRESHOLD) {
    return {
      valid: false,
      validCount: 0,
      threshold: VALIDATOR_THRESHOLD,
      validators: []
    }
  }

  const validSignatures = signatures.filter(sig =>
    verifyByzantineSignature(operation, data, sig)
  )

  const validValidators = validSignatures.map(sig => sig.validator)

  return {
    valid: validSignatures.length >= VALIDATOR_THRESHOLD,
    validCount: validSignatures.length,
    threshold: VALIDATOR_THRESHOLD,
    validators: validValidators
  }
}

/**
 * Create admin operation with Byzantine consensus
 * @param {string} operation - Operation identifier
 * @param {Object} data - Operation data
 * @returns {{signatures: ByzantineSignature[], consensus: Object}}
 */
export function createAdminOperation(operation, data) {
  const signatures = []

  // Get first N validators to sign (simulate consensus)
  const validators = Array.from(validatorStore.keys()).slice(0, VALIDATOR_THRESHOLD)

  for (const validatorId of validators) {
    const signature = createByzantineSignature(operation, data, validatorId)
    signatures.push(signature)
  }

  const consensus = verifyByzantineConsensus(operation, data, signatures)

  return {
    signatures,
    consensus
  }
}

/**
 * User registration
 * @param {string} email - User email
 * @param {string} password - User password
 * @param {string[]} roles - User roles
 * @returns {Promise<{userId: string, email: string, roles: string[]}>}
 */
export async function registerUser(email, password, roles = ['user']) {
  if (userStore.has(email)) {
    throw new Error('User already exists')
  }

  const userId = crypto.randomUUID()
  const passwordHash = await hashPassword(password)

  userStore.set(email, {
    id: userId,
    email,
    passwordHash,
    roles
  })

  return { userId, email, roles }
}

/**
 * User authentication
 * @param {string} email - User email
 * @param {string} password - User password
 * @returns {Promise<{user: Object, tokens: TokenPair}|null>}
 */
export async function authenticateUser(email, password) {
  const user = userStore.get(email)
  if (!user) {
    return null
  }

  const isValid = await verifyPassword(password, user.passwordHash)
  if (!isValid) {
    return null
  }

  const tokens = generateTokenPair(user.id, user.email, user.roles)

  return {
    user: {
      id: user.id,
      email: user.email,
      roles: user.roles
    },
    tokens
  }
}

/**
 * Get user by ID
 * @param {string} userId - User ID
 * @returns {Object|null} User object or null
 */
export function getUserById(userId) {
  for (const user of userStore.values()) {
    if (user.id === userId) {
      return {
        id: user.id,
        email: user.email,
        roles: user.roles
      }
    }
  }
  return null
}

/**
 * Get all validators (for admin)
 * @returns {Array<{id: string, publicKey: string}>}
 */
export function getAllValidators() {
  return Array.from(validatorStore.values()).map(v => ({
    id: v.id,
    publicKey: v.publicKey
  }))
}

// Create default admin user on initialization
;(async () => {
  if (!userStore.has('admin@unrdf.local')) {
    await registerUser('admin@unrdf.local', 'admin123', ['admin', 'user'])
  }
})()

export {
  JWT_SECRET,
  JWT_REFRESH_SECRET,
  JWT_EXPIRY,
  JWT_REFRESH_EXPIRY,
  VALIDATOR_THRESHOLD,
  TOTAL_VALIDATORS,
  userStore,
  validatorStore
}
