/**
 * @file API Key Authentication Layer
 * @module @unrdf/daemon/auth/api-key-auth
 * @description Comprehensive API key authentication for daemon operations.
 * Provides key validation, environment variable support, and graceful degradation
 * for development vs production environments.
 */

import { z } from 'zod';
import { hashApiKey, verifyApiKey, generateApiKeyPair } from './crypto-utils.mjs';

/**
 * API Key validation schema
 * Minimum 32 characters (16 bytes hex), maximum 128 characters (64 bytes hex)
 */
export const ApiKeySchema = z.string().min(32).max(128).regex(/^[a-f0-9]+$/i, {
  message: 'API key must be a valid hexadecimal string',
});

/**
 * Authentication context schema
 */
export const AuthContextSchema = z.object({
  authenticated: z.boolean(),
  keyHash: z.string().optional(),
  environment: z.enum(['development', 'production', 'test']),
  timestamp: z.date(),
  source: z.enum(['header', 'env', 'none']),
});

/**
 * API Key Authentication Manager
 * Handles key validation, storage, and authentication for daemon operations
 */
export class ApiKeyAuthenticator {
  /**
   * @param {Object} options - Authentication configuration
   * @param {string} [options.storedKeyHash] - Pre-computed BLAKE3 hash of valid API key
   * @param {string} [options.environment='development'] - Runtime environment
   * @param {boolean} [options.requireInDev=false] - Require API key in development
   * @param {Function} [options.logger=console] - Logger instance
   */
  constructor(options = {}) {
    this.storedKeyHash = options.storedKeyHash || null;
    this.environment = options.environment || process.env.NODE_ENV || 'development';
    this.requireInDev = options.requireInDev || false;
    this.logger = options.logger || console;
    this.authLog = [];
  }

  /**
   * Set the stored API key hash
   * @param {string} hash - BLAKE3 hash of the API key
   * @throws {Error} If hash is invalid
   */
  setStoredHash(hash) {
    if (typeof hash !== 'string' || hash.length < 32) {
      throw new Error('Invalid API key hash');
    }
    this.storedKeyHash = hash;
  }

  /**
   * Initialize authenticator with a new API key pair
   * @returns {Promise<{key: string, hash: string}>} Generated key pair
   * @example
   * const authenticator = new ApiKeyAuthenticator();
   * const { key, hash } = await authenticator.initialize();
   * // Give key to user, store hash
   */
  async initialize() {
    const { key, hash } = await generateApiKeyPair();
    this.storedKeyHash = hash;
    return { key, hash };
  }

  /**
   * Validate API key from request context
   * @param {Object} context - Request context
   * @param {Object} [context.headers={}] - Request headers
   * @param {Object} [context.env=process.env] - Environment variables
   * @returns {Promise<Object>} Authentication result with context
   * @throws {Error} If authentication fails in production
   * @example
   * const result = await authenticator.authenticate({
   *   headers: { 'x-api-key': 'user-provided-key' }
   * });
   */
  async authenticate(context = {}) {
    const headers = context.headers || {};
    const env = context.env || process.env;

    // Extract API key from various sources
    const apiKey = this.extractApiKey(headers, env);

    // Create auth context
    const authContext = {
      authenticated: false,
      environment: this.environment,
      timestamp: new Date(),
      source: 'none',
    };

    // No API key provided
    if (!apiKey) {
      return this.handleMissingKey(authContext);
    }

    // Validate API key format
    try {
      ApiKeySchema.parse(apiKey);
    } catch (err) {
      const error = new Error(`Invalid API key format: ${err.message}`);
      this.logAuthAttempt(authContext, false, 'Invalid format');
      throw error;
    }

    // Check against stored hash
    if (!this.storedKeyHash) {
      const error = new Error('No stored API key hash configured');
      this.logAuthAttempt(authContext, false, 'No hash configured');
      throw error;
    }

    const isValid = await verifyApiKey(apiKey, this.storedKeyHash);

    if (!isValid) {
      const error = new Error('Invalid API key');
      this.logAuthAttempt(authContext, false, 'Key mismatch');
      throw error;
    }

    // Success
    authContext.authenticated = true;
    authContext.keyHash = this.storedKeyHash;
    authContext.source = this.determineSource(headers, env);

    this.logAuthAttempt(authContext, true, 'Success');

    return AuthContextSchema.parse(authContext);
  }

  /**
   * Extract API key from headers or environment
   * @private
   * @param {Object} headers - Request headers
   * @param {Object} env - Environment variables
   * @returns {string|null} Extracted API key or null
   */
  extractApiKey(headers, env) {
    // Priority: header > environment variable
    const headerKey = headers['x-api-key'] || headers['X-API-Key'];
    if (headerKey) return headerKey;

    const envKey = env.UNRDF_API_KEY;
    if (envKey) return envKey;

    return null;
  }

  /**
   * Determine the source of the API key
   * @private
   */
  determineSource(headers, env) {
    if (headers['x-api-key'] || headers['X-API-Key']) return 'header';
    if (env.UNRDF_API_KEY) return 'env';
    return 'none';
  }

  /**
   * Handle missing API key based on environment
   * @private
   * @param {Object} authContext - Current auth context
   * @returns {Object} Auth context (may throw in production)
   * @throws {Error} If in production or requireInDev is true
   */
  handleMissingKey(authContext) {
    const isProd = this.environment === 'production';

    if (isProd || this.requireInDev) {
      const error = new Error(
        `API key required in ${this.environment} environment`
      );
      this.logAuthAttempt(authContext, false, 'Missing key (required)');
      throw error;
    }

    // Development mode - warn but allow
    this.logger.warn(
      '⚠️  No API key provided - running in INSECURE mode (development only)'
    );

    authContext.authenticated = false;
    this.logAuthAttempt(authContext, false, 'Missing key (allowed in dev)');

    return AuthContextSchema.parse(authContext);
  }

  /**
   * Log authentication attempt for audit trail
   * @private
   */
  logAuthAttempt(context, success, reason) {
    const logEntry = {
      timestamp: context.timestamp,
      environment: context.environment,
      source: context.source,
      success,
      reason,
    };

    this.authLog.push(logEntry);

    // Keep last 1000 entries
    if (this.authLog.length > 1000) {
      this.authLog.shift();
    }
  }

  /**
   * Get authentication audit log
   * @returns {Array} Array of authentication attempts
   */
  getAuditLog() {
    return [...this.authLog];
  }

  /**
   * Clear audit log
   */
  clearAuditLog() {
    this.authLog = [];
  }
}

/**
 * Create authentication middleware for daemon operations
 * @param {ApiKeyAuthenticator} authenticator - Authenticator instance
 * @returns {Function} Middleware function
 * @example
 * const authenticator = new ApiKeyAuthenticator({ storedKeyHash: hash });
 * const authMiddleware = createAuthMiddleware(authenticator);
 *
 * // Use in request handler
 * await authMiddleware(req);
 */
export function createAuthMiddleware(authenticator) {
  return async function authMiddleware(context) {
    const authResult = await authenticator.authenticate(context);

    if (!authResult.authenticated && authenticator.environment === 'production') {
      throw new Error('Authentication required');
    }

    return authResult;
  };
}

/**
 * Convenience function to create and initialize authenticator
 * @param {Object} options - Authenticator options
 * @returns {Promise<{authenticator: ApiKeyAuthenticator, key: string}>}
 * @example
 * const { authenticator, key } = await createAuthenticator({
 *   environment: 'production'
 * });
 * console.log('Save this key:', key);
 */
export async function createAuthenticator(options = {}) {
  const authenticator = new ApiKeyAuthenticator(options);
  const { key, hash } = await authenticator.initialize();
  return { authenticator, key };
}
