/**
 * @file API Key Authentication Tests
 * @description Comprehensive test suite for API key authentication layer
 * Coverage: key generation, validation, environment handling, security
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  ApiKeyAuthenticator,
  createAuthMiddleware,
  createAuthenticator,
  ApiKeySchema,
} from '../src/auth/api-key-auth.mjs';
import {
  generateSecureApiKey,
  hashApiKey,
  verifyApiKey,
  generateApiKeyPair,
} from '../src/auth/crypto-utils.mjs';

describe('Crypto Utils', () => {
  describe('generateSecureApiKey()', () => {
    it('should generate 64 character hex string by default', () => {
      // Arrange & Act
      const key = generateSecureApiKey();

      // Assert
      expect(key).toHaveLength(64); // 32 bytes = 64 hex chars
      expect(key).toMatch(/^[a-f0-9]+$/i);
    });

    it('should generate key of specified length', () => {
      // Arrange & Act
      const key = generateSecureApiKey(16);

      // Assert
      expect(key).toHaveLength(32); // 16 bytes = 32 hex chars
    });

    it('should throw error for length < 16 bytes', () => {
      // Arrange & Act & Assert
      expect(() => generateSecureApiKey(8)).toThrow(RangeError);
      expect(() => generateSecureApiKey(8)).toThrow('between 16 and 64 bytes');
    });

    it('should throw error for length > 64 bytes', () => {
      // Arrange & Act & Assert
      expect(() => generateSecureApiKey(128)).toThrow(RangeError);
    });

    it('should generate unique keys on each call', () => {
      // Arrange & Act
      const key1 = generateSecureApiKey();
      const key2 = generateSecureApiKey();

      // Assert
      expect(key1).not.toBe(key2);
    });
  });

  describe('hashApiKey()', () => {
    it('should hash API key using BLAKE3', async () => {
      // Arrange
      const apiKey = 'test-api-key-123456789abcdef0';

      // Act
      const hash = await hashApiKey(apiKey);

      // Assert
      expect(hash).toBeTruthy();
      expect(hash).toHaveLength(64); // BLAKE3 produces 256-bit hash
      expect(hash).toMatch(/^[a-f0-9]+$/i);
    });

    it('should produce same hash for same input', async () => {
      // Arrange
      const apiKey = 'consistent-key-for-testing';

      // Act
      const hash1 = await hashApiKey(apiKey);
      const hash2 = await hashApiKey(apiKey);

      // Assert
      expect(hash1).toBe(hash2);
    });

    it('should produce different hashes for different inputs', async () => {
      // Arrange
      const key1 = 'first-key-12345678';
      const key2 = 'second-key-87654321';

      // Act
      const hash1 = await hashApiKey(key1);
      const hash2 = await hashApiKey(key2);

      // Assert
      expect(hash1).not.toBe(hash2);
    });

    it('should throw TypeError for non-string input', async () => {
      // Arrange & Act & Assert
      await expect(hashApiKey(12345)).rejects.toThrow(TypeError);
      await expect(hashApiKey(null)).rejects.toThrow(TypeError);
      await expect(hashApiKey(undefined)).rejects.toThrow(TypeError);
    });

    it('should throw error for empty string', async () => {
      // Arrange & Act & Assert
      await expect(hashApiKey('')).rejects.toThrow('cannot be empty');
    });
  });

  describe('verifyApiKey()', () => {
    it('should verify valid API key against hash', async () => {
      // Arrange
      const apiKey = 'my-secret-key-123456789abcdef';
      const hash = await hashApiKey(apiKey);

      // Act
      const isValid = await verifyApiKey(apiKey, hash);

      // Assert
      expect(isValid).toBe(true);
    });

    it('should reject invalid API key', async () => {
      // Arrange
      const correctKey = 'correct-key-abcdef123456';
      const wrongKey = 'wrong-key-fedcba654321';
      const hash = await hashApiKey(correctKey);

      // Act
      const isValid = await verifyApiKey(wrongKey, hash);

      // Assert
      expect(isValid).toBe(false);
    });

    it('should use constant-time comparison', async () => {
      // Arrange
      const key = 'timing-safe-key-12345678';
      const hash = await hashApiKey(key);

      // Act
      const startTime = Date.now();
      await verifyApiKey(key, hash);
      const validTime = Date.now() - startTime;

      const startTime2 = Date.now();
      await verifyApiKey('wrong-key-87654321abcdef', hash);
      const invalidTime = Date.now() - startTime2;

      // Assert - timing should be similar (within 50ms)
      const timingDiff = Math.abs(validTime - invalidTime);
      expect(timingDiff).toBeLessThan(50);
    });

    it('should throw TypeError for non-string arguments', async () => {
      // Arrange
      const hash = await hashApiKey('test-key');

      // Act & Assert
      await expect(verifyApiKey(123, hash)).rejects.toThrow(TypeError);
      await expect(verifyApiKey('test', 123)).rejects.toThrow(TypeError);
    });
  });

  describe('generateApiKeyPair()', () => {
    it('should generate key and hash pair', async () => {
      // Arrange & Act
      const { key, hash } = await generateApiKeyPair();

      // Assert
      expect(key).toBeTruthy();
      expect(hash).toBeTruthy();
      expect(key).toHaveLength(64);
      expect(hash).toHaveLength(64);
    });

    it('should generate verifiable key-hash pair', async () => {
      // Arrange & Act
      const { key, hash } = await generateApiKeyPair();

      // Assert
      const isValid = await verifyApiKey(key, hash);
      expect(isValid).toBe(true);
    });

    it('should generate unique pairs on each call', async () => {
      // Arrange & Act
      const pair1 = await generateApiKeyPair();
      const pair2 = await generateApiKeyPair();

      // Assert
      expect(pair1.key).not.toBe(pair2.key);
      expect(pair1.hash).not.toBe(pair2.hash);
    });
  });
});

describe('ApiKeySchema', () => {
  it('should validate valid API key', () => {
    // Arrange
    const validKey = 'a'.repeat(64); // 64 char hex string

    // Act
    const result = ApiKeySchema.safeParse(validKey);

    // Assert
    expect(result.success).toBe(true);
  });

  it('should reject key shorter than 32 characters', () => {
    // Arrange
    const shortKey = 'abc123'; // Too short

    // Act
    const result = ApiKeySchema.safeParse(shortKey);

    // Assert
    expect(result.success).toBe(false);
  });

  it('should reject key longer than 128 characters', () => {
    // Arrange
    const longKey = 'a'.repeat(129);

    // Act
    const result = ApiKeySchema.safeParse(longKey);

    // Assert
    expect(result.success).toBe(false);
  });

  it('should reject non-hexadecimal characters', () => {
    // Arrange
    const invalidKey = 'z'.repeat(64); // z is not hex

    // Act
    const result = ApiKeySchema.safeParse(invalidKey);

    // Assert
    expect(result.success).toBe(false);
  });
});

describe('ApiKeyAuthenticator', () => {
  let authenticator;
  let validKey;
  let validHash;

  beforeEach(async () => {
    const pair = await generateApiKeyPair();
    validKey = pair.key;
    validHash = pair.hash;

    authenticator = new ApiKeyAuthenticator({
      storedKeyHash: validHash,
      environment: 'development',
    });
  });

  describe('constructor', () => {
    it('should create authenticator with default options', () => {
      // Arrange & Act
      const auth = new ApiKeyAuthenticator();

      // Assert
      // In test environment, NODE_ENV is 'test', not 'development'
      expect(auth.environment).toBe('test');
      expect(auth.requireInDev).toBe(false);
      expect(auth.storedKeyHash).toBeNull();
    });

    it('should create authenticator with custom options', () => {
      // Arrange & Act
      const auth = new ApiKeyAuthenticator({
        storedKeyHash: validHash,
        environment: 'production',
        requireInDev: true,
      });

      // Assert
      expect(auth.storedKeyHash).toBe(validHash);
      expect(auth.environment).toBe('production');
      expect(auth.requireInDev).toBe(true);
    });
  });

  describe('setStoredHash()', () => {
    it('should set stored hash', () => {
      // Arrange
      const auth = new ApiKeyAuthenticator();

      // Act
      auth.setStoredHash(validHash);

      // Assert
      expect(auth.storedKeyHash).toBe(validHash);
    });

    it('should throw error for invalid hash', () => {
      // Arrange
      const auth = new ApiKeyAuthenticator();

      // Act & Assert
      expect(() => auth.setStoredHash('short')).toThrow('Invalid API key hash');
      expect(() => auth.setStoredHash(123)).toThrow('Invalid API key hash');
    });
  });

  describe('initialize()', () => {
    it('should generate and store new API key pair', async () => {
      // Arrange
      const auth = new ApiKeyAuthenticator();

      // Act
      const { key, hash } = await auth.initialize();

      // Assert
      expect(key).toBeTruthy();
      expect(hash).toBeTruthy();
      expect(auth.storedKeyHash).toBe(hash);
    });
  });

  describe('authenticate() - success cases', () => {
    it('should authenticate valid API key from header', async () => {
      // Arrange
      const context = {
        headers: { 'x-api-key': validKey },
      };

      // Act
      const result = await authenticator.authenticate(context);

      // Assert
      expect(result.authenticated).toBe(true);
      expect(result.source).toBe('header');
      expect(result.environment).toBe('development');
    });

    it('should authenticate valid API key from environment', async () => {
      // Arrange
      const context = {
        headers: {},
        env: { UNRDF_API_KEY: validKey },
      };

      // Act
      const result = await authenticator.authenticate(context);

      // Assert
      expect(result.authenticated).toBe(true);
      expect(result.source).toBe('env');
    });

    it('should prioritize header over environment', async () => {
      // Arrange
      const context = {
        headers: { 'x-api-key': validKey },
        env: { UNRDF_API_KEY: 'different-key-12345678' },
      };

      // Act
      const result = await authenticator.authenticate(context);

      // Assert
      expect(result.authenticated).toBe(true);
      expect(result.source).toBe('header');
    });
  });

  describe('authenticate() - failure cases', () => {
    it('should reject invalid API key format', async () => {
      // Arrange
      const context = {
        headers: { 'x-api-key': 'invalid' },
      };

      // Act & Assert
      await expect(authenticator.authenticate(context)).rejects.toThrow('Invalid API key format');
    });

    it('should reject wrong API key', async () => {
      // Arrange
      const wrongKey = await generateSecureApiKey();
      const context = {
        headers: { 'x-api-key': wrongKey },
      };

      // Act & Assert
      await expect(authenticator.authenticate(context)).rejects.toThrow('Invalid API key');
    });

    it('should throw error if no hash configured', async () => {
      // Arrange
      const auth = new ApiKeyAuthenticator({ environment: 'production' });
      const context = {
        headers: { 'x-api-key': validKey },
      };

      // Act & Assert
      await expect(auth.authenticate(context)).rejects.toThrow('No stored API key hash');
    });
  });

  describe('authenticate() - environment handling', () => {
    it('should allow missing key in development with warning', async () => {
      // Arrange
      const mockLogger = { warn: vi.fn() };
      const auth = new ApiKeyAuthenticator({
        storedKeyHash: validHash,
        environment: 'development',
        logger: mockLogger,
      });

      // Act
      const result = await auth.authenticate({ headers: {} });

      // Assert
      expect(result.authenticated).toBe(false);
      expect(mockLogger.warn).toHaveBeenCalledWith(
        expect.stringContaining('INSECURE mode')
      );
    });

    it('should reject missing key in production', async () => {
      // Arrange
      const auth = new ApiKeyAuthenticator({
        storedKeyHash: validHash,
        environment: 'production',
      });

      // Act & Assert
      await expect(auth.authenticate({ headers: {} })).rejects.toThrow(
        'API key required in production'
      );
    });

    it('should reject missing key when requireInDev is true', async () => {
      // Arrange
      const auth = new ApiKeyAuthenticator({
        storedKeyHash: validHash,
        environment: 'development',
        requireInDev: true,
      });

      // Act & Assert
      await expect(auth.authenticate({ headers: {} })).rejects.toThrow(
        'API key required'
      );
    });
  });

  describe('getAuditLog()', () => {
    it('should record authentication attempts', async () => {
      // Arrange
      const context = {
        headers: { 'x-api-key': validKey },
      };

      // Act
      await authenticator.authenticate(context);
      const log = authenticator.getAuditLog();

      // Assert
      expect(log).toHaveLength(1);
      expect(log[0].success).toBe(true);
      expect(log[0].reason).toBe('Success');
    });

    it('should record failed attempts', async () => {
      // Arrange
      const wrongKey = await generateSecureApiKey();
      const context = {
        headers: { 'x-api-key': wrongKey },
      };

      // Act
      try {
        await authenticator.authenticate(context);
      } catch {
        // Expected
      }
      const log = authenticator.getAuditLog();

      // Assert
      expect(log).toHaveLength(1);
      expect(log[0].success).toBe(false);
    });

    it('should limit audit log to 1000 entries', async () => {
      // Arrange
      const context = { headers: { 'x-api-key': validKey } };

      // Act - authenticate 1100 times
      for (let i = 0; i < 1100; i++) {
        await authenticator.authenticate(context);
      }
      const log = authenticator.getAuditLog();

      // Assert
      expect(log).toHaveLength(1000);
    });
  });

  describe('clearAuditLog()', () => {
    it('should clear audit log', async () => {
      // Arrange
      await authenticator.authenticate({
        headers: { 'x-api-key': validKey },
      });

      // Act
      authenticator.clearAuditLog();

      // Assert
      expect(authenticator.getAuditLog()).toHaveLength(0);
    });
  });
});

describe('createAuthMiddleware()', () => {
  it('should create middleware function', async () => {
    // Arrange
    const { key, hash } = await generateApiKeyPair();
    const authenticator = new ApiKeyAuthenticator({
      storedKeyHash: hash,
      environment: 'development',
    });

    // Act
    const middleware = createAuthMiddleware(authenticator);

    // Assert
    expect(middleware).toBeInstanceOf(Function);
  });

  it('should authenticate requests via middleware', async () => {
    // Arrange
    const { key, hash } = await generateApiKeyPair();
    const authenticator = new ApiKeyAuthenticator({
      storedKeyHash: hash,
    });
    const middleware = createAuthMiddleware(authenticator);

    // Act
    const result = await middleware({
      headers: { 'x-api-key': key },
    });

    // Assert
    expect(result.authenticated).toBe(true);
  });

  it('should throw in production if not authenticated', async () => {
    // Arrange
    const authenticator = new ApiKeyAuthenticator({
      environment: 'production',
    });
    const middleware = createAuthMiddleware(authenticator);

    // Act & Assert
    await expect(middleware({ headers: {} })).rejects.toThrow();
  });
});

describe('createAuthenticator()', () => {
  it('should create and initialize authenticator', async () => {
    // Arrange & Act
    const { authenticator, key } = await createAuthenticator({
      environment: 'production',
    });

    // Assert
    expect(authenticator).toBeInstanceOf(ApiKeyAuthenticator);
    expect(key).toBeTruthy();
    expect(authenticator.storedKeyHash).toBeTruthy();
  });

  it('should create authenticator with valid key pair', async () => {
    // Arrange & Act
    const { authenticator, key } = await createAuthenticator();

    // Assert
    const result = await authenticator.authenticate({
      headers: { 'x-api-key': key },
    });
    expect(result.authenticated).toBe(true);
  });
});
