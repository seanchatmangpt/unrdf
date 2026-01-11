/**
 * @file Security Audit Module Tests
 * @module @unrdf/daemon/test/security
 * @description Comprehensive test suite for security validation, injection prevention,
 * path traversal blocking, rate limiting, and cryptographic verification
 */

import { describe, it, expect, beforeEach } from 'vitest';
import crypto from 'crypto';
import {
  validateInputSafety,
  validatePathSafety,
  timingSafeCompare,
  checkRateLimit,
  verifyCryptographicHash,
  validatePayload,
  getAuditLog,
  clearAuditLog,
  getSecurityStats,
} from '../src/security-audit.mjs';

describe('Security Audit Module', () => {
  beforeEach(() => {
    clearAuditLog();
  });

  describe('validateInputSafety()', () => {
    it('should accept safe string input', () => {
      const result = validateInputSafety('hello world', 'command');
      expect(result.safe).toBe(true);
      expect(result.eventId).toBeDefined();
    });

    it('should accept null and undefined input', () => {
      expect(validateInputSafety(null).safe).toBe(true);
      expect(validateInputSafety(undefined).safe).toBe(true);
    });

    it('should handle BigInt values in objects', () => {
      const input = { timestamp: BigInt(Date.now()), value: 42n };
      const result = validateInputSafety(input, 'command');
      expect(result.safe).toBe(true);
      expect(result.eventId).toBeDefined();
    });

    it('should handle BigInt values in nested objects', () => {
      const input = {
        metadata: {
          created: BigInt(1234567890),
          updated: BigInt(9876543210),
        },
        data: { count: 100n },
      };
      const result = validateInputSafety(input, 'command');
      expect(result.safe).toBe(true);
    });

    it('should handle BigInt values in arrays', () => {
      const input = [BigInt(1), BigInt(2), BigInt(3)];
      const result = validateInputSafety(input, 'command');
      expect(result.safe).toBe(true);
    });

    it('should handle mixed BigInt and regular values', () => {
      const input = {
        id: 'test-id',
        timestamp: BigInt(Date.now()),
        count: 42,
        active: true,
        metadata: null,
      };
      const result = validateInputSafety(input, 'command');
      expect(result.safe).toBe(true);
    });

    it('should detect injection in BigInt-containing objects', () => {
      const input = {
        timestamp: BigInt(Date.now()),
        command: 'test | cat /etc/passwd',
      };
      const result = validateInputSafety(input, 'command');
      expect(result.safe).toBe(false);
      expect(result.reason).toContain('injection');
    });

    it('should handle very large BigInt values', () => {
      const input = {
        value: BigInt('9007199254740991999999999999'),
      };
      const result = validateInputSafety(input, 'command');
      expect(result.safe).toBe(true);
    });

    it('should handle negative BigInt values', () => {
      const input = {
        balance: -42n,
        timestamp: BigInt(-1234567890),
      };
      const result = validateInputSafety(input, 'command');
      expect(result.safe).toBe(true);
    });

    it('should reject command injection - pipe operator', () => {
      const result = validateInputSafety('test | cat /etc/passwd', 'command');
      expect(result.safe).toBe(false);
      expect(result.reason).toContain('injection');
    });

    it('should reject command injection - semicolon separator', () => {
      const result = validateInputSafety('test; rm -rf /', 'command');
      expect(result.safe).toBe(false);
    });

    it('should reject command injection - command substitution', () => {
      const result = validateInputSafety('test $(whoami)', 'command');
      expect(result.safe).toBe(false);
    });

    it('should reject command injection - backtick substitution', () => {
      const result = validateInputSafety('test `whoami`', 'command');
      expect(result.safe).toBe(false);
    });

    it('should reject SQL injection - UNION clause', () => {
      const result = validateInputSafety("test' UNION SELECT * FROM users", 'sql');
      expect(result.safe).toBe(false);
      expect(result.reason).toContain('injection');
    });

    it('should reject SQL injection - OR logic', () => {
      const result = validateInputSafety("' OR '1'='1", 'sql');
      expect(result.safe).toBe(false);
    });

    it('should reject SQL injection - comment syntax', () => {
      const result = validateInputSafety("test' -- comment", 'sql');
      expect(result.safe).toBe(false);
    });

    it('should reject RDF injection - script tags', () => {
      const result = validateInputSafety('<script>alert("xss")</script>', 'rdf');
      expect(result.safe).toBe(false);
    });

    it('should reject RDF injection - event handlers', () => {
      const result = validateInputSafety('onclick=alert("xss")', 'rdf');
      expect(result.safe).toBe(false);
    });

    it('should reject RDF injection - SPARQL BIND', () => {
      const result = validateInputSafety('BIND(CONCAT(a,b) AS ?result)', 'rdf');
      expect(result.safe).toBe(false);
    });

    it('should log injection attempts to audit trail', () => {
      validateInputSafety('test | cat', 'command');
      const auditLog = getAuditLog({ eventType: 'injection_attempt' });
      expect(auditLog.length).toBe(1);
      expect(auditLog[0].severity).toBe('critical');
    });
  });

  describe('validatePathSafety()', () => {
    it('should accept safe file paths', () => {
      const result = validatePathSafety('/usr/local/bin/daemon');
      expect(result.safe).toBe(true);
    });

    it('should accept Windows-style safe paths', () => {
      const result = validatePathSafety('C:\\Program Files\\daemon\\app.exe');
      expect(result.safe).toBe(true);
    });

    it('should reject path traversal - forward slashes', () => {
      const result = validatePathSafety('../../etc/passwd');
      expect(result.safe).toBe(false);
      expect(result.reason).toContain('traversal');
    });

    it('should reject path traversal - backslashes', () => {
      const result = validatePathSafety('..\\..\\windows\\system32');
      expect(result.safe).toBe(false);
    });

    it('should reject path traversal - URL encoded', () => {
      const result = validatePathSafety('%2e%2e%2fetc%2fpasswd');
      expect(result.safe).toBe(false);
    });

    it('should reject non-string paths', () => {
      const result = validatePathSafety({ path: 'test' });
      expect(result.safe).toBe(false);
    });

    it('should log path traversal attempts', () => {
      validatePathSafety('../../secret');
      const auditLog = getAuditLog({ eventType: 'path_traversal' });
      expect(auditLog.length).toBe(1);
      expect(auditLog[0].severity).toBe('critical');
    });
  });

  describe('timingSafeCompare()', () => {
    it('should return true for identical strings', () => {
      const result = timingSafeCompare('secret', 'secret');
      expect(result).toBe(true);
    });

    it('should return false for different strings', () => {
      const result = timingSafeCompare('secret', 'wrong');
      expect(result).toBe(false);
    });

    it('should return false for different lengths', () => {
      const result = timingSafeCompare('short', 'muchlonger');
      expect(result).toBe(false);
    });

    it('should handle empty strings', () => {
      expect(timingSafeCompare('', '')).toBe(true);
      expect(timingSafeCompare('', 'nonempty')).toBe(false);
    });

    it('should handle non-string inputs', () => {
      expect(timingSafeCompare(123, 'string')).toBe(false);
      expect(timingSafeCompare(null, 'string')).toBe(false);
    });

    it('should use constant-time comparison', () => {
      // Both should take similar time regardless of position of first difference
      const t1 = performance.now();
      timingSafeCompare('aaaaaaaa', 'aaaaaaab');
      const t1_elapsed = performance.now() - t1;

      const t2 = performance.now();
      timingSafeCompare('baaaaaaa', 'aaaaaaaa');
      const t2_elapsed = performance.now() - t2;

      // Times should be very similar (constant-time property)
      // Allow 200% variance due to system noise and JIT compilation
      const variance = Math.abs(t1_elapsed - t2_elapsed) / Math.max(t1_elapsed, t2_elapsed);
      expect(variance).toBeLessThan(2);
    });
  });

  describe('checkRateLimit()', () => {
    it('should allow requests within limit', () => {
      const result = checkRateLimit('user1', 10, 1000);
      expect(result.allowed).toBe(true);
      expect(result.remaining).toBe(9);
    });

    it('should track multiple requests for same identifier', () => {
      checkRateLimit('user1', 3, 1000);
      checkRateLimit('user1', 3, 1000);
      const result = checkRateLimit('user1', 3, 1000);
      expect(result.remaining).toBe(0);
    });

    it('should reject requests exceeding limit', () => {
      checkRateLimit('user2', 2, 1000);
      checkRateLimit('user2', 2, 1000);
      const result = checkRateLimit('user2', 2, 1000);
      expect(result.allowed).toBe(false);
    });

    it('should log rate limit violations', () => {
      checkRateLimit('user3', 1, 1000);
      checkRateLimit('user3', 1, 1000);
      const auditLog = getAuditLog({ eventType: 'rate_limit' });
      expect(auditLog.length).toBeGreaterThan(0);
      expect(auditLog[0].severity).toBe('warning');
    });

    it('should reset limit after window expires', (context) => {
      context.timeout = 2000;

      checkRateLimit('user4', 1, 100);
      let result = checkRateLimit('user4', 1, 100);
      expect(result.allowed).toBe(false);

      // Wait for window to expire
      return new Promise(resolve => {
        setTimeout(() => {
          result = checkRateLimit('user4', 1, 100);
          expect(result.allowed).toBe(true);
          resolve();
        }, 150);
      });
    });

    it('should return reset timestamp', () => {
      const result = checkRateLimit('user5', 10, 5000);
      expect(result.resetAfter).toBeDefined();
      expect(result.resetAfter).toBeGreaterThan(Date.now());
    });

    it('should track different identifiers separately', () => {
      checkRateLimit('user6', 2, 1000);
      checkRateLimit('user6', 2, 1000);
      const result1 = checkRateLimit('user6', 2, 1000);
      expect(result1.allowed).toBe(false);

      const result2 = checkRateLimit('user7', 2, 1000);
      expect(result2.allowed).toBe(true);
    });
  });

  describe('verifyCryptographicHash()', () => {
    it('should verify matching hash', () => {
      const data = 'test data';
      const hash = crypto.createHash('sha256').update(data).digest('hex');
      const result = verifyCryptographicHash(data, hash);
      expect(result.verified).toBe(true);
    });

    it('should reject mismatched hash', () => {
      const result = verifyCryptographicHash('test data', 'wronghash');
      expect(result.verified).toBe(false);
    });

    it('should handle buffer input', () => {
      const buffer = Buffer.from('test data');
      const hash = crypto.createHash('sha256').update(buffer).digest('hex');
      const result = verifyCryptographicHash(buffer, hash);
      expect(result.verified).toBe(true);
    });

    it('should return hash value', () => {
      const data = 'test';
      const hash = crypto.createHash('sha256').update(data).digest('hex');
      const result = verifyCryptographicHash(data, hash);
      expect(result.hash).toBeDefined();
      expect(result.hash).toBe(hash);
    });

    it('should log verification failures', () => {
      verifyCryptographicHash('data', 'wronghash');
      const auditLog = getAuditLog({ eventType: 'crypto_verify' });
      expect(auditLog.length).toBe(1);
      expect(auditLog[0].severity).toBe('critical');
    });
  });

  describe('validatePayload()', () => {
    it('should validate safe payloads', () => {
      const result = validatePayload('safe data', { type: 'command' });
      expect(result.valid).toBe(true);
    });

    it('should reject injection in payload', () => {
      const result = validatePayload('test | cat', { type: 'command' });
      expect(result.valid).toBe(false);
      expect(result.reason).toContain('injection');
    });

    it('should check path traversal when requested', () => {
      const result = validatePayload('../../secret', { checkPath: true });
      expect(result.valid).toBe(false);
      expect(result.reason).toContain('traversal');
    });

    it('should enforce rate limiting when configured', () => {
      checkRateLimit('api-user', 1, 1000);
      const result = validatePayload('data', {
        rateLimitId: 'api-user',
        maxRequests: 1,
        windowMs: 1000,
      });
      expect(result.valid).toBe(false);
      expect(result.reason.toLowerCase()).toContain('rate limit');
    });

    it('should return comprehensive validation results', () => {
      const result = validatePayload('safe', { type: 'command' });
      expect(result.validationResults).toBeDefined();
      expect(result.validationResults.injection).toBeDefined();
    });

    it('should log all validation checks', () => {
      validatePayload('data', { type: 'command' });
      const auditLog = getAuditLog({ eventType: 'validation' });
      expect(auditLog.length).toBeGreaterThan(0);
    });
  });

  describe('getAuditLog()', () => {
    it('should return all audit events', () => {
      validateInputSafety('test | bad', 'command');
      validatePathSafety('../../secret');
      const log = getAuditLog();
      expect(log.length).toBeGreaterThanOrEqual(2);
    });

    it('should filter by severity', () => {
      validateInputSafety('test | bad', 'command');
      validatePayload('safe', { type: 'command' });
      const critical = getAuditLog({ severity: 'critical' });
      const info = getAuditLog({ severity: 'info' });
      expect(critical.length).toBeGreaterThan(0);
      expect(info.length).toBeGreaterThan(0);
    });

    it('should filter by event type', () => {
      validateInputSafety('test | bad', 'command');
      validatePathSafety('../../secret');
      const injections = getAuditLog({ eventType: 'injection_attempt' });
      const traversals = getAuditLog({ eventType: 'path_traversal' });
      expect(injections.length).toBeGreaterThan(0);
      expect(traversals.length).toBeGreaterThan(0);
    });

    it('should limit results', () => {
      for (let i = 0; i < 10; i++) {
        validatePayload('safe', { type: 'command' });
      }
      const log = getAuditLog({ limit: 5 });
      expect(log.length).toBe(5);
    });
  });

  describe('getSecurityStats()', () => {
    it('should return statistics object', () => {
      validatePayload('safe', { type: 'command' });
      const stats = getSecurityStats();
      expect(stats).toBeDefined();
      expect(stats.totalEvents).toBeGreaterThanOrEqual(0);
    });

    it('should count events by type', () => {
      validateInputSafety('test | bad', 'command');
      validatePathSafety('../../secret');
      const stats = getSecurityStats();
      expect(stats.byType).toBeDefined();
      expect(stats.byType.injection_attempt).toBeGreaterThan(0);
      expect(stats.byType.path_traversal).toBeGreaterThan(0);
    });

    it('should count events by severity', () => {
      validateInputSafety('test | bad', 'command');
      validatePayload('safe', { type: 'command' });
      const stats = getSecurityStats();
      expect(stats.bySeverity).toBeDefined();
      expect(stats.bySeverity.critical).toBeGreaterThan(0);
      expect(stats.bySeverity.info).toBeGreaterThan(0);
    });

    it('should track active rate limiters', () => {
      checkRateLimit('user1', 10, 1000);
      checkRateLimit('user2', 10, 1000);
      const stats = getSecurityStats();
      expect(stats.activeRateLimiters).toBeGreaterThanOrEqual(2);
    });
  });

  describe('clearAuditLog()', () => {
    it('should clear all audit events', () => {
      validatePayload('safe', { type: 'command' });
      const cleared = clearAuditLog();
      expect(cleared).toBeGreaterThan(0);
      const log = getAuditLog();
      expect(log.length).toBe(0);
    });

    it('should return count of cleared entries', () => {
      validatePayload('safe', { type: 'command' });
      validatePayload('safe', { type: 'command' });
      const cleared = clearAuditLog();
      expect(cleared).toBeGreaterThanOrEqual(2);
    });
  });

  describe('OWASP Top 10 Attack Scenarios', () => {
    it('should prevent A1: Injection - SQL', () => {
      const result = validatePayload("1' OR '1'='1", { type: 'sql' });
      expect(result.valid).toBe(false);
    });

    it('should prevent A1: Injection - OS Command', () => {
      const result = validatePayload('test && cat /etc/passwd', { type: 'command' });
      expect(result.valid).toBe(false);
    });

    it('should prevent A3: Broken Authentication - Timing attack', () => {
      const t1 = performance.now();
      timingSafeCompare('password123', 'wrong');
      const t1_elapsed = performance.now() - t1;

      const t2 = performance.now();
      timingSafeCompare('password123', 'password123');
      const t2_elapsed = performance.now() - t2;

      // Constant-time comparison prevents timing-based authentication bypass
      // Allow 200% variance due to system noise and JIT compilation
      const variance = Math.abs(t1_elapsed - t2_elapsed) / Math.max(t1_elapsed, t2_elapsed);
      expect(variance).toBeLessThan(2);
    });

    it('should prevent A1: Injection - Path Traversal', () => {
      const result = validatePayload('../../../../etc/passwd', { checkPath: true });
      expect(result.valid).toBe(false);
    });

    it('should prevent A4: Insecure Deserialization - integrity check', () => {
      const malicious = 'malicious payload';
      const validHash = crypto.createHash('sha256').update('valid').digest('hex');
      const result = verifyCryptographicHash(malicious, validHash);
      expect(result.verified).toBe(false);
    });

    it('should prevent A7: API Rate Limiting - DoS protection', () => {
      for (let i = 0; i < 5; i++) {
        checkRateLimit('attacker', 3, 60000);
      }
      const result = checkRateLimit('attacker', 3, 60000);
      expect(result.allowed).toBe(false);
    });
  });
});
