/**
 * @file Security Headers Middleware Tests
 * @module @unrdf/daemon/test/security-headers
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  SecurityHeadersMiddleware,
  createSecurityMiddleware,
  DEFAULT_SECURITY_CONFIG,
  CSPConfigSchema,
  CORSConfigSchema,
  RequestLimitsSchema,
  RateLimitConfigSchema,
  SecurityHeadersConfigSchema,
} from '../src/middleware/security-headers.mjs';

describe('SecurityHeadersMiddleware', () => {
  let middleware;

  beforeEach(() => {
    middleware = new SecurityHeadersMiddleware();
  });

  afterEach(() => {
    if (middleware) {
      middleware.cleanup();
    }
  });

  describe('Schema Validation', () => {
    it('should validate CSP configuration', () => {
      const config = CSPConfigSchema.parse({
        defaultSrc: ["'self'"],
        scriptSrc: ["'self'", "'unsafe-inline'"],
      });

      expect(config.defaultSrc).toEqual(["'self'"]);
      expect(config.scriptSrc).toEqual(["'self'", "'unsafe-inline'"]);
      expect(config.reportOnly).toBe(false);
    });

    it('should validate CORS configuration', () => {
      const config = CORSConfigSchema.parse({
        origin: 'http://localhost:3000',
        methods: ['GET', 'POST'],
      });

      expect(config.origin).toBe('http://localhost:3000');
      expect(config.methods).toEqual(['GET', 'POST']);
      expect(config.credentials).toBe(false);
    });

    it('should validate request limits configuration', () => {
      const config = RequestLimitsSchema.parse({
        maxBodySize: 5 * 1024 * 1024,
        timeout: 15000,
      });

      expect(config.maxBodySize).toBe(5 * 1024 * 1024);
      expect(config.timeout).toBe(15000);
    });

    it('should validate rate limit configuration', () => {
      const config = RateLimitConfigSchema.parse({
        windowMs: 30000,
        maxRequests: 50,
      });

      expect(config.windowMs).toBe(30000);
      expect(config.maxRequests).toBe(50);
    });

    it('should validate complete security headers configuration', () => {
      const config = SecurityHeadersConfigSchema.parse({
        enableHSTS: true,
        hstsMaxAge: 31536000,
        enableNoSniff: true,
      });

      expect(config.enableHSTS).toBe(true);
      expect(config.hstsMaxAge).toBe(31536000);
      expect(config.enableNoSniff).toBe(true);
    });

    it('should use default values when not specified', () => {
      const config = SecurityHeadersConfigSchema.parse({});

      expect(config.enableHSTS).toBe(true);
      expect(config.enableNoSniff).toBe(true);
      expect(config.enableXFrameOptions).toBe(true);
      expect(config.xFrameOptions).toBe('DENY');
    });
  });

  describe('CSP Header Generation', () => {
    it('should generate basic CSP header', () => {
      middleware = new SecurityHeadersMiddleware({
        csp: {
          defaultSrc: ["'self'"],
          scriptSrc: ["'self'"],
        },
      });

      const header = middleware.generateCSPHeader();
      expect(header).toContain("default-src 'self'");
      expect(header).toContain("script-src 'self'");
    });

    it('should generate CSP header with nonce', () => {
      middleware = new SecurityHeadersMiddleware({
        csp: {
          scriptSrc: ["'self'", "'nonce'"],
        },
      });

      const nonce = 'abc123';
      const header = middleware.generateCSPHeader(nonce);
      expect(header).toContain(`'nonce-${nonce}'`);
    });

    it('should include report-uri in CSP header', () => {
      middleware = new SecurityHeadersMiddleware({
        csp: {
          defaultSrc: ["'self'"],
          reportUri: 'https://example.com/csp-report',
        },
      });

      const header = middleware.generateCSPHeader();
      expect(header).toContain('report-uri https://example.com/csp-report');
    });

    it('should handle all CSP directives', () => {
      middleware = new SecurityHeadersMiddleware({
        csp: {
          defaultSrc: ["'self'"],
          scriptSrc: ["'self'"],
          styleSrc: ["'self'", "'unsafe-inline'"],
          imgSrc: ["'self'", 'data:'],
          connectSrc: ["'self'"],
          fontSrc: ["'self'"],
          objectSrc: ["'none'"],
          mediaSrc: ["'self'"],
          frameSrc: ["'none'"],
        },
      });

      const header = middleware.generateCSPHeader();
      expect(header).toContain('default-src');
      expect(header).toContain('script-src');
      expect(header).toContain('style-src');
      expect(header).toContain('img-src');
      expect(header).toContain('connect-src');
      expect(header).toContain('font-src');
      expect(header).toContain('object-src');
      expect(header).toContain('media-src');
      expect(header).toContain('frame-src');
    });

    it('should generate random nonce', () => {
      const nonce1 = middleware.generateNonce();
      const nonce2 = middleware.generateNonce();

      expect(nonce1).toBeTruthy();
      expect(nonce2).toBeTruthy();
      expect(nonce1).not.toBe(nonce2);
      expect(nonce1.length).toBeGreaterThan(0);
    });
  });

  describe('CORS Handling', () => {
    it('should allow wildcard origin', () => {
      middleware = new SecurityHeadersMiddleware({
        cors: { origin: '*' },
      });

      const allowed = middleware.checkCORSOrigin('http://example.com');
      expect(allowed).toBe(true);
    });

    it('should allow specific origin', () => {
      middleware = new SecurityHeadersMiddleware({
        cors: { origin: 'http://localhost:3000' },
      });

      expect(middleware.checkCORSOrigin('http://localhost:3000')).toBe(true);
      expect(middleware.checkCORSOrigin('http://example.com')).toBe(false);
    });

    it('should allow multiple origins from array', () => {
      middleware = new SecurityHeadersMiddleware({
        cors: {
          origin: ['http://localhost:3000', 'http://localhost:8080'],
        },
      });

      expect(middleware.checkCORSOrigin('http://localhost:3000')).toBe(true);
      expect(middleware.checkCORSOrigin('http://localhost:8080')).toBe(true);
      expect(middleware.checkCORSOrigin('http://example.com')).toBe(false);
    });

    it('should allow origin from function', () => {
      middleware = new SecurityHeadersMiddleware({
        cors: {
          origin: (origin) => origin.startsWith('http://localhost'),
        },
      });

      expect(middleware.checkCORSOrigin('http://localhost:3000')).toBe(true);
      expect(middleware.checkCORSOrigin('http://localhost:8080')).toBe(true);
      expect(middleware.checkCORSOrigin('http://example.com')).toBe(false);
    });

    it('should apply CORS headers to response', () => {
      middleware = new SecurityHeadersMiddleware({
        cors: {
          origin: 'http://localhost:3000',
          credentials: true,
          methods: ['GET', 'POST'],
          allowedHeaders: ['Content-Type'],
        },
      });

      const request = {
        headers: { origin: 'http://localhost:3000' },
        method: 'GET',
      };
      const response = {};

      middleware.applyCORSHeaders(request, response);

      expect(response.headers['Access-Control-Allow-Origin']).toBe('http://localhost:3000');
      expect(response.headers['Access-Control-Allow-Credentials']).toBe('true');
    });

    it('should handle preflight requests', () => {
      middleware = new SecurityHeadersMiddleware({
        cors: {
          origin: '*',
          methods: ['GET', 'POST', 'PUT', 'DELETE'],
          allowedHeaders: ['Content-Type', 'Authorization'],
          maxAge: 86400,
        },
      });

      const request = {
        headers: { origin: 'http://example.com' },
        method: 'OPTIONS',
      };
      const response = {};

      middleware.applyCORSHeaders(request, response);

      expect(response.headers['Access-Control-Allow-Methods']).toBe('GET, POST, PUT, DELETE');
      expect(response.headers['Access-Control-Allow-Headers']).toBe('Content-Type, Authorization');
      expect(response.headers['Access-Control-Max-Age']).toBe('86400');
    });

    it('should expose headers when configured', () => {
      middleware = new SecurityHeadersMiddleware({
        cors: {
          origin: '*',
          exposedHeaders: ['X-Custom-Header', 'X-Another-Header'],
        },
      });

      const request = {
        headers: { origin: 'http://example.com' },
        method: 'OPTIONS',
      };
      const response = {};

      middleware.applyCORSHeaders(request, response);

      expect(response.headers['Access-Control-Expose-Headers']).toBe('X-Custom-Header, X-Another-Header');
    });
  });

  describe('Rate Limiting', () => {
    it('should allow requests within limit', () => {
      middleware = new SecurityHeadersMiddleware({
        rateLimit: {
          windowMs: 60000,
          maxRequests: 10,
        },
      });

      const result = middleware.checkRateLimit('test-key');
      expect(result.allowed).toBe(true);
      expect(result.remaining).toBe(9);
      expect(result.limit).toBe(10);
    });

    it('should block requests exceeding limit', () => {
      middleware = new SecurityHeadersMiddleware({
        rateLimit: {
          windowMs: 60000,
          maxRequests: 3,
        },
      });

      // Make 3 requests (should all pass)
      for (let i = 0; i < 3; i++) {
        const result = middleware.checkRateLimit('test-key');
        expect(result.allowed).toBe(true);
      }

      // 4th request should be blocked
      const result = middleware.checkRateLimit('test-key');
      expect(result.allowed).toBe(false);
      expect(result.remaining).toBe(0);
    });

    it('should reset rate limit after window expires', async () => {
      middleware = new SecurityHeadersMiddleware({
        rateLimit: {
          windowMs: 100, // 100ms window
          maxRequests: 2,
        },
      });

      // Use up the limit
      middleware.checkRateLimit('test-key');
      middleware.checkRateLimit('test-key');

      const blocked = middleware.checkRateLimit('test-key');
      expect(blocked.allowed).toBe(false);

      // Wait for window to reset
      await new Promise(resolve => setTimeout(resolve, 150));

      const allowed = middleware.checkRateLimit('test-key');
      expect(allowed.allowed).toBe(true);
    });

    it('should track different keys separately', () => {
      middleware = new SecurityHeadersMiddleware({
        rateLimit: {
          windowMs: 60000,
          maxRequests: 2,
        },
      });

      middleware.checkRateLimit('key1');
      middleware.checkRateLimit('key1');
      const key1Result = middleware.checkRateLimit('key1');
      expect(key1Result.allowed).toBe(false);

      const key2Result = middleware.checkRateLimit('key2');
      expect(key2Result.allowed).toBe(true);
    });

    it('should cleanup expired rate limits', async () => {
      middleware = new SecurityHeadersMiddleware({
        rateLimit: {
          windowMs: 50,
          maxRequests: 5,
        },
      });

      middleware.checkRateLimit('key1');
      middleware.checkRateLimit('key2');

      expect(middleware.rateLimitStore.size).toBe(2);

      await new Promise(resolve => setTimeout(resolve, 100));
      middleware.cleanupRateLimits();

      expect(middleware.rateLimitStore.size).toBe(0);
    });
  });

  describe('Input Sanitization', () => {
    it('should sanitize SQL injection patterns', () => {
      const input = "SELECT * FROM users WHERE id = 1; DROP TABLE users;--";
      const sanitized = middleware.sanitizeInput(input);

      expect(sanitized).not.toContain('SELECT');
      expect(sanitized).not.toContain('DROP');
      expect(sanitized).not.toContain('--');
    });

    it('should sanitize XSS patterns', () => {
      const input = '<script>alert("XSS")</script>';
      const sanitized = middleware.sanitizeInput(input);

      expect(sanitized).not.toContain('<script>');
      expect(sanitized).not.toContain('</script>');
      expect(sanitized).not.toContain('javascript:');
    });

    it('should sanitize path traversal patterns', () => {
      const input = '../../../etc/passwd';
      const sanitized = middleware.sanitizeInput(input);

      expect(sanitized).not.toContain('..');
    });

    it('should sanitize command injection patterns', () => {
      const input = 'file.txt; rm -rf /';
      const sanitized = middleware.sanitizeInput(input);

      expect(sanitized).not.toContain(';');
      expect(sanitized).toBeTruthy();
    });

    it('should trim whitespace', () => {
      const input = '  test  ';
      const sanitized = middleware.sanitizeInput(input);

      expect(sanitized).toBe('test');
    });

    it('should limit string length', () => {
      const input = 'a'.repeat(1000);
      const sanitized = middleware.sanitizeInput(input, { maxLength: 100 });

      expect(sanitized.length).toBe(100);
    });

    it('should sanitize object properties', () => {
      const obj = {
        name: '<script>alert("XSS")</script>',
        query: "SELECT * FROM users",
        path: '../../../etc/passwd',
      };

      const sanitized = middleware.sanitizeObject(obj);

      expect(sanitized.name).not.toContain('<script>');
      expect(sanitized.query).not.toContain('SELECT');
      expect(sanitized.path).not.toContain('..');
    });

    it('should sanitize nested objects', () => {
      const obj = {
        user: {
          name: '<script>XSS</script>',
          data: {
            query: "DROP TABLE users",
          },
        },
      };

      const sanitized = middleware.sanitizeObject(obj);

      expect(sanitized.user.name).not.toContain('<script>');
      expect(sanitized.user.data.query).not.toContain('DROP');
    });

    it('should sanitize arrays', () => {
      const arr = [
        'alert("bad")',
        'SELECT * FROM users',
        '../../../etc/passwd',
      ];

      const sanitized = middleware.sanitizeObject(arr);

      // String content should remain (no dangerous patterns here)
      expect(sanitized[0]).toBeTruthy();
      expect(sanitized[1]).not.toContain('SELECT');
      expect(sanitized[2]).not.toContain('..');
    });

    it('should skip sanitization when disabled', () => {
      const input = "SELECT * FROM users";
      const sanitized = middleware.sanitizeInput(input, { checkSQL: false });

      expect(sanitized).toContain('SELECT');
    });

    it('should preserve non-string values', () => {
      const obj = {
        name: 'test',
        age: 25,
        active: true,
        data: null,
      };

      const sanitized = middleware.sanitizeObject(obj);

      expect(sanitized.age).toBe(25);
      expect(sanitized.active).toBe(true);
      expect(sanitized.data).toBe(null);
    });
  });

  describe('Request Limits', () => {
    it('should validate request body size', () => {
      middleware = new SecurityHeadersMiddleware({
        requestLimits: {
          maxBodySize: 1024, // 1KB
        },
      });

      const request = {
        body: { data: 'x'.repeat(2000) },
      };

      const result = middleware.checkRequestLimits(request);
      expect(result.valid).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
    });

    it('should validate URL length', () => {
      middleware = new SecurityHeadersMiddleware({
        requestLimits: {
          maxUrlLength: 100,
        },
      });

      const request = {
        url: 'x'.repeat(200),
      };

      const result = middleware.checkRequestLimits(request);
      expect(result.valid).toBe(false);
      expect(result.errors[0]).toContain('URL length');
    });

    it('should validate header size', () => {
      middleware = new SecurityHeadersMiddleware({
        requestLimits: {
          maxHeaderSize: 100,
        },
      });

      const request = {
        headers: {
          'x-custom': 'x'.repeat(200),
        },
      };

      const result = middleware.checkRequestLimits(request);
      expect(result.valid).toBe(false);
      expect(result.errors[0]).toContain('Header size');
    });

    it('should pass valid request', () => {
      middleware = new SecurityHeadersMiddleware({
        requestLimits: {
          maxBodySize: 1024 * 1024,
          maxUrlLength: 2048,
          maxHeaderSize: 8192,
        },
      });

      const request = {
        body: { data: 'test' },
        url: '/api/test',
        headers: { 'content-type': 'application/json' },
      };

      const result = middleware.checkRequestLimits(request);
      expect(result.valid).toBe(true);
      expect(result.errors.length).toBe(0);
    });
  });

  describe('Security Headers', () => {
    it('should apply HSTS header', () => {
      middleware = new SecurityHeadersMiddleware({
        enableHSTS: true,
        hstsMaxAge: 31536000,
      });

      const response = {};
      middleware.applySecurityHeaders(response);

      expect(response.headers['Strict-Transport-Security']).toBe('max-age=31536000; includeSubDomains');
    });

    it('should apply X-Content-Type-Options header', () => {
      middleware = new SecurityHeadersMiddleware({
        enableNoSniff: true,
      });

      const response = {};
      middleware.applySecurityHeaders(response);

      expect(response.headers['X-Content-Type-Options']).toBe('nosniff');
    });

    it('should apply X-Frame-Options header', () => {
      middleware = new SecurityHeadersMiddleware({
        enableXFrameOptions: true,
        xFrameOptions: 'DENY',
      });

      const response = {};
      middleware.applySecurityHeaders(response);

      expect(response.headers['X-Frame-Options']).toBe('DENY');
    });

    it('should apply X-XSS-Protection header', () => {
      middleware = new SecurityHeadersMiddleware({
        enableXSSProtection: true,
      });

      const response = {};
      middleware.applySecurityHeaders(response);

      expect(response.headers['X-XSS-Protection']).toBe('1; mode=block');
    });

    it('should apply Referrer-Policy header', () => {
      middleware = new SecurityHeadersMiddleware({
        enableReferrerPolicy: true,
        referrerPolicy: 'strict-origin-when-cross-origin',
      });

      const response = {};
      middleware.applySecurityHeaders(response);

      expect(response.headers['Referrer-Policy']).toBe('strict-origin-when-cross-origin');
    });

    it('should apply Permissions-Policy header', () => {
      middleware = new SecurityHeadersMiddleware({
        enablePermissionsPolicy: true,
      });

      const response = {};
      middleware.applySecurityHeaders(response);

      expect(response.headers['Permissions-Policy']).toBeTruthy();
    });

    it('should apply custom headers', () => {
      middleware = new SecurityHeadersMiddleware({
        customHeaders: {
          'X-Custom-Header': 'custom-value',
          'X-Another-Header': 'another-value',
        },
      });

      const response = {};
      middleware.applySecurityHeaders(response);

      expect(response.headers['X-Custom-Header']).toBe('custom-value');
      expect(response.headers['X-Another-Header']).toBe('another-value');
    });

    it('should apply CSP header when configured', () => {
      middleware = new SecurityHeadersMiddleware({
        csp: {
          defaultSrc: ["'self'"],
        },
      });

      const response = {};
      middleware.applySecurityHeaders(response);

      expect(response.headers['Content-Security-Policy']).toBeTruthy();
      expect(response.cspNonce).toBeTruthy();
    });

    it('should use CSP report-only mode', () => {
      middleware = new SecurityHeadersMiddleware({
        csp: {
          defaultSrc: ["'self'"],
          reportOnly: true,
        },
      });

      const response = {};
      middleware.applySecurityHeaders(response);

      expect(response.headers['Content-Security-Policy-Report-Only']).toBeTruthy();
      expect(response.headers['Content-Security-Policy']).toBeUndefined();
    });
  });

  describe('Timeout Handling', () => {
    it('should execute handler within timeout', async () => {
      const handler = () => Promise.resolve('success');

      const result = await middleware.processWithTimeout('test-1', handler, 1000);
      expect(result).toBe('success');
    });

    it('should reject on timeout', async () => {
      const handler = () => new Promise(resolve => setTimeout(resolve, 200));

      await expect(
        middleware.processWithTimeout('test-2', handler, 100)
      ).rejects.toThrow('Request timeout');
    });

    it('should cleanup timeout on success', async () => {
      const handler = () => Promise.resolve('success');

      await middleware.processWithTimeout('test-3', handler, 1000);
      expect(middleware.requestTimeouts.has('test-3')).toBe(false);
    });

    it('should cleanup timeout on error', async () => {
      const handler = () => Promise.reject(new Error('Test error'));

      await expect(
        middleware.processWithTimeout('test-4', handler, 1000)
      ).rejects.toThrow('Test error');

      expect(middleware.requestTimeouts.has('test-4')).toBe(false);
    });
  });

  describe('Middleware Integration', () => {
    it('should handle complete request flow', async () => {
      middleware = new SecurityHeadersMiddleware({
        cors: { origin: '*' },
        requestLimits: {
          maxBodySize: 1024 * 1024,
        },
      });

      const request = {
        method: 'POST',
        url: '/api/test',
        headers: { origin: 'http://example.com' },
        body: { data: 'test' },
      };
      const response = {};

      let nextCalled = false;
      const next = () => {
        nextCalled = true;
        return Promise.resolve();
      };

      await middleware.handle(request, response, next);

      expect(nextCalled).toBe(true);
      expect(response.headers).toBeTruthy();
      expect(request.sanitizedBody).toBeTruthy();
    });

    it('should handle preflight request', async () => {
      middleware = new SecurityHeadersMiddleware({
        cors: {
          origin: '*',
          methods: ['GET', 'POST'],
        },
      });

      const request = {
        method: 'OPTIONS',
        headers: { origin: 'http://example.com' },
      };
      const response = {};

      await middleware.handle(request, response);

      expect(response.statusCode).toBe(204);
      expect(response.headers['Access-Control-Allow-Methods']).toBeTruthy();
    });

    it('should reject oversized request', async () => {
      middleware = new SecurityHeadersMiddleware({
        requestLimits: {
          maxBodySize: 100,
        },
      });

      const request = {
        body: { data: 'x'.repeat(200) },
      };
      const response = {};

      await middleware.handle(request, response);

      expect(response.statusCode).toBe(413);
      expect(response.body.error).toBe('Request too large');
    });

    it('should reject rate limited request', async () => {
      middleware = new SecurityHeadersMiddleware({
        rateLimit: {
          windowMs: 60000,
          maxRequests: 1,
        },
      });

      const request = {
        ip: '127.0.0.1',
      };
      const response = {};

      // First request should pass
      await middleware.handle(request, { ...response }, () => Promise.resolve());

      // Second request should be rate limited
      await middleware.handle(request, response);

      expect(response.statusCode).toBe(429);
      expect(response.body.error).toBe('Too many requests');
    });

    it('should handle errors gracefully', async () => {
      const request = {};
      const response = {};

      const next = () => {
        throw new Error('Test error');
      };

      await middleware.handle(request, response, next);

      expect(response.statusCode).toBe(500);
      expect(response.body.error).toBe('Internal server error');
    });

    it('should create middleware function', async () => {
      const mw = middleware.middleware();
      expect(typeof mw).toBe('function');

      const request = {};
      const response = {};
      const next = () => Promise.resolve();

      await mw(request, response, next);
      expect(response.headers).toBeTruthy();
    });
  });

  describe('Factory Functions', () => {
    it('should create middleware with default config', () => {
      const mw = createSecurityMiddleware();
      expect(mw).toBeInstanceOf(SecurityHeadersMiddleware);
    });

    it('should create middleware with custom config', () => {
      const mw = createSecurityMiddleware({
        enableHSTS: false,
        xFrameOptions: 'SAMEORIGIN',
      });

      expect(mw.config.enableHSTS).toBe(false);
      expect(mw.config.xFrameOptions).toBe('SAMEORIGIN');
    });

    it('should export DEFAULT_SECURITY_CONFIG', () => {
      expect(DEFAULT_SECURITY_CONFIG).toBeTruthy();
      expect(DEFAULT_SECURITY_CONFIG.csp).toBeTruthy();
      expect(DEFAULT_SECURITY_CONFIG.cors).toBeTruthy();
      expect(DEFAULT_SECURITY_CONFIG.requestLimits).toBeTruthy();
    });
  });

  describe('Cleanup', () => {
    it('should cleanup rate limit store', () => {
      middleware.checkRateLimit('key1');
      middleware.checkRateLimit('key2');

      expect(middleware.rateLimitStore.size).toBeGreaterThan(0);

      middleware.cleanup();

      expect(middleware.rateLimitStore.size).toBe(0);
    });

    it('should cleanup request timeouts', async () => {
      const handler = () => new Promise(() => {}); // Never resolves
      middleware.processWithTimeout('test', handler, 5000).catch(() => {});

      expect(middleware.requestTimeouts.size).toBe(1);

      middleware.cleanup();

      expect(middleware.requestTimeouts.size).toBe(0);
    });
  });
});
