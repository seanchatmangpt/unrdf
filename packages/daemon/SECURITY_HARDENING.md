# Security Hardening Guide - UNRDF Daemon

**Version**: 1.0.0
**Status**: ✅ Production Ready
**Priority**: P1 Critical Security

---

## Overview

This guide provides comprehensive security hardening measures for production deployment of the UNRDF Daemon. It implements defense-in-depth security controls including Content Security Policy (CSP), CORS configuration, request limits, timeout enforcement, input sanitization, and security headers.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Security Features](#security-features)
3. [Configuration](#configuration)
4. [Best Practices](#best-practices)
5. [Security Testing](#security-testing)
6. [Incident Response](#incident-response)
7. [Compliance](#compliance)

---

## Quick Start

### Basic Implementation

```javascript
import { createSecurityMiddleware, DEFAULT_SECURITY_CONFIG } from '@unrdf/daemon';

// Production-ready configuration
const security = createSecurityMiddleware(DEFAULT_SECURITY_CONFIG);

// Apply to your request handler
app.use(security.middleware());
```

### Custom Configuration

```javascript
import { createSecurityMiddleware } from '@unrdf/daemon';

const security = createSecurityMiddleware({
  // Content Security Policy
  csp: {
    defaultSrc: ["'self'"],
    scriptSrc: ["'self'", "https://trusted-cdn.com"],
    styleSrc: ["'self'", "'unsafe-inline'"],
    imgSrc: ["'self'", "data:", "https:"],
    connectSrc: ["'self'", "https://api.example.com"],
    reportUri: 'https://csp-reports.example.com/report',
  },

  // CORS Configuration
  cors: {
    origin: ['https://app.example.com', 'https://admin.example.com'],
    methods: ['GET', 'POST', 'PUT', 'DELETE'],
    allowedHeaders: ['Content-Type', 'Authorization', 'X-API-Key'],
    credentials: true,
    maxAge: 86400,
  },

  // Request Limits
  requestLimits: {
    maxBodySize: 10 * 1024 * 1024, // 10MB
    maxHeaderSize: 8192,            // 8KB
    maxUrlLength: 2048,             // 2KB
    timeout: 30000,                 // 30 seconds
  },

  // Rate Limiting
  rateLimit: {
    windowMs: 60000,      // 1 minute
    maxRequests: 100,     // 100 requests per minute
  },

  // Security Headers
  enableHSTS: true,
  hstsMaxAge: 31536000,
  enableNoSniff: true,
  enableXFrameOptions: true,
  xFrameOptions: 'DENY',
  enableXSSProtection: true,
  referrerPolicy: 'strict-origin-when-cross-origin',
});
```

---

## Security Features

### 1. Content Security Policy (CSP)

CSP prevents XSS attacks by controlling which resources can be loaded and executed.

**Features:**
- Configurable directives for all resource types
- Nonce support for inline scripts/styles
- Report-only mode for testing
- CSP violation reporting

**Example:**

```javascript
const security = createSecurityMiddleware({
  csp: {
    defaultSrc: ["'self'"],
    scriptSrc: ["'self'", "'nonce'"],
    styleSrc: ["'self'", "'unsafe-inline'"], // Only if necessary
    imgSrc: ["'self'", "data:", "https:"],
    connectSrc: ["'self'", "https://api.example.com"],
    fontSrc: ["'self'", "https://fonts.googleapis.com"],
    objectSrc: ["'none'"],
    mediaSrc: ["'self'"],
    frameSrc: ["'none'"],
    reportUri: 'https://csp-reports.example.com/report',
    reportOnly: false, // Set to true for testing
  },
});

// Access nonce in response
app.get('/page', (req, res) => {
  const response = {};
  security.applySecurityHeaders(response);

  // Use nonce in inline scripts
  const html = `
    <script nonce="${response.cspNonce}">
      console.log('Allowed inline script');
    </script>
  `;

  res.send(html);
});
```

**Best Practices:**
- Start with `reportOnly: true` to test without breaking functionality
- Avoid `'unsafe-inline'` and `'unsafe-eval'` when possible
- Use nonces for legitimate inline scripts
- Monitor CSP violation reports
- Gradually tighten policy based on reports

### 2. CORS Configuration

Restrictive Cross-Origin Resource Sharing configuration to prevent unauthorized cross-origin requests.

**Features:**
- String, array, or function-based origin validation
- Method and header whitelisting
- Credentials support
- Preflight request handling
- Exposed headers configuration

**Example:**

```javascript
const security = createSecurityMiddleware({
  cors: {
    // Option 1: Single origin
    origin: 'https://app.example.com',

    // Option 2: Multiple origins
    origin: [
      'https://app.example.com',
      'https://admin.example.com',
      'https://mobile.example.com',
    ],

    // Option 3: Dynamic validation
    origin: (origin) => {
      const allowedPatterns = [
        /^https:\/\/.*\.example\.com$/,
        /^http:\/\/localhost:\d+$/,
      ];
      return allowedPatterns.some(pattern => pattern.test(origin));
    },

    methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
    allowedHeaders: [
      'Content-Type',
      'Authorization',
      'X-API-Key',
      'X-Request-ID',
    ],
    exposedHeaders: ['X-Total-Count', 'X-Rate-Limit-Remaining'],
    credentials: true,
    maxAge: 86400, // 24 hours
  },
});
```

**Best Practices:**
- Never use `origin: '*'` in production with credentials
- Validate origins strictly (whitelist approach)
- Minimize allowed headers
- Use short `maxAge` for frequently changing policies
- Monitor cross-origin requests

### 3. Request Size Limits

Prevent denial-of-service attacks through oversized requests.

**Features:**
- Body size limits
- Header size limits
- URL length limits
- Configurable per-endpoint

**Example:**

```javascript
const security = createSecurityMiddleware({
  requestLimits: {
    maxBodySize: 10 * 1024 * 1024,  // 10MB default
    maxHeaderSize: 8192,             // 8KB
    maxUrlLength: 2048,              // 2KB
    timeout: 30000,                  // 30 seconds
  },
});

// Custom limits for specific endpoints
app.post('/upload', (req, res) => {
  const uploadSecurity = createSecurityMiddleware({
    requestLimits: {
      maxBodySize: 100 * 1024 * 1024, // 100MB for uploads
      timeout: 300000,                 // 5 minutes
    },
  });

  // Use custom security for this endpoint
});
```

**Best Practices:**
- Set limits based on actual requirements
- Use smaller limits by default, larger for specific endpoints
- Monitor rejected requests for tuning
- Log oversized requests for analysis
- Consider streaming for large uploads

### 4. Timeout Enforcement

Automatic request timeout to prevent resource exhaustion.

**Features:**
- Configurable timeout per request
- Automatic cleanup on completion
- Timeout error handling

**Example:**

```javascript
const security = createSecurityMiddleware({
  requestLimits: {
    timeout: 30000, // 30 seconds default
  },
});

// Process with timeout
async function handleRequest(req, res) {
  try {
    await security.processWithTimeout(
      req.id,
      async () => {
        // Your handler logic
        const result = await someAsyncOperation();
        return result;
      },
      30000 // 30 second timeout
    );
  } catch (error) {
    if (error.message.includes('timeout')) {
      res.status(504).json({ error: 'Request timeout' });
    }
  }
}
```

**Best Practices:**
- Set realistic timeouts based on operation complexity
- Use shorter timeouts for simple operations
- Monitor timeout occurrences
- Implement retry logic on client side
- Log slow operations for optimization

### 5. Input Sanitization

Comprehensive input sanitization to prevent injection attacks.

**Features:**
- SQL injection prevention
- XSS attack prevention
- Path traversal prevention
- Command injection prevention
- LDAP injection prevention
- NoSQL injection prevention
- Length limiting
- Recursive object sanitization

**Example:**

```javascript
const security = createSecurityMiddleware();

// Sanitize string input
const userInput = "<script>alert('XSS')</script>";
const sanitized = security.sanitizeInput(userInput);
// Result: "" (script tags removed)

// Sanitize object
const requestData = {
  name: "John'; DROP TABLE users;--",
  email: "test@example.com",
  bio: "<script>steal()</script>",
};

const sanitizedData = security.sanitizeObject(requestData);
// All injection attempts removed

// Custom sanitization options
const customSanitized = security.sanitizeInput(input, {
  checkSQL: true,
  checkXSS: true,
  checkPathTraversal: true,
  checkCommandInjection: true,
  maxLength: 1000,
});

// Automatic sanitization in middleware
app.post('/api/data', (req, res) => {
  // Original body
  console.log(req.body);

  // Sanitized body (automatically created by middleware)
  console.log(req.sanitizedBody);

  // Always use sanitizedBody for processing
  processData(req.sanitizedBody);
});
```

**Best Practices:**
- Always use sanitized input for processing
- Sanitize at entry points (middleware level)
- Don't rely solely on client-side validation
- Validate data types with Zod schemas
- Log rejected inputs for monitoring
- Combine with parameterized queries/prepared statements

### 6. Security Headers

Essential HTTP security headers for defense-in-depth.

**Implemented Headers:**

#### Strict-Transport-Security (HSTS)
```javascript
{
  enableHSTS: true,
  hstsMaxAge: 31536000, // 1 year
}
// Header: Strict-Transport-Security: max-age=31536000; includeSubDomains
```

#### X-Content-Type-Options
```javascript
{
  enableNoSniff: true,
}
// Header: X-Content-Type-Options: nosniff
```

#### X-Frame-Options
```javascript
{
  enableXFrameOptions: true,
  xFrameOptions: 'DENY', // or 'SAMEORIGIN'
}
// Header: X-Frame-Options: DENY
```

#### X-XSS-Protection
```javascript
{
  enableXSSProtection: true,
}
// Header: X-XSS-Protection: 1; mode=block
```

#### Referrer-Policy
```javascript
{
  enableReferrerPolicy: true,
  referrerPolicy: 'strict-origin-when-cross-origin',
}
// Header: Referrer-Policy: strict-origin-when-cross-origin
```

#### Permissions-Policy
```javascript
{
  enablePermissionsPolicy: true,
}
// Header: Permissions-Policy: geolocation=(), microphone=(), camera=()
```

#### Custom Headers
```javascript
{
  customHeaders: {
    'X-Custom-Security': 'enabled',
    'X-Application-Version': '1.0.0',
  },
}
```

**Example:**

```javascript
const security = createSecurityMiddleware({
  enableHSTS: true,
  hstsMaxAge: 31536000,
  enableNoSniff: true,
  enableXFrameOptions: true,
  xFrameOptions: 'DENY',
  enableXSSProtection: true,
  enableReferrerPolicy: true,
  referrerPolicy: 'strict-origin-when-cross-origin',
  enablePermissionsPolicy: true,
  customHeaders: {
    'X-Content-Security-Policy': 'default-src \'self\'',
    'X-Application-Name': 'UNRDF-Daemon',
  },
});
```

**Best Practices:**
- Enable all security headers by default
- Use HSTS with long max-age in production
- Set X-Frame-Options to DENY unless iframes needed
- Monitor for header-related issues
- Test headers with security scanners

### 7. Rate Limiting

Protect against brute force and denial-of-service attacks.

**Features:**
- Configurable window and request limit
- Per-IP or custom key tracking
- Automatic cleanup of expired entries
- Rate limit headers in responses

**Example:**

```javascript
const security = createSecurityMiddleware({
  rateLimit: {
    windowMs: 60000,      // 1 minute window
    maxRequests: 100,     // 100 requests per window

    // Custom key generator
    keyGenerator: (request) => {
      // Rate limit by API key instead of IP
      return request.headers['x-api-key'] || request.ip;
    },

    skipSuccessfulRequests: false,
    skipFailedRequests: false,
  },
});

// Rate limit info in response headers
// X-RateLimit-Limit: 100
// X-RateLimit-Remaining: 95
// X-RateLimit-Reset: 45 (seconds until reset)
```

**Different Limits for Different Endpoints:**

```javascript
// Strict rate limit for authentication
const authSecurity = createSecurityMiddleware({
  rateLimit: {
    windowMs: 300000,  // 5 minutes
    maxRequests: 5,    // 5 attempts per 5 minutes
  },
});

// Standard rate limit for API
const apiSecurity = createSecurityMiddleware({
  rateLimit: {
    windowMs: 60000,   // 1 minute
    maxRequests: 100,  // 100 requests per minute
  },
});

// Relaxed rate limit for public endpoints
const publicSecurity = createSecurityMiddleware({
  rateLimit: {
    windowMs: 60000,   // 1 minute
    maxRequests: 1000, // 1000 requests per minute
  },
});

app.post('/auth/login', authSecurity.middleware(), loginHandler);
app.get('/api/data', apiSecurity.middleware(), dataHandler);
app.get('/public/info', publicSecurity.middleware(), infoHandler);
```

**Best Practices:**
- Set conservative limits initially
- Monitor rate limit violations
- Use different limits for different endpoints
- Consider authenticated vs. unauthenticated limits
- Implement exponential backoff on client side
- Log repeated violations for security monitoring

---

## Configuration

### Environment-Based Configuration

```javascript
// config/security.mjs
export function getSecurityConfig() {
  const env = process.env.NODE_ENV || 'production';

  if (env === 'production') {
    return {
      csp: {
        defaultSrc: ["'self'"],
        scriptSrc: ["'self'"],
        styleSrc: ["'self'"],
        reportOnly: false,
      },
      cors: {
        origin: [
          process.env.APP_URL,
          process.env.ADMIN_URL,
        ],
        credentials: true,
      },
      requestLimits: {
        maxBodySize: 10 * 1024 * 1024,
        timeout: 30000,
      },
      rateLimit: {
        windowMs: 60000,
        maxRequests: 100,
      },
      enableHSTS: true,
      hstsMaxAge: 31536000,
    };
  }

  if (env === 'development') {
    return {
      cors: {
        origin: '*', // Allow all origins in dev
      },
      requestLimits: {
        maxBodySize: 100 * 1024 * 1024, // Larger limits
        timeout: 300000,                 // Longer timeouts
      },
      rateLimit: {
        windowMs: 60000,
        maxRequests: 10000, // Relaxed rate limits
      },
      enableHSTS: false, // Don't enforce HTTPS in dev
    };
  }

  // Staging configuration
  return {
    csp: {
      reportOnly: true, // Test CSP without blocking
    },
    cors: {
      origin: [process.env.STAGING_URL],
    },
    // ... similar to production but more permissive for testing
  };
}
```

### Secrets Management

**Environment Variables:**

```bash
# .env.production
NODE_ENV=production
APP_URL=https://app.example.com
ADMIN_URL=https://admin.example.com
API_KEY_HASH=<your-api-key-hash>
CSP_REPORT_URI=https://csp-reports.example.com/report

# Rate limiting
RATE_LIMIT_WINDOW_MS=60000
RATE_LIMIT_MAX_REQUESTS=100

# Request limits
MAX_BODY_SIZE=10485760
REQUEST_TIMEOUT=30000
```

**Secrets Rotation:**

See [SECURITY_INCIDENT_RESPONSE.md](./SECURITY_INCIDENT_RESPONSE.md) for detailed secrets rotation procedures.

---

## Best Practices

### 1. Defense in Depth

Implement multiple layers of security controls:

```javascript
const security = createSecurityMiddleware({
  // Layer 1: Network controls
  cors: { /* strict origin policy */ },

  // Layer 2: Request validation
  requestLimits: { /* size and timeout limits */ },

  // Layer 3: Rate limiting
  rateLimit: { /* request throttling */ },

  // Layer 4: Input sanitization
  // (automatic in middleware)

  // Layer 5: Security headers
  enableHSTS: true,
  enableXFrameOptions: true,
  // ...
});

// Layer 6: Authentication (separate middleware)
import { createAuthMiddleware } from '@unrdf/daemon';
const auth = createAuthMiddleware();

// Layer 7: Authorization (application logic)
function checkPermissions(user, resource) {
  // ...
}

// Apply all layers
app.use(security.middleware());
app.use(auth.middleware());
app.use(authorizationMiddleware);
```

### 2. Principle of Least Privilege

Grant minimum necessary permissions:

```javascript
// Restrictive by default
const defaultSecurity = createSecurityMiddleware({
  cors: {
    origin: [], // No origins allowed by default
  },
  rateLimit: {
    maxRequests: 10, // Very restrictive
  },
});

// Explicit allowances for specific endpoints
const publicSecurity = createSecurityMiddleware({
  cors: {
    origin: ['https://app.example.com'],
  },
  rateLimit: {
    maxRequests: 100,
  },
});
```

### 3. Fail Securely

Handle errors without leaking information:

```javascript
app.use(async (req, res, next) => {
  try {
    await security.handle(req, res, next);
  } catch (error) {
    // Log detailed error internally
    console.error('Security error:', error);

    // Return generic error to client
    res.status(500).json({
      error: 'Internal server error',
      // Don't include error details in production
    });
  }
});
```

### 4. Monitor and Alert

Implement comprehensive monitoring:

```javascript
import { trace, context } from '@opentelemetry/api';

const tracer = trace.getTracer('security-middleware');

app.use(async (req, res, next) => {
  const span = tracer.startSpan('security.check');

  try {
    await security.handle(req, res, next);

    // Log security events
    if (res.statusCode === 429) {
      span.addEvent('rate_limit_exceeded', {
        ip: req.ip,
        endpoint: req.url,
      });
    }

    if (res.statusCode === 413) {
      span.addEvent('request_too_large', {
        ip: req.ip,
        size: req.body ? Buffer.byteLength(JSON.stringify(req.body)) : 0,
      });
    }
  } finally {
    span.end();
  }
});
```

### 5. Regular Security Audits

Schedule periodic security reviews:

```bash
# Weekly: Check for security updates
pnpm audit

# Monthly: Review security logs
node scripts/analyze-security-logs.mjs

# Quarterly: Penetration testing
node scripts/security-scan.mjs

# Annually: Full security audit
# Engage external security firm
```

### 6. Keep Dependencies Updated

```bash
# Check for vulnerabilities
pnpm audit

# Update dependencies
pnpm update

# Check for outdated packages
pnpm outdated
```

---

## Security Testing

### Automated Tests

```bash
# Run security tests
pnpm test test/security-headers.test.mjs

# Expected: 40+ tests passing
```

### Manual Security Testing

#### 1. CSP Violation Testing

```bash
# Test CSP with inline script
curl -X GET http://localhost:3000/test \
  -H "User-Agent: Mozilla/5.0"

# Check for CSP header in response
# Should include: Content-Security-Policy: default-src 'self'
```

#### 2. CORS Testing

```bash
# Test CORS preflight
curl -X OPTIONS http://localhost:3000/api/test \
  -H "Origin: http://example.com" \
  -H "Access-Control-Request-Method: POST" \
  -v

# Should return appropriate CORS headers
```

#### 3. Rate Limiting Testing

```bash
# Test rate limiting
for i in {1..150}; do
  curl -X GET http://localhost:3000/api/test
done

# Should receive 429 after hitting limit
```

#### 4. Request Size Testing

```bash
# Test body size limit
dd if=/dev/zero bs=1M count=20 | \
  curl -X POST http://localhost:3000/api/upload \
    -H "Content-Type: application/octet-stream" \
    --data-binary @- \
    -v

# Should return 413 if exceeds limit
```

#### 5. Input Sanitization Testing

```bash
# Test SQL injection prevention
curl -X POST http://localhost:3000/api/data \
  -H "Content-Type: application/json" \
  -d '{"query": "SELECT * FROM users; DROP TABLE users;--"}'

# Should return sanitized data
```

### Security Scanning

```bash
# Use security scanning tools
npm install -g snyk
snyk test

# OWASP ZAP scanning
docker run -t owasp/zap2docker-stable zap-baseline.py \
  -t http://localhost:3000

# Mozilla Observatory
# Visit: https://observatory.mozilla.org/
```

---

## Compliance

### OWASP Top 10 Coverage

| Risk | Mitigation | Status |
|------|------------|--------|
| A01:2021 Broken Access Control | API key auth + rate limiting | ✅ |
| A02:2021 Cryptographic Failures | HSTS + secure headers | ✅ |
| A03:2021 Injection | Input sanitization | ✅ |
| A04:2021 Insecure Design | Security by default | ✅ |
| A05:2021 Security Misconfiguration | Secure defaults + validation | ✅ |
| A06:2021 Vulnerable Components | Dependency monitoring | ✅ |
| A07:2021 Authentication Failures | API key system | ✅ |
| A08:2021 Software/Data Integrity | CSP + headers | ✅ |
| A09:2021 Logging/Monitoring Failures | OTEL integration | ✅ |
| A10:2021 Server-Side Request Forgery | Input validation + sanitization | ✅ |

### Security Standards

- **CWE-79**: Cross-site Scripting (XSS) - Mitigated via CSP and input sanitization
- **CWE-89**: SQL Injection - Mitigated via input sanitization
- **CWE-200**: Information Exposure - Mitigated via secure error handling
- **CWE-306**: Missing Authentication - Mitigated via API key system
- **CWE-352**: CSRF - Mitigated via CORS and SameSite cookies
- **CWE-400**: Resource Exhaustion - Mitigated via rate limiting and timeouts
- **CWE-770**: Unrestricted Resource Allocation - Mitigated via request limits

---

## Additional Resources

- [API Key Authentication Guide](./AUTHENTICATION.md)
- [Security Incident Response Runbook](./SECURITY_INCIDENT_RESPONSE.md)
- [OWASP Secure Headers Project](https://owasp.org/www-project-secure-headers/)
- [Content Security Policy Reference](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP)
- [CORS Specification](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS)

---

## Summary

This security hardening implementation provides:

✅ **Content Security Policy** - XSS prevention
✅ **CORS Configuration** - Cross-origin request control
✅ **Request Size Limits** - DoS prevention (10MB default)
✅ **Timeout Enforcement** - Resource exhaustion prevention (30s default)
✅ **Input Sanitization** - Injection attack prevention
✅ **Security Headers** - Defense-in-depth protection
✅ **Rate Limiting** - Brute force and DoS prevention
✅ **Comprehensive Tests** - 40+ security tests
✅ **Production Ready** - Zero breaking changes

**Status**: ✅ **PRODUCTION READY**
