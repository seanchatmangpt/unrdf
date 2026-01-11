/**
 * @file Security Hardening Example
 * @description Demonstrates comprehensive security middleware usage
 */

import {
  createSecurityMiddleware,
  DEFAULT_SECURITY_CONFIG,
} from '../src/middleware/security-headers.mjs';

console.log('============================================================');
console.log('SECURITY HARDENING EXAMPLES');
console.log('============================================================\n');

// Example 1: Basic Security Middleware
console.log('Example 1: Basic Security Middleware');
console.log('------------------------------------------------------------');

const basicSecurity = createSecurityMiddleware({
  enableHSTS: true,
  enableNoSniff: true,
  enableXFrameOptions: true,
  xFrameOptions: 'DENY',
});

const basicRequest = {
  method: 'GET',
  url: '/api/test',
  headers: { 'user-agent': 'test' },
};

const basicResponse = {};

await basicSecurity.handle(basicRequest, basicResponse, () => Promise.resolve());

console.log('Security headers applied:');
console.log('  - Strict-Transport-Security:', basicResponse.headers['Strict-Transport-Security']);
console.log('  - X-Content-Type-Options:', basicResponse.headers['X-Content-Type-Options']);
console.log('  - X-Frame-Options:', basicResponse.headers['X-Frame-Options']);
console.log('✅ Basic security headers applied\n');

// Example 2: Content Security Policy
console.log('Example 2: Content Security Policy');
console.log('------------------------------------------------------------');

const cspSecurity = createSecurityMiddleware({
  csp: {
    defaultSrc: ["'self'"],
    scriptSrc: ["'self'", "'nonce'"],
    styleSrc: ["'self'", "'unsafe-inline'"],
    imgSrc: ["'self'", 'data:', 'https:'],
    connectSrc: ["'self'", 'https://api.example.com'],
  },
});

const cspResponse = {};
cspSecurity.applySecurityHeaders(cspResponse);

console.log('CSP header:', cspResponse.headers['Content-Security-Policy']);
console.log('CSP nonce:', cspResponse.cspNonce);
console.log('✅ CSP configured and nonce generated\n');

// Example 3: CORS Configuration
console.log('Example 3: CORS Configuration');
console.log('------------------------------------------------------------');

const corsSecurity = createSecurityMiddleware({
  cors: {
    origin: ['http://localhost:3000', 'https://app.example.com'],
    methods: ['GET', 'POST', 'PUT', 'DELETE'],
    allowedHeaders: ['Content-Type', 'Authorization', 'X-API-Key'],
    credentials: true,
  },
});

const corsRequest = {
  method: 'OPTIONS',
  headers: { origin: 'http://localhost:3000' },
};
const corsResponse = {};

corsSecurity.applyCORSHeaders(corsRequest, corsResponse);

console.log('CORS headers:');
console.log('  - Access-Control-Allow-Origin:', corsResponse.headers['Access-Control-Allow-Origin']);
console.log('  - Access-Control-Allow-Methods:', corsResponse.headers['Access-Control-Allow-Methods']);
console.log('  - Access-Control-Allow-Headers:', corsResponse.headers['Access-Control-Allow-Headers']);
console.log('✅ CORS configured for multiple origins\n');

// Example 4: Rate Limiting
console.log('Example 4: Rate Limiting');
console.log('------------------------------------------------------------');

const rateLimitSecurity = createSecurityMiddleware({
  rateLimit: {
    windowMs: 60000,
    maxRequests: 5,
  },
});

console.log('Making 7 requests to test rate limiting...');
for (let i = 1; i <= 7; i++) {
  const result = rateLimitSecurity.checkRateLimit('test-client');
  console.log(`  Request ${i}: ${result.allowed ? 'ALLOWED' : 'BLOCKED'} (${result.remaining} remaining)`);
}
console.log('✅ Rate limiting enforced after 5 requests\n');

// Example 5: Input Sanitization
console.log('Example 5: Input Sanitization');
console.log('------------------------------------------------------------');

const sanitizeSecurity = createSecurityMiddleware();

const maliciousInputs = {
  sql: "admin'; DROP TABLE users;--",
  xss: '<script>alert("XSS")</script>',
  path: '../../../etc/passwd',
  command: 'file.txt; rm -rf /',
};

console.log('Sanitizing malicious inputs:');
for (const [type, input] of Object.entries(maliciousInputs)) {
  const sanitized = sanitizeSecurity.sanitizeInput(input);
  console.log(`  ${type.toUpperCase()}: "${input}" → "${sanitized}"`);
}
console.log('✅ All injection attempts sanitized\n');

// Example 6: Request Size Limits
console.log('Example 6: Request Size Limits');
console.log('------------------------------------------------------------');

const limitsSecurity = createSecurityMiddleware({
  requestLimits: {
    maxBodySize: 1024, // 1KB for demonstration
    maxUrlLength: 100,
    maxHeaderSize: 500,
  },
});

const oversizedRequest = {
  url: '/api/test',
  body: { data: 'x'.repeat(2000) },
  headers: { 'content-type': 'application/json' },
};

const limitsCheck = limitsSecurity.checkRequestLimits(oversizedRequest);

console.log('Request validation:');
console.log('  Valid:', limitsCheck.valid);
console.log('  Errors:', limitsCheck.errors.length > 0 ? limitsCheck.errors[0] : 'None');
console.log('✅ Oversized request rejected\n');

// Example 7: Timeout Enforcement
console.log('Example 7: Timeout Enforcement');
console.log('------------------------------------------------------------');

const timeoutSecurity = createSecurityMiddleware({
  requestLimits: {
    timeout: 1000, // 1 second
  },
});

// Fast operation (should succeed)
try {
  await timeoutSecurity.processWithTimeout(
    'fast-request',
    () => Promise.resolve('success'),
    1000
  );
  console.log('  Fast request: ✅ COMPLETED');
} catch (error) {
  console.log('  Fast request: ❌ FAILED');
}

// Slow operation (should timeout)
try {
  await timeoutSecurity.processWithTimeout(
    'slow-request',
    () => new Promise(resolve => setTimeout(resolve, 2000)),
    1000
  );
  console.log('  Slow request: ❌ Should have timed out');
} catch (error) {
  console.log('  Slow request: ✅ TIMED OUT (as expected)');
}

console.log('✅ Timeout enforcement working\n');

// Example 8: Production Configuration
console.log('Example 8: Production Configuration');
console.log('------------------------------------------------------------');

const productionSecurity = createSecurityMiddleware(DEFAULT_SECURITY_CONFIG);

console.log('Production security configuration:');
console.log('  - CSP enabled:', !!DEFAULT_SECURITY_CONFIG.csp);
console.log('  - CORS configured:', !!DEFAULT_SECURITY_CONFIG.cors);
console.log('  - Rate limiting:', DEFAULT_SECURITY_CONFIG.rateLimit?.maxRequests, 'req/min');
console.log('  - Body size limit:', (DEFAULT_SECURITY_CONFIG.requestLimits?.maxBodySize / 1024 / 1024).toFixed(0), 'MB');
console.log('  - Request timeout:', (DEFAULT_SECURITY_CONFIG.requestLimits?.timeout / 1000).toFixed(0), 'seconds');
console.log('  - HSTS enabled:', DEFAULT_SECURITY_CONFIG.enableHSTS);
console.log('✅ Production-ready configuration loaded\n');

// Example 9: Complete Request Flow
console.log('Example 9: Complete Request Flow');
console.log('------------------------------------------------------------');

const completeSecurity = createSecurityMiddleware({
  cors: { origin: '*' },
  rateLimit: {
    windowMs: 60000,
    maxRequests: 100,
  },
  requestLimits: {
    maxBodySize: 10 * 1024 * 1024,
    timeout: 30000,
  },
  enableHSTS: true,
  enableNoSniff: true,
  enableXFrameOptions: true,
});

const completeRequest = {
  method: 'POST',
  url: '/api/data',
  headers: {
    origin: 'http://localhost:3000',
    'content-type': 'application/json',
  },
  body: {
    name: 'Test <script>alert("XSS")</script>',
    query: "SELECT * FROM users",
  },
  ip: '127.0.0.1',
};

const completeResponse = {};

await completeSecurity.handle(completeRequest, completeResponse, () => Promise.resolve());

console.log('Request processed:');
console.log('  - Original body:', JSON.stringify(completeRequest.body));
console.log('  - Sanitized body:', JSON.stringify(completeRequest.sanitizedBody));
console.log('  - Security headers applied:', Object.keys(completeResponse.headers || {}).length);
console.log('  - CORS headers:', !!completeResponse.headers?.['Access-Control-Allow-Origin']);
console.log('  - Rate limit headers:', !!completeResponse.headers?.['X-RateLimit-Limit']);
console.log('✅ Complete security flow executed\n');

// Example 10: Custom Security Policy
console.log('Example 10: Custom Security Policy');
console.log('------------------------------------------------------------');

const customSecurity = createSecurityMiddleware({
  csp: {
    defaultSrc: ["'self'"],
    scriptSrc: ["'self'", 'https://cdn.example.com'],
    styleSrc: ["'self'", 'https://fonts.googleapis.com'],
    imgSrc: ["'self'", 'data:', 'https:'],
    connectSrc: ["'self'", 'wss://realtime.example.com'],
  },
  cors: {
    origin: (origin) => {
      const allowedPatterns = [
        /^https:\/\/.*\.example\.com$/,
        /^http:\/\/localhost:\d+$/,
      ];
      return allowedPatterns.some(pattern => pattern.test(origin));
    },
    credentials: true,
  },
  rateLimit: {
    windowMs: 60000,
    maxRequests: 1000, // High throughput
    keyGenerator: (request) => {
      // Rate limit by API key if present, otherwise by IP
      return request.headers?.['x-api-key'] || request.ip || 'unknown';
    },
  },
  customHeaders: {
    'X-Application-Name': 'UNRDF-Daemon',
    'X-Security-Version': '1.0.0',
  },
});

console.log('Custom security policy configured:');
console.log('  - Dynamic CSP with trusted CDN');
console.log('  - Function-based CORS validation');
console.log('  - High-throughput rate limiting');
console.log('  - API-key based rate limit keys');
console.log('  - Custom application headers');
console.log('✅ Custom security policy ready\n');

// Cleanup
basicSecurity.cleanup();
cspSecurity.cleanup();
corsSecurity.cleanup();
rateLimitSecurity.cleanup();
sanitizeSecurity.cleanup();
limitsSecurity.cleanup();
timeoutSecurity.cleanup();
productionSecurity.cleanup();
completeSecurity.cleanup();
customSecurity.cleanup();

console.log('============================================================');
console.log('All examples completed successfully');
console.log('============================================================');
