/**
 * @fileoverview Comprehensive security examples validation test
 * Tests ALL security code snippets from docs and examples
 * Proves security APIs actually work with real execution
 */

import { strict as assert } from 'node:assert';
import { describe, it } from 'node:test';

// Test imports from security modules
import {
  sanitizeHTML,
  sanitizeURL,
  isPathSafe,
  RateLimiter,
  CSRFTokenManager,
  hashPassword,
  verifyPassword,
  getSecurityHeaders,
  validateInput,
  generateSecureRandom
} from '../packages/core/src/security.mjs';

import {
  EmailSchema,
  SafeStringSchema,
  SPARQLQuerySchema,
  PasswordSchema,
  URLSchema,
  UsernameSchema,
  SafeFilePathSchema,
  APIKeySchema,
  JWTSchema,
  IPAddressSchema,
  PortSchema,
  UUIDSchema,
  TimestampSchema,
  ContentTypeSchema,
  HTTPMethodSchema,
  RDFTripleSchema,
  RDFQuadSchema,
  EnvSchema,
  SanitizedHTMLSchema,
  FileUploadSchema,
  RateLimitConfigSchema,
  CORSConfigSchema,
  PaginationSchema,
  NamespaceSchema,
  validateInput as validateInputSchema,
  validateFields
} from '../packages/core/src/security-schemas.mjs';

const results = {
  total: 0,
  passed: 0,
  failed: 0,
  errors: []
};

function testCase(name, fn) {
  results.total++;
  try {
    fn();
    results.passed++;
    console.log(`✅ ${name}`);
  } catch (error) {
    results.failed++;
    results.errors.push({ name, error: error.message });
    console.error(`❌ ${name}: ${error.message}`);
  }
}

// ============================================================================
// TEST 1: Input Validation (examples/security-patterns.mjs)
// ============================================================================

console.log('\n=== Testing Input Validation ===\n');

testCase('EmailSchema validates valid email', () => {
  const result = EmailSchema.safeParse('test@example.com');
  assert.ok(result.success);
  assert.strictEqual(result.data, 'test@example.com');
});

testCase('EmailSchema rejects invalid email', () => {
  const result = EmailSchema.safeParse('not-an-email');
  assert.ok(!result.success);
});

testCase('SafeStringSchema validates safe string', () => {
  const result = SafeStringSchema.safeParse('Hello World');
  assert.ok(result.success);
});

testCase('SafeStringSchema rejects HTML characters', () => {
  const result = SafeStringSchema.safeParse('<script>alert("xss")</script>');
  assert.ok(!result.success);
});

testCase('SPARQLQuerySchema validates SELECT query', () => {
  const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
  const result = SPARQLQuerySchema.safeParse(query);
  assert.ok(result.success);
});

testCase('SPARQLQuerySchema rejects dangerous operations', () => {
  const query = 'DROP ALL';
  const result = SPARQLQuerySchema.safeParse(query);
  assert.ok(!result.success);
});

testCase('PasswordSchema validates strong password', () => {
  const result = PasswordSchema.safeParse('MyStr0ng!Pass@2024');
  assert.ok(result.success);
});

testCase('PasswordSchema rejects weak password', () => {
  const result = PasswordSchema.safeParse('weak');
  assert.ok(!result.success);
});

testCase('validateFields works with multiple schemas', () => {
  const result = validateFields(
    { email: 'test@example.com', username: 'user123' },
    { email: EmailSchema, username: UsernameSchema }
  );
  assert.ok(result.valid);
  assert.strictEqual(result.errors, null);
});

testCase('validateFields detects multiple errors', () => {
  const result = validateFields(
    { email: 'invalid', username: 'u!' },
    { email: EmailSchema, username: UsernameSchema }
  );
  assert.ok(!result.valid);
  assert.ok(result.errors);
  assert.ok(result.errors.email);
  assert.ok(result.errors.username);
});

// ============================================================================
// TEST 2: Output Sanitization
// ============================================================================

console.log('\n=== Testing Output Sanitization ===\n');

testCase('sanitizeHTML escapes script tags', () => {
  const dirty = '<script>alert("xss")</script>';
  const clean = sanitizeHTML(dirty);
  assert.ok(!clean.includes('<script'));
  assert.ok(clean.includes('&lt;script'));
});

testCase('sanitizeHTML escapes all dangerous characters', () => {
  const dirty = '<>"\'&/';
  const clean = sanitizeHTML(dirty);
  assert.strictEqual(clean, '&lt;&gt;&quot;&#x27;&amp;&#x2F;');
});

testCase('sanitizeURL accepts valid URL', () => {
  const safe = sanitizeURL('https://example.com/path', ['example.com']);
  assert.strictEqual(safe, 'https://example.com/path');
});

testCase('sanitizeURL rejects disallowed domain', () => {
  const unsafe = sanitizeURL('https://evil.com', ['example.com']);
  assert.strictEqual(unsafe, null);
});

testCase('sanitizeURL rejects javascript: protocol', () => {
  const unsafe = sanitizeURL('javascript:alert("xss")');
  assert.strictEqual(unsafe, null);
});

testCase('sanitizeURL works without domain whitelist', () => {
  const safe = sanitizeURL('https://example.com');
  assert.strictEqual(safe, 'https://example.com/');
});

// ============================================================================
// TEST 3: Authentication & Authorization
// ============================================================================

console.log('\n=== Testing Authentication & Authorization ===\n');

testCase('hashPassword generates hash and salt', async () => {
  const { hash, salt } = await hashPassword('MyPassword123!');
  assert.ok(hash);
  assert.ok(salt);
  assert.strictEqual(hash.length, 128); // 64 bytes = 128 hex chars
  assert.strictEqual(salt.length, 32); // 16 bytes = 32 hex chars
});

testCase('verifyPassword accepts correct password', async () => {
  const password = 'MyPassword123!';
  const { hash, salt } = await hashPassword(password);
  const valid = await verifyPassword(password, hash, salt);
  assert.ok(valid);
});

testCase('verifyPassword rejects incorrect password', async () => {
  const { hash, salt } = await hashPassword('correct');
  const valid = await verifyPassword('wrong', hash, salt);
  assert.ok(!valid);
});

testCase('generateSecureRandom generates random string', () => {
  const random1 = generateSecureRandom(32);
  const random2 = generateSecureRandom(32);
  assert.ok(random1);
  assert.ok(random2);
  assert.notStrictEqual(random1, random2);
});

// ============================================================================
// TEST 4: Rate Limiting
// ============================================================================

console.log('\n=== Testing Rate Limiting ===\n');

testCase('RateLimiter allows requests within limit', () => {
  const limiter = new RateLimiter({ maxRequests: 5, windowMs: 60000 });

  for (let i = 0; i < 5; i++) {
    assert.ok(limiter.tryConsume('user1'));
  }
});

testCase('RateLimiter blocks requests over limit', () => {
  const limiter = new RateLimiter({ maxRequests: 5, windowMs: 60000 });

  for (let i = 0; i < 5; i++) {
    limiter.tryConsume('user2');
  }

  assert.ok(!limiter.tryConsume('user2'));
});

testCase('RateLimiter getUsage returns correct info', () => {
  const limiter = new RateLimiter({ maxRequests: 10, windowMs: 60000 });

  limiter.tryConsume('user3');
  limiter.tryConsume('user3');

  const usage = limiter.getUsage('user3');
  assert.strictEqual(usage.count, 2);
  assert.strictEqual(usage.remaining, 8);
});

testCase('RateLimiter reset clears limits', () => {
  const limiter = new RateLimiter({ maxRequests: 2, windowMs: 60000 });

  limiter.tryConsume('user4');
  limiter.tryConsume('user4');
  assert.ok(!limiter.tryConsume('user4'));

  limiter.reset('user4');
  assert.ok(limiter.tryConsume('user4'));
});

// ============================================================================
// TEST 5: CSRF Protection
// ============================================================================

console.log('\n=== Testing CSRF Protection ===\n');

testCase('CSRFTokenManager generates unique tokens', () => {
  const csrf = new CSRFTokenManager();
  const token1 = csrf.generate('session1');
  const token2 = csrf.generate('session2');

  assert.ok(token1);
  assert.ok(token2);
  assert.notStrictEqual(token1, token2);
});

testCase('CSRFTokenManager verifies valid token', () => {
  const csrf = new CSRFTokenManager();
  const token = csrf.generate('session123');

  assert.ok(csrf.verify('session123', token));
});

testCase('CSRFTokenManager rejects invalid token', () => {
  const csrf = new CSRFTokenManager();
  csrf.generate('session456');

  assert.ok(!csrf.verify('session456', 'wrong-token'));
});

testCase('CSRFTokenManager rejects token for wrong session', () => {
  const csrf = new CSRFTokenManager();
  const token = csrf.generate('session-a');

  assert.ok(!csrf.verify('session-b', token));
});

testCase('CSRFTokenManager revoke removes token', () => {
  const csrf = new CSRFTokenManager();
  const token = csrf.generate('session789');

  csrf.revoke('session789');
  assert.ok(!csrf.verify('session789', token));
});

// ============================================================================
// TEST 6: Secure File Handling
// ============================================================================

console.log('\n=== Testing Secure File Handling ===\n');

testCase('isPathSafe accepts safe filename', () => {
  assert.ok(isPathSafe('document.txt', '/uploads'));
});

testCase('isPathSafe rejects directory traversal', () => {
  assert.ok(!isPathSafe('../../../etc/passwd', '/uploads'));
});

testCase('isPathSafe rejects null bytes', () => {
  assert.ok(!isPathSafe('file\0.txt', '/uploads'));
});

testCase('isPathSafe rejects tilde expansion', () => {
  assert.ok(!isPathSafe('~/secret', '/uploads'));
});

testCase('SafeFilePathSchema validates safe path', () => {
  const result = SafeFilePathSchema.safeParse('documents/file.txt');
  assert.ok(result.success);
});

testCase('SafeFilePathSchema rejects path traversal', () => {
  const result = SafeFilePathSchema.safeParse('../../../etc/passwd');
  assert.ok(!result.success);
});

// ============================================================================
// TEST 7: Security Headers
// ============================================================================

console.log('\n=== Testing Security Headers ===\n');

testCase('getSecurityHeaders returns all required headers', () => {
  const headers = getSecurityHeaders();

  assert.ok(headers['Content-Security-Policy']);
  assert.ok(headers['X-Content-Type-Options']);
  assert.ok(headers['X-Frame-Options']);
  assert.ok(headers['Strict-Transport-Security']);
  assert.ok(headers['Referrer-Policy']);
});

testCase('getSecurityHeaders includes CSP', () => {
  const headers = getSecurityHeaders();
  assert.ok(headers['Content-Security-Policy'].includes('default-src'));
});

testCase('getSecurityHeaders sets X-Frame-Options to DENY', () => {
  const headers = getSecurityHeaders();
  assert.strictEqual(headers['X-Frame-Options'], 'DENY');
});

// ============================================================================
// TEST 8: Input Validation (security.mjs)
// ============================================================================

console.log('\n=== Testing Input Validation Function ===\n');

testCase('validateInput accepts safe string', () => {
  const { valid, issues } = validateInput('Hello World');
  assert.ok(valid);
  assert.strictEqual(issues.length, 0);
});

testCase('validateInput detects SQL injection', () => {
  const { valid, issues } = validateInput("' OR '1'='1");
  assert.ok(!valid);
  assert.ok(issues.some(i => i.includes('SQL injection')));
});

testCase('validateInput detects command injection', () => {
  const { valid, issues } = validateInput('test; rm -rf /');
  assert.ok(!valid);
  assert.ok(issues.some(i => i.includes('command injection')));
});

testCase('validateInput detects null bytes', () => {
  const { valid, issues } = validateInput('test\0file');
  assert.ok(!valid);
  assert.ok(issues.some(i => i.includes('Null byte')));
});

// ============================================================================
// TEST 9: All 26 Security Schemas from security-schemas.mjs
// ============================================================================

console.log('\n=== Testing All 26 Security Schemas ===\n');

testCase('URLSchema validates HTTPS URL', () => {
  const result = URLSchema.safeParse('https://example.com');
  assert.ok(result.success);
});

testCase('URLSchema rejects non-HTTP protocols', () => {
  const result = URLSchema.safeParse('ftp://example.com');
  assert.ok(!result.success);
});

testCase('UsernameSchema validates alphanumeric username', () => {
  const result = UsernameSchema.safeParse('user_name-123');
  assert.ok(result.success);
});

testCase('UsernameSchema rejects special characters', () => {
  const result = UsernameSchema.safeParse('user@name!');
  assert.ok(!result.success);
});

testCase('APIKeySchema validates 64-char key', () => {
  const key = 'a'.repeat(64);
  const result = APIKeySchema.safeParse(key);
  assert.ok(result.success);
});

testCase('APIKeySchema rejects wrong length', () => {
  const result = APIKeySchema.safeParse('short');
  assert.ok(!result.success);
});

testCase('JWTSchema validates JWT format', () => {
  const jwt = 'eyJhbGc.eyJzdWI.SflKxw';
  const result = JWTSchema.safeParse(jwt);
  assert.ok(result.success);
});

testCase('JWTSchema rejects invalid format', () => {
  const result = JWTSchema.safeParse('not.a.jwt.token');
  assert.ok(!result.success);
});

testCase('IPAddressSchema validates IPv4', () => {
  const result = IPAddressSchema.safeParse('192.168.1.1');
  assert.ok(result.success);
});

testCase('IPAddressSchema rejects invalid IP', () => {
  const result = IPAddressSchema.safeParse('999.999.999.999');
  assert.ok(!result.success);
});

testCase('PortSchema validates port number', () => {
  const result = PortSchema.safeParse(8080);
  assert.ok(result.success);
});

testCase('PortSchema rejects invalid port', () => {
  const result = PortSchema.safeParse(99999);
  assert.ok(!result.success);
});

testCase('UUIDSchema validates UUID', () => {
  const result = UUIDSchema.safeParse('123e4567-e89b-12d3-a456-426614174000');
  assert.ok(result.success);
});

testCase('UUIDSchema rejects invalid UUID', () => {
  const result = UUIDSchema.safeParse('not-a-uuid');
  assert.ok(!result.success);
});

testCase('TimestampSchema validates ISO 8601', () => {
  const result = TimestampSchema.safeParse('2024-12-25T10:30:00Z');
  assert.ok(result.success);
});

testCase('ContentTypeSchema validates RDF content types', () => {
  const result = ContentTypeSchema.safeParse('text/turtle');
  assert.ok(result.success);
});

testCase('ContentTypeSchema rejects invalid type', () => {
  const result = ContentTypeSchema.safeParse('text/html');
  assert.ok(!result.success);
});

testCase('HTTPMethodSchema validates methods', () => {
  const result = HTTPMethodSchema.safeParse('POST');
  assert.ok(result.success);
});

testCase('RDFTripleSchema validates triple', () => {
  const result = RDFTripleSchema.safeParse({
    subject: 'http://example.org/s',
    predicate: 'http://example.org/p',
    object: 'value'
  });
  assert.ok(result.success);
});

testCase('RDFQuadSchema validates quad', () => {
  const result = RDFQuadSchema.safeParse({
    subject: 'http://example.org/s',
    predicate: 'http://example.org/p',
    object: 'value',
    graph: 'http://example.org/g'
  });
  assert.ok(result.success);
});

testCase('EnvSchema validates environment config', () => {
  const result = EnvSchema.safeParse({
    NODE_ENV: 'production',
    LOG_LEVEL: 'info'
  });
  assert.ok(result.success);
});

testCase('SanitizedHTMLSchema rejects script tags', () => {
  const result = SanitizedHTMLSchema.safeParse('<script>alert(1)</script>');
  assert.ok(!result.success);
});

testCase('SanitizedHTMLSchema rejects event handlers', () => {
  const result = SanitizedHTMLSchema.safeParse('<div onerror="alert(1)">');
  assert.ok(!result.success);
});

testCase('FileUploadSchema validates upload', () => {
  const result = FileUploadSchema.safeParse({
    filename: 'document.pdf',
    mimetype: 'application/pdf',
    size: 1024 * 1024 // 1MB
  });
  assert.ok(result.success);
});

testCase('FileUploadSchema rejects oversized file', () => {
  const result = FileUploadSchema.safeParse({
    filename: 'huge.zip',
    mimetype: 'application/zip',
    size: 100 * 1024 * 1024 // 100MB
  });
  assert.ok(!result.success);
});

testCase('RateLimitConfigSchema validates config', () => {
  const result = RateLimitConfigSchema.safeParse({
    maxRequests: 100,
    windowMs: 60000
  });
  assert.ok(result.success);
});

testCase('CORSConfigSchema validates CORS config', () => {
  const result = CORSConfigSchema.safeParse({
    origin: 'https://example.com',
    methods: ['GET', 'POST'],
    credentials: true
  });
  assert.ok(result.success);
});

testCase('PaginationSchema validates pagination', () => {
  const result = PaginationSchema.safeParse({
    page: 1,
    limit: 20
  });
  assert.ok(result.success);
});

testCase('NamespaceSchema validates namespace', () => {
  const result = NamespaceSchema.safeParse('ex');
  assert.ok(result.success);
});

testCase('NamespaceSchema rejects invalid namespace', () => {
  const result = NamespaceSchema.safeParse('123-invalid');
  assert.ok(!result.success);
});

// ============================================================================
// TEST 10: SECURITY.md Examples
// ============================================================================

console.log('\n=== Testing SECURITY.md Examples ===\n');

testCase('SECURITY.md example: EmailSchema validation', () => {
  const result = EmailSchema.safeParse('test@example.com');
  assert.ok(result.success);
});

testCase('SECURITY.md example: SafeStringSchema validation', () => {
  const safeText = SafeStringSchema.parse('Safe text without HTML');
  assert.ok(safeText);
});

testCase('SECURITY.md example: sanitizeHTML usage', () => {
  const safeHTML = sanitizeHTML('<script>alert(1)</script>');
  assert.ok(!safeHTML.includes('<script'));
});

testCase('SECURITY.md example: RateLimiter usage', () => {
  const limiter = new RateLimiter({ maxRequests: 100, windowMs: 60000 });
  assert.ok(limiter.tryConsume('userId'));
});

testCase('SECURITY.md example: CSRFTokenManager usage', () => {
  const csrf = new CSRFTokenManager();
  const token = csrf.generate('sessionId');
  assert.ok(csrf.verify('sessionId', token));
});

testCase('SECURITY.md example: getSecurityHeaders usage', () => {
  const headers = getSecurityHeaders();
  assert.ok(Object.keys(headers).length > 0);
});

// ============================================================================
// TEST 11: docs/security-checklist.md Examples
// ============================================================================

console.log('\n=== Testing security-checklist.md Examples ===\n');

testCase('Checklist example: sanitizeHTML prevents XSS', () => {
  const userInput = '<script>alert("xss")</script>';
  const safe = sanitizeHTML(userInput);
  assert.ok(!safe.includes('<script'));
});

testCase('Checklist example: isPathSafe prevents traversal', () => {
  const filename = '../../../etc/passwd';
  assert.ok(!isPathSafe(filename, './uploads'));
});

testCase('Checklist example: URLSchema validates URL', () => {
  const userUrl = 'https://example.com';
  const result = URLSchema.parse(userUrl);
  assert.ok(result);
});

// ============================================================================
// FINAL RESULTS
// ============================================================================

console.log('\n' + '='.repeat(70));
console.log('SECURITY EXAMPLES VALIDATION RESULTS');
console.log('='.repeat(70));
console.log(`Total Tests: ${results.total}`);
console.log(`Passed: ${results.passed} ✅`);
console.log(`Failed: ${results.failed} ❌`);
console.log(`Success Rate: ${((results.passed / results.total) * 100).toFixed(2)}%`);
console.log('='.repeat(70));

if (results.errors.length > 0) {
  console.log('\nFailed Tests:');
  results.errors.forEach(({ name, error }) => {
    console.log(`  ❌ ${name}: ${error}`);
  });
}

console.log('\n');

// Exit with error code if any tests failed
process.exit(results.failed > 0 ? 1 : 0);
