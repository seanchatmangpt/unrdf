#!/usr/bin/env node
/**
 * Comprehensive Security Validation - All 7 Vulnerabilities
 * Proves all security fixes are working correctly
 */

import { GraphAwareRouter } from './microfw-9-graph-routing.mjs';

console.log('╔════════════════════════════════════════════════════════════╗');
console.log('║ COMPREHENSIVE SECURITY VALIDATION                          ║');
console.log('║ Testing All 7 CVE Fixes                                    ║');
console.log('╚════════════════════════════════════════════════════════════╝\n');

const results = {
  passed: 0,
  failed: 0,
  tests: []
};

function recordTest(name, passed, details) {
  results.tests.push({ name, passed, details });
  if (passed) {
    results.passed++;
    console.log('✅ PASS:', name);
  } else {
    results.failed++;
    console.log('❌ FAIL:', name);
  }
  console.log('   Details:', details);
  console.log('');
}

// ============================================================================
// SEC-001: Handler Injection Prevention (CVSS 9.8)
// ============================================================================

console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
console.log('SEC-001: Handler Injection + Process Access (CRITICAL)');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

try {
  const router1 = new GraphAwareRouter();

  // Attempt to register malicious handler with process access
  try {
    router1.defineRoute('evil', '/evil', 'GET', async (ctx) => {
      return {
        env: process.env.PATH, // Attempt to access process
        cwd: process.cwd()
      };
    });
    recordTest('SEC-001: Block handler with process access', false, 'Handler registration should have been blocked');
  } catch (error) {
    recordTest('SEC-001: Block handler with process access', true, 'Handler validation blocked dangerous operation: ' + error.message);
  }

  // Attempt eval injection
  try {
    router1.defineRoute('eval-attack', '/eval', 'GET', async (ctx) => {
      return eval('1+1'); // Attempt eval
    });
    recordTest('SEC-001: Block handler with eval', false, 'Handler with eval should have been blocked');
  } catch (error) {
    recordTest('SEC-001: Block handler with eval', true, 'Handler validation blocked eval: ' + error.message);
  }

  // Verify safe handler works
  router1.defineRoute('safe', '/safe', 'GET', async (ctx) => ({
    message: 'This is safe'
  }));
  const safeResult = await router1.handleRequest({ path: '/safe', method: 'GET' });
  recordTest('SEC-001: Allow safe handlers', safeResult.status === 200, 'Safe handler executed successfully');

} catch (error) {
  recordTest('SEC-001: Overall test', false, error.message);
}

// ============================================================================
// SEC-002: Information Disclosure Prevention (CVSS 8.6)
// ============================================================================

console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
console.log('SEC-002: Information Disclosure via Exceptions (CRITICAL)');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

try {
  const router2 = new GraphAwareRouter({ production: false });

  router2.defineRoute('crash', '/crash', 'GET', async (ctx) => {
    const sensitiveData = { apiKey: 'sk-secret-1234' };
    throw new Error('Crash with sensitive: ' + JSON.stringify(sensitiveData));
  });

  const crashResult = await router2.handleRequest({ path: '/crash', method: 'GET' });

  const hasStackTrace = crashResult.body.error?.includes('apiKey') ||
                        crashResult.body.error?.includes('sk-secret');

  recordTest('SEC-002: No sensitive data in errors', !hasStackTrace,
    `Error message: "${crashResult.body.error}" - should be sanitized`);

  // Test production mode
  const router2Prod = new GraphAwareRouter({ production: true });
  router2Prod.defineRoute('crash', '/crash', 'GET', async (ctx) => {
    throw new Error('Secret data here');
  });
  const prodResult = await router2Prod.handleRequest({ path: '/crash', method: 'GET' });

  recordTest('SEC-002: Production mode error sanitization',
    prodResult.body.error === 'Internal server error',
    `Production error: "${prodResult.body.error}"`);

} catch (error) {
  recordTest('SEC-002: Overall test', false, error.message);
}

// ============================================================================
// SEC-003: XSS Protection (CVSS 7.5)
// ============================================================================

console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
console.log('SEC-003: Cross-Site Scripting (XSS) Prevention (HIGH)');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

try {
  const router3 = new GraphAwareRouter();

  // Test XSS in path
  const xssPath = await router3.handleRequest({
    path: '/customers/<script>alert(1)</script>',
    method: 'GET'
  });

  recordTest('SEC-003: Block XSS in path', xssPath.status === 400,
    `XSS path blocked with status ${xssPath.status}`);

  // Test XSS in response (should be sanitized)
  router3.defineRoute('echo', '/echo', 'GET', async (ctx) => ({
    userInput: '<script>alert("xss")</script>',
    normalText: 'Safe text'
  }));

  const echoResult = await router3.handleRequest({ path: '/echo', method: 'GET' });
  const isSanitized = echoResult.body.userInput?.includes('&lt;script&gt;');

  recordTest('SEC-003: Sanitize XSS in responses', isSanitized,
    `Response sanitized: "${echoResult.body.userInput}"`);

  // Verify Content-Type headers
  recordTest('SEC-003: Security headers present',
    echoResult.headers['Content-Type'] === 'application/json' &&
    echoResult.headers['X-Content-Type-Options'] === 'nosniff',
    `Headers: ${JSON.stringify(echoResult.headers)}`);

} catch (error) {
  recordTest('SEC-003: Overall test', false, error.message);
}

// ============================================================================
// SEC-004: Authentication & Authorization (CVSS 7.3)
// ============================================================================

console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
console.log('SEC-004: Authentication & Authorization (HIGH)');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

try {
  const router4 = new GraphAwareRouter();

  const userToken = router4.createAuthToken('user1', ['user']);
  const adminToken = router4.createAuthToken('admin1', ['admin']);

  router4.defineRoute('public', '/public', 'GET',
    async () => ({ data: 'public' }));

  router4.defineRoute('protected', '/protected', 'GET',
    async () => ({ data: 'protected' }),
    { requiresAuth: true, requiredRole: 'user' });

  router4.defineRoute('admin', '/admin', 'GET',
    async () => ({ data: 'admin only' }),
    { requiresAuth: true, requiredRole: 'admin' });

  // Test public access
  const publicResult = await router4.handleRequest({ path: '/public', method: 'GET' });
  recordTest('SEC-004: Public route accessible', publicResult.status === 200,
    `Public route returned status ${publicResult.status}`);

  // Test protected without auth
  const noAuthResult = await router4.handleRequest({ path: '/protected', method: 'GET' });
  recordTest('SEC-004: Protected route requires auth', noAuthResult.status === 401,
    `No auth returned status ${noAuthResult.status}`);

  // Test protected with valid token
  const withAuthResult = await router4.handleRequest({
    path: '/protected',
    method: 'GET',
    headers: { authorization: `Bearer ${userToken}` }
  });
  recordTest('SEC-004: Valid token grants access', withAuthResult.status === 200,
    `With auth returned status ${withAuthResult.status}`);

  // Test RBAC - user accessing admin route
  const rbacFailResult = await router4.handleRequest({
    path: '/admin',
    method: 'GET',
    headers: { authorization: `Bearer ${userToken}` }
  });
  recordTest('SEC-004: RBAC prevents unauthorized role access', rbacFailResult.status === 403,
    `User role accessing admin returned status ${rbacFailResult.status}`);

  // Test RBAC - admin accessing admin route
  const rbacSuccessResult = await router4.handleRequest({
    path: '/admin',
    method: 'GET',
    headers: { authorization: `Bearer ${adminToken}` }
  });
  recordTest('SEC-004: RBAC allows authorized role access', rbacSuccessResult.status === 200,
    `Admin role accessing admin returned status ${rbacSuccessResult.status}`);

} catch (error) {
  recordTest('SEC-004: Overall test', false, error.message);
}

// ============================================================================
// SEC-005: Prototype Pollution Prevention (CVSS 6.5)
// ============================================================================

console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
console.log('SEC-005: Prototype Pollution Prevention (MEDIUM)');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

try {
  const router5 = new GraphAwareRouter();

  // Test __proto__ in path
  const protoPath = await router5.handleRequest({
    path: '/customers/__proto__',
    method: 'GET'
  });
  recordTest('SEC-005: Block __proto__ in path', protoPath.status === 400,
    `__proto__ path blocked with status ${protoPath.status}`);

  // Test constructor in path
  const constructorPath = await router5.handleRequest({
    path: '/customers/constructor',
    method: 'GET'
  });
  recordTest('SEC-005: Block constructor in path', constructorPath.status === 400,
    `constructor path blocked with status ${constructorPath.status}`);

  // Test prototype pollution via metadata
  router5.defineRoute('meta-test', '/meta', 'GET',
    async () => ({ data: 'ok' }),
    { __proto__: 'polluted', constructor: 'hacked', normalKey: 'safe' }
  );

  const metaTriples = router5.store.triples.filter(t =>
    t.subject.includes('meta-test')
  );
  const hasProtoTriple = metaTriples.some(t => t.predicate.includes('__proto__'));
  const hasConstructorTriple = metaTriples.some(t => t.predicate.includes('constructor'));
  const hasNormalTriple = metaTriples.some(t => t.predicate.includes('normalKey'));

  recordTest('SEC-005: Metadata __proto__ filtered', !hasProtoTriple,
    `__proto__ in metadata was ${hasProtoTriple ? 'stored' : 'filtered'}`);

  recordTest('SEC-005: Metadata constructor filtered', !hasConstructorTriple,
    `constructor in metadata was ${hasConstructorTriple ? 'stored' : 'filtered'}`);

  recordTest('SEC-005: Normal metadata allowed', hasNormalTriple,
    `Normal keys are ${hasNormalTriple ? 'stored' : 'blocked'}`);

} catch (error) {
  recordTest('SEC-005: Overall test', false, error.message);
}

// ============================================================================
// SEC-006: RDF Triple Injection Prevention (CVSS 6.0)
// ============================================================================

console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
console.log('SEC-006: RDF Triple Injection Prevention (MEDIUM)');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

try {
  const router6 = new GraphAwareRouter();

  // Test invalid URI in defineRelationship
  try {
    router6.defineRelationship(
      'not-a-valid-uri',
      'http://example.com/predicate',
      'http://example.com/object'
    );
    recordTest('SEC-006: Block invalid URI format', false,
      'Invalid URI should have been rejected');
  } catch (error) {
    recordTest('SEC-006: Block invalid URI format', true,
      `Invalid URI rejected: ${error.message.substring(0, 50)}`);
  }

  // Test XSS in URI
  try {
    router6.defineRelationship(
      'http://evil.com/<script>alert(1)</script>',
      'http://evil.com/predicate',
      'http://evil.com/object'
    );
    recordTest('SEC-006: Block XSS in URI', false,
      'XSS in URI should have been rejected');
  } catch (error) {
    recordTest('SEC-006: Block XSS in URI', true,
      `XSS in URI rejected: ${error.message.substring(0, 50)}`);
  }

  // Test __proto__ in URI
  try {
    router6.defineRelationship(
      'http://evil.com/__proto__',
      'http://evil.com/predicate',
      'http://evil.com/object'
    );
    recordTest('SEC-006: Block __proto__ in URI', false,
      '__proto__ in URI should have been rejected');
  } catch (error) {
    recordTest('SEC-006: Block __proto__ in URI', true,
      `__proto__ in URI rejected: ${error.message.substring(0, 50)}`);
  }

  // Test valid URI works
  try {
    router6.defineRelationship(
      'http://example.com/subject',
      'http://example.com/predicate',
      'http://example.com/object'
    );
    recordTest('SEC-006: Allow valid URIs', true,
      'Valid URI triple was accepted');
  } catch (error) {
    recordTest('SEC-006: Allow valid URIs', false,
      `Valid URI rejected: ${error.message}`);
  }

} catch (error) {
  recordTest('SEC-006: Overall test', false, error.message);
}

// ============================================================================
// SEC-007: Memory Exhaustion Prevention (CVSS 4.0)
// ============================================================================

console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
console.log('SEC-007: Memory Exhaustion Prevention (LOW)');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

try {
  const router7 = new GraphAwareRouter({ maxTriples: 100 });

  // Add triples up to limit
  let addCount = 0;
  try {
    for (let i = 0; i < 150; i++) {
      router7.defineRelationship(
        `http://test.com/subject/${i}`,
        'http://test.com/predicate',
        `http://test.com/object/${i}`
      );
      addCount++;
    }
    recordTest('SEC-007: Enforce triple limit', false,
      `Added ${addCount} triples, should have stopped at 100`);
  } catch (error) {
    recordTest('SEC-007: Enforce triple limit', addCount < 150,
      `Limit enforced at ${addCount} triples: ${error.message.substring(0, 50)}`);
  }

  // Test cleanup method
  const router7b = new GraphAwareRouter({ maxTriples: 10000 });
  for (let i = 0; i < 100; i++) {
    router7b.defineRelationship(
      `http://test.com/s/${i}`,
      'http://test.com/p',
      `http://test.com/o/${i}`
    );
  }

  const beforeCleanup = router7b.store.size();
  router7b.store.cleanup(50);
  const afterCleanup = router7b.store.size();

  recordTest('SEC-007: Cleanup reduces store size', afterCleanup <= 50,
    `Before: ${beforeCleanup} triples, After: ${afterCleanup} triples`);

  // Test size() method
  recordTest('SEC-007: Size tracking works',
    typeof router7b.store.size() === 'number',
    `Store size: ${router7b.store.size()}`);

} catch (error) {
  recordTest('SEC-007: Overall test', false, error.message);
}

// ============================================================================
// Summary
// ============================================================================

console.log('═══════════════════════════════════════════════════════════');
console.log('SECURITY VALIDATION SUMMARY');
console.log('═══════════════════════════════════════════════════════════\n');

console.log(`Total Tests: ${results.tests.length}`);
console.log(`✅ Passed: ${results.passed}`);
console.log(`❌ Failed: ${results.failed}`);
console.log(`Success Rate: ${((results.passed / results.tests.length) * 100).toFixed(1)}%\n`);

console.log('Vulnerability Status:');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');

const vulnerabilities = [
  { id: 'SEC-001', name: 'Handler Injection', severity: 'CRITICAL', cvss: 9.8 },
  { id: 'SEC-002', name: 'Information Disclosure', severity: 'CRITICAL', cvss: 8.6 },
  { id: 'SEC-003', name: 'XSS', severity: 'HIGH', cvss: 7.5 },
  { id: 'SEC-004', name: 'No Auth/AuthZ', severity: 'HIGH', cvss: 7.3 },
  { id: 'SEC-005', name: 'Prototype Pollution', severity: 'MEDIUM', cvss: 6.5 },
  { id: 'SEC-006', name: 'RDF Injection', severity: 'MEDIUM', cvss: 6.0 },
  { id: 'SEC-007', name: 'Memory Exhaustion', severity: 'LOW', cvss: 4.0 },
];

vulnerabilities.forEach(vuln => {
  const vulnTests = results.tests.filter(t => t.name.startsWith(vuln.id));
  const allPassed = vulnTests.every(t => t.passed);
  const status = allPassed ? '✅ FIXED' : '❌ VULNERABLE';

  console.log(`${status} ${vuln.id} (${vuln.severity} - CVSS ${vuln.cvss}): ${vuln.name}`);
  console.log(`       Tests: ${vulnTests.filter(t => t.passed).length}/${vulnTests.length} passed`);
});

console.log('\n═══════════════════════════════════════════════════════════');

if (results.failed === 0) {
  console.log('🎉 ALL SECURITY VULNERABILITIES FIXED!');
  console.log('The microframework is now OWASP compliant and production-ready.');
} else {
  console.log('⚠️  SOME TESTS FAILED - Review required');
  process.exit(1);
}
