#!/usr/bin/env node
/**
 * Advanced Security Testing: Handler Injection & ReDoS
 */

import { GraphAwareRouter } from './microfw-9-graph-routing.mjs';

console.log('═══════════════════════════════════════════════════════════');
console.log('ADVANCED SECURITY TESTS');
console.log('═══════════════════════════════════════════════════════════\n');

// Test 1: Malicious Handler Injection
console.log('TEST 1: Malicious Handler Injection');
console.log('Attempt: Register handler that modifies global state');

const router1 = new GraphAwareRouter();
let globalState = { compromised: false };

// Malicious handler that modifies global state
router1.defineRoute('evil', '/evil', 'GET', async (ctx) => {
  globalState.compromised = true;
  // Attempt to access process (if available)
  try {
    if (typeof process !== 'undefined') {
      return {
        message: 'Process access available!',
        env: Object.keys(process.env || {}).slice(0, 5),
        cwd: process.cwd?.() || 'N/A'
      };
    }
  } catch (e) {
    return { error: e.message };
  }
  return { message: 'Handler executed, global state modified' };
});

const r1 = await router1.handleRequest({ path: '/evil', method: 'GET' });
console.log('Result:', JSON.stringify(r1, null, 2));
console.log('Global State Compromised:', globalState.compromised);
console.log('VULNERABILITY:', globalState.compromised ? 'HANDLER INJECTION SUCCESSFUL' : 'SAFE');
console.log('');

// Test 2: Handler Exception Leaking Sensitive Data
console.log('TEST 2: Exception Information Disclosure');
console.log('Attempt: Trigger error to leak stack trace');

const router2 = new GraphAwareRouter();
router2.defineRoute('crash', '/crash', 'GET', async (ctx) => {
  const sensitiveData = {
    apiKey: 'sk-1234567890abcdef',
    dbPassword: 'super-secret-password',
    privateKey: '-----BEGIN PRIVATE KEY-----'
  };

  // Trigger error with sensitive data in scope
  throw new Error('Internal error with stack trace: ' + JSON.stringify(sensitiveData));
});

const r2 = await router2.handleRequest({ path: '/crash', method: 'GET' });
console.log('Result:', JSON.stringify(r2, null, 2));
console.log('VULNERABILITY:', r2.body.error?.includes('apiKey') ? 'INFORMATION DISCLOSURE' : 'SAFE');
console.log('');

// Test 3: Metadata Injection Attack
console.log('TEST 3: Metadata Injection Attack');
console.log('Attempt: Inject malicious metadata into route definition');

const router3 = new GraphAwareRouter();
router3.defineRoute('meta-attack', '/meta', 'GET',
  async (ctx) => ({ message: 'ok' }),
  {
    // Inject malicious metadata
    '__proto__': 'polluted',
    'constructor': 'hacked',
    'admin': 'true',
    'bypass_auth': 'true'
  }
);

// Check if metadata was stored in RDF graph
const metaTriples = router3.store.triples.filter(t =>
  t.subject.includes('meta-attack')
);
console.log('Stored Metadata Triples:', metaTriples.length);
console.log('Sample:', JSON.stringify(metaTriples.slice(0, 3), null, 2));
console.log('VULNERABILITY:', metaTriples.some(t => t.predicate.includes('__proto__')) ?
  'METADATA POLLUTION POSSIBLE' : 'SAFE');
console.log('');

// Test 4: RDF Graph Query Injection
console.log('TEST 4: RDF Graph Query Injection');
console.log('Attempt: Inject malicious triples via defineRelationship');

const router4 = new GraphAwareRouter();
router4.defineRelationship(
  'http://evil.com/inject',
  'http://evil.com/grants',
  'http://evil.com/admin-access'
);

// Check if malicious triple was stored
const evilTriples = router4.store.triples.filter(t =>
  t.subject.includes('evil.com') || t.object.includes('evil.com')
);
console.log('Evil Triples Count:', evilTriples.length);
console.log('VULNERABILITY:', evilTriples.length > 0 ?
  'TRIPLE INJECTION SUCCESSFUL' : 'SAFE');
console.log('');

// Test 5: Route Pattern Collision Attack
console.log('TEST 5: Route Pattern Collision/Shadowing');
console.log('Attempt: Define overlapping routes to shadow legitimate ones');

const router5 = new GraphAwareRouter();
router5.defineRoute('admin', '/admin', 'GET',
  async () => ({ role: 'admin', authenticated: true }));
router5.defineRoute('fake-admin', '/admin', 'GET',
  async () => ({ role: 'fake', message: 'I shadowed the real route!' }));

const r5 = await router5.handleRequest({ path: '/admin', method: 'GET' });
console.log('Result:', JSON.stringify(r5, null, 2));
console.log('VULNERABILITY:', r5.body.message?.includes('shadowed') ?
  'ROUTE SHADOWING SUCCESSFUL' : 'SAFE');
console.log('');

// Test 6: Memory Exhaustion via Triple Store
console.log('TEST 6: Memory Exhaustion Attack');
console.log('Attempt: Flood triple store with data');

const router6 = new GraphAwareRouter();
const startMem = process.memoryUsage?.().heapUsed || 0;
const startTime = Date.now();

// Add 10,000 triples
for (let i = 0; i < 10000; i++) {
  router6.defineRelationship(
    `http://spam.com/entity/${i}`,
    'http://spam.com/relates',
    `http://spam.com/target/${i}`
  );
}

const endTime = Date.now();
const endMem = process.memoryUsage?.().heapUsed || 0;
const memDelta = ((endMem - startMem) / 1024 / 1024).toFixed(2);

console.log('Triples Added: 10,000');
console.log('Time Taken:', endTime - startTime, 'ms');
console.log('Memory Delta:', memDelta, 'MB');
console.log('VULNERABILITY:', endMem > startMem * 10 ?
  'POTENTIAL MEMORY EXHAUSTION' : 'MANAGEABLE');
console.log('');

// Test 7: startsWith() Prefix Matching Vulnerability
console.log('TEST 7: Prefix Matching Ambiguity');
console.log('Attempt: Exploit startsWith() for unintended route matching');

const router7 = new GraphAwareRouter();
router7.defineRoute('users', '/users', 'GET',
  async () => ({ data: 'public user list' }));
router7.defineRoute('user-admin', '/user-admin', 'GET',
  async () => ({ data: 'SENSITIVE ADMIN DATA' }));

// Request /user-admin - which route matches?
const r7a = await router7.handleRequest({ path: '/users', method: 'GET' });
const r7b = await router7.handleRequest({ path: '/user-admin', method: 'GET' });

console.log('Request /users:', JSON.stringify(r7a.body, null, 2));
console.log('Request /user-admin:', JSON.stringify(r7b.body, null, 2));
console.log('VULNERABILITY:', r7b.body.data?.includes('SENSITIVE') && r7a.status === 200 ?
  'PREFIX MATCHING WORKING AS DESIGNED' : 'POTENTIAL ISSUE');
console.log('');

console.log('═══════════════════════════════════════════════════════════');
console.log('ADVANCED SECURITY TESTS COMPLETE');
console.log('═══════════════════════════════════════════════════════════');
