#!/usr/bin/env node
/**
 * Security Testing: Malicious Input Scenarios
 * Tests microfw-9-graph-routing.mjs for vulnerabilities
 */

import { GraphAwareRouter } from './microfw-9-graph-routing.mjs';

const router = new GraphAwareRouter();

// Define vulnerable route
router.defineRoute('customer_detail', '/customers/', 'GET', async (ctx) => {
  const customerId = ctx.path.split('/')[2];
  const customerUri = ctx.graph.ns.api + `customer/${customerId}`;

  return {
    customerId,
    customerUri,
    message: 'Customer detail',
  };
});

console.log('═══════════════════════════════════════════════════════════');
console.log('SECURITY TEST: Malicious Input Scenarios');
console.log('═══════════════════════════════════════════════════════════\n');

// Test 1: Path Traversal Attack
console.log('TEST 1: Path Traversal Attack');
console.log('Input: /customers/../../../etc/passwd');
const t1 = await router.handleRequest({
  path: '/customers/../../../etc/passwd',
  method: 'GET'
});
console.log('Result:', JSON.stringify(t1, null, 2));
console.log('VULNERABILITY:', t1.body.customerId === '../../../etc/passwd' ? 'FOUND' : 'SAFE');
console.log('');

// Test 2: Null Byte Injection
console.log('TEST 2: Null Byte Injection');
console.log('Input: /customers/1%00admin');
const t2 = await router.handleRequest({
  path: '/customers/1\0admin',
  method: 'GET'
});
console.log('Result:', JSON.stringify(t2, null, 2));
console.log('');

// Test 3: Special Characters / XSS Payload
console.log('TEST 3: XSS Payload in Path');
console.log('Input: /customers/<script>alert(1)</script>');
const t3 = await router.handleRequest({
  path: '/customers/<script>alert(1)</script>',
  method: 'GET'
});
console.log('Result:', JSON.stringify(t3, null, 2));
console.log('VULNERABILITY:', t3.body.customerId?.includes('<script>') ? 'XSS POSSIBLE' : 'SAFE');
console.log('');

// Test 4: Array Index Out of Bounds
console.log('TEST 4: Array Access Without Validation');
console.log('Input: /customers (no trailing ID)');
const t4 = await router.handleRequest({
  path: '/customers',
  method: 'GET'
});
console.log('Result:', JSON.stringify(t4, null, 2));
console.log('');

// Test 5: Extremely Long Path (DoS)
console.log('TEST 5: DoS - Extremely Long Path');
const longPath = '/customers/' + 'A'.repeat(100000);
console.log('Input: /customers/' + 'A'.repeat(50) + '... (100,000 chars total)');
const startTime = Date.now();
const t5 = await router.handleRequest({
  path: longPath,
  method: 'GET'
});
const duration = Date.now() - startTime;
console.log('Result:', JSON.stringify(t5.body).substring(0, 100) + '...');
console.log('Duration:', duration + 'ms');
console.log('VULNERABILITY:', duration > 100 ? 'POTENTIAL DoS' : 'SAFE');
console.log('');

// Test 6: Unicode/Emoji Path Components
console.log('TEST 6: Unicode/Emoji Injection');
console.log('Input: /customers/😈🔥💀');
const t6 = await router.handleRequest({
  path: '/customers/😈🔥💀',
  method: 'GET'
});
console.log('Result:', JSON.stringify(t6, null, 2));
console.log('');

// Test 7: URL Encoding Bypass
console.log('TEST 7: URL Encoding Bypass');
console.log('Input: /customers/%2e%2e%2f%2e%2e%2fadmin');
const t7 = await router.handleRequest({
  path: '/customers/%2e%2e%2f%2e%2e%2fadmin',
  method: 'GET'
});
console.log('Result:', JSON.stringify(t7, null, 2));
console.log('');

// Test 8: Prototype Pollution via Path
console.log('TEST 8: Prototype Pollution Attempt');
console.log('Input: /customers/__proto__');
const t8 = await router.handleRequest({
  path: '/customers/__proto__',
  method: 'GET'
});
console.log('Result:', JSON.stringify(t8, null, 2));
console.log('VULNERABILITY:', t8.body.customerId === '__proto__' ? 'POTENTIAL POLLUTION' : 'SAFE');
console.log('');

// Test 9: Multiple Slashes
console.log('TEST 9: Multiple Slashes (Normalization Bypass)');
console.log('Input: /customers//admin//secret');
const t9 = await router.handleRequest({
  path: '/customers//admin//secret',
  method: 'GET'
});
console.log('Result:', JSON.stringify(t9, null, 2));
console.log('');

// Test 10: Method Confusion Attack
console.log('TEST 10: HTTP Method Confusion');
console.log('Input: GET /customers/1 with method override');
const t10 = await router.handleRequest({
  path: '/customers/1',
  method: 'DELETE'
});
console.log('Result:', JSON.stringify(t10, null, 2));
console.log('VULNERABILITY:', t10.status === 404 ? 'SAFE (no route)' : 'CHECK AUTH');
console.log('');

console.log('═══════════════════════════════════════════════════════════');
console.log('SECURITY TEST COMPLETE');
console.log('═══════════════════════════════════════════════════════════');
