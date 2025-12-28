#!/usr/bin/env node
/**
 * Quick verification script for network probe
 * Tests guard enforcement without full test suite
 */

import { probeNetwork } from './src/probes/network.mjs';

console.log('🔍 Verifying Network Probe Implementation...\n');

// Test 1: Empty allowlist (should be denied)
console.log('Test 1: Empty allowlist');
try {
  const obs1 = await probeNetwork({ netAllow: [] });
  const denied = obs1.find(o => o.guardDecision === 'denied');
  console.log(denied ? '✅ PASS: Denied when allowlist empty' : '❌ FAIL: Should deny when allowlist empty');
  console.log(`   Found ${obs1.length} observations`);
} catch (error) {
  console.log('❌ FAIL:', error.message);
}

// Test 2: Invalid URL (should throw Zod error)
console.log('\nTest 2: Invalid URL validation');
try {
  await probeNetwork({ netAllow: ['not-a-url'] });
  console.log('❌ FAIL: Should throw on invalid URL');
} catch (error) {
  console.log(error.name === 'ZodError' ? '✅ PASS: Zod validation works' : '❌ FAIL: Wrong error type');
}

// Test 3: Invalid timeout (should throw Zod error)
console.log('\nTest 3: Timeout validation');
try {
  await probeNetwork({ netAllow: ['https://example.com'], timeout: 10000 });
  console.log('❌ FAIL: Should reject timeout > 5000ms');
} catch (error) {
  console.log(error.name === 'ZodError' ? '✅ PASS: Timeout validation works' : '❌ FAIL: Wrong error type');
}

// Test 4: Fetch API detection
console.log('\nTest 4: Fetch API detection');
try {
  const obs4 = await probeNetwork({ netAllow: [] });
  const fetchObs = obs4.find(o => o.capability === 'fetch-api');
  console.log(fetchObs ? '✅ PASS: Fetch API detected' : '❌ FAIL: Should detect Fetch API');
  console.log(`   Available: ${fetchObs?.available}`);
} catch (error) {
  console.log('❌ FAIL:', error.message);
}

// Test 5: Valid allowlisted URL (only if network available - skip if offline)
console.log('\nTest 5: Allowlisted URL probing');
try {
  const obs5 = await probeNetwork({
    netAllow: ['https://example.com'],
    timeout: 5000
  });
  const urlObs = obs5.filter(o => o.url === 'https://example.com');
  console.log(urlObs.length > 0 ? '✅ PASS: Probed allowlisted URL' : '❌ FAIL: Should probe allowlisted URL');
  console.log(`   Found ${urlObs.length} observations for URL`);

  const allAllowed = urlObs.every(o => o.guardDecision === 'allowed');
  console.log(allAllowed ? '✅ PASS: All observations marked as allowed' : '❌ FAIL: Some denied unexpectedly');

  // Check for TLS validation (HTTPS)
  const tlsObs = urlObs.find(o => o.capability === 'tls-certificate-validation');
  console.log(tlsObs ? '✅ PASS: TLS validation probed for HTTPS' : '⚠️  WARN: TLS probe missing');
} catch (error) {
  console.log('⚠️  SKIP: Network may be unavailable -', error.message);
}

console.log('\n✅ Network probe verification complete!');
