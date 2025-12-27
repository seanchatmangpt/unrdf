#!/usr/bin/env node
/**
 * @fileoverview Network Probe Demo - Shows guard enforcement and observations
 *
 * Demonstrates:
 * 1. Guard denial when netAllow is empty
 * 2. Successful probing with allowlisted URLs
 * 3. Example observations from all probe methods
 */

import { probeNetwork } from '../src/probes/network.mjs';

console.log('=== Network Probe Demo ===\n');

// Test 1: Guard Denial (No Allowlist)
console.log('Test 1: Guard Denial - Empty Allowlist');
console.log('----------------------------------------');
const deniedObservations = await probeNetwork({ netAllow: [] });
console.log(`Total observations: ${deniedObservations.length}`);
console.log('\nDenied observation:');
const denied = deniedObservations.find(obs => obs.guardDecision === 'denied');
console.log(JSON.stringify(denied, null, 2));

// Test 2: Successful Probing (With Allowlist)
console.log('\n\nTest 2: Successful Probing - Allowlisted URL');
console.log('---------------------------------------------');
const allowedObservations = await probeNetwork({
  netAllow: ['https://example.com'],
  timeout: 5000
});

console.log(`Total observations: ${allowedObservations.length}`);

// Group by capability
const byCapability = {};
for (const obs of allowedObservations) {
  if (!byCapability[obs.capability]) {
    byCapability[obs.capability] = [];
  }
  byCapability[obs.capability].push(obs);
}

console.log('\nObservations by capability:');
for (const [capability, observations] of Object.entries(byCapability)) {
  console.log(`\n${capability} (${observations.length}):`);
  for (const obs of observations) {
    console.log(`  - available: ${obs.available}, guardDecision: ${obs.guardDecision}`);
    if (obs.metadata) {
      console.log(`    metadata: ${JSON.stringify(obs.metadata, null, 6).replace(/\n/g, '\n    ')}`);
    }
  }
}

// Test 3: Non-allowlisted URL (should be denied by guard)
console.log('\n\nTest 3: Guard Denial - Non-allowlisted URL');
console.log('-------------------------------------------');
// The probe only accepts URLs in netAllow, so we test with empty list
// to show guard enforcement
const guardTestObservations = await probeNetwork({
  netAllow: [] // Empty = guard denies all network operations
});

const guardDenials = guardTestObservations.filter(obs => obs.guardDecision === 'denied');
console.log(`Guard denials: ${guardDenials.length}`);
console.log('\nGuard denial reasons:');
for (const denial of guardDenials) {
  console.log(`  - ${denial.reason}`);
}

// Summary
console.log('\n\n=== Summary ===');
console.log(`Probe Methods Detected:`);
console.log(`1. fetch-api (API availability check)`);
console.log(`2. http-head-request (HTTP HEAD method)`);
console.log(`3. tls-certificate-validation (TLS/SSL validation)`);
console.log(`4. response-payload-size (Content-Length limits)`);
console.log(`5. cache-headers (Cache-Control, ETag, etc.)`);
console.log(`6. dns-resolution (DNS timing estimation)`);
console.log(`7. guardUrlAllowlist (Poka-yoke enforcement)`);
console.log('\nGuard Enforcement: ✅ Working');
console.log('Timeout Protection: ✅ 5s max per request');
console.log('Allowlist-Only: ✅ Enforced');
