/**
 * Manual test for KGC Probe Reporter
 * Run with: node test/reporter-manual.mjs
 */

import {
  observationsToRdf,
  generateReport,
  deriveClaims,
} from '../src/reporter.mjs';

console.log('🧪 Testing KGC Probe Reporter...\n');

// Sample observations
const observations = [
  {
    method: 'probeRuntime',
    domain: 'runtime',
    timestamp: Date.now(),
    outputs: {
      node: 'v18.19.0',
      v8: '11.3.244',
      worker_threads: true,
    },
  },
  {
    method: 'probeWasm',
    domain: 'wasm',
    timestamp: Date.now() + 1000,
    outputs: {
      available: true,
      maxMemory: '2GB',
    },
  },
  {
    method: 'probeFilesystem',
    domain: 'filesystem',
    timestamp: Date.now() + 2000,
    outputs: {
      allowedPaths: ['/tmp', '/workspace'],
    },
  },
  {
    method: 'probeNetwork',
    domain: 'network',
    timestamp: Date.now() + 3000,
    guardDecision: 'denied',
    error: 'Network access blocked',
  },
];

// Test 1: observationsToRdf
console.log('Test 1: observationsToRdf');
try {
  const turtle = observationsToRdf(observations);
  console.assert(typeof turtle === 'string', 'Should return string');
  console.assert(turtle.includes('kgc:Observation'), 'Should contain kgc:Observation');
  console.assert(turtle.includes('kgc:method'), 'Should contain kgc:method');
  console.assert(turtle.includes('probeRuntime'), 'Should contain method names');
  console.log('✅ observationsToRdf passed\n');
  console.log('Sample output (first 500 chars):');
  console.log(turtle.substring(0, 500) + '...\n');
} catch (error) {
  console.error('❌ observationsToRdf failed:', error.message);
  process.exit(1);
}

// Test 2: deriveClaims
console.log('Test 2: deriveClaims');
try {
  const { capabilities, constraints } = deriveClaims(observations);
  console.assert(Array.isArray(capabilities), 'Should return capabilities array');
  console.assert(Array.isArray(constraints), 'Should return constraints array');
  console.assert(capabilities.length > 0, 'Should find some capabilities');
  console.assert(constraints.length > 0, 'Should find some constraints');

  console.log(`✅ deriveClaims passed`);
  console.log(`   Found ${capabilities.length} capabilities:`);
  capabilities.forEach(c => console.log(`   - ${c.title}`));
  console.log(`   Found ${constraints.length} constraints:`);
  constraints.forEach(c => console.log(`   - ${c.title}`));
  console.log('');
} catch (error) {
  console.error('❌ deriveClaims failed:', error.message);
  process.exit(1);
}

// Test 3: generateReport
console.log('Test 3: generateReport');
try {
  const report = generateReport(observations);
  console.assert(typeof report === 'string', 'Should return string');
  console.assert(report.includes('# KGC Probe Report'), 'Should have title');
  console.assert(report.includes('## Summary'), 'Should have summary');
  console.assert(report.includes('## Capabilities'), 'Should have capabilities');
  console.assert(report.includes('## Constraints'), 'Should have constraints');
  console.assert(report.includes('## Observations by Domain'), 'Should group by domain');
  console.log('✅ generateReport passed\n');
  console.log('Sample output (first 1000 chars):');
  console.log(report.substring(0, 1000) + '...\n');
} catch (error) {
  console.error('❌ generateReport failed:', error.message);
  process.exit(1);
}

// Test 4: Deterministic RDF output
console.log('Test 4: Deterministic RDF output');
try {
  const shuffled = [...observations].reverse();
  const turtle1 = observationsToRdf(observations);
  const turtle2 = observationsToRdf(shuffled);
  console.assert(turtle1 === turtle2, 'Should produce same output regardless of input order');
  console.log('✅ Deterministic output verified\n');
} catch (error) {
  console.error('❌ Deterministic output test failed:', error.message);
  process.exit(1);
}

// Test 5: Empty observations
console.log('Test 5: Empty observations handling');
try {
  const emptyTurtle = observationsToRdf([]);
  const emptyClaims = deriveClaims([]);
  const emptyReport = generateReport([]);

  console.assert(typeof emptyTurtle === 'string', 'Should handle empty observations');
  console.assert(emptyClaims.capabilities.length === 0, 'Should return empty capabilities');
  console.assert(emptyClaims.constraints.length === 0, 'Should return empty constraints');
  console.assert(emptyReport.includes('Total observations**: 0'), 'Report should show 0 observations');
  console.log('✅ Empty observations handled correctly\n');
} catch (error) {
  console.error('❌ Empty observations test failed:', error.message);
  process.exit(1);
}

console.log('🎉 All tests passed!');
