/**
 * KGC Probe Reporter - Usage Example
 *
 * This example demonstrates how to use the reporter module to:
 * 1. Convert observations to RDF/Turtle format
 * 2. Derive high-level capabilities and constraints
 * 3. Generate human-readable Markdown reports
 *
 * NOTE: Run after installing dependencies:
 *   pnpm install --filter @unrdf/kgc-probe
 */

// ===== Example 1: Basic Usage =====

import {
  observationsToRdf,
  generateReport,
  deriveClaims,
} from './reporter.mjs';

// Sample observations from probes
const observations = [
  {
    method: 'probeRuntime',
    domain: 'runtime',
    timestamp: Date.now(),
    outputs: {
      node: process.version,
      v8: process.versions.v8,
      worker_threads: true,
      platform: process.platform,
      arch: process.arch,
    },
  },
  {
    method: 'probeWasm',
    domain: 'wasm',
    timestamp: Date.now() + 100,
    outputs: {
      available: typeof WebAssembly !== 'undefined',
      maxMemory: '2GB',
      compileTime: 2.5,
    },
  },
  {
    method: 'probeFilesystem',
    domain: 'filesystem',
    timestamp: Date.now() + 200,
    outputs: {
      allowedPaths: ['/tmp', '/workspace', process.cwd()],
      canWrite: true,
      canRead: true,
    },
  },
  {
    method: 'probeFilesystemSensitive',
    domain: 'filesystem',
    timestamp: Date.now() + 300,
    guardDecision: 'denied',
    error: 'Access to /etc denied by filesystem guard',
  },
  {
    method: 'probeNetwork',
    domain: 'network',
    timestamp: Date.now() + 400,
    outputs: {
      allowedUrls: [
        'https://api.unrdf.dev',
        'https://data.example.org',
      ],
    },
  },
  {
    method: 'probeNetworkExternal',
    domain: 'network',
    timestamp: Date.now() + 500,
    guardDecision: 'denied',
    error: 'Network request blocked: URL not in allowlist',
  },
  {
    method: 'probeBenchmarkQuad',
    domain: 'performance',
    timestamp: Date.now() + 600,
    outputs: {
      operation: 'quad insertion',
      executionTime: 15.3,
      iterations: 10000,
      opsPerSec: 653.59,
    },
  },
  {
    method: 'probeBenchmarkQuery',
    domain: 'performance',
    timestamp: Date.now() + 700,
    outputs: {
      operation: 'SPARQL query',
      executionTime: 42.1,
      iterations: 1000,
      opsPerSec: 23.75,
    },
  },
];

// ===== Example 2: Convert to RDF/Turtle =====

console.log('='.repeat(70));
console.log('RDF/Turtle Serialization');
console.log('='.repeat(70));

const turtle = observationsToRdf(observations);
console.log(turtle);
console.log('\n');

// ===== Example 3: Derive Claims =====

console.log('='.repeat(70));
console.log('Derived Claims');
console.log('='.repeat(70));

const { capabilities, constraints } = deriveClaims(observations);

console.log('\nCAPABILITIES:');
capabilities.forEach((cap, i) => {
  console.log(`\n${i + 1}. ${cap.title}`);
  console.log(`   ${cap.description}`);
  console.log(`   Evidence: ${cap.evidence.join(', ')}`);
});

console.log('\nCONSTRAINTS:');
constraints.forEach((cons, i) => {
  console.log(`\n${i + 1}. ${cons.title}`);
  console.log(`   ${cons.description}`);
  console.log(`   Evidence: ${cons.evidence.join(', ')}`);
});

console.log('\n');

// ===== Example 4: Generate Markdown Report =====

console.log('='.repeat(70));
console.log('Markdown Report');
console.log('='.repeat(70));

const report = generateReport(observations);
console.log(report);

// ===== Example 5: Save to Files =====

import fs from 'fs/promises';
import path from 'path';

const outputDir = './output';

try {
  await fs.mkdir(outputDir, { recursive: true });

  // Save RDF
  await fs.writeFile(
    path.join(outputDir, 'observations.ttl'),
    turtle,
    'utf-8'
  );
  console.log(`\n✅ RDF saved to ${path.join(outputDir, 'observations.ttl')}`);

  // Save claims as JSON
  await fs.writeFile(
    path.join(outputDir, 'claims.json'),
    JSON.stringify({ capabilities, constraints }, null, 2),
    'utf-8'
  );
  console.log(`✅ Claims saved to ${path.join(outputDir, 'claims.json')}`);

  // Save report
  await fs.writeFile(
    path.join(outputDir, 'report.md'),
    report,
    'utf-8'
  );
  console.log(`✅ Report saved to ${path.join(outputDir, 'report.md')}\n`);
} catch (error) {
  console.error('Error saving files:', error.message);
}

// ===== Example 6: Validate Observations =====

import { ObservationSchema } from './reporter.mjs';

console.log('='.repeat(70));
console.log('Observation Validation');
console.log('='.repeat(70));

try {
  observations.forEach((obs, i) => {
    ObservationSchema.parse(obs);
    console.log(`✅ Observation ${i + 1} (${obs.method}) is valid`);
  });
  console.log('\nAll observations validated successfully!\n');
} catch (error) {
  console.error('❌ Validation failed:', error.message);
}

// ===== Example 7: Custom Observation =====

console.log('='.repeat(70));
console.log('Custom Observation Example');
console.log('='.repeat(70));

const customObs = [
  {
    method: 'probeCustomResource',
    domain: 'custom',
    timestamp: Date.now(),
    outputs: {
      resourceType: 'GPU',
      available: false,
      reason: 'Not supported in environment',
    },
    guardDecision: 'allowed',
  },
];

const customReport = generateReport(customObs);
console.log(customReport);
