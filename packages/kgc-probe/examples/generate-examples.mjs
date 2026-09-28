#!/usr/bin/env node
/**
 * @fileoverview Generate example RDF/Turtle and Markdown outputs
 *
 * Demonstrates the RDF converter and Markdown reporter using sample observations.
 */

import { readFile, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { convertToTurtle } from '../src/reporters/rdf.mjs';
import { renderReport } from '../src/reporters/markdown.mjs';

// Sample observations (subset from actual probe output)
const sampleObservations = [
  {
    method: 'concurrency.worker_threads_available',
    domain: 'concurrency',
    outputs: {
      available: true,
      module: 'worker_threads',
      nodeVersion: 'v22.21.1'
    },
    timestamp: 1766824576989,
    guardDecision: 'allowed',
    metadata: {
      minVersion: 'v10.5.0'
    }
  },
  {
    method: 'concurrency.shared_array_buffer',
    domain: 'concurrency',
    outputs: {
      available: true,
      testSize: 8,
      functional: true
    },
    timestamp: 1766824576989,
    guardDecision: 'allowed'
  },
  {
    method: 'concurrency.event_loop_latency',
    domain: 'concurrency',
    outputs: {
      mean: 2.636688799999999,
      median: 0.07676700000001802,
      p95: 22.712030999999996,
      min: 0.01639599999998609,
      max: 22.712030999999996,
      stddev: 6.757490655403836,
      unit: 'ms',
      samples: 10
    },
    timestamp: 1766824576989,
    guardDecision: 'allowed'
  },
  {
    method: 'runtime.node_version',
    domain: 'runtime',
    outputs: {
      nodeVersion: 'v22.21.1',
      platform: 'linux',
      arch: 'x64'
    },
    timestamp: 1766824576988,
    guardDecision: 'allowed'
  },
  {
    method: 'wasm.compilation_available',
    domain: 'wasm',
    outputs: {
      available: true,
      wasm: true,
      maxMemory: 4294967296
    },
    timestamp: 1766824576990,
    guardDecision: 'allowed'
  },
  {
    method: 'filesystem.access_denied',
    domain: 'filesystem',
    outputs: {
      path: '/etc/passwd',
      error: 'Access denied by guard'
    },
    timestamp: 1766824576991,
    guardDecision: 'denied',
    error: 'Filesystem access outside allowed roots'
  },
  {
    method: 'concurrency.stack_depth',
    domain: 'concurrency',
    outputs: {
      maxStackDepth: 8946,
      hitGuardLimit: false,
      guardLimit: 10000
    },
    timestamp: 1766824576997,
    guardDecision: 'allowed',
    metadata: {
      errorType: 'RangeError',
      note: 'Recursion bounded to prevent VM crash'
    }
  },
  {
    method: 'network.fetch_blocked',
    domain: 'network',
    outputs: {
      url: 'https://evil.example.com',
      error: 'URL not in allowlist'
    },
    timestamp: 1766824576992,
    guardDecision: 'denied',
    error: 'Network request blocked by guard'
  }
];

async function main() {
  console.log('[Example Generator] Generating example outputs...\n');

  // Generate Turtle output
  console.log('[1/3] Converting to RDF/Turtle...');
  const turtle = await convertToTurtle(sampleObservations);
  const turtlePath = join(import.meta.dirname, 'example-output.ttl');
  await writeFile(turtlePath, turtle, 'utf-8');
  console.log(`✅ Turtle written to: ${turtlePath}`);
  console.log(`   Size: ${turtle.length} bytes\n`);

  // Generate Markdown report
  console.log('[2/3] Rendering Markdown report...');
  const report = renderReport(sampleObservations);
  const reportPath = join(import.meta.dirname, 'example-report.md');
  await writeFile(reportPath, report, 'utf-8');
  console.log(`✅ Report written to: ${reportPath}`);
  console.log(`   Size: ${report.length} bytes\n`);

  // Validate Turtle with N3.js
  console.log('[3/3] Validating Turtle with N3.js...');
  try {
    const N3 = await import('n3');
    const parser = new N3.Parser();
    const quads = [];

    await new Promise((resolve, reject) => {
      parser.parse(turtle, (error, quad) => {
        if (error) {
          reject(error);
        } else if (quad) {
          quads.push(quad);
        } else {
          resolve();
        }
      });
    });

    console.log(`✅ Turtle is valid!`);
    console.log(`   Parsed ${quads.length} quads\n`);

    // Show sample quads
    console.log('Sample quads (first 5):');
    for (let i = 0; i < Math.min(5, quads.length); i++) {
      const q = quads[i];
      console.log(`  ${i + 1}. ${q.subject.value} ${q.predicate.value} ${q.object.value}`);
    }
  } catch (err) {
    console.error('❌ Turtle validation failed:', err.message);
    process.exit(1);
  }

  console.log('\n[Example Generator] Done!');
  console.log('\nOutput files:');
  console.log(`  - ${turtlePath}`);
  console.log(`  - ${reportPath}`);
}

main().catch(err => {
  console.error('Fatal error:', err);
  process.exit(1);
});
