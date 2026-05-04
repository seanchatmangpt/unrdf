/**
 * Basic Filesystem Probe Example
 */

import { probeFilesystem, guardPath } from '../src/index.mjs';
import path from 'path';
import os from 'os';

// Example 1: Test guard function
console.log('=== Guard Path Tests ===\n');

const tests = [
  ['/home/user/project/file.txt', ['/home/user/project']],
  ['/etc/passwd', ['/home/user/project']],
  ['/home/user/.ssh/id_rsa', ['/home/user']],
  ['/home/user/project/.env', ['/home/user/project']],
];

for (const [testPath, roots] of tests) {
  const result = guardPath(testPath, roots);
  console.log(`Path: ${testPath}`);
  console.log(`Allowed: ${result.allowed}`);
  if (!result.allowed) {
    console.log(`Reason: ${result.reason}`);
  }
  console.log('');
}

// Example 2: Run filesystem probe
console.log('=== Filesystem Probe ===\n');

const testRoot = path.join(os.tmpdir(), 'kgc-probe-example');
const outDir = path.join(testRoot, 'out');

try {
  const observations = await probeFilesystem({
    roots: [testRoot],
    out: outDir,
    budgetMs: 5000
  });

  console.log(`Total observations: ${observations.length}\n`);

  for (const obs of observations) {
    console.log(`Method: ${obs.method}`);
    console.log(`Guard Decision: ${obs.guardDecision}`);

    if (obs.guardDecision === 'denied') {
      console.log(`Reason: ${obs.guardReason}`);
    } else if (obs.outputs) {
      console.log(`Outputs:`, JSON.stringify(obs.outputs, null, 2));
    }

    if (obs.error) {
      console.log(`Error: ${obs.error}`);
    }

    console.log(`Hash: ${obs.hash}`);
    console.log('');
  }
} catch (err) {
  console.error('Probe failed:', err.message);
}
