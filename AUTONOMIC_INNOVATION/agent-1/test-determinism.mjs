/**
 * @fileoverview Determinism validation test
 * Runs demo.mjs twice and compares output hashes
 * @module agent-1/test-determinism
 */

import { spawn } from 'node:child_process';
import { createHash } from 'node:crypto';

/**
 * Run demo and capture output
 * @returns {Promise<string>} Demo output
 */
function runDemo() {
  return new Promise((resolve, reject) => {
    const proc = spawn('node', ['demo.mjs'], {
      cwd: '/home/user/unrdf/AUTONOMIC_INNOVATION',
      stdio: 'pipe',
    });

    let stdout = '';
    let stderr = '';

    proc.stdout.on('data', data => {
      stdout += data.toString();
    });

    proc.stderr.on('data', data => {
      stderr += data.toString();
    });

    proc.on('close', code => {
      if (code !== 0) {
        reject(new Error(`Demo exited with code ${code}: ${stderr}`));
      } else {
        resolve(stdout);
      }
    });

    // Timeout after 5 seconds
    setTimeout(() => {
      proc.kill();
      reject(new Error('Demo timeout (5s)'));
    }, 5000);
  });
}

/**
 * Hash output
 * @param {string} output - Output to hash
 * @returns {string} SHA-256 hash
 */
function hashOutput(output) {
  // Remove timestamp lines for determinism
  const normalized = output
    .split('\n')
    .filter(line => !line.includes('Timestamp:'))
    .join('\n');

  return createHash('sha256')
    .update(normalized)
    .digest('hex')
    .slice(0, 16);
}

/**
 * Main test
 */
async function main() {
  console.log('Running determinism test...\n');

  try {
    console.log('Run 1...');
    const output1 = await runDemo();
    const hash1 = hashOutput(output1);
    console.log(`Demo output hash: ${hash1}\n`);

    console.log('Run 2...');
    const output2 = await runDemo();
    const hash2 = hashOutput(output2);
    console.log(`Demo output hash: ${hash2}\n`);

    if (hash1 === hash2) {
      console.log('✅ Determinism verified');
      console.log(`Hashes match: ${hash1}`);
      process.exit(0);
    } else {
      console.error('❌ Determinism FAILED');
      console.error(`Hash 1: ${hash1}`);
      console.error(`Hash 2: ${hash2}`);
      console.error('\nOutputs differ - check for non-deterministic behavior');
      process.exit(1);
    }
  } catch (err) {
    console.error('❌ Test failed:', err.message);
    process.exit(1);
  }
}

main();
