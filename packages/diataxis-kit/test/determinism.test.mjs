/**
 * @file Determinism tests for Diátaxis Kit
 * @description Validates that DETERMINISTIC=1 produces identical output across runs
 */

import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { readFile, readdir, rm } from 'node:fs/promises';
import { join, resolve, dirname } from 'node:path';
import { createHash } from 'node:crypto';
import { existsSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

const execFileAsync = promisify(execFile);
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const packageRoot = resolve(__dirname, '..');

let passCount = 0;
let failCount = 0;

/**
 * Test runner with timeout support
 * @param {string} name - Test name
 * @param {Function} fn - Async test function
 * @param {number} [timeout=30000] - Timeout in milliseconds
 */
async function test(name, fn, timeout = 30000) {
  const timeoutPromise = new Promise((_, reject) => {
    setTimeout(() => reject(new Error(`Test timeout after ${timeout}ms`)), timeout);
  });

  try {
    await Promise.race([fn(), timeoutPromise]);
    console.log(`✅ ${name}`);
    passCount++;
  } catch (err) {
    console.log(`❌ ${name}: ${err.message}`);
    if (err.stack && process.env.VERBOSE) {
      console.log(err.stack);
    }
    failCount++;
  }
}

/**
 * Hash a file's contents
 * @param {string} filePath - Path to file
 * @returns {Promise<string>} SHA256 hash
 */
async function hashFile(filePath) {
  const content = await readFile(filePath, 'utf8');
  return createHash('sha256').update(content, 'utf8').digest('hex');
}

/**
 * Recursively find all files in a directory
 * @param {string} dir - Directory to search
 * @param {string[]} results - Accumulator for results
 * @returns {Promise<string[]>} Array of file paths
 */
async function findAllFiles(dir, results = []) {
  if (!existsSync(dir)) {
    return results;
  }

  const entries = await readdir(dir, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = join(dir, entry.name);
    if (entry.isDirectory()) {
      await findAllFiles(fullPath, results);
    } else if (entry.isFile()) {
      results.push(fullPath);
    }
  }

  return results;
}

/**
 * Clean output directories
 * @returns {Promise<void>}
 */
async function cleanOutputs() {
  const artifactsDir = join(packageRoot, 'ARTIFACTS');
  const outDir = join(packageRoot, 'OUT');

  if (existsSync(artifactsDir)) {
    await rm(artifactsDir, { recursive: true, force: true });
  }
  if (existsSync(outDir)) {
    await rm(outDir, { recursive: true, force: true });
  }
}

/**
 * Run bin/run.mjs with DETERMINISTIC=1
 * @returns {Promise<void>}
 */
async function runPipeline() {
  const binPath = join(packageRoot, 'bin', 'run.mjs');

  await execFileAsync('node', [binPath], {
    cwd: packageRoot,
    env: {
      ...process.env,
      DETERMINISTIC: '1'
    }
  });
}

/**
 * Test: Determinism - running twice produces identical hashes
 */
async function testDeterminism() {
  // Run 1
  await cleanOutputs();
  await runPipeline();

  const artifactsDir = join(packageRoot, 'ARTIFACTS');
  const outDir = join(packageRoot, 'OUT');

  const files1 = await findAllFiles(artifactsDir);
  const hashes1 = new Map();

  for (const file of files1) {
    const hash = await hashFile(file);
    const relativePath = file.replace(artifactsDir + '/', '');
    hashes1.set(relativePath, hash);
  }

  // Sleep 100ms
  await new Promise(resolve => setTimeout(resolve, 100));

  // Run 2
  await cleanOutputs();
  await runPipeline();

  const files2 = await findAllFiles(artifactsDir);
  const hashes2 = new Map();

  for (const file of files2) {
    const hash = await hashFile(file);
    const relativePath = file.replace(artifactsDir + '/', '');
    hashes2.set(relativePath, hash);
  }

  // Compare file counts
  if (files1.length !== files2.length) {
    throw new Error(`File count mismatch: ${files1.length} vs ${files2.length}`);
  }

  // Compare hashes
  const mismatches = [];
  for (const [path, hash1] of hashes1) {
    const hash2 = hashes2.get(path);
    if (!hash2) {
      mismatches.push(`Missing in run 2: ${path}`);
    } else if (hash1 !== hash2) {
      mismatches.push(`Hash mismatch: ${path}\n  Run 1: ${hash1}\n  Run 2: ${hash2}`);
    }
  }

  if (mismatches.length > 0) {
    throw new Error(`Determinism failed:\n${mismatches.join('\n')}`);
  }

  console.log(`  Verified ${hashes1.size} files identical across runs`);
}

/**
 * Test: Consistent timestamps when DETERMINISTIC=1
 */
async function testConsistentTimestamps() {
  await cleanOutputs();
  await runPipeline();

  const artifactsDir = join(packageRoot, 'ARTIFACTS', 'diataxis');

  // Check inventory.json
  const inventoryPath = join(artifactsDir, 'inventory.json');
  const inventory = JSON.parse(await readFile(inventoryPath, 'utf8'));

  if (inventory.generatedAt !== '2000-01-01T00:00:00.000Z') {
    throw new Error(`Inventory timestamp not fixed: ${inventory.generatedAt}`);
  }

  // Check 5 random diataxis.json files
  const packageDirs = await readdir(artifactsDir, { withFileTypes: true });
  const validDirs = packageDirs.filter(d => d.isDirectory()).slice(0, 5);

  for (const dir of validDirs) {
    const diataxisPath = join(artifactsDir, dir.name, 'diataxis.json');
    if (existsSync(diataxisPath)) {
      const diataxis = JSON.parse(await readFile(diataxisPath, 'utf8'));
      if (diataxis.generatedAt !== '2000-01-01T00:00:00.000Z') {
        throw new Error(`${dir.name} timestamp not fixed: ${diataxis.generatedAt}`);
      }
    }
  }

  console.log(`  Verified timestamps fixed at 2000-01-01T00:00:00.000Z`);
}

/**
 * Test: Stable ordering in output files
 */
async function testStableOrdering() {
  await cleanOutputs();
  await runPipeline();

  const artifactsDir = join(packageRoot, 'ARTIFACTS', 'diataxis');
  const inventoryPath = join(artifactsDir, 'inventory.json');
  const inventory = JSON.parse(await readFile(inventoryPath, 'utf8'));

  // Verify packages are sorted by name
  const packageNames = inventory.packages.map(p => p.name);
  const sortedNames = [...packageNames].sort((a, b) => a.localeCompare(b));

  for (let i = 0; i < packageNames.length; i++) {
    if (packageNames[i] !== sortedNames[i]) {
      throw new Error(`Packages not sorted: ${packageNames[i]} at index ${i}, expected ${sortedNames[i]}`);
    }
  }

  // Check one diataxis.json for sorted tutorials/howtos
  const firstPackageDir = inventory.packages[0].name;
  const diataxisPath = join(artifactsDir, firstPackageDir, 'diataxis.json');

  if (existsSync(diataxisPath)) {
    const diataxis = JSON.parse(await readFile(diataxisPath, 'utf8'));

    // Verify tutorials sorted by id
    if (diataxis.tutorials && diataxis.tutorials.length > 1) {
      const tutorialIds = diataxis.tutorials.map(t => t.id);
      const sortedIds = [...tutorialIds].sort((a, b) => a.localeCompare(b));
      for (let i = 0; i < tutorialIds.length; i++) {
        if (tutorialIds[i] !== sortedIds[i]) {
          throw new Error(`Tutorials not sorted in ${firstPackageDir}`);
        }
      }
    }

    // Verify howtos sorted by id
    if (diataxis.howtos && diataxis.howtos.length > 1) {
      const howtoIds = diataxis.howtos.map(h => h.id);
      const sortedIds = [...howtoIds].sort((a, b) => a.localeCompare(b));
      for (let i = 0; i < howtoIds.length; i++) {
        if (howtoIds[i] !== sortedIds[i]) {
          throw new Error(`How-tos not sorted in ${firstPackageDir}: ${howtoIds[i]} at index ${i}, expected ${sortedIds[i]}`);
        }
      }
    }
  }

  console.log(`  Verified stable ordering for ${packageNames.length} packages`);
}

/**
 * Test: Proof hashes in generated markdown files
 */
async function testProofHashes() {
  await cleanOutputs();
  await runPipeline();

  const outDir = join(packageRoot, 'OUT');
  const allMarkdownFiles = await findAllFiles(outDir);
  const markdownFiles = allMarkdownFiles.filter(f => f.endsWith('.md'));

  if (markdownFiles.length === 0) {
    throw new Error('No markdown files generated');
  }

  let proofCount = 0;
  const invalidProofs = [];

  for (const mdFile of markdownFiles) {
    const content = await readFile(mdFile, 'utf8');

    // Parse frontmatter for proof field
    const frontmatterMatch = content.match(/^---\n([\s\S]+?)\n---/);
    if (!frontmatterMatch) {
      continue; // Skip files without frontmatter
    }

    const frontmatter = frontmatterMatch[1];
    const proofMatch = frontmatter.match(/^proof:\s*([a-f0-9]{64})$/m);

    if (proofMatch) {
      const proof = proofMatch[1];
      proofCount++;

      // Verify it's a valid 64-character hex string (SHA256)
      if (!/^[a-f0-9]{64}$/.test(proof)) {
        invalidProofs.push(`${mdFile}: invalid proof format "${proof}"`);
      }
    }
  }

  if (invalidProofs.length > 0) {
    throw new Error(`Invalid proofs:\n${invalidProofs.join('\n')}`);
  }

  console.log(`  Verified ${proofCount} proof hashes in ${markdownFiles.length} markdown files`);
}

// Run all tests
async function runTests() {
  console.log('Running determinism tests...\n');

  await test('determinism: inventory and artifacts hashes match', testDeterminism, 60000);
  await test('determinism: timestamps fixed at 2000-01-01', testConsistentTimestamps, 30000);
  await test('determinism: stable ordering', testStableOrdering, 30000);
  await test('determinism: proof hashes', testProofHashes, 30000);

  console.log(`\n${passCount}/${passCount + failCount} tests passed`);
  process.exit(failCount > 0 ? 1 : 0);
}

runTests();
