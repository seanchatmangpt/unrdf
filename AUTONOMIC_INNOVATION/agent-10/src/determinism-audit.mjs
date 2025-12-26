/**
 * Determinism Audit - Byte-Level Reproducibility Verification
 *
 * Validates that operations produce identical outputs across multiple runs
 */

import crypto from 'node:crypto';

/**
 * Validate determinism by running demo multiple times and comparing hashes
 * @param {Function} demo - Demo function to run (receives { seed })
 * @param {number} [runs=2] - Number of runs to compare
 * @returns {Promise<{deterministic: boolean, runs: number, seed: number, hashes: Object, mismatches: Array, evidence: string[]}>}
 */
export async function validateDeterminism(demo, runs = 2) {
  // Input validation
  if (typeof demo !== 'function') {
    throw new Error('demo must be a function');
  }
  if (typeof runs !== 'number' || runs < 2 || !Number.isInteger(runs)) {
    throw new Error('runs must be an integer >= 2');
  }

  const input = { demo, runs };

  const seed = 12345;
  const captures = [];

  for (let i = 0; i < input.runs; i++) {
    captures.push(await captureRun(input.demo, seed));
  }

  // Compare all hashes
  const baseline = captures[0].hashes;
  const mismatches = [];

  for (let i = 1; i < input.runs; i++) {
    const current = captures[i].hashes;

    // Check for different files
    const baselinePaths = new Set(Object.keys(baseline));
    const currentPaths = new Set(Object.keys(current));

    for (const path of baselinePaths) {
      if (!currentPaths.has(path)) {
        mismatches.push({
          path,
          issue: `Missing in run ${i + 1}`
        });
      }
    }

    for (const path of currentPaths) {
      if (!baselinePaths.has(path)) {
        mismatches.push({
          path,
          issue: `Extra in run ${i + 1}`
        });
      }
    }

    // Compare hashes for common files
    for (const path of baselinePaths) {
      if (currentPaths.has(path)) {
        if (baseline[path] !== current[path]) {
          mismatches.push({
            path,
            issue: 'Hash mismatch',
            run1: baseline[path],
            run2: current[path]
          });
        }
      }
    }
  }

  return {
    deterministic: mismatches.length === 0,
    runs: input.runs,
    seed,
    hashes: baseline,
    mismatches,
    evidence: mismatches.length === 0
      ? [`${input.runs} runs with identical hashes (${Object.keys(baseline).length} outputs)`]
      : []
  };
}

/**
 * Capture all outputs from a single run
 * @param {Function} demo - Demo function to run
 * @param {number} seed - Deterministic seed
 * @returns {Promise<{returnValue: any, outputs: Object, hashes: Object}>}
 */
async function captureRun(demo, seed) {
  const outputs = {};

  // Run the demo
  let returnValue;
  try {
    returnValue = await demo({ seed });
  } catch (error) {
    returnValue = { error: error.message };
  }

  // If demo returns outputs, capture them
  if (returnValue && typeof returnValue === 'object') {
    Object.assign(outputs, returnValue);
  }

  // Compute hashes
  const hashes = await computeHashes(outputs);

  return {
    returnValue,
    outputs,
    hashes
  };
}

/**
 * Compute SHA256 hashes for all outputs
 * @param {Object} outputs - Map of path -> content
 * @returns {Promise<Object>} Map of path -> hash
 */
async function computeHashes(outputs) {
  const hashes = {};

  for (const [path, content] of Object.entries(outputs)) {
    const normalized = normalizeContent(content);
    const hash = crypto.createHash('sha256');
    hash.update(normalized);
    hashes[path] = hash.digest('hex');
  }

  return hashes;
}

/**
 * Normalize content for consistent hashing (line endings, JSON keys, etc.)
 * @param {string|Buffer|Object} content - Content to normalize
 * @returns {string|Buffer} Normalized content
 */
function normalizeContent(content) {
  if (typeof content === 'string') {
    // Normalize line endings to LF
    return content.replace(/\r\n/g, '\n');
  }

  if (Buffer.isBuffer(content)) {
    return content;
  }

  if (typeof content === 'object' && content !== null) {
    // Canonical JSON serialization
    return JSON.stringify(content, Object.keys(content).sort());
  }

  return String(content);
}
