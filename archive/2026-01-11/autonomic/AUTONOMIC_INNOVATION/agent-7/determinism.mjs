/**
 * @fileoverview Deterministic code canonicalization and hashing
 * @module agent-7/determinism
 */

import crypto from 'crypto';

/**
 * Canonicalize generated code for deterministic comparison
 * Removes comments, normalizes whitespace, sorts imports
 *
 * @param {string} code - JavaScript source code
 * @returns {string} Canonicalized code
 */
export function canonicalizeGenerated(code) {
  let canonical = code;

  // 1. Remove multi-line comments /* */
  canonical = canonical.replace(/\/\*[\s\S]*?\*\//g, '');

  // 2. Remove single-line comments //
  canonical = canonical.replace(/\/\/.*$/gm, '');

  // 3. Remove JSDoc comments
  canonical = canonical.replace(/\/\*\*[\s\S]*?\*\//g, '');

  // 4. Normalize line endings to \n
  canonical = canonical.replace(/\r\n/g, '\n').replace(/\r/g, '\n');

  // 5. Convert tabs to 2 spaces
  canonical = canonical.replace(/\t/g, '  ');

  // 6. Remove trailing whitespace from each line
  canonical = canonical.replace(/[ \t]+$/gm, '');

  // 7. Collapse multiple blank lines to single blank line
  canonical = canonical.replace(/\n{3,}/g, '\n\n');

  // 8. Normalize string quotes to single quotes (simple approach)
  // This is simplified - full JS parser would be better
  canonical = canonical.replace(/"([^"\\]*(\\.[^"\\]*)*)"/g, "'$1'");

  // 9. Sort import statements alphabetically
  const lines = canonical.split('\n');
  const importLines = [];
  const otherLines = [];

  for (const line of lines) {
    if (line.trim().startsWith('import ')) {
      importLines.push(line);
    } else {
      otherLines.push(line);
    }
  }

  importLines.sort();

  // 10. Reconstruct with sorted imports at top
  canonical = [...importLines, ...otherLines].join('\n');

  // 11. Trim leading/trailing whitespace
  canonical = canonical.trim();

  // 12. Ensure single trailing newline
  canonical = canonical + '\n';

  return canonical;
}

/**
 * Hash canonicalized code using SHA-256
 *
 * @param {string} code - Source code to hash
 * @returns {string} SHA-256 hex digest
 */
export function hashGeneratedCode(code) {
  const canonical = canonicalizeGenerated(code);
  const hash = crypto.createHash('sha256');
  hash.update(canonical, 'utf8');
  return hash.digest('hex');
}

/**
 * Create deterministic hash of input parameters
 * Used to track what inputs generated what outputs
 *
 * @param {Object} spec - Service specification
 * @param {Object} profile - Compiled profile
 * @param {Object} lens - Compiled lens
 * @returns {string} SHA-256 hex digest of inputs
 */
export function hashInputs(spec, profile, lens) {
  // Sort keys to ensure deterministic JSON stringification
  const sortedSpec = JSON.stringify(spec, Object.keys(spec).sort());
  const sortedProfile = JSON.stringify(profile, Object.keys(profile).sort());
  const sortedLens = JSON.stringify(lens, Object.keys(lens).sort());

  const combined = sortedSpec + sortedProfile + sortedLens;

  const hash = crypto.createHash('sha256');
  hash.update(combined, 'utf8');
  return hash.digest('hex');
}

/**
 * Verify determinism by generating multiple times and comparing hashes
 *
 * @param {Function} generatorFn - Function that generates code
 * @param {number} [iterations=100] - Number of times to generate
 * @returns {Object} Verification result { deterministic: boolean, uniqueHashes: Set, iterations: number }
 */
export async function verifyDeterminism(generatorFn, iterations = 100) {
  const hashes = new Set();

  for (let i = 0; i < iterations; i++) {
    const result = await generatorFn();
    const hash = typeof result === 'string'
      ? hashGeneratedCode(result)
      : hashGeneratedCode(result.code);
    hashes.add(hash);
  }

  return {
    deterministic: hashes.size === 1,
    uniqueHashes: hashes,
    iterations,
    success: hashes.size === 1
  };
}

/**
 * Compare two code strings for semantic equivalence after canonicalization
 *
 * @param {string} code1 - First code string
 * @param {string} code2 - Second code string
 * @returns {boolean} True if semantically equivalent
 */
export function areEquivalent(code1, code2) {
  const canonical1 = canonicalizeGenerated(code1);
  const canonical2 = canonicalizeGenerated(code2);
  return canonical1 === canonical2;
}

/**
 * Get diff between two code strings after canonicalization
 * Returns line-by-line differences
 *
 * @param {string} code1 - First code string
 * @param {string} code2 - Second code string
 * @returns {Array<Object>} Array of diff objects { line: number, type: 'add'|'remove'|'change', content: string }
 */
export function getDiff(code1, code2) {
  const canonical1 = canonicalizeGenerated(code1);
  const canonical2 = canonicalizeGenerated(code2);

  if (canonical1 === canonical2) {
    return [];
  }

  const lines1 = canonical1.split('\n');
  const lines2 = canonical2.split('\n');
  const diff = [];

  const maxLen = Math.max(lines1.length, lines2.length);

  for (let i = 0; i < maxLen; i++) {
    const line1 = lines1[i];
    const line2 = lines2[i];

    if (line1 === undefined) {
      diff.push({ line: i + 1, type: 'add', content: line2 });
    } else if (line2 === undefined) {
      diff.push({ line: i + 1, type: 'remove', content: line1 });
    } else if (line1 !== line2) {
      diff.push({
        line: i + 1,
        type: 'change',
        before: line1,
        after: line2
      });
    }
  }

  return diff;
}

/**
 * Format diff for human-readable output
 *
 * @param {Array<Object>} diff - Diff array from getDiff
 * @returns {string} Formatted diff string
 */
export function formatDiff(diff) {
  if (diff.length === 0) {
    return 'No differences';
  }

  const lines = ['Code differences:'];
  for (const item of diff) {
    if (item.type === 'add') {
      lines.push(`+ Line ${item.line}: ${item.content}`);
    } else if (item.type === 'remove') {
      lines.push(`- Line ${item.line}: ${item.content}`);
    } else if (item.type === 'change') {
      lines.push(`~ Line ${item.line}:`);
      lines.push(`  - ${item.before}`);
      lines.push(`  + ${item.after}`);
    }
  }

  return lines.join('\n');
}
