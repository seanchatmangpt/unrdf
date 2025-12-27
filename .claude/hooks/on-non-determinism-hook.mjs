/**
 * On-Non-Determinism Hook
 *
 * Ensures deterministic output from dynamic kgc:* blocks.
 *
 * RULES:
 * 1. Output must be deterministic (stable ordering, no timestamps, no randomness)
 * 2. Compare current output hash with expected hash from receipt
 * 3. WARN if hash mismatch; suggest debugging steps
 *
 * @module on-non-determinism-hook
 */

import {
  extractFrontmatter,
  computeContentHash,
  generateDenialReceipt,
  safeReadFile,
  checkReceiptExists,
  DENIAL_REASONS,
} from './hooks-shared.mjs';
import { readFile } from 'node:fs/promises';

// =============================================================================
// Hook Configuration
// =============================================================================

export const HOOK_NAME = 'on-non-determinism';
export const HOOK_DESCRIPTION = 'Validates deterministic output from dynamic blocks';

export const TRIGGERS = [
  'kgc:query:after',
  'kgc:proof:after',
  'kgc:render:after',
  'kgc:dynamic:after',
];

// =============================================================================
// Determinism Detection
// =============================================================================

/**
 * Patterns that indicate non-deterministic output
 */
export const NON_DETERMINISTIC_PATTERNS = [
  // Timestamps
  /\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/,  // ISO timestamps
  /\d{13,}/,  // Unix timestamps (milliseconds)
  /Date\.now\(\)/,
  /new Date\(/,

  // Randomness
  /Math\.random\(/,
  /crypto\.randomUUID\(/,
  /uuid\.v4\(/,

  // Process-specific
  /process\.pid/,
  /process\.hrtime/,

  // Unstable ordering indicators
  /Object\.keys\(/,  // May not preserve order
  /for\s+\w+\s+in\s+/,  // for-in loops unstable

  // Dynamic IDs without seeding
  /id:\s*Math\.floor\(/,
  /id:\s*\d{13,}/,
];

/**
 * Check if output contains non-deterministic patterns
 *
 * @param {string} output - Output string to check
 * @returns {{ isDeterministic: boolean, violations: Array<Object> }}
 */
export function checkDeterminism(output) {
  const violations = [];

  for (const pattern of NON_DETERMINISTIC_PATTERNS) {
    const matches = output.match(pattern);
    if (matches) {
      violations.push({
        pattern: pattern.source,
        match: matches[0],
        position: matches.index,
      });
    }
  }

  return {
    isDeterministic: violations.length === 0,
    violations,
  };
}

/**
 * Normalize output for deterministic comparison
 *
 * @param {string} output - Raw output
 * @returns {string} Normalized output
 */
export function normalizeOutput(output) {
  let normalized = output;

  // Remove trailing whitespace
  normalized = normalized.trim();

  // Normalize line endings
  normalized = normalized.replace(/\r\n/g, '\n');

  // Remove multiple consecutive blank lines
  normalized = normalized.replace(/\n\n+/g, '\n\n');

  // Sort JSON keys if output is JSON
  try {
    const parsed = JSON.parse(normalized);
    normalized = JSON.stringify(parsed, Object.keys(parsed).sort(), 2);
  } catch {
    // Not JSON, use as-is
  }

  return normalized;
}

/**
 * Extract expected hash from receipt
 *
 * @param {string} receiptId - Receipt ID
 * @returns {Promise<{ hash?: string, error?: string }>}
 */
export async function getExpectedHash(receiptId) {
  const { exists, path } = await checkReceiptExists(receiptId);

  if (!exists) {
    return { error: 'Receipt not found' };
  }

  try {
    const content = await readFile(path, 'utf-8');
    const receipt = JSON.parse(content);

    // Look for expectedHash in payload or context
    const expectedHash =
      receipt.payload?.expectedHash ||
      receipt.payload?.context?.expectedHash ||
      receipt.expectedHash;

    if (!expectedHash) {
      return { error: 'Receipt does not contain expectedHash' };
    }

    return { hash: expectedHash };
  } catch (error) {
    return { error: error.message };
  }
}

// =============================================================================
// Hash Verification
// =============================================================================

/**
 * Compare output hash with expected hash
 *
 * @param {string} output - Actual output
 * @param {string} expectedHash - Expected BLAKE3 hash
 * @returns {Promise<{ matches: boolean, actualHash: string, expectedHash: string }>}
 */
export async function verifyOutputHash(output, expectedHash) {
  const normalized = normalizeOutput(output);
  const actualHash = await computeContentHash(normalized);

  return {
    matches: actualHash === expectedHash,
    actualHash,
    expectedHash,
  };
}

// =============================================================================
// Validation Logic
// =============================================================================

/**
 * Main validation function
 *
 * @param {Object} params - Hook parameters
 * @param {string} params.filePath - Document path
 * @param {string} params.operation - Operation type
 * @param {string} params.output - Operation output
 * @param {string} [params.receiptId] - Receipt ID for hash verification
 * @param {string} [params.expectedHash] - Expected hash (overrides receipt)
 * @param {boolean} [params.dryRun=false] - Don't write denial receipt
 * @param {boolean} [params.warnOnly=true] - Only warn, don't deny
 * @returns {Promise<{ allowed: boolean, receipt?: Object, errors?: Array<string>, warnings?: Array<string> }>}
 */
export async function validate(params) {
  const {
    filePath,
    operation,
    output,
    receiptId,
    expectedHash: providedExpectedHash,
    dryRun = false,
    warnOnly = true,  // By default, only warn for determinism issues
  } = params;

  const errors = [];
  const warnings = [];

  // Step 1: Check for non-deterministic patterns
  const determinismCheck = checkDeterminism(output);

  if (!determinismCheck.isDeterministic) {
    const violationSummary = determinismCheck.violations
      .map(v => `Pattern: ${v.pattern.slice(0, 50)}... at position ${v.position}`)
      .join('\n  ');

    warnings.push(`Output contains non-deterministic patterns:\n  ${violationSummary}`);
  }

  // Step 2: Hash verification (if expected hash provided)
  let expectedHash = providedExpectedHash;

  if (!expectedHash && receiptId) {
    const hashResult = await getExpectedHash(receiptId);
    if (hashResult.hash) {
      expectedHash = hashResult.hash;
    } else {
      warnings.push(`Could not retrieve expected hash from receipt: ${hashResult.error}`);
    }
  }

  if (expectedHash) {
    const hashVerification = await verifyOutputHash(output, expectedHash);

    if (!hashVerification.matches) {
      const message = `Output hash mismatch:\n  Expected: ${hashVerification.expectedHash}\n  Actual:   ${hashVerification.actualHash}`;

      if (warnOnly) {
        warnings.push(message);
      } else {
        errors.push(message);
      }

      // Generate receipt (as warning or error)
      if (!dryRun) {
        const receipt = await generateDenialReceipt({
          operation,
          targetPath: filePath,
          reasonCode: warnOnly
            ? DENIAL_REASONS.HASH_MISMATCH
            : DENIAL_REASONS.NON_DETERMINISTIC_OUTPUT,
          message: warnOnly
            ? `Warning: Non-deterministic output detected`
            : `Operation denied: Non-deterministic output`,
          details: {
            [warnOnly ? 'warnings' : 'errors']: warnOnly ? warnings : errors,
            determinismCheck,
            hashVerification,
            operation,
            outputPreview: output.slice(0, 500),
          },
          remediation: {
            steps: [
              'Output is non-deterministic - hash does not match expected value',
              '',
              'Common causes:',
              '1. Timestamps in output (use static dates or relative times)',
              '2. Random IDs (use deterministic seeding)',
              '3. Unstable ordering (sort results explicitly)',
              '4. Process-specific values (PID, timestamps)',
              '',
              'Debugging steps:',
              '1. Review output for timestamp patterns',
              '2. Check for Math.random() or crypto.randomUUID()',
              '3. Ensure SPARQL results are sorted (ORDER BY)',
              '4. Verify JSON key ordering is stable',
              '',
              'Expected hash: ' + hashVerification.expectedHash,
              'Actual hash:   ' + hashVerification.actualHash,
              '',
              'If output is intentionally dynamic:',
              '- Remove expectedHash from frontmatter, OR',
              '- Update expectedHash with new value',
            ],
            documentation: 'docs/bb80-20-methodology.md',
          },
        });

        if (warnOnly) {
          return { allowed: true, receipt, warnings };
        } else {
          return { allowed: false, receipt, errors };
        }
      }
    }
  }

  // If only warnings and warnOnly=true, allow
  if (warnings.length > 0 && warnOnly) {
    return { allowed: true, warnings };
  }

  // If errors and not warnOnly, deny
  if (errors.length > 0 && !warnOnly) {
    return { allowed: false, errors };
  }

  // All checks passed
  return { allowed: true };
}

// =============================================================================
// Hook Executor
// =============================================================================

/**
 * Execute hook (called by Claude Code hooks system)
 *
 * @param {Object} context - Hook execution context
 * @param {string} context.operation - Operation name
 * @param {Object} context.params - Operation parameters
 * @param {string} context.output - Operation output
 * @returns {Promise<{ proceed: boolean, message?: string, receipt?: Object }>}
 */
export async function execute(context) {
  const { operation, params, output } = context;

  // Read document frontmatter to get expectedHash
  const fileRead = await safeReadFile(params.filePath || params.documentPath);
  let expectedHash = params.expectedHash;
  let receiptId = params.receiptId;

  if (fileRead.success) {
    const { frontmatter } = extractFrontmatter(fileRead.content);
    expectedHash = expectedHash || frontmatter?.expectedHash;
    receiptId = receiptId || frontmatter?.receipts?.[0];
  }

  const result = await validate({
    filePath: params.filePath || params.documentPath,
    operation,
    output,
    receiptId,
    expectedHash,
    dryRun: false,
    warnOnly: true,  // By default, only warn
  });

  if (!result.allowed) {
    return {
      proceed: false,
      message: result.errors?.join('\n') || 'Non-deterministic output detected',
      receipt: result.receipt,
    };
  }

  if (result.warnings && result.warnings.length > 0) {
    return {
      proceed: true,
      message: 'Warnings:\n' + result.warnings.join('\n'),
      receipt: result.receipt,
    };
  }

  return { proceed: true };
}

// =============================================================================
// Exports
// =============================================================================

export default {
  name: HOOK_NAME,
  description: HOOK_DESCRIPTION,
  triggers: TRIGGERS,
  execute,
  validate,
  checkDeterminism,
  normalizeOutput,
  verifyOutputHash,
  getExpectedHash,
};
