/**
 * On-Write-Docs Hook
 *
 * Enforces poka-yoke rules when Write tool attempts to write to docs/ directory.
 *
 * RULES:
 * 1. Document must have valid frontmatter with o_hash, policy_id, receipts array
 * 2. Each receipt ID in frontmatter must exist in repo receipts/ directory
 * 3. DENY if missing frontmatter/receipts; suggest /kgc:prove
 *
 * @module on-write-docs-hook
 */

import {
  isDocsPath,
  extractFrontmatter,
  validateFrontmatter,
  validateReceipts,
  generateDenialReceipt,
  DENIAL_REASONS,
} from './hooks-shared.mjs';

// =============================================================================
// Hook Configuration
// =============================================================================

export const HOOK_NAME = 'on-write-docs';
export const HOOK_DESCRIPTION = 'Validates frontmatter and receipts on docs write';

/**
 * Hook triggers
 */
export const TRIGGERS = [
  'Write:before',  // Before Write tool executes
];

/**
 * Path patterns that trigger this hook
 */
export const PATH_PATTERNS = [
  /^\/home\/user\/unrdf\/docs\//,
  /^docs\//,
  /\.md$/,
  /\.kgcmd$/,
];

// =============================================================================
// Validation Logic
// =============================================================================

/**
 * Check if path matches trigger patterns
 *
 * @param {string} filePath - File path to check
 * @returns {boolean}
 */
export function shouldTrigger(filePath) {
  if (!isDocsPath(filePath)) {
    return false;
  }

  return PATH_PATTERNS.some(pattern => pattern.test(filePath));
}

/**
 * Validate frontmatter structure
 *
 * @param {Object} frontmatter - Parsed frontmatter
 * @returns {{ valid: boolean, errors: Array<string> }}
 */
export function validateFrontmatterStructure(frontmatter) {
  const errors = [];

  if (!frontmatter) {
    errors.push('Missing frontmatter block');
    return { valid: false, errors };
  }

  // Check required fields
  if (!frontmatter.o_hash) {
    errors.push('Missing required field: o_hash');
  }

  if (!frontmatter.policy_id) {
    errors.push('Missing required field: policy_id');
  }

  if (!frontmatter.receipts) {
    errors.push('Missing required field: receipts (array)');
  } else if (!Array.isArray(frontmatter.receipts)) {
    errors.push('Field receipts must be an array');
  }

  // Validate o_hash format (should be hex string)
  if (frontmatter.o_hash && !/^[a-fA-F0-9]{64}$/.test(frontmatter.o_hash)) {
    errors.push('o_hash must be a 64-character hex string (BLAKE3)');
  }

  // Validate policy_id format
  if (frontmatter.policy_id && frontmatter.policy_id.length < 3) {
    errors.push('policy_id must be at least 3 characters');
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

/**
 * Main validation function
 *
 * @param {Object} params - Hook parameters
 * @param {string} params.filePath - Target file path
 * @param {string} params.content - Content to write
 * @param {boolean} [params.dryRun=false] - If true, don't write denial receipt
 * @returns {Promise<{ allowed: boolean, receipt?: Object, errors?: Array<string> }>}
 */
export async function validate(params) {
  const { filePath, content, dryRun = false } = params;

  // Check if this hook should trigger
  if (!shouldTrigger(filePath)) {
    return { allowed: true };
  }

  const errors = [];

  // Step 1: Extract frontmatter
  const { frontmatter, content: bodyContent } = extractFrontmatter(content);

  // Step 2: Validate frontmatter structure
  const structureValidation = validateFrontmatterStructure(frontmatter);
  if (!structureValidation.valid) {
    errors.push(...structureValidation.errors);

    // Generate denial receipt
    if (!dryRun) {
      const receipt = await generateDenialReceipt({
        operation: 'write',
        targetPath: filePath,
        reasonCode: frontmatter
          ? DENIAL_REASONS.INVALID_FRONTMATTER
          : DENIAL_REASONS.MISSING_FRONTMATTER,
        message: `Document write denied: ${errors.join('; ')}`,
        details: {
          errors,
          hasFrontmatter: !!frontmatter,
          frontmatter,
        },
        remediation: {
          command: '/kgc:prove',
          steps: [
            'Add valid frontmatter to the document:',
            '  ---',
            '  o_hash: <64-char-blake3-hash>',
            '  policy_id: <policy-identifier>',
            '  receipts: []',
            '  ---',
            '',
            'Or use /kgc:prove to generate receipts and frontmatter automatically',
          ],
          documentation: 'docs/chapter-5-receipts-chaining-verification.md',
        },
      });

      return { allowed: false, receipt, errors };
    }

    return { allowed: false, errors };
  }

  // Step 3: Validate frontmatter against schema
  const schemaValidation = validateFrontmatter(frontmatter);
  if (!schemaValidation.valid) {
    errors.push(`Schema validation failed: ${schemaValidation.error}`);

    if (!dryRun) {
      const receipt = await generateDenialReceipt({
        operation: 'write',
        targetPath: filePath,
        reasonCode: DENIAL_REASONS.INVALID_FRONTMATTER,
        message: `Frontmatter schema validation failed: ${schemaValidation.error}`,
        details: {
          errors,
          frontmatter,
          schemaError: schemaValidation.error,
        },
        remediation: {
          command: '/kgc:refresh',
          steps: [
            'Fix frontmatter schema errors',
            'Ensure all required fields are present and correctly typed',
            'Use /kgc:refresh to regenerate frontmatter',
          ],
          documentation: 'docs/chapter-5-receipts-chaining-verification.md',
        },
      });

      return { allowed: false, receipt, errors };
    }

    return { allowed: false, errors };
  }

  // Step 4: Validate receipts exist
  const receiptValidation = await validateReceipts(frontmatter.receipts || []);
  if (!receiptValidation.valid) {
    if (receiptValidation.missing.length > 0) {
      errors.push(`Missing receipts: ${receiptValidation.missing.join(', ')}`);
    }
    if (receiptValidation.invalid.length > 0) {
      errors.push(`Invalid receipts: ${receiptValidation.invalid.join(', ')}`);
    }

    if (!dryRun) {
      const receipt = await generateDenialReceipt({
        operation: 'write',
        targetPath: filePath,
        reasonCode: DENIAL_REASONS.RECEIPT_NOT_FOUND,
        message: `Referenced receipts not found or invalid`,
        details: {
          errors,
          missingReceipts: receiptValidation.missing,
          invalidReceipts: receiptValidation.invalid,
          totalReceipts: frontmatter.receipts?.length || 0,
        },
        remediation: {
          command: '/kgc:prove',
          steps: [
            'Generate missing receipts:',
            '  /kgc:prove <document-path>',
            '',
            'Or remove invalid receipt IDs from frontmatter',
            `Missing: ${receiptValidation.missing.join(', ')}`,
            `Invalid: ${receiptValidation.invalid.join(', ')}`,
          ],
          documentation: 'docs/chapter-5-receipts-chaining-verification.md',
        },
      });

      return { allowed: false, receipt, errors };
    }

    return { allowed: false, errors };
  }

  // All validations passed
  return { allowed: true };
}

// =============================================================================
// Hook Executor
// =============================================================================

/**
 * Execute hook (called by Claude Code hooks system)
 *
 * @param {Object} context - Hook execution context
 * @param {string} context.tool - Tool name ('Write')
 * @param {Object} context.params - Tool parameters
 * @param {string} context.params.file_path - Target file path
 * @param {string} context.params.content - Content to write
 * @returns {Promise<{ proceed: boolean, message?: string, receipt?: Object }>}
 */
export async function execute(context) {
  const { tool, params } = context;

  if (tool !== 'Write') {
    return { proceed: true };
  }

  const result = await validate({
    filePath: params.file_path,
    content: params.content,
    dryRun: false,
  });

  if (!result.allowed) {
    return {
      proceed: false,
      message: result.errors?.join('\n') || 'Validation failed',
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
  shouldTrigger,
};
