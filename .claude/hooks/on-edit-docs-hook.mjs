/**
 * On-Edit-Docs Hook
 *
 * Enforces poka-yoke rules when Edit tool modifies content in docs/ directory.
 *
 * RULES:
 * 1. If content changes, o_hash must be updated to reflect new universe snapshot
 * 2. Validate new o_hash matches declared universe state (via receipts)
 * 3. DENY if o_hash unchanged after content edit; suggest /kgc:refresh
 *
 * @module on-edit-docs-hook
 */

import { readFile } from 'node:fs/promises';
import {
  isDocsPath,
  extractFrontmatter,
  validateFrontmatter,
  computeContentHash,
  extractBodyContent,
  generateDenialReceipt,
  safeReadFile,
  DENIAL_REASONS,
} from './hooks-shared.mjs';

// =============================================================================
// Hook Configuration
// =============================================================================

export const HOOK_NAME = 'on-edit-docs';
export const HOOK_DESCRIPTION = 'Validates o_hash updates on content edits';

export const TRIGGERS = [
  'Edit:before',
];

export const PATH_PATTERNS = [
  /^\/home\/user\/unrdf\/docs\//,
  /^docs\//,
  /\.md$/,
  /\.kgcmd$/,
];

// =============================================================================
// Content Comparison
// =============================================================================

/**
 * Detect if edit modifies frontmatter or body content
 *
 * @param {string} oldContent - Original content
 * @param {string} oldString - String being replaced
 * @param {string} newString - Replacement string
 * @returns {{ modifiesFrontmatter: boolean, modifiesBody: boolean }}
 */
export function detectEditScope(oldContent, oldString, newString) {
  const { frontmatter, raw, content: bodyContent } = extractFrontmatter(oldContent);

  // Check if old_string is in frontmatter
  const modifiesFrontmatter = raw.includes(oldString);

  // Check if old_string is in body
  const modifiesBody = bodyContent.includes(oldString);

  return { modifiesFrontmatter, modifiesBody };
}

/**
 * Apply edit to content
 *
 * @param {string} content - Original content
 * @param {string} oldString - String to replace
 * @param {string} newString - Replacement string
 * @param {boolean} replaceAll - Replace all occurrences
 * @returns {string} Modified content
 */
export function applyEdit(content, oldString, newString, replaceAll = false) {
  if (replaceAll) {
    return content.replaceAll(oldString, newString);
  }

  return content.replace(oldString, newString);
}

/**
 * Check if o_hash changed between old and new frontmatter
 *
 * @param {Object} oldFrontmatter - Original frontmatter
 * @param {Object} newFrontmatter - New frontmatter
 * @returns {boolean}
 */
export function oHashChanged(oldFrontmatter, newFrontmatter) {
  const oldHash = oldFrontmatter?.o_hash || '';
  const newHash = newFrontmatter?.o_hash || '';

  return oldHash !== newHash;
}

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
 * Main validation function
 *
 * @param {Object} params - Hook parameters
 * @param {string} params.filePath - Target file path
 * @param {string} params.oldString - String being replaced
 * @param {string} params.newString - Replacement string
 * @param {boolean} [params.replaceAll=false] - Replace all occurrences
 * @param {boolean} [params.dryRun=false] - Don't write denial receipt
 * @returns {Promise<{ allowed: boolean, receipt?: Object, errors?: Array<string>, warnings?: Array<string> }>}
 */
export async function validate(params) {
  const { filePath, oldString, newString, replaceAll = false, dryRun = false } = params;

  // Check if this hook should trigger
  if (!shouldTrigger(filePath)) {
    return { allowed: true };
  }

  const errors = [];
  const warnings = [];

  // Step 1: Read current file content
  const fileRead = await safeReadFile(filePath);
  if (!fileRead.success) {
    // File doesn't exist - this might be a new file, let on-write-docs handle it
    return { allowed: true };
  }

  const oldContent = fileRead.content;

  // Step 2: Detect edit scope
  const editScope = detectEditScope(oldContent, oldString, newString);

  // If editing only frontmatter (like updating o_hash), allow it
  if (editScope.modifiesFrontmatter && !editScope.modifiesBody) {
    return { allowed: true };
  }

  // If not editing body, no validation needed
  if (!editScope.modifiesBody) {
    return { allowed: true };
  }

  // Step 3: Extract old frontmatter
  const { frontmatter: oldFrontmatter } = extractFrontmatter(oldContent);

  if (!oldFrontmatter) {
    // No frontmatter - let on-write-docs handle this
    warnings.push('Document has no frontmatter, skipping o_hash validation');
    return { allowed: true, warnings };
  }

  // Step 4: Apply edit and extract new frontmatter
  const newContent = applyEdit(oldContent, oldString, newString, replaceAll);
  const { frontmatter: newFrontmatter } = extractFrontmatter(newContent);

  // Step 5: Check if o_hash changed
  const hashChanged = oHashChanged(oldFrontmatter, newFrontmatter);

  if (!hashChanged) {
    // Content changed but o_hash did not - this is the error condition
    errors.push('Content modified but o_hash unchanged');
    errors.push('Universe snapshot must be updated to reflect content changes');

    if (!dryRun) {
      const receipt = await generateDenialReceipt({
        operation: 'edit',
        targetPath: filePath,
        reasonCode: DENIAL_REASONS.O_HASH_UNCHANGED,
        message: 'Edit denied: Content changed but o_hash not updated',
        details: {
          errors,
          oldHash: oldFrontmatter.o_hash,
          newHash: newFrontmatter.o_hash,
          editScope,
          editSummary: {
            oldString: oldString.slice(0, 100),
            newString: newString.slice(0, 100),
            replaceAll,
          },
        },
        remediation: {
          command: '/kgc:refresh',
          steps: [
            'Content edits require o_hash update to maintain universe consistency',
            '',
            'Option 1: Use /kgc:refresh to auto-update o_hash',
            '  /kgc:refresh ' + filePath,
            '',
            'Option 2: Manually update o_hash in frontmatter',
            '  1. Compute new universe state hash',
            '  2. Update o_hash: <new-hash>',
            '  3. Generate new receipt with /kgc:prove',
            '',
            'Current o_hash: ' + oldFrontmatter.o_hash,
          ],
          documentation: 'docs/chapter-5-receipts-chaining-verification.md',
        },
      });

      return { allowed: false, receipt, errors };
    }

    return { allowed: false, errors };
  }

  // Step 6: Validate new frontmatter
  const validation = validateFrontmatter(newFrontmatter);
  if (!validation.valid) {
    errors.push(`New frontmatter invalid: ${validation.error}`);

    if (!dryRun) {
      const receipt = await generateDenialReceipt({
        operation: 'edit',
        targetPath: filePath,
        reasonCode: DENIAL_REASONS.INVALID_FRONTMATTER,
        message: 'Edit denied: Updated frontmatter is invalid',
        details: {
          errors,
          validationError: validation.error,
          newFrontmatter,
        },
        remediation: {
          command: '/kgc:refresh',
          steps: [
            'The updated frontmatter has schema errors',
            'Use /kgc:refresh to regenerate valid frontmatter',
            '',
            'Error: ' + validation.error,
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
 * @param {string} context.tool - Tool name ('Edit')
 * @param {Object} context.params - Tool parameters
 * @param {string} context.params.file_path - Target file path
 * @param {string} context.params.old_string - String being replaced
 * @param {string} context.params.new_string - Replacement string
 * @param {boolean} context.params.replace_all - Replace all occurrences
 * @returns {Promise<{ proceed: boolean, message?: string, receipt?: Object }>}
 */
export async function execute(context) {
  const { tool, params } = context;

  if (tool !== 'Edit') {
    return { proceed: true };
  }

  const result = await validate({
    filePath: params.file_path,
    oldString: params.old_string,
    newString: params.new_string,
    replaceAll: params.replace_all || false,
    dryRun: false,
  });

  if (!result.allowed) {
    return {
      proceed: false,
      message: result.errors?.join('\n') || 'Validation failed',
      receipt: result.receipt,
    };
  }

  if (result.warnings && result.warnings.length > 0) {
    return {
      proceed: true,
      message: 'Warning: ' + result.warnings.join('\n'),
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
