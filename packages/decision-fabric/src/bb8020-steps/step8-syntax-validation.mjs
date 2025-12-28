/**
 * BB8020 Step 8: Syntax Validation
 * Real node --check validation
 */

import { execSync } from 'child_process';
import { z } from 'zod';

/**
 * Safely escape shell argument - prevents command injection
 * @param {string} str - String to escape
 * @returns {string} Escaped string safe for shell
 */
function escapeShellArg(str) {
  if (typeof str !== 'string') throw new TypeError('Argument must be a string');
  // Single quote escaping: replace ' with '\'' (end quote, escaped quote, start quote)
  return `'${str.replace(/'/g, "'\\''")}'`;
}

/**
 * Schema for validating file paths - prevent path traversal and special chars
 */
const SafeFilePathSchema = z.string()
  .refine(
    (p) => !p.includes('\0') && !p.includes('\n') && !p.includes('\r'),
    'File path contains invalid control characters'
  )
  .refine(
    (p) => !p.startsWith('-'),
    'File path cannot start with dash (could be interpreted as flag)'
  );

/**
 * Execute Step 8: Syntax validation
 * Uses node --check on generated files
 * FAIL FAST - Throws on syntax errors
 */
export async function executeStep8SyntaxValidation({ generatedFiles }) {
  const start = Date.now();

  console.log(`\n[Step 8] Validating syntax for ${generatedFiles.length} files...`);

  const errors = [];
  const validatedFiles = [];

  for (const file of generatedFiles) {
    try {
      // Validate file path to prevent command injection
      const validatedFile = SafeFilePathSchema.parse(file);
      // Use escaped argument to prevent shell injection
      const cmd = `node --check ${escapeShellArg(validatedFile)}`;
      execSync(cmd, {
        encoding: 'utf8',
        stdio: 'pipe',
        shell: '/bin/sh'
      });
      validatedFiles.push(file);
      console.log(`[Step 8] ✓ ${file}`);
    } catch (err) {
      const errorLine = extractLineNumber(err.stderr || err.message);
      errors.push({
        file,
        error: err.stderr || err.message,
        line: errorLine
      });
      console.error(`[Step 8] ✗ ${file}:${errorLine} - ${err.message}`);
    }
  }

  const valid = errors.length === 0;

  if (!valid) {
    throw new Error(`Syntax validation failed: ${errors.length} errors found`);
  }

  return {
    valid,
    errors,
    files_checked: generatedFiles.length,
    files_valid: validatedFiles.length,
    duration_ms: Date.now() - start
  };
}

/**
 * Extract line number from Node.js syntax error
 */
function extractLineNumber(stderr) {
  const match = stderr.match(/:(\d+):\d+/);
  return match ? parseInt(match[1]) : null;
}
