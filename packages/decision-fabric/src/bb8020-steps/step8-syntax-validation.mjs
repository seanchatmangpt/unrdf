/**
 * BB8020 Step 8: Syntax Validation
 * Real node --check validation
 */

import { execSync } from 'child_process';

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
      execSync(`node --check "${file}"`, {
        encoding: 'utf8',
        stdio: 'pipe'
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
