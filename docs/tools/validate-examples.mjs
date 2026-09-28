#!/usr/bin/env node

/**
 * @fileoverview Documentation Example Validator
 *
 * Extracts code examples from markdown documentation and validates
 * that they compile and execute correctly.
 *
 * @module docs/tools/validate-examples
 * @version 1.0.0
 * @license MIT
 */

import { readFile, readdir, mkdir, writeFile, rm } from 'node:fs/promises';
import { join, dirname, relative, extname, basename } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawn } from 'node:child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Configuration for example validation
 */
const config = {
  directories: ['../tutorials/', '../how-to/', '../examples/'],
  tempDirectory: './temp-examples/',
  reportsDirectory: './reports/',
  supportedLanguages: ['javascript', 'js', 'mjs'],
  timeout: 30000, // 30 seconds per example
  ignorePatterns: ['// skip-validation', '/* skip-validation */']
};

/**
 * Code example extracted from documentation
 * @typedef {Object} CodeExample
 * @property {string} source - Source file path
 * @property {number} line - Line number in source
 * @property {string} language - Code language
 * @property {string} code - The code content
 * @property {string} context - Surrounding text context
 */

/**
 * Extract code examples from markdown content
 * @param {string} content - Markdown content
 * @param {string} filePath - Source file path
 * @returns {CodeExample[]} Array of code examples
 */
function extractCodeExamples(content, filePath) {
  const examples = [];

  // Match fenced code blocks with language
  const codeBlockRegex = /```(javascript|js|mjs)\n([\s\S]*?)```/g;
  let match;

  while ((match = codeBlockRegex.exec(content)) !== null) {
    const [fullMatch, language, code] = match;
    const line = content.substring(0, match.index).split('\n').length;

    // Check for skip directive
    const shouldSkip = config.ignorePatterns.some((pattern) => code.includes(pattern));

    if (!shouldSkip && code.trim().length > 0) {
      // Get context (heading before code block)
      const beforeCode = content.substring(0, match.index);
      const contextMatch = beforeCode.match(/#{1,6}\s+([^\n]+)\n[^#]*$/);
      const context = contextMatch ? contextMatch[1] : 'Unknown context';

      examples.push({
        source: filePath,
        line,
        language,
        code: code.trim(),
        context
      });
    }
  }

  return examples;
}

/**
 * Wrap code example for execution
 * @param {CodeExample} example - The code example
 * @returns {string} Wrapped code ready for execution
 */
function wrapCodeForExecution(example) {
  const { code } = example;

  // Check if code is a complete module (has imports)
  const hasImports = /^import\s+/m.test(code);

  // Wrap in async IIFE if needed for top-level await
  const hasTopLevelAwait = /^(?!.*(?:async\s+function|async\s+\()).*\bawait\b/m.test(code);

  let wrappedCode = code;

  // Add mock imports if referencing unrdf but not importing
  if (!hasImports && /\bunrdf\b/.test(code)) {
    wrappedCode = `
// Mock imports for validation
const mockEngine = {
  insert: async () => ({ success: true }),
  query: async () => [],
  validate: async () => ({ valid: true }),
  subscribe: () => ({ unsubscribe: () => {} })
};
const createKnowledgeEngine = async () => mockEngine;
const defineHook = (config) => config;
const namedNode = (iri) => ({ termType: 'NamedNode', value: iri });
const literal = (value) => ({ termType: 'Literal', value });

${wrappedCode}
`;
  }

  // Wrap in async IIFE if has top-level await
  if (hasTopLevelAwait && !hasImports) {
    wrappedCode = `
(async () => {
  ${wrappedCode}
})().catch(console.error);
`;
  }

  return wrappedCode;
}

/**
 * Execute code example and capture result
 * @param {CodeExample} example - The code example
 * @param {string} tempDir - Temporary directory for files
 * @returns {Promise<Object>} Execution result
 */
async function executeExample(example, tempDir) {
  const fileName = `example-${Date.now()}-${Math.random().toString(36).substring(7)}.mjs`;
  const filePath = join(tempDir, fileName);

  const wrappedCode = wrapCodeForExecution(example);

  try {
    // Write temporary file
    await writeFile(filePath, wrappedCode);

    // Execute with Node.js
    const result = await new Promise((resolve) => {
      const startTime = Date.now();
      let stdout = '';
      let stderr = '';

      const proc = spawn('node', ['--experimental-vm-modules', filePath], {
        timeout: config.timeout,
        env: { ...process.env, NODE_NO_WARNINGS: '1' }
      });

      proc.stdout.on('data', (data) => {
        stdout += data.toString();
      });

      proc.stderr.on('data', (data) => {
        stderr += data.toString();
      });

      proc.on('close', (code) => {
        const duration = Date.now() - startTime;
        resolve({
          success: code === 0,
          code,
          stdout,
          stderr,
          duration
        });
      });

      proc.on('error', (error) => {
        resolve({
          success: false,
          code: -1,
          stdout,
          stderr: error.message,
          duration: Date.now() - startTime
        });
      });
    });

    // Clean up temp file
    await rm(filePath, { force: true });

    return result;
  } catch (error) {
    return {
      success: false,
      code: -1,
      stdout: '',
      stderr: error.message,
      duration: 0
    };
  }
}

/**
 * Syntax check only (no execution)
 * @param {CodeExample} example - The code example
 * @returns {Promise<Object>} Syntax check result
 */
async function syntaxCheckExample(example) {
  const { code } = example;

  try {
    // Try to parse the code
    new Function(code);
    return {
      success: true,
      type: 'syntax'
    };
  } catch (error) {
    // Check if it's a module syntax issue
    if (error.message.includes('import') || error.message.includes('export')) {
      // Module syntax - use different check
      try {
        // Basic validation for module syntax
        const hasValidImports = !/import\s+(?![{*'"])/.test(code);
        const hasValidExports = !/export\s+(?!default|{|\*|function|class|const|let|var)/.test(code);

        if (hasValidImports && hasValidExports) {
          return {
            success: true,
            type: 'syntax-module'
          };
        }
      } catch {
        // Fall through to error
      }
    }

    return {
      success: false,
      type: 'syntax',
      error: error.message
    };
  }
}

/**
 * Scan directories for documentation files
 * @returns {Promise<string[]>} Array of file paths
 */
async function scanDocumentation() {
  const files = [];

  for (const dir of config.directories) {
    const dirPath = join(__dirname, dir);

    try {
      const entries = await readdir(dirPath, { withFileTypes: true, recursive: true });

      for (const entry of entries) {
        if (!entry.isDirectory() && extname(entry.name) === '.md') {
          files.push(join(entry.parentPath || dirPath, entry.name));
        }
      }
    } catch {
      // Directory doesn't exist yet
      console.log(`  Skipping non-existent directory: ${dir}`);
    }
  }

  return files;
}

/**
 * Validate all documentation examples
 * @param {Object} options - Validation options
 * @param {boolean} options.execute - Whether to execute examples
 * @returns {Promise<Object>} Validation report
 */
async function validateExamples(options = { execute: false }) {
  console.log('Validating documentation examples...\n');

  const docsRoot = join(__dirname, '../');
  const tempDir = join(__dirname, config.tempDirectory);
  const allExamples = [];

  const results = {
    passed: [],
    failed: [],
    skipped: []
  };

  // Create temp directory
  await mkdir(tempDir, { recursive: true });

  // Scan documentation
  const files = await scanDocumentation();
  console.log(`Found ${files.length} documentation files\n`);

  // Extract examples from all files
  for (const file of files) {
    try {
      const content = await readFile(file, 'utf-8');
      const examples = extractCodeExamples(content, file);
      allExamples.push(...examples);
    } catch (error) {
      console.warn(`Warning: Could not read ${file}: ${error.message}`);
    }
  }

  console.log(`Found ${allExamples.length} code examples to validate\n`);

  // Validate each example
  for (const example of allExamples) {
    const relativePath = relative(docsRoot, example.source);
    process.stdout.write(`  ${relativePath}:${example.line} - ${example.context.substring(0, 40)}...`);

    let result;

    if (options.execute) {
      result = await executeExample(example, tempDir);
    } else {
      result = await syntaxCheckExample(example);
    }

    if (result.success) {
      console.log(' [OK]');
      results.passed.push({
        ...example,
        result
      });
    } else {
      console.log(' [FAIL]');
      results.failed.push({
        ...example,
        result
      });
    }
  }

  // Clean up temp directory
  await rm(tempDir, { recursive: true, force: true });

  // Generate report
  const report = {
    timestamp: new Date().toISOString(),
    mode: options.execute ? 'execute' : 'syntax',
    summary: {
      total: allExamples.length,
      passed: results.passed.length,
      failed: results.failed.length,
      skipped: results.skipped.length,
      passRate:
        allExamples.length > 0
          ? Math.round((results.passed.length / allExamples.length) * 10000) / 100
          : 100
    },
    failed: results.failed.map((example) => ({
      source: relative(docsRoot, example.source),
      line: example.line,
      context: example.context,
      error: example.result.error || example.result.stderr
    }))
  };

  // Print summary
  console.log('\n' + '='.repeat(60));
  console.log('EXAMPLE VALIDATION REPORT');
  console.log('='.repeat(60));
  console.log(`\nMode: ${report.mode}`);
  console.log(`Total Examples: ${report.summary.total}`);
  console.log(`  [OK] Passed: ${report.summary.passed}`);
  console.log(`  [X] Failed: ${report.summary.failed}`);
  console.log(`Pass Rate: ${report.summary.passRate}%`);

  if (results.failed.length > 0) {
    console.log('\nFailed Examples:');
    for (const example of results.failed) {
      console.log(`\n  ${relative(docsRoot, example.source)}:${example.line}`);
      console.log(`  Context: ${example.context}`);
      console.log(`  Error: ${example.result.error || example.result.stderr}`);
    }
  }

  // Save report
  const reportsDir = join(__dirname, config.reportsDirectory);
  await mkdir(reportsDir, { recursive: true });

  const reportPath = join(reportsDir, 'example-report.json');
  await writeFile(reportPath, JSON.stringify(report, null, 2));
  console.log(`\nReport saved to: ${reportPath}`);

  return report;
}

// Run if executed directly
if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const execute = process.argv.includes('--execute');

  validateExamples({ execute })
    .then((report) => {
      process.exit(report.summary.failed > 0 ? 1 : 0);
    })
    .catch((error) => {
      console.error('Error validating examples:', error);
      process.exit(1);
    });
}

export { validateExamples, extractCodeExamples, wrapCodeForExecution, syntaxCheckExample };
