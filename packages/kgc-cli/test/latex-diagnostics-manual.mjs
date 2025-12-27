/**
 * @fileoverview Manual test for LaTeX diagnostics (no test framework needed).
 *
 * Run with: node test/latex-diagnostics-manual.mjs
 */

import { rm, readFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import {
  LatexCompileError,
  writeLatexRunLog,
  parseMissingInputsFromLog,
  extractErrorSummary,
  isCompileSuccessful
} from '../src/lib/latex/diagnostics.mjs';

async function runTests() {
  console.log('🧪 Running LaTeX Diagnostics Manual Tests\n');

  let passed = 0;
  let failed = 0;

  // Test 1: LatexCompileError creation
  try {
    const error = new LatexCompileError('Test failure', {
      engine: 'pdflatex',
      inputTexPath: '/test.tex',
      logFilePath: '/test.log',
      missingInputs: ['missing.tex']
    });

    console.assert(error.engine === 'pdflatex', 'Error engine mismatch');
    console.assert(error.missingInputs.length === 1, 'Missing inputs mismatch');
    console.assert(error.toJSON().name === 'LatexCompileError', 'JSON serialization failed');

    console.log('✅ Test 1: LatexCompileError creation');
    passed++;
  } catch (e) {
    console.error('❌ Test 1 failed:', e.message);
    failed++;
  }

  // Test 2: Parse missing inputs
  try {
    const log = `
! LaTeX Error: File \`chapter1.tex' not found.
! I can't find file \`logo.pdf'.
! File \`missing.sty' not found.
    `;

    const missing = parseMissingInputsFromLog(log);

    console.assert(missing.length === 3, `Expected 3 missing files, got ${missing.length}`);
    console.assert(missing.includes('chapter1.tex'), 'Missing chapter1.tex');
    console.assert(missing.includes('logo.pdf'), 'Missing logo.pdf');
    console.assert(missing.includes('missing.sty'), 'Missing missing.sty');

    console.log('✅ Test 2: Parse missing inputs');
    passed++;
  } catch (e) {
    console.error('❌ Test 2 failed:', e.message);
    failed++;
  }

  // Test 3: Extract error summary
  try {
    const log = `
This is pdfTeX
! Undefined control sequence.
l.42 \\badcommand
    `;

    const summary = extractErrorSummary(log);
    console.assert(summary === '! Undefined control sequence.', 'Error summary mismatch');

    console.log('✅ Test 3: Extract error summary');
    passed++;
  } catch (e) {
    console.error('❌ Test 3 failed:', e.message);
    failed++;
  }

  // Test 4: Detect successful compilation
  try {
    const successLog = 'Output written on main.pdf (10 pages, 12345 bytes).';
    const failLog = '! Emergency stop.\nNo pages of output.';

    console.assert(isCompileSuccessful(successLog) === true, 'Should detect success');
    console.assert(isCompileSuccessful(failLog) === false, 'Should detect failure');

    console.log('✅ Test 4: Detect compilation success');
    passed++;
  } catch (e) {
    console.error('❌ Test 4 failed:', e.message);
    failed++;
  }

  // Test 5: Write log file
  try {
    const testCacheDir = join(tmpdir(), `latex-manual-test-${Date.now()}`);
    await mkdir(testCacheDir, { recursive: true });

    const logFilePath = await writeLatexRunLog({
      cacheDir: testCacheDir,
      engine: 'pdflatex',
      inputTexPath: '/thesis/main.tex',
      logText: 'Test log content'
    });

    console.assert(logFilePath.includes('pdflatex'), 'Log path should contain engine name');
    console.assert(logFilePath.endsWith('.log'), 'Log path should end with .log');

    const contents = await readFile(logFilePath, 'utf8');
    console.assert(contents.includes('Test log content'), 'Log should contain content');
    console.assert(contents.includes('# Engine: pdflatex'), 'Log should contain metadata');

    // Cleanup
    await rm(testCacheDir, { recursive: true, force: true });

    console.log('✅ Test 5: Write log file');
    passed++;
  } catch (e) {
    console.error('❌ Test 5 failed:', e.message);
    failed++;
  }

  // Test 6: Deduplication of missing files
  try {
    const log = `
! LaTeX Error: File \`dup.tex' not found.
! I can't find file \`dup.tex'.
! LaTeX Error: File \`dup.tex' not found.
    `;

    const missing = parseMissingInputsFromLog(log);
    console.assert(missing.length === 1, `Expected 1 unique file, got ${missing.length}`);
    console.assert(missing[0] === 'dup.tex', 'Expected dup.tex');

    console.log('✅ Test 6: Deduplicate missing files');
    passed++;
  } catch (e) {
    console.error('❌ Test 6 failed:', e.message);
    failed++;
  }

  // Test 7: Empty/null handling
  try {
    console.assert(parseMissingInputsFromLog('').length === 0, 'Empty string should return []');
    console.assert(parseMissingInputsFromLog(null).length === 0, 'Null should return []');
    console.assert(extractErrorSummary('') === undefined, 'Empty should return undefined');
    console.assert(isCompileSuccessful('') === false, 'Empty should be unsuccessful');

    console.log('✅ Test 7: Handle empty/null inputs');
    passed++;
  } catch (e) {
    console.error('❌ Test 7 failed:', e.message);
    failed++;
  }

  // Summary
  console.log(`\n${'='.repeat(50)}`);
  console.log(`Tests passed: ${passed}`);
  console.log(`Tests failed: ${failed}`);
  console.log(`Total: ${passed + failed}`);
  console.log('='.repeat(50));

  if (failed > 0) {
    process.exit(1);
  }
}

runTests().catch(err => {
  console.error('Fatal error:', err);
  process.exit(1);
});
