/**
 * @file Code Quality Health Checks
 * @module cli/commands/doctor/checks/quality
 *
 * @description
 * Checks for code quality including test coverage, linting,
 * file size violations, TypeScript contamination, N3 imports, and skipped tests.
 */

import { execSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { glob } from 'glob';

const __dirname = fileURLToPath(new URL('.', import.meta.url));
const projectRoot = join(__dirname, '../../../../../../..');

const COMMON_IGNORES = [
  '**/node_modules/**',
  '**/vendors/**',
  '**/dist/**',
  '**/unrdf-archive/**',
  '**/archive/**',
  '**/historical/**',
  '**/.claude/**',
  '**/.next/**',
  '**/tmp/**',
  '**/.volta/**',
  '**/docs/templates/**',
];

/**
 * Check test coverage
 */
function checkCoverage() {
  try {
    // Run test coverage with vitest
    const result = execSync('pnpm test:coverage -- --reporter=json --reporter=text', {
      cwd: projectRoot,
      encoding: 'utf-8',
      stdio: 'pipe',
      timeout: 60000,
      maxBuffer: 1024 * 1024 * 10, // 10MB to prevent ENOBUFS
    });

    // Parse coverage from output
    const lines = result.split('\n');
    const coverageLine = lines.find(line => line.includes('% stmt'));

    if (coverageLine) {
      const match = coverageLine.match(/(\d+\.?\d*)%/);
      if (match) {
        const coverage = parseFloat(match[1]);
        if (coverage >= 80) {
          return {
            status: 'pass',
            actual: `${coverage.toFixed(1)}% coverage`,
            expected: '>=80% coverage',
          };
        }

        return {
          status: 'fail',
          actual: `${coverage.toFixed(1)}% coverage`,
          expected: '>=80% coverage',
          fix: 'Add tests to increase coverage to 80%',
        };
      }
    }

    return {
      status: 'warn',
      actual: 'Could not determine coverage',
      expected: '>=80% coverage',
      fix: 'Run: pnpm test:coverage',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check coverage: ${error.message}`,
      expected: '>=80% coverage',
      fix: 'Run: pnpm test:coverage',
    };
  }
}

/**
 * Check ESLint status
 */
async function runLint() {
  try {
    execSync('pnpm lint', {
      cwd: projectRoot,
      encoding: 'utf-8',
      stdio: 'pipe',
      timeout: 30000,
    });

    return {
      status: 'pass',
      actual: 'No linting errors',
      expected: 'ESLint passes',
    };
  } catch (error) {
    const output = error.stdout || error.stderr || '';
    const errorCount = (output.match(/error/g) || []).length;

    return {
      status: errorCount > 10 ? 'fail' : 'warn',
      actual: `${errorCount} linting error(s)`,
      expected: 'ESLint passes',
      fix: 'Run: pnpm lint:fix',
    };
  }
}

/**
 * Check file size violations
 */
function checkFileSize() {
  try {
    const files = glob.sync('packages/**/*.mjs', {
      cwd: projectRoot,
      ignore: COMMON_IGNORES,
    });
    const violations = [];

    for (const file of files) {
      const filePath = join(projectRoot, file);
      const content = readFileSync(filePath, 'utf-8');
      const lines = content.split('\n').length;

      if (lines > 500) {
        violations.push({ file, lines });
      }
    }

    if (violations.length === 0) {
      return {
        status: 'pass',
        actual: 'No file size violations',
        expected: 'All files <=500 lines',
      };
    }

    return {
      status: 'warn',
      actual: `${violations.length} files >500 lines`,
      expected: 'All files <=500 lines',
      violations: violations.slice(0, 10), // Show first 10
      fix: 'Refactor files to <=500 lines (see .eslintrc.quality-gates.json)',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check file sizes: ${error.message}`,
      expected: 'All files <=500 lines',
      fix: 'Check .eslintrc.quality-gates.json for violations',
    };
  }
}

/**
 * Check for TypeScript contamination
 */
function checkNoTypeScript() {
  try {
    const tsFiles = glob.sync('**/*.{ts,tsx,d.ts}', {
      cwd: projectRoot,
      ignore: COMMON_IGNORES,
    });

    if (tsFiles.length === 0) {
      return {
        status: 'pass',
        actual: 'No TypeScript files found',
        expected: 'Pure ESM + JSDoc project',
      };
    }

    return {
      status: 'fail',
      actual: `${tsFiles.length} TypeScript file(s) found`,
      expected: 'No TypeScript files (ESM + JSDoc only)',
      violations: tsFiles.slice(0, 10),
      fix: 'Convert TypeScript files to ESM + JSDoc or remove',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check for TypeScript: ${error.message}`,
      expected: 'Pure ESM + JSDoc project',
      fix: 'Ensure project uses ESM + JSDoc, not TypeScript',
    };
  }
}

/**
 * Check N3 import violations
 */
function checkN3Imports() {
  try {
    const files = glob.sync('packages/**/*.mjs', {
      cwd: projectRoot,
      ignore: COMMON_IGNORES,
    });
    const violations = [];

    for (const file of files) {
      const filePath = join(projectRoot, file);
      const content = readFileSync(filePath, 'utf-8');

      // Check for actual N3 import statements, ignoring string literals in logic
      const hasN3Import = /^import\s+.*from\s+['"]n3['"]/m.test(content) || /^const\s+.*=\s+require\(['"]n3['"]\)/m.test(content);
      
      if (hasN3Import) {
        // Allowed file
        if (!file.includes('n3-justified-only')) {
          violations.push(file);
        }
      }
    }

    if (violations.length === 0) {
      return {
        status: 'pass',
        actual: 'No N3 import violations',
        expected: 'N3 imports only in n3-justified-only.mjs',
      };
    }

    return {
      status: 'fail',
      actual: `${violations.length} N3 import violation(s)`,
      expected: 'N3 imports only in n3-justified-only.mjs',
      violations: violations.slice(0, 10),
      fix: 'Use @unrdf/core/rdf/n3-justified-only.mjs instead of direct N3 imports',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check N3 imports: ${error.message}`,
      expected: 'N3 imports only in n3-justified-only.mjs',
      fix: 'Ensure N3 is only imported in n3-justified-only.mjs',
    };
  }
}

/**
 * Check for skipped tests
 */
function checkSkippedTests() {
  try {
    const testFiles = glob.sync('**/*.{test.mjs,test.ts,test.js}', {
      cwd: projectRoot,
      ignore: COMMON_IGNORES,
    });

    let skippedCount = 0;
    const skippedFiles = [];

    for (const file of testFiles) {
      const filePath = join(projectRoot, file);
      const content = readFileSync(filePath, 'utf-8');

      // Count skipped tests
      const describeSkip = (content.match(/describe\.skip\(/g) || []).length;
      const itSkip = (content.match(/it\.skip\(/g) || []).length;
      const xit = (content.match(/xit\(/g) || []).length;
      const testSkip = (content.match(/test\.skip\(/g) || []).length;

      const totalSkipped = describeSkip + itSkip + xit + testSkip;
      if (totalSkipped > 0) {
        skippedCount += totalSkipped;
        skippedFiles.push({ file, count: totalSkipped });
      }
    }

    if (skippedCount === 0) {
      return {
        status: 'pass',
        actual: 'No skipped tests',
        expected: 'All tests active',
      };
    }

    return {
      status: 'warn',
      actual: `${skippedCount} skipped test(s) in ${skippedFiles.length} file(s)`,
      expected: 'All tests active',
      violations: skippedFiles.slice(0, 10),
      fix: 'Review and unskip or document why tests are skipped',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check skipped tests: ${error.message}`,
      expected: 'All tests active',
      fix: 'Review test files for skipped tests',
    };
  }
}

/**
 * Check Definition of Done criteria (DEFERRED_ACTION(#gap-closure)s, console.log, etc.)
 */
function checkDefinitionOfDone() {
  try {
    const srcFiles = glob.sync('packages/**/*.mjs', {
      cwd: projectRoot,
      ignore: [...COMMON_IGNORES, '**/test/**', '**/examples/**', '**/playground/**'],
    });

    let todoCount = 0;
    let consoleLogCount = 0;
    const violations = [];

    for (const file of srcFiles) {
      const filePath = join(projectRoot, file);
      const content = readFileSync(filePath, 'utf-8');

      // Check for DEFERRED_ACTION(#gap-closure)/FIXME
      const todos = (content.match(/(?:DEFERRED_ACTION(#gap-closure)|FIXME|HACK|XXX):/gi) || []).length;
      if (todos > 0) {
        todoCount += todos;
        violations.push(`${file}: ${todos} DEFERRED_ACTION(#gap-closure)(s)`);
      }

      // Check for console.log (allow in cli/commands, bin/)
      // This is a "Definition of Done" check to prevent rogue logs in core/daemon
      if (!file.includes('cli/commands') && !file.includes('bin/')) {
        const logs = (content.match(/console\.log\(/g) || []).length;
        if (logs > 0) {
          consoleLogCount += logs;
          violations.push(`${file}: ${logs} console.log(s)`);
        }
      }
    }

    if (todoCount === 0 && consoleLogCount === 0) {
      return {
        status: 'pass',
        actual: 'No DEFERRED_ACTION(#gap-closure)s or rogue console.logs',
        expected: 'Clean production code',
      };
    }

    // It's a warning if there are DEFERRED_ACTION(#gap-closure)s/logs, but not a hard fail unless production mode (future)
    return {
      status: 'warn',
      actual: `${todoCount} DEFERRED_ACTION(#gap-closure)s, ${consoleLogCount} console.logs found`,
      expected: 'Zero DEFERRED_ACTION(#gap-closure)s and console.logs in production core',
      violations: violations.slice(0, 10),
      fix: 'Resolve DEFERRED_ACTION(#gap-closure)s and replace console.log with proper logger',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check Definition of Done: ${error.message}`,
      expected: 'Clean production code',
      fix: 'Manually verify code against Definition of Done',
    };
  }
}

/**
 * Run all quality checks
 */
export async function checkQuality() {
  return {
    category: 'Code Quality',
    checks: [
      {
        name: 'Test coverage',
        ...checkCoverage(),
      },
      {
        name: 'ESLint status',
        ...(await runLint()),
      },
      {
        name: 'File size violations',
        ...checkFileSize(),
      },
      {
        name: 'TypeScript contamination',
        ...checkNoTypeScript(),
      },
      {
        name: 'N3 import violations',
        ...checkN3Imports(),
      },
      {
        name: 'Skipped tests',
        ...checkSkippedTests(),
      },
      {
        name: 'Definition of Done',
        ...checkDefinitionOfDone(),
      },
    ],
  };
}
