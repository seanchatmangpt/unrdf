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

function checkCoverage() {
  try {
    const result = execSync('pnpm test:coverage -- --reporter=json --reporter=text', {
      cwd: projectRoot,
      encoding: 'utf-8',
      stdio: 'pipe',
      timeout: 60000,
      maxBuffer: 1024 * 1024 * 10,
    });
    const coverageLine = result.split('\n').find(line => line.includes('% stmt'));
    const match = coverageLine?.match(/(\d+\.?\d*)%/);
    if (match) {
      const coverage = parseFloat(match[1]);
      if (coverage >= 80) {
        return { status: 'pass', actual: `${coverage.toFixed(1)}% coverage`, expected: '>=80% coverage' };
      }
      return {
        status: 'fail',
        actual: `${coverage.toFixed(1)}% coverage`,
        expected: '>=80% coverage',
        fix: 'Add tests to increase coverage to 80%',
      };
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

async function runLint() {
  try {
    execSync('pnpm lint', {
      cwd: projectRoot,
      encoding: 'utf-8',
      stdio: 'pipe',
      timeout: 30000,
    });
    return { status: 'pass', actual: 'No linting errors', expected: 'ESLint passes' };
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

function checkFileSize() {
  try {
    const files = glob.sync('packages/**/*.mjs', { cwd: projectRoot, ignore: COMMON_IGNORES });
    const violations = [];
    for (const file of files) {
      const lines = readFileSync(join(projectRoot, file), 'utf-8').split('\n').length;
      if (lines > 500) violations.push({ file, lines });
    }
    if (violations.length === 0) {
      return { status: 'pass', actual: 'No file size violations', expected: 'All files <=500 lines' };
    }
    return {
      status: 'warn',
      actual: `${violations.length} files >500 lines`,
      expected: 'All files <=500 lines',
      violations: violations.slice(0, 10),
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

function checkNoTypeScript() {
  try {
    const tsFiles = glob.sync('**/*.{ts,tsx,d.ts}', { cwd: projectRoot, ignore: COMMON_IGNORES });
    if (tsFiles.length === 0) {
      return { status: 'pass', actual: 'No TypeScript files found', expected: 'Pure ESM + JSDoc project' };
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

function checkN3Imports() {
  try {
    const files = glob.sync('packages/**/*.mjs', { cwd: projectRoot, ignore: COMMON_IGNORES });
    const violations = [];
    for (const file of files) {
      const content = readFileSync(join(projectRoot, file), 'utf-8');
      const hasN3Import = /^import\s+.*from\s+['"]n3['"]/m.test(content) ||
        /^const\s+.*=\s+require\(['"]n3['"]\)/m.test(content);
      if (hasN3Import && !file.includes('n3-justified-only')) violations.push(file);
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

/** Replace comments and string literals with spaces while retaining line structure. */
function stripNonCode(content) {
  let state = 'code';
  let quote = null;
  let result = '';
  for (let index = 0; index < content.length; index += 1) {
    const char = content[index];
    const next = content[index + 1];
    if (state === 'line-comment') {
      if (char === '\n') {
        state = 'code';
        result += '\n';
      } else result += ' ';
      continue;
    }
    if (state === 'block-comment') {
      if (char === '*' && next === '/') {
        result += '  ';
        index += 1;
        state = 'code';
      } else result += char === '\n' ? '\n' : ' ';
      continue;
    }
    if (state === 'string') {
      if (char === '\\') {
        result += ' ';
        if (index + 1 < content.length) {
          result += content[index + 1] === '\n' ? '\n' : ' ';
          index += 1;
        }
      } else if (char === quote) {
        result += ' ';
        state = 'code';
        quote = null;
      } else result += char === '\n' ? '\n' : ' ';
      continue;
    }
    if (char === '/' && next === '/') {
      result += '  ';
      index += 1;
      state = 'line-comment';
    } else if (char === '/' && next === '*') {
      result += '  ';
      index += 1;
      state = 'block-comment';
    } else if (char === '"' || char === "'" || char === '`') {
      result += ' ';
      state = 'string';
      quote = char;
    } else result += char;
  }
  return result;
}

function extractComments(content) {
  return [...content.matchAll(/\/\*[\s\S]*?\*\/|\/\/[^\n]*/g)].map(match => match[0]);
}

function checkSkippedTests() {
  try {
    const testFiles = glob.sync('**/*.{test.mjs,test.ts,test.js}', {
      cwd: projectRoot,
      ignore: COMMON_IGNORES,
    });
    let skippedCount = 0;
    const skippedFiles = [];
    for (const file of testFiles) {
      const code = stripNonCode(readFileSync(join(projectRoot, file), 'utf-8'));
      const matches = code.match(/\b(?:describe|it|test)\.skip\s*\(|\b(?:xit|xdescribe)\s*\(/g) || [];
      if (matches.length > 0) {
        skippedCount += matches.length;
        skippedFiles.push({ file, count: matches.length });
      }
    }
    if (skippedCount === 0) {
      return { status: 'pass', actual: 'No skipped tests', expected: 'All tests active' };
    }
    return {
      status: 'warn',
      actual: `${skippedCount} skipped test(s) in ${skippedFiles.length} file(s)`,
      expected: 'All tests active',
      violations: skippedFiles.slice(0, 10),
      fix: 'Review and unskip or explicitly classify each exclusion',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check skipped tests: ${error.message}`,
      expected: 'All tests active',
      fix: 'Review test files for executable skip syntax',
    };
  }
}

function checkDefinitionOfDone() {
  try {
    const srcFiles = glob.sync('packages/**/*.mjs', {
      cwd: projectRoot,
      ignore: [...COMMON_IGNORES, '**/test/**', '**/examples/**', '**/playground/**'],
    });
    let deferredCount = 0;
    let consoleLogCount = 0;
    const violations = [];
    for (const file of srcFiles) {
      const content = readFileSync(join(projectRoot, file), 'utf-8');
      const comments = extractComments(content).join('\n');
      const deferred = comments.match(/\b(?:TODO|FIXME|HACK|XXX)\b\s*:/gi) || [];
      if (deferred.length > 0) {
        deferredCount += deferred.length;
        violations.push(`${file}: ${deferred.length} deferred marker(s)`);
      }
      if (!file.includes('cli/commands') && !file.includes('bin/')) {
        const logs = stripNonCode(content).match(/console\.log\s*\(/g) || [];
        if (logs.length > 0) {
          consoleLogCount += logs.length;
          violations.push(`${file}: ${logs.length} console.log(s)`);
        }
      }
    }
    if (deferredCount === 0 && consoleLogCount === 0) {
      return {
        status: 'pass',
        actual: 'No deferred markers or rogue console.logs',
        expected: 'Clean production code',
      };
    }
    return {
      status: 'warn',
      actual: `${deferredCount} deferred marker(s), ${consoleLogCount} console.logs found`,
      expected: 'Zero deferred markers and console.logs in production core',
      violations: violations.slice(0, 10),
      fix: 'Resolve deferred markers and replace console.log with the package logger',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check Definition of Done: ${error.message}`,
      expected: 'Clean production code',
      fix: 'Run the repository WIP audit and inspect the reported source locations',
    };
  }
}

export async function checkQuality() {
  return {
    category: 'Code Quality',
    checks: [
      { name: 'Test coverage', ...checkCoverage() },
      { name: 'ESLint status', ...(await runLint()) },
      { name: 'File size violations', ...checkFileSize() },
      { name: 'TypeScript contamination', ...checkNoTypeScript() },
      { name: 'N3 import violations', ...checkN3Imports() },
      { name: 'Skipped tests', ...checkSkippedTests() },
      { name: 'Definition of Done', ...checkDefinitionOfDone() },
    ],
  };
}
