#!/usr/bin/env node
/**
 * v6 Validation Script
 *
 * Validates the v6-core package structure and functionality.
 * Generates a validation receipt on success.
 *
 * Usage:
 *   node scripts/v6-validate.mjs
 *   node scripts/v6-validate.mjs --comprehensive
 */

import { execSync } from 'node:child_process';
import { existsSync, statSync } from 'node:fs';
import { readdir } from 'node:fs/promises';
import { join, resolve } from 'node:path';

const V6_CORE_PATH = resolve(process.cwd(), 'packages/v6-core');
const COMPREHENSIVE = process.argv.includes('--comprehensive');

// ANSI colors
const colors = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
};

const log = {
  info: (msg) => console.log(`${colors.blue}ℹ${colors.reset} ${msg}`),
  success: (msg) => console.log(`${colors.green}✓${colors.reset} ${msg}`),
  error: (msg) => console.log(`${colors.red}✗${colors.reset} ${msg}`),
  warn: (msg) => console.log(`${colors.yellow}⚠${colors.reset} ${msg}`),
  section: (msg) => console.log(`\n${colors.cyan}═══ ${msg} ═══${colors.reset}\n`),
};

// Validation results
const results = {
  passed: [],
  failed: [],
  warnings: [],
};

/**
 * Run a validation check
 * @param {string} name - Check name
 * @param {Function} fn - Check function (returns boolean)
 */
async function check(name, fn) {
  try {
    const result = await fn();
    if (result) {
      log.success(name);
      results.passed.push(name);
    } else {
      log.error(name);
      results.failed.push(name);
    }
  } catch (error) {
    log.error(`${name}: ${error.message}`);
    results.failed.push(name);
  }
}

/**
 * Warning check (doesn't fail validation)
 * @param {string} name - Check name
 * @param {Function} fn - Check function
 */
async function warn(name, fn) {
  try {
    const result = await fn();
    if (!result) {
      log.warn(name);
      results.warnings.push(name);
    } else {
      log.success(name);
    }
  } catch (error) {
    log.warn(`${name}: ${error.message}`);
    results.warnings.push(name);
  }
}

// Validation Checks

log.section('v6 Validation');

// 1. Directory Structure
log.section('Directory Structure');

await check('v6-core directory exists', () => {
  return existsSync(V6_CORE_PATH);
});

await check('src/ directory exists', () => {
  return existsSync(join(V6_CORE_PATH, 'src'));
});

await check('test/ directory exists', () => {
  return existsSync(join(V6_CORE_PATH, 'test'));
});

const expectedDirs = [
  'src/receipts',
  'src/receipts/merkle',
  'src/delta',
  'src/delta/adapters',
  'src/cli',
  'src/cli/commands',
  'src/grammar',
  'src/docs',
  'test/receipts',
  'test/delta',
  'test/grammar',
  'test/integration',
];

for (const dir of expectedDirs) {
  await check(`${dir}/ exists`, () => {
    return existsSync(join(V6_CORE_PATH, dir));
  });
}

// 2. Required Files
log.section('Required Files');

const requiredFiles = [
  'package.json',
  'README.md',
  'src/index.mjs',
  'src/receipts/index.mjs',
  'src/receipts/merkle/index.mjs',
  'src/delta/index.mjs',
  'src/delta/adapters/index.mjs',
  'src/cli/index.mjs',
  'src/cli/commands/index.mjs',
  'src/grammar/index.mjs',
  'src/docs/index.mjs',
  'test/integration/v6-smoke.test.mjs',
];

for (const file of requiredFiles) {
  await check(`${file} exists`, () => {
    const path = join(V6_CORE_PATH, file);
    return existsSync(path) && statSync(path).isFile();
  });
}

// 3. Package.json Validation
log.section('Package Configuration');

await check('package.json is valid JSON', () => {
  const pkg = JSON.parse(
    execSync('cat packages/v6-core/package.json', { encoding: 'utf8' })
  );
  return pkg.name === '@unrdf/v6-core';
});

await check('package.json has correct version', () => {
  const pkg = JSON.parse(
    execSync('cat packages/v6-core/package.json', { encoding: 'utf8' })
  );
  return pkg.version === '6.0.0-alpha.1';
});

await check('package.json has required dependencies', () => {
  const pkg = JSON.parse(
    execSync('cat packages/v6-core/package.json', { encoding: 'utf8' })
  );
  const deps = Object.keys(pkg.dependencies || {});
  return deps.includes('zod') &&
         deps.includes('@unrdf/kgc-substrate') &&
         deps.includes('@unrdf/oxigraph');
});

await check('package.json has test script', () => {
  const pkg = JSON.parse(
    execSync('cat packages/v6-core/package.json', { encoding: 'utf8' })
  );
  return pkg.scripts && pkg.scripts.test;
});

// 4. Module Imports
log.section('Module Imports');

await check('src/index.mjs exports V6_VERSION', async () => {
  try {
    const mod = await import(join(V6_CORE_PATH, 'src/index.mjs'));
    return mod.V6_VERSION === '6.0.0-alpha.1';
  } catch {
    return false;
  }
});

await check('src/index.mjs exports V6_FEATURES', async () => {
  try {
    const mod = await import(join(V6_CORE_PATH, 'src/index.mjs'));
    return mod.V6_FEATURES && mod.V6_FEATURES.receipts === true;
  } catch {
    return false;
  }
});

await check('receipts module exports createReceipt', async () => {
  try {
    const mod = await import(join(V6_CORE_PATH, 'src/receipts/index.mjs'));
    return typeof mod.createReceipt === 'function';
  } catch {
    return false;
  }
});

await check('delta module exports createDeltaProposal', async () => {
  try {
    const mod = await import(join(V6_CORE_PATH, 'src/delta/index.mjs'));
    return typeof mod.createDeltaProposal === 'function';
  } catch {
    return false;
  }
});

await check('cli module exports buildCLISpine', async () => {
  try {
    const mod = await import(join(V6_CORE_PATH, 'src/cli/index.mjs'));
    return typeof mod.buildCLISpine === 'function';
  } catch {
    return false;
  }
});

// 5. Smoke Tests
log.section('Smoke Tests');

await check('smoke tests run successfully', () => {
  try {
    const output = execSync(
      'timeout 5s node --test packages/v6-core/test/integration/v6-smoke.test.mjs',
      { encoding: 'utf8', cwd: process.cwd() }
    );
    return !output.includes('failing');
  } catch (error) {
    log.error(`Test output: ${error.stdout || error.message}`);
    return false;
  }
});

// 6. File Size Checks (warnings)
log.section('Code Quality Checks');

await warn('src files are reasonable size (<500 lines)', async () => {
  const srcFiles = await readdir(join(V6_CORE_PATH, 'src'), { recursive: true });
  const jsFiles = srcFiles.filter(f => f.endsWith('.mjs'));

  for (const file of jsFiles) {
    const fullPath = join(V6_CORE_PATH, 'src', file);
    if (existsSync(fullPath) && statSync(fullPath).isFile()) {
      const lines = execSync(`wc -l "${fullPath}"`, { encoding: 'utf8' });
      const lineCount = parseInt(lines.trim().split(' ')[0], 10);
      if (lineCount > 500) {
        log.warn(`  ${file}: ${lineCount} lines`);
        return false;
      }
    }
  }
  return true;
});

// 7. Comprehensive Tests (optional)
if (COMPREHENSIVE) {
  log.section('Comprehensive Validation');

  await check('all tests pass', () => {
    try {
      execSync('timeout 10s pnpm --filter @unrdf/v6-core test', {
        encoding: 'utf8',
        cwd: process.cwd(),
      });
      return true;
    } catch {
      return false;
    }
  });

  await warn('linting passes', () => {
    try {
      execSync('pnpm --filter @unrdf/v6-core lint', {
        encoding: 'utf8',
        cwd: process.cwd(),
      });
      return true;
    } catch {
      return false;
    }
  });
}

// Generate Validation Receipt
log.section('Validation Receipt');

const receipt = {
  id: `v6-validation-${Date.now()}`,
  operation: 'v6-core-validation',
  timestamp: new Date().toISOString(),
  results: {
    passed: results.passed.length,
    failed: results.failed.length,
    warnings: results.warnings.length,
    total: results.passed.length + results.failed.length,
  },
  status: results.failed.length === 0 ? 'PASS' : 'FAIL',
  comprehensive: COMPREHENSIVE,
  details: {
    passed: results.passed,
    failed: results.failed,
    warnings: results.warnings,
  },
};

console.log(JSON.stringify(receipt, null, 2));

// Summary
log.section('Summary');

log.info(`Passed: ${colors.green}${results.passed.length}${colors.reset}`);
log.info(`Failed: ${colors.red}${results.failed.length}${colors.reset}`);
log.info(`Warnings: ${colors.yellow}${results.warnings.length}${colors.reset}`);
log.info(`Status: ${receipt.status === 'PASS' ? colors.green : colors.red}${receipt.status}${colors.reset}`);

// Exit code
process.exit(results.failed.length === 0 ? 0 : 1);
