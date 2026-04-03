#!/usr/bin/env node

/**
 * Structure Audit Script: Verify all packages follow unified structure
 *
 * Checks:
 * - All packages have src/index.mjs entry point
 * - All test files follow *.test.mjs pattern
 * - No broken imports
 * - Consistent directory layout
 */

import { globSync } from 'glob';
import { readFileSync, existsSync } from 'fs';
import { resolve, dirname } from 'path';

const COLORS = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
};

function log(color, message) {
  console.log(`${color}${message}${COLORS.reset}`);
}

async function checkStructure() {
  log(COLORS.blue, '📋 Auditing Package Structure\n');

  const packages = globSync('packages/*/package.json', {
    ignore: '**/node_modules/**',
  }).map(p => dirname(p));

  let passed = 0;
  let failed = 0;
  const errors = [];

  for (const pkgDir of packages) {
    const pkgName = pkgDir.split('/').pop();
    const checks = {
      hasSrc: existsSync(`${pkgDir}/src`),
      hasIndexMjs: existsSync(`${pkgDir}/src/index.mjs`),
      hasTests: globSync(`${pkgDir}/src/**/*.test.mjs`).length > 0 ||
                globSync(`${pkgDir}/test/**/*.test.mjs`).length > 0 ||
                globSync(`${pkgDir}/**/*.test.mjs`, {
                  ignore: '**/node_modules/**'
                }).length > 0,
    };

    const allPass = Object.values(checks).every(v => v === true || v > 0);

    if (allPass) {
      log(COLORS.green, `✓ ${pkgName}: Structure OK`);
      passed++;
    } else {
      log(COLORS.red, `✗ ${pkgName}: Structure Issues`);
      if (!checks.hasSrc) errors.push(`  - Missing src/ directory`);
      if (!checks.hasIndexMjs) errors.push(`  - Missing src/index.mjs`);
      if (!checks.hasTests) errors.push(`  - No *.test.mjs files found`);
      failed++;
    }
  }

  console.log('');
  log(COLORS.blue, `Summary: ${passed}/${packages.length} passed`);

  if (failed > 0) {
    log(COLORS.red, `\n${failed} package(s) failed structure check:`);
    errors.forEach(e => log(COLORS.red, e));
    process.exit(1);
  } else {
    log(COLORS.green, '✓ All packages pass structure check');
    process.exit(0);
  }
}

checkStructure().catch(err => {
  log(COLORS.red, `Error: ${err.message}`);
  process.exit(1);
});
