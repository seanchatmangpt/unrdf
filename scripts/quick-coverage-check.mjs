#!/usr/bin/env node
/**
 * Quick coverage assessment for all 21 UNRDF packages
 * Runs tests and extracts coverage metrics without waiting for full reports
 */

import { execSync } from 'child_process';
import { readdirSync, statSync, existsSync } from 'fs';
import { join } from 'path';

const packagesDir = join(process.cwd(), 'packages');
const packages = readdirSync(packagesDir)
  .filter(name => {
    const pkgPath = join(packagesDir, name);
    return statSync(pkgPath).isDirectory() && existsSync(join(pkgPath, 'package.json'));
  })
  .sort();

console.log(`\n📦 Found ${packages.length} packages\n`);

const results = [];

for (const pkg of packages) {
  const pkgPath = join(packagesDir, pkg);
  process.chdir(pkgPath);

  console.log(`\n🧪 Testing: ${pkg}`);

  try {
    // Run tests with a short timeout (10s per package)
    const output = execSync('timeout 20s pnpm exec vitest run --coverage --reporter=json 2>&1', {
      encoding: 'utf-8',
      stdio: 'pipe'
    });

    // Extract coverage summary from JSON if available
    const match = output.match(/All files\s+\|\s+([\d.]+)\s+\|\s+([\d.]+)\s+\|\s+([\d.]+)\s+\|\s+([\d.]+)/);

    if (match) {
      const [, stmts, branches, funcs, lines] = match;
      const coverage = {
        package: pkg,
        statements: parseFloat(stmts),
        branches: parseFloat(branches),
        functions: parseFloat(funcs),
        lines: parseFloat(lines),
        status: parseFloat(lines) >= 80 && parseFloat(branches) >= 80 ? '✅' : '❌'
      };
      results.push(coverage);

      console.log(`  Lines: ${lines}% | Branches: ${branches}% | ${coverage.status}`);
    } else {
      results.push({ package: pkg, status: '⚠️', error: 'No coverage data' });
      console.log(`  ⚠️ No coverage data found`);
    }
  } catch (error) {
    const errorMsg = error.message.includes('timeout') ? 'Timeout (>20s)' :
                     error.message.includes('test failed') ? 'Test failures' : 'Error';
    results.push({ package: pkg, status: '❌', error: errorMsg });
    console.log(`  ❌ ${errorMsg}`);
  }
}

// Summary table
console.log('\n\n📊 COVERAGE SUMMARY (21 Packages)\n');
console.log('| Package | Lines | Branches | Functions | Statements | Status |');
console.log('|---------|-------|----------|-----------|------------|--------|');

for (const result of results) {
  if (result.lines !== undefined) {
    console.log(`| ${result.package.padEnd(15)} | ${String(result.lines).padEnd(5)}% | ${String(result.branches).padEnd(8)}% | ${String(result.functions).padEnd(9)}% | ${String(result.statements).padEnd(10)}% | ${result.status} |`);
  } else {
    console.log(`| ${result.package.padEnd(15)} | - | - | - | - | ${result.status} (${result.error}) |`);
  }
}

// Count results
const passing = results.filter(r => r.status === '✅').length;
const failing = results.filter(r => r.status === '❌').length;
const warnings = results.filter(r => r.status === '⚠️').length;

console.log(`\n\n📈 Summary: ${passing}/${results.length} packages ≥80% coverage`);
console.log(`   ✅ Pass: ${passing}`);
console.log(`   ❌ Fail: ${failing}`);
console.log(`   ⚠️  Warn: ${warnings}`);

process.exit(failing > 0 ? 1 : 0);
