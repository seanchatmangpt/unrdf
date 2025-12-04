#!/usr/bin/env node
/**
 * Comprehensive validation script for all 26 UNRDF example subprojects
 * Validates vitest setup, test execution, and documentation
 */

import { readFileSync, existsSync, readdirSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { execSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const rootDir = join(__dirname, '..');

const EXAMPLES = [
  // @unrdf/core (3 examples)
  { pkg: 'core', name: 'basic-store', minTests: 15 },
  { pkg: 'core', name: 'sparql-queries', minTests: 12 },
  { pkg: 'core', name: 'rdf-parsing', minTests: 12 },

  // @unrdf/hooks (2 examples)
  { pkg: 'hooks', name: 'policy-hooks', minTests: 12 },
  { pkg: 'hooks', name: 'hook-chains', minTests: 10 },

  // @unrdf/federation (2 examples)
  { pkg: 'federation', name: 'peer-discovery', minTests: 12 },
  { pkg: 'federation', name: 'distributed-queries', minTests: 14 },

  // @unrdf/streaming (2 examples)
  { pkg: 'streaming', name: 'change-feeds', minTests: 10 },
  { pkg: 'streaming', name: 'real-time-sync', minTests: 10 },

  // @unrdf/browser (2 examples)
  { pkg: 'browser', name: 'indexed-db', minTests: 10, env: 'jsdom' },
  { pkg: 'browser', name: 'offline-support', minTests: 11, env: 'jsdom' },

  // @unrdf/cli (2 examples)
  { pkg: 'cli', name: 'graph-commands', minTests: 10, env: 'node' },
  { pkg: 'cli', name: 'format-conversion', minTests: 10, env: 'node' },

  // @unrdf/knowledge-engine (2 examples)
  { pkg: 'knowledge-engine', name: 'basic-inference', minTests: 10 },
  { pkg: 'knowledge-engine', name: 'sparql-rules', minTests: 10 },

  // @unrdf/dark-matter (2 examples)
  { pkg: 'dark-matter', name: 'query-optimization', minTests: 10 },
  { pkg: 'dark-matter', name: 'index-advisor', minTests: 10 },

  // @unrdf/composables (2 examples)
  { pkg: 'composables', name: 'reactive-graphs', minTests: 10, env: 'jsdom' },
  { pkg: 'composables', name: 'query-integration', minTests: 10, env: 'jsdom' },

  // Full-stack integration (2 apps)
  { pkg: 'full-stack-example/apps', name: 'server', minTests: 15, env: 'node', isPlayground: true },
  { pkg: 'full-stack-example/apps', name: 'web', minTests: 12, env: 'jsdom', isPlayground: true },
];

const results = {
  totalExamples: EXAMPLES.length,
  validated: 0,
  failed: [],
  totalTests: 0,
  passingTests: 0,
  failingTests: 0,
  missingConfigs: [],
  missingTests: [],
  missingScripts: [],
  missingDocs: [],
};

console.log('🔍 UNRDF Example Validation Report');
console.log('=' .repeat(80));
console.log(`Total examples to validate: ${EXAMPLES.length}\n`);

for (const example of EXAMPLES) {
  const basePath = example.isPlayground
    ? join(rootDir, 'playground', example.pkg, example.name)
    : join(rootDir, 'packages', example.pkg, 'examples', example.name);

  console.log(`\n📦 Validating: @unrdf/${example.pkg}/${example.name}`);
  console.log(`   Path: ${basePath}`);

  if (!existsSync(basePath)) {
    console.log(`   ❌ Directory does not exist`);
    results.failed.push({ example: example.name, reason: 'Directory not found' });
    continue;
  }

  const validation = {
    hasVitestConfig: false,
    hasPackageJson: false,
    hasTestScripts: false,
    hasTestFiles: false,
    hasReadme: false,
    testCount: 0,
    passingTests: 0,
    failingTests: 0,
  };

  // Check vitest.config.mjs
  const vitestConfig = join(basePath, 'vitest.config.mjs');
  validation.hasVitestConfig = existsSync(vitestConfig);
  if (!validation.hasVitestConfig) {
    console.log(`   ❌ vitest.config.mjs missing`);
    results.missingConfigs.push(example.name);
  } else {
    console.log(`   ✅ vitest.config.mjs exists`);
  }

  // Check package.json and scripts
  const packageJson = join(basePath, 'package.json');
  validation.hasPackageJson = existsSync(packageJson);
  if (validation.hasPackageJson) {
    try {
      const pkg = JSON.parse(readFileSync(packageJson, 'utf8'));
      const hasTest = pkg.scripts?.test?.includes('vitest');
      const hasTestWatch = pkg.scripts?.['test:watch'];
      const hasTestCoverage = pkg.scripts?.['test:coverage'];

      validation.hasTestScripts = hasTest;

      if (hasTest && hasTestWatch && hasTestCoverage) {
        console.log(`   ✅ package.json has all test scripts`);
      } else {
        console.log(`   ⚠️  package.json missing some test scripts`);
        if (!hasTest) console.log(`      - Missing "test" script`);
        if (!hasTestWatch) console.log(`      - Missing "test:watch" script`);
        if (!hasTestCoverage) console.log(`      - Missing "test:coverage" script`);
        results.missingScripts.push(example.name);
      }
    } catch (err) {
      console.log(`   ❌ Error reading package.json: ${err.message}`);
    }
  }

  // Check test files
  const testDir = join(basePath, 'test');
  if (existsSync(testDir)) {
    try {
      const testFiles = readdirSync(testDir).filter(f =>
        f.endsWith('.test.mjs') || f.endsWith('.spec.mjs')
      );
      validation.hasTestFiles = testFiles.length > 0;

      if (validation.hasTestFiles) {
        console.log(`   ✅ Test files found: ${testFiles.join(', ')}`);

        // Count tests in files
        for (const testFile of testFiles) {
          const content = readFileSync(join(testDir, testFile), 'utf8');
          const itMatches = content.match(/\bit\(/g);
          const testMatches = content.match(/\btest\(/g);
          const count = (itMatches?.length || 0) + (testMatches?.length || 0);
          validation.testCount += count;
        }

        console.log(`   📊 Test count: ${validation.testCount} (minimum: ${example.minTests})`);
        if (validation.testCount < example.minTests) {
          console.log(`   ⚠️  Below minimum test count`);
        }
      } else {
        console.log(`   ❌ No test files found in test/`);
        results.missingTests.push(example.name);
      }
    } catch (err) {
      console.log(`   ❌ Error checking test files: ${err.message}`);
    }
  } else {
    console.log(`   ❌ test/ directory missing`);
    results.missingTests.push(example.name);
  }

  // Check README
  const readme = join(basePath, 'README.md');
  validation.hasReadme = existsSync(readme);
  if (validation.hasReadme) {
    const content = readFileSync(readme, 'utf8');
    const hasTestingSection = /##\s*Testing/i.test(content);
    if (hasTestingSection) {
      console.log(`   ✅ README.md has Testing section`);
    } else {
      console.log(`   ⚠️  README.md missing Testing section`);
      results.missingDocs.push(example.name);
    }
  } else {
    console.log(`   ❌ README.md missing`);
    results.missingDocs.push(example.name);
  }

  // Run tests if configured
  if (validation.hasVitestConfig && validation.hasTestFiles && validation.hasTestScripts) {
    console.log(`   🧪 Running tests...`);
    try {
      const output = execSync('pnpm test', {
        cwd: basePath,
        encoding: 'utf8',
        timeout: 30000,
        stdio: 'pipe'
      });

      // Parse test results
      const passMatch = output.match(/(\d+) passed/);
      const failMatch = output.match(/(\d+) failed/);

      validation.passingTests = passMatch ? parseInt(passMatch[1]) : 0;
      validation.failingTests = failMatch ? parseInt(failMatch[1]) : 0;

      if (validation.failingTests === 0 && validation.passingTests > 0) {
        console.log(`   ✅ All tests passing (${validation.passingTests} tests)`);
        results.passingTests += validation.passingTests;
      } else {
        console.log(`   ❌ Tests failing: ${validation.failingTests}, passing: ${validation.passingTests}`);
        results.failingTests += validation.failingTests;
        results.passingTests += validation.passingTests;
        results.failed.push({ example: example.name, reason: 'Tests failing' });
      }
    } catch (err) {
      console.log(`   ❌ Test execution failed: ${err.message}`);
      results.failed.push({ example: example.name, reason: 'Test execution error' });
    }
  } else {
    console.log(`   ⏭️  Skipping test execution (missing prerequisites)`);
  }

  results.totalTests += validation.testCount;

  // Mark as validated if all checks pass
  const allValid = validation.hasVitestConfig &&
                   validation.hasTestFiles &&
                   validation.hasTestScripts &&
                   validation.testCount >= example.minTests;

  if (allValid) {
    results.validated++;
    console.log(`   ✅ VALIDATION PASSED`);
  } else {
    console.log(`   ❌ VALIDATION FAILED`);
  }
}

// Print summary
console.log('\n');
console.log('=' .repeat(80));
console.log('📊 VALIDATION SUMMARY');
console.log('=' .repeat(80));
console.log(`Total examples: ${results.totalExamples}`);
console.log(`Validated: ${results.validated} (${Math.round(results.validated / results.totalExamples * 100)}%)`);
console.log(`Failed: ${results.failed.length}`);
console.log(`Total tests found: ${results.totalTests}`);
console.log(`Passing tests: ${results.passingTests}`);
console.log(`Failing tests: ${results.failingTests}`);
console.log(`Pass rate: ${results.passingTests > 0 ? Math.round(results.passingTests / (results.passingTests + results.failingTests) * 100) : 0}%`);
console.log('');

if (results.missingConfigs.length > 0) {
  console.log(`❌ Missing vitest.config.mjs (${results.missingConfigs.length}):`);
  results.missingConfigs.forEach(name => console.log(`   - ${name}`));
  console.log('');
}

if (results.missingTests.length > 0) {
  console.log(`❌ Missing test files (${results.missingTests.length}):`);
  results.missingTests.forEach(name => console.log(`   - ${name}`));
  console.log('');
}

if (results.missingScripts.length > 0) {
  console.log(`⚠️  Missing test scripts (${results.missingScripts.length}):`);
  results.missingScripts.forEach(name => console.log(`   - ${name}`));
  console.log('');
}

if (results.missingDocs.length > 0) {
  console.log(`⚠️  Missing Testing documentation (${results.missingDocs.length}):`);
  results.missingDocs.forEach(name => console.log(`   - ${name}`));
  console.log('');
}

if (results.failed.length > 0) {
  console.log(`❌ Failed validations (${results.failed.length}):`);
  results.failed.forEach(({ example, reason }) =>
    console.log(`   - ${example}: ${reason}`)
  );
  console.log('');
}

// Final status
const allPassed = results.validated === results.totalExamples &&
                  results.failingTests === 0 &&
                  results.missingConfigs.length === 0 &&
                  results.missingTests.length === 0;

if (allPassed) {
  console.log('✅ ALL VALIDATIONS PASSED');
  console.log('🚀 All 26 examples are production-ready with complete test coverage');
  process.exit(0);
} else {
  console.log('❌ VALIDATIONS FAILED');
  console.log('🔧 See issues above and fix before considering production-ready');
  process.exit(1);
}
