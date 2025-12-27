#!/usr/bin/env node

/**
 * Run all smoke tests
 */

import { spawn } from 'child_process';

const colors = {
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  reset: '\x1b[0m',
  bold: '\x1b[1m'
};

const tests = [
  { name: 'Knowledge Engine', file: './smoke-test.mjs' },
  { name: 'Composables API', file: './composables-test.mjs' },
  { name: 'Utils API', file: './utils-test.mjs' }
];

async function runTest(test) {
  return new Promise((resolve) => {
    const proc = spawn('node', [test.file], {
      stdio: 'inherit'
    });

    proc.on('close', (code) => {
      resolve({ name: test.name, passed: code === 0 });
    });
  });
}

async function runAllTests() {
  console.log(`${colors.bold}${colors.blue}
╔════════════════════════════════════════════════╗
║         UNRDF v3.0.1 Full Smoke Test          ║
║                                                ║
║  Testing all components of UNRDF              ║
╚════════════════════════════════════════════════╝
${colors.reset}\n`);

  const results = [];

  for (const test of tests) {
    console.log(`${colors.bold}${colors.yellow}Running ${test.name}...${colors.reset}\n`);
    const result = await runTest(test);
    results.push(result);
    console.log('\n');
  }

  // Summary
  console.log(`${colors.bold}${colors.blue}
╔════════════════════════════════════════════════╗
║              TEST SUITE SUMMARY                ║
╚════════════════════════════════════════════════╝
${colors.reset}\n`);

  const passed = results.filter(r => r.passed).length;
  const failed = results.filter(r => !r.passed).length;

  results.forEach(result => {
    if (result.passed) {
      console.log(`${colors.green}✅ ${result.name}${colors.reset}`);
    } else {
      console.log(`${colors.red}❌ ${result.name}${colors.reset}`);
    }
  });

  console.log(`\n${colors.bold}${colors.blue}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${colors.reset}\n`);

  const passRate = ((passed / results.length) * 100).toFixed(1);

  if (failed === 0) {
    console.log(`${colors.green}${colors.bold}
╔════════════════════════════════════════════════╗
║     🎉 ALL TEST SUITES PASSED! 🎉            ║
║                                                ║
║  ${passed}/${results.length} test suites passed (${passRate}%)           ║
║                                                ║
║  UNRDF v3.0.1 is fully validated! 🚀         ║
╚════════════════════════════════════════════════╝
${colors.reset}`);
    process.exit(0);
  } else {
    console.log(`${colors.red}${colors.bold}
╔════════════════════════════════════════════════╗
║       ⚠️  SOME TEST SUITES FAILED  ⚠️         ║
║                                                ║
║  Passed: ${passed}/${results.length}                                  ║
║  Failed: ${failed}/${results.length}                                  ║
╚════════════════════════════════════════════════╝
${colors.reset}`);
    process.exit(1);
  }
}

runAllTests().catch((error) => {
  console.error(`${colors.red}${colors.bold}Fatal error:${colors.reset}`, error);
  process.exit(1);
});
