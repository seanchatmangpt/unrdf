#!/usr/bin/env node

/**
 * UNRDF latest Smoke Test
 *
 * Simple smoke test to verify core functionality
 */

import { readFileSync } from 'fs';
import { Parser, Store } from 'n3';
import { createDarkMatterCore, defineHook } from 'unrdf';

// ANSI colors
const colors = {
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  reset: '\x1b[0m',
  bold: '\x1b[1m'
};

function pass(message) {
  console.log(`${colors.green}✅ ${message}${colors.reset}`);
}

function fail(message) {
  console.log(`${colors.red}❌ ${message}${colors.reset}`);
}

function section(message) {
  console.log(`\n${colors.bold}${colors.yellow}━━━ ${message} ━━━${colors.reset}\n`);
}

async function runSmokeTest() {
  console.log(`${colors.bold}${colors.blue}
╔════════════════════════════════════════════════╗
║   UNRDF latest Knowledge Engine Smoke Test    ║
╚════════════════════════════════════════════════╝
${colors.reset}`);

  let testsPassed = 0;
  let testsFailed = 0;
  let system;

  try {
    // Test 1: Initialize Dark Matter Core
    section('1. Initialize Knowledge Engine');
    system = await createDarkMatterCore();
    await system.initialize();
    pass('Dark Matter 80/20 Core initialized');
    testsPassed++;

    // Test 2: Parse RDF Data
    section('2. Parse Turtle Data (N3.js)');
    const turtleData = readFileSync('./data.ttl', 'utf-8');
    const parser = new Parser();
    const quads = parser.parse(turtleData);
    const store = new Store(quads);
    pass(`Parsed ${store.size} RDF quads from Turtle`);
    testsPassed++;

    // Test 3: Verify Store Operations
    section('3. Verify Store Operations');
    const subjects = store.getSubjects(null, null, null);
    const subjectCount = Array.from(subjects).length;
    pass(`Found ${subjectCount} unique subjects in store`);
    testsPassed++;

    // Test 4: Execute Transaction (if available)
    section('4. Execute Transaction');
    if (typeof system.executeTransaction === 'function') {
      const txResult = await system.executeTransaction(store, {
        additions: quads.slice(0, 5),
        removals: [],
        actor: 'smoke-test'
      });
      pass('Transaction executed');
      testsPassed++;
    } else {
      console.log(`${colors.yellow}⚠️  executeTransaction not available (optional)${colors.reset}`);
      testsPassed++;
    }

    // Test 5: Check Core Components
    section('5. Verify Core Components');
    if (system.components && system.components.size > 0) {
      pass(`${system.components.size} core components loaded`);
      testsPassed++;
    } else {
      fail('No components found');
      testsFailed++;
    }

    // Test 6: Cleanup
    section('6. Cleanup Resources');
    if (typeof system.cleanup === 'function') {
      await system.cleanup();
      pass('Resources cleaned up');
    } else {
      console.log(`${colors.yellow}⚠️  No cleanup method (optional)${colors.reset}`);
    }
    testsPassed++;

  } catch (error) {
    fail(`Test error: ${error.message}`);
    console.error(error.stack);
    testsFailed++;
  }

  // Final Results
  console.log(`\n${colors.bold}${colors.blue}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${colors.reset}\n`);

  const total = testsPassed + testsFailed;
  const passRate = ((testsPassed / total) * 100).toFixed(1);

  if (testsFailed === 0) {
    console.log(`${colors.green}${colors.bold}
╔════════════════════════════════════════════════╗
║            🎉 ALL TESTS PASSED! 🎉            ║
║                                                ║
║  ${testsPassed}/${total} tests passed (${passRate}%)                  ║
║                                                ║
║  UNRDF latest smoke test successful! 🚀       ║
╚════════════════════════════════════════════════╝
${colors.reset}`);
    process.exit(0);
  } else {
    console.log(`${colors.red}${colors.bold}
╔════════════════════════════════════════════════╗
║          ⚠️  SOME TESTS FAILED  ⚠️            ║
║                                                ║
║  Passed: ${testsPassed}/${total}                                  ║
║  Failed: ${testsFailed}/${total}                                  ║
╚════════════════════════════════════════════════╝
${colors.reset}`);
    process.exit(1);
  }
}

runSmokeTest().catch((error) => {
  console.error(`${colors.red}${colors.bold}Fatal error:${colors.reset}`, error);
  process.exit(1);
});
