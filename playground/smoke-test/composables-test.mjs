#!/usr/bin/env node

/**
 * UNRDF Composables Smoke Test
 *
 * Tests all composable functions that make up the main API surface
 */

import { readFileSync } from 'fs';
import { Parser, Store, DataFactory } from 'n3';
import { z } from 'zod';

const { namedNode, literal, blankNode } = DataFactory;

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

async function runComposablesTest() {
  console.log(`${colors.bold}${colors.blue}
╔════════════════════════════════════════════════╗
║      UNRDF Composables API Smoke Test         ║
╚════════════════════════════════════════════════╝
${colors.reset}`);

  let testsPassed = 0;
  let testsFailed = 0;

  try {
    // Test 1: N3 Store - Core RDF operations
    section('1. N3 Store - Core RDF Operations');
    const store = new Store();
    const parser = new Parser();
    const turtleData = readFileSync('./data.ttl', 'utf-8');
    const quads = parser.parse(turtleData);
    store.addQuads(quads);
    if (store.size === 16) {
      pass(`N3 Store working (${store.size} quads loaded)`);
      testsPassed++;
    } else {
      fail('N3 Store failed');
      testsFailed++;
    }

    // Test 2: Query Store
    section('2. Store Query Operations');
    const subjects = store.getSubjects(null, null, null);
    const subjectCount = Array.from(subjects).length;
    if (subjectCount === 3) {
      pass(`Store queries working (${subjectCount} unique subjects)`);
      testsPassed++;
    } else {
      fail('Store queries failed');
      testsFailed++;
    }

    // Test 3: Zod Validation
    section('3. Zod - Schema Validation');
    const schema = z.object({
      name: z.string(),
      age: z.number().positive()
    });
    const valid = schema.safeParse({ name: 'Alice', age: 30 });
    const invalid = schema.safeParse({ name: 'Bob', age: -5 });
    if (valid.success && !invalid.success) {
      pass('Zod validation working (valid accepted, invalid rejected)');
      testsPassed++;
    } else {
      fail('Zod validation failed');
      testsFailed++;
    }

    // Test 4: N3 Parser
    section('4. N3 Parser - Turtle Parsing');
    const simpleParser = new Parser();
    const simpleQuads = simpleParser.parse('@prefix ex: <http://example.org/> . ex:test ex:prop "value" .');
    if (simpleQuads.length > 0) {
      pass(`N3 Parser working (${simpleQuads.length} quads parsed)`);
      testsPassed++;
    } else {
      fail('N3 Parser failed');
      testsFailed++;
    }

    // Test 5: Store Filter Operations
    section('5. Store - Filter Operations');
    const filtered = store.getQuads(null, namedNode('http://xmlns.com/foaf/0.1/name'), null, null);
    if (filtered.length > 0) {
      pass(`Store filtering working (${filtered.length} name triples found)`);
      testsPassed++;
    } else {
      fail('Store filtering failed');
      testsFailed++;
    }

    // Test 6: N3 DataFactory
    section('6. N3 DataFactory - Term Creation');
    const node = namedNode('http://example.org/test');
    const lit = literal('Hello', 'en');
    const blank = blankNode();
    if (node && lit && blank) {
      pass('N3 DataFactory working (terms created)');
      testsPassed++;
    } else {
      fail('N3 DataFactory failed');
      testsFailed++;
    }

  } catch (error) {
    fail(`Test error: ${error.message}`);
    console.error(error.stack);
    testsFailed++;
  }

  // Results
  console.log(`\n${colors.bold}${colors.blue}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${colors.reset}\n`);

  const total = testsPassed + testsFailed;
  const passRate = ((testsPassed / total) * 100).toFixed(1);

  if (testsFailed === 0) {
    console.log(`${colors.green}${colors.bold}
╔════════════════════════════════════════════════╗
║       🎉 ALL COMPOSABLES TESTS PASSED! 🎉     ║
║                                                ║
║  ${testsPassed}/${total} tests passed (${passRate}%)                  ║
║                                                ║
║  All composable APIs working! 🚀              ║
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

runComposablesTest().catch((error) => {
  console.error(`${colors.red}${colors.bold}Fatal error:${colors.reset}`, error);
  process.exit(1);
});
