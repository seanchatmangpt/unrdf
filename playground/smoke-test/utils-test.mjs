#!/usr/bin/env node

/**
 * UNRDF Utils Smoke Test
 *
 * Tests utility functions
 */

import {
import { createStore, dataFactory } from '@unrdf/oxigraph';
  generateId,
  validateIRI,
  validateNamedNode,
  mergeStores,
  createNamespaceManager
} from 'unrdf/utils';
const { namedNode, literal, quad  } = dataFactory;

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

async function runUtilsTest() {
  console.log(`${colors.bold}${colors.blue}
╔════════════════════════════════════════════════╗
║        UNRDF Utils API Smoke Test             ║
╚════════════════════════════════════════════════╝
${colors.reset}`);

  let testsPassed = 0;
  let testsFailed = 0;

  try {
    // Test 1: generateId
    section('1. generateId - ID Generation');
    const id = generateId();
    if (id && id.length > 0) {
      pass(`generateId working (${id.substring(0, 20)}...)`);
      testsPassed++;
    } else {
      fail('generateId failed');
      testsFailed++;
    }

    // Test 2: validateNamedNode
    section('2. validateNamedNode - Named Node Validation');
    const validNode = namedNode('http://example.org/resource');
    const isValid = validateNamedNode(validNode);
    if (isValid) {
      pass('validateNamedNode working (named node is valid)');
      testsPassed++;
    } else {
      fail('validateNamedNode failed');
      testsFailed++;
    }

    // Test 3: validateIRI
    section('3. validateIRI - IRI Validation');
    const validIRI = validateIRI('http://example.org/resource');
    const invalidIRI = validateIRI('not a valid iri');
    if (validIRI && !invalidIRI) {
      pass('validateIRI working (validates correctly)');
      testsPassed++;
    } else {
      fail('validateIRI failed');
      testsFailed++;
    }

    // Test 4: createNamespaceManager
    section('4. createNamespaceManager - Namespace Management');
    const nsManager = createNamespaceManager();
    nsManager.addNamespace('ex', 'http://example.org/');
    const expandedIRI = nsManager.expandIRI('ex:test');
    if (expandedIRI === 'http://example.org/test') {
      pass(`createNamespaceManager working (expanded ex:test to ${expandedIRI})`);
      testsPassed++;
    } else {
      fail('createNamespaceManager failed');
      testsFailed++;
    }

    // Test 5: mergeStores
    section('5. mergeStores - Graph Merging');
    const { Store } = await import('n3');
    const q1 = quad(
      namedNode('http://example.org/s1'),
      namedNode('http://example.org/p1'),
      literal('object1')
    );
    const q2 = quad(
      namedNode('http://example.org/s2'),
      namedNode('http://example.org/p2'),
      literal('object2')
    );
    const store1 = createStore([q1]);
    const store2 = createStore([q2]);
    const merged = mergeStores(store1, store2);
    if (merged && merged.size === 2) {
      pass(`mergeStores working (${merged.size} quads merged)`);
      testsPassed++;
    } else {
      fail('mergeStores failed');
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
║         🎉 ALL UTILS TESTS PASSED! 🎉         ║
║                                                ║
║  ${testsPassed}/${total} tests passed (${passRate}%)                  ║
║                                                ║
║  All utility functions working! 🚀            ║
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

runUtilsTest().catch((error) => {
  console.error(`${colors.red}${colors.bold}Fatal error:${colors.reset}`, error);
  process.exit(1);
});
