/**
 * @fileoverview Browser compatibility validation with OTEL spans
 *
 * Validates browser implementation using OpenTelemetry tracing:
 * - IndexedDB quad store
 * - SPARQL query execution
 * - Lockchain audit trail
 * - Performance benchmarks
 *
 * Usage:
 *   node validation/browser-validation.mjs
 *
 * @module validation/browser-validation
 */

import { trace, context } from '@opentelemetry/api';
import { IndexedDBQuadStore } from '../src/browser/indexeddb-store.mjs';
import { BrowserQueryExecutor } from '../src/browser/comunica-browser-adapter.mjs';
import { BrowserLockchainWriter } from '../src/browser/browser-lockchain-writer.mjs';
import { DataFactory } from '@rdfjs/data-model';

const { namedNode, literal, quad } = DataFactory;

// Mock IndexedDB for Node.js testing
if (typeof indexedDB === 'undefined') {
  const { IDBFactory } = await import('fake-indexeddb');
  global.indexedDB = new IDBFactory();
}

// Mock Web Crypto API
if (typeof globalThis.crypto === 'undefined' || !globalThis.crypto.subtle) {
  globalThis.crypto = {
    subtle: {
      digest: async (algo, data) => {
        const hash = new Uint8Array(32);
        for (let i = 0; i < 32; i++) {
          hash[i] = (data[i % data.length] || 0) ^ i;
        }
        return hash.buffer;
      },
    },
    getRandomValues: (arr) => {
      for (let i = 0; i < arr.length; i++) {
        arr[i] = Math.floor(Math.random() * 256);
      }
      return arr;
    },
  };
}

const tracer = trace.getTracer('browser-validation');

/**
 * Validation results
 */
const results = {
  passed: 0,
  failed: 0,
  warnings: 0,
  tests: [],
};

/**
 * Record test result
 */
function recordTest(name, passed, details = {}) {
  results.tests.push({ name, passed, ...details });
  if (passed) {
    results.passed++;
    console.log(`✅ ${name}`);
  } else {
    results.failed++;
    console.log(`❌ ${name}`);
  }
}

/**
 * Validate IndexedDB quad store
 */
async function validateQuadStore() {
  return tracer.startActiveSpan('validate-quad-store', async (span) => {
    try {
      span.setAttribute('component', 'browser');
      span.setAttribute('test.type', 'quad-store');

      const store = new IndexedDBQuadStore();
      await store.init();

      // Test 1: Add and retrieve quads
      const testQuad = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      await store.addQuad(testQuad);
      const size = await store.size();

      recordTest('Quad Store: Add and retrieve', size === 1, {
        expected: 1,
        actual: size,
      });

      // Test 2: Pattern matching
      const matches = await store.match({
        subject: namedNode('http://example.org/alice'),
      });

      recordTest('Quad Store: Pattern matching', matches.length === 1, {
        expected: 1,
        actual: matches.length,
      });

      // Test 3: Performance (10K quads)
      const startTime = Date.now();
      const quads = [];

      for (let i = 0; i < 10000; i++) {
        quads.push(
          quad(
            namedNode(`http://example.org/person${i}`),
            namedNode('http://xmlns.com/foaf/0.1/name'),
            literal(`Person ${i}`)
          )
        );
      }

      await store.addQuads(quads);
      const loadTime = Date.now() - startTime;

      span.setAttribute('quads.count', 10000);
      span.setAttribute('performance.load_time_ms', loadTime);

      recordTest('Quad Store: Load 10K quads', loadTime < 5000, {
        threshold: '< 5000ms',
        actual: `${loadTime}ms`,
      });

      // Test 4: Query performance
      const queryStart = Date.now();
      const queryResults = await store.match({
        predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
      });
      const queryTime = Date.now() - queryStart;

      span.setAttribute('performance.query_time_ms', queryTime);

      recordTest('Quad Store: Query latency < 200ms', queryTime < 200, {
        threshold: '< 200ms',
        actual: `${queryTime}ms`,
      });

      await store.clear();
      store.close();

      span.setStatus({ code: 1 }); // OK
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message }); // ERROR
      recordTest('Quad Store: Initialization', false, { error: error.message });
    } finally {
      span.end();
    }
  });
}

/**
 * Validate SPARQL query executor
 */
async function validateQueryExecutor() {
  return tracer.startActiveSpan('validate-query-executor', async (span) => {
    try {
      span.setAttribute('component', 'browser');
      span.setAttribute('test.type', 'query-executor');

      const store = new IndexedDBQuadStore();
      const executor = new BrowserQueryExecutor(store);
      await executor.init();

      // Add sample data
      const quads = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        ),
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        ),
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/age'),
          literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
        ),
      ];

      await store.addQuads(quads);

      // Test 1: SELECT query
      const selectQuery = `
        SELECT ?name WHERE {
          <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> ?name .
        }
      `;

      const selectResults = await executor.select(selectQuery);
      recordTest('Query Executor: SELECT query', selectResults.length === 1, {
        expected: 1,
        actual: selectResults.length,
      });

      // Test 2: ASK query
      const askQuery = `
        ASK {
          <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
        }
      `;

      const askResult = await executor.ask(askQuery);
      recordTest('Query Executor: ASK query', askResult === true, {
        expected: true,
        actual: askResult,
      });

      // Test 3: CONSTRUCT query
      const constructQuery = `
        CONSTRUCT {
          ?person <http://example.org/hasName> ?name .
        } WHERE {
          ?person <http://xmlns.com/foaf/0.1/name> ?name .
        }
      `;

      const constructResults = await executor.construct(constructQuery);
      recordTest('Query Executor: CONSTRUCT query', constructResults.length === 1, {
        expected: 1,
        actual: constructResults.length,
      });

      await executor.close();
      await store.clear();
      store.close();

      span.setStatus({ code: 1 }); // OK
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message }); // ERROR
      recordTest('Query Executor: Initialization', false, { error: error.message });
    } finally {
      span.end();
    }
  });
}

/**
 * Validate lockchain writer
 */
async function validateLockchainWriter() {
  return tracer.startActiveSpan('validate-lockchain-writer', async (span) => {
    try {
      span.setAttribute('component', 'browser');
      span.setAttribute('test.type', 'lockchain-writer');

      const lockchain = new BrowserLockchainWriter();
      await lockchain.init();

      // Test 1: Record change
      const hash = await lockchain.recordChange({
        type: 'add',
        data: 'test data',
        author: 'validator',
        message: 'Validation test',
      });

      recordTest('Lockchain: Record change', typeof hash === 'string', {
        expected: 'string hash',
        actual: typeof hash,
      });

      // Test 2: Get history
      const history = await lockchain.getHistory();
      recordTest('Lockchain: Get history', history.length === 1, {
        expected: 1,
        actual: history.length,
      });

      // Test 3: Verify chain
      const verification = await lockchain.verifyChain();
      recordTest('Lockchain: Verify chain', verification.valid === true, {
        expected: true,
        actual: verification.valid,
      });

      // Test 4: Export/Import
      const exported = await lockchain.export();
      await lockchain.clear();

      await lockchain.import(exported);
      const importedHistory = await lockchain.getHistory();

      recordTest('Lockchain: Export/Import', importedHistory.length === 1, {
        expected: 1,
        actual: importedHistory.length,
      });

      await lockchain.clear();
      lockchain.close();

      span.setStatus({ code: 1 }); // OK
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message }); // ERROR
      recordTest('Lockchain: Initialization', false, { error: error.message });
    } finally {
      span.end();
    }
  });
}

/**
 * Calculate validation score
 */
function calculateScore() {
  const total = results.passed + results.failed;
  const percentage = total > 0 ? Math.round((results.passed / total) * 100) : 0;

  return {
    score: `${results.passed}/${total}`,
    percentage,
    passed: percentage >= 85, // Target: 85% minimum
  };
}

/**
 * Run all validations
 */
async function runValidations() {
  console.log('🌐 Browser Compatibility Validation\n');
  console.log('═══════════════════════════════════════════════════\n');

  await validateQuadStore();
  console.log('');

  await validateQueryExecutor();
  console.log('');

  await validateLockchainWriter();
  console.log('');

  const score = calculateScore();

  console.log('═══════════════════════════════════════════════════');
  console.log(`\n📊 Validation Results:`);
  console.log(`   Score: ${score.score} (${score.percentage}%)`);
  console.log(`   Passed: ${results.passed}`);
  console.log(`   Failed: ${results.failed}`);
  console.log(`   Warnings: ${results.warnings}`);

  if (score.passed) {
    console.log(`\n✅ PASSED: Browser compatibility meets acceptance criteria (≥85%)`);
  } else {
    console.log(`\n❌ FAILED: Browser compatibility below acceptance criteria (${score.percentage}% < 85%)`);
  }

  console.log('═══════════════════════════════════════════════════\n');

  // Exit with appropriate code
  process.exit(score.passed ? 0 : 1);
}

// Run validations
runValidations().catch(error => {
  console.error('❌ Validation failed:', error);
  process.exit(1);
});
