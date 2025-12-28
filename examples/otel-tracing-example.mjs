#!/usr/bin/env node
/**
 * @fileoverview OTEL Tracing Example
 *
 * Demonstrates how to use the OTEL instrumentation module for
 * distributed tracing in UNRDF operations.
 *
 * Run: node examples/otel-tracing-example.mjs
 */

import {
  createSpan,
  withSpan,
  recordAttribute,
  recordError,
  recordMetric,
  traceSPARQLQuery,
  traceTriplePattern,
  traceRPCCall,
  traceCacheOperation,
  traceBatchOperation,
  traceWithTiming,
  SERVICE_NAME,
  TRACER_VERSION,
} from '../src/otel-instrumentation.mjs';

/**
 * Simulate a database query
 */
async function simulateDatabaseQuery() {
  return traceSPARQLQuery('SELECT * WHERE { ?s ?p ?o } LIMIT 10', async () => {
    await new Promise(resolve => setTimeout(resolve, 15));
    return [
      { s: 'ex:alice', p: 'ex:knows', o: 'ex:bob' },
      { s: 'ex:bob', p: 'ex:knows', o: 'ex:charlie' },
    ];
  });
}

/**
 * Simulate a triple pattern match
 */
async function simulateTripleMatch() {
  return traceTriplePattern('ex:alice', 'ex:knows', null, async () => {
    await new Promise(resolve => setTimeout(resolve, 10));
    return [{ object: 'ex:bob' }];
  });
}

/**
 * Simulate an RPC call
 */
async function simulateRPCCall() {
  return traceRPCCall('node://worker-1:8080', 'rdf_handler', async () => {
    await new Promise(resolve => setTimeout(resolve, 20));
    return { status: 'ok', result: 'processed' };
  });
}

/**
 * Simulate cache operations
 */
async function simulateCacheOperation() {
  // Cache miss
  const missResult = await traceCacheOperation('query:hash123', false, async () => {
    await new Promise(resolve => setTimeout(resolve, 25));
    return { data: 'fetched' };
  });

  // Cache hit
  const hitResult = await traceCacheOperation('query:hash123', true, async () => {
    await new Promise(resolve => setTimeout(resolve, 1));
    return { data: 'cached' };
  });

  return { missResult, hitResult };
}

/**
 * Simulate batch operation
 */
async function simulateBatchInsert() {
  return traceBatchOperation('triple_insert', 1000, async () => {
    await new Promise(resolve => setTimeout(resolve, 50));
    return { inserted: 1000, failed: 0 };
  });
}

/**
 * Main example function
 */
async function main() {
  console.log(`OTEL Tracing Example`);
  console.log(`Service: ${SERVICE_NAME}, Version: ${TRACER_VERSION}`);
  console.log(`${'='.repeat(50)}\n`);

  // Example 1: Basic span with attributes
  console.log('1. Creating basic span with attributes...');
  const span = createSpan('example.operation', {
    'example.custom_attr': 'custom-value',
    'example.numeric_attr': 42,
  });
  recordMetric(span, 'items.count', 100);
  span.end();
  console.log('   Basic span created and ended.\n');

  // Example 2: withSpan helper
  console.log('2. Using withSpan helper...');
  const result = await withSpan('example.async_operation', async () => {
    await new Promise(resolve => setTimeout(resolve, 10));
    return 'operation-result';
  }, { 'operation.type': 'example' });
  console.log(`   Result: ${result}\n`);

  // Example 3: SPARQL query tracing
  console.log('3. Tracing SPARQL query...');
  const queryResults = await simulateDatabaseQuery();
  console.log(`   Query returned ${queryResults.length} results.\n`);

  // Example 4: Triple pattern tracing
  console.log('4. Tracing triple pattern match...');
  const matches = await simulateTripleMatch();
  console.log(`   Found ${matches.length} matches.\n`);

  // Example 5: RPC call tracing
  console.log('5. Tracing RPC call...');
  const rpcResult = await simulateRPCCall();
  console.log(`   RPC status: ${rpcResult.status}\n`);

  // Example 6: Cache operations
  console.log('6. Tracing cache operations...');
  const cacheResults = await simulateCacheOperation();
  console.log(`   Miss result: ${cacheResults.missResult.data}`);
  console.log(`   Hit result: ${cacheResults.hitResult.data}\n`);

  // Example 7: Batch operation
  console.log('7. Tracing batch insert...');
  const batchResult = await simulateBatchInsert();
  console.log(`   Inserted: ${batchResult.inserted}, Failed: ${batchResult.failed}\n`);

  // Example 8: Timing measurement
  console.log('8. Tracing with timing...');
  const { result: timedResult, duration } = await traceWithTiming('timed_operation', async () => {
    await new Promise(resolve => setTimeout(resolve, 30));
    return 'timed-result';
  });
  console.log(`   Result: ${timedResult}, Duration: ${duration.toFixed(2)}ms\n`);

  // Example 9: Error handling
  console.log('9. Error handling in spans...');
  try {
    await withSpan('error.example', async () => {
      throw new Error('Simulated error for demonstration');
    });
  } catch (e) {
    console.log(`   Caught error: ${e.message}`);
    console.log('   Error was recorded in span.\n');
  }

  // Example 10: Nested spans
  console.log('10. Nested span hierarchy...');
  await withSpan('parent.operation', async () => {
    console.log('    -> In parent span');
    await withSpan('child.operation', async () => {
      console.log('    -> In child span');
      await withSpan('grandchild.operation', async () => {
        console.log('    -> In grandchild span');
      });
    });
  });
  console.log('    Nested spans completed.\n');

  console.log(`${'='.repeat(50)}`);
  console.log('Example trace output complete.');
  console.log('\nTrace hierarchy created:');
  console.log(`
  example.operation
  example.async_operation
  query.sparql (SELECT)
    |-- triple.pattern (ex:alice, ex:knows, *)
  rpc.call (node://worker-1:8080)
  cache.operation (miss: query:hash123)
  cache.operation (hit: query:hash123)
  batch.operation (triple_insert, size=1000)
  perf.timed_operation
  error.example (ERROR)
  parent.operation
    |-- child.operation
        |-- grandchild.operation
  `);
}

main().catch(console.error);
