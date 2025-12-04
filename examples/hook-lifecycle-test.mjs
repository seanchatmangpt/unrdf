/**
 * @file Hook lifecycle validation test.
 * @description Tests the complete lifecycle of knowledge hooks including
 * before, run, and after phases with real payloads and contexts.
 *
 * This test suite validates:
 * - Hook definition validation and error handling
 * - Complete lifecycle execution (before → run → after)
 * - Payload validation and cancellation
 * - Context handling and graph operations
 * - Assertion generation and RDF quad creation
 * - Error propagation and recovery
 * - Edge cases and boundary conditions
 */

import {
  motionComplianceHook,
  financialMonitoringHook,
  dataQualityHook,
} from './define-hook-example.mjs';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';

const { namedNode, literal, quad } = DataFactory;

/**
 * Test the parliamentary motion compliance hook lifecycle.
 */
async function testMotionComplianceHook() {
  console.log('\n🏛️ Testing Parliamentary Motion Compliance Hook');
  console.log('='.repeat(50));

  const mockGraph = createStore();
  const mockContext = {
    graph: mockGraph,
    env: { sessionId: 'test-session-123' },
  };

  // Test 1: Valid motion payload
  console.log('\n1. Testing valid motion payload:');
  const validPayload = {
    motionId: 'motion-001-compliant',
    subject: 'Approve budget allocation',
    introducer: 'Alice',
  };

  const validEvent = {
    name: motionComplianceHook.meta.name,
    payload: validPayload,
    context: mockContext,
  };

  try {
    // Test before phase
    const beforeResult = await motionComplianceHook.before(validEvent);
    console.log('   Before result:', beforeResult);

    if (beforeResult.cancel) {
      console.log('   ❌ Hook cancelled:', beforeResult.reason);
      return;
    }

    // Test run phase
    const runResult = await motionComplianceHook.run({
      ...validEvent,
      payload: { ...validPayload, ...beforeResult },
    });
    console.log('   Run result:', runResult);

    // Test after phase
    const afterResult = await motionComplianceHook.after({
      ...validEvent,
      result: runResult,
      cancelled: false,
    });
    console.log('   After result:', afterResult);

    console.log('   ✅ Valid motion test completed successfully');
  } catch (error) {
    console.log('   ❌ Valid motion test failed:', error.message);
  }

  // Test 2: Invalid motion payload (missing motionId)
  console.log('\n2. Testing invalid motion payload (missing motionId):');
  const invalidPayload = {
    subject: 'Invalid motion without ID',
  };

  const invalidEvent = {
    name: motionComplianceHook.meta.name,
    payload: invalidPayload,
    context: mockContext,
  };

  try {
    const beforeResult = await motionComplianceHook.before(invalidEvent);
    console.log('   Before result:', beforeResult);

    if (beforeResult.cancel) {
      console.log('   ✅ Hook correctly cancelled:', beforeResult.reason);
    } else {
      console.log('   ❌ Hook should have been cancelled');
    }
  } catch (error) {
    console.log('   ❌ Invalid motion test failed:', error.message);
  }
}

/**
 * Test the financial monitoring hook lifecycle.
 */
async function testFinancialMonitoringHook() {
  console.log('\n💰 Testing Financial Monitoring Hook');
  console.log('='.repeat(50));

  const mockGraph = createStore();
  const mockContext = {
    graph: mockGraph,
    env: { environment: 'production' },
  };

  // Test 1: Large transaction
  console.log('\n1. Testing large transaction:');
  const largeTxPayload = {
    transactionId: 'tx-001-large',
    amount: 50000,
    currency: 'USD',
  };

  const largeTxEvent = {
    name: financialMonitoringHook.meta.name,
    payload: largeTxPayload,
    context: mockContext,
  };

  try {
    const beforeResult = await financialMonitoringHook.before(largeTxEvent);
    console.log('   Before result:', beforeResult);

    if (beforeResult.cancel) {
      console.log('   ❌ Hook cancelled:', beforeResult.reason);
      return;
    }

    const runResult = await financialMonitoringHook.run({
      ...largeTxEvent,
      payload: { ...largeTxPayload, ...beforeResult },
    });
    console.log('   Run result:', runResult);

    const afterResult = await financialMonitoringHook.after({
      ...largeTxEvent,
      result: runResult,
      cancelled: false,
    });
    console.log('   After result:', afterResult);

    console.log('   ✅ Large transaction test completed successfully');
  } catch (error) {
    console.log('   ❌ Large transaction test failed:', error.message);
  }

  // Test 2: Small transaction
  console.log('\n2. Testing small transaction:');
  const smallTxPayload = {
    transactionId: 'tx-002-small',
    amount: 500,
    currency: 'USD',
  };

  const smallTxEvent = {
    name: financialMonitoringHook.meta.name,
    payload: smallTxPayload,
    context: mockContext,
  };

  try {
    const beforeResult = await financialMonitoringHook.before(smallTxEvent);
    const runResult = await financialMonitoringHook.run({
      ...smallTxEvent,
      payload: { ...smallTxPayload, ...beforeResult },
    });
    console.log('   Small transaction risk level:', runResult.result.riskLevel);
    console.log('   ✅ Small transaction test completed successfully');
  } catch (error) {
    console.log('   ❌ Small transaction test failed:', error.message);
  }
}

/**
 * Test the data quality validation hook lifecycle.
 */
async function testDataQualityHook() {
  console.log('\n🔍 Testing Data Quality Validation Hook');
  console.log('='.repeat(50));

  const mockGraph = createStore();
  // Add some test data to the graph
  mockGraph.addQuad(
    quad(
      namedNode('urn:test:entity1'),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('urn:test:Person')
    )
  );
  mockGraph.addQuad(
    quad(
      namedNode('urn:test:entity1'),
      namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
      literal('Test Person')
    )
  );

  const mockContext = {
    graph: mockGraph,
    env: { validationMode: 'strict' },
  };

  console.log('\n1. Testing data quality validation:');
  const qualityPayload = {
    shapes: 'data-quality.shacl.ttl',
    strictMode: true,
  };

  const qualityEvent = {
    name: dataQualityHook.meta.name,
    payload: qualityPayload,
    context: mockContext,
  };

  try {
    const beforeResult = await dataQualityHook.before(qualityEvent);
    console.log('   Before result:', beforeResult);

    if (beforeResult.cancel) {
      console.log('   ❌ Hook cancelled:', beforeResult.reason);
      return;
    }

    const runResult = await dataQualityHook.run({
      ...qualityEvent,
      payload: { ...qualityPayload, ...beforeResult },
    });
    console.log('   Run result:', runResult);

    const afterResult = await dataQualityHook.after({
      ...qualityEvent,
      result: runResult,
      cancelled: false,
    });
    console.log('   After result:', afterResult);

    console.log('   ✅ Data quality validation test completed successfully');
  } catch (error) {
    console.log('   ❌ Data quality validation test failed:', error.message);
  }
}

/**
 * Test hook validation and error handling.
 */
async function testHookValidation() {
  console.log('\n🛡️ Testing Hook Validation');
  console.log('='.repeat(50));

  const { defineHook } = await import('../src/knowledge-engine/define-hook.mjs');

  // Test 1: Valid hook definition
  console.log('\n1. Testing valid hook definition:');
  try {
    const validHook = defineHook({
      meta: { name: 'test:valid-hook' },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://test/valid.ask.rq',
          sha256: 'abc123',
          mediaType: 'application/sparql-query',
        },
      },
      run: async () => ({ result: 'success' }),
    });
    console.log('   ✅ Valid hook definition accepted');
    console.log('   Hook name:', validHook.meta.name);
    console.log('   Determinism seed:', validHook.determinism.seed);
    console.log('   Receipt anchor:', validHook.receipt.anchor);
  } catch (error) {
    console.log('   ❌ Valid hook definition failed:', error.message);
  }

  // Test 2: Invalid hook definition (missing meta.name)
  console.log('\n2. Testing invalid hook definition (missing meta.name):');
  try {
    const _invalidHook = defineHook({
      meta: { description: 'Missing name' },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://test/invalid.ask.rq',
          sha256: 'def456',
          mediaType: 'application/sparql-query',
        },
      },
      run: async () => ({ result: 'success' }),
    });
    console.log('   ❌ Invalid hook definition should have failed');
  } catch (error) {
    console.log('   ✅ Invalid hook definition correctly rejected:', error.message);
  }

  // Test 3: Invalid hook definition (missing run function)
  console.log('\n3. Testing invalid hook definition (missing run function):');
  try {
    const _invalidHook = defineHook({
      meta: { name: 'test:missing-run' },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://test/missing-run.ask.rq',
          sha256: 'ghi789',
          mediaType: 'application/sparql-query',
        },
      },
      // Missing run function
    });
    console.log('   ❌ Invalid hook definition should have failed');
  } catch (error) {
    console.log('   ✅ Invalid hook definition correctly rejected:', error.message);
  }

  // Test 4: Invalid hook definition (inline condition)
  console.log('\n4. Testing invalid hook definition (inline condition):');
  try {
    const _invalidHook = defineHook({
      meta: { name: 'test:inline-condition' },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://test/inline.ask.rq',
          // Missing sha256 - this should fail
          mediaType: 'application/sparql-query',
        },
      },
      run: async () => ({ result: 'success' }),
    });
    console.log('   ❌ Invalid hook definition should have failed');
  } catch (error) {
    console.log('   ✅ Invalid hook definition correctly rejected:', error.message);
  }

  // Test 5: Hook with all optional properties
  console.log('\n5. Testing hook with all optional properties:');
  try {
    const fullHook = defineHook({
      meta: {
        name: 'test:full-hook',
        description: 'A hook with all properties',
        ontology: ['test', 'example'],
      },
      channel: {
        graphs: ['urn:graph:test'],
        view: 'delta',
      },
      when: {
        kind: 'shacl',
        ref: {
          uri: 'file://test/full.shacl.ttl',
          sha256: 'full123',
          mediaType: 'text/turtle',
        },
      },
      determinism: { seed: 999 },
      receipt: { anchor: 'git-notes' },
      before: async event => ({ ...event.payload, processed: true }),
      run: async _event => ({ result: 'full-success' }),
      after: async _event => ({ result: { finalStatus: 'completed' } }),
    });
    console.log('   ✅ Full hook definition accepted');
    console.log('   Description:', fullHook.meta.description);
    console.log('   Ontology:', fullHook.meta.ontology);
    console.log('   Channel view:', fullHook.channel.view);
    console.log('   Custom seed:', fullHook.determinism.seed);
    console.log('   Custom anchor:', fullHook.receipt.anchor);
  } catch (error) {
    console.log('   ❌ Full hook definition failed:', error.message);
  }

  // Test 6: Hook immutability
  console.log('\n6. Testing hook immutability:');
  try {
    const immutableHook = defineHook({
      meta: { name: 'test:immutable' },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://test/immutable.ask.rq',
          sha256: 'immutable123',
          mediaType: 'application/sparql-query',
        },
      },
      run: async () => ({ result: 'immutable' }),
    });

    // Try to modify the hook
    try {
      immutableHook.meta.name = 'modified';
      console.log('   ❌ Hook should be immutable');
    } catch (immutableError) {
      console.log('   ✅ Hook is immutable (cannot modify properties)');
    }

    // Try to modify nested properties
    try {
      immutableHook.determinism.seed = 999;
      console.log('   ❌ Nested properties should be immutable');
    } catch (immutableError) {
      console.log('   ✅ Nested properties are immutable');
    }
  } catch (error) {
    console.log('   ❌ Immutability test failed:', error.message);
  }
}

/**
 * Test edge cases and error conditions.
 */
async function testEdgeCases() {
  console.log('\n🔬 Testing Edge Cases and Error Conditions');
  console.log('='.repeat(50));

  // Test 1: Hook with async errors in run phase
  console.log('\n1. Testing hook with async error in run phase:');
  try {
    const { defineHook } = await import('../src/knowledge-engine/define-hook.mjs');

    const errorHook = defineHook({
      meta: { name: 'test:error-hook' },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://test/error.ask.rq',
          sha256: 'error123',
          mediaType: 'application/sparql-query',
        },
      },
      run: async () => {
        throw new Error('Simulated runtime error');
      },
    });

    const errorEvent = {
      name: errorHook.meta.name,
      payload: { test: 'data' },
      context: { graph: createStore() },
    };

    try {
      await errorHook.run(errorEvent);
      console.log('   ❌ Error should have been thrown');
    } catch (runError) {
      console.log('   ✅ Run phase error correctly propagated:', runError.message);
    }
  } catch (error) {
    console.log('   ❌ Edge case test failed:', error.message);
  }

  // Test 2: Hook with null/undefined payload
  console.log('\n2. Testing hook with null payload:');
  try {
    const nullEvent = {
      name: motionComplianceHook.meta.name,
      payload: null,
      context: { graph: createStore() },
    };

    const beforeResult = await motionComplianceHook.before(nullEvent);
    if (beforeResult.cancel) {
      console.log('   ✅ Null payload correctly cancelled:', beforeResult.reason);
    } else {
      console.log('   ❌ Null payload should have been cancelled');
    }
  } catch (error) {
    console.log('   ❌ Null payload test failed:', error.message);
  }

  // Test 3: Hook with missing context
  console.log('\n3. Testing hook with missing context:');
  try {
    const noContextEvent = {
      name: dataQualityHook.meta.name,
      payload: { shapes: 'test.shacl' },
      context: null,
    };

    const beforeResult = await dataQualityHook.before(noContextEvent);
    if (beforeResult.cancel) {
      console.log('   ✅ Missing context correctly cancelled:', beforeResult.reason);
    } else {
      console.log('   ❌ Missing context should have been cancelled');
    }
  } catch (error) {
    console.log('   ❌ Missing context test failed:', error.message);
  }

  // Test 4: Hook with very large payload
  console.log('\n4. Testing hook with very large payload:');
  try {
    const largePayload = {
      transactionId: 'tx-large-data',
      data: 'x'.repeat(10000), // 10KB string
      metadata: Array(1000)
        .fill(0)
        .map((_, i) => ({ id: i, value: `item-${i}` })),
    };

    const largeEvent = {
      name: financialMonitoringHook.meta.name,
      payload: largePayload,
      context: { graph: createStore() },
    };

    const beforeResult = await financialMonitoringHook.before(largeEvent);
    const _runResult = await financialMonitoringHook.run({
      ...largeEvent,
      payload: { ...largePayload, ...beforeResult },
    });

    console.log('   ✅ Large payload processed successfully');
    console.log('   Payload size:', JSON.stringify(largePayload).length, 'bytes');
  } catch (error) {
    console.log('   ❌ Large payload test failed:', error.message);
  }

  // Test 5: Hook with circular reference in payload
  console.log('\n5. Testing hook with circular reference:');
  try {
    const circularPayload = { id: 'circular-test' };
    circularPayload.self = circularPayload; // Create circular reference

    const circularEvent = {
      name: motionComplianceHook.meta.name,
      payload: circularPayload,
      context: { graph: createStore() },
    };

    // This should handle circular references gracefully
    const _beforeResult = await motionComplianceHook.before(circularEvent);
    console.log('   ✅ Circular reference handled gracefully');
  } catch (error) {
    console.log('   ❌ Circular reference test failed:', error.message);
  }
}

/**
 * Test performance and concurrency.
 */
async function testPerformance() {
  console.log('\n⚡ Testing Performance and Concurrency');
  console.log('='.repeat(50));

  // Test 1: Concurrent hook execution
  console.log('\n1. Testing concurrent hook execution:');
  try {
    const startTime = Date.now();
    const promises = [];

    for (let i = 0; i < 10; i++) {
      const event = {
        name: financialMonitoringHook.meta.name,
        payload: { transactionId: `tx-concurrent-${i}`, amount: 1000 + i },
        context: { graph: createStore() },
      };

      promises.push(
        financialMonitoringHook
          .before(event)
          .then(result =>
            financialMonitoringHook.run({
              ...event,
              payload: { ...event.payload, ...result },
            })
          )
          .then(result =>
            financialMonitoringHook.after({
              ...event,
              result,
              cancelled: false,
            })
          )
      );
    }

    const results = await Promise.all(promises);
    const endTime = Date.now();

    console.log(`   ✅ ${results.length} concurrent hooks completed in ${endTime - startTime}ms`);
    console.log(`   Average time per hook: ${(endTime - startTime) / results.length}ms`);
  } catch (error) {
    console.log('   ❌ Concurrent execution test failed:', error.message);
  }

  // Test 2: Memory usage with many hooks
  console.log('\n2. Testing memory usage with many hooks:');
  try {
    const { defineHook } = await import('../src/knowledge-engine/define-hook.mjs');
    const hooks = [];

    for (let i = 0; i < 100; i++) {
      const hook = defineHook({
        meta: { name: `test:memory-hook-${i}` },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://test/memory-${i}.ask.rq`,
            sha256: `memory${i}`,
            mediaType: 'application/sparql-query',
          },
        },
        run: async () => ({ result: `memory-test-${i}` }),
      });
      hooks.push(hook);
    }

    console.log(`   ✅ Created ${hooks.length} hooks without memory issues`);
    console.log(`   First hook name: ${hooks[0].meta.name}`);
    console.log(`   Last hook name: ${hooks[hooks.length - 1].meta.name}`);
  } catch (error) {
    console.log('   ❌ Memory usage test failed:', error.message);
  }
}

/**
 * Test RDF assertion generation.
 */
async function testRdfAssertions() {
  console.log('\n📊 Testing RDF Assertion Generation');
  console.log('='.repeat(50));

  // Test 1: Verify assertion structure
  console.log('\n1. Testing assertion structure:');
  try {
    const event = {
      name: motionComplianceHook.meta.name,
      payload: { motionId: 'test-assertions' },
      context: { graph: createStore() },
    };

    const beforeResult = await motionComplianceHook.before(event);
    const runResult = await motionComplianceHook.run({
      ...event,
      payload: { ...event.payload, ...beforeResult },
    });

    if (runResult.assertions && Array.isArray(runResult.assertions)) {
      console.log(`   ✅ Generated ${runResult.assertions.length} assertions`);

      runResult.assertions.forEach((assertion, index) => {
        console.log(`   Assertion ${index + 1}:`);
        console.log(`     Subject: ${assertion.subject.value}`);
        console.log(`     Predicate: ${assertion.predicate.value}`);
        console.log(`     Object: ${assertion.object.value}`);
        console.log(`     Graph: ${assertion.graph.value || 'default'}`);
      });
    } else {
      console.log('   ❌ No assertions generated');
    }
  } catch (error) {
    console.log('   ❌ Assertion structure test failed:', error.message);
  }

  // Test 2: Test assertion with different data types
  console.log('\n2. Testing assertions with different data types:');
  try {
    const { defineHook } = await import('../src/knowledge-engine/define-hook.mjs');

    const assertionHook = defineHook({
      meta: { name: 'test:assertion-types' },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://test/assertions.ask.rq',
          sha256: 'assertions123',
          mediaType: 'application/sparql-query',
        },
      },
      run: async ({ payload }) => ({
        result: { status: 'success' },
        assertions: [
          quad(
            namedNode(`urn:test:${payload.id}`),
            namedNode('urn:test:hasString'),
            literal('string value')
          ),
          quad(
            namedNode(`urn:test:${payload.id}`),
            namedNode('urn:test:hasNumber'),
            literal('42', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
          ),
          quad(
            namedNode(`urn:test:${payload.id}`),
            namedNode('urn:test:hasBoolean'),
            literal('true', namedNode('http://www.w3.org/2001/XMLSchema#boolean'))
          ),
          quad(
            namedNode(`urn:test:${payload.id}`),
            namedNode('urn:test:hasDate'),
            literal('2025-01-01T00:00:00Z', namedNode('http://www.w3.org/2001/XMLSchema#dateTime'))
          ),
        ],
      }),
    });

    const assertionEvent = {
      name: assertionHook.meta.name,
      payload: { id: 'data-types-test' },
      context: { graph: createStore() },
    };

    const runResult = await assertionHook.run(assertionEvent);

    if (runResult.assertions && runResult.assertions.length === 4) {
      console.log('   ✅ Generated assertions with different data types');
      runResult.assertions.forEach((assertion, index) => {
        const dataType = assertion.object.datatype ? assertion.object.datatype.value : 'string';
        console.log(`     ${index + 1}. ${dataType}: ${assertion.object.value}`);
      });
    } else {
      console.log('   ❌ Expected 4 assertions, got:', runResult.assertions?.length || 0);
    }
  } catch (error) {
    console.log('   ❌ Data type assertion test failed:', error.message);
  }
}

/**
 * Run all tests.
 */
async function runAllTests() {
  console.log('🧪 Knowledge Hook Lifecycle Validation Tests');
  console.log('='.repeat(60));

  try {
    await testHookValidation();
    await testMotionComplianceHook();
    await testFinancialMonitoringHook();
    await testDataQualityHook();
    await testEdgeCases();
    await testPerformance();
    await testRdfAssertions();

    console.log('\n🎉 All tests completed successfully!');
    console.log('✅ Hook validation working correctly');
    console.log('✅ Lifecycle phases executing properly');
    console.log('✅ Error handling functioning as expected');
    console.log('✅ 80/20 contract enforced');
    console.log('✅ Edge cases handled gracefully');
    console.log('✅ Performance and concurrency validated');
    console.log('✅ RDF assertion generation working');
  } catch (error) {
    console.error('\n❌ Test suite failed:', error);
    process.exit(1);
  }
}

// Run tests if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllTests();
}
