#!/usr/bin/env node

/**
 * @file Test Utils Demo
 * @description
 * Demonstrates the test utilities with scenario DSL and fluent assertions.
 */

import { scenario, expect, createTestContext, TestHelpers } from '../packages/test-utils/index.mjs';
import { KnowledgeHookManager } from '../packages/knowledge-engine/src/knowledge-hook-manager.mjs';
import { createStore } from '@unrdf/oxigraph';

console.log('🧪 Test Utils Demo\n');

async function demonstrateTestUtils() {
  try {
    // Test 1: Basic scenario with fluent assertions
    console.log('1️⃣  Basic Scenario Test:');

    const basicTest = await scenario('Basic Store Operations')
      .setupScenario(async () => {
        return createTestContext()
          .withStore(createStore())
          .withMetadata({ testType: 'basic' })
          .build();
      })
      .step(
        'Add initial data',
        async context => {
          const quad1 = TestHelpers.createQuad('ex:alice', 'ex:hasName', 'Alice');
          const quad2 = TestHelpers.createQuad('ex:bob', 'ex:hasName', 'Bob');

          context.store.addQuad(quad1);
          context.store.addQuad(quad2);

          return { quadsAdded: 2 };
        },
        [
          (context, result) => expect(context, result).toHaveProperty('quadsAdded', 2),
          (context, result) => expect(context, result).toHaveSize(2),
        ]
      )
      .step(
        'Validate store contents',
        async context => {
          const quads = context.store.getQuads();
          const names = quads
            .filter(q => q.predicate.value === 'ex:hasName')
            .map(q => q.object.value);

          return { names, totalQuads: quads.length };
        },
        [
          (context, result) => expect(context, result).toHaveProperty('totalQuads', 2),
          (context, result) => expect(context, result).toHaveProperty('names', ['Alice', 'Bob']),
        ]
      )
      .execute();

    console.log(`  ✅ Basic test: ${basicTest.success ? 'passed' : 'failed'}`);
    console.log(`  📊 Steps: ${basicTest.steps.length}, Duration: ${basicTest.duration}ms`);

    // Test 2: Knowledge Hook Manager integration
    console.log('\n2️⃣  Knowledge Hook Manager Test:');

    const hookTest = await scenario('Knowledge Hook Execution')
      .setupScenario(async () => {
        const manager = new KnowledgeHookManager({
          basePath: process.cwd(),
          strictMode: false,
        });

        // Add a test hook
        const testHook = {
          meta: {
            name: 'test-hook',
            description: 'Test hook for validation',
            version: '1.0.0',
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: 'file://test.rq',
              sha256: 'test-hash',
              mediaType: 'application/sparql-query',
            },
          },
          run: async _event => {
            return {
              success: true,
              message: 'Hook executed successfully',
              timestamp: Date.now(),
            };
          },
        };

        manager.addKnowledgeHook(testHook);

        return createTestContext().withStore(createStore()).withManager(manager).build();
      })
      .step(
        'Execute knowledge hook',
        async context => {
          const event = {
            name: 'test-event',
            payload: { data: 'test' },
            context: {
              graph: context.store,
              env: { testMode: true },
            },
          };

          const result = await context.manager.executeKnowledgeHook('test-hook', event);
          return result;
        },
        [
          (context, result) => expect(context, result).toHaveProperty('success', true),
          (context, result) =>
            expect(context, result).toHaveProperty('result.message', 'Hook executed successfully'),
        ]
      )
      .step(
        'Verify hook registration',
        async context => {
          const hooks = context.manager.getKnowledgeHooks();
          return {
            hookCount: hooks.length,
            hookNames: hooks.map(h => h.meta.name),
          };
        },
        [
          (context, result) => expect(context, result).toHaveProperty('hookCount', 1),
          (context, result) => expect(context, result).toHaveProperty('hookNames', ['test-hook']),
        ]
      )
      .execute();

    console.log(`  ✅ Hook test: ${hookTest.success ? 'passed' : 'failed'}`);
    console.log(`  📊 Steps: ${hookTest.steps.length}, Duration: ${hookTest.duration}ms`);

    // Test 3: Complex transaction scenario
    console.log('\n3️⃣  Complex Transaction Test:');

    const transactionTest = await scenario('Complex Transaction with Hooks')
      .setupScenario(async () => {
        const manager = new KnowledgeHookManager({
          basePath: process.cwd(),
          strictMode: false,
        });

        // Add multiple hooks
        const hooks = [
          {
            meta: {
              name: 'validation-hook',
              description: 'Validates data',
              version: '1.0.0',
            },
            when: {
              kind: 'sparql-ask',
              ref: {
                uri: 'file://validate.rq',
                sha256: 'val-hash',
                mediaType: 'application/sparql-query',
              },
            },
            run: async _event => ({ validated: true }),
          },
          {
            meta: {
              name: 'audit-hook',
              description: 'Audits changes',
              version: '1.0.0',
            },
            when: {
              kind: 'sparql-ask',
              ref: {
                uri: 'file://audit.rq',
                sha256: 'audit-hash',
                mediaType: 'application/sparql-query',
              },
            },
            run: async _event => ({ audited: true }),
          },
        ];

        for (const hook of hooks) {
          manager.addKnowledgeHook(hook);
        }

        return createTestContext().withStore(createStore()).withManager(manager).build();
      })
      .step(
        'Apply transaction with hooks',
        async context => {
          const delta = TestHelpers.createDelta([
            TestHelpers.createQuad('ex:user', 'ex:hasRole', 'ex:admin'),
            TestHelpers.createQuad('ex:user', 'ex:hasPermission', 'ex:write'),
          ]);

          const result = await context.manager.apply(context.store, delta, {
            actor: 'test-user',
          });
          return result;
        },
        [
          (context, result) => expect(context, result).toBeCommitted(),
          (context, result) =>
            expect(context, result).toHaveProperty('receipt.hookResults.length', 2),
        ]
      )
      .step(
        'Verify store state',
        async context => {
          const quads = context.store.getQuads();
          const roleQuads = quads.filter(q => q.predicate.value === 'ex:hasRole');
          const permissionQuads = quads.filter(q => q.predicate.value === 'ex:hasPermission');

          return {
            totalQuads: quads.length,
            roleQuads: roleQuads.length,
            permissionQuads: permissionQuads.length,
          };
        },
        [
          (context, result) => expect(context, result).toHaveProperty('totalQuads', 2),
          (context, result) => expect(context, result).toHaveProperty('roleQuads', 1),
          (context, result) => expect(context, result).toHaveProperty('permissionQuads', 1),
        ]
      )
      .execute();

    console.log(`  ✅ Transaction test: ${transactionTest.success ? 'passed' : 'failed'}`);
    console.log(
      `  📊 Steps: ${transactionTest.steps.length}, Duration: ${transactionTest.duration}ms`
    );

    // Test 4: Performance and timing assertions
    console.log('\n4️⃣  Performance Test:');

    const performanceTest = await scenario('Performance Validation')
      .setupScenario(async () => {
        return createTestContext().withStore(createStore()).build();
      })
      .step(
        'Fast operation',
        async _context => {
          const start = Date.now();
          // Simulate fast operation
          await new Promise(resolve => setTimeout(resolve, 10));
          const duration = Date.now() - start;

          return { duration, operation: 'fast' };
        },
        [(context, result) => expect(context, result).toCompleteWithin(100)]
      )
      .step(
        'Slow operation',
        async _context => {
          const start = Date.now();
          // Simulate slow operation
          await new Promise(resolve => setTimeout(resolve, 200));
          const duration = Date.now() - start;

          return { duration, operation: 'slow' };
        },
        [(context, result) => expect(context, result).toCompleteWithin(500)]
      )
      .execute();

    console.log(`  ✅ Performance test: ${performanceTest.success ? 'passed' : 'failed'}`);
    console.log(
      `  📊 Steps: ${performanceTest.steps.length}, Duration: ${performanceTest.duration}ms`
    );

    // Test 5: Error handling and negative assertions
    console.log('\n5️⃣  Error Handling Test:');

    const errorTest = await scenario('Error Handling Validation')
      .setupScenario(async () => {
        return createTestContext().withStore(createStore()).build();
      })
      .step(
        'Successful operation',
        async _context => {
          return { success: true, data: 'valid' };
        },
        [
          (context, result) => expect(context, result).toHaveProperty('success', true),
          (context, result) => expect(context, result).toNotHaveProperty('error'),
        ]
      )
      .step(
        'Failed operation',
        async _context => {
          try {
            throw new Error('Simulated error');
          } catch (error) {
            return { success: false, error: error.message };
          }
        },
        [
          (context, result) => expect(context, result).toHaveProperty('success', false),
          (context, result) => expect(context, result).toHaveProperty('error', 'Simulated error'),
        ]
      )
      .execute();

    console.log(`  ✅ Error test: ${errorTest.success ? 'passed' : 'failed'}`);
    console.log(`  📊 Steps: ${errorTest.steps.length}, Duration: ${errorTest.duration}ms`);

    // Summary
    const allTests = [basicTest, hookTest, transactionTest, performanceTest, errorTest];
    const passedTests = allTests.filter(t => t.success).length;

    console.log('\n📊 Test Summary:');
    console.log(`  Total tests: ${allTests.length}`);
    console.log(`  Passed: ${passedTests}`);
    console.log(`  Failed: ${allTests.length - passedTests}`);
    console.log(`  Success rate: ${((passedTests / allTests.length) * 100).toFixed(1)}%`);

    console.log('\n🎉 Test Utils demo completed successfully!');
  } catch (error) {
    console.error('❌ Test Utils demo failed:', error.message);
    throw error;
  }
}

// Run the demo
demonstrateTestUtils().catch(error => {
  console.error('💥 Demo failed:', error);
  process.exit(1);
});
