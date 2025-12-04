/**
 * @fileoverview Knowledge Hooks Event System - Complete Example
 *
 * Demonstrates the new event-based knowledge hooks system with:
 * - ObservableStore with before/after events
 * - Engine-level event emission
 * - Hook registration and management
 * - Batch operations
 * - Performance monitoring
 *
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { initStore } from '../src/context/index.mjs';
import { useKnowledgeHooks, defineHook } from '../src/composables/use-knowledge-hooks.mjs';
import { EVENTS } from '../src/engines/event-bus.mjs';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';

const { namedNode, literal } = DataFactory; // quad unused

/**
 * Example 1: Basic Event Hooks
 */
async function basicEventHooks() {
  console.log('\n=== Basic Event Hooks ===');

  const runApp = initStore([], {
    baseIRI: 'http://example.org/',
    eventsEnabled: true,
  });

  await runApp(async () => {
    const hooks = useKnowledgeHooks();

    // Register hook for quad additions
    const unregister1 = hooks.registerEventHook(
      EVENTS.AFTER_ADD_QUAD,
      async payload => {
        console.log(
          `✅ Quad added: ${payload.quad.subject.value} ${payload.quad.predicate.value} ${payload.quad.object.value}`
        );
        console.log(
          `   Context: ${payload.context.source} at ${payload.context.timestamp.toISOString()}`
        );
      },
      { id: 'quad-monitor' }
    );

    // Register hook for quad removals
    const unregister2 = hooks.registerEventHook(
      EVENTS.AFTER_REMOVE_QUAD,
      async payload => {
        console.log(
          `❌ Quad removed: ${payload.quad.subject.value} ${payload.quad.predicate.value} ${payload.quad.object.value}`
        );
      },
      { id: 'removal-monitor' }
    );

    // Register hook for store clearing
    const unregister3 = hooks.registerEventHook(
      EVENTS.AFTER_CLEAR,
      async payload => {
        console.log(`🧹 Store cleared: ${payload.quad.length} quads removed`);
      },
      { id: 'clear-monitor' }
    );

    // Perform operations
    const store = hooks.engine.store;

    console.log('Adding quads...');
    store.addQuad(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/name'),
      literal('Alice')
    );

    store.addQuad(
      namedNode('http://example.org/bob'),
      namedNode('http://example.org/name'),
      literal('Bob')
    );

    console.log('Removing a quad...');
    store.removeQuad(
      namedNode('http://example.org/bob'),
      namedNode('http://example.org/name'),
      literal('Bob')
    );

    console.log('Clearing store...');
    store.clear();

    // Cleanup
    unregister1.unregister();
    unregister2.unregister();
    unregister3.unregister();
  });
}

/**
 * Example 2: Before Hooks with Veto Capability
 */
async function beforeHooksWithVeto() {
  console.log('\n=== Before Hooks with Veto ===');

  const runApp = initStore([], {
    baseIRI: 'http://example.org/',
    eventsEnabled: true,
  });

  await runApp(async () => {
    const hooks = useKnowledgeHooks();

    // Register before hook that can veto operations
    const unregister = hooks.registerEventHook(
      EVENTS.BEFORE_ADD_QUAD,
      async payload => {
        const quad = payload.quad;

        // Veto if trying to add a quad with "blocked" predicate
        if (quad.predicate.value.includes('blocked')) {
          console.log(`🚫 VETOED: Blocked predicate detected: ${quad.predicate.value}`);
          return false; // Veto the operation
        }

        console.log(
          `✅ ALLOWED: ${quad.subject.value} ${quad.predicate.value} ${quad.object.value}`
        );
        return true; // Allow the operation
      },
      { id: 'veto-monitor' }
    );

    const store = hooks.engine.store;

    console.log('Trying to add allowed quad...');
    const result1 = store.addQuad(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/name'),
      literal('Alice')
    );
    console.log(`Result: ${result1 ? 'Added' : 'Blocked'}`);

    console.log('Trying to add blocked quad...');
    const result2 = store.addQuad(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/blocked'),
      literal('Blocked')
    );
    console.log(`Result: ${result2 ? 'Added' : 'Blocked'}`);

    console.log(`Store size: ${store.size}`);

    unregister.unregister();
  });
}

/**
 * Example 3: Engine-Level Events
 */
async function engineLevelEvents() {
  console.log('\n=== Engine-Level Events ===');

  const runApp = initStore([], {
    baseIRI: 'http://example.org/',
    eventsEnabled: true,
  });

  await runApp(async () => {
    const hooks = useKnowledgeHooks();

    // Register hook for import events
    const unregister1 = hooks.registerEventHook(
      EVENTS.AFTER_IMPORT,
      async payload => {
        console.log(`📥 Import completed: ${payload.context.quadCount} quads imported`);
        console.log(`   Format: ${payload.context.metadata.format}`);
        console.log(`   Size: ${payload.context.metadata.size} bytes`);
      },
      { id: 'import-monitor' }
    );

    // Register hook for reasoning events
    const unregister2 = hooks.registerEventHook(
      EVENTS.AFTER_REASON,
      async payload => {
        console.log(`🧠 Reasoning completed: ${payload.context.newQuadCount} new triples inferred`);
      },
      { id: 'reasoning-monitor' }
    );

    // Register hook for SPARQL UPDATE events
    const unregister3 = hooks.registerEventHook(
      EVENTS.AFTER_UPDATE,
      async payload => {
        console.log(`🔄 SPARQL UPDATE completed: ${payload.context.operation}`);
      },
      { id: 'update-monitor' }
    );

    const engine = hooks.engine;

    console.log('Importing Turtle data...');
    engine.parseTurtle(`
      @prefix ex: <http://example.org/> .
      @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
      
      ex:alice rdf:type ex:Person ;
               ex:name "Alice" .
      ex:bob rdf:type ex:Person ;
             ex:name "Bob" .
    `);

    console.log('Executing SPARQL UPDATE...');
    await engine.query(`
      PREFIX ex: <http://example.org/>
      INSERT { ex:alice ex:hasFriend ex:bob }
      WHERE { ex:alice ex:name "Alice" }
    `);

    console.log('Running reasoning...');
    const rulesStore = engine.parseTurtle(`
      @prefix ex: <http://example.org/> .
      @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
      
      { ?x ex:hasFriend ?y } => { ?y ex:hasFriend ?x } .
    `);

    await engine.reason(engine.store, rulesStore);

    unregister1.unregister();
    unregister2.unregister();
    unregister3.unregister();
  });
}

/**
 * Example 4: Knowledge Hooks with Event Triggers
 */
async function knowledgeHooksWithEvents() {
  console.log('\n=== Knowledge Hooks with Event Triggers ===');

  const runApp = initStore([], {
    baseIRI: 'http://example.org/',
    eventsEnabled: true,
  });

  await runApp(async () => {
    const hooks = useKnowledgeHooks();

    // Define a knowledge hook that triggers on quad additions
    const errorHook = defineHook({
      id: 'error-count-monitor',
      events: [EVENTS.AFTER_ADD_QUAD],
      query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
      predicates: [{ kind: 'COUNT', spec: { operator: '>', value: 2 } }],
      options: {
        callback: async (result, payload) => {
          console.log(`🚨 ALERT: Error count exceeded threshold!`);
          console.log(`   Current errors: ${result.data.count}`);
          console.log(`   Triggered by: ${payload.quad.subject.value}`);
        },
      },
    });

    // Register the knowledge hook
    const unregister = hooks.registerKnowledgeHook(errorHook);

    const store = hooks.engine.store;

    console.log('Adding error entities...');
    store.addQuad(
      namedNode('http://example.org/error1'),
      namedNode('http://example.org/type'),
      namedNode('http://example.org/Error')
    );

    store.addQuad(
      namedNode('http://example.org/error2'),
      namedNode('http://example.org/type'),
      namedNode('http://example.org/Error')
    );

    console.log('Adding third error (should trigger alert)...');
    store.addQuad(
      namedNode('http://example.org/error3'),
      namedNode('http://example.org/type'),
      namedNode('http://example.org/Error')
    );

    unregister.unregister();
  });
}

/**
 * Example 5: Batch Operations
 */
async function batchOperations() {
  console.log('\n=== Batch Operations ===');

  const runApp = initStore([], {
    baseIRI: 'http://example.org/',
    eventsEnabled: true,
  });

  await runApp(async () => {
    const hooks = useKnowledgeHooks();

    // Register hook for batch operations
    const unregister = hooks.registerEventHook(
      EVENTS.AFTER_ADD_QUADS,
      async payload => {
        console.log(`📦 Batch operation: ${payload.context.quadCount} quads added`);
        console.log(`   Operation ID: ${payload.context.operationId}`);
      },
      { id: 'batch-monitor' }
    );

    const store = hooks.engine.store;

    console.log('Performing batch operations...');
    await hooks.batch(() => {
      // Multiple operations in batch - only one event fired
      store.addQuad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/name'),
        literal('Alice')
      );

      store.addQuad(
        namedNode('http://example.org/bob'),
        namedNode('http://example.org/name'),
        literal('Bob')
      );

      store.addQuad(
        namedNode('http://example.org/charlie'),
        namedNode('http://example.org/name'),
        literal('Charlie')
      );
    });

    console.log(`Final store size: ${store.size}`);

    unregister.unregister();
  });
}

/**
 * Example 6: Performance Monitoring
 */
async function performanceMonitoring() {
  console.log('\n=== Performance Monitoring ===');

  const runApp = initStore([], {
    baseIRI: 'http://example.org/',
    eventsEnabled: true,
  });

  await runApp(async () => {
    const hooks = useKnowledgeHooks();

    // Register performance monitoring hook
    const unregister = hooks.registerEventHook(
      EVENTS.AFTER_ADD_QUAD,
      async _payload => {
        // This hook intentionally does nothing to measure overhead
      },
      { id: 'performance-monitor' }
    );

    const store = hooks.engine.store;

    console.log('Measuring performance with 1000 quad additions...');

    const startTime = Date.now();

    for (let i = 0; i < 1000; i++) {
      store.addQuad(
        namedNode(`http://example.org/subject${i}`),
        namedNode('http://example.org/predicate'),
        literal(`value${i}`)
      );
    }

    const endTime = Date.now();
    const duration = endTime - startTime;

    console.log(`Performance Results:`);
    console.log(`   Total time: ${duration}ms`);
    console.log(`   Average per quad: ${(duration / 1000).toFixed(3)}ms`);
    console.log(`   Quads per second: ${Math.round(1000 / (duration / 1000))}`);

    // Get event statistics
    const stats = hooks.getEventStats();
    console.log(`Event Statistics:`);
    console.log(`   Total events: ${stats.totalEvents}`);
    console.log(`   Total hooks: ${stats.totalHooks}`);
    console.log(`   Average execution time: ${stats.avgExecutionTime}ms`);
    console.log(`   Errors: ${stats.errors}`);

    unregister.unregister();
  });
}

/**
 * Run all examples
 */
async function runAllExamples() {
  console.log('🚀 Knowledge Hooks Event System - Complete Examples');
  console.log('='.repeat(60));

  try {
    await basicEventHooks();
    await beforeHooksWithVeto();
    await engineLevelEvents();
    await knowledgeHooksWithEvents();
    await batchOperations();
    await performanceMonitoring();

    console.log('\n✅ All examples completed successfully!');
  } catch (error) {
    console.error('❌ Example failed:', error);
  }
}

// Run examples if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllExamples();
}

export {
  basicEventHooks,
  beforeHooksWithVeto,
  engineLevelEvents,
  knowledgeHooksWithEvents,
  batchOperations,
  performanceMonitoring,
  runAllExamples,
};
