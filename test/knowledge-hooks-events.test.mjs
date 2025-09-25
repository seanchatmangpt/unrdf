/**
 * @fileoverview Knowledge Hooks Event System - Test Suite
 * 
 * Comprehensive tests for the event-based knowledge hooks system:
 * - ObservableStore event emission
 * - Before/after hooks with veto capability
 * - Engine-level event emission
 * - Hook registration/unregistration
 * - Batch operations
 * - Performance benchmarks
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { initStore } from "../src/context/index.mjs";
import { useKnowledgeHooks, defineHook } from "../src/composables/use-knowledge-hooks.mjs";
import { EVENTS, EventBus } from "../src/engines/event-bus.mjs";
import { ObservableStore } from "../src/engines/observable-store.mjs";
import { RdfEngine } from "../src/engines/rdf-engine.mjs";
import { DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

describe('EventBus', () => {
  let eventBus;

  beforeEach(() => {
    eventBus = new EventBus();
  });

  afterEach(() => {
    eventBus.clear();
  });

  it('should register and emit events', async () => {
    const events = [];
    const handler = (payload) => events.push(payload);

    eventBus.on('test-event', handler);
    await eventBus.emit('test-event', { data: 'test' });

    expect(events).toHaveLength(1);
    expect(events[0].data).toBe('test');
  });

  it('should unregister hooks', () => {
    const handler = () => {};
    const registration = eventBus.on('test-event', handler);
    
    expect(eventBus.off('test-event', registration.id)).toBe(true);
    expect(eventBus.off('test-event', registration.id)).toBe(false);
  });

  it('should handle before hooks with veto capability', async () => {
    const beforeHandler = () => false; // Veto
    const afterHandler = () => { throw new Error('Should not execute'); };

    eventBus.on('beforeTest', beforeHandler);
    eventBus.on('afterTest', afterHandler);

    const result = await eventBus.emit('beforeTest', {});
    expect(result).toBe(false);
  });

  it('should batch events correctly', async () => {
    const events = [];
    const handler = (payload) => events.push(payload);

    eventBus.on('test-event', handler);
    
    eventBus.startBatch();
    await eventBus.emit('test-event', { data: '1' });
    await eventBus.emit('test-event', { data: '2' });
    await eventBus.endBatch();

    expect(events).toHaveLength(2);
    expect(events[0].data).toBe('1');
    expect(events[1].data).toBe('2');
  });

  it('should track statistics', async () => {
    const handler = () => {};
    eventBus.on('test-event', handler);
    
    await eventBus.emit('test-event', {});
    await eventBus.emit('test-event', {});

    const stats = eventBus.getStats();
    expect(stats.totalEvents).toBe(2);
    expect(stats.totalHooks).toBe(1);
  });
});

describe('ObservableStore', () => {
  let store;

  beforeEach(() => {
    store = new ObservableStore();
  });

  afterEach(() => {
    store.clearHooks();
  });

  it('should emit events on quad addition', async () => {
    const events = [];
    const handler = (payload) => events.push(payload);

    store.on(EVENTS.AFTER_ADD_QUAD, handler);

    const testQuad = quad(
      namedNode('http://example.org/subject'),
      namedNode('http://example.org/predicate'),
      literal('object')
    );

    store.addQuad(testQuad);

    expect(events).toHaveLength(1);
    expect(events[0].event).toBe(EVENTS.AFTER_ADD_QUAD);
    expect(events[0].quad).toEqual(testQuad);
  });

  it('should emit events on quad removal', async () => {
    const events = [];
    const handler = (payload) => events.push(payload);

    store.on(EVENTS.AFTER_REMOVE_QUAD, handler);

    const testQuad = quad(
      namedNode('http://example.org/subject'),
      namedNode('http://example.org/predicate'),
      literal('object')
    );

    store.addQuad(testQuad);
    store.removeQuad(testQuad);

    expect(events).toHaveLength(1);
    expect(events[0].event).toBe(EVENTS.AFTER_REMOVE_QUAD);
  });

  it('should emit events on store clear', async () => {
    const events = [];
    const handler = (payload) => events.push(payload);

    store.on(EVENTS.AFTER_CLEAR, handler);

    store.addQuad(quad(
      namedNode('http://example.org/subject'),
      namedNode('http://example.org/predicate'),
      literal('object')
    ));

    store.clear();

    expect(events).toHaveLength(1);
    expect(events[0].event).toBe(EVENTS.AFTER_CLEAR);
    expect(events[0].quad).toHaveLength(1);
  });

  it('should support before hooks with veto', async () => {
    const beforeHandler = (payload) => {
      // Veto if predicate contains 'blocked'
      return !payload.quad.predicate.value.includes('blocked');
    };

    store.on(EVENTS.BEFORE_ADD_QUAD, beforeHandler);

    // This should be allowed
    const result1 = store.addQuad(quad(
      namedNode('http://example.org/subject'),
      namedNode('http://example.org/allowed'),
      literal('value')
    ));

    // This should be vetoed
    const result2 = store.addQuad(quad(
      namedNode('http://example.org/subject'),
      namedNode('http://example.org/blocked'),
      literal('value')
    ));

    expect(result1).toBeDefined();
    expect(result2).toBeDefined(); // Quad is created but not added
    expect(store.size).toBe(1); // Only one quad was actually added
  });

  it('should batch operations correctly', async () => {
    const events = [];
    const handler = (payload) => events.push(payload);

    store.on(EVENTS.AFTER_ADD_QUAD, handler);

    store.startBatch();
    store.addQuad(quad(
      namedNode('http://example.org/subject1'),
      namedNode('http://example.org/predicate'),
      literal('value1')
    ));
    store.addQuad(quad(
      namedNode('http://example.org/subject2'),
      namedNode('http://example.org/predicate'),
      literal('value2')
    ));
    await store.endBatch();

    // Should have 2 events (one for each quad)
    expect(events).toHaveLength(2);
    expect(store.size).toBe(2);
  });
});

describe('RdfEngine with Events', () => {
  let engine;

  beforeEach(() => {
    engine = new RdfEngine({ eventsEnabled: true });
  });

  afterEach(() => {
    engine.clearHooks();
  });

  it('should emit events on Turtle import', async () => {
    const events = [];
    const handler = (payload) => events.push(payload);

    engine.on(EVENTS.AFTER_IMPORT, handler);

    engine.parseTurtle(`
      @prefix ex: <http://example.org/> .
      ex:alice ex:name "Alice" .
    `);

    expect(events).toHaveLength(1);
    expect(events[0].event).toBe(EVENTS.AFTER_IMPORT);
    expect(events[0].context.metadata.format).toBe('Turtle');
  });

  it('should emit events on SPARQL UPDATE', async () => {
    const events = [];
    const handler = (payload) => events.push(payload);

    engine.on(EVENTS.AFTER_UPDATE, handler);

    await engine.query(`
      PREFIX ex: <http://example.org/>
      INSERT { ex:alice ex:name "Alice" }
      WHERE { }
    `);

    expect(events).toHaveLength(1);
    expect(events[0].event).toBe(EVENTS.AFTER_UPDATE);
  });

  it('should emit events on reasoning', async () => {
    const events = [];
    const handler = (payload) => events.push(payload);

    engine.on(EVENTS.AFTER_REASON, handler);

    // Add some data
    engine.parseTurtle(`
      @prefix ex: <http://example.org/> .
      ex:alice ex:hasParent ex:bob .
    `);

    // Add rules
    const rulesStore = engine.parseTurtle(`
      @prefix ex: <http://example.org/> .
      { ?x ex:hasParent ?y } => { ?y ex:hasChild ?x } .
    `);

    await engine.reason(engine.store, rulesStore);

    expect(events).toHaveLength(1);
    expect(events[0].event).toBe(EVENTS.AFTER_REASON);
  });
});

describe('useKnowledgeHooks Integration', () => {
  let runApp;

  beforeEach(() => {
    runApp = initStore([], { 
      baseIRI: 'http://example.org/',
      eventsEnabled: true 
    });
  });

  it('should register event hooks', async () => {
    await runApp(async () => {
      const hooks = useKnowledgeHooks();
      const events = [];

      const unregister = hooks.registerEventHook(
        EVENTS.AFTER_ADD_QUAD,
        (payload) => events.push(payload),
        { id: 'test-hook' }
      );

      hooks.engine.store.addQuad(quad(
        namedNode('http://example.org/subject'),
        namedNode('http://example.org/predicate'),
        literal('object')
      ));

      expect(events).toHaveLength(1);
      expect(events[0].event).toBe(EVENTS.AFTER_ADD_QUAD);

      unregister.unregister();
    });
  });

  it('should register knowledge hooks with events', async () => {
    await runApp(async () => {
      const hooks = useKnowledgeHooks();
      const callbacks = [];

      const errorHook = defineHook({
        id: 'error-monitor',
        events: [EVENTS.AFTER_ADD_QUAD],
        query: 'SELECT ?s WHERE { ?s ex:type ex:Error }',
        predicates: [
          { kind: 'COUNT', spec: { operator: '>', value: 0 } }
        ],
        options: {
          callback: async (result, payload) => {
            callbacks.push({ result, payload });
          }
        }
      });

      const unregister = hooks.registerKnowledgeHook(errorHook);

      // Add an error entity
      hooks.engine.store.addQuad(quad(
        namedNode('http://example.org/error1'),
        namedNode('http://example.org/type'),
        namedNode('http://example.org/Error')
      ));

      // Wait for async callback
      await new Promise(resolve => setTimeout(resolve, 100));

      expect(callbacks).toHaveLength(1);
      expect(callbacks[0].result.fired).toBe(true);

      unregister.unregister();
    });
  });

  it('should support batch operations', async () => {
    await runApp(async () => {
      const hooks = useKnowledgeHooks();
      const events = [];

      const unregister = hooks.registerEventHook(
        EVENTS.AFTER_ADD_QUAD,
        (payload) => events.push(payload),
        { id: 'batch-monitor' }
      );

      await hooks.batch(() => {
        hooks.engine.store.addQuad(quad(
          namedNode('http://example.org/subject1'),
          namedNode('http://example.org/predicate'),
          literal('value1')
        ));
        hooks.engine.store.addQuad(quad(
          namedNode('http://example.org/subject2'),
          namedNode('http://example.org/predicate'),
          literal('value2')
        ));
      });

      expect(events).toHaveLength(2);
      expect(hooks.engine.store.size).toBe(2);

      unregister.unregister();
    });
  });

  it('should provide event statistics', async () => {
    await runApp(async () => {
      const hooks = useKnowledgeHooks();

      hooks.registerEventHook(
        EVENTS.AFTER_ADD_QUAD,
        () => {},
        { id: 'stats-hook' }
      );

      hooks.engine.store.addQuad(quad(
        namedNode('http://example.org/subject'),
        namedNode('http://example.org/predicate'),
        literal('object')
      ));

      const stats = hooks.getEventStats();
      expect(stats.totalEvents).toBeGreaterThan(0);
      expect(stats.totalHooks).toBe(1);
    });
  });
});

describe('Performance Benchmarks', () => {
  let runApp;

  beforeEach(() => {
    runApp = initStore([], { 
      baseIRI: 'http://example.org/',
      eventsEnabled: true 
    });
  });

  it('should have minimal overhead with no hooks', async () => {
    await runApp(async () => {
      const hooks = useKnowledgeHooks();
      const store = hooks.engine.store;

      const iterations = 1000;
      const startTime = Date.now();

      for (let i = 0; i < iterations; i++) {
        store.addQuad(quad(
          namedNode(`http://example.org/subject${i}`),
          namedNode('http://example.org/predicate'),
          literal(`value${i}`)
        ));
      }

      const endTime = Date.now();
      const duration = endTime - startTime;
      const avgPerQuad = duration / iterations;

      // Should be very fast without hooks
      expect(avgPerQuad).toBeLessThan(1); // Less than 1ms per quad
      expect(store.size).toBe(iterations);
    });
  });

  it('should maintain performance with hooks', async () => {
    await runApp(async () => {
      const hooks = useKnowledgeHooks();
      const store = hooks.engine.store;

      // Register a simple hook
      const unregister = hooks.registerEventHook(
        EVENTS.AFTER_ADD_QUAD,
        () => {}, // Empty handler
        { id: 'perf-hook' }
      );

      const iterations = 1000;
      const startTime = Date.now();

      for (let i = 0; i < iterations; i++) {
        store.addQuad(quad(
          namedNode(`http://example.org/subject${i}`),
          namedNode('http://example.org/predicate'),
          literal(`value${i}`)
        ));
      }

      const endTime = Date.now();
      const duration = endTime - startTime;
      const avgPerQuad = duration / iterations;

      // Should still be reasonably fast with hooks
      expect(avgPerQuad).toBeLessThan(5); // Less than 5ms per quad
      expect(store.size).toBe(iterations);

      unregister.unregister();
    });
  });

  it('should scale to 10k quads', async () => {
    await runApp(async () => {
      const hooks = useKnowledgeHooks();
      const store = hooks.engine.store;

      const iterations = 10000;
      const startTime = Date.now();

      for (let i = 0; i < iterations; i++) {
        store.addQuad(quad(
          namedNode(`http://example.org/subject${i}`),
          namedNode('http://example.org/predicate'),
          literal(`value${i}`)
        ));
      }

      const endTime = Date.now();
      const duration = endTime - startTime;
      const avgPerQuad = duration / iterations;

      // Should handle 10k quads efficiently
      expect(avgPerQuad).toBeLessThan(1); // Less than 1ms per quad
      expect(store.size).toBe(iterations);
    });
  });
});

describe('Error Handling', () => {
  let runApp;

  beforeEach(() => {
    runApp = initStore([], { 
      baseIRI: 'http://example.org/',
      eventsEnabled: true 
    });
  });

  it('should handle hook errors gracefully', async () => {
    await runApp(async () => {
      const hooks = useKnowledgeHooks();
      const errors = [];

      // Register error handler
      const errorHandler = (error, context) => {
        errors.push({ error: error.message, context });
      };

      const eventBus = hooks.engine.eventBus;
      eventBus.onError = errorHandler;

      // Register hook that throws error
      const unregister = hooks.registerEventHook(
        EVENTS.AFTER_ADD_QUAD,
        () => {
          throw new Error('Hook error');
        },
        { id: 'error-hook' }
      );

      hooks.engine.store.addQuad(quad(
        namedNode('http://example.org/subject'),
        namedNode('http://example.org/predicate'),
        literal('object')
      ));

      // Wait for async error handling
      await new Promise(resolve => setTimeout(resolve, 100));

      expect(errors).toHaveLength(1);
      expect(errors[0].error).toBe('Hook error');

      unregister.unregister();
    });
  });

  it('should continue operation after hook errors', async () => {
    await runApp(async () => {
      const hooks = useKnowledgeHooks();
      const events = [];

      // Register hook that throws error
      hooks.registerEventHook(
        EVENTS.AFTER_ADD_QUAD,
        () => {
          throw new Error('Hook error');
        },
        { id: 'error-hook' }
      );

      // Register another hook that should still work
      const unregister = hooks.registerEventHook(
        EVENTS.AFTER_ADD_QUAD,
        (payload) => events.push(payload),
        { id: 'working-hook' }
      );

      hooks.engine.store.addQuad(quad(
        namedNode('http://example.org/subject'),
        namedNode('http://example.org/predicate'),
        literal('object')
      ));

      // Wait for async processing
      await new Promise(resolve => setTimeout(resolve, 100));

      expect(events).toHaveLength(1);
      expect(hooks.engine.store.size).toBe(1);

      unregister.unregister();
    });
  });
});
