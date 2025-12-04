/**
 * Test Fixtures for Benchmarks
 *
 * Reusable test data, hooks, events, and stores for all benchmarks.
 * Reduces duplication and ensures consistency across benchmark suites.
 */

import { Store } from '@unrdf/oxigraph';
import { DataFactory } from 'n3';

const { namedNode, literal, quad } = DataFactory;

/**
 * Create a simple hook for basic benchmarks
 * @param {string} [id='simple-hook'] - Hook identifier
 * @returns {Object} Hook configuration
 */
export function createSimpleHook(id = 'simple-hook') {
  return {
    id,
    event: 'test:event',
    handler: async (context) => {
      return { success: true, processed: true };
    },
    enabled: true
  };
}

/**
 * Create a complex hook with validation
 * @param {string} [id='complex-hook'] - Hook identifier
 * @returns {Object} Hook configuration with validation
 */
export function createComplexHook(id = 'complex-hook') {
  return {
    id,
    event: 'test:complex',
    conditions: [
      {
        type: 'property',
        property: 'dataset',
        operator: 'exists'
      }
    ],
    validation: {
      validateInput: async (context) => {
        if (!context.dataset) {
          throw new Error('Dataset required');
        }
        return true;
      }
    },
    handler: async (context) => {
      const store = context.dataset.getStore();
      const quads = [];

      for (const q of store.match()) {
        quads.push(q);
      }

      return {
        success: true,
        processed: true,
        quadCount: quads.length
      };
    },
    enabled: true,
    priority: 100
  };
}

/**
 * Create a test event
 * @param {string} [type='test:event'] - Event type
 * @param {Object} [payload={}] - Event payload
 * @returns {Object} Event object
 */
export function createEvent(type = 'test:event', payload = {}) {
  return {
    type,
    timestamp: Date.now(),
    payload,
    dataset: payload.dataset || null
  };
}

/**
 * Create a test N3/Oxigraph store with sample data
 * @param {number} [quadCount=10] - Number of quads to generate
 * @returns {Store} Oxigraph store with quads
 */
export function createStore(quadCount = 10) {
  const store = new Store();

  for (let i = 0; i < quadCount; i++) {
    const subject = namedNode(`http://example.org/subject${i}`);
    const predicate = namedNode('http://example.org/predicate');
    const object = literal(`value${i}`);

    store.add(quad(subject, predicate, object));
  }

  return store;
}

/**
 * Create an array of N hooks
 * @param {number} count - Number of hooks to create
 * @param {string} [prefix='hook'] - Hook ID prefix
 * @returns {Array<Object>} Array of hook configurations
 */
export function createHookArray(count, prefix = 'hook') {
  const hooks = [];

  for (let i = 0; i < count; i++) {
    hooks.push(createSimpleHook(`${prefix}-${i}`));
  }

  return hooks;
}

/**
 * Create a dataset wrapper for a store
 * @param {Store} [store=null] - Oxigraph store (creates new if null)
 * @returns {Object} Dataset wrapper
 */
export function createDataset(store = null) {
  const datasetStore = store || createStore();

  return {
    getStore: () => datasetStore,
    size: () => {
      let count = 0;
      for (const _ of datasetStore.match()) {
        count++;
      }
      return count;
    }
  };
}

/**
 * Create a hook with async delay for latency testing
 * @param {number} delayMs - Delay in milliseconds
 * @param {string} [id='delayed-hook'] - Hook identifier
 * @returns {Object} Hook with async delay
 */
export function createDelayedHook(delayMs, id = 'delayed-hook') {
  return {
    id,
    event: 'test:delayed',
    handler: async (context) => {
      await new Promise(resolve => setTimeout(resolve, delayMs));
      return { success: true, delayed: true, delayMs };
    },
    enabled: true
  };
}

/**
 * Create a hook that throws errors for error handling benchmarks
 * @param {string} [id='error-hook'] - Hook identifier
 * @returns {Object} Hook that throws
 */
export function createErrorHook(id = 'error-hook') {
  return {
    id,
    event: 'test:error',
    handler: async (context) => {
      throw new Error('Intentional test error');
    },
    enabled: true
  };
}

/**
 * Create a batch of events
 * @param {number} count - Number of events to create
 * @param {string} [type='test:batch'] - Event type
 * @returns {Array<Object>} Array of events
 */
export function createEventBatch(count, type = 'test:batch') {
  const events = [];

  for (let i = 0; i < count; i++) {
    events.push(createEvent(type, { index: i }));
  }

  return events;
}

/**
 * Create a large store for memory benchmarks
 * @param {number} [quadCount=10000] - Number of quads
 * @returns {Store} Large store
 */
export function createLargeStore(quadCount = 10000) {
  return createStore(quadCount);
}

/**
 * Create hooks with different priorities for ordering tests
 * @param {number} count - Number of hooks
 * @returns {Array<Object>} Hooks with varying priorities
 */
export function createPrioritizedHooks(count) {
  const hooks = [];

  for (let i = 0; i < count; i++) {
    hooks.push({
      ...createSimpleHook(`priority-hook-${i}`),
      priority: i * 10
    });
  }

  return hooks;
}
