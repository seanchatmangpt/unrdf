/**
 * @file Shared Vitest Test Helpers for UNRDF
 * @description Reusable test utilities for all 26 examples
 */

import { createStore, dataFactory } from '@unrdf/oxigraph'

const { namedNode, literal, quad, defaultGraph } = dataFactory

// ============================================================================
// Sample Data Generators
// ============================================================================

/**
 * Create a sample RDF quad
 * @param {string} subject - Subject URI or blank node
 * @param {string} predicate - Predicate URI
 * @param {string} object - Object value
 * @param {string} [graph] - Optional graph URI
 * @returns {import('n3').Quad}
 */
export function createSampleQuad(subject, predicate, object, graph) {
  const s = subject.startsWith('_:') ? DataFactory.blankNode(subject.slice(2)) : namedNode(subject)
  const p = namedNode(predicate)
  const o = object.startsWith('http') ? namedNode(object) : literal(object)
  const g = graph ? namedNode(graph) : defaultGraph()
  return quad(s, p, o, g)
}

/**
 * Create a sample N3 Store
 * @param {boolean} [withData=true] - Pre-populate with sample data
 * @returns {import('n3').Store}
 */
export function createSampleStore(withData = true) {
  const store = createStore()

  if (withData) {
    // Add sample FOAF data
    store.addQuad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Alice')
    )
    store.addQuad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/email'),
      literal('alice@example.org')
    )
    store.addQuad(
      namedNode('http://example.org/bob'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Bob')
    )
    store.addQuad(
      namedNode('http://example.org/bob'),
      namedNode('http://xmlns.com/foaf/0.1/email'),
      literal('bob@example.org')
    )
  }

  return store
}

/**
 * Create a sample RDF graph with random data
 * @param {number} [size=10] - Number of quads to generate
 * @returns {import('n3').Store}
 */
export function createSampleGraph(size = 10) {
  const store = createStore()

  for (let i = 0; i < size; i++) {
    store.addQuad(
      namedNode(`http://example.org/entity${i}`),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://example.org/Thing')
    )
    store.addQuad(
      namedNode(`http://example.org/entity${i}`),
      namedNode('http://example.org/value'),
      literal(`Value ${i}`)
    )
  }

  return store
}

// ============================================================================
// Assertion Helpers
// ============================================================================

/**
 * Assert two quads are equal
 * @param {import('n3').Quad} quad1
 * @param {import('n3').Quad} quad2
 * @param {string} [message] - Optional error message
 */
export function assertQuadEquals(quad1, quad2, message) {
  if (!quad1 || !quad2) {
    throw new Error(message || `Expected both quads to be defined, got: ${quad1}, ${quad2}`)
  }

  if (!quad1.equals(quad2)) {
    throw new Error(
      message ||
      `Expected quads to be equal:\nGot:      ${quad1.toJSON()}\nExpected: ${quad2.toJSON()}`
    )
  }
}

/**
 * Assert two arrays of quads are equal (order-independent)
 * @param {import('n3').Quad[]} quads1
 * @param {import('n3').Quad[]} quads2
 * @param {string} [message] - Optional error message
 */
export function assertQuadsEqual(quads1, quads2, message) {
  if (quads1.length !== quads2.length) {
    throw new Error(
      message ||
      `Expected same number of quads: got ${quads1.length}, expected ${quads2.length}`
    )
  }

  const store1 = createStore(quads1)
  const store2 = createStore(quads2)

  for (const q of quads1) {
    if (!store2.has(q)) {
      throw new Error(message || `Expected quad not found in second set: ${q.toJSON()}`)
    }
  }

  for (const q of quads2) {
    if (!store1.has(q)) {
      throw new Error(message || `Unexpected quad found in second set: ${q.toJSON()}`)
    }
  }
}

/**
 * Assert a store contains a specific quad
 * @param {import('n3').Store} store
 * @param {import('n3').Quad} quad
 * @param {string} [message] - Optional error message
 */
export function assertStoreHasQuad(store, quad, message) {
  if (!store.has(quad)) {
    throw new Error(
      message ||
      `Expected store to contain quad: ${quad.toJSON()}\nStore has ${store.size} quads`
    )
  }
}

/**
 * Assert a store has a specific size
 * @param {import('n3').Store} store
 * @param {number} expectedSize
 * @param {string} [message] - Optional error message
 */
export function assertStoreSize(store, expectedSize, message) {
  if (store.size !== expectedSize) {
    throw new Error(
      message ||
      `Expected store size ${expectedSize}, got ${store.size}`
    )
  }
}

// ============================================================================
// Async Helpers
// ============================================================================

/**
 * Delay execution for specified milliseconds
 * @param {number} ms - Milliseconds to delay
 * @returns {Promise<void>}
 */
export async function delay(ms) {
  return new Promise(resolve => setTimeout(resolve, ms))
}

/**
 * Wait for a condition to become true
 * @param {() => boolean | Promise<boolean>} condition - Condition to check
 * @param {number} [timeout=5000] - Max time to wait in ms
 * @param {number} [interval=100] - Check interval in ms
 * @returns {Promise<void>}
 * @throws {Error} If timeout is reached
 */
export async function waitFor(condition, timeout = 5000, interval = 100) {
  const startTime = Date.now()

  while (Date.now() - startTime < timeout) {
    const result = await condition()
    if (result) return
    await delay(interval)
  }

  throw new Error(`Timeout waiting for condition after ${timeout}ms`)
}

/**
 * Wait for a value to change
 * @template T
 * @param {() => T} getValue - Function to get current value
 * @param {T} expectedValue - Value to wait for
 * @param {number} [timeout=5000] - Max time to wait in ms
 * @returns {Promise<T>}
 */
export async function waitForValue(getValue, expectedValue, timeout = 5000) {
  await waitFor(
    () => getValue() === expectedValue,
    timeout
  )
  return expectedValue
}

// ============================================================================
// Mock Creators
// ============================================================================

/**
 * Create a mock peer for federation tests
 * @param {string} id - Peer ID
 * @param {string} [status='healthy'] - Peer status
 * @returns {Object}
 */
export function createMockPeer(id, status = 'healthy') {
  return {
    id,
    status,
    lastSeen: Date.now(),
    syncState: {
      lastSync: Date.now(),
      version: 1,
      pending: []
    },
    metadata: {
      name: `Peer ${id}`,
      capabilities: ['query', 'sync']
    }
  }
}

/**
 * Create a mock change feed for streaming tests
 * @returns {Object}
 */
export function createMockChangeFeed() {
  const subscribers = new Set()

  return {
    subscribe(callback) {
      subscribers.add(callback)
      return () => subscribers.delete(callback)
    },
    emit(change) {
      for (const callback of subscribers) {
        callback(change)
      }
    },
    clear() {
      subscribers.clear()
    },
    size() {
      return subscribers.size
    }
  }
}

/**
 * Create a mock subscription for reactive tests
 * @returns {Object}
 */
export function createMockSubscription() {
  let active = true
  const handlers = {
    next: null,
    error: null,
    complete: null
  }

  return {
    subscribe(nextHandler, errorHandler, completeHandler) {
      handlers.next = nextHandler
      handlers.error = errorHandler
      handlers.complete = completeHandler
      return this
    },
    next(value) {
      if (active && handlers.next) handlers.next(value)
    },
    error(err) {
      if (active && handlers.error) handlers.error(err)
      active = false
    },
    complete() {
      if (active && handlers.complete) handlers.complete()
      active = false
    },
    unsubscribe() {
      active = false
    },
    isActive() {
      return active
    }
  }
}

/**
 * Create a mock HTTP server response
 * @param {number} [statusCode=200] - HTTP status code
 * @param {Object} [body={}] - Response body
 * @returns {Object}
 */
export function createMockResponse(statusCode = 200, body = {}) {
  return {
    statusCode,
    body,
    headers: {},
    json() {
      return Promise.resolve(body)
    },
    text() {
      return Promise.resolve(JSON.stringify(body))
    }
  }
}

// ============================================================================
// Test Data Validation
// ============================================================================

/**
 * Validate a quad has correct structure
 * @param {any} quad - Value to validate
 * @returns {boolean}
 */
export function isValidQuad(quad) {
  return (
    quad &&
    typeof quad === 'object' &&
    quad.subject &&
    quad.predicate &&
    quad.object &&
    quad.graph !== undefined
  )
}

/**
 * Validate a store is properly initialized
 * @param {any} store - Value to validate
 * @returns {boolean}
 */
export function isValidStore(store) {
  return (
    store &&
    typeof store === 'object' &&
    typeof store.size === 'number' &&
    typeof store.addQuad === 'function' &&
    typeof store.getQuads === 'function'
  )
}

// ============================================================================
// Test Cleanup Helpers
// ============================================================================

/**
 * Clean up test resources
 * @param {Object} resources - Resources to clean up
 */
export function cleanupResources(resources) {
  if (resources.store) {
    resources.store = null
  }
  if (resources.subscriptions) {
    for (const sub of resources.subscriptions) {
      if (typeof sub.unsubscribe === 'function') {
        sub.unsubscribe()
      }
    }
    resources.subscriptions = []
  }
  if (resources.timers) {
    for (const timer of resources.timers) {
      clearTimeout(timer)
    }
    resources.timers = []
  }
}

/**
 * Create a test context with automatic cleanup
 * @returns {Object}
 */
export function createTestContext() {
  const context = {
    store: null,
    subscriptions: [],
    timers: [],
    cleanup() {
      cleanupResources(this)
    }
  }

  return context
}
