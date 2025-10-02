/**
 * @fileoverview Mock SPARQL Query Engine (London School TDD)
 * @description Test double for Comunica SPARQL engine - interaction-focused
 */

import { vi } from 'vitest';

/**
 * Creates a mock query result stream
 * @param {Array} bindings - Mock result bindings
 * @returns {object} Mock result stream
 */
export function createMockResultStream(bindings = []) {
  return {
    [Symbol.asyncIterator]: async function* () {
      for (const binding of bindings) {
        yield binding;
      }
    },
    on: vi.fn(),
    once: vi.fn(),
    emit: vi.fn()
  };
}

/**
 * Creates a mock SPARQL query engine
 * @param {object} options - Configuration options
 * @returns {object} Mock query engine
 */
export function createMockSparqlEngine(options = {}) {
  const {
    mockResults = [],
    shouldError = false,
    errorMessage = 'Mock SPARQL error'
  } = options;

  return {
    queryBindings: vi.fn(async (query, context) => {
      if (shouldError) {
        throw new Error(errorMessage);
      }
      return createMockResultStream(mockResults);
    }),

    queryQuads: vi.fn(async (query, context) => {
      if (shouldError) {
        throw new Error(errorMessage);
      }
      return createMockResultStream(mockResults);
    }),

    queryBoolean: vi.fn(async (query, context) => {
      if (shouldError) {
        throw new Error(errorMessage);
      }
      return true;
    }),

    queryVoid: vi.fn(async (query, context) => {
      if (shouldError) {
        throw new Error(errorMessage);
      }
    }),

    // Expose configuration for verification
    __config: {
      mockResults,
      shouldError,
      errorMessage
    }
  };
}

/**
 * Creates mock SPARQL bindings
 * @param {object} data - Key-value pairs for bindings
 * @returns {object} Mock binding
 */
export function createMockBinding(data = {}) {
  const bindings = new Map();

  for (const [key, value] of Object.entries(data)) {
    bindings.set(key, {
      value: value,
      termType: 'Literal',
      datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' }
    });
  }

  return {
    has: vi.fn(key => bindings.has(key)),
    get: vi.fn(key => bindings.get(key)),
    entries: vi.fn(() => bindings.entries()),
    keys: vi.fn(() => bindings.keys()),
    values: vi.fn(() => bindings.values()),
    size: bindings.size
  };
}

/**
 * Factory for complete SPARQL mock ecosystem
 * @param {object} options - Configuration
 * @returns {object} Complete SPARQL mocks
 */
export function createSparqlMocks(options = {}) {
  const engine = createMockSparqlEngine(options);

  return {
    engine,
    createBinding: createMockBinding,
    createResultStream: createMockResultStream
  };
}
