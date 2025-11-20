/**
 * @file Test Data Builder for Knowledge Engine Tests
 * @module test-data-builder
 * 
 * @description
 * Builder pattern for creating test data objects with sensible defaults
 * and easy customization.
 */

import { defineHook } from '../../../src/knowledge-engine/define-hook.mjs';
import { Store } from 'n3';
import { DataFactory } from 'n3';
import crypto from 'node:crypto';

const { namedNode, quad } = DataFactory;

/**
 * Builder for creating test data objects
 */
export class TestDataBuilder {
  /**
   *
   */
  constructor() {
    this.counter = 0;
  }

  /**
   * Generate a unique identifier
   * @param {string} prefix - Prefix for the identifier
   * @returns {string} Unique identifier
   */
  generateId(prefix = 'test') {
    return `${prefix}-${Date.now()}-${++this.counter}`;
  }

  /**
   * Build a KnowledgeHook with sensible defaults
   * @param {Object} overrides - Override default values
   * @returns {Object} KnowledgeHook definition
   */
  buildHook(overrides = {}) {
    const defaults = {
      meta: {
        name: this.generateId('hook'),
        description: 'Test hook for knowledge engine'
      },
      when: this.buildCondition(),
      run: async (event) => {
        return { success: true, hookName: event.name };
      }
    };

    return { ...defaults, ...overrides };
  }

  /**
   * Build a HookEvent with sensible defaults
   * @param {Object} overrides - Override default values
   * @returns {Object} HookEvent
   */
  buildEvent(overrides = {}) {
    const defaults = {
      name: this.generateId('event'),
      payload: {},
      context: {
        graph: this.buildStore(),
        user: { id: 'test-user', role: 'user' },
        timestamp: new Date().toISOString()
      },
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      source: 'test'
    };

    return { ...defaults, ...overrides };
  }

  /**
   * Build a Store with optional quads
   * @param {Array} quads - Array of quads to add to the store
   * @returns {Store} N3 Store instance
   */
  buildStore(quads = []) {
    const store = new Store();
    if (quads.length > 0) {
      store.addQuads(quads);
    } else {
      // Add some default test data
      const defaultQuads = [
        quad(
          namedNode('http://example.org/subject1'),
          namedNode('http://example.org/predicate1'),
          namedNode('http://example.org/object1')
        ),
        quad(
          namedNode('http://example.org/subject2'),
          namedNode('http://example.org/predicate2'),
          namedNode('http://example.org/object2')
        )
      ];
      store.addQuads(defaultQuads);
    }
    return store;
  }

  /**
   * Build a Condition with sensible defaults
   * @param {Object} overrides - Override default values
   * @returns {Object} Condition
   */
  buildCondition(overrides = {}) {
    const defaults = {
      kind: 'sparql-ask',
      ref: this.buildFileRef()
    };

    return { ...defaults, ...overrides };
  }

  /**
   * Build a FileRef with sensible defaults
   * @param {Object} overrides - Override default values
   * @returns {Object} FileRef
   */
  buildFileRef(overrides = {}) {
    const defaults = {
      uri: `file://test-${this.generateId('file')}.sparql`,
      sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
      mediaType: 'application/sparql-query'
    };

    return { ...defaults, ...overrides };
  }

  /**
   * Build a TransactionDelta with sensible defaults
   * @param {Object} overrides - Override default values
   * @returns {Object} TransactionDelta
   */
  buildTransactionDelta(overrides = {}) {
    const defaults = {
      additions: [],
      removals: [],
      metadata: {},
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString()
    };

    return { ...defaults, ...overrides };
  }

  /**
   * Build a HookContext with sensible defaults
   * @param {Object} overrides - Override default values
   * @returns {Object} HookContext
   */
  buildHookContext(overrides = {}) {
    const defaults = {
      graph: this.buildStore(),
      user: { id: 'test-user', role: 'user' },
      env: {},
      metadata: {},
      transactionId: crypto.randomUUID(),
      timestamp: new Date().toISOString()
    };

    return { ...defaults, ...overrides };
  }

  /**
   * Build a ManagerConfig with sensible defaults
   * @param {Object} overrides - Override default values
   * @returns {Object} ManagerConfig
   */
  buildManagerConfig(overrides = {}) {
    const defaults = {
      basePath: '/tmp/test',
      enableKnowledgeHooks: true,
      strictMode: false,
      maxConcurrentHooks: 10,
      hookTimeout: 30000,
      retryAttempts: 3
    };

    return { ...defaults, ...overrides };
  }

  /**
   * Build a SPARQL query string
   * @param {string} type - Type of query (ask, select, construct, describe)
   * @param {Object} options - Query options
   * @returns {string} SPARQL query
   */
  buildSparqlQuery(type = 'ask', options = {}) {
    const { subject = '?s', predicate = '?p', object = '?o' } = options;
    
    switch (type) {
      case 'ask':
        return `ASK WHERE { ${subject} ${predicate} ${object} }`;
      case 'select':
        return `SELECT * WHERE { ${subject} ${predicate} ${object} }`;
      case 'construct':
        return `CONSTRUCT { ${subject} ${predicate} ${object} } WHERE { ${subject} ${predicate} ${object} }`;
      case 'describe':
        return `DESCRIBE ${subject}`;
      default:
        return `ASK WHERE { ${subject} ${predicate} ${object} }`;
    }
  }

  /**
   * Build a SHACL shape definition
   * @param {Object} options - Shape options
   * @returns {string} SHACL shape in Turtle format
   */
  buildShaclShape(options = {}) {
    const { shapeName = 'TestShape', propertyPath = 'ex:testProperty' } = options;
    
    return `
@prefix ex: <http://example.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:${shapeName}
  a sh:NodeShape ;
  sh:targetClass ex:TestClass ;
  sh:property [
    sh:path ${propertyPath} ;
    sh:datatype xsd:string ;
    sh:minCount 1 ;
    sh:maxCount 1
  ] .
`;
  }

  /**
   * Build a hook that will succeed
   * @param {Object} overrides - Override default values
   * @returns {Object} KnowledgeHook definition
   */
  buildSuccessHook(overrides = {}) {
    return this.buildHook({
      run: async (event) => {
        return { success: true, result: 'Hook executed successfully' };
      },
      ...overrides
    });
  }

  /**
   * Build a hook that will fail
   * @param {Object} overrides - Override default values
   * @param {Error} [error] - Error to throw
   * @returns {Object} KnowledgeHook definition
   */
  buildFailureHook(overrides = {}, error = new Error('Test hook failure')) {
    return this.buildHook({
      run: async (event) => {
        throw error;
      },
      ...overrides
    });
  }

  /**
   * Build a hook that will timeout
   * @param {Object} overrides - Override default values
   * @param {number} [timeout=100] - Timeout in milliseconds
   * @returns {Object} KnowledgeHook definition
   */
  buildTimeoutHook(overrides = {}, timeout = 100) {
    return this.buildHook({
      run: async (event) => {
        await new Promise(resolve => setTimeout(resolve, timeout));
        return { success: true, result: 'Hook completed after timeout' };
      },
      ...overrides
    });
  }
}
