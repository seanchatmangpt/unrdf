/**
 * @file SPARQL Performance Benchmarks
 * @module benchmarks/core/sparql-performance
 *
 * @description
 * Comprehensive benchmarks for SPARQL query operations:
 * - Query parsing speed
 * - Query execution time
 * - Triple insertion performance
 * - Store scaling characteristics
 */

import { suite, randomString } from '../framework.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad } = dataFactory;

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Generate random triple
 * @returns {object} Quad object
 */
function generateTriple() {
  const subject = namedNode(`http://example.org/entity/${randomString(8)}`);
  const predicate = namedNode('http://example.org/property/name');
  const object = literal(`Name ${randomString(8)}`);
  return quad(subject, predicate, object);
}

/**
 * Populate store with triples
 * @param {object} store - Oxigraph store
 * @param {number} count - Number of triples to insert
 */
async function populateStore(store, count) {
  const triples = [];
  for (let i = 0; i < count; i++) {
    triples.push(generateTriple());
  }

  for (const triple of triples) {
    await store.add(triple);
  }
}

// =============================================================================
// Benchmark Suite
// =============================================================================

export const sparqlBenchmarks = suite('SPARQL Performance', {
  'create store': {
    fn: () => {
      return createStore();
    },
    iterations: 1000,
    warmup: 100
  },

  'insert single triple': {
    setup: () => {
      const store = createStore();
      return { store };
    },
    fn: async function() {
      const triple = generateTriple();
      return await this.store.add(triple);
    },
    iterations: 10000,
    warmup: 1000
  },

  'insert batch of 100 triples': {
    setup: () => {
      const store = createStore();
      return { store };
    },
    fn: async function() {
      const triples = [];
      for (let i = 0; i < 100; i++) {
        triples.push(generateTriple());
      }

      for (const triple of triples) {
        await this.store.add(triple);
      }
    },
    iterations: 1000,
    warmup: 100
  },

  'query all triples (100 in store)': {
    setup: async () => {
      const store = createStore();
      await populateStore(store, 100);
      return { store };
    },
    fn: async function() {
      const query = 'SELECT * WHERE { ?s ?p ?o } LIMIT 100';
      return await this.store.query(query);
    },
    iterations: 5000,
    warmup: 500
  },

  'query all triples (1000 in store)': {
    setup: async () => {
      const store = createStore();
      await populateStore(store, 1000);
      return { store };
    },
    fn: async function() {
      const query = 'SELECT * WHERE { ?s ?p ?o } LIMIT 100';
      return await this.store.query(query);
    },
    iterations: 3000,
    warmup: 300
  },

  'query with filter': {
    setup: async () => {
      const store = createStore();
      await populateStore(store, 500);
      return { store };
    },
    fn: async function() {
      const query = `
        SELECT ?s ?o WHERE {
          ?s <http://example.org/property/name> ?o .
          FILTER(STRLEN(?o) > 10)
        }
        LIMIT 50
      `;
      return await this.store.query(query);
    },
    iterations: 3000,
    warmup: 300
  },

  'construct query': {
    setup: async () => {
      const store = createStore();
      await populateStore(store, 200);
      return { store };
    },
    fn: async function() {
      const query = `
        CONSTRUCT {
          ?s <http://example.org/property/label> ?o
        } WHERE {
          ?s <http://example.org/property/name> ?o
        }
        LIMIT 50
      `;
      return await this.store.query(query);
    },
    iterations: 2000,
    warmup: 200
  },

  'ask query (exists)': {
    setup: async () => {
      const store = createStore();
      await populateStore(store, 100);
      return { store };
    },
    fn: async function() {
      const query = `
        ASK {
          ?s <http://example.org/property/name> ?o
        }
      `;
      return await this.store.query(query);
    },
    iterations: 10000,
    warmup: 1000
  },

  'count triples': {
    setup: async () => {
      const store = createStore();
      await populateStore(store, 500);
      return { store };
    },
    fn: async function() {
      const query = `
        SELECT (COUNT(*) as ?count) WHERE {
          ?s ?p ?o
        }
      `;
      return await this.store.query(query);
    },
    iterations: 5000,
    warmup: 500
  }
});

// =============================================================================
// Runner
// =============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await sparqlBenchmarks();
  const { formatDetailedReport } = await import('../framework.mjs');
  console.log('\n' + formatDetailedReport(result));
  process.exit(0);
}
