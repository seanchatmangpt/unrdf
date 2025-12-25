/**
 * @file Federation Performance Benchmarks
 * @module benchmarks/integration/federation-benchmark
 *
 * @description
 * Benchmarks for federated SPARQL query operations:
 * - Distributed query latency
 * - Federation overhead
 * - Load balancing efficiency
 * - Multi-endpoint queries
 */

import { suite, randomString } from '../framework.mjs';
import { createFederatedEngine } from '../../packages/federation/src/index.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad } = dataFactory;

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Create multiple endpoint stores
 * @param {number} count - Number of endpoints
 * @param {number} triplesPerStore - Triples per store
 * @returns {Promise<object[]>} Array of endpoint configurations
 */
async function createEndpoints(count, triplesPerStore = 100) {
  const endpoints = [];

  for (let i = 0; i < count; i++) {
    const store = createStore();

    // Populate store with domain-specific data
    for (let j = 0; j < triplesPerStore; j++) {
      const subject = namedNode(`http://endpoint${i}.org/resource/${j}`);
      const predicate = namedNode('http://schema.org/name');
      const object = literal(`Resource ${i}-${j}`);
      await store.add(quad(subject, predicate, object));
    }

    endpoints.push({
      id: `endpoint_${i}`,
      url: `http://endpoint${i}.org/sparql`,
      store: store
    });
  }

  return endpoints;
}

// =============================================================================
// Benchmark Suite
// =============================================================================

export const federationBenchmarks = suite('Federation Performance', {
  'create federated engine (2 endpoints)': {
    fn: async () => {
      const endpoints = await createEndpoints(2, 50);
      return createFederatedEngine(endpoints);
    },
    iterations: 1000,
    warmup: 100
  },

  'create federated engine (5 endpoints)': {
    fn: async () => {
      const endpoints = await createEndpoints(5, 50);
      return createFederatedEngine(endpoints);
    },
    iterations: 500,
    warmup: 50
  },

  'query single endpoint': {
    setup: async () => {
      const endpoints = await createEndpoints(2, 100);
      const engine = createFederatedEngine(endpoints);
      return { engine };
    },
    fn: async function() {
      const query = `
        SELECT ?s ?o WHERE {
          ?s <http://schema.org/name> ?o
        }
        LIMIT 10
      `;
      return await this.engine.query(query);
    },
    iterations: 3000,
    warmup: 300
  },

  'federated query (2 endpoints)': {
    setup: async () => {
      const endpoints = await createEndpoints(2, 100);
      const engine = createFederatedEngine(endpoints);
      return { engine };
    },
    fn: async function() {
      const query = `
        SELECT ?s ?o WHERE {
          SERVICE <http://endpoint0.org/sparql> {
            ?s <http://schema.org/name> ?o
          }
          UNION
          SERVICE <http://endpoint1.org/sparql> {
            ?s <http://schema.org/name> ?o
          }
        }
        LIMIT 20
      `;
      return await this.engine.query(query);
    },
    iterations: 2000,
    warmup: 200
  },

  'federated join (2 endpoints)': {
    setup: async () => {
      const endpoints = await createEndpoints(2, 50);
      const engine = createFederatedEngine(endpoints);
      return { engine };
    },
    fn: async function() {
      const query = `
        SELECT ?s ?name1 ?name2 WHERE {
          SERVICE <http://endpoint0.org/sparql> {
            ?s <http://schema.org/name> ?name1
          }
          SERVICE <http://endpoint1.org/sparql> {
            ?s <http://schema.org/name> ?name2
          }
        }
        LIMIT 10
      `;
      return await this.engine.query(query);
    },
    iterations: 1000,
    warmup: 100
  },

  'parallel endpoint queries': {
    setup: async () => {
      const endpoints = await createEndpoints(3, 100);
      const engine = createFederatedEngine(endpoints);
      return { engine };
    },
    fn: async function() {
      const queries = [
        'SELECT ?s ?o WHERE { ?s <http://schema.org/name> ?o } LIMIT 10',
        'SELECT ?s ?o WHERE { ?s <http://schema.org/name> ?o } LIMIT 10',
        'SELECT ?s ?o WHERE { ?s <http://schema.org/name> ?o } LIMIT 10'
      ];

      return await Promise.all(queries.map(q => this.engine.query(q)));
    },
    iterations: 1000,
    warmup: 100
  }
});

// =============================================================================
// Runner
// =============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await federationBenchmarks();
  const { formatDetailedReport } = await import('../framework.mjs');
  console.log('\n' + formatDetailedReport(result));
  process.exit(0);
}
