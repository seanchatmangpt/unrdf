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
import {
  buildFederationPlan,
  executeFederationPlan,
} from '../../packages/federation/src/index.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad } = dataFactory;

const NAME = 'http://schema.org/name';

/**
 * Convert an oxigraph term into the string form the federation planner binds
 * (IRIs as bare strings, literals as quoted strings).
 * @param {object} term - RDF term
 * @returns {string} Binding value
 */
function termToBinding(term) {
  return term.termType === 'Literal' ? JSON.stringify(term.value) : term.value;
}

/**
 * Create an in-process federated engine over oxigraph endpoint stores using the
 * federation query planner (bind-join across sources).
 * @param {object[]} endpoints - Endpoint configurations from createEndpoints
 * @returns {{query: function(string): Promise<object>}} Engine
 */
function createFederatedEngine(endpoints) {
  const sources = endpoints.map(endpoint => ({
    id: endpoint.id,
    metadata: { predicates: endpoint.predicates, cardinality: endpoint.size },
    query: async subquery => endpoint.store.query(subquery).map(row => {
      const out = {};
      for (const [key, term] of row.entries()) out[key] = termToBinding(term);
      return out;
    })
  }));
  return {
    query: async sparql => executeFederationPlan(buildFederationPlan(sparql, sources))
  };
}

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
      const subject = namedNode(`http://shared.org/resource/${j}`);
      const object = literal(`Resource ${i}-${j}`);
      await store.add(quad(subject, namedNode(NAME), object));
      await store.add(quad(subject, namedNode(`http://endpoint${i}.org/name`), object));
    }

    endpoints.push({
      id: `endpoint_${i}`,
      url: `http://endpoint${i}.org/sparql`,
      predicates: [NAME, `http://endpoint${i}.org/name`],
      size: triplesPerStore * 2,
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
      const query = `SELECT ?s ?o WHERE { ?s <${NAME}> ?o . }`;
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
      const query = `SELECT ?s ?a ?b WHERE {
          ?s <http://endpoint0.org/name> ?a .
          ?s <http://endpoint1.org/name> ?b .
        }`;
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
      const query = `SELECT ?s ?name1 ?name2 WHERE {
          ?s <http://endpoint0.org/name> ?name1 .
          ?s <http://endpoint1.org/name> ?name2 .
        }`;
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
        'SELECT ?s ?o WHERE { ?s <http://endpoint0.org/name> ?o . }',
        'SELECT ?s ?o WHERE { ?s <http://endpoint1.org/name> ?o . }',
        'SELECT ?s ?o WHERE { ?s <http://endpoint2.org/name> ?o . }'
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
